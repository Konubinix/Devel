;;; KONIX_claude-code-usage.el ---              -*- lexical-binding: t; -*-

;; Copyright (C) 2026  konubinix

;; Author: konubinix <konubinixweb@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Claude Code API usage tracking and display.

;;; Code:

(require 'plz)
(require 'term)

(defconst konix/claude-code--credentials-file "~/.claude/.credentials.json"
  "Where the Claude Code CLI stores its OAuth credentials.")

(defconst konix/claude-code--oauth-token-url
  "https://platform.claude.com/v1/oauth/token"
  "Token endpoint the Claude Code CLI performs its OAuth grants against.")

(defconst konix/claude-code--oauth-client-id
  "9d1c250a-e61b-44d9-88ed-5944d1962f5e"
  "OAuth client id of the Claude Code CLI.")

(defconst konix/claude-code--oauth-default-scopes
  '("user:inference" "user:profile" "user:sessions:claude_code"
    "user:mcp_servers" "user:file_upload")
  "Scopes to ask for when the credentials file records none.")

(defvar konix/claude-code--refresh-margin 300
  "Renew the access token when fewer than this many seconds remain.")

(define-error 'konix/claude-code-refresh-error
  "Claude Code token refresh failed")

(define-error 'konix/claude-code-refresh-rejected
  "Claude Code refresh token is no longer usable"
  'konix/claude-code-refresh-error)

(defun konix/claude-code--read-credentials ()
  "Read the Claude Code credentials file, or nil when it does not exist.
Returns the whole top level object, so that keys we do not know about
survive a rewrite."
  (let ((file (expand-file-name konix/claude-code--credentials-file)))
    (when (file-exists-p file)
      (let ((json-object-type 'alist)
            (json-array-type 'list))
        (json-read-file file)))))

(defun konix/claude-code--oauth-expiry (oauth)
  "Return when OAUTH expires, as a Unix time in seconds, or nil.
The credentials file stores `expiresAt' in milliseconds."
  (let ((expires-at (alist-get 'expiresAt oauth)))
    (when (numberp expires-at)
      (/ expires-at 1000.0))))

(defun konix/claude-code--token-fresh-p (oauth)
  "Return non-nil when OAUTH holds a token good for a while longer."
  (let ((token (alist-get 'accessToken oauth))
        (expiry (konix/claude-code--oauth-expiry oauth)))
    (and (stringp token)
         (not (string-empty-p token))
         expiry
         (> expiry (+ (float-time) konix/claude-code--refresh-margin)))))

(defun konix/claude-code--write-credentials (credentials)
  "Write CREDENTIALS to the Claude Code credentials file, atomically.
Goes through a temporary file in the same directory, created with mode
600 by `make-temp-file', so that a crash cannot leave a truncated
credentials file behind."
  (let* ((file (expand-file-name konix/claude-code--credentials-file))
         (tmp (make-temp-file (concat file "."))))
    (with-temp-file tmp
      (insert (json-encode credentials)))
    (set-file-modes tmp #o600)
    (rename-file tmp file t)))

(defun konix/claude-code--oauth-refresh-request (oauth)
  "POST a refresh_token grant for OAUTH and return the parsed response.
Signals `konix/claude-code-refresh-rejected' when the server refuses the
refresh token itself, `konix/claude-code-refresh-error' otherwise."
  (let ((body (json-encode
               `((grant_type . "refresh_token")
                 (refresh_token . ,(alist-get 'refreshToken oauth))
                 (client_id . ,konix/claude-code--oauth-client-id)
                 (scope . ,(string-join
                            (or (alist-get 'scopes oauth)
                                konix/claude-code--oauth-default-scopes)
                            " "))))))
    (condition-case err
        (plz 'post konix/claude-code--oauth-token-url
          :headers '(("content-type" . "application/json"))
          :body body
          :as (lambda ()
                (let ((json-object-type 'alist)
                      (json-array-type 'list))
                  (json-read)))
          :timeout 30)
      (plz-error
       (let* ((response (plz-error-response (caddr err)))
              (status (and response (plz-response-status response)))
              (payload (and response (plz-response-body response))))
         (signal (if (and payload (string-match-p "invalid_grant" payload))
                     'konix/claude-code-refresh-rejected
                   'konix/claude-code-refresh-error)
                 (list (format "HTTP %s: %s"
                               (or status "?")
                               (or payload "no body")))))))))

(defun konix/claude-code--refresh-credentials (&optional force)
  "Renew the Claude Code access token using the stored refresh token.
Returns the fresh access token.

Re-reads the credentials file first: the Claude CLI or an agent-shell
subprocess may have renewed it in the meantime, and refresh tokens are
rotated on every use, so racing them would invalidate the file.  Unless
FORCE is non-nil, a token that is still fresh is returned as is."
  (let* ((credentials (konix/claude-code--read-credentials))
         (oauth (alist-get 'claudeAiOauth credentials))
         (refresh-token (alist-get 'refreshToken oauth)))
    (unless credentials
      (signal 'konix/claude-code-refresh-rejected (list "No credentials file")))
    (if (and (not force) (konix/claude-code--token-fresh-p oauth))
        (alist-get 'accessToken oauth)
      (unless (and (stringp refresh-token) (not (string-empty-p refresh-token)))
        (signal 'konix/claude-code-refresh-rejected (list "No refresh token")))
      (let* ((response (konix/claude-code--oauth-refresh-request oauth))
             (access-token (alist-get 'access_token response))
             (expires-in (alist-get 'expires_in response))
             (refresh-expires-in (alist-get 'refresh_token_expires_in response))
             (scope (alist-get 'scope response))
             (now (float-time)))
        (unless (stringp access-token)
          (signal 'konix/claude-code-refresh-error
                  (list "Response carried no access_token")))
        ;; Without a usable expires_in we would store a fresh token behind a
        ;; stale expiry, and re-grant on every single call afterwards.
        (unless (numberp expires-in)
          (signal 'konix/claude-code-refresh-error
                  (list (format "Response carried no usable expires_in: %S"
                                expires-in))))
        (setf (alist-get 'accessToken oauth) access-token)
        (setf (alist-get 'refreshToken oauth)
              (or (alist-get 'refresh_token response) refresh-token))
        (setf (alist-get 'expiresAt oauth) (round (* 1000 (+ now expires-in))))
        (when (numberp refresh-expires-in)
          (setf (alist-get 'refreshTokenExpiresAt oauth)
                (round (* 1000 (+ now refresh-expires-in)))))
        (when (stringp scope)
          (setf (alist-get 'scopes oauth) (split-string scope " " t)))
        (setf (alist-get 'claudeAiOauth credentials) oauth)
        (konix/claude-code--write-credentials credentials)
        (message "Claude Code access token renewed.")
        access-token))))

(defun konix/claude-code--renew-credentials ()
  "Start the interactive Claude Code login flow in a terminal buffer.
Only needed when the refresh token itself is gone or was revoked.  The
flow goes through a browser, so it cannot complete synchronously: this
always signals an error, asking to retry once login is done."
  (when (yes-or-no-p "Claude Code credentials cannot be refreshed.  Log in again? ")
    (pop-to-buffer
     (let ((buffer (make-term "claude-auth" "claude-code" nil "auth" "login")))
       (with-current-buffer buffer
         (term-mode)
         (term-char-mode))
       buffer)))
  (error "Claude Code credentials are expired.  Complete `claude-code auth login', then retry"))

(defun konix/claude-code--ensure-valid-credentials (&optional stale-token)
  "Return a valid Claude Code access token, renewing it when needed.

STALE-TOKEN, when given, is a token the API just rejected.  Any *other*
token already sitting in the credentials file is then preferred over
performing a new grant: the Claude CLI or an agent-shell subprocess may
well have renewed it in the meantime, and the token endpoint rate limits
redundant refreshes."
  (let* ((credentials (konix/claude-code--read-credentials))
         (oauth (alist-get 'claudeAiOauth credentials))
         (token (alist-get 'accessToken oauth)))
    (cond
     ((null credentials)
      (konix/claude-code--renew-credentials))
     ((and stale-token (stringp token) (not (equal token stale-token)))
      token)
     ((and (not stale-token) (konix/claude-code--token-fresh-p oauth))
      token)
     (t
      (condition-case err
          (konix/claude-code--refresh-credentials (and stale-token t))
        (konix/claude-code-refresh-rejected
         (message "Claude Code token refresh rejected: %s"
                  (error-message-string err))
         (konix/claude-code--renew-credentials)))))))

(defun konix/claude-code--format-time-until (unix-timestamp)
  "Format the time remaining until UNIX-TIMESTAMP as a human-readable string."
  (let* ((now (float-time))
         (diff (- unix-timestamp now)))
    (if (<= diff 0)
        "now"
      (let* ((hours (floor (/ diff 3600)))
             (minutes (floor (/ (mod diff 3600) 60))))
        (cond
         ((>= hours 24)
          (format "%dd %dh" (/ hours 24) (mod hours 24)))
         ((> hours 0)
          (format "%dh %dm" hours minutes))
         (t
          (format "%dm" minutes)))))))

(defun konix/claude-code--format-duration (seconds)
  "Format a duration in SECONDS as a human-readable string."
  (let* ((abs-seconds (abs seconds))
         (negative-p (< seconds 0))
         (hours (floor (/ abs-seconds 3600)))
         (minutes (floor (/ (mod abs-seconds 3600) 60)))
         (formatted (cond
                     ((>= hours 24)
                      (format "%dd %dh" (/ hours 24) (mod hours 24)))
                     ((> hours 0)
                      (format "%dh %dm" hours minutes))
                     (t
                      (format "%dm" minutes)))))
    (if negative-p
        (concat "-" formatted)
      formatted)))

(defun konix/claude-code--elapsed-percent (reset-timestamp interval-seconds)
  "Compute the percentage of INTERVAL-SECONDS elapsed.
RESET-TIMESTAMP is the Unix time when the window resets.
INTERVAL-SECONDS is the total window duration (e.g. 18000 for 5h).
Returns a float between 0 and 100."
  (let* ((now (float-time))
         (window-start (- reset-timestamp interval-seconds))
         (elapsed (- now window-start)))
    (min 100.0 (max 0.0 (* 100.0 (/ elapsed (float interval-seconds)))))))

(defvar konix/claude-code--usage-cache nil
  "Cache for `konix/claude-code---usage' result.")
(defvar konix/claude-code--usage-cache-time nil
  "Timestamp for `konix/claude-code---usage' cache.")

(defun konix/claude-code--call-rate-limit-probe (token)
  "POST a 1-token completion and return the `plz-response' struct.
On non-2xx the response is recovered from the signaled `plz-error',
so the caller can inspect status + rate-limit headers uniformly."
  (condition-case err
      (plz 'post "https://api.anthropic.com/v1/messages"
        :headers `(("Authorization" . ,(concat "Bearer " token))
                   ("anthropic-version" . "2023-06-01")
                   ("anthropic-beta" . "oauth-2025-04-20")
                   ("content-type" . "application/json"))
        :body (json-encode
               '((model . "claude-haiku-4-5-20251001")
                 (max_tokens . 1)
                 (messages . [((role . "user") (content . "hi"))])))
        :as 'response :timeout 30)
    (plz-http-error
     (or (plz-error-response (caddr err))
         (signal (car err) (cdr err))))))

;;;###autoload
(defun konix/claude-code---usage ()
  "Get Claude Code API usage information.
Caches the result for 10 minutes.

Makes a minimal API call to retrieve rate limit headers from the Anthropic API.
Returns usage information including:
- 5-hour window utilization percentage
- 7-day window status
- Time until reset"
  (if (and
       (not current-prefix-arg)
       konix/claude-code--usage-cache
       konix/claude-code--usage-cache-time
       (< (- (float-time) konix/claude-code--usage-cache-time) 600))
      konix/claude-code--usage-cache
    (let* ((token (konix/claude-code--ensure-valid-credentials))
           (response (konix/claude-code--call-rate-limit-probe token))
           (response (if (eql (plz-response-status response) 401)
                         (konix/claude-code--call-rate-limit-probe
                          (konix/claude-code--ensure-valid-credentials token))
                       response))
           (headers (plz-response-headers response))
           (util-5h (string-to-number
                     (or (alist-get 'anthropic-ratelimit-unified-5h-utilization
                                    headers)
                         "0")))
           (reset-5h (string-to-number
                      (or (alist-get 'anthropic-ratelimit-unified-5h-reset
                                     headers)
                          "0")))
           (reset-7d (string-to-number
                      (or (alist-get 'anthropic-ratelimit-unified-7d-reset
                                     headers)
                          "0")))
           (util-7d (string-to-number
                     (or (alist-get 'anthropic-ratelimit-unified-7d-utilization
                                    headers)
                         "0")))
           (util-5h-pct (* util-5h 100))
           (util-7d-pct (* util-7d 100))
           (elapsed-5h-pct (konix/claude-code--elapsed-percent reset-5h 18000))
           (elapsed-7d-pct (konix/claude-code--elapsed-percent reset-7d 604800))
           (wait-5h-secs (* (/ (- util-5h-pct elapsed-5h-pct) 100.0) 18000))
           (wait-7d-secs (* (/ (- util-7d-pct elapsed-7d-pct) 100.0) 604800))
           (result
            (json-encode
             `((usage_5h_percent . ,util-5h-pct)
               (elapsed_5h_percent . ,elapsed-5h-pct)
               (wait_5h . ,(konix/claude-code--format-duration wait-5h-secs))
               (wait_5h_secs . ,wait-5h-secs)
               (reset_5h_secs . ,reset-5h)
               (reset_5h . ,(if (< reset-5h 0) "N/A" (konix/claude-code--format-time-until reset-5h)))
               (reset_5h_datetime . ,(if (< reset-5h 0) "N/A" (format-time-string
                                                               "%Y-%m-%d %H:%M" (seconds-to-time reset-5h))))
               (usage_7d_percent . ,util-7d-pct)
               (elapsed_7d_percent . ,elapsed-7d-pct)
               (wait_7d . ,(konix/claude-code--format-duration wait-7d-secs))
               (wait_7d_secs . ,wait-7d-secs)
               (reset_7d_secs . ,reset-7d)
               (reset_7d . ,(if (< reset-7d 0) "N/A" (konix/claude-code--format-time-until reset-7d)))
               (reset_7d_datetime . ,(if (< reset-7d 0) "N/A" (format-time-string
                                                               "%Y-%m-%d %H:%M"
                                                               (seconds-to-time
                                                                reset-7d))))))))
      (setq konix/claude-code--usage-cache result
            konix/claude-code--usage-cache-time (float-time))
      result)))

;;;###autoload
(defun konix/claude-code-usage ()
  "Display Claude Code API usage in the minibuffer.
Interactive command for quick usage check."
  (interactive)
  (condition-case err
      (let* ((json-object-type 'alist)
             (result (json-read-from-string
                      (konix/claude-code---usage)))
             (usage-5h (alist-get 'usage_5h_percent result))
             (elapsed-5h (alist-get 'elapsed_5h_percent result))
             (wait-5h (alist-get 'wait_5h result))
             (reset-5h (alist-get 'reset_5h result))
             (reset-5h-dt (alist-get 'reset_5h_datetime result))
             (usage-7d (alist-get 'usage_7d_percent result))
             (elapsed-7d (alist-get 'elapsed_7d_percent result))
             (wait-7d (alist-get 'wait_7d result))
             (reset-7d (alist-get 'reset_7d result))
             (reset-7d-dt (alist-get 'reset_7d_datetime result)))
        (message "5h:%3d%%/%3d%% w:%-7s -> %-10s @ %s\n7d:%3d%%/%3d%% w:%-7s -> %-10s @ %s"
                 usage-5h elapsed-5h wait-5h reset-5h reset-5h-dt
                 usage-7d elapsed-7d wait-7d reset-7d reset-7d-dt))
    (error (message "Error getting Claude Code usage: %s" (error-message-string err)))))

(defun konix/claude-code-wait-seconds ()
  "Return seconds to wait for Claude Code usage to be ok.
Returns the maximum of the 5-hour and 7-day wait times."
  (condition-case err
      (let* ((json-object-type 'alist)
             (result (json-read-from-string
                      (konix/claude-code---usage)))
             (wait-5h (alist-get 'wait_5h_secs result))
             (wait-7d (alist-get 'wait_7d_secs result)))
        (max wait-5h wait-7d))
    (error (message "Error getting Claude Code usage: %s" (error-message-string err)))))

(provide 'KONIX_claude-code-usage)
;;; KONIX_claude-code-usage.el ends here
