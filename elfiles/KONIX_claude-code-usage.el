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

(defun konix/claude-code--parse-claude-credentials ()
  "Parse Claude Code credentials from ~/.claude/.credentials.json.
Returns the OAuth access token or nil if not found."
  (let ((creds-file (expand-file-name "~/.claude/.credentials.json")))
    (when (file-exists-p creds-file)
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (creds (json-read-file creds-file)))
        (alist-get 'accessToken (alist-get 'claudeAiOauth creds))))))

(defun konix/claude-code--credentials-expired-p ()
  "Check if the Claude Code OAuth credentials are expired.
Returns non-nil if the credentials file is missing, the token is absent,
or the expiresAt timestamp has passed."
  (let ((creds-file (expand-file-name "~/.claude/.credentials.json")))
    (or (not (file-exists-p creds-file))
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (creds (json-read-file creds-file))
               (oauth (alist-get 'claudeAiOauth creds))
               (token (alist-get 'accessToken oauth))
               (expires-at (alist-get 'expiresAt oauth)))
          (or (null token)
              (string-empty-p token)
              (and expires-at
                   (numberp expires-at)
                   (<= expires-at (float-time))))))))

(defun konix/claude-code--renew-credentials ()
  "Renew Claude Code OAuth credentials by running `claude auth login'.
This launches the Claude CLI authentication flow to obtain fresh tokens.
Uses `call-process' with /dev/null as stdin so the process runs
non-interactively (no TTY prompt that would hang Emacs)."
  (if (yes-or-no-p "Claude Code credentials are expired or missing. Renew them now?")
      (progn
        (message "Running `claude auth login' to renew credentials...")
        (let ((exit-code (call-process "claude-code" "/dev/null" "*claude-auth*" nil "auth" "login")))
          (if (zerop exit-code)
              (progn
                (message "Claude Code credentials renewed successfully.")
                t)
            (display-buffer "*claude-auth*")
            (error "Failed to renew Claude Code credentials (exit code %d). See *claude-auth* buffer" exit-code))))
    (error "Claude Code credentials are expired. Please run `claude auth login' manually")))

(defun konix/claude-code--ensure-valid-credentials ()
  "Ensure Claude Code credentials are valid, renewing if needed.
Returns the access token."
  (when (konix/claude-code--credentials-expired-p)
    (konix/claude-code--renew-credentials))
  (or (konix/claude-code--parse-claude-credentials)
      (error "Claude Code credentials not found in ~/.claude/.credentials.json")))

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
    (let ((result
           (let ((token (konix/claude-code--ensure-valid-credentials)))
             (let* ((url-request-method "POST")
                    (url-request-extra-headers
                     `(("Authorization" . ,(concat "Bearer " token))
                       ("anthropic-version" . "2023-06-01")
                       ("anthropic-beta" . "oauth-2025-04-20")
                       ("content-type" . "application/json")))
                    (url-request-data
                     (json-encode
                      '((model . "claude-haiku-4-5-20251001")
                        (max_tokens . 1)
                        (messages . [((role . "user") (content . "hi"))]))))
                    (response-headers nil)
                    (response-status nil)
                    (buffer (url-retrieve-synchronously
                             "https://api.anthropic.com/v1/messages" t t 30)))
               (unless buffer
                 (error "Failed to connect to Anthropic API"))
               (with-current-buffer buffer
                 ;; Parse status code
                 (goto-char (point-min))
                 (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                   (setq response-status (string-to-number (match-string 1))))
                 ;; Parse headers from the response
                 (goto-char (point-min))
                 (while (re-search-forward "^\\([^:]+\\): \\(.+\\)$" nil t)
                   (push (cons (downcase (match-string 1)) (match-string 2))
                         response-headers))
                 (kill-buffer))
               ;; If we got a 401, credentials are stale — renew and retry once
               (when (eql response-status 401)
                 (konix/claude-code--renew-credentials)
                 (setq token (konix/claude-code--parse-claude-credentials))
                 (let* ((url-request-method "POST")
                        (url-request-extra-headers
                         `(("Authorization" . ,(concat "Bearer " token))
                           ("anthropic-version" . "2023-06-01")
                           ("anthropic-beta" . "oauth-2025-04-20")
                           ("content-type" . "application/json")))
                        (url-request-data
                         (json-encode
                          '((model . "claude-haiku-4-5-20251001")
                            (max_tokens . 1)
                            (messages . [((role . "user") (content . "hi"))]))))
                        (retry-buffer (url-retrieve-synchronously
                                       "https://api.anthropic.com/v1/messages" t t 30)))
                   (unless retry-buffer
                     (error "Failed to connect to Anthropic API on retry"))
                   (setq response-headers nil)
                   (with-current-buffer retry-buffer
                     (goto-char (point-min))
                     (while (re-search-forward "^\\([^:]+\\): \\(.+\\)$" nil t)
                       (push (cons (downcase (match-string 1)) (match-string 2))
                             response-headers))
                     (kill-buffer))))
               (let* ((util-5h (string-to-number
                                (or (alist-get "anthropic-ratelimit-unified-5h-utilization"
                                               response-headers nil nil #'string=)
                                    "0")))
                      (reset-5h (string-to-number
                                 (or (alist-get "anthropic-ratelimit-unified-5h-reset"
                                                response-headers nil nil #'string=)
                                     "0")))
                      (reset-7d (string-to-number
                                 (or (alist-get "anthropic-ratelimit-unified-7d-reset"
                                                response-headers nil nil #'string=)
                                     "0")))
                      (util-7d (string-to-number
                                (or (alist-get "anthropic-ratelimit-unified-7d-utilization"
                                               response-headers nil nil #'string=)
                                    "0")))
                      (util-5h-pct (* util-5h 100))
                      (util-7d-pct (* util-7d 100))
                      (elapsed-5h-pct (konix/claude-code--elapsed-percent reset-5h 18000))
                      (elapsed-7d-pct (konix/claude-code--elapsed-percent reset-7d 604800))
                      (wait-5h-secs (* (/ (- util-5h-pct elapsed-5h-pct) 100.0) 18000))
                      (wait-7d-secs  (* (/ (- util-7d-pct elapsed-7d-pct) 100.0) 604800)))
                 (json-encode
                  `((usage_5h_percent . ,util-5h-pct)
                    (elapsed_5h_percent . ,elapsed-5h-pct)
                    (wait_5h . ,(konix/claude-code--format-duration wait-5h-secs))
                    (wait_5h_secs . ,wait-5h-secs)
                    (reset_5h . ,(if (< reset-5h 0) "N/A" (konix/claude-code--format-time-until reset-5h)))
                    (reset_5h_datetime . ,(if (< reset-5h 0) "N/A" (format-time-string
                                                                    "%Y-%m-%d %H:%M" (seconds-to-time reset-5h))))
                    (usage_7d_percent . ,util-7d-pct)
                    (elapsed_7d_percent . ,elapsed-7d-pct)
                    (wait_7d . ,(konix/claude-code--format-duration wait-7d-secs))
                    (wait_7d_secs . ,wait-7d-secs)
                    (reset_7d . ,(if (< reset-7d 0) "N/A" (konix/claude-code--format-time-until reset-7d)))
                    (reset_7d_datetime . ,(if (< reset-7d 0) "N/A" (format-time-string
                                                                    "%Y-%m-%d %H:%M"
                                                                    (seconds-to-time
                                                                     reset-7d)))))))))))
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
