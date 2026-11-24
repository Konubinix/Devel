;;; KONIX_agent-shell-permissions-curl.el ---  -*- lexical-binding: t; -*-

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

;; The `@web-get(HOST)' evaluator, offered filled in for a waiting Fetch or `curl',
;; and `@curl-post(URL)' for one POSTing inline data to that URL alone.
;; `curl' can send data and write files, so its options are whitelisted:
;; anything unlisted means a prompt.

;;; Code:

(require 'subr-x)
(require 'url-parse)
(require 'KONIX_agent-shell-permissions)

(defconst konix/agent-shell--curl-read-flags
  '("-s" "--silent" "-S" "--show-error" "-f" "--fail" "-L" "--location"
    "-i" "--include" "-I" "--head" "-v" "--verbose" "-g" "--globoff"
    "-N" "--no-buffer" "--compressed")
  "`curl' options without a value that cannot make it send or write anything.")

(defun konix/agent-shell--curl-value-read-p (option value &optional post)
  "Non-nil when `curl' OPTION given VALUE neither sends nor writes outside the project.
With POST, it may also POST data written inline, never a file's.
nil for an OPTION not known to be harmless."
  (and value
       (pcase option
         ((or "-m" "--max-time" "--connect-timeout") t)
         ((or "-w" "--write-out" "-H" "--header" "-A" "--user-agent")
          (not (string-prefix-p "@" value)))
         ((or "-X" "--request")
          (member (upcase value) (if post '("GET" "HEAD" "POST") '("GET" "HEAD"))))
         ((or "-d" "--data" "--data-raw")
          (and post (not (string-prefix-p "@" value))))
         ((or "-o" "--output")
          (or (member value konix/agent-shell--null-devices)
              (konix/agent-shell--path-inside-project-p value)))
         ((or "-D" "--dump-header")
          (or (equal value "-")
              (member value konix/agent-shell--null-devices)
              (konix/agent-shell--path-inside-project-p value))))))

(defun konix/agent-shell--curl-url-p (string)
  "Non-nil when STRING is an http(s) URL naming a host."
  (let ((url (url-generic-parse-url string)))
    (and (member (url-type url) '("http" "https")) (url-host url))))

(defun konix/agent-shell--curl-read-urls (command &optional post)
  "Return the URLs COMMAND node's `curl' only reads from, or nil if it may do more.
With POST, a POST of data written inline counts as reading."
  (let ((tokens (konix/agent-shell--command-unwrapped command))
        urls (sound t) urls-only)
    (when (and (equal (car tokens) "curl") (not (memq nil tokens)))
      (let ((rest (cdr tokens)))
        (while (and sound rest)
          (let ((token (pop rest)))
            (cond
             ((or urls-only (not (string-prefix-p "-" token)))
              (if (konix/agent-shell--curl-url-p token)
                  (push token urls)
                (setq sound nil)))
             ((equal token "--") (setq urls-only t))
             ((member token konix/agent-shell--curl-read-flags))
             ((string-match "\\`\\(--[^=]+\\)=\\(.*\\)\\'" token)
              (setq sound (konix/agent-shell--curl-value-read-p
                           (match-string 1 token) (match-string 2 token) post)))
             ((string-prefix-p "--" token)
              (setq sound (konix/agent-shell--curl-value-read-p token (pop rest) post)))
             (t
              (let ((letters (mapcar (lambda (c) (string ?- c)) (substring token 1))))
                (while (and sound (cdr letters))
                  (setq sound (member (pop letters) konix/agent-shell--curl-read-flags)))
                (when sound
                  (setq sound (or (member (car letters) konix/agent-shell--curl-read-flags)
                                  (konix/agent-shell--curl-value-read-p
                                   (car letters) (pop rest) post)))))))))
        (and sound urls (delete-dups (nreverse urls)))))))

(defun konix/agent-shell--curl-hosts (urls)
  "Return the hosts URLS name."
  (delete-dups (mapcar (lambda (url) (url-host (url-generic-parse-url url))) urls)))

(defun konix/agent-shell--curl-url-under-p (url target)
  "Non-nil when URL is TARGET, or has its scheme, user, host and port and a path below it."
  (let* ((parsed (url-generic-parse-url url))
         (base (url-generic-parse-url target))
         (path (or (car (url-path-and-query parsed)) ""))
         (prefix (string-remove-suffix "/" (or (car (url-path-and-query base)) ""))))
    (or (equal url target)
        (and (equal (url-type parsed) (url-type base))
             (equal (url-user parsed) (url-user base))
             (equal (url-host parsed) (url-host base))
             (equal (url-port parsed) (url-port base))
             (null (cdr (url-path-and-query base)))
             (not (string-match-p "/\\.\\.?\\(?:/\\|\\'\\)\\|%2[eE]" path))
             (or (equal path prefix) (string-prefix-p (concat prefix "/") path))))))

(defun konix/agent-shell--fetch-url (tool-call)
  "Return the http(s) URL TOOL-CALL fetches, when it is a Fetch, else nil."
  (when (equal (map-elt tool-call :kind) "fetch")
    (let* ((input (map-elt tool-call :raw-input))
           (url (or (map-elt input 'url) (map-elt input "url" nil #'equal))))
      (and (stringp url) (konix/agent-shell--curl-url-p url) url))))

(defun konix/agent-shell--urls-within-p (urls target)
  "Non-nil when all URLS lie under TARGET, a URL, or on host TARGET."
  (if (konix/agent-shell--curl-url-p target)
      (seq-every-p (lambda (url) (konix/agent-shell--curl-url-under-p url target)) urls)
    (equal (konix/agent-shell--curl-hosts urls) (list target))))

(konix/agent-shell-define-tool-evaluator "web-get" (tool-call target)
  "Match a Fetch, or a line whose only command is a `curl', reading under TARGET.
TARGET is a URL, or a host.  The `curl' sends nothing, writes only inside the
project and reads no file outside it."
  (let ((target (string-trim target)))
    (if-let* ((url (konix/agent-shell--fetch-url tool-call)))
        (konix/agent-shell--urls-within-p (list url) target)
      (konix/agent-shell--with-bash-ast root tool-call
        (let ((commands (konix/agent-shell--command-nodes root)))
          (and (= (length commands) 1)
               (when-let* ((urls (konix/agent-shell--curl-read-urls (car commands))))
                 (konix/agent-shell--urls-within-p urls target))
               (konix/agent-shell--reads-inside-p root default-directory)))))))

(konix/agent-shell-define-tool-evaluator "curl-post" (tool-call target)
  "Match a line whose only command is a `curl' at TARGET, a URL, alone.
It may POST data written inline, writes only inside the project and reads no
file outside it.  TARGET may well take writes too: granting it trusts them."
  (konix/agent-shell--with-bash-ast root tool-call
    (let ((commands (konix/agent-shell--command-nodes root)))
      (and (= (length commands) 1)
           (equal (konix/agent-shell--curl-read-urls (car commands) t)
                  (list (string-trim target)))
           (konix/agent-shell--reads-inside-p root default-directory)))))

(konix/agent-shell-define-tool-evaluator "curl-several-hosts" (tool-call)
  "Match a line whose `curl' names more than one host, whatever its options."
  (konix/agent-shell--with-bash-ast root tool-call
    (seq-some
     (lambda (command)
       (let ((tokens (konix/agent-shell--command-unwrapped command)))
         (and (equal (car tokens) "curl")
              (cdr (konix/agent-shell--curl-hosts
                    (seq-filter (lambda (token)
                                  (and (stringp token)
                                       (konix/agent-shell--curl-url-p token)))
                                (cdr tokens)))))))
     (konix/agent-shell--command-nodes root))))

(defun konix/agent-shell--curl-candidates (tool-call)
  "Return the `@web-get' keys filled with TOOL-CALL's URL then host, or nil.
A Fetch gets them as a `curl' does.  A `curl' that POSTs inline data to one
URL gets that URL's `@curl-post'."
  (if-let* ((url (konix/agent-shell--fetch-url tool-call)))
      (mapcar (lambda (target) (format "@web-get(%s)" target))
              (cons url (konix/agent-shell--curl-hosts (list url))))
  (konix/agent-shell--with-bash-ast root tool-call
    (let ((commands (konix/agent-shell--command-nodes root)))
      (when (= (length commands) 1)
        (if-let* ((urls (konix/agent-shell--curl-read-urls (car commands))))
            (when-let* ((hosts (konix/agent-shell--curl-hosts urls))
                        ((null (cdr hosts))))
              (mapcar (lambda (target) (format "@web-get(%s)" target))
                      (append (unless (cdr urls) urls) hosts)))
          (when-let* ((urls (konix/agent-shell--curl-read-urls (car commands) t))
                      ((null (cdr urls))))
            (list (format "@curl-post(%s)" (car urls))))))))))

(add-to-list 'konix/agent-shell-tool-candidate-functions
             #'konix/agent-shell--curl-candidates)

(provide 'KONIX_agent-shell-permissions-curl)
;;; KONIX_agent-shell-permissions-curl.el ends here
