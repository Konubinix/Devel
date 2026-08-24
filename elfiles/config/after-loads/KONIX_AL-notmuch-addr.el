;;; KONIX_AL-notmuch-addr.el ---                     -*- lexical-binding: t; -*-

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

;;

;;; Code:

(defun konix/notmuch-addr-query-with-words (orig)
  "Like `notmuch-addr-query-with-words' but use tag:sent instead and use the sender as well."
  (or (and (not current-prefix-arg)
           notmuch-addr--cache)
      (setq notmuch-addr--cache
            (prog2 (message "Collecting email addresses...")
                (process-lines
                 notmuch-command "address" "--format=text" "--format-version=4"
                 "--output=recipients" "--output=sender" "--deduplicate=address"
                 "tag:sent")
              (message "Collecting email addresses...done")))))

(advice-add 'notmuch-addr-query-with-words :around 'konix/notmuch-addr-query-with-words)

(provide 'KONIX_AL-notmuch-addr)
;;; KONIX_AL-notmuch-addr.el ends here
