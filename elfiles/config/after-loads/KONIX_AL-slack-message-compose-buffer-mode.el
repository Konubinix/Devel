;;; KONIX_AL-slack-message-compose-buffer-mode.el ---   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  konubinix

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

(defun konix/slack-message-compose-buffer-mode-hook ()
  (visual-line-mode)
  (defvar electric-pair-pairs)
  (setq-local electric-pair-pairs
              (append
               '(
                 (?\` . ?\`)
                 )
               electric-pair-pairs)
              )
  (abbrev-mode 1)
  )

(add-hook 'slack-message-compose-buffer-mode-hook
          'konix/slack-message-compose-buffer-mode-hook)


(provide 'KONIX_AL-slack-message-compose-buffer-mode)
;;; KONIX_AL-slack-message-compose-buffer-mode.el ends here
