;;; KONIX_AL-comint.el ---

;; Copyright (C) 2012  konubinix

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(setq-default comint-process-echoes t)
(setq-default comint-input-ignoredups t)
(defun konix/comint-kill-last-output ()
  (interactive)
  (goto-char (point-max))
  (let (
                (end (point))
                beg
                )
        (comint-previous-prompt 1)
        (setq beg (point))
        (comint-kill-region beg end)
        )
  )

(defun konix/comint-hide-or-delete-before-last-output ()
  (interactive)
  (save-excursion
        (goto-char (point-max))
        (comint-previous-prompt 1)
        (if current-prefix-arg
                (comint-kill-region (point-min) (point))
          (narrow-to-region (point) (point-max)))
        )
  )

(defun konix/comint/send-command-redirect (command handler &optional display)
  "The shell must support echo"
  (let (
                (end_of_output nil)
                (temp_file (make-temp-file "konix_shell_redirection_"))
                callback_output_finished
                (result nil)
                (current-process (get-buffer-process (current-buffer)))
                )
        (unwind-protect
                (progn
                  (setq callback_output_finished
                                (lambda(elt)
                                  (if (string-match-p "^FINISHED" elt)
                                          (setq end_of_output t)
                                        )
                                  )
                                )
                  (comint-skip-input)
                  (comint-send-string
                   current-process
                   (concat command
                                   (if display
                                           "| tee "
                                         "> "
                                         )
                                   temp_file)
                   )
                  (comint-send-input)
                  (add-hook 'comint-output-filter-functions
                                        callback_output_finished)
                  (comint-send-string current-process "echo FINISHED")
                  (comint-send-input)
                  ;; Wait for the FINISHED output
                  (while (not end_of_output)
                        (accept-process-output nil 1)
                        )
                  (remove-hook 'comint-output-filter-functions
                                           callback_output_finished)
                  (with-temp-buffer
                        (insert-file-contents temp_file)
                        (beginning-of-buffer)
                        (setq result (funcall handler))
                        )
                  )
          (delete-file temp_file)
          )
        result
        )
  )

(defun konix/comint-dynamic-complete ()
  (interactive)
  )

(defun konix/comint-dynamic-complete-no-error (&rest args)
  (interactive)
  (and (ignore-errors (konix/comint-dynamic-complete))
           t)
  )

(defun konix/comint-mode-hook()
  (keymap-local-set "C-w" 'comint-kill-region)
  (keymap-local-set "C-c C-w" 'konix/comint-kill-last-output)
  (keymap-local-set "C-c C-M-w" 'konix/comint-hide-or-delete-before-last-output)
  (keymap-local-set "<tab>" 'konix/comint-dynamic-complete)
  )
(add-hook 'comint-mode-hook 'konix/comint-mode-hook)

(setq-default comint-process-echoes t)


(provide '700-KONIX_comint)
;;; KONIX_AL-comint.el ends here
