;;; 700-KONIX_egg-mode.el ---

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

(defun konix/egg-load-hook()
  ;; Variables
  (setq egg-background-idle-period 30)
  (setq egg-buffer-hide-help-on-start (quote (egg-status-buffer-mode egg-log-buffer-mode egg-file-log-buffer-mode egg-reflog-buffer-mode egg-diff-buffer-mode egg-commit-buffer-mode)))
  (setq egg-buffer-hide-section-type-on-start (quote ((egg-status-buffer-mode . :hunk) (egg-commit-buffer-mode . :hunk) (egg-diff-buffer-mode . :hunk))))
  (setq egg-buffer-hide-sub-blocks-on-start (quote (egg-status-buffer-mode egg-log-buffer-mode egg-file-log-buffer-mode egg-reflog-buffer-mode egg-diff-buffer-mode egg-commit-buffer-mode)))
  (setq egg-confirm-next-action t)
  (setq egg-enable-tooltip t)
  (setq egg-refresh-index-in-backround t)
  (setq egg-show-key-help-in-buffers (quote (:status :log :file-log :reflog :diff :commit)))
  (define-key egg-hide-show-map (kbd "TAB") 'egg-section-cmd-toggle-hide-show)
  (define-key egg-hide-show-map (kbd "S-TAB") 'egg-section-cmd-toggle-hide-show-children)
  ;; Hotkeys
  (define-key egg-hide-show-map (kbd "TAB") 'egg-section-cmd-toggle-hide-show)
  (define-key egg-hide-show-map (kbd "<backtab>") 'egg-section-cmd-toggle-hide-show-children)
  (define-key egg-hunk-section-map (kbd "V") 'konix/egg-hunk-section-cmd-view-file-other-window)
  (define-key egg-status-buffer-mode-map (kbd "l") 'magit-log)
  (define-key egg-file-cmd-map (kbd "l") 'magit-log)
  (define-key egg-buffer-mode-map "q" 'konix/quit-and-delete-window)
  (define-key egg-file-cmd-map "s" 'konix/egg-status)
  (global-set-key  "\M-gu" 'uncomment-region)
  (global-set-key  "\M-gc" 'comment-region)
  )
(add-hook 'egg-load-hook 'konix/egg-load-hook)

(provide '700-KONIX_egg-mode)
;;; 700-KONIX_egg-mode.el ends here
