;;; KONIX_AL-helm-files.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2015  konubinix

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

(define-key helm-find-files-map (kbd "M-<left>") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "<backtab>") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "M-p") 'helm-ff-file-name-history)
(define-key helm-find-files-map (kbd "C-c h") 'helm-ff-run-switch-to-history)
(define-key helm-find-files-map (kbd "<left>") 'left-char)
(define-key helm-find-files-map (kbd "<right>") 'right-char)

(define-key helm-map (kbd "<backtab>") 'helm-find-files-up-one-level)

(setq-default helm-ff-search-library-in-sexp t
			  helm-ff-file-name-history-use-recentf t
			  helm-ff-lynx-style-map nil
			  helm-ff-auto-update-initial-value nil
			  helm-ff-fuzzy-matching nil
			  helm-ff-guess-ffap-filenames t
			  helm-ff-guess-ffap-urls t
			  helm-ff-history-max-length 3000
			  helm-ff-skip-boring-files t
			  helm-file-cache-fuzzy-match nil
			  )

(setq helm-source--ff-file-name-history
	  (helm-build-sync-source "File name history"
		:init (lambda ()
				(with-helm-alive-p
				  (require 'recentf)
				  (or recentf-mode (recentf-mode 1))))
		:candidates (lambda ()
					  (append file-name-history recentf-list))
		:fuzzy-match nil
		:persistent-action 'ignore
		:migemo t
		:filtered-candidate-transformer 'helm-file-name-history-transformer
		:action (helm-make-actions
				 "Find file" (lambda (candidate)
							   (helm-set-pattern
								(expand-file-name candidate))
							   (with-helm-after-update-hook (helm-exit-minibuffer)))
				 "Find file in helm" (lambda (candidate)
									   (helm-set-pattern
										(expand-file-name candidate))))))

(provide 'KONIX_AL-helm-files)
;;; KONIX_AL-helm-files.el ends here
