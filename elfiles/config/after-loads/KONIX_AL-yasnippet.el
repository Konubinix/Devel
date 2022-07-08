;;; KONIX_AL-yasnippet.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sam

;; Author: sam <sam@konixwork>
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

(yas-global-mode 1)
(setq-default
 yas-snippet-dirs
 (apply
  'append
  (mapcar
   (lambda (dir)
     (list
      (expand-file-name "snippets" dir)
      (expand-file-name "yasnippet/snippets" dir)
      )
     )
   (list
    home-elfiles
    perso-host-elfiles
    perso-elfiles
    elfiles)
   )
  )
 )

(mapc '(lambda(elt) (if (not (file-exists-p elt)) (make-directory elt t))) yas-snippet-dirs)
(mapc 'yas-load-directory yas-snippet-dirs)
(setq-default yas-fallback-behavior 'call-other-command)

(provide 'KONIX_AL-yasnippet)
;;; KONIX_AL-yasnippet.el ends here
