;;; KONIX_AL-org-clock.el ---

;; Copyright (C) 2013  konubinix

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

(setq-default org-clocktable-defaults
			  (list
			   :maxlevel 2
			   :lang (or (org-bound-and-true-p org-export-default-language) "en")
			   :scope 'file
			   :block nil
			   :wstart 1
			   :mstart 1
			   :tstart nil
			   :tend nil
			   :step nil
			   :stepskip0 nil
			   :fileskip0 nil
			   :tags nil
			   :emphasize nil
			   :link nil
			   :narrow '120!
			   :indent t
			   :formula nil
			   :timestamp nil
			   :level nil
			   :tcolumns nil
			   :formatter nil)
			  )

(setq-default org-clock-frame-title-format '(
                                             "konix_emacs: "
                                             t org-mode-line-string
                                             )
              )

(provide 'KONIX_AL-org-clock)
;;; KONIX_AL-org-clock.el ends here
