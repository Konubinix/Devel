;;; 700-KONIX_nxml.el ---

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

(defvar konix/nxml-indent-line-functions '()
  "Functions called in a xml file line to perform the indentation.

There are some cases when some semantic into the xml file cause a need to indent
specifically. For instance, some code put in programlisting tags should not be
indented as xml. This is a list given to `run-hook-with-args-until-success'. It
should return t if it has successfully indented the line, else nil. The
functions are tested till one succeed in indenting the line.
"
  )

(provide '700-KONIX_nxml)
;;; 700-KONIX_nxml.el ends here
