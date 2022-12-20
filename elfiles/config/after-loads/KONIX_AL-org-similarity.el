;;; KONIX_AL-org-similarity.el ---                   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  konubinix

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

;; directory to scan for possibly similar documents.
;; org-roam users might want to change it to org-roam-directory
(setq org-similarity-directory org-roam-directory)
;; the language passed to nltk's Snowball stemmer
(setq org-similarity-language "english")
;; how many similar entries to list at the end of the buffer
(setq org-similarity-number-of-documents 10)
;; whether to prepend the list entries with their cosine similarity score
(setq org-similarity-show-scores nil)

(provide 'KONIX_AL-org-similarity)
;;; KONIX_AL-org-similarity.el ends here
