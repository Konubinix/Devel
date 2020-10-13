;; ######################################################################
;; Settings that should be done before loading org
;; ######################################################################
(setq-default org-empty-line-terminates-plain-lists t)
(setq-default org-modules
			  '(
				org-id
				org-bbdb
				org-bibtex
				org-info
				org-jsinfo
				org-irc
				org-w3m
				org-wl
				org-babel
				org-checklist
                org-inlinetask
				)
			  )

(defun konix/org-goto-id (id)
  (interactive "sID: ")
  (org-goto-marker-or-bmk (org-id-find id 'marker))
  )

(defcustom konix/org-agenda-text-properties '() "")
