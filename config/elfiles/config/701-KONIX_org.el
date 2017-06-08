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
				org-habit
				org-w3m
				org-wl
				org-babel
				org-depend
				org-checklist
				)
			  )

(defun konix/org-goto-id (id)
  (interactive "sID: ")
  (org-goto-marker-or-bmk (org-id-find id 'marker))
  )
