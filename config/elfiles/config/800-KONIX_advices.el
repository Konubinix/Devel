;; ################################################################################
;; I want that when appt shows me the appointment, it still appear for
;; some time but does not directly vanish because if I am not in front
;; of my computer I would have never seen it and it is a possible miss
;; of information. Then I ask it to stay at least 1 day (86400
;; seconds) in my emacs if I don't do anything.
;;
;; The cool thing is that when there is the input that let the message
;; disapear, the character will be correctly handled by emacs and
;; there will not be any lack of character in my text
;; ################################################################################
(defadvice appt-delete-window (before wait-for-confirmation ())
  "Wait an entire day that the user says he has seen the appt"
  (message "Please press anything to say you have seen your appointment")
  (sit-for 86400)
)
(ad-activate 'appt-delete-window)

(defadvice find-tag (before push-mark ())
  (push-mark)
  )
(ad-activate 'find-tag)

(defadvice find-tag (before push-mark ())
  (push-mark)
)
(ad-activate 'find-tag)

(defadvice flyspell-goto-next-error (before push-mark ())
  "Met une marque avant d'aller sur l'erreur prochaine"
  (push-mark)
  )
(ad-activate 'flyspell-goto-next-error)

(defadvice delete-other-windows (around konix/dedicated-windows activate)
  "This is advice to make konix/dedicated-windows avoid being deleted.
Dedicated window can't be deleted by command `delete-other-windows'."
  (let ((current-window (selected-window)))
	(dolist (win (window-list))
	  (when (and (window-live-p win)
				 (not (eq current-window win))
				 (not (eq win konix/dedicated-windows) ))
		(delete-window win)
		)
	  )
	)
  )
(ad-activate 'delete-other-windows)
