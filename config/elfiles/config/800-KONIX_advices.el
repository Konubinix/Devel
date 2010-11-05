;; ####################################################################################################
;; Some advices
;; ####################################################################################################
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
