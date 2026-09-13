;;; 300-KONIX_time-helpers.el ---                    -*- lexical-binding: t; -*-
(defun konix/time-range-to-list (range)
  (let* (
		 (start (car range))
		 (current start)
		 (end (cdr range))
		 (res nil)
		 )
	(unless (member current res)
	  (setq res (append res (list current))))
	(while (time-less-p current end)
	  (setq current
			(time-add current (seconds-to-time (* 60 60 24)))
			)
	  (unless (member current res)
		(setq res (append res (list current))))
	  )
	res
	)
  )

(defun konix/flatten-time-ranges (time_ranges)
  (let (
		(res nil)
		)
	(mapc
	 (lambda (time_range)
	   (setq res
			 (append
			  res
			  (konix/time-range-to-list time_range)
			  )
			 )
	   )
	 time_ranges
	 )
	res
	)
  )


(defun konix/time-string-to-hours (timestring)
  (if (string-match "\\(\\([0-9]+\\)d \\)?\\([0-9]+\\):\\([0-9]+\\)" timestring)
	  (let* (
			 (days (string-to-number (or
                                      (match-string-no-properties 2 timestring)
                                      "0"
                                      )))
			 (hours (string-to-number (match-string-no-properties 3 timestring)))
			 (minutes (string-to-number (match-string-no-properties 4 timestring)))
			 (minutes_hour_fraction (/ minutes 60.0))
			 (days_to_hour (* days 24))
			 (new_hour (+ days_to_hour hours minutes_hour_fraction))
			 )
		new_hour
		)
	0
	)
  )
