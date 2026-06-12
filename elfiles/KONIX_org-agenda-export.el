;;; KONIX_org-agenda-export.el ---

;; Copyright (C) 2014  konubinix

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

(defun konix/org-agenda-get-start-time (&optional dateprop)
  (setq dateprop (or dateprop 'date))
  (let (
        (time-of-day (org-get-at-bol 'time-of-day))
        (date (org-get-at-bol dateprop))
        )
    (unless (listp date)
      (setq date (calendar-gregorian-from-absolute date))
      )
    (encode-time
     0
     (if time-of-day (% time-of-day 100) 0)
     (if time-of-day (/ time-of-day 100) 0)
     (second date)
     (first date)
     (third date)
     )
    )
  )

(defun konix/org-agenda-get-end-time nil
  (let* (
         (start-time (konix/org-agenda-get-start-time))
         (duration (org-get-at-bol 'duration))
         (start-seconds (time-to-seconds start-time))
         (end-seconds (+ start-seconds (* duration 60)))
         (end-time (seconds-to-time end-seconds))
         )
    end-time
    )
  )

(setq konix/org-agenda-export-description-cache (make-hash-table))

(defun konix/org-agenda-export-description nil
  (replace-regexp-in-string "
" "\\\\n"
(konix/org-with-point-on-heading
 (let (
       (org-export-with-toc nil)
       (org-export-babel-evaluate nil)
       (outline (org-format-outline-path (org-get-outline-path)))
       content
       (key (intern (format "%s-%s" (buffer-file-name) (point))))
       )
   (if-let (
            (content (gethash key konix/org-agenda-export-description-cache))
            )
       (progn
         (message "Getting %s from cache" key)
         content
         )
     (save-excursion
       (org-back-to-heading)
       (org-end-of-meta-data)
       (let ((content (buffer-substring-no-properties (point)
                                                      (org-entry-end-position))))
         (with-temp-buffer
           ;; use another buffer so that the changes made won't impact my files
           (insert content)
           (org-mode)
           (konix/org-roam-export/unify-ipfs-links)
           (konix/org-roam-export/convert-standalone-links)
           (konix/org-roam-export/convert-remaining-ipfs-links)
           (org-ascii-export-as-ascii nil nil t t)
           )
         )
       )
     (with-current-buffer "*Org ASCII Export*"
       (setq content (buffer-substring-no-properties (point-min) (point-max)))
       )
     ;; (org-md-export-as-markdown nil t)
     (puthash key (concat outline "

" content) konix/org-agenda-export-description-cache ))
   )
 )
)
  )


(defun konix/org-agenda-to-ics/format-item (type)
  (let* (
         (res "")
         (date (org-get-at-bol 'date))
         (day (org-get-at-bol 'day))
         (rowtype (org-get-at-bol 'type))
         (summary (if (string-equal rowtype "sexp")
                      (org-link-display-format
                       (substring-no-properties (org-get-at-bol 'txt))
                       )
                    (konix/org-with-point-on-heading
                     (konix/org-trim-active-timestamp
                      (konix/org-trim-link
                       (org-get-heading t t t t)
                       )
                      )
                     )
                    ))
         (duration (org-get-at-bol 'duration))
         (stamp (format-time-string "%Y%m%dT%H%M%SZ"))
         (id (konix/org-get-id))
         (categories (mapconcat 'identity (konix/org-get-tags) ","))
         (description (konix/org-with-point-on-heading
                       (format
                        "%s\\\\n%s:%s"
                        (konix/org-agenda-export-description)
                        (current-buffer)
                        (line-number-at-pos (point))
                        )
                       )
                      )
         (location
          (konix/org-with-point-on-heading
           (or (org-entry-get (point) "LOCATION") nil)
           )
          )
         (warn-time (konix/org-with-point-on-heading
                     (string-to-number (or (org-entry-get (point) "APPT_WARNTIME") "30"))
                     ))
         (extra (or (org-get-at-bol 'extra) ""))
         (deadline
          (cond
           ((string-match-p ".+ago" extra)
            (konix/org-with-point-on-heading (org-get-deadline-time (point)))
            )
           ((string-match-p "Deadline" extra)
            (konix/org-agenda-get-start-time 'day)
            )
           (t
            nil
            )
           )
          )
         )
    (when (string-prefix-p "deadline" (org-get-at-bol 'type))
      (error "Nope")
      (setq summary (concat "DL: " summary))
      )
    (when (and (not (null date)) (not (null day)))
      (unless (listp date)
        ;; when the date is an integer, the real date is in the day
        ;; property. Strange...
        (setq date (calendar-gregorian-from-absolute day))
        )
      )
    (setq res (concat res "BEGIN:V" type))
    (when deadline
      (setq res (concat res "
DUE" (if duration
         (format-time-string ":%Y%m%dT%H%M%S" deadline)
       (format-time-string ";VALUE=DATE:%Y%m%d" deadline)
       )
)
            )
      )
    (setq res (concat res
                      (format "
SUMMARY:%s
CATEGORIES:%s
DESCRIPTION:%s%s
DTSTAMP:%s
BEGIN:VALARM
ACTION:DISPLAY
DESCRIPTION:%s
TRIGGER:-PT%sM
END:VALARM"
                              summary
                              categories
                              description
                              (if location (format "
LOCATION:%s" location) "")
                              stamp
                              summary
                              warn-time
                              )
                      )
          )
    (setq res
          (concat res
                  (if (null (org-get-at-bol 'time-of-day))
                      (if (null date)
                          (format "
UID:%s_%s_%s
"
                                  id
                                  (sha1 summary)
                                  type
                                  )
                        (format "
DTSTART;VALUE=DATE:%02d%02d%02d
UID:%s_%s_%02d%02d%02d_%s
"
                                (third date)
                                (first date)
                                (second date)
                                id
                                (sha1 summary)
                                (third date)
                                (first date)
                                (second date)
                                type
                                )
                        )
                    (let (
                          (start-time (konix/org-agenda-get-start-time))
                          (end-time (konix/org-agenda-get-end-time))
                          )
                      (format "
DTSTART:%s
DTEND:%s
UID:%s_%s_%s
"
                              (format-time-string "%Y%m%dT%H%M00" start-time)
                              (format-time-string "%Y%m%dT%H%M00" end-time)
                              id
                              (format-time-string "%Y%m%dT%H%M00" start-time)
                              type
                              )
                      )
                    )
                  )
          )
    (setq res (concat res "END:V" type "
"))
    )
  )

(defun konix/org-agenda-to-ics ()
  (goto-char (point-min))
  (save-excursion
    (let (
          (res "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//konubinix//Emacs with Org mode//EN
CALSCALE:GREGORIAN
X-WR-CALDESC:
X-WR-CALNAME:OrgMode
X-WR-TIMEZONE:CEST
")
          )
      (while (konix/org-agenda-next-entry-1)
        (when (and
               (not
                (-any
                 (lambda (tag)
                   (member tag konix/org-icalendar-exclude-tags)
                   )
                 (org-get-at-bol 'tags)
                 )
                )
               (not (string-match-p ".+ago" (or (org-get-at-bol 'extra) "")))
               )
          (when (or
                 ;; (string-prefix-p "deadline" (org-get-at-bol 'type))
                 (string-prefix-p "tag" (org-get-at-bol 'type))
                 )
            (setq res (concat res (konix/org-agenda-to-ics/format-item "TODO")))
            )
          (when (or
                 ;; (string-prefix-p "deadline" (org-get-at-bol 'type))
                 (string-prefix-p "timestamp" (org-get-at-bol 'type))
                 ;; not sure what "block" means, but a simple * <date> is a block
                 (string-prefix-p "block" (org-get-at-bol 'type))
                 (string-prefix-p "sexp" (org-get-at-bol 'type))
                 )
            (setq res (concat res (konix/org-agenda-to-ics/format-item "EVENT")))
            )
          )
        )
      (setq res (concat res "END:VCALENDAR
          ")
            )
      res
      )
    )
  )

(defun konix/org-agenda-format-item/trim-active-timestamp (orig-fun extra txt &rest args)
  (apply orig-fun extra (konix/org-trim-active-timestamp txt) args)
  )

(provide 'KONIX_org-agenda-export)
;;; KONIX_org-agenda-export.el ends here
