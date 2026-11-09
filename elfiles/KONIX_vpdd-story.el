;; [[id:bc735d50-fea8-4910-9f5f-1ef50d8d3847][the story drawer, tangled:1]]
;;; KONIX_vpdd-story.el --- draw a story for a question -*- lexical-binding: t -*-

(defconst konix/vpdd-story-colours
  '(("kept" . "white") ("added" . "lightgreen") ("changed" . "gold"))
  "Colour of a story step, by its mark.")

(defun konix/vpdd-draw-story (steps directory)
  "Draw STEPS, each {step, mark}, in a row with a colour legend in DIRECTORY; return the png."
  (unless (seq-some (lambda (step) (not (equal (alist-get 'mark step) "kept"))) steps)
    (error "This story adds or changes nothing: there is nothing to ask"))
  (make-directory directory t)
  (let* ((base (expand-file-name (format-time-string "story-%Y%m%d%H%M%S%N") directory))
         (puml (concat base ".puml")))
    (with-temp-file puml
      (insert "@startuml\nskinparam defaultFontSize 14\n")
      (seq-do-indexed
       (lambda (step i)
         (insert (format "rectangle \"%d. %s\" as s%d #%s\n" (1+ i)
                         (string-replace "\"" "'" (alist-get 'step step)) i
                         (or (cdr (assoc (alist-get 'mark step) konix/vpdd-story-colours))
                             (error "Mark %s is none of kept, added, changed"
                                    (alist-get 'mark step)))))
         (when (> i 0)
           (insert (format "s%d --> s%d\n" (1- i) i))))
       steps)
      (insert "legend right\n"
              (mapconcat (lambda (colour) (format "|<#%s> %s |" (cdr colour) (car colour)))
                         konix/vpdd-story-colours "\n")
              "\nendlegend\n@enduml\n"))
    (unless (eq 0 (call-process "plantuml" nil nil nil "-tpng" puml))
      (error "plantuml failed on %s" puml))
    (concat base ".png")))

(provide 'KONIX_vpdd-story)
;; the story drawer, tangled:1 ends here
