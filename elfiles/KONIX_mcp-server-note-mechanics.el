;; [[id:6872a682-fc48-4e82-bbcd-6d4055a55f77::-*- lexical-binding: t; -*-][-*- lexical-binding: t; -*-]]
;;; KONIX_mcp-server-note-mechanics.el --- tangled from how_to_write_and_audit_a_note.org
(defconst konix/note-intention-words
  '(("tl;dr"      . "what the note says, in one line")
    ("authorship" . "who wrote this note")
    ("what"       . "the thing the bullet names or defines")
    ("why"        . "the user's reason for what precedes")
    ("therefore"  . "the conclusion that follows from what precedes")
    ("how"        . "the way it is done, the mechanism")
    ("who"        . "the party the bullet is about")
    ("rule"       . "what has to hold")
    ("scope"      . "where the rule above reaches, and where it stops")
    ("example"    . "one instance of what precedes"))
  "The intention words a note may use, each with what it does.")
(defun konix/mcp-server--visible-length (line &optional markdown)
  "Length of LINE as it renders — an org link counts its description, or its target
where it has none.  With MARKDOWN, a =[text](target)= counts its text alone, the
target being an =href= the reader is never shown."
  (let ((s (replace-regexp-in-string
            "\\[\\[\\([^]]*?\\)\\]\\[\\([^]]*?\\)\\]\\]" "\\2"
            (replace-regexp-in-string "\\[\\[\\([^]]*?\\)\\]\\]" "\\1" line))))
    (length (if markdown
                (replace-regexp-in-string
                 "\\[\\([^]]*?\\)\\](\\([^)]*?\\))" "\\1" s)
              s))))

(defun konix/mcp-server--rendered-link-ranges (tree)
  "Every span in TREE where a =[text](target)= is a link whose target no reader
sees — an argdown block org exports as a map, or not at all, rather than as its
own source.  Org is asked what it will export, since a header argument reaches a
block from a =#+HEADER:= line, a property or a default as well as from its own."
  (org-element-map tree 'src-block
    (lambda (el)
      (when (equal (org-element-property :language el) "argdown")
        (let ((exports (save-excursion
                         (goto-char (org-element-property :post-affiliated el))
                         (cdr (assq :exports
                                    (nth 2 (org-babel-get-src-block-info t)))))))
          (when (member exports '("results" "none"))
            (cons (org-element-property :begin el)
                  (org-element-property :end el))))))))

(defun konix/mcp-server--own-form-p (el)
  "Non-nil where EL is no stray body line — it is either a bullet's own text, whose
parent is the item itself at any nesting, or it sits in a quote block or a footnote
definition, both of which keep their own form.  Those two are looked for at any
depth, a quote block standing between EL and its bullet, and a definition holding
blocks that hold EL."
  (or (eq (org-element-type (org-element-property :parent el)) 'item)
      (let ((p (org-element-property :parent el)))
        (while (and p (not (memq (org-element-type p)
                                 '(quote-block footnote-definition))))
          (setq p (org-element-property :parent p)))
        (and p t))))

(defun konix/mcp-server--bullet-length (item)
  "How long ITEM reads — its « intention word », its =::= and its own text, wrapping
folded back into one run.  A nested bullet is a bullet of its own and counts there,
a quote block keeps its own form and is not the bullet's to answer for, and an org
link counts as it renders."
  (let* ((own (seq-remove (lambda (c)
                            (memq (org-element-type c)
                                  '(plain-list table quote-block src-block
                                    example-block export-block fixed-width)))
                          (org-element-contents item)))
         (tag (org-element-property :tag item))
         (say (lambda (d) (org-no-properties (org-element-interpret-data d))))
         (text (concat (when tag (concat (funcall say tag) " :: "))
                       (mapconcat say own ""))))
    (konix/mcp-server--visible-length
     (string-trim (replace-regexp-in-string "[ \t\n]+" " " text)))))

(defun konix/mcp-server--no-antecedent-p (item first-heading)
  "Non-nil where ITEM has nothing at all before it to refer back to.
A heading counts as an antecedent for the bullets under it, and so does an
enclosing bullet or an earlier sibling, so only a bullet opening a top-level
list above FIRST-HEADING is left with nothing."
  (let* ((lst (org-element-property :parent item))
         (opens-its-list (eq item (car (org-element-contents lst))))
         (nested (eq (org-element-type (org-element-property :parent lst)) 'item)))
    (and opens-its-list
         (not nested)
         (or (null first-heading)
             (< (org-element-property :begin item) first-heading)))))

(defun konix/mcp-server--note-intentions ()
  "Every « intention word » this note may use — the vocabulary, plus the words its
own =#+INTENTION_WORDS:= lines add to it."
  (append (mapcar #'car konix/note-intention-words)
          (mapcan (lambda (v) (split-string v "[ \t,]+" t))
                  (cdr (assoc "INTENTION_WORDS"
                              (org-collect-keywords '("INTENTION_WORDS")))))))

(defun konix/mcp-server--catalog-note-p ()
  "Non-nil where this note's keywords carry =#+MODE: catalog= — its structure lives in
its tags, so the depth rule has nothing to guard."
  (and (member "catalog"
               (mapcan (lambda (v) (split-string v "[ \t,]+" t))
                       (cdr (assoc "MODE" (org-collect-keywords '("MODE"))))))
       t))

(defun konix/mcp-server--babel-result-ranges ()
  "Every babel result in the buffer, as a list of (BEG . END) — the lines babel wrote.
Found by the =#+RESULTS:= keyword rather than by walking the source blocks, since a
=#+CALL:= line writes one too and has no block of its own to walk from."
  (let (ranges)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-result-regexp nil t)
        (let ((beg (match-beginning 0)) end)
          (goto-char beg)
          (forward-line 1)
          (setq end (org-babel-result-end))
          (push (cons beg end) ranges)
          (goto-char (max end (1+ beg))))))
    ranges))

(defun konix/mcp-server--skip-ranges (tree)
  "Every span in TREE whose lines are not the writer's prose — a source block, an
export block, a babel result, a footnote definition, a =:noexport:= subtree — as a
list of (BEG . END)."
  (append (konix/mcp-server--babel-result-ranges)
          (org-element-map tree '(src-block export-block footnote-definition)
            (lambda (el) (cons (org-element-property :begin el)
                               (org-element-property :end el))))
          (org-element-map tree 'headline
            (lambda (hl)
              (when (member "noexport" (org-element-property :tags hl))
                (cons (org-element-property :begin hl)
                      (org-element-property :end hl)))))))

(defun konix/mcp-server--in-ranges-p (pos ranges)
  "Non-nil where POS falls inside one of RANGES."
  (catch 'found
    (dolist (r ranges)
      (when (and (>= pos (car r)) (< pos (cdr r))) (throw 'found t)))
    nil))

(defun konix/mcp-server--body-lines (beg end ranges)
  "How many lines between BEG and END a reader wades through — not blank, not a
heading, not a drawer or planning line, not a comment, and not inside RANGES.
A nested bullet counts here though it does not count as a sibling: the two counts
ask different questions, one how many things sit side by side, whose fix is nesting,
and one how much text a reader wades through, which nesting does not shorten."
  (let ((n 0))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (unless (or (string-match-p "\\`[ \t]*\\'" line)
                      (string-match-p
                       (concat "\\`[ \t]*\\(\\*+ \\|:[A-Za-z_]+:\\|CLOSED:"
                               "\\|SCHEDULED:\\|DEADLINE:\\|#\\([^+]\\|\\'\\)\\)")
                       line)
                      (konix/mcp-server--in-ranges-p
                       (line-beginning-position) ranges))
            (setq n (1+ n))))
        (forward-line 1)))
    n))

(defun konix/mcp-server--search-hit-p (search &optional avoid-pos)
  "Non-nil where SEARCH finds its target in this buffer, by org's own semantics.
AVOID-POS is a position whose neighbourhood does not count as a match — a fuzzy
link's own text.  `org-link-search-must-match-exact-headline' is bound so org
signals instead of offering to create a heading, which also matches what export
resolves."
  (save-excursion
    (condition-case nil
        (let ((org-link-search-must-match-exact-headline t))
          (org-link-search search avoid-pos t)
          t)
      (error nil))))

(defun konix/mcp-server--file-search-hit-p (file search)
  "Non-nil where SEARCH finds its target in FILE, opening no buffer for it."
  (let ((buf (get-file-buffer file)))
    (if buf
        (with-current-buffer buf (konix/mcp-server--search-hit-p search))
      (with-temp-buffer
        (insert-file-contents file)
        (delay-mode-hooks (org-mode))
        (konix/mcp-server--search-hit-p search)))))

(defun konix/mcp-server--link-scheme (s)
  "The =scheme:= opening S, or nil where it has none."
  (and (string-match "\\`\\([a-z][a-z0-9+._-]*\\):" s) (match-string 1 s)))

(defun konix/mcp-server--link-flaw (type path search &optional avoid-pos)
  "nil where a link resolves, else the flaw: `broken' an internal reference with no
target, `no-file' a file link whose file is missing, `no-place' a file link whose
file exists and whose search option misses, `unknown' a type org has no entry for."
  (pcase type
    ((or "fuzzy" "custom-id" "radio")
     (unless (konix/mcp-server--search-hit-p
              (if (string= type "custom-id") (concat "#" path) path)
              avoid-pos)
       (let ((scheme (and (string= type "fuzzy")
                          (konix/mcp-server--link-scheme path))))
         (if (and scheme (not (assoc scheme org-link-parameters)))
             'unknown
           'broken))))
    ("id" (unless (org-id-find path) 'broken))
    ("coderef" nil)
    ("file"
     (let ((f (expand-file-name path (file-name-directory
                                      (or (buffer-file-name) default-directory)))))
       (cond ((not (file-exists-p f)) 'no-file)
             ((and search (not (konix/mcp-server--file-search-hit-p f search)))
              'no-place))))
    (_ (unless (assoc type org-link-parameters) 'unknown))))

(defun konix/mcp-server--exempt-line-p (pos ranges)
  "Non-nil where the 120-character rule does not reach the line at POS — a keyword
line or a table row, neither of which can be wrapped, or a line babel wrote."
  (or (save-excursion (goto-char pos) (looking-at-p "[ \t]*\\(#\\+\\||\\)"))
      (catch 'found
        (dolist (r ranges)
          (when (and (>= pos (car r)) (< pos (cdr r))) (throw 'found t)))
        nil)))

(defun konix/mcp-server-note-mechanics (buffer-name)
  "Report the mechanical flaws of an org note, and count each intention word in use.
See the note this is tangled from for what a checker may decide.

MCP Parameters:
  buffer-name - Name of the org-mode buffer to check."
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (unless (derived-mode-p 'org-mode)
       (error "Buffer %s is not in org-mode" buffer-name))
     (konix/mcp-server-assert-buffer-fresh buffer-name)
     (save-restriction
       (widen)
       (let* ((tree (org-element-parse-buffer))
              (skip (konix/mcp-server--skip-ranges tree))
              (allowed (konix/mcp-server--note-intentions))
              (catalog (konix/mcp-server--catalog-note-p))
              (first-heading (org-element-map tree 'headline
                               (lambda (h) (org-element-property :begin h)) nil t))
              (toc-kw (org-collect-keywords '("TOC" "OPTIONS")))
              (has-toc (or (cdr (assoc "TOC" toc-kw))
                           (seq-some (lambda (v) (string-match-p "toc:t" v))
                                     (cdr (assoc "OPTIONS" toc-kw)))))
              (heading-count
               (length (org-element-map tree 'headline
                         (lambda (hl)
                           (unless (or (member "noexport" (org-element-property :tags hl))
                                       (equal (org-element-property :raw-value hl)
                                              (or org-footnote-section "Footnotes")))
                             hl)))))
              not-a-bullet no-tag long-tag unknown-tag long-line long-bullet bad-link
              deep inline-fn undef-fn stray-top orphan
              (seen (make-hash-table :test #'equal)))
         (org-element-map tree '(paragraph item)
           (lambda (el)
             (let ((line (line-number-at-pos (org-element-property :post-affiliated el)))
                   (tag (org-element-property :tag el)))
               (pcase (org-element-type el)
                 ('paragraph
                  (unless (konix/mcp-server--own-form-p el)
                    (push line not-a-bullet)))
                 ('item
                  (let ((n (konix/mcp-server--bullet-length el)))
                    (when (> n 300) (push (cons line n) long-bullet)))
                  (if (null tag)
                      (push line no-tag)
                    (let ((word (string-trim (org-no-properties
                                              (org-element-interpret-data tag)))))
                      (if (string-match-p "\\`[^[:space:]]+\\'" word)
                          (progn
                            (puthash word (1+ (gethash word seen 0)) seen)
                            (when (and (member word '("tl;dr" "authorship"))
                                       first-heading
                                       (> (org-element-property :begin el) first-heading))
                              (push (cons line word) stray-top))
                            (when (and (member word '("why" "therefore"))
                                       (konix/mcp-server--no-antecedent-p el first-heading))
                              (push (cons line word) orphan))
                            (unless (member word allowed)
                              (push (cons line word) unknown-tag)))
                        (push (cons line word) long-tag)))))))))
         (let ((ranges (konix/mcp-server--babel-result-ranges))
               (rendered (konix/mcp-server--rendered-link-ranges tree)))
           (save-excursion
             (goto-char (point-min))
             (while (not (eobp))
               (let* ((bol (line-beginning-position))
                      (vis (konix/mcp-server--visible-length
                            (buffer-substring-no-properties bol (line-end-position))
                            (konix/mcp-server--in-ranges-p bol rendered))))
                 (when (and (>= vis 120)
                            (not (konix/mcp-server--exempt-line-p bol ranges)))
                   (push (cons (line-number-at-pos) vis) long-line)))
               (forward-line 1))))
         (org-element-map tree 'headline
           (lambda (hl)
             (let* ((subs (org-element-map (org-element-contents hl) 'headline
                            #'identity nil nil 'headline))
                    (bullets (apply #'+ (or (org-element-map (org-element-contents hl)
                                                'plain-list
                                              (lambda (pl)
                                                (length (org-element-contents pl)))
                                              nil nil '(headline item))
                                            '(0))))
                    (beg (org-element-property :begin hl))
                    (end (if (car subs)
                             (org-element-property :begin (car subs))
                           (org-element-property :end hl)))
                    (lines (konix/mcp-server--body-lines beg end skip))
                    (line (line-number-at-pos beg)))
               (unless catalog
                 (when (> (length subs) 7) (push (list line 'subs (length subs)) deep))
                 (when (> bullets 7) (push (list line 'bullets bullets) deep)))
               (when (> lines 50) (push (list line 'lines lines) deep)))))
         (org-element-map tree '(link keyword)
           (lambda (el)
             (pcase (org-element-type el)
               ('link
                (let ((flaw (konix/mcp-server--link-flaw
                             (org-element-property :type el)
                             (org-element-property :path el)
                             (org-element-property :search-option el)
                             (org-element-property :begin el))))
                  (when flaw
                    (push (list (line-number-at-pos (org-element-property :begin el))
                                flaw (org-element-property :raw-link el) nil)
                          bad-link))))
               ('keyword
                (let ((transclude (equal (org-element-property :key el) "TRANSCLUDE"))
                      (v (or (org-element-property :value el) ""))
                      (pos 0))
                  (while (string-match "\\[\\[\\([^]]+?\\)\\]" v pos)
                    (setq pos (match-end 0))
                    (let* ((raw (match-string 1 v))
                           (parts (split-string raw "::"))
                           (target (car parts))
                           (search (cadr parts))
                           (scheme (konix/mcp-server--link-scheme target))
                           (type (cond (scheme scheme)
                                       ((string-prefix-p "#" target) "custom-id")
                                       (t "fuzzy")))
                           (p (if scheme
                                  (substring target (1+ (length scheme)))
                                target))
                           (flaw (konix/mcp-server--link-flaw type p search)))
                      (when flaw
                        (push (list (line-number-at-pos
                                     (org-element-property :begin el))
                                    flaw raw transclude)
                              bad-link)))))))))
         (let ((defined
                (append
                 (org-element-map tree 'footnote-definition
                   (lambda (fd) (org-element-property :label fd)))
                 (org-element-map tree 'footnote-reference
                   (lambda (fn)
                     (when (eq (org-element-property :type fn) 'inline)
                       (org-element-property :label fn)))))))
           (org-element-map tree 'footnote-reference
             (lambda (fn)
               (let ((line (line-number-at-pos (org-element-property :begin fn)))
                     (label (org-element-property :label fn)))
                 (if (eq (org-element-property :type fn) 'inline)
                     (push line inline-fn)
                   (when (and label (not (member label defined)))
                     (push (cons line label) undef-fn)))))))
         (let (inventory
               (say (lambda (f)
                      (pcase f
                        ('broken "an internal reference with no target")
                        ('unknown "org has no entry for this link type")
                        ('no-file "the file is missing")
                        ('no-place "the file exists, the place in it does not"))))
               (group (lambda (pred)
                        (seq-filter pred (reverse bad-link)))))
           (maphash (lambda (k v) (push (cons k v) inventory)) seen)
           (string-join
            (delq nil
                  (list
                   (when not-a-bullet
                     (format "not-a-bullet — a body line that is not a bullet:\n%s"
                             (mapconcat (lambda (l) (format "  line %d" l))
                                        (nreverse not-a-bullet) "\n")))
                   (when no-tag
                     (format "no-tag — a bullet with no \" :: \":\n%s"
                             (mapconcat (lambda (l) (format "  line %d" l))
                                        (nreverse no-tag) "\n")))
                   (when long-tag
                     (format "tag — an intention word that is more than one word:\n%s"
                             (mapconcat (lambda (c) (format "  line %d: %S" (car c) (cdr c)))
                                        (nreverse long-tag) "\n")))
                   (when unknown-tag
                     (format (concat "unknown-intention — the vocabulary does not define it;"
                                     " use one of\n%s\n  or have the user accept the word,"
                                     " then declare it with a \"#+INTENTION_WORDS:\" line:\n%s")
                             (mapconcat (lambda (c) (format "  %-10s %s" (car c) (cdr c)))
                                        konix/note-intention-words "\n")
                             (mapconcat (lambda (c) (format "  line %d: %S" (car c) (cdr c)))
                                        (nreverse unknown-tag) "\n")))
                   (when long-line
                     (format "long-line — 120 visible characters or more:\n%s"
                             (mapconcat (lambda (c) (format "  line %d: %d visible"
                                                            (car c) (cdr c)))
                                        (nreverse long-line) "\n")))
                   (when long-bullet
                     (format "long-bullet — more than 300 visible characters:\n%s"
                             (mapconcat (lambda (c) (format "  line %d: %d visible"
                                                            (car c) (cdr c)))
                                        (nreverse long-bullet) "\n")))
                   (when stray-top
                     (format "top-place — this bullet belongs before the first heading:\n%s"
                             (mapconcat (lambda (c) (format "  line %d: %s" (car c) (cdr c)))
                                        (nreverse stray-top) "\n")))
                   (when orphan
                     (format "no-antecedent — nothing precedes this bullet:\n%s"
                             (mapconcat (lambda (c) (format "  line %d: %s" (car c) (cdr c)))
                                        (nreverse orphan) "\n")))
                   (when (and (not catalog) (not has-toc) (> heading-count 7))
                     (format "no-toc — %d headings and no table of contents"
                             heading-count))
                   (when inline-fn
                     (format "inline-footnote — its text sits in the line, not at the foot:\n%s"
                             (mapconcat (lambda (l) (format "  line %d" l))
                                        (nreverse inline-fn) "\n")))
                   (when undef-fn
                     (format "undefined-footnote — the reference has no definition:\n%s"
                             (mapconcat (lambda (c) (format "  line %d: %s" (car c) (cdr c)))
                                        (nreverse undef-fn) "\n")))
                   (let ((ls (funcall group (lambda (l)
                                              (and (not (nth 3 l))
                                                   (memq (nth 1 l) '(broken unknown)))))))
                     (when ls
                       (format "broken-link — does not resolve:\n%s"
                               (mapconcat
                                (lambda (l) (format "  line %d: %s — %s" (nth 0 l) (nth 2 l)
                                                    (funcall say (nth 1 l))))
                                ls "\n"))))
                   (let ((ls (funcall group (lambda (l)
                                              (and (not (nth 3 l))
                                                   (memq (nth 1 l) '(no-file no-place)))))))
                     (when ls
                       (format "dead-link — exports quietly:\n%s"
                               (mapconcat
                                (lambda (l) (format "  line %d: %s — %s" (nth 0 l) (nth 2 l)
                                                    (funcall say (nth 1 l))))
                                ls "\n"))))
                   (let ((ls (funcall group (lambda (l) (nth 3 l)))))
                     (when ls
                       (format
                        "empty-transclusion — the section it should fill exports empty:\n%s"
                        (mapconcat
                         (lambda (l) (format "  line %d: %s — %s" (nth 0 l) (nth 2 l)
                                             (funcall say (nth 1 l))))
                         ls "\n"))))
                   (when deep
                     (format "deep — go deeper:\n%s"
                             (mapconcat
                              (lambda (d)
                                (format "  line %d: %d %s" (nth 0 d) (nth 2 d)
                                        (pcase (nth 1 d)
                                          ('subs "sub-headings")
                                          ('bullets "bullets side by side")
                                          ('lines "lines of body"))))
                              (reverse deep) "\n")))
                   (format "intention words used:\n%s"
                           (mapconcat (lambda (c) (format "  %-10s %d" (car c) (cdr c)))
                                      (sort inventory (lambda (a b) (> (cdr a) (cdr b))))
                                      "\n"))))
            "\n")))))))

(provide 'KONIX_mcp-server-note-mechanics)
;; note-mechanics ends here
