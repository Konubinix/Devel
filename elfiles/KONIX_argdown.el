;; [[id:4a21535e-8f34-4a0b-8014-bc862bda9785::-*- lexical-binding: t; -*-][-*- lexical-binding: t; -*-]]
;;; KONIX_argdown.el --- Argdown mode + org-babel  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  konubinix
;; Author: konubinix <konubinixweb@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;;; Code:

(require 'ob)
(require 'ox)
(require 'cl-lib)

(defface argdown-supportive-claim-face '((t :foreground "green"))
  "Face for argdown supportive claims."
  :group 'argdown)

(defface argdown-unsupportive-claim-face '((t :foreground "red"))
  "Face for argdown unsupportive claims."
  :group 'argdown)

(defface argdown-countradict-claim-face '((t :foreground "red"))
  "Face for argdown countradict claims."
  :group 'argdown)

(defvar
  argdown-highlights
  '(
    ("\\[\\([^]]+\\)\\]:?" (1 font-lock-function-name-face))
    ("<\\([^>]+\\)>:?" (1 font-lock-function-name-face))
    ("^ +\\(\\+\\) " (1 'argdown-supportive-claim-face))
    ("^ +\\(\\-\\) " (1 'argdown-unsupportive-claim-face))
    ("^ +\\(><\\) " (1 'argdown-countradict-claim-face))
    )
  "Specific argdown construct to highlight."
  )

;;;###autoload
(define-derived-mode argdown-mode markdown-mode "argdown"
  "Major mode for editing argdown document."
  (setq font-lock-defaults '(argdown-highlights))
  (setq-local
   markdown-asymmetric-header t
   markdown-unordered-list-item-prefix "  + "
   )
  )
;; -*- lexical-binding: t; -*- ends here

(defun argdown--require-bin ()
  "Error unless the `argdown' CLI is reachable (it ships its own Graphviz)."
  (unless (executable-find "argdown")
    (error "argdown: not on PATH — its hardlias lazily installs the flake; \
tangle/build it (see argdown_in_org_mode.org)")))

(defun argdown--with-input (body fn)
  "Write BODY to a temp .argdown file and call FN with its absolute path."
  (let ((in (org-babel-temp-file "argdown-" ".argdown")))
    (with-temp-file in (insert body))
    (funcall fn (expand-file-name in))))

(defun argdown--run (cmd)
  "Run argdown shell CMD, returning stdout.  On a non-zero exit or empty
output, signal an error carrying argdown's OWN diagnostics — stderr, or a
re-run without `--silent' — instead of letting a downstream JSON/SVG parse
fail with a cryptic \"End of file while parsing JSON\"."
  (let ((err (make-temp-file "argdown-err")) out code stderr)
    (unwind-protect
        (progn
          (with-temp-buffer
            (setq code (call-process-shell-command
                        cmd nil (list (current-buffer) err) nil))
            (setq out (buffer-string)))
          (setq stderr (with-temp-buffer
                         (insert-file-contents err) (buffer-string)))
          (when (or (not (eq code 0)) (string-empty-p (string-trim out)))
            (let ((diag (string-trim stderr)))
              (when (string-empty-p diag)   ; --silent can swallow the error
                (setq diag (string-trim
                            (shell-command-to-string
                             (concat (replace-regexp-in-string " --silent\\b" "" cmd)
                                     " 2>&1")))))
              (error "argdown failed (exit %s): %s" code
                     (if (string-empty-p diag) out diag))))
          out)
      (delete-file err))))

(defun argdown--stdout (fmt in)
  "Return Argdown's own map of INPUT file exported in FMT, via --stdout."
  (argdown--run
   (format "argdown map -f %s --stdout --silent %s"
           (shell-quote-argument fmt) (shell-quote-argument in))))

(defun argdown--render (fmt in out)
  "Render INPUT file's map to file OUT in FMT, using Argdown's renderer.  A
`:file' export is a *static* image, so it rides Argdown's own Graphviz layout:
`svg' is Argdown's SVG (`argdown--stdout'); `dot'/`gv' write Argdown's DOT;
`pdf' uses Argdown's bundled Graphviz (it refuses stdout, so via a temp
folder); png/jpg/webp are an ImageMagick step on the svg.  (The interactive,
self-contained map is a different artifact — see `argdown--map-html'.)"
  (pcase fmt
    ("svg" (with-temp-file out (insert (argdown--stdout "svg" in))))
    ((or "dot" "gv") (with-temp-file out (insert (argdown--stdout "dot" in))))
    ("pdf"
     (let ((dir (make-temp-file "argdown-pdf" t)))
       (unwind-protect
           (progn
             (org-babel-eval
              (format "argdown map -f pdf --silent %s %s"
                      (shell-quote-argument in) (shell-quote-argument dir)) "")
             (let ((made (car (directory-files dir t "\\.pdf\\'"))))
               (unless made (error "argdown: pdf export produced no file"))
               (copy-file made out t)))
         (delete-directory dir t))))
    ((or "png" "jpg" "jpeg" "webp")
     (let ((svg (org-babel-temp-file "argdown-" ".svg"))
           (magick (or (executable-find "magick") (executable-find "convert"))))
       (unless magick (error "argdown: need ImageMagick (magick/convert) for %s" fmt))
       (with-temp-file svg (insert (argdown--stdout "svg" in)))
       (org-babel-eval (format "%s %s %s" magick
                               (shell-quote-argument (expand-file-name svg))
                               (shell-quote-argument (expand-file-name out))) "")))
    (_ (error "argdown: unsupported :file format %s" fmt))))

(defun argdown--to-ipfs (file suffix)
  "Upload FILE to IPFS via `konix/ipfa-buffer', return the URL plus SUFFIX."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (concat (konix/ipfa-buffer nil) suffix)))

(defvar org-babel-default-header-args:argdown '((:cache . "yes") (:results . "output html"))
  "Default header args for argdown src blocks — the interactive map, cached.")

(defconst argdown--epistemic-tag-colors
      '(;; GENERIC ladder of proof — by warrant TYPE, weakest→strongest.  Grounded
        ;; in the zététique « échelle de la preuve » (Durand) and the AFIS
        ;; « niveaux de preuve » (Caroti), themselves resting on Hume/Laplace and
        ;; the GRADE evidence hierarchy — not invented here.
        ("bare assertion"          . "#a50026")
        ("interested testimony"    . "#d73027")
        ("anecdote"                . "#f46d43")
        ("received opinion"        . "#fdae61")
        ("disinterested testimony" . "#fee08b")
        ("expert judgment"         . "#d9ef8b")
        ("convergent testimony"    . "#a6d96a")
        ("documented observation"  . "#66bd63")
        ("reproducible study"      . "#1a9850")
        ("established consensus"   . "#006837")
        ;; Evidence-LAW aliases — the same rungs in legal vocabulary, at the
        ;; matching colour, so law reads as one INSTANTIATION of the generic
        ;; ladder and existing legal notes keep rendering.  (présomption is
        ;; legacy: a derivation, not a warrant — new notes let PCS propagation
        ;; colour the conclusion instead of tagging it.)
        ("affirmation péremptoire" . "#a50026")   ; = bare assertion
        ("témoignage d'une partie" . "#d73027")   ; = interested testimony
        ("témoignage de tiers"     . "#fee08b")   ; = disinterested testimony
        ("présomption"             . "#a6d96a")   ; legacy (a derivation)
        ("constat"                 . "#66bd63")   ; = documented observation
        ("acte authentique"        . "#006837"))  ; = established consensus
      "House epistemic-strength scale for argument-map tags, weakest→strongest, as
    a dialed-back red→green (RdYlGn) ramp.  A statement/argument tagged
    `#(<level>)' takes that colour as its node border; the *pure* red/green are
    left to the relation edges (their polarity — for/against), so the tags use the
    muted RdYlGn hues.  The rungs are a GENERIC ladder of proof (by warrant type), so the scale
    serves any domain; the legal terms are aliases mapping evidence-law's types
    onto the same rungs/colours.  A cross-note convention — injected into *every*
    map by `argdown--frontmatter', never redefined per note.  See the \"Epistemic
    nuance scale\" section.")

    (defconst argdown--epistemic-ramp
      '("#a50026" "#d73027" "#f46d43" "#fdae61" "#fee08b"
        "#d9ef8b" "#a6d96a" "#66bd63" "#1a9850" "#006837")
      "The dialed-back red→green ramp, indexed by epistemic RANK 0 (weakest) → 9
    (strongest) — the colour carrier for `argdown--epistemic-tag-rank' and for the
    propagated conclusion/argument colours.  Pure #ff0000/#00ff00 stay reserved for
    the relation edges, so these are the muted RdYlGn hues.")

    (defconst argdown--epistemic-tag-rank
      '(("bare assertion"          . 0)
        ("interested testimony"    . 1)
        ("anecdote"                . 2)
        ("received opinion"        . 3)
        ("disinterested testimony" . 4)
        ("expert judgment"         . 5)
        ("convergent testimony"    . 6)
        ("documented observation"  . 7)
        ("reproducible study"      . 8)
        ("established consensus"   . 9)
        ;; legal aliases → the rank of their generic rung
        ("affirmation péremptoire" . 0)
        ("témoignage d'une partie" . 1)
        ("témoignage de tiers"     . 4)
        ("présomption"             . 6)
        ("constat"                 . 7)
        ("acte authentique"        . 9))
      "Tag → epistemic RANK (0–9) on `argdown--epistemic-ramp'; legal aliases share
    their generic rung's rank.  Used by `argdown--strength-colors' to seed and
    propagate weakest-link strength.  (`argdown--epistemic-tag-colors' is the same
    mapping pre-resolved to colours, for the `tagColors' frontmatter.)")

    (defun argdown--yaml-key (s)
      "Quote S as a YAML mapping key.  Statement/argument titles carry spaces,
`≠', `:', `« »'… which a bare key cannot; double-quote and escape any `\"'."
      (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\""))

    (defun argdown--color-map (key colors)
      "A `color:' sub-block KEY (e.g. \"statementColors\") for COLORS (alist
title→hex), or nil when empty.  Titles are quoted YAML keys (`argdown--yaml-key')."
      (when colors
        (concat "\n    " key ":\n"
                (mapconcat (lambda (c) (format "        %s: \"%s\""
                                               (argdown--yaml-key (car c)) (cdr c)))
                           colors "\n"))))

    (defun argdown--frontmatter (mode &optional statement-colors argument-colors)
      "The single frontmatter block prepended to every composed Argdown document:
    the house epistemic tag colours (`argdown--epistemic-tag-colors', always); the
    propagated conclusion border colours STATEMENT-COLORS and the per-argument fill
    colours ARGUMENT-COLORS (alists title→hex, when given — see
    `argdown--strength-colors'); and `model.mode: strict' unless MODE is \"loose\".
    Strict is the default because these notes require inferences to hold literally —
    in loose mode a `+' between statements claims only dialectical support, which
    lets a mere corroboration read as a deduction with nothing flagging it.
    `:argdown-mode loose' is the explicit opt-out.
    Everything under one `===' block — Argdown accepts frontmatter only at the very
    top and only once, so colours + mode must share it (a second block, or one
    lower down, is a parse error)."
      (concat
       "===\ncolor:\n    tagColors:\n"
       (mapconcat (lambda (tc) (format "        %s: \"%s\"" (car tc) (cdr tc)))
                  argdown--epistemic-tag-colors "\n")
       (argdown--color-map "statementColors" statement-colors)
       (argdown--color-map "argumentColors" argument-colors)
       (unless (equal mode "loose") "\nmodel:\n    mode: strict")
       "\n==="))

    (defun argdown--compose (body params &optional statement-colors argument-colors)
      "Prepend the frontmatter + :argdown-include / :argdown-collect fragments to
    BODY per PARAMS — frontmatter first, then included premises, then collected
    notes, then BODY — joined so Argdown merges them by title.  Shared by
    `org-babel-execute:argdown' and `konix/argdown-preview', so a preview composes
    its sources exactly as the published render does.  `argdown--frontmatter'
    always leads with the house epistemic tag colours, optionally the propagated
    conclusion border colours STATEMENT-COLORS and per-argument fill colours
    ARGUMENT-COLORS (see `argdown--render-input'), and — unless :argdown-mode is
    \"loose\" — folds `model.mode: strict' into that same single block (Argdown
    requires one frontmatter, at the very top, else a parse error): in strict mode
    + / - / >< between statements read as logical entails / contrary /
    contradictory instead of dialectical support / attack, while an argument's
    + / - stay support / attack."
      (let ((inc (let ((c (cdr (assq :argdown-include params)))) (and c (format "%s" c))))
            (col (let ((c (cdr (assq :argdown-collect params)))) (and c (format "%s" c))))
            (mode (let ((c (cdr (assq :argdown-mode params)))) (and c (format "%s" c)))))
        (mapconcat #'identity
                   (delq nil (list (argdown--frontmatter mode statement-colors argument-colors)
                                   (and inc (konix/argdown--expand-includes inc))
                                   (and col (konix/argdown-collect col))
                                   body))
                   "\n\n")))

    (defun org-babel-execute:argdown (body params)
      "Render an Argdown BODY.  Dispatch on headers:
    - :file F                -> write a static map image to F (svg/dot/pdf/png/jpg/webp),
                                return nil so Org inserts the [[file:F]] link
    - :results output html   -> our own inline-SVG map fragment (interactive: fold,
                                inline source links); the shared engine that drives
                                it is injected once per page on export
                                (`argdown--inject-runtime')
    - :results ... pdf|png   -> render and upload to IPFS, return the URL
    Composition (prepended to BODY via `argdown--compose'): :argdown-include REFS
    pulls named blocks (local or `file.org:name', recursive); :argdown-collect SPEC
    pulls whole linked notes.  Argdown then merges everything by title."
      (argdown--require-bin)
      (let* ((full (argdown--render-input body params))
             (rp (cdr (assq :result-params params)))
             (file (cdr (assq :file params))))
        (argdown--with-input
         full
         (lambda (in)
           (cond
            (file
             (let ((fmt (let ((e (downcase (or (file-name-extension file) "svg"))))
                          (pcase e ("gv" "dot") ("jpeg" "jpg") (_ e)))))
               (argdown--render fmt in (expand-file-name file))
               nil))
            ((member "html" rp) (argdown--map-html in))
            ((member "pdf" rp)
             (let ((out (org-babel-temp-file "argdown-" ".pdf")))
               (argdown--render "pdf" in out)
               (argdown--to-ipfs out "?a.pdf")))
            ((member "png" rp)
             (let ((out (org-babel-temp-file "argdown-" ".png")))
               (argdown--render "png" in out)
               (argdown--to-ipfs out "?a.png")))
            (t (error "argdown: give :file F, or :results output html|pdf|png")))))))

    (defun konix/argdown-preview ()
      "Open the argdown src block at point in a browser as the very map the
    published page will embed — what you see is what you'll publish.  Sources are
    composed (:argdown-include / :argdown-collect) exactly as on render, then
    `argdown--map-html' builds the map fragment; the page pairs it with the shared
    `argdown--runtime-html' — the same one-per-page assembly the publish path does
    — wrapped in a minimal standalone document (charset + body).  It carries its
    own inline SVG, CSS and JS — no network, no CDN."
      (interactive)
      (argdown--require-bin)
      (let ((info (org-babel-get-src-block-info 'light)))
        (unless (and info (equal (nth 0 info) "argdown"))
          (user-error "Point is not in an argdown src block"))
        (let* ((full (argdown--render-input (nth 1 info) (nth 2 info)))
               (frag (argdown--with-input full #'argdown--map-html))
               (html (concat "<!DOCTYPE html><html><head><meta charset=\"utf-8\">"
                             "</head><body>\n" (argdown--runtime-html) "\n"
                             frag "\n</body></html>"))
               (file (make-temp-file "argdown-preview-" nil ".html")))
          (with-temp-file file (insert html))
      (shell-command (format "clk ipfs browse '%s' &" file))
          (message "argdown preview → %s" file))))

(require 'json)
(require 'cl-lib)

(defun argdown--json (in)
  "Parse the `argdown json' model of INPUT file into an alist tree."
  (argdown--split-steps
   (json-parse-string
    (argdown--run (format "argdown json --stdout --silent %s" (shell-quote-argument in)))
    :object-type 'alist :array-type 'list :null-object nil :false-object nil)))

(defun argdown--with-role (m role)
  "Member M with its `role' set to ROLE."
  (cons (cons 'role role) (assq-delete-all 'role (copy-alist m))))

(defun argdown--argument-steps (arg)
  "ARG's pcs cut into one member list per inference step: a step holds the
premises it consumes — the previous step's conclusion among them — and ends on
the conclusion it establishes."
  (let (steps pending)
    (dolist (m (alist-get 'pcs arg))
      (if (member (alist-get 'role m) '("intermediary-conclusion" "main-conclusion"))
          (progn
            (push (append (nreverse pending)
                          (list (argdown--with-role m "main-conclusion")))
                  steps)
            (setq pending (list (argdown--with-role m "premise"))))
        (push m pending)))
    (nreverse steps)))

(defun argdown--split-steps (model)
  "MODEL with every argument of several steps replaced by one argument per step,
titled after it.  A step keeps the whole argument's members, so its box reads the
same description."
  (let (out)
    (dolist (a (alist-get 'arguments model))
      (let* ((arg (cdr a))
             (steps (argdown--argument-steps arg))
             (n (length steps))
             (k 0))
        (if (<= n 1)
            (push a out)
          (dolist (s steps)
            (setq k (1+ k))
            (let* ((title (format "%s (%d/%d)" (alist-get 'title arg) k n))
                   (rest (assq-delete-all 'title (assq-delete-all 'pcs (copy-alist arg)))))
              (push (cons (intern title)
                          (append (list (cons 'title title) (cons 'pcs s)) rest))
                    out))))))
    (append (list (cons 'arguments (nreverse out)))
            (assq-delete-all 'arguments (copy-alist model)))))

(defconst konix/argdown--marker-re
  "[ \t]*\\(\\[[^]]*\\]\\|<[^>]*>\\|([0-9]+)\\|[-+]\\|><\\|=+\\|----\\|#\\)"
  "Regexp matching the start of an argdown structural line (statement,
argument, premise number, relation, inference…).")

(defun konix/ox-hugo--argdown-statements (src html)
  "Group HTML into (INDENT . TEXT) statements, SRC deciding where each opens."
  (let* ((chop (lambda (s) (replace-regexp-in-string "\n\\'" "" s)))
         (srcs (split-string (funcall chop src) "\n"))
         (htmls (split-string (funcall chop html) "\n"))
         (col (lambda (s) (- (length s) (length (string-trim-left s)))))
         (out (list (cons (funcall col (car srcs)) (car htmls)))))
    (setq srcs (cdr srcs))
    (dolist (h (cdr htmls))
      (let ((s (or (pop srcs) "")))
        (if (or (string-empty-p (string-trim s))
                (string-match-p (concat "\\`" konix/argdown--marker-re) s))
            (push (cons (funcall col s) h) out)
          (setcdr (car out) (concat (cdar out) " " (string-trim h))))))
    (nreverse out)))

(defconst konix/argdown--hang 2
  "Columns a statement's marker hangs out of its text column.")

(defun konix/ox-hugo--argdown-block (statements)
  "Render STATEMENTS, each (INDENT . TEXT), as one block keeping its columns."
  (concat "<div class=\"src src-argdown\" style=\"font-family:monospace;\">\n"
          (mapconcat
           (lambda (s)
             (let ((text (string-trim-left (cdr s))))
               (format "<div style=\"padding-left:%dch;text-indent:-%dch;\">%s</div>"
                       (+ (car s) konix/argdown--hang) konix/argdown--hang
                       (if (string-empty-p text) "<br>" text))))
           statements "\n")
          "\n</div>\n"))

(defun konix/ox-hugo--argdown-html (src-block info)
  "Return SRC-BLOCK fontified as inline-styled argdown HTML."
  (let ((src (org-export-format-code-default src-block info)))
    (konix/ox-hugo--argdown-block
     (konix/ox-hugo--argdown-statements src (org-html-fontify-code src "argdown")))))

(defun konix/ox-hugo-src-block--argdown (orig src-block contents info)
  "Fontify argdown src blocks with Emacs; defer everything else to ORIG."
  (if (string= (org-element-property :language src-block) "argdown")
      (konix/ox-hugo--argdown-html src-block info)
    (funcall orig src-block contents info)))

(with-eval-after-load 'ox-hugo
  (advice-add 'org-hugo-src-block :around #'konix/ox-hugo-src-block--argdown))

(defconst argdown--inference-force-ranks
  '(("non sequitur" . 0) ("ténue" . 2) ("plausible" . 4)
    ("solide" . 6) ("forte" . 8) ("déductive" . 9))
  "How strongly the premises bring the conclusion — a scale for the *inference*
(the `----'), independent of the premises' evidential weight, spread onto the
same 0–9 rank as `argdown--epistemic-ramp' so the two combine by weakest link
(`déductive' = 9 never caps a premise-driven rank; `non sequitur' = 0 collapses
it).  Marked as inference data: `-- {force: \"<level>\"} --'.")

(defun argdown--lighten (hex frac)
  "Blend HEX (\"#rrggbb\") toward white by FRAC (0.0–1.0).  For argument fills:
the whole box takes the colour, so a lightened shade keeps the black label
readable while still reading as the rank's hue."
  (let* ((r (string-to-number (substring hex 1 3) 16))
         (g (string-to-number (substring hex 3 5) 16))
         (b (string-to-number (substring hex 5 7) 16))
         (mix (lambda (c) (round (+ c (* (- 255 c) frac))))))
    (format "#%02x%02x%02x" (funcall mix r) (funcall mix g) (funcall mix b))))

(defun argdown--arg-inference-rank (pcs)
  "Weakest inference-force rank among PCS's inference steps (`data.force' →
`argdown--inference-force-ranks'), or nil if none is marked."
  (let ((r nil))
    (dolist (m pcs)
      (let* ((inf (alist-get 'inference m))
             (force (and inf (alist-get 'force (alist-get 'data inf))))
             (fr (and force (cdr (assoc force argdown--inference-force-ranks)))))
        (when fr (setq r (if r (min r fr) fr)))))
    r))

(defun argdown--strength (title tag-rank concludes memo inprog)
  "Propagated epistemic rank of statement TITLE — an index into
`argdown--epistemic-ramp' (lower = weaker) — or nil if undetermined.
An asserted tag wins; else the best (max) supporting argument's weakest (min)
link — the links being the argument's premises AND its inference force.
Recursive over chains, memoised in MEMO, cycle-guarded by INPROG.  CONCLUDES
maps a conclusion title to a list of (premise-titles . inference-rank), one per
concluding argument; TAG-RANK maps a tagged statement title to its rank."
  (let ((cached (gethash title memo 'unset)))
    (cond
     ((not (eq cached 'unset)) (and (numberp cached) cached))
     ((gethash title inprog) nil)            ; cycle: break the back-edge
     (t
      (puthash title t inprog)
      (let ((result
             (or (gethash title tag-rank)     ; asserted tag wins
                 (let ((best nil))
                   (dolist (arg (gethash title concludes))
                     (let ((mn (cdr arg)))    ; seed with the inference rank
                       (dolist (p (car arg))
                         (let ((ps (argdown--strength p tag-rank concludes memo inprog)))
                           (when ps (setq mn (if mn (min mn ps) ps)))))
                       (when mn (setq best (if best (max best mn) mn)))))
                   best))))                   ; best argument across the lot
        (remhash title inprog)
        (puthash title (or result 'none) memo)
        result)))))

(defun argdown--strength-colors (in)
  "Propagate the epistemic scale through INPUT file's argument structure
(weakest-link over premises AND inference force, see `argdown--strength').
Return (STATEMENT-COLORS . ARGUMENT-COLORS): conclusion title→border hex (only
UNTAGGED conclusions — asserted tags keep their own colour, and `statementColors'
would otherwise override them), and argument title→*lightened* fill hex (that
argument's own weakest link)."
  (let* ((model (argdown--json in))
         (tag-rank (make-hash-table :test 'equal))
         (concludes (make-hash-table :test 'equal))
         (memo (make-hash-table :test 'equal))
         (inprog (make-hash-table :test 'equal))
         (args nil) (scolors nil) (acolors nil))
    (dolist (s (alist-get 'statements model))
      (let* ((st (cdr s))
             (title (alist-get 'title st))
             (tags (alist-get 'tags st))
             (rank (cl-some (lambda (tag)
                              (cdr (assoc tag argdown--epistemic-tag-rank)))
                            tags)))
        (when (and title rank) (puthash title rank tag-rank))))
    (dolist (a (alist-get 'arguments model))
      (let* ((arg (cdr a))
             (atitle (alist-get 'title arg))
             (pcs (alist-get 'pcs arg))
             (concl (cl-some (lambda (m) (and (equal (alist-get 'role m) "main-conclusion")
                                              (alist-get 'title m)))
                             pcs))
             (premises (delq nil (mapcar (lambda (m)
                                           (and (equal (alist-get 'role m) "premise")
                                                (alist-get 'title m)))
                                         pcs)))
             (inf (argdown--arg-inference-rank pcs)))
        (when (and concl (or premises inf))
          (puthash concl (cons (cons premises inf) (gethash concl concludes)) concludes))
        (when atitle (push (list atitle premises inf) args))))
    ;; conclusion border colours — untagged conclusions only
    (dolist (s (alist-get 'statements model))
      (let* ((st (cdr s))
             (title (alist-get 'title st)))
        (when (and title
                   (or (alist-get 'isUsedAsMainConclusion st)
                       (alist-get 'isUsedAsIntermediaryConclusion st))
                   (not (gethash title tag-rank)))
          (let ((rank (argdown--strength title tag-rank concludes memo inprog)))
            (when rank
              (push (cons title (nth rank argdown--epistemic-ramp)) scolors))))))
    ;; argument fill colours — each argument's own weakest link, lightened
    (dolist (a args)
      (let ((atitle (nth 0 a)) (mn (nth 2 a)))   ; seed with inference rank
        (dolist (p (nth 1 a))
          (let ((ps (argdown--strength p tag-rank concludes memo inprog)))
            (when ps (setq mn (if mn (min mn ps) ps)))))
        (when mn
          (push (cons atitle (argdown--lighten
                              (nth mn argdown--epistemic-ramp) 0.7))
                acolors))))
    (cons scolors acolors)))

(defun argdown--render-input (body params)
  "Composed Argdown for BODY/PARAMS, with conclusion borders and argument fills
coloured by propagated epistemic strength (weakest-link over premises AND
inference force; see `argdown--strength-colors').  Two-pass: compose once, read
the model, recompose injecting the colours as `statementColors' / `argumentColors'.
The model pass is skipped when the input carries no tag nor inference force."
  (let ((full (argdown--compose body params)))
    (if (not (string-match-p "#(\\|force:" full))
        full
      (let* ((colors (argdown--with-input full #'argdown--strength-colors))
             (scolors (car colors)) (acolors (cdr colors)))
        (if (or scolors acolors)
            (argdown--compose body params scolors acolors)
          full)))))

(defun konix/argdown--bodies-in-file (file)
  "Return the bodies of every argdown src block in FILE.
Common leading indentation is stripped (`org-remove-indentation') so the
fragment's top-level statements land in column 0 — Argdown is
indentation-sensitive, and blocks are often indented under a heading."
  (with-temp-buffer
    (insert-file-contents file)
    (delay-mode-hooks (org-mode))
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (sb)
        (when (string= (org-element-property :language sb) "argdown")
          (string-trim-right
           (org-remove-indentation (or (org-element-property :value sb) ""))))))))

(defun konix/argdown--link-ids (&optional subtree)
  "Return the `id:' link targets in the current buffer, in order.
With SUBTREE non-nil, restrict to the heading subtree at point (the section
that owns the block being rendered) — this honours the per-section
\"n'utilise en source que les notes mentionnées ici\" convention."
  (save-excursion
    (save-restriction
      (when (and subtree (not (org-before-first-heading-p)))
        (org-back-to-heading t)
        (org-narrow-to-subtree))
      (let (ids)
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (l)
            (when (string= (org-element-property :type l) "id")
              (push (org-element-property :path l) ids))))
        (nreverse ids)))))

(defun konix/argdown--linked-files (&optional subtree)
  "Files of the `id:' links in the current buffer (or SUBTREE at point)."
  (delete-dups
   (delq nil
         (mapcar (lambda (id)
                   (when-let* ((node (org-roam-node-from-id id)))
                     (org-roam-node-file node)))
                 (konix/argdown--link-ids subtree)))))

(defun konix/argdown--backlink-files ()
  "Files of notes that link to any node in the current file."
  (delete-dups
   (delq nil
         (mapcar (lambda (bl)
                   (ignore-errors
                     (org-roam-node-file (org-roam-backlink-source-node bl))))
                 (apply #'append
                        (mapcar #'org-roam-backlinks-get
                                (konix/org-roam-nodes-in-file)))))))

(defun konix/argdown--tagged-files (tag)
  "Files of notes carrying TAG."
  (delete-dups
   (delq nil
         (mapcar (lambda (row)
                   (when-let* ((node (org-roam-node-from-id (car row))))
                     (org-roam-node-file node)))
                 (org-roam-db-query
                  [:select [node_id] :from tags :where (= tag $s1)] tag)))))

(defun konix/argdown-collect (spec)
  "Concatenate argdown fragments from notes selected by SPEC.
SPEC is \"links\" (notes this one links to), \"subtree\" (notes linked within
the current heading subtree), \"backlinks\" (notes linking here), or a tag
name.  The current file is always excluded.  Argdown merges
statements/arguments by title, so the concatenation renders as one map."
  (require 'org-roam)
  (let* ((files (pcase spec
                  ("links" (konix/argdown--linked-files))
                  ("subtree" (konix/argdown--linked-files t))
                  ("backlinks" (konix/argdown--backlink-files))
                  (_ (konix/argdown--tagged-files spec))))
         (self (buffer-file-name))
         (files (cl-remove-if (lambda (f) (and self (file-equal-p f self))) files)))
    (mapconcat (lambda (f)
                 (mapconcat #'identity (konix/argdown--bodies-in-file f) "\n\n"))
               files "\n\n")))

;;; :argdown-include — precise composition by named block (« notre mode »)

(defun konix/argdown--parse-include (params)
  "Extract the :argdown-include value from a src-block PARAMS string, or nil.
Stops at the next ` :key', so values may contain colons (file.org:name)."
  (when (and params
             (string-match
              ":argdown-include[ \t]+\\(.*?\\)\\(?:[ \t]+:[a-zA-Z]\\|$\\)" params))
    (match-string 1 params)))

(defun konix/argdown--resolve-file (file)
  "Resolve a .org FILE ref to an absolute path among the roam notes."
  (or (and (file-name-absolute-p file) file)
      (and (boundp 'org-roam-directory)
           (let ((p (expand-file-name file org-roam-directory)))
             (and (file-exists-p p) p)))
      (expand-file-name file)))

(defun konix/argdown--named-block (name &optional file)
  "Return (VALUE . PARAMS) of the argdown src block named NAME in FILE
\(or the current buffer when FILE is nil).  VALUE is dedented."
  (let ((find
         (lambda ()
           (org-element-map (org-element-parse-buffer) 'src-block
             (lambda (sb)
               (when (and (string= (org-element-property :language sb) "argdown")
                          (equal (org-element-property :name sb) name))
                 (cons (org-remove-indentation
                        (or (org-element-property :value sb) ""))
                       (org-element-property :parameters sb))))
             nil t))))
    (if file
        (with-temp-buffer
          (insert-file-contents file)
          (delay-mode-hooks (org-mode))
          (funcall find))
      (funcall find))))

(defun konix/argdown--expand-into (spec seen context-file)
  "Resolve SPEC (refs string) to concatenated argdown.
Local refs resolve against CONTEXT-FILE (nil = current buffer); a `file.org:name'
ref switches the context to that file for its own sub-includes.  SEEN is a hash
table keying (file . name) to break cycles.  Included premises come first."
  (let (out)
    (dolist (ref (split-string (or spec "") "[ \t\n,]+" t))
      (let* ((m (string-match "\\`\\(.+\\.org\\):\\(.+\\)\\'" ref))
             (file (if m (konix/argdown--resolve-file (match-string 1 ref))
                     context-file))
             (name (if m (match-string 2 ref) ref))
             (key (format "%s\0%s" (or file "") name)))
        (unless (gethash key seen)
          (puthash key t seen)
          (let ((blk (konix/argdown--named-block name file)))
            (if (not blk)
                (push (format "// [argdown-include introuvable : %s]" ref) out)
              (let ((sub (konix/argdown--parse-include (cdr blk))))
                (when sub
                  (push (konix/argdown--expand-into sub seen file) out)))
              (push (car blk) out))))))
    (mapconcat #'identity (nreverse out) "\n\n")))

(defun konix/argdown--expand-includes (spec)
  "Public entry: resolve SPEC to concatenated argdown (recursive, cycle-safe)."
  (konix/argdown--expand-into spec (make-hash-table :test 'equal) nil))

;;; Editing comfort — wrap long statement lines, on M-q

(defun konix/argdown--stmt-bounds ()
  "Return (BEG . END) of the argdown statement paragraph at point, or nil.
BEG is the bol of its structural start line, END the eol of its last
continuation line."
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp))
                (not (looking-at konix/argdown--marker-re))
                (looking-at "[ \t]*\\S-"))
      (forward-line -1))
    (when (looking-at konix/argdown--marker-re)
      (let ((beg (line-beginning-position)))
        (forward-line 1)
        (while (and (not (eobp))
                    (looking-at "[ \t]*\\S-")
                    (not (looking-at konix/argdown--marker-re)))
          (forward-line 1))
        (cons beg (line-end-position 0))))))

(defun konix/argdown-fill-paragraph (&optional _justify)
  "Fill the argdown statement at point: merge its lines, re-wrap to
`fill-column' with continuation lines indented 4 more than the title (so
Argdown reads them as description continuations).  Returns t, so it serves
as a `fill-paragraph-function'."
  (interactive)
  (let ((b (konix/argdown--stmt-bounds)))
    (when b
      (let* ((beg (car b)) (end (cdr b))
             (lines (split-string (buffer-substring-no-properties beg end) "\n"))
             (indent (progn (string-match "\\`[ \t]*" (car lines))
                            (match-string 0 (car lines))))
             (text (mapconcat #'string-trim lines " "))
             (cont (concat indent "    "))
             (fill (or fill-column 78))
             (out '()) (curpref indent) (cur '()))
        (dolist (w (split-string text " " t))
          (let ((cand (concat curpref
                              (mapconcat #'identity (reverse (cons w cur)) " "))))
            (if (and cur (> (length cand) fill))
                (progn (push (concat curpref
                                     (mapconcat #'identity (reverse cur) " ")) out)
                       (setq curpref cont cur (list w)))
              (push w cur))))
        (when cur
          (push (concat curpref (mapconcat #'identity (reverse cur) " ")) out))
        (delete-region beg end)
        (goto-char beg)
        (insert (mapconcat #'identity (reverse out) "\n")))))
  t)

(defun konix/argdown-fill-buffer (&optional fill)
  "Re-wrap every argdown statement of the current buffer's src blocks."
  (interactive)
  (let ((fill-column (or fill fill-column 78)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+begin_src argdown" nil t)
        (forward-line 1)
        (let ((end (save-excursion
                     (and (re-search-forward "^[ \t]*#\\+end_src" nil t)
                          (copy-marker (match-beginning 0))))))
          (when end
            (while (< (point) (marker-position end))
              (if (looking-at konix/argdown--marker-re)
                  (let ((b (konix/argdown--stmt-bounds)))
                    (konix/argdown-fill-paragraph)
                    (let ((b2 (konix/argdown--stmt-bounds)))
                      (goto-char (if b2 (cdr b2) (or (cdr b) (line-end-position))))
                      (forward-line 1)))
                (forward-line 1)))
            (goto-char (marker-position end))))))))

(defun konix/argdown--in-src-p ()
  "Non-nil when point is inside an argdown src block of an org buffer."
  (and (derived-mode-p 'org-mode)
       (let ((el (org-element-context)))
         (and (memq (org-element-type el) '(src-block inline-src-block))
              (equal (org-element-property :language el) "argdown")))))

;; M-q inside argdown-mode (e.g. the C-c ' edit buffer, or .argdown files)
(add-hook 'argdown-mode-hook
          (lambda ()
            (setq-local fill-paragraph-function #'konix/argdown-fill-paragraph)))

;; M-q directly on a statement inside an argdown src block of an org note
(with-eval-after-load 'org
  (advice-add 'org-fill-paragraph :before-until
              (lambda (&rest _)
                (and (konix/argdown--in-src-p)
                     (konix/argdown-fill-paragraph)))))

(provide 'KONIX_argdown)
;;; KONIX_argdown.el ends here

(defun argdown--node-id (kind title)
  "Unique node identity: KIND (\"s\" statement / \"a\" argument) prefixed to
TITLE.  Argdown merges by title only within a kind, so a [statement] and an
<argument> of the same title are two nodes; the kind prefix keeps them apart."
  (concat kind ":" title))

(defun argdown--map-nodes (model)
  "Every node of the argdown MODEL as (ID TITLE KIND) — statements (kind \"s\")
then arguments (kind \"a\"), in order.  ID is the kind+title key
(`argdown--node-id'); TITLE is what the box shows; KIND selects the right body
text (`argdown--node-html')."
  (append
   (delq nil (mapcar (lambda (s) (let ((tt (alist-get 'title (cdr s))))
                                   (and tt (list (argdown--node-id "s" tt) tt "s"))))
                     (alist-get 'statements model)))
   (delq nil (mapcar (lambda (a) (let ((tt (alist-get 'title (cdr a))))
                                   (and tt (list (argdown--node-id "a" tt) tt "a"))))
                     (alist-get 'arguments model)))))

(defun argdown--xml-escape (s)
  "Escape `&', `<', `>' in S for XML text content."
  (let* ((s (replace-regexp-in-string "&" "&amp;" s t t))
         (s (replace-regexp-in-string "<" "&lt;" s t t)))
    (replace-regexp-in-string ">" "&gt;" s t t)))

(defconst argdown--node-w 220
  "Fixed node-box width.  Constant so the strength badge and the fold ⊕ sit at
stable offsets and the body text wraps to a known column; the box's *height* is
what varies, remeasured to the wrapped text by the layout pass.")

(defun argdown--node-width (_title)
  "Node-box width — the constant `argdown--node-w'."
  argdown--node-w)

(defun argdown--strip-annotations (s)
  "Drop Argdown inline `#(tag)' / `#tag' grade annotations from S and squeeze
runs of whitespace to one space (edges kept, so a stripped span still joins its
neighbours): the badge names the grade, so the body stays prose."
  (let* ((s (replace-regexp-in-string "#([^)]*)" "" s))
         (s (replace-regexp-in-string "#[[:alnum:]_-]+" "" s)))
    (replace-regexp-in-string "[ \t\n]+" " " s)))

(defun argdown--first-member (members)
  "The first MEMBER whose `text' is non-empty, or nil.  A node declared as a bare
reference before it is defined (`+ <arg>' then later `<arg>: …') contributes an
empty member first; skip it and take the one with the real text (and its
ranges)."
  (cl-some (lambda (m)
             (let ((tx (alist-get 'text m)))
               (and tx (not (string-empty-p (string-trim tx))) m)))
           members))

(defun argdown--render-ranges (text ranges)
  "TEXT rendered to inline body HTML, honouring its RANGES: each `link' range
(Argdown character offsets, stop inclusive) becomes an <a> on its own label
where it sits, the rest is grade-stripped (`argdown--strip-annotations') and
escaped.  So a source citation reads as a real link in the prose."
  (let ((links (sort (seq-filter (lambda (r) (equal (alist-get 'type r) "link"))
                                 (copy-sequence ranges))
                     (lambda (a b) (< (alist-get 'start a) (alist-get 'start b)))))
        (pos 0) (len (length text)) (out ""))
    (dolist (r links)
      (let ((s (alist-get 'start r)) (e (1+ (alist-get 'stop r))) (url (alist-get 'url r)))
        (when (and url (>= s pos) (<= e len))
          (when (> s pos)
            (setq out (concat out (argdown--xml-escape
                                   (argdown--strip-annotations (substring text pos s))))))
          (setq out (concat out (format "<a href=\"%s\" target=\"_blank\" rel=\"noopener\">%s</a>"
                                        (argdown--xml-escape url)
                                        (argdown--xml-escape (substring text s e)))))
          (setq pos e))))
    (when (< pos len)
      (setq out (concat out (argdown--xml-escape
                             (argdown--strip-annotations (substring text pos))))))
    (string-trim out)))

(defun argdown--node-html (model title kind)
  "Body HTML for the KIND (\"s\"/\"a\") node titled TITLE: its first non-empty
member's text rendered with that member's ranges (`argdown--render-ranges') — a
source citation kept as an inline link, the #(grade) tag dropped (the badge is
its home).  KIND picks the right collection, so a [statement] and an <argument>
sharing a title each show their own words.  Empty string for a node with no body."
  (let ((mem (cl-some (lambda (e)
                        (and (equal (alist-get 'title (cdr e)) title)
                             (argdown--first-member (alist-get 'members (cdr e)))))
                      (alist-get (if (equal kind "a") 'arguments 'statements) model))))
    (if mem (argdown--render-ranges (alist-get 'text mem) (alist-get 'ranges mem)) "")))

(defun argdown--node-g (id title html)
  "A <g> node box identified by ID (its unique kind+title key, in `data-id')
carrying its body HTML: a rounded rect and, as selectable rich text in a
<foreignObject> (so a reader can sweep it for a hypothes.is anchor and click a
source link inline), the title over the body prose.  `data-id' is the
layout/fold handle; the box height is a placeholder the layout pass remeasures
to the wrapped content.

A statement written inline, with no author-given title, is auto-named
`Untitled N' by Argdown (no flag distinguishes it — only the name's shape).
That name is noise, so it is not shown: such a node drops the heading and
shows its text alone; a real title still reads as the bold heading above the
prose.  Should an untitled node somehow have no text either, the auto-name is
the last resort, so the box is never blank."
  (let* ((w (argdown--node-width title))
         (eid (argdown--xml-escape id))
         (untitled (string-match-p "\\`Untitled [0-9]+\\'" title))
         (head (if (or untitled (string-empty-p title)) ""
                 (format "<div class=\"argdown-node-title\">%s</div>"
                         (argdown--xml-escape title))))
         (body (if (string-empty-p html) ""
                 (format "<div class=\"argdown-node-text\">%s</div>" html)))
         (content (if (string-empty-p (concat head body))
                      (format "<div class=\"argdown-node-title\">%s</div>"
                              (argdown--xml-escape title))
                    (concat head body))))
    (format (concat "<g class=\"argdown-node\" data-id=\"%s\">"
                    "<rect width=\"%d\" height=\"40\" rx=\"4\" fill=\"#fff\" stroke=\"#888\"/>"
                    "<foreignObject width=\"%d\" height=\"40\">"
                    "<div xmlns=\"http://www.w3.org/1999/xhtml\" class=\"argdown-node-body\">"
                    "%s</div></foreignObject></g>")
            eid w w content)))

(defun argdown--edge-id (type title)
  "Node id for an edge endpoint titled TITLE, from its Argdown TYPE: an
\"argument\" keys to \"a\", any statement type (\"equivalence-class\") to \"s\"
— matching `argdown--map-nodes' so the endpoint names the same node."
  (argdown--node-id (if (equal type "argument") "a" "s") title))

(defun argdown--map-edges (model)
  "Every edge of the argdown MODEL as (FROM-ID TO-ID TYPE): the top-level
relations Argdown reports (`relationType' — support/attack, and in strict mode
entails/contrary/contradictory/undercut), their endpoints keyed by
`fromType'/`toType' (`argdown--edge-id') so they name the right node when a
title is shared, plus the inferential edges synthesized from each argument's pcs
(`argdown--pcs-edges')."
  (append
   (mapcar (lambda (r)
             (list (argdown--edge-id (alist-get 'fromType r) (alist-get 'from r))
                   (argdown--edge-id (alist-get 'toType r) (alist-get 'to r))
                   (alist-get 'relationType r)))
           (alist-get 'relations model))
   (argdown--pcs-edges model)))

(defconst argdown--relation-styles
  '(("support"       "#00ff00" ""          nil "dialectical · for")
    ("attack"        "#ff0000" ""          nil "dialectical · against")
    ("entails"       "#00ff00" "6 4"       nil "logical · for")
    ("contrary"      "#ff0000" "6 4"       nil "logical · against")
    ("contradictory" "#ff0000" "2 3"       t   "logical · mutually exclusive")
    ("undercut"      "#ff0000" "8 3 2 3"   nil "attacks the inference"))
  "Edge look per Argdown relation type: (TYPE STROKE DASHARRAY DOUBLE-HEADED
GLOSS).  Polarity is the colour — pure green for, pure red against (Argdown's
own convention, and why the epistemic node scale stays off pure red/green);
kind is the line style — solid for the dialectical pair (support/attack),
dashed for the logical entails/contrary, dotted for the mutual contradictory
(drawn with an arrowhead at both ends), dash-dot for the inference-aimed
undercut.  No two combinations coincide; GLOSS is the legend's plain reading.")

(defun argdown--relation-style (type)
  "The `argdown--relation-styles' row for relation TYPE, or a neutral grey solid
fallback (with TYPE as its own gloss) for any type not foreseen."
  (or (assoc type argdown--relation-styles)
      (list type "#888888" "" nil type)))

(defun argdown--map-edges-svg (model)
  "Two <path>s per edge in MODEL, a contiguous pair.  The first is the visible
hairline: endpoint node ids (`data-from'/`data-to' — the layout adapter's
routing inputs), a per-type class, and the look from `argdown--relation-style'
— stroke colour (polarity), dash (kind), and the shared `argdown-arrow' marker
at the end (and, for the mutual contradictory, the start too).  The second is
its `argdown-edge-hit' twin: no paint, a fat stroke, a finger-sized tap band
laid over the hairline so the edge can be tapped to travel it — carrying the
same endpoints, since that is what the tap navigates by."
  (mapconcat
   (lambda (e)
     (let* ((type (nth 2 e))
            (from (argdown--xml-escape (nth 0 e)))
            (to (argdown--xml-escape (nth 1 e)))
            (st (argdown--relation-style type))
            (stroke (nth 1 st)) (dash (nth 2 st)) (double (nth 3 st)))
       (format (concat "<path class=\"argdown-edge argdown-edge--%s\""
                       " data-from=\"%s\" data-to=\"%s\" fill=\"none\""
                       " stroke=\"%s\"%s marker-end=\"url(#argdown-arrow)\"%s/>"
                       "<path class=\"argdown-edge-hit\" data-from=\"%s\" data-to=\"%s\"/>")
               (argdown--xml-escape type) from to
               stroke
               (if (string-empty-p dash) "" (format " stroke-dasharray=\"%s\"" dash))
               (if double " marker-start=\"url(#argdown-arrow)\"" "")
               from to)))
   (argdown--map-edges model) "\n"))

(defun argdown--pcs-edges (model)
  "Support edges from each reconstructed argument's pcs, as node ids
(`argdown--node-id'): premise→argument for every role=\"premise\" member and
argument→conclusion for the role=\"main-conclusion\" (premises and conclusions
are statements, so \"s\"; the argument itself \"a\")."
  (let (out)
    (dolist (a (alist-get 'arguments model))
      (let ((aid (argdown--node-id "a" (alist-get 'title (cdr a)))))
        (dolist (m (alist-get 'pcs (cdr a)))
          (pcase (alist-get 'role m)
            ("premise" (push (list (argdown--node-id "s" (alist-get 'title m)) aid "support") out))
            ("main-conclusion" (push (list aid (argdown--node-id "s" (alist-get 'title m)) "support") out))))))
    (nreverse out)))

(defconst argdown--dagre-path
  (expand-file-name "argdown-vendor/dagre-0.8.5.min.js"
                    (file-name-directory (or load-file-name "~/prog/devel/elfiles/")))
  "Where `vendor-dagre' wrote the bundle, beside this file.")

(defun argdown--dagre-js ()
  "The vendored dagre UMD bundle as a string, to inline into a fragment."
  (with-temp-buffer (insert-file-contents argdown--dagre-path) (buffer-string)))

(defconst argdown--fold-dom-js
  (concat
   "  var nodes = [].slice.call(root.querySelectorAll('.argdown-node'));\n"
   "  var edges = [].slice.call(root.querySelectorAll('.argdown-edge'));\n"
   "  var byId = {}, children = {}, folded = {};\n"
   "  nodes.forEach(function(n){ byId[n.getAttribute('data-id')] = n; });\n"
   "  edges.forEach(function(e){\n"
   "    var f = e.getAttribute('data-from'), t = e.getAttribute('data-to');\n"
   "    (children[t] = children[t] || []).push(f);\n"
   "  });\n")
  "DOM handles and the supporter adjacency (children[Y] = the nodes supporting Y),
scoped to one map's ROOT element so several maps on a page never mix.")

(defconst argdown--fold-visible-js
  (concat
   "  function visibleSet(){\n"
   "    var vis = {};\n"
   "    nodes.forEach(function(n){ vis[n.getAttribute('data-id')] = true; });\n"
   "    Object.keys(folded).forEach(function(ft){\n"
   "      if(!folded[ft]) return;\n"
   "      var stack = (children[ft] || []).slice();\n"
   "      while(stack.length){\n"
   "        var c = stack.pop();\n"
   "        if(vis[c]){ vis[c] = false; (children[c] || []).forEach(function(x){ stack.push(x); }); }\n"
   "      }\n"
   "    });\n"
   "    return vis;\n"
   "  }\n")
  "Node ids still visible: all except the transitive supporters of folded nodes.")

(defconst argdown--fold-mark-js
  (concat
   "  function marks(){\n"
   "    nodes.forEach(function(n){\n"
   "      var t = n.getAttribute('data-id');\n"
   "      var collapsed = folded[t] && (children[t] || []).length > 0;\n"
   "      var box = n.querySelector('rect:not(.argdown-fold-stack)');\n"
   "      n.querySelectorAll('.argdown-fold-stack').forEach(function(s){ s.remove(); });\n"
   "      if(collapsed){\n"
   "        [5, 10].forEach(function(off){\n"
   "          var s = document.createElementNS('http://www.w3.org/2000/svg', 'rect');\n"
   "          s.setAttribute('class', 'argdown-fold-stack');\n"
   "          s.setAttribute('x', off); s.setAttribute('y', off);\n"
   "          s.setAttribute('width', box.getAttribute('width'));\n"
   "          s.setAttribute('height', box.getAttribute('height'));\n"
   "          s.setAttribute('rx', 4);\n"
   "          s.setAttribute('fill', box.getAttribute('fill'));\n"
   "          s.setAttribute('stroke', box.getAttribute('stroke'));\n"
   "          n.insertBefore(s, n.firstChild);\n"
   "        });\n"
   "      }\n"
   "      var mark = n.querySelector('.argdown-foldmark');\n"
   "      if(collapsed && !mark){\n"
   "        mark = document.createElementNS('http://www.w3.org/2000/svg', 'text');\n"
   "        mark.setAttribute('class', 'argdown-foldmark');\n"
   "        mark.setAttribute('x', +box.getAttribute('width') - 5);\n"
   "        mark.setAttribute('y', 20);\n"
   "        mark.setAttribute('text-anchor', 'end');\n"
   "        mark.textContent = '⊕';\n"
   "        n.appendChild(mark);\n"
   "      } else if(!collapsed && mark){ mark.remove(); }\n"
   "    });\n"
   "  }\n")
  "Mark each folded node (one with hidden supporters): a card or two stacked\nbehind the box (`argdown-fold-stack' rects) show the hidden subtree as depth,\nand a ⊕ sits in its corner; both clear when it expands.")

(defconst argdown--place-js
  (concat
   "  function place(nodes, edges){\n"
   "    var FAN_WRAP_MIN = 6;\n"
   "    var g = new dagre.graphlib.Graph({multigraph:true});\n"
   "    g.setGraph({rankdir:'BT', nodesep:40, ranksep:60, marginx:20, marginy:20});\n"
   "    g.setDefaultEdgeLabel(function(){ return {}; });\n"
   "    nodes.forEach(function(n){ g.setNode(n.id, {width:n.w, height:n.h}); });\n"
   "    var indeg = {}, outdeg = {};\n"
   "    edges.forEach(function(e){ outdeg[e.from] = (outdeg[e.from]||0) + 1;\n"
   "      indeg[e.to] = (indeg[e.to]||0) + 1; });\n"
   "    var minlen = edges.map(function(){ return 1; }), fans = {};\n"
   "    edges.forEach(function(e, i){\n"
   "      if((indeg[e.from]||0) === 0 && outdeg[e.from] === 1) (fans[e.to] = fans[e.to] || []).push(i);\n"
   "    });\n"
   "    Object.keys(fans).forEach(function(t){\n"
   "      var idx = fans[t];\n"
   "      if(idx.length > FAN_WRAP_MIN){\n"
   "        var rows = Math.ceil(Math.sqrt(idx.length));\n"
   "        idx.forEach(function(j, k){ minlen[j] = 1 + (k % rows); });\n"
   "      }\n"
   "    });\n"
   "    edges.forEach(function(e, i){ g.setEdge(e.from, e.to, {minlen:minlen[i]}, 'e'+i); });\n"
   "    dagre.layout(g);\n"
   "    var pos = {};\n"
   "    nodes.forEach(function(n){ var nd = g.node(n.id); pos[n.id] = {x:nd.x, y:nd.y}; });\n"
   "    var points = edges.map(function(e, i){\n"
   "      var ed = g.edge(e.from, e.to, 'e'+i); return ed && ed.points ? ed.points : [];\n"
   "    });\n"
   "    var gr = g.graph();\n"
   "    return {width:gr.width, height:gr.height, pos:pos, points:points};\n"
   "  }\n")
  "The layout-engine adapter: sized boxes + from→to edges in, positions +
routed points + size out.  Wide fans are staggered across ranks via edge
`minlen' (the `unflatten' technique): a target's leaf supporters, once past
`FAN_WRAP_MIN', wrap into a balanced grid of about √n rows rather than one
very wide row.  The sole dagre-specific piece.")

(defconst argdown--fold-layout-js
  (concat
   "  function edgePath(pts){\n"
   "    if(pts.length < 3) return 'M' + pts.map(function(p){ return p.x+' '+p.y; }).join(' L');\n"
   "    var d = 'M' + pts[0].x + ' ' + pts[0].y;\n"
   "    for(var i=1;i<pts.length-1;i++){\n"
   "      var xc=(pts[i].x+pts[i+1].x)/2, yc=(pts[i].y+pts[i+1].y)/2;\n"
   "      d += ' Q ' + pts[i].x + ' ' + pts[i].y + ' ' + xc + ' ' + yc;\n"
   "    }\n"
   "    return d + ' L ' + pts[pts.length-1].x + ' ' + pts[pts.length-1].y;\n"
   "  }\n"
   "  function layout(){\n"
   "    var vis = visibleSet();\n"
   "    var boxes = [], links = [], els = [];\n"
   "    root.querySelectorAll('.argdown-node').forEach(function(n){\n"
   "      var t = n.getAttribute('data-id');\n"
   "      n.style.display = vis[t] ? '' : 'none';\n"
   "      if(!vis[t]) return;\n"
   "      var r = n.querySelector('rect:not(.argdown-fold-stack)');\n"
   "      var body = n.querySelector('.argdown-node-body');\n"
   "      if(body){\n"
   "        var h = Math.ceil(body.scrollHeight) + 2;\n"
   "        r.setAttribute('height', h);\n"
   "        var fo = n.querySelector('foreignObject');\n"
   "        if(fo) fo.setAttribute('height', h);\n"
   "      }\n"
   "      boxes.push({id:t, w:+r.getAttribute('width'), h:+r.getAttribute('height')});\n"
   "    });\n"
   "    root.querySelectorAll('.argdown-edge').forEach(function(e){\n"
   "      var f = e.getAttribute('data-from'), t = e.getAttribute('data-to'), on = vis[f] && vis[t];\n"
   "      e.style.display = on ? '' : 'none';\n"
   "      var hit = e.nextElementSibling;\n"
   "      if(hit && hit.classList.contains('argdown-edge-hit')) hit.style.display = on ? '' : 'none';\n"
   "      if(on){ links.push({from:f, to:t}); els.push(e); }\n"
   "    });\n"
   "    var res = place(boxes, links);\n"
   "    boxes.forEach(function(b){\n"
   "      var p = res.pos[b.id];\n"
   "      byId[b.id].setAttribute('transform', 'translate(' + (p.x-b.w/2) + ',' + (p.y-b.h/2) + ')');\n"
   "    });\n"
   "    els.forEach(function(el, i){\n"
   "      var pts = res.points[i];\n"
   "      if(pts && pts.length){\n"
   "        var dp = edgePath(pts);\n"
   "        el.setAttribute('d', dp);\n"
   "        var hit = el.nextElementSibling;\n"
   "        if(hit && hit.classList.contains('argdown-edge-hit')) hit.setAttribute('d', dp);\n"
   "      }\n"
   "    });\n"
   "    var svg = root.querySelector('svg');\n"
   "    svg.setAttribute('width', res.width); svg.setAttribute('height', res.height);\n"
   "    svg.setAttribute('viewBox', '0 0 ' + res.width + ' ' + res.height);\n"
   "    marks();\n"
   "    var vp = root.querySelector('.argdown-viewport');\n"
   "    if(vp) vp.style.visibility = 'visible';\n"
   "  }\n")
  "Fit each visible box's height to its wrapped text (`scrollHeight' — a
layout metric in CSS pixels, so it is independent of the browser's zoom), then
gather the boxes+edges, ask the adapter to place them, and write back
transforms, edge d, svg size, fold markers — and finally reveal the content
group (it ships `visibility:hidden' so the un-positioned stack never paints;
the reader sees the laid-out map appear, not a stack flinging into place).")

(defconst argdown--flash-js
  (concat
   "  function flash(n){\n"
   "    n.classList.remove('argdown-flash');\n"
   "    void n.getBoundingClientRect();\n"
   "    n.classList.add('argdown-flash');\n"
   "    setTimeout(function(){ n.classList.remove('argdown-flash'); }, 800);\n"
   "  }\n")
  "Pulse a node's border to catch the eye on arrival: remove `argdown-flash',
force a reflow (so re-adding restarts the animation even on a repeat), add it,
and clear it once the pulse is done.")

(defconst argdown--fold-click-js
  (concat
   "  nodes.forEach(function(n){\n"
   "    n.style.cursor = 'pointer';\n"
   "    n.addEventListener('click', function(ev){\n"
   "      if(ev.target.closest('a')) return;\n"
   "      if(window.getSelection && String(window.getSelection()).length) return;\n"
   "      var t = n.getAttribute('data-id');\n"
   "      folded[t] = !folded[t];\n"
   "      layout();\n"
   "      n.scrollIntoView({block:'center', inline:'center'});\n"
   "      flash(n);\n"
   "    });\n"
   "  });\n")
  "Click a box to toggle its fold and relayout — unless the click landed on a
real link (it navigates) or on a live text selection (the reader is sweeping
the sentence to annotate it, not folding).  The relayout can fling the clicked
box far (a wide subtree collapsing re-packs the whole map), so afterwards the
box is scrolled to the centre of the view and `flash'ed — the claim you folded
stays where you are looking, and the eye catches where it landed.")

(defconst argdown--legend-toggle-js
  (concat
   "  var lt = root.querySelector('.argdown-legend-toggle');\n"
   "  if(lt){ lt.addEventListener('click', function(){\n"
   "    lt.closest('.argdown-legend').classList.toggle('argdown-legend-collapsed');\n"
   "  }); }\n")
  "Fold the legend body away (and back) when its toggle is clicked.")

(defconst argdown--trace-js
  (concat
   "  if(window.matchMedia && matchMedia('(hover: hover)').matches){\n"
   "    var mapEl = root;\n"
   "    nodes.forEach(function(n){\n"
   "      var id = n.getAttribute('data-id');\n"
   "      n.addEventListener('mouseenter', function(){\n"
   "        mapEl.classList.add('argdown-tracing');\n"
   "        edges.forEach(function(e){\n"
   "          e.classList.toggle('argdown-edge-hl',\n"
   "            e.getAttribute('data-from')===id || e.getAttribute('data-to')===id);\n"
   "        });\n"
   "      });\n"
   "      n.addEventListener('mouseleave', function(){\n"
   "        mapEl.classList.remove('argdown-tracing');\n"
   "        edges.forEach(function(e){ e.classList.remove('argdown-edge-hl'); });\n"
   "      });\n"
   "    });\n"
   "  }\n")
  "Hover a node to trace its web: the map takes `argdown-tracing' (dimming
every edge) and the node's incident edges take `argdown-edge-hl' (lit to full
strength), so a dense map is read one claim at a time.  Wired only where a
pointer can hover (`matchMedia('(hover: hover)')') — on a touch device a tap is
a click, so tracing there would fire on the same tap as the fold; a tap folds
instead.  `nodes'/`edges' are the handles from `argdown--fold-dom-js'.")

(defconst argdown--edge-nav-js
  (concat
   "  root.querySelectorAll('.argdown-edge-hit').forEach(function(h){\n"
   "    h.addEventListener('click', function(ev){\n"
   "      var f = byId[h.getAttribute('data-from')], t = byId[h.getAttribute('data-to')];\n"
   "      if(!f || !t) return;\n"
   "      function far(n){ var r = n.getBoundingClientRect();\n"
   "        return Math.hypot(r.left + r.width/2 - ev.clientX, r.top + r.height/2 - ev.clientY); }\n"
   "      var target = far(f) >= far(t) ? f : t;\n"
   "      target.scrollIntoView({behavior:'smooth', block:'center', inline:'center'});\n"
   "      flash(target);\n"
   "    });\n"
   "  });\n")
  "Tap an edge to travel it: glide (a smooth-scroll, not a jump) to its far end
— the endpoint farther from where the finger landed, the node you are not at —
and `flash' it on arrival.  So a tap near a premise reaches the argument using
it, and a tap near a conclusion reaches the argument supporting it; the edge
runs both ways.  The tap band paints above the nodes, so a tap within a few px
of where an edge meets a box travels the edge rather than folding the box: a
small dead-zone at the node's rim, the price of a finger-sized target.")

(defconst argdown--layout-js
  (concat "function argdownInitAll(){\n"
          "  document.querySelectorAll('.argdown-map').forEach(function(root){\n"
          "    if(root.dataset.argdownReady) return;\n"
          "    root.dataset.argdownReady = '1';\n"
          argdown--fold-dom-js
          argdown--fold-visible-js
          argdown--fold-mark-js
          argdown--place-js
          argdown--fold-layout-js
          argdown--flash-js
          argdown--fold-click-js
          "  layout();\n"
          argdown--legend-toggle-js
          argdown--trace-js
          argdown--edge-nav-js
          "  });\n"
          "}\n"
          "if(document.readyState === 'loading')"
          " document.addEventListener('DOMContentLoaded', argdownInitAll);\n"
          "else argdownInitAll();\n")
  "Lay out every map on the page, each scoped to its own `root': DOM handles,
fold visibility, the fold markers, the layout-engine adapter, the layout pass,
the arrival flash, the click wiring, the legend toggle, the hover-trace, and
the edge-tap travel.  Runs when the maps are in the DOM; the `argdownReady'
flag makes a repeat call a no-op.")

(defun argdown--map-grades (model)
  "Alist of node title → (GRADE . COLOUR) for nodes whose tags name an
epistemic rung (`argdown--epistemic-tag-colors')."
  (let (out)
    (dolist (key '(statements arguments))
      (dolist (e (alist-get key model))
        (let* ((o (cdr e))
               (title (alist-get 'title o))
               (grade (cl-some (lambda (tag)
                                 (and (assoc tag argdown--epistemic-tag-colors) tag))
                               (alist-get 'tags o))))
          (when (and title grade)
            (push (cons title (cons grade (cdr (assoc grade argdown--epistemic-tag-colors))))
                  out)))))
    out))

(defun argdown--node-badge (grade color)
  "A strength badge stating GRADE, filled with its house COLOR, riding the box."
  (format (concat "<g class=\"argdown-badge\" transform=\"translate(0,-15)\">"
                  "<rect width=\"%d\" height=\"14\" rx=\"2\" fill=\"%s\"/>"
                  "<text x=\"4\" y=\"11\" font-size=\"10\" fill=\"#fff\">%s</text></g>")
          (+ 8 (* 6 (length grade))) color (argdown--xml-escape grade)))

(defun argdown--strength-labels (in)
  "Weakest-link *labels* to badge propagated nodes — the reading companion of
`argdown--strength-colors', keeping the capping link's own word rather than
its hue.  Return (CONCLUSION-LABELS . ARGUMENT-LABELS), each an alist
title→(WORD . RANK): an argument wears its weakest link — the lowest-ranked of
its premises' epistemic tags and its inference forces — named with that link's
word; an untagged conclusion inherits its strongest concluding argument's
weakest link.  Only directly-tagged premises and marked inference forces carry
a word here, so a premise whose strength is itself propagated adds no label."
  (let* ((model (argdown--json in))
         (tag (make-hash-table :test 'equal))        ; statement title → (word . rank)
         (per-concl (make-hash-table :test 'equal))  ; conclusion → list of (word . rank)
         (arg-labels nil))
    (dolist (s (alist-get 'statements model))
      (let* ((st (cdr s))
             (title (alist-get 'title st))
             (word (cl-some (lambda (tg) (and (assoc tg argdown--epistemic-tag-rank) tg))
                            (alist-get 'tags st))))
        (when (and title word)
          (puthash title (cons word (cdr (assoc word argdown--epistemic-tag-rank))) tag))))
    (dolist (a (alist-get 'arguments model))
      (let* ((arg (cdr a))
             (atitle (alist-get 'title arg))
             (pcs (alist-get 'pcs arg))
             (concl (cl-some (lambda (m) (and (equal (alist-get 'role m) "main-conclusion")
                                              (alist-get 'title m)))
                             pcs))
             (fword nil) (frank nil)
             (weakest nil))
        (dolist (m pcs)
          (let* ((inf (alist-get 'inference m))
                 (f (and inf (alist-get 'force (alist-get 'data inf))))
                 (fr (and f (cdr (assoc f argdown--inference-force-ranks)))))
            (when (and fr (or (not frank) (< fr frank)))
              (setq frank fr fword f))))
        (when fword (setq weakest (cons fword frank)))
        (dolist (m pcs)
          (when (equal (alist-get 'role m) "premise")
            (let ((pt (gethash (alist-get 'title m) tag)))
              (when (and pt (or (not weakest) (< (cdr pt) (cdr weakest))))
                (setq weakest pt)))))
        (when weakest
          (when atitle (push (cons atitle weakest) arg-labels))
          (when concl (puthash concl (cons weakest (gethash concl per-concl)) per-concl)))))
    (let (concl-labels)
      (maphash (lambda (title ws)
                 (let ((best (car ws)))
                   (dolist (w (cdr ws)) (when (> (cdr w) (cdr best)) (setq best w)))
                   (push (cons title best) concl-labels)))
               per-concl)
      (cons concl-labels (nreverse arg-labels)))))

(defun argdown--legend-relations (model)
  "The `argdown--relation-styles' rows for the relation types present in MODEL's
edges, kept in the styles' canonical order."
  (let ((present (delete-dups (mapcar (lambda (e) (nth 2 e)) (argdown--map-edges model)))))
    (seq-filter (lambda (row) (member (car row) present)) argdown--relation-styles)))

(defun argdown--legend-epistemic (in model)
  "Alist (COLOUR . LABEL) for every epistemic colour the map renders, weakest→
strongest: each directly applied tag (labelled by its word) at its rank, plus
each propagated-strength rank present that no direct tag already covers
(labelled by its rung on the scale).  So a border/fill tint is never a hue with
no legend row."
  (let ((byrank (make-hash-table)))
    (dolist (g (argdown--map-grades model))       ; g = (title . (tag . colour))
      (let* ((tag (cadr g))
             (r (cdr (assoc tag argdown--epistemic-tag-rank))))
        (when r
          (let ((cur (gethash r byrank)))
            (puthash r (if (and cur (not (member tag (split-string cur ", "))))
                           (concat cur ", " tag)
                         (or cur tag))
                     byrank)))))
    (let* ((sc (argdown--strength-colors in))
           (light (mapcar (lambda (c) (argdown--lighten c 0.7)) argdown--epistemic-ramp)))
      (dolist (h (append (mapcar #'cdr (car sc)) (mapcar #'cdr (cdr sc))))
        (let ((r (or (cl-position h argdown--epistemic-ramp :test #'equal)
                     (cl-position h light :test #'equal))))
          (when (and r (not (gethash r byrank)))
            (puthash r (car (rassoc r (seq-take argdown--epistemic-tag-rank 10))) byrank)))))
    (let (rows)
      (dolist (r (sort (hash-table-keys byrank) #'<))
        (push (cons (nth r argdown--epistemic-ramp) (gethash r byrank)) rows))
      (nreverse rows))))

(defun argdown--legend-relation-row (row)
  "A legend line for relation-style ROW: a miniature of its own line (colour,
dash, end arrow, and a start arrow for the double-headed) beside its gloss."
  (let ((type (nth 0 row)) (stroke (nth 1 row)) (dash (nth 2 row))
        (double (nth 3 row)) (gloss (nth 4 row)))
    (concat
     "<div class=\"argdown-legend-row\">"
     (format (concat "<svg class=\"argdown-legend-swatch\" width=\"34\" height=\"12\">"
                     "<line x1=\"3\" y1=\"6\" x2=\"27\" y2=\"6\" stroke=\"%s\" stroke-width=\"2\""
                     "%s marker-end=\"url(#argdown-arrow)\"%s/></svg>")
             stroke
             (if (string-empty-p dash) "" (format " stroke-dasharray=\"%s\"" dash))
             (if double " marker-start=\"url(#argdown-arrow)\"" ""))
     (format "<span><b>%s</b> — %s</span>"
             (argdown--xml-escape type) (argdown--xml-escape gloss))
     "</div>")))

(defun argdown--legend-epistemic-row (pair)
  "A legend line for epistemic PAIR (COLOUR . LABEL): a colour chip and its name."
  (format (concat "<div class=\"argdown-legend-row\">"
                  "<span class=\"argdown-legend-chip\" style=\"background:%s\"></span>"
                  "<span>%s</span></div>")
          (car pair) (argdown--xml-escape (cdr pair))))

(defun argdown--map-legend (in model)
  "The map's colour key as an HTML panel — every relation type and every
epistemic colour present, each with its swatch.  Starts collapsed
(`argdown-legend-collapsed', body hidden) so it never covers the map; the
toggle opens it.  Empty string when the map carries no coloured relation or
grade.  Sits in the map corner."
  (let ((rels (argdown--legend-relations model))
        (epi (argdown--legend-epistemic in model)))
    (if (not (or rels epi)) ""
      (concat
       "<div class=\"argdown-legend argdown-legend-collapsed\">"
       "<button class=\"argdown-legend-toggle\" type=\"button\">Legend</button>"
       "<div class=\"argdown-legend-body\">"
       (when rels
         (concat "<div class=\"argdown-legend-head\">Relations</div>"
                 (mapconcat #'argdown--legend-relation-row rels "")))
       (when epi
         (concat "<div class=\"argdown-legend-head\">Strength</div>"
                 (mapconcat #'argdown--legend-epistemic-row epi "")
                 "<div class=\"argdown-legend-note\">Node border/fill = propagated"
                 " strength, same scale (fill paler).</div>"))
       "</div></div>"))))

(defconst argdown--map-css
  (concat
   "<style>"
   ".argdown-map{position:relative;}"
   ".argdown-map .argdown-node-body{font:13px/1.35 system-ui,-apple-system,sans-serif;"
   "padding:5px 7px;box-sizing:border-box;color:#111;"
   "-webkit-user-select:text;user-select:text;}"
   ".argdown-map .argdown-node-title{font-weight:600;margin-bottom:2px;}"
   ".argdown-map .argdown-node-text{font-weight:400;}"
   ".argdown-map foreignObject{overflow:visible;}"
   ".argdown-map .argdown-edge{opacity:.35;transition:opacity .1s;}"
   ".argdown-map .argdown-edge-hit{fill:none;stroke:transparent;stroke-width:12;"
   "pointer-events:stroke;cursor:pointer;}"
   ".argdown-map.argdown-tracing .argdown-edge{opacity:.08;}"
   ".argdown-map.argdown-tracing .argdown-edge.argdown-edge-hl{opacity:1;stroke-width:2.5;}"
   "@keyframes argdown-flash{0%{stroke:#1a73e8;stroke-width:5;}25%{stroke-width:1;}"
   "50%{stroke:#1a73e8;stroke-width:5;}75%{stroke-width:1;}100%{stroke:#1a73e8;stroke-width:5;}}"
   ".argdown-map .argdown-node.argdown-flash > rect:not(.argdown-fold-stack){animation:argdown-flash .7s ease-out;}"
   ".argdown-map .argdown-legend{position:absolute;top:8px;right:8px;"
   "font:12px/1.4 system-ui,-apple-system,sans-serif;background:rgba(255,255,255,.94);"
   "border:1px solid #ccc;border-radius:6px;padding:6px 8px;max-width:19em;"
   "box-shadow:0 1px 4px rgba(0,0,0,.15);}"
   ".argdown-map .argdown-legend-toggle{font:inherit;font-weight:600;cursor:pointer;"
   "background:none;border:0;padding:0;color:#333;}"
   ".argdown-map .argdown-legend-toggle::after{content:' \\25BE';}"
   ".argdown-map .argdown-legend-collapsed .argdown-legend-toggle::after{content:' \\25B8';}"
   ".argdown-map .argdown-legend-collapsed .argdown-legend-body{display:none;}"
   ".argdown-map .argdown-legend-body{margin-top:5px;}"
   ".argdown-map .argdown-legend-head{font-weight:600;margin:5px 0 2px;color:#555;}"
   ".argdown-map .argdown-legend-row{display:flex;align-items:center;gap:6px;margin:1px 0;}"
   ".argdown-map .argdown-legend-chip{display:inline-block;width:14px;height:14px;"
   "border-radius:3px;flex:none;}"
   ".argdown-map .argdown-legend-note{margin-top:4px;color:#777;font-size:11px;}"
   "</style>")
  "Scoped styling: the node bodies (a readable HTML column, title bold over
prose, text selectable — the hypothes.is anchor rides `user-select:text' — and
`overflow:visible' so text shows through the placeholder box until the layout
fits it); the edges, which recede at rest (translucent) and, while the map is
`argdown-tracing', dim further except the hovered node's `argdown-edge-hl' set;
the corner legend (`position:absolute' on the `position:relative' map, a
`-collapsed' class the toggle flips to fold the body away, the ▾/▸ caret
tracking it); and the `argdown-flash' keyframes that pulse a just-folded
node's border.")

(defun argdown--map-html (in)
  "Render INPUT's argument model as one map fragment: a single
`.argdown-map' element carrying its inline SVG.  It holds no engine and no
styling of its own — those are shared, emitted once per page by
`argdown--runtime-html' — so a page with many maps carries the heavy layout
code just once, and each stored result stays small."
  (let* ((model (argdown--json in))
         (grades (argdown--map-grades model))
         (slabels (let ((sl (argdown--strength-labels in)))
                    (append (car sl) (cdr sl))))
         (colors (argdown--strength-colors in))
         (borders (car colors)) (fills (cdr colors))
         (nodes (mapconcat
                 (lambda (nd)
                   (let* ((id (nth 0 nd)) (title (nth 1 nd)) (kind (nth 2 nd))
                          (gd (cdr (assoc title grades)))
                          (sl (and (not gd) (cdr (assoc title slabels))))
                          (b (cdr (assoc title borders)))
                          (f (cdr (assoc title fills)))
                          (g (argdown--node-g id title (argdown--node-html model title kind)))
                          (g (if b (string-replace "stroke=\"#888\""
                                                   (format "stroke=\"%s\"" b) g) g))
                          (g (if f (string-replace "fill=\"#fff\""
                                                   (format "fill=\"%s\"" f) g) g))
                          (g (cond
                              (gd (string-replace
                                   "</g>"
                                   (concat (argdown--node-badge (car gd) (cdr gd)) "</g>") g))
                              (sl (string-replace
                                   "</g>"
                                   (concat (argdown--node-badge
                                            (car sl) (nth (cdr sl) argdown--epistemic-ramp))
                                           "</g>") g))
                              (t g))))
                     g))
                 (argdown--map-nodes model) "\n")))
    (concat
     "<div class=\"argdown-map\"><svg>\n"
     "<defs><marker id=\"argdown-arrow\" viewBox=\"0 0 10 10\" refX=\"9\" refY=\"5\""
     " markerWidth=\"7\" markerHeight=\"7\" orient=\"auto-start-reverse\">"
     "<path d=\"M0,0 L10,5 L0,10 z\" fill=\"context-stroke\"/></marker></defs>\n"
     "<g class=\"argdown-viewport\" style=\"visibility:hidden\">\n"
     nodes "\n" (argdown--map-edges-svg model) "\n"
     "</g>\n</svg>" (argdown--map-legend in model) "</div>\n")))

(defun argdown--runtime-html ()
  "The one-per-page runtime shared by every map: the layout engine, the map
CSS (injected into the head), and `argdownInitAll'.  The `window.argdownInitAll'
guard keeps extra copies harmless when a loaded page is stitched from several
exported pieces — the engine installs from the first, the rest no-op."
  (concat
   "<script data-argdown-runtime>\n"
   "if(!window.argdownInitAll){\n"
   (argdown--dagre-js) "\n"
   "document.head.insertAdjacentHTML('beforeend', "
   (replace-regexp-in-string "</" "<\\\\/" (json-encode argdown--map-css)) ");\n"
   argdown--layout-js
   "}\n</script>"))

(defun argdown--inject-runtime (output _backend _info)
  "Export filter: emit the shared map runtime once per page.  If OUTPUT holds a
map fragment but no runtime, insert `argdown--runtime-html' just before the
first map; otherwise return OUTPUT unchanged."
  (if (and (string-match-p "class=\"argdown-map\"" output)
           (not (string-match-p "data-argdown-runtime" output)))
      (let ((pos (string-match "<div class=\"argdown-map\"" output)))
        (concat (substring output 0 pos)
                (argdown--runtime-html) "\n"
                (substring output pos)))
    output))
(add-to-list 'org-export-filter-final-output-functions #'argdown--inject-runtime)
