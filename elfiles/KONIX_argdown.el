;; [[id:4a21535e-8f34-4a0b-8014-bc862bda9785::KONIX_argdown-mode][KONIX_argdown-mode]]
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
;; KONIX_argdown-mode ends here

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

(defconst argdown--iframe-reset-style
  "<style>html,body{margin:0;padding:0;height:100%}.argdown-figure{margin:0;height:100%}argdown-map{display:block;height:100%}svg a[*|href]{cursor:pointer}svg a[*|href] text[font-weight=\"bold\"]{fill:#1a73e8;text-decoration:underline}</style>"
  "Strip the iframe's default body margin and let Argdown's web-component
fill the fixed viewport (its toolbar — zoom-lock, fullscreen, source toggle —
is the component's own).  The rest is the *link affordance*: the component
draws every node alike, so a node we made clickable (its `<a>' got an injected
`xlink:href') is otherwise indistinguishable.  We mark it — the bold title
line turns blue + underlined, the cursor a pointer — by styling `a[*|href]'
(the `*|' matches the namespaced `xlink:href'); plain nodes and edge anchors
have no href and stay untouched.  This document-level CSS reaches the SVG even
though it is *slotted* light-DOM inside the component (slotted content is
styled by the host document, not the shadow tree).")

(defconst argdown--iframe-click-guard
  ;; concat + \n: short source lines, and a multi-line script in the emitted
  ;; #+RESULTS rather than one giant line.
  (concat
   "<script>\n"
   "(function(){\n"
   "  var sx=null, sy=null, moved=false;\n"
   "  addEventListener('pointerdown', function(e){ sx=e.clientX; sy=e.clientY; moved=false; }, true);\n"
   "  addEventListener('pointermove', function(e){\n"
   "    if(sx!==null && Math.hypot(e.clientX-sx, e.clientY-sy) > 6) moved=true; }, true);\n"
   "  addEventListener('click', function(e){\n"
   "    if(moved){ e.preventDefault(); e.stopPropagation(); moved=false; } }, true);\n"
   "})();\n"
   "</script>")
  "The one bit of behaviour we add to the web-component: cancel the click that
*ends a drag*.  Argdown's component pans the map on pointer-drag but doesn't
suppress the trailing `click', so releasing a pan over a source node fired its
link — opening Légifrance mid-pan.  This capture-phase guard remembers whether
the pointer moved past ~6px between `pointerdown' and `click'; if so it
`preventDefault'+`stopPropagation's the click (no navigation) — a still click
passes through and follows the link.  Capture phase + a window listener run
before the component's own handlers, so it works without touching the
component.  It is NOT a pan/zoom reimplementation — the component still does
all the panning and zooming; we only veto the spurious end-of-drag click.")

(defconst argdown--open-in-tab
  ;; concat + \n: short source lines, and a readable multi-line script in the
  ;; emitted #+RESULTS rather than one giant line.
  (concat
   "<button id=\"argdown-open-tab\" title=\"open this map in a new tab\""
   " style=\"position:fixed;top:8px;right:8px;z-index:2147483647;"
   "font:13px/1 sans-serif;padding:6px 9px;border:1px solid #ccc;border-radius:6px;"
   "background:#fff;color:#1a73e8;cursor:pointer;display:none\">"
   "⛶ open in new tab</button>\n"
   "<script>\n"
   "(function(){\n"
   "  var b=document.getElementById('argdown-open-tab');\n"
   "  if(!b) return;\n"
   "  if(window.top===window.self){ b.remove(); return; }\n"
   "  b.style.display='block';\n"
   "  b.addEventListener('click', function(){\n"
   "    var html='<!DOCTYPE html>\\n'+document.documentElement.outerHTML;\n"
   "    var url=URL.createObjectURL(new Blob([html],{type:'text/html'}));\n"
   "    window.open(url,'_blank');\n"
   "  });\n"
   "})();\n"
   "</script>")
  "An *open-in-tab* button for the publish iframe.  The web-component's own
fullscreen button is not always a real fullscreen: depending on the browser and
the embedding page's permissions policy it only fills the 70vh `srcdoc' *stage*,
not the screen — so there is no fullscreen view.  This button is the way out: on
click it serialises the iframe's *own* document to a `blob:' URL and opens it as
a top-level tab, where the map fills the whole viewport (and the component's
fullscreen then works against the full screen).  The publish path has no
standalone URL — the map lives only inside `srcdoc' — so re-serialising the live
document is the only handle we have.  Guarded on `window.top !== window.self': it
shows only when framed, and removes itself in the resulting top-level tab (and in
`konix/argdown-preview', already top-level), so it never clutters a full view.")

(defun argdown--srcdoc-escape (html)
  "Escape HTML for a double-quoted `srcdoc' attribute value.
Escape `&', `<', `>', `\"' and `=' (escape `&' first) — the browser peels
off exactly one layer before parsing the value as a document, so inner
entities like `&#39;' survive and the tags come back intact.  `<'/`>' must
go too: leaving them raw lets Hugo's link rewriter find tags *inside* the
attribute and corrupt them.  And `=' must go because that rewriter is a
blunt `s|href=|target=\"_blank\" href=|' sed: the web-component pulls a CDN
`<link href=…>' stylesheet, and even with the `<' escaped the literal text
`href=' is still there for the sed to match — injecting raw quotes that
truncate the srcdoc.  Encoding every `=' as `&#61;' leaves nothing for it
to match; the browser decodes `&#61;'→`=' in the same single pass.  Runs of
blank lines are collapsed to a single newline (a blank line ends a Goldmark
type-6 HTML block); real newlines stay, so the emitted attribute is readable
in the org source rather than one giant line."
  (let* ((s (replace-regexp-in-string "&" "&amp;" html t t))
         (s (replace-regexp-in-string "<" "&lt;" s t t))
         (s (replace-regexp-in-string ">" "&gt;" s t t))
         (s (replace-regexp-in-string "\"" "&quot;" s t t))
         (s (replace-regexp-in-string "=" "&#61;" s t t))
         (s (replace-regexp-in-string "\n\\(?:[ \t]*\n\\)+" "\n" s)))
    s))

(defun argdown--document (content-html)
  "Wrap CONTENT-HTML in a standalone HTML document.
This is the single source of truth for both destinations: the publish path
embeds it in the iframe (`argdown--iframe-wrap'), and `konix/argdown-preview'
opens it in a browser — so the preview is faithful by construction.  CONTENT
is Argdown's web-component (it carries its own CDN css/js and toolbar); the
document adds a margin reset + link affordance (`argdown--iframe-reset-style')
and the end-of-drag click guard (`argdown--iframe-click-guard'), plus the
framed-only open-in-tab escape hatch (`argdown--open-in-tab').  Newlines
between parts keep the embedded form readable in the org source."
  (concat "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\">\n"
          argdown--iframe-reset-style "\n</head>\n<body>\n"
          content-html "\n" argdown--iframe-click-guard
          "\n" argdown--open-in-tab
          "\n</body></html>"))

(defun argdown--iframe-wrap (content-html)
  "Wrap CONTENT-HTML in a srcdoc iframe (the publish path), entity-escaping
`argdown--document' so it survives Goldmark and the link rewriter (see
`argdown-helpers' narrative).  `allowfullscreen' lets the web-component's
fullscreen button expand the map out of its 70vh stage."
  (format
   "<iframe class=\"argdown-frame\" allowfullscreen style=\"width:100%%;height:70vh;min-height:320px;border:0\" srcdoc=\"%s\"></iframe>"
   (argdown--srcdoc-escape (argdown--document content-html))))

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

(defun argdown--map-svg (in)
  "Return Argdown's own SVG map of INPUT file with a clickable source link
injected into each node that cites one (matched by `xlink:title' against the
`argdown json' link model — see `argdown--inject-links').  We ride Argdown's
renderer — its layout, argument→conclusion edges, styling — and only splice
in the href.  The `<?xml?>'/doctype prolog is stripped so the SVG embeds in
an HTML body."
  (let* ((raw (argdown--stdout "svg" in))
         (svg (if (string-match "<svg" raw) (substring raw (match-beginning 0)) raw)))
    (argdown--inject-links svg (argdown--source-urls in))))

(defun argdown--web-component (in)
  "Return Argdown's own web-component HTML for INPUT file, with a clickable
source link injected into each node that cites one.  `argdown web-component'
emits the CDN `<link>'/`<script>' tags plus a `<figure>'/`<argdown-map>'
whose `<div slot=\"map\">' holds the very SVG `argdown map' produces — same
`<a xlink:title>' nodes `argdown--inject-links' splices an `xlink:href' into.
We ride the whole viewer (its map, zoom, fullscreen and source-toggle
toolbar); the href is the only thing that's ours."
  (argdown--inject-links
   (org-babel-eval
    (format "argdown web-component --stdout --silent %s" (shell-quote-argument in))
    "")
   (argdown--source-urls in)))

(defun argdown--publish-content (in)
  "The published view of INPUT file: Argdown's web-component, whose slotted
map nodes carry the clickable source links (`[label](url)' → real `<a href>')."
  (argdown--web-component in))

(defun argdown--publish-iframe (in)
  "Return the published map for INPUT file as a web-component srcdoc iframe."
  (argdown--iframe-wrap (argdown--publish-content in)))

(defun argdown--render (fmt in out)
  "Render INPUT file's map to file OUT in FMT, using Argdown's renderer.
`svg' goes through `argdown--map-svg' (so the saved map is clickable too);
`dot'/`gv' write Argdown's DOT; `pdf' uses Argdown's bundled Graphviz (it
refuses stdout, so via a temp folder); png/jpg/webp are an ImageMagick step
on the svg."
  (pcase fmt
    ("svg" (with-temp-file out (insert (argdown--map-svg in))))
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
       (with-temp-file svg (insert (argdown--map-svg in)))
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

(defconst argdown--epistemic-tag-colors
      '(("affirmation péremptoire" . "#d73027")
        ("témoignage d'une partie" . "#fc8d59")
        ("témoignage de tiers"     . "#fee08b")
        ("présomption"             . "#d9ef8b")
        ("constat"                 . "#91cf60")
        ("acte authentique"        . "#1a9850"))
      "House epistemic-strength scale for argument-map tags, weakest→strongest,
    as a dialed-back red→green (RdYlGn) ramp.  A statement/argument tagged
    `#(<level>)' takes that colour as its node border; the *pure* red/green are
    left to the relation edges (attack/support), so the tags use the muted RdYlGn
    hues.  A cross-note convention — injected into *every* map by
    `argdown--frontmatter', never redefined per note.  See the \"Epistemic nuance
    scale\" section.")

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
    `argdown--strength-colors'); and `model.mode: strict' when MODE is \"strict\".
    Everything under one `===' block — Argdown accepts frontmatter only at the very
    top and only once, so colours + mode must share it (a second block, or one
    lower down, is a parse error)."
      (concat
       "===\ncolor:\n    tagColors:\n"
       (mapconcat (lambda (tc) (format "        %s: \"%s\"" (car tc) (cdr tc)))
                  argdown--epistemic-tag-colors "\n")
       (argdown--color-map "statementColors" statement-colors)
       (argdown--color-map "argumentColors" argument-colors)
       (and (equal mode "strict") "\nmodel:\n    mode: strict")
       "\n==="))

    (defun argdown--compose (body params &optional statement-colors argument-colors)
      "Prepend the frontmatter + :argdown-include / :argdown-collect fragments to
    BODY per PARAMS — frontmatter first, then included premises, then collected
    notes, then BODY — joined so Argdown merges them by title.  Shared by
    `org-babel-execute:argdown' and `konix/argdown-preview', so a preview composes
    its sources exactly as the published render does.  `argdown--frontmatter'
    always leads with the house epistemic tag colours, optionally the propagated
    conclusion border colours STATEMENT-COLORS and per-argument fill colours
    ARGUMENT-COLORS (see `argdown--render-input'), and — when :argdown-mode is
    \"strict\" — folds `model.mode: strict' into that same single block (Argdown
    requires one frontmatter, at the very top, else a parse error): in strict mode
    + / - / >< between statements then read as logical entails / contrary /
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
    - :file F                -> write the map to F (svg/dot/pdf/png/jpg/webp),
                                return nil so Org inserts the [[file:F]] link
    - :results output html   -> web-component map iframe (interactive viewer:
                                zoom/fullscreen/source toggle), links clickable
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
            ((member "html" rp) (argdown--publish-iframe in))
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
      "Open the argdown src block at point as a standalone HTML document in a
    browser — the very document the publish path embeds in its iframe, so what
    you see is what you'll publish.  Sources are composed (:argdown-include /
    :argdown-collect) exactly as on render.  The viewer is Argdown's
    web-component, which loads its toolbar/zoom/fullscreen from a CDN — so the
    interactive map needs the network; offline, only the static slotted SVG
    paints."
      (interactive)
      (argdown--require-bin)
      (let ((info (org-babel-get-src-block-info 'light)))
        (unless (and info (equal (nth 0 info) "argdown"))
          (user-error "Point is not in an argdown src block"))
        (let* ((full (argdown--render-input (nth 1 info) (nth 2 info)))
               (html (argdown--with-input
                      full (lambda (in)
                             (argdown--document (argdown--publish-content in)))))
               (file (make-temp-file "argdown-preview-" nil ".html")))
          (with-temp-file file (insert html))
      (shell-command (format "clk ipfs browse '%s' &" file))
          (message "argdown preview → %s" file))))

(require 'json)
(require 'cl-lib)

(defun argdown--json (in)
  "Parse the `argdown json' model of INPUT file into an alist tree."
  (json-parse-string
   (argdown--run (format "argdown json --stdout --silent %s" (shell-quote-argument in)))
   :object-type 'alist :array-type 'list :null-object nil :false-object nil))

(defun argdown--source-urls (in)
  "Hash of each statement/argument description *text* → its first source url
(the `[label](url)' a node carries), from the `argdown json' model — the
lookup table for link injection.  One source per node: only the first link a
statement carries is kept."
  (let ((model (argdown--json in)) (h (make-hash-table :test 'equal)))
    (dolist (key '(statements arguments))
      (dolist (e (alist-get key model))
        (let* ((m (car (alist-get 'members (cdr e))))
               (url (and m (cl-some (lambda (r)
                                      (and (equal (alist-get 'type r) "link")
                                           (alist-get 'url r)))
                                    (alist-get 'ranges m)))))
          (when url (puthash (or (alist-get 'text m) "") url h)))))
    h))

(defun argdown--svg-unescape (s)
  "Decode the entities Graphviz writes into an SVG attribute, so an
`xlink:title' can be compared to the model's plain text."
  (let* ((s (replace-regexp-in-string "&#39;" "'" s t t))
         (s (replace-regexp-in-string "&#45;" "-" s t t))
         (s (replace-regexp-in-string "&quot;" "\"" s t t))
         (s (replace-regexp-in-string "&lt;" "<" s t t))
         (s (replace-regexp-in-string "&gt;" ">" s t t)))
    (replace-regexp-in-string "&amp;" "&" s t t)))

(defun argdown--inject-links (svg urls)
  "Splice xlink:href into each node <a> of SVG whose `xlink:title' matches a
text in URLS (text→url), making the whole node a clickable link to its
source.  Argdown wraps every node — and every edge — in `<a xlink:title=…>'
with no href; an edge title (e.g. \"support\") has no model match and is left
untouched.  Argdown's own layout, argument edges, and styling are preserved.
The link gets `target=\"_blank\"' (with `rel=\"noopener\"'): the map lives in
a srcdoc iframe, and a source like Légifrance sends `X-Frame-Options: DENY'
— navigating *inside* the frame would just show \"refused to connect\", so the
source must open at the top level, in a new tab."
  (with-temp-buffer
    (insert svg)
    (goto-char (point-min))
    (while (re-search-forward "<a xlink:title=\"\\([^\"]*\\)\">" nil t)
      (let* ((attr (match-string 1))
             (url (gethash (argdown--svg-unescape attr) urls)))
        (when url
          (replace-match
           (concat "<a target=\"_blank\" rel=\"noopener\" xlink:href=\""
                   url "\" xlink:title=\"" attr "\">")
           t t))))
    (buffer-string)))

(defun konix/ox-hugo--argdown-html (src-block info)
  "Return SRC-BLOCK fontified as inline-styled argdown HTML.
Chroma has no argdown lexer, so colorize with `argdown-mode' + htmlize.
`org-html-fontify-code' strips the enclosing <pre>, so re-wrap it."
  (format "<pre class=\"src src-argdown\">\n%s</pre>\n"
          (org-html-fontify-code
           (org-export-format-code-default src-block info)
           "argdown")))

(defun konix/ox-hugo-src-block--argdown (orig src-block contents info)
  "Fontify argdown src blocks with Emacs; defer everything else to ORIG."
  (if (string= (org-element-property :language src-block) "argdown")
      (konix/ox-hugo--argdown-html src-block info)
    (funcall orig src-block contents info)))

(with-eval-after-load 'ox-hugo
  (advice-add 'org-hugo-src-block :around #'konix/ox-hugo-src-block--argdown))

(defconst argdown--inference-force-ranks
  '(("non sequitur" . 0) ("ténue" . 1) ("plausible" . 2)
    ("solide" . 3) ("forte" . 4) ("déductive" . 5))
  "How strongly the premises bring the conclusion — a scale for the *inference*
(the `----'), independent of the premises' evidential weight, mapped onto the
same 0–5 rank as `argdown--epistemic-tag-colors' so the two combine by weakest
link.  Marked as inference data: `-- {force: \"<level>\"} --'.")

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
`argdown--epistemic-tag-colors' (lower = weaker) — or nil if undetermined.
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
             (rank (cl-position-if (lambda (tc) (member (car tc) tags))
                                   argdown--epistemic-tag-colors)))
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
              (push (cons title (cdr (nth rank argdown--epistemic-tag-colors))) scolors))))))
    ;; argument fill colours — each argument's own weakest link, lightened
    (dolist (a args)
      (let ((atitle (nth 0 a)) (mn (nth 2 a)))   ; seed with inference rank
        (dolist (p (nth 1 a))
          (let ((ps (argdown--strength p tag-rank concludes memo inprog)))
            (when ps (setq mn (if mn (min mn ps) ps)))))
        (when mn
          (push (cons atitle (argdown--lighten
                              (cdr (nth mn argdown--epistemic-tag-colors)) 0.7))
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

(defconst konix/argdown--marker-re
  "[ \t]*\\(\\[[^]]*\\]\\|<[^>]*>\\|([0-9]+)\\|[-+]\\|><\\|=+\\|----\\|#\\)"
  "Regexp matching the start of an argdown structural line (statement,
argument, premise number, relation, inference…), anchored at point via
`looking-at'.  Lines that don't match are description continuations.")

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
