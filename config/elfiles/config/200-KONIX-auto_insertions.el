(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory (expand-file-name (concat elfiles "/autoinsert/")))

(define-auto-insert
  (cons "\\.\\([hH]\\|hh\\|hpp\\)\\'" "Mon en-tête C/C++")
  '(nil
    "/**" (make-string 69 ?*) "\n"
    " * \\file "(file-name-nondirectory buffer-file-name)"\n"
    " *\n"
    " * \\author Konubinix  (konubinix@gmail.com)\n"
    " * \\date "(format-time-string "%a %H:%M:%S %d/%m/%Y" (current-time))"\n"
    " *" (make-string 69 ?*) "*/\n"
		(setq filename )

	  "#ifndef " (setq ident (concat "__" (upcase (replace-regexp-in-string "\\." "_" (file-name-nondirectory buffer-file-name))) "__")) "\n"
		"#define " ident "\n\n"
		"" _ ""
		"\n\n#endif /* " ident " */\n")
)

(define-auto-insert (cons "\\.\\(sh\\)\\'" "Mon entête shell")
  '(nil
    "#!/bin/bash\n"
    "#" (make-string 69 ?#) "\n"
    "#  \\file "(file-name-nondirectory buffer-file-name)"\n"
    "# \n"
    "#  \\author Konubinix  (konubinix@gmail.com)\n"
    "#  \\date "(format-time-string "%a %H:%M:%S %d/%m/%Y" (current-time))"\n"
    "#" (make-string 69 ?#) "\n"))

(define-auto-insert (cons "\\.\\(c\\|cpp\\|cc\\)\\'" "Mon implémentation C")
  '(nil
    "/*" (make-string 69 ?*) "\n"
    " * \\file "(file-name-nondirectory buffer-file-name)"\n"
    " *\n"
    " * \\author Konubinix  (konubinix@gmail.com)\n"
    " * \\date "(format-time-string "%a %H:%M:%S %d/%m/%Y" (current-time))"\n"
    " *" (make-string 69 ?*) "*/\n"
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
	   (nopath (file-name-nondirectory noext))
	   (ident (concat "__" (upcase nopath) "_H__")))
      )))

(define-auto-insert "\.py" "python.py")
