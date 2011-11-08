(defvar grin-hist nil)

(defvar grind-hist nil)

(defgroup grin nil
  "Run grin and grind (python replacements for grep and find) putting hits in a grep buffer."
  :group 'tools
  :group 'processes)

(defcustom grin-cmd "grin.py -i --emacs"
  "The grin command."
  :type 'string
  :group 'grin)

(defcustom grind-cmd "grind"
  "The grind command."
  :type 'string
  :group 'grin)

(defun grin ()
  (interactive)
  (let* ((cmd (read-shell-command "Command: " (concat grin-cmd " ") 'grin-hist))
         (null-device nil))
    (grep cmd)))

(defun grind ()
  (interactive)
  (let* ((args (read-shell-command "Command: " (concat grind-cmd " ") 'grind-hist))
         (cmd (concat args " | sed s/$/\\:1\\:/"))
         (null-device nil))
    (grep cmd)))

(provide 'grin)
