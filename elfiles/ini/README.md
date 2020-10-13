# ini.el
**Author** Daniel Ness  

### About
This is a library with which to handle [INI](http://en.wikipedia.org/wiki/INI_file) style configuration files with 
Emacs Lisp. It has been developed as part of a larger project that I am currently working on, as a simple form of
configuration persistence.

### Features
* Conversion of INI-format to Elisp Association Lists
* Conversion of an Association List to INI-format string

### Usage
```Lisp
(require 'ini.el)

(let ((fname "/path/to/file.ini"))

  ;; To parse an ini file
  (setq txt
    (with-temp-buffer
      (insert-file-contents "/path/to/file.ini")
      (buffer-string)))
  (setq alist (ini-decode txt))

  ;; To write to an ini file
  (with-temp-buffer
    (let ((txt (ini-encode alist)))
      (insert txt)
      (append-to-file (beginning-of-buffer) (end-of-buffer) "/path/to/other/file.ini"))))
```

### License
This file is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This file is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.