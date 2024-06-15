;;; javamonkey.el --- Elisp for editing Java.

;; Copyright (C) 1999 Peter Seibel

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

(require 'cl-lib)
(require 'cc-mode)

(defvar javamonkey-java-package-history nil)
(defvar javamonkey-java-source-roots nil)
(defvar javamonkey-user-name-for-comment nil
  "*User name inserted in comments - defaults to user-mail-address")

(defvar javamonkey-dwim-imports t
  "*If non-nil, causes typenames to be automatically imported.")

(defvar javamonkey-type-regexp nil
  "Regexp that matches a potential Java type name.")
;(setq javamonkey-type-regexp "[^.]\\b\\([A-Z]+_?[a-z][_A-Za-z]*\\b\\)\\s *")
(setq javamonkey-type-regexp "[^.]\\b\\([A-Z]+_?[a-z0-9][_A-Za-z0-9]*\\b\\)\\s *")
;(setq javamonkey-type-regexp "[^.]\\b\\(\\([A-Z]+_?[a-z][_A-Za-z]*\\)\\|\\([A-Z]+_?[0-9a-z][_0-9A-Za-z]*\\)\\b\\)\\s *")

(defun javamonkey-search-forward-regexp (regexp &optional bound noerror count)
  (let ((case-fold-search nil))
    (re-search-forward regexp bound noerror count)))

(defun javamonkey-search-backward-regexp (regexp &optional bound noerror count)
  (let ((case-fold-search nil))
    (re-search-backward regexp bound noerror count)))

(c-add-style
 "javamonkey-java" '("java"
                     ;(c-basic-offset . 2)
                     (c-offsets-alist . ((arglist-intro . +)
                                         (substatement-open . 0)))
                     (c-hanging-comment-ender-p . nil)))

(defun javamonkey-initialize-javamonkey ()
  (javamonkey-add-nomenclature-movement)
  (c-set-style "javamonkey-java")
  (javamonkey-slurp-imports)
  (if javamonkey-dwim-imports
      (javamonkey-prepare-auto-import)))

(defun javamonkey-add-nomenclature-movement ()
  "Add keybindings so standard movement commands grok InterCaps"
  (define-key java-mode-map "\M-b" 'c-backward-into-nomenclature)
  (define-key java-mode-map "\M-f" 'c-forward-into-nomenclature)
  (define-key java-mode-map [(meta backspace)]
    'javamonkey-backward-into-nomenclature-kill-word)
  (define-key java-mode-map "\M-\^?"
    'javamonkey-backward-into-nomenclature-kill-word)
  (define-key java-mode-map "\M-d"
    'javamonkey-forward-into-nomenclature-kill-word))

(defun javamonkey-fill-to-right (s)
  "Fill from the current position on the line to the right with the
given string (or fraction thereof) until the fill-column is hit."
  (interactive "sFill String: ")
  (let ((len (length s)))
    (while (> fill-column (+ (current-column) len))
      (insert s))
    (insert (substring s 0 (- fill-column (current-column))))))

(defun javamonkey-align (pat)
  "Find the left-most occurance of a given regex in each line in the region
and insert spaces so that the text matching the regex in each line begins in
the same column. For example:

 public int foo = 10;
 public int bazblat = 20;
 public Foo x = new Foo();

aligned on the regex '=' becomes:

 public int foo     = 10;
 public int bazblat = 20;
 public Foo x       = new Foo();
"

  (interactive "sRegex to align on: ")
  (let ((max 0)
        (beg (region-beginning))
        (end (region-end)))

    ;; first find the rightmost column containing the regex
    (goto-char end)
    (end-of-line)
    (while (< beg (point))
      (cond ((looking-at pat)
             (let ((col (current-column)))
               (just-one-space)
               (setf end (- end (1+ (- col (current-column))))))
            (setq max (max max (current-column)))
            (beginning-of-line))
           (t (backward-char))))

    ;; Now space everything out.
    (goto-char beg)
    (beginning-of-line)
    (while (> end (point))
      (cond ((looking-at pat)
             (while (> max (current-column))
               (insert " ")
               (setq end (+ 1 end)))
             (beginning-of-line 2))
            (t (forward-char))))))

(defun javamonkey-tidy-parens-and-braces (arg)
  (interactive "P")
  (cl-flet ((cl-replace (pat rep)
          (goto-char (point-min))
          (if arg
              (replace-regexp pat rep)
            (query-replace-regexp pat rep))))
    (save-excursion
      (cl-replace "(\\s +" "(")
      (cl-replace "\\s +(" "(")
      (cl-replace "\\(if\\|while\\|for\\|catch\\|switch\\|=\\)\\s *(" "\\1 (")
      (cl-replace "\\s +)" ")")
      (cl-replace "\\(\\S +\\){" "\\1 {")
      (cl-replace "}\\(\\S +\\)" "} \\1")
      (cl-replace "}\\s +$" "}"))))

(defun javamonkey-forward-into-nomenclature-kill-word (&optional arg)
  (interactive "p")
  (let ((times (or arg 1)))
    (save-excursion
      (let ((beg (point))
            (end 0))
        (c-forward-into-nomenclature times)
        (setq end (point))
        (kill-region beg end)))))

(defun javamonkey-backward-into-nomenclature-kill-word (&optional arg)
  (interactive "p")
  (let ((times (or arg 1)))
    (save-excursion
      (let ((end (point))
            (beg 0))
        (c-backward-into-nomenclature times)
        (setq beg (point))
        (kill-region beg end)))))

(defun javamonkey-add-source-root (dir)
  (interactive "DSource root: ")
  (setq javamonkey-java-source-roots (cons dir javamonkey-java-source-roots)))


(defun javamonkey-remove-source-root ()
  (interactive)
  (let ((dir
         (read-from-minibuffer
          "Source root: "
          (car javamonkey-java-source-roots)
          nil
          nil
          'javamonkey-java-source-roots
          nil)))
    (setq javamonkey-java-source-roots
          (delete dir javamonkey-java-source-roots))))

(defun javamonkey-find-package ()
  (let ((pkg
         (or (javamonkey-current-package)
             (javamonkey-deduce-package))))
    (let* ((whole-dir (file-name-directory (buffer-file-name)))
           (root (javamonkey-extract-source-root whole-dir pkg)))
      (if (not (member root javamonkey-java-source-roots))
          (javamonkey-add-source-root root)))
    pkg))

(defun javamonkey-current-package ()
  (save-excursion
    (goto-char (point-min))
    (if (javamonkey-search-forward-regexp "^package\\s +\\([^;]+\\);" (point-max) t)
        (match-string-no-properties 1)
      nil)))

(defun javamonkey-deduce-package ()
  (let ((list javamonkey-java-source-roots)
        (cwd (file-name-directory (buffer-file-name)))
        (found nil))
    (while (and list (not found))
      (let ((entry (car list)))
        (if (javamonkey-starts-with cwd entry)
            (setq found (javamonkey-dir-to-package
                         (substring cwd (length entry))))))
      (setq list (cdr list)))
    (if found
        found
      (javamonkey-get-package))))


(defun javamonkey-check-package-statement ()
  (interactive)
  (let ((current-package (javamonkey-current-package))
        (correct-package (javamonkey-deduce-package)))
    (if (string-equal current-package correct-package)
        (message "package ok.")
      (message (concat "package wrong. should be: " correct-package " is " current-package)))))

(defun javamonkey-dir-to-package (pkgdir)
  (let* ((parts (mapcan
                 (lambda (x) (and (> (length x) 0) (list x)))
                 (split-string pkgdir "/")))
         (pkg (mapconcat (lambda (s) s) parts ".")))
    pkg))

(defun javamonkey-package-to-dir (pkg)
  (let* ((parts (split-string pkg "\\."))
         (dir (mapconcat (lambda (s) s) parts "/")))
    dir))

(defun javamonkey-extract-source-root (dir pkg)
  (let ((pkgdir (javamonkey-package-to-dir pkg)))
    (substring dir 0 (- (length dir) (+ (length pkgdir) 2)))))

(defun javamonkey-get-package ()
  (read-from-minibuffer
   "Package: "
   (car javamonkey-java-package-history)
   nil ; keymap
   nil ; read as Lisp Object
   'javamonkey-java-package-history))

(defun javamonkey-starts-with (s1 s2)
  (let ((l1 (length s1))
        (l2 (length s2)))
    (and
     (> l1 l2)
     (eq t (string-equal (substring s1 0 l2) (substring s2 0 l2))))))

(defun javamonkey-current-classname ()
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))

(defun javamonkey-debrace ()
  (interactive)
  (save-excursion
    (end-of-line)
    (backward-char 1)
    (if (not (looking-at "{"))
        (error "No opening brace at end of line")
      (progn
        (save-excursion
          (forward-line 2)
          (beginning-of-line)
          (if (not (looking-at "\\s *}\\s *$"))
              (error "No closing brace at end of third line")
            (kill-line))))
        (delete-char 1)
        (delete-horizontal-space)
        (forward-line 1)
        (beginning-of-line)
        (delete-horizontal-space)
        (delete-backward-char 1)
        (insert " "))))

(defvar javamonkey-comment-start nil)


(defun javamonkey-labeled-timestamp-comment (label)
  "Insert a labled, timestamped, and signed comment."
  (insert (or javamonkey-comment-start comment-start "#")
          (if (string-match " $" comment-start) "" " ")
          label " " (format-time-string "%Y-%m-%d")
          " <" (javamonkey-user-email) "> -- ")
  (indent-according-to-mode))

(defun javamonkey-user-email ()
  (or javamonkey-user-name-for-comment user-mail-address))

(defun javamonkey-fixme-comment ()
  "Insert a timestamped, signed FIXME comment."
  (interactive)
  (javamonkey-labeled-timestamp-comment "FIXME"))

(defun javamonkey-review-comment ()
  "Insert a timestamped, signed REVIEW comment."
  (interactive)
  (javamonkey-labeled-timestamp-comment "REVIEW"))

(defun javamonkey-optimize-comment ()
  "Insert a timestamped, signed OPTIMIZE comment."
  (interactive)
  (javamonkey-labeled-timestamp-comment "OPTIMIZE"))


;;; Importing functions.

(defvar javamonkey-imported-types ())

(defvar javamonkey-local-imported-types nil)
(make-variable-buffer-local 'javamonkey-local-imported-types)

(defun javamonkey-import-type (u)
  (interactive "p")
  (let* ((type (javamonkey-token-before-point))
        (prompt (concat "Package for type '" type "': ")))
    (if type
        (let ((package
               (or (and (= 4 u) (read-from-minibuffer prompt))
                   (javamonkey-get-package-for-import type)
                   (read-from-minibuffer prompt))))
          (javamonkey-save-imported-type type package)
          (save-excursion
            (javamonkey-find-imports)
            (insert (concat "import " package "." type ";\n")))
          (javamonkey-tidy-imports)
          (message (concat "Imported '" package "." type "'.")))
      (message "Not token found before point."))))

(defun javamonkey-import-given-type (type)
  (let ((prompt (concat "Package for type '" type "': ")))
    (let ((package
           (or (javamonkey-get-package-for-import type)
               (read-from-minibuffer prompt))))
      (javamonkey-save-imported-type type package)
      (save-excursion
        (javamonkey-find-imports)
        (insert (concat "import " package "." type ";\n")))
      (javamonkey-tidy-imports)
      (message (concat "Imported '" package "." type "'.")))))

(defun javamonkey-clear-imported-type ()
  (interactive)
  (let ((type (javamonkey-token-before-point)))
    (setq javamonkey-imported-types
          (delq (assoc type javamonkey-imported-types)
                javamonkey-imported-types))))

(defun javamonkey-clear-local-imported-types ()
  (interactive)
  (setq javamonkey-local-imported-types nil))

(defun javamonkey-clear-all-imported-types ()
  (interactive)
  (setq javamonkey-imported-types nil))

(defun javamonkey-prepare-auto-import ()
  (interactive)
  (abbrev-mode t)
  (add-hook 'pre-abbrev-expand-hook 'javamonkey-check-import nil t))

(defun javamonkey-in-comment-or-string ()
  (or (javamonkey-in-comment)
      (javamonkey-in-string)))

(defun javamonkey-in-comment ()
  (let ((syntax (c-guess-basic-syntax)))
    (or (assoc 'c syntax)
        (assoc 'comment-intro syntax)
        ; handle // comment in line with code on it.
        (let ((bol nil))
          (save-excursion
            (beginning-of-line)
            (setq bol (point)))
          (save-excursion
            (search-backward "//" bol t)))
        (javamonkey-in-slash-star))))

(defun javamonkey-in-slash-star ()
  (interactive)
  (let ((prev-start (save-excursion (search-backward "/\*" nil t)))
        (prev-end   (save-excursion (search-backward "\*/" nil t)))
        (next-start (save-excursion (search-forward "/\*" nil t)))
        (next-end   (save-excursion (search-forward "\*/" nil t))))
    (and prev-start next-end
         (or (not prev-end) (> prev-start prev-end))
         (or (not next-start) (< next-end next-start)))))

(defun javamonkey-in-string ()
  (interactive)
  (let ((bol (save-excursion
               (beginning-of-line)
               (point))))
    (let ((num-quotes 0))
      (save-excursion
        (while (search-backward "\"" bol t)
          (let ((num-slashes 0))
            (while (= (char-before) ?\\)
              (setq num-slashes (+ 1 num-slashes))
              (backward-char))
            (if (or (= num-slashes 0) (evenp num-slashes))
                (setq num-quotes (+ 1 num-quotes))))))
      (not (evenp num-quotes)))))

(defun javamonkey-check-import ()
  "Check whether the point is after a Java type name (starts with an upper
case letter and isn't all upper case) and if so if it needs to be imported.

This function is wired into abbrev expansion via javamonkey-prepare-auto-import
which can be added to java-mode-hook."
  (if (not (javamonkey-in-comment-or-string))
      (let* ((type (javamonkey-type-name-before-point))
             (prompt (concat "Package for type '" type "': ")))
        (if (and type (not (string= type (javamonkey-current-classname))))
            (if (javamonkey-needs-import-p type)
                (let ((package
                       (or (javamonkey-get-package-for-import type)
                           (read-from-minibuffer prompt))))
                  (javamonkey-add-import type package)
                  (javamonkey-save-imported-type type package)
                  (javamonkey-tidy-imports)
                  (message (concat "Imported '" package "." type "'."))))))))

(defun javamonkey-add-missing-imports ()
  "Insert missing imports."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (javamonkey-search-forward-regexp javamonkey-type-regexp (point-max) t)
      (javamonkey-check-import))))


(defun javamonkey-add-import (type package)
  (save-excursion
    (javamonkey-find-imports)
    (insert (concat "import " package "." type ";\n"))))


(defun javamonkey-needs-import-p (type)
  (not
   (or
    (assoc type javamonkey-local-imported-types)
    (string= (javamonkey-find-package)
            (cdr (assoc type javamonkey-imported-types)))
    (string= (cdr (assoc type javamonkey-imported-types)) "java.lang")
    (javamonkey-inner-class-p type))))


(defun javamonkey-inner-class-p (type)
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp
     (concat "\\(class\\|interface\\|enum\\)[ \t\n]+" type "\\b")
     (point-max) t)))


(defun javamonkey-defining-class-p ()
  (let ((type (javamonkey-token-before-point)))
    (let ((min (save-excursion
                 (beginning-of-line)
                 (point))))
      (save-excursion
        (if
            (search-backward-regexp
             (concat "\\(class\\|interface\\|enum\\)[ \t\n]+" type "\\s *")
             min t)
            (javamonkey-save-in type (javamonkey-find-package)
                                javamonkey-local-imported-types))))))


(defun javamonkey-save-imported-type (type package)
  (javamonkey-save-in type package javamonkey-imported-types)
  (javamonkey-save-in type package javamonkey-local-imported-types))

(defmacro javamonkey-save-in (key value list)
  `(if (not (assoc ,key ,list))
       (setq ,list (cons (cons ,key ,value) ,list))
     (setcdr (assoc ,key ,list) ,value)))

(defun javamonkey-get-package-for-import (type)
  (cdr (assoc type javamonkey-imported-types)))

(defun javamonkey-token-before-point ()
  (save-excursion
    (if (javamonkey-search-backward-regexp "[^_A-Za-z]\\([_A-Za-z]+\\)" (point-min) t)
        (match-string-no-properties 1)
      nil)))

(defun javamonkey-type-name-before-point ()
  "Find a type name before the point where 'type name' is defined as
starting with one or more upper case letters."
  (let ((case-fold-search nil))
    (if (save-excursion
          (javamonkey-search-backward-regexp javamonkey-type-regexp (point-min) t))
        (if (= (match-end 0) (point))
            (match-string-no-properties 1)
          nil)
        nil)))

(defun javamonkey-find-imports ()
 (catch 'done
  (if (search-backward-regexp "^import.*" (point-min) t)
      (progn
        (beginning-of-line)
        (throw 'done t))
    (progn
      (javamonkey-find-package-statement)
      (next-line 1)
      (insert "\n")))))

(defun javamonkey-find-package-statement ()
  (goto-char (point-min))
  (search-forward-regexp "^package .*" (point-max) t))



(defun javamonkey-tidy-imports ()
  (let ((beg (javamonkey-imports-beginning))
        (end (javamonkey-imports-ending)))
    (and beg end (sort-lines nil beg end))
    (save-excursion
      (goto-char end)
      (insert "\n\n") ; make sure there's more than one.
      (delete-blank-lines)
      (goto-char beg)
      (insert "\n\n") ; make sure there's more than one.
      (previous-line 1)
      (delete-blank-lines))))

(defun javamonkey-refresh-imports ()
  (interactive)
  (let ((beg (javamonkey-imports-beginning))
        (end (javamonkey-imports-ending)))
    (and beg end (kill-region beg end))
    (javamonkey-clear-local-imported-types)
    (javamonkey-add-missing-imports)))


(defun javamonkey-slurp-imports ()
  ;; Get the type of this file itself based on the file name so new
  ;; files are accounted for immediately.
  (javamonkey-save-imported-type
   (javamonkey-current-classname) (javamonkey-find-package))

  ;; Find types defined in this file.
  (let ((pkg (javamonkey-find-package))
        (pat (concat "\\(class\\|interface\\|enum\\)[ \n\t]+"
                     javamonkey-type-regexp)))
    (save-excursion
      (goto-char (point-min))
      (while (javamonkey-search-forward-regexp pat (point-max) t)
        (javamonkey-save-imported-type
         (match-string-no-properties 2) pkg)))

    (javamonkey-slurp-imports-this-directory pkg))



  (let ((beg (javamonkey-imports-beginning))
        (end (javamonkey-imports-ending)))
    (if (and beg end)
        (save-restriction
          (save-excursion
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (search-forward-regexp
                  "^import \\(.*\\)\\.\\([^.;]+\\);" (point-max) t)
            (javamonkey-save-imported-type
             (match-string-no-properties 2) (match-string-no-properties 1))))
          t))))

(defun javamonkey-slurp-imports-this-directory (pkg)
  (let* ((file-name (buffer-file-name))
         (dir-name (file-name-directory file-name))
         (other-files (directory-files dir-name nil "\.java$")))
    (mapc
     (lambda (file)
       (let ((type (file-name-sans-extension file)))
         (javamonkey-save-in type pkg javamonkey-imported-types)))
     other-files)))

(defun javamonkey-imports-beginning ()
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "^import.*" (point-max) t)
        (progn
          (beginning-of-line)
          (point))
      nil)))

(defun javamonkey-imports-ending ()
  (save-excursion
    (goto-char (point-max))
    (if (search-backward-regexp "^import.*" (point-min) t)
        (progn
          (next-line 1)
          (beginning-of-line)
          (point))
      nil)))


(defun javamonkey-switch-to-test-or-source()
  (interactive)
  (let ((package (javamonkey-current-package))
        (classname (javamonkey-current-classname)))
    (find-file
     (javamonkey-find-source-file
      (concat package "." (javamonkey-test-or-source-name classname))))))

(defun javamonkey-test-or-source-name (classname)
  "Figure out the name of the corresponding test or source class for
the given classname. (Tests end in 'Test')."
  (cond ((string-match "\\(.*\\)Test" classname)
         (match-string-no-properties 1 classname))
        (t (concat classname "Test"))))


(defun javamonkey-switch-to-test-or-src ()
  "Deprecated"
  (interactive)
  (javamonkey-switch-to-test-or-source))


(defun javamonkey-make-enum ()
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (let ((v 0))
      (while (javamonkey-search-forward-regexp "\\([_A-Z]+\\);?$" (point-max) t)
        (replace-match
         (concat "private final static int \\1 = " (number-to-string v) ";")
         t)
        (setq v (+ 1 v))))
    (javamonkey-align "=")))

(defun javamonkey-extract-method (name)
  (interactive "sMethod name: ")
  (kill-region (point) (mark))
  (insert (concat name "();"))
  (indent-according-to-mode)
  (insert "\n")
  (javamonkey-goto-end-of-class)
  (let ((start (point)))
    (insert "\n")
    (insert (concat "private void " name "() {\n"))
    (yank)
    (insert "}\n\n")
    (indent-region start (point-max) nil)
    (goto-char start)))




(defun javamonkey-goto-end-of-class ()
  "Move to the end of the top level class in the buffer we're in."
  (interactive)
  (goto-char (point-max))
  (search-backward "}"))


(defun javamonkey-super-jump-to-source ()
  (interactive)
  (or
   (javamonkey-ediff-from-diff-q)
   (javamonkey-jump-to-source-from-import)
   (javamonkey-jump-to-source-from-jikes-output)
   (javamonkey-jump-to-source-from-stacktrace)
   (javamonkey-jump-to-source-from-findstr)
   (javamonkey-jump-to-source-from-filename)
   (javamonkey-jump-to-source-from-simple-name)))

(defun javamonkey-jump-to-superclass-source ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "extends\\s +" (point-max) t)
        (javamonkey-jump-to-source-from-simple-name))))

(defun javamonkey-jump-to-source-from-import ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (search-forward-regexp "import\\s +\\([^ ;]+\\)\\s *;" (javamonkey-eol) t)
        (let ((classname (match-string-no-properties 1)))
          (if classname
              (find-file (javamonkey-find-source-file classname))))
      nil)))

(defun javamonkey-ediff-from-diff-q ()
  "Files foo and baz differ"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (search-forward-regexp "Files \\(.*\\) and \\(.*\\) differ" (javamonkey-eol) t)
        (let ((a (match-string-no-properties 1))
              (b (match-string-no-properties 2)))
          (ediff a b))
        nil)))



(defun javamonkey-jump-to-source-from-stacktrace ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (or (search-forward-regexp "at \\([^$]+\\)\\$[^\\.]+\\.[^(]+([^:]+:\\([0-9]+\\))"  (javamonkey-eol) t)
            (search-forward-regexp "at \\([^(]+\\)\\.[^(]+([^:]+:\\([0-9]+\\))"  (javamonkey-eol) t))
        (let ((classname (match-string-no-properties 1))
              (line-number (string-to-number (match-string-no-properties 2))))
          (if (and classname line-number)
              (progn
                (find-file (javamonkey-find-source-file classname))
                (goto-line line-number))))
      nil)))

(defun javamonkey-jump-to-source-from-findstr ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (search-forward-regexp "\\([^:]*\\):\\(\\([0-9]+\\):\\)?" (javamonkey-eol) t)
        (let ((filename (match-string-no-properties 1))
              (line-number (match-string-no-properties 3)))
          (if filename
              (progn
                (find-file filename)
                (if line-number (goto-line (string-to-number line-number))))))
      nil)))

(defun javamonkey-jump-to-source-from-filename ()
  "Jump to the file named on the current line. (Works great for
grep -r output.)"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (search-forward-regexp ".*$" (point-max) t)
    (let ((filename (match-string-no-properties 0)))
      (if (file-exists-p filename)
          (find-file filename)
        nil))))

(defun javamonkey-jump-to-source-from-jikes-output ()
  "Jump to the file named on the current line of jikes compile error output."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (search-forward-regexp "compiling \"\\(.*\\.java\\)\"" (point-max) t)
        (let ((filename (match-string-no-properties 1)))
          (if (file-exists-p filename)
              (find-file filename)
            nil))
      nil)))


(defun javamonkey-jump-to-source-from-simple-name ()
  (interactive)
  (save-excursion
    (if (not (looking-at "\\b"))
        (search-forward-regexp "\\b")
      (if (not (javamonkey-type-name-before-point))
          (progn
            (forward-char 1)
            (search-forward-regexp "\\b"))))
    (let ((type (javamonkey-type-name-before-point)))
      (if (not type)
          (search-forward-regexp javamonkey-type-regexp))
      (javamonkey-jump-to-type (javamonkey-type-name-before-point)))))

(defun javamonkey-jump-to-type (type)
  (let ((pkg (javamonkey-get-package-for-import type)))
    (if (not pkg)
        (message (concat "Don't know package for " type))
      (find-file (javamonkey-find-source-file (concat pkg "." type))))))


(defun javamonkey-eol ()
  (save-excursion
    (end-of-line)
    (point)))

(defun javamonkey-find-source-file (classname)
  "Find the file for a given class. Searches the known set of java
source roots in order based on where you are."
  (let ((filename (javamonkey-classname-to-filename classname))
        (full-file-name nil))
    (mapc
     (lambda (dir)
       (if (not full-file-name)
           (let ((name (concat dir "/" filename)))
             (if (file-exists-p name) (setq full-file-name name)))))
     (javamonkey-search-order-source-roots))
    (or full-file-name
        (read-file-name "You're on your own: "))))

(defun javamonkey-search-order-source-roots ()
  (sort (copy-list javamonkey-java-source-roots)
        'javamonkey-source-root-search-predicate))

(defun javamonkey-source-root-search-predicate (a b)
  "Returns t if a is less than b for purposes of searching for java
files.  When searching for source files, we want to search first in
the source roots that have the most in common with our current
directory. For example if we're in somewhere under
prama/server/java/server, we want to look first under that root, then
under other roots under prama/server/java/. Finally, we may look under
prama_stage/server/java/ roots."
  (let ((filename (buffer-file-name)))
    (if filename
        (let* ((current-dir (file-name-directory filename))
               (a-common (javamonkey-common-start-length a current-dir))
               (b-common (javamonkey-common-start-length b current-dir)))
          (cond
           ((string= a current-dir) t)
           ((string= b current-dir) nil)
           ((not (= (abs a-common) (abs b-common)))
            (> (abs a-common) (abs b-common)))
           (t (string< a b))))
      (string< a b))))

(defun javamonkey-common-start-length (a b)
  (cl-do ((max (min (length a) (length b)))
       (idx 0 (setq idx (1+ idx))))
      ((or (>= idx max)
           (not (char-equal (aref a idx) (aref b idx))))
           idx)))

(defun javamonkey-classname-to-filename (classname)
  (let ((basic-file-name classname)
        (dot t))
    (while (setq dot (string-match "\\." basic-file-name))
      (aset basic-file-name dot (string-to-char "/")))
    (concat basic-file-name ".java")))


(mapc (lambda (type)
        (javamonkey-save-in type "java.lang" javamonkey-imported-types))
      '("AbstractMethodError" "ArithmeticException"
        "ArrayIndexOutOfBoundsException" "ArrayStoreException"
        "Boolean" "Byte" "Character" "Class" "ClassCastException"
        "ClassCircularityError" "ClassFormatError" "ClassLoader"
        "ClassNotFoundException" "CloneNotSupportedException" "Cloneable"
        "Comparable" "Compiler" "Double" "Error" "Exception"
        "ExceptionInInitializerError" "Float" "FloatingDecimal"
        "IllegalAccessError" "IllegalAccessException"
        "IllegalArgumentException" "IllegalMonitorStateException"
        "IllegalStateException" "IllegalThreadStateException"
        "IncompatibleClassChangeError" "IndexOutOfBoundsException"
        "InheritableThreadLocal" "InstantiationError"
        "InstantiationException" "Integer" "InternalError"
        "InterruptedException" "LinkageError" "Long" "Math"
        "NegativeArraySizeException" "NoClassDefFoundError"
        "NoSuchFieldError" "NoSuchFieldException" "NoSuchMethodError"
        "NoSuchMethodException" "NullPointerException" "Number"
        "NumberFormatException" "Object" "OutOfMemoryError"
        "Package" "Process" "Runnable" "Runtime" "RuntimeException"
        "RuntimePermission" "SecurityException" "SecurityManager"
        "Short" "StackOverflowError" "String" "StringBuffer"
        "StringIndexOutOfBoundsException" "System" "Thread"
        "ThreadDeath" "ThreadGroup" "ThreadLocal" "Throwable" "UnknownError"
        "UnsatisfiedLinkError" "UnsupportedClassVersionError"
        "UnsupportedOperationException" "VerifyError" "VirtualMachineError"
        "Void"))


(add-hook 'java-mode-hook #'javamonkey-initialize-javamonkey)

(provide 'javamonkey)
