;;; nwscript-mode.el --- major mode for editing Neverwinter Script -*- lexical-binding: t -*-
;;; based on simpc-mode.el - Copyright (C) Alexey Kutepov aka. Rexim
;;; author: Błażej Niewiadomski - github.com/implicit-image

;; nwscript-mode.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; nwscript-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with nwscript-mode.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'subr-x)
(require 'ffap)
(require 'cl-lib)
(require 'nwscript-flymake)

(defcustom nwscript-include-root 'project
  "Directory from which to search for `nwscript-include-dirs'. If it is the \
symbol `project', use `project-root' to find the directory. If it is the \
symbol `projectile', use `projectile-project-root' to find the directory. \
If it is any other symbol, it must be a function of 0 arguments that returns \
the absolute path of the directory. Otherwise it must be an absolute path of \
the directory."
  :type '(choice (const :tag "project.el" project)
                 (const :tag "projectile.el" projectile)
                 function
                 string))

(defcustom nwscript-enable-flymake t
  "Non-nil if `nwscript-mode' should configure `flymake'." )

(defcustom nwscript-get-includes-function #'nwscript-get-includes "Function to get a list of includes fora compile command. It should return a list of absolute paths to include directories." )


(defvar nwscript-include-dirs '("include" "Scripts" "Scripts_X1" "Scripts_X2")
  "Directories in which to search for include files.")


(defvar-local nwscript--local-include-root nil
  "Local directory from which to search for `nwscript-include-dirs'.")

(defvar-local nwscript--local-include-dirs nil
  "Local directories to search for include files.")

(defvar nwscript--beginning-of-defun-regex "^\\(struct \\b[A-Za-z0-9_]+\\b\\|int\\|void\\|float\\|object\\|itemproperty\\|effect\\|talent\\|location\\|command\\|action\\|cassowary\\|event\\|json\\|sqlquery\\|vector\\|string\\) +\\([A-Za-z]+[A-Za-z_0-9]*\\) *\\((\\)"
  "TODO")

(defvar nwscript-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table for `nwscript-mode'.")

(defvar nwscript-indent-offset 4
  "Indent offset for `nwscript-mode'.")

(defvar nwscript--compilation-error-regexp-alist
  (list 'nwscript
        "\\(\\b[A-Za-z0-9_-]\.nss\\b\\)\\(\\): Error"
        1 ;; file
        2 ;;line
        3 ;; column
        2 ;;type
        "" ;hyperlink
        ;; highlights
        '()))

(defvar nwscript--compilation-warning-regexp-alist
  (list 'nwscript ""
        1 ;; file
        2 ;; line
        3 ;; column
        4 ;; type
        "" ;; hyperlink
        ;; highlights
        ""))

(defun nwscript-completion-setup ()
  "Setup local include root dir, include source dirs and `completion-at\
-point-functions'."
  (interactive)
  (setq-local completion-at-point-functions nil)
  (add-hook 'completion-at-point-functions 'nwscript-completion-at-point nil t)
  (setq-local nwscript--code-completion-function
              (cond ((and (fboundp 'cape-dabbrev)
                          (fboundp 'company-dabbrev-code))
                     (require 'cape)
                     (cape-capf-super (cape-company-to-capf 'company-dabbrev-code)
                                      'cape-file))
                    ((fboundp 'cape-dabbrev)
                     (require 'cape)
                     (cape-capf-super 'cape-dabbrev
                                      'cape-file))
                    (t 'dabbrev-capf)))

  (add-hook 'completion-at-point-functions nwscript--code-completion-function nil t)

  (setq-local nwscript--local-include-root (nwscript--find-root)
              nwscript--local-include-dirs (nwscript--find-include-dirs)))

(defun nwscript--which-func-imenu-joiner-function (args)
  "Joiner function for imenu entries in `which-func-mode'. Return\
ARGS interspersed with separators."
  (car (last args)))

;;;; Completion at point function
(defun nwscript-completion-at-point ()
  "Complete include files from `nwscript-include-dirs'."
  (cond
   ;; inside quotes after `#include' complete include files
   ((thing-at-point-looking-at "\#include +\".*")
    (let ((start (save-mark-and-excursion
                   (1+ (search-backward "\"" (pos-bol) t 1))))
          (end (save-mark-and-excursion
                 (1- (search-forward "\"" (pos-eol) t 1))))
          (table (completion-table-dynamic
                  (lambda (prefix)
                    (let* ((files (flatten-tree (mapcar
                                                 (lambda (dir)
                                                   (directory-files-recursively dir (format ".*%s.*\.nss" prefix) nil))
                                                 nwscript--local-include-dirs)))
                           (data (mapcar 'file-name-base files)))
                      data)))))
      (list start end table)))
   ;; last case, no completions
   (t nil)))

(defvar nwscript--types '("int" "float" "object" "itemproperty" "effect" "talent" "location" "command" "action" "cassowary" "event" "json" "sqlquery" "string" "vector" "void")
  "List of Nwscript type identifiers.")

(defvar nwscript--operators '("==" "!=" "<" ">" ">=" "<=" "&&" "||" "%" "%=" "+" "+=" "-" "-=" "*" "*=" "/" "/=" "--" "++" "|" "|=" "&" "&=" "~" "~=" "^" "^=" ">>" ">>=" "<<" "<<=" ">>>" ">>>=")
  "List of Nwscript operators.")

(defvar nwscript--keywords '("for" "while" "do" "if" "else" "struct" "return" "const" "switch" "case" "default" "break" "continue")
  "List of Nwscript keywords.")

(defvar nwscript--directive-regex "\\(\#include\\|\#define\\)"
  "Regex for finding Nwscript directives.")

(defvar nwscript--struct-regex "\\(\\bstruct\\b\\) \\(\\b[A-Za-z]+[A-Za-z0-9_]*\\b\\)"
  "Regex for finding Nwscript structs.")

(defvar nwscript--constant-regex "\\b[A-Z][A-Z_0-9]+\\b"
  "Regex for finding Nwscript constants.")

(defvar nwscript--number-regex "\\(\\b[0-9]+[0-9\.]*f*\\b\\)"
  "Regex for finding numbers.")

(defvar nwscript--function-declaration-regex  "^\\(struct [A-Za-z0-9_]+\\|int\\|void\\|float\\|object\\|itemproperty\\|effect\\|talent\\|location\\|command\\|action\\|cassowary\\|event\\|json\\|sqlquery\\|vector\\|string\\)[ \t]+\\([A-Za-z]+[A-Za-z_0-9]*\\).*$"
  "Regex for function declarations and definitions.")

(defun nwscript-font-lock-keywords ()
  "Font lock keywords for `nwscript-mode'."
  (list
   `(,nwscript--directive-regex . 'font-lock-keyword-face)
   ;; special struct highlighting
   `(,nwscript--struct-regex . (2 'font-lock-type-face))
   ;; constants
   `(,nwscript--constant-regex . 'font-lock-constant-face)
   ;; keywords
   `(,(regexp-opt nwscript--keywords 'symbols) . 'font-lock-keyword-face)
   ;; types
   `(,(regexp-opt nwscript--types 'symbols) . 'font-lock-type-face)
   `(,nwscript--number-regex . 'font-lock-number-face)
   ;; function declarations
   `(,nwscript--function-declaration-regex . (2 font-lock-function-name-face))))

(defun nwscript--space-prefix-len (line)
  "Get the length of indent on LINE."
  (- (length line)
     (length (string-trim-left line))))

(defun nwscript--previous-non-empty-line (pos)
  "Get first previous line that is not empty starting at POS."
  (save-mark-and-excursion
    (forward-line (- pos))
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

(defun nwscript--strip-comments (line)
  "Strip comments from LINE if it is not just a comment line."
  (let ((substr (substring line 0 (string-match " */[/\*].*$" line))))
    (if (not (string-empty-p substr))
        substr
      line)))

(defun nwscript--one-after-another-backward (first second)
  "FIRST SECOND."
  (save-mark-and-excursion
    (search-backward first
                     (save-mark-and-excursion
                       (search-backward second nil t 1))
                     t 1)))

(defun nwscript--one-after-another-backward-regexp (first second)
  "FIRST SECOND."
  (save-mark-and-excursion
    (search-backward-regexp first
                            (save-mark-and-excursion
                              (search-backward-regexp second nil t 1))
                            t 1)))

(defun nwscript--indent-at-last-matching-paren (start-pos close-paren)
  "START-POS CLOSE-PAREN."
  (save-mark-and-excursion
    (goto-char start-pos)
    (search-backward close-paren nil t)
    (forward-char 1)
    (backward-sexp)
    (nwscript--space-prefix-len (thing-at-point 'line t))))

(defun nwscript--match-line-at-last-matching-paren (regexp start-pos close-paren)
  (save-mark-and-excursion
    (goto-char start-pos)
    (search-backward close-paren nil t)
    (forward-char 1)
    (backward-sexp)
    (string-match-p regexp
                    (string-trim-left
                     (nwscript--strip-comments
                      (thing-at-point 'line t))))))

(defun nwscript--line-at-last-matching-paren (start-pos close-paren)
  "START-POS CLOSE-PAREN."
  (save-mark-and-excursion
    (goto-char start-pos)
    (search-backward close-paren nil t)
    (forward-char 1)
    (backward-sexp)
    (thing-at-point 'line t)))

(defun nwscript--desired-indentation ()
  "Return desired indentation at current point."
  (let ((cur-line (string-trim-right (nwscript--strip-comments
                                      (thing-at-point 'line t))))
        (prev-line (string-trim-right (nwscript--strip-comments
                                       (nwscript--previous-non-empty-line 1))))
        (indent-len nwscript-indent-offset))
    (cond
     ;;;; Multi line function handling
     ((string-suffix-p "(" prev-line)
      (message "Multi line start")
      (+ (nwscript--space-prefix-len prev-line) indent-len))

     ((and (string-suffix-p ");" prev-line)
           (string-suffix-p "}" cur-line))
      (message "after multi line if/for/while condition brace indent")
      (max (- (nwscript--indent-at-last-matching-paren (pos-eol) ")")
              indent-len)
           0))

     ((and (string-match-p ") +{" prev-line)
           (string-suffix-p "}" cur-line))
      (message "after multi line if/for/while brace")
      (nwscript--indent-at-last-matching-paren (pos-eol) ")"))

     ((string-match-p ") +{" prev-line)
      (message "indent after same line ) {")
      (+ (nwscript--indent-at-last-matching-paren (pos-bol) ")") indent-len))

     ((or (and (string-suffix-p ")" prev-line)
               (string-suffix-p "{" cur-line))
          (string-suffix-p ");" prev-line))
      (nwscript--indent-at-last-matching-paren (pos-bol) ")"))

     ((and (string-suffix-p "}" cur-line)
           (not (string-suffix-p "}" prev-line))
           (or (not (nwscript--one-after-another-backward "{" "default:"))))
      (max (- (nwscript--space-prefix-len prev-line)
              (* 2 indent-len))
           0))

     ((string-suffix-p "," prev-line)
      (cond
       ((string-match-p "\\b\\([A-Za-z][A-Za-z0-9_]+\(.?*[^)],\\)\\b" prev-line)
        (+ 1 (save-mark-and-excursion
               (forward-line -1)
               (end-of-line)
               (- (search-backward "(" (pos-bol) t)
                  (pos-bol)))))
       ((string-suffix-p ")," prev-line)
        (nwscript--indent-at-last-matching-paren (pos-bol) ")"))
       (t (nwscript--space-prefix-len prev-line))))

     ;;;; switch statement handling
     ((or (string-suffix-p "break;" prev-line)
          (and (string-prefix-p "return" (string-trim-left prev-line))
               (nwscript--one-after-another-backward ":" "case ")))
      (max (- (nwscript--space-prefix-len prev-line) indent-len) 0))

     ((and (string-suffix-p ":" prev-line)
           (not (or (string-prefix-p "case " (string-trim-left cur-line))
                    (string-prefix-p "default:" (string-trim-left cur-line)))))
      (if (string-prefix-p "{" (string-trim-left cur-line))
          (nwscript--space-prefix-len prev-line)
        (+ (nwscript--space-prefix-len prev-line) indent-len)))

     ((and (string-prefix-p "case " (string-trim-left cur-line))
           (not (or (string-prefix-p "case " (string-trim-left prev-line))
                    (string-prefix-p "default:" (string-trim-left prev-line))))
           (string-suffix-p ";" prev-line))
      (max (- (nwscript--space-prefix-len prev-line) indent-len) 0))

     ;;;; standard indent handling
     ((and (string-suffix-p "{" prev-line)
           (string-prefix-p "}" (string-trim-left cur-line)))
      (nwscript--space-prefix-len prev-line))

     ((string-suffix-p "{" prev-line)
      (+ (nwscript--space-prefix-len prev-line) indent-len))

     ((string-prefix-p "}" (string-trim-left cur-line))
      (max (- (nwscript--space-prefix-len prev-line) indent-len) 0))

     (t (nwscript--space-prefix-len prev-line)))))

(defun nwscript-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((current-indentation
            (nwscript--space-prefix-len (thing-at-point 'line t)))
           (desired-indentation
            (nwscript--desired-indentation))
           (n (max (- (current-column) current-indentation) 0)))
      (indent-line-to desired-indentation)
      (forward-char n))))

(defun nwscript--beginning-of-defun (&optional arg)
  (interactive "^p")
  (if (> 0 arg)
      (let ((beginning-of-defun-function nil))
        (progn (beginning-of-defun-raw arg)))
    (re-search-backward nwscript--beginning-of-defun-regex nil 'move arg)
    (beginning-of-line)))

(define-derived-mode nwscript-mode prog-mode "NWScript"
  "Simple major mode for editing Neverwinter Script files."
  :syntax-table nwscript-mode-syntax-table
  ;; local variables
  (setq-local font-lock-defaults '(nwscript-font-lock-keywords)
              defun-prompt-regexp "\\b\\(struct +[A-Za-z0-9\_]+\\|int\\|void\\|float\\|object\\|itemproperty\\|effect\\|talent\\|location\\|command\\|action\\|cassowary\\|event\\|json\\|sqlquery\\|vector\\|string\\)"
              beginning-of-defun-function 'nwscript--beginning-of-defun
              comment-start "// "
              ;; which-func
              which-func-imenu-joiner-function 'nwscript--which-func-imenu-joiner-function
              indent-line-function 'nwscript-indent-line
              ;; dabbrev
              dabbrev-case-fold-search nil
              dabbrev-upcase-means-case-search nil
              dabbrev-check-other-buffers t
              dabbrev-check-all-buffers nil
              ;; outline minor mode
              outline-regexp "\\(\\(struct[ \t]+[A-Za-z0-9_]+\\|int\\|void\\|float\\|object\\|itemproperty\\|effect\\|talent\\|location\\|command\\|action\\|cassowary\\|event\\|json\\|sqlquery\\|vector\\|string\\|[ \t]*if +(\\|[ \t]*for +(\\|[ \t]*while +(\\\|[ \t]*switch +(\\|[ \t]*case +[A-Za-z0-9]+:\\)[ \t]+\\([A-Za-z]+[A-Za-z_0-9]*[ \t]*(.*)\\)\\)"
              outline-heading-end-regexp ";\n\\|,\n\\|:\n"
              ;; imenu
              imenu-max-item-length 150
              imenu-flatten t
              imenu-generic-expression
              `(("Includes"
                 "#include[ \t]+\"\\([A-Za-z0-9\_]+\\)\"\n" 1)
                ("Function declarations"
                 "\\(struct +[A-Za-z0-9\_]+\\|int\\|void\\|float\\|object\\|itemproperty\\|effect\\|talent\\|location\\|command\\|action\\|cassowary\\|event\\|json\\|sqlquery\\|vector\\|string\\)[ \t]+\\([A-Za-z]+[A-Za-z_0-9]*[ \t]*\\)\\(([A-Za-z0-9_, \n=]*)\\);" 2)
                ("Function definitions"
                 "\\(struct +[A-Za-z0-9\_]+\\|int\\|void\\|float\\|object\\|itemproperty\\|effect\\|talent\\|location\\|command\\|action\\|cassowary\\|event\\|json\\|sqlquery\\|vector\\|string\\)[ \t]+\\([A-Za-z]+[A-Za-z_0-9]*\\)\\([ \t]*([^{]*)\\)[ \t\n]*{" 2)
                ("Constants" "\\b\\(const[ \t]+\\(int\\|float\\|string\\)[ \t]+\\)\\([A-Z0-9_]+\\)[ \t]+=.*" 3)
                ("Structs" "\\b\\(^struct[ \t]+\\)\\([A-Za-z0-9\_]+{*$\\)" 2)))

  ;; consult-imenu config
  (when (fboundp 'consult-imenu)
    (require 'consult-imenu)
    (add-to-list 'consult-imenu-config
                 '(nwscript-mode :toplevel "Functions"
                                 :types ((?i "Includes")
                                         (?f "Function declarations" font-lock-function-name-face)
                                         (?d "Function definitions" font-lock-function-name-face)
                                         (?s "Structs" font-lock-type-face)
                                         (?c "Constants" font-lock-constant-face)))))

  ;; compilation setup
  ;; (nwscript-setup-compilation-options)
  ;; flymake setup
  (when (bound-and-true-p nwscript-enable-flymake)
    (nwscript-flymake-setup))
  ;; capf setup
  (nwscript-completion-setup))

(provide 'nwscript-mode)
