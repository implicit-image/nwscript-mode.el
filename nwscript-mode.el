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

(defvar nwscript-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C/C++ style comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table))

(defvar nwscript-indent-offset 4
  "Indent offset for `nwscript-mode'.")

(defun nwscript-types ()
  '("int" "float" "object" "itemproperty" "effect" "talent" "location" "command" "action" "cassowary" "event" "json" "sqlquery" "string" "vector" "void"))

(defun nwscript-operators ()
  '("==" "!=" "<" ">" ">=" "<=" "&&" "||" "%" "%=" "+" "+=" "-" "-=" "*" "*=" "/" "/=" "--" "++" "|" "|=" "&" "&=" "~" "~=" "^" "^=" ">>" ">>=" "<<" "<<=" ">>>" ">>>="))

(defun nwscript-keywords ()
  '("for" "while" "do" "if" "else" "struct" "return" "const" "switch" "case" "default" "break"))

(defun nwscript-font-lock-keywords ()
  (list
   `("\\(\#include\\|\#define\\)" . font-lock-keyword-face)
   ;; special struct highlighting
   `("\\(\\bstruct\\b\\) \\(\\b[A-Za-z]+[A-Za-z0-9_]*\\b\\)" . (2 font-lock-type-face))
   ;; constants
   `("\\b[A-Z][A-Z_0-9]+\\b" . font-lock-constant-face)
   ;; keywords
   `(,(regexp-opt (nwscript-keywords) 'symbols) . font-lock-keyword-face)
   ;; types
   `(,(regexp-opt (nwscript-types) 'symbols) . font-lock-type-face)
   ;; function declarations
   `("\\b\\(struct \\b[A-Za-z0-9_]+\\b\\|int\\|void\\|float\\|object\\|itemproperty\\|effect\\|talent\\|location\\|command\\|action\\|cassowary\\|event\\|json\\|sqlquery\\|vector\\|string\\) +\\([A-Za-z]+[A-Za-z_0-9]*\\) *\\((\\)" . (2 font-lock-function-name-face))
   ;; function calls
   ;; TODO: figure out how to find function calls
   ;; `("\\b\\([A-Za-z]+[A-Za-z_0-9]*\\b\\)\\((\\)" . (0 font-lock-function-call-face))
   ;; `(,(regexp-opt (nwscript-operators) 'symbols) . font-lock-negation-char-face)
   ))

;;; TODO: try to replace nwscript--space-prefix-len with current-indentation
(defun nwscript--space-prefix-len (line)
  (- (length line)
     (length (string-trim-left line))))

(defun nwscript--previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

(defun nwscript--desired-indentation ()
  (let ((cur-line (string-trim-right (thing-at-point 'line t)))
        (prev-line (string-trim-right (nwscript--previous-non-empty-line)))
        (indent-len nwscript-indent-offset))
    (cond
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

(define-derived-mode nwscript-mode prog-mode "NWScript"
  "Simple major mode for editing C files."
  :syntax-table nwscript-mode-syntax-table
  ;; imenu integration
  (setq-local imenu-max-item-length 150
              imenu-flatten t
              imenu-generic-expression
              `(("Function declarations" "\\b\\(\\(struct[ \t]+[A-Za-z0-9_]+\\|int\\|void\\|float\\|object\\|itemproperty\\|effect\\|talent\\|location\\|command\\|action\\|cassowary\\|event\\|json\\|sqlquery\\|vector\\|string\\)[ \t]+\\([A-Za-z]+[A-Za-z_0-9]*[ \t]*(.*)\\)\\);" 1)
                ("Function definitions" "\\b\\(\\(struct[ \t]+[A-Za-z0-9_]+\\|int\\|void\\|float\\|object\\|itemproperty\\|effect\\|talent\\|location\\|command\\|action\\|cassowary\\|event\\|json\\|sqlquery\\|vector\\|string\\)[ \t]+\\([A-Za-z]+[A-Za-z_0-9]*[ \t]*(.*)\\)\\)[^;]" 1)
                ("Constants" "\\b\\(const[ \t]+\\(int\\|float\\|string\\)[ \t]+[A-Z0-9_]+\\)[ \t]+=.*;" 1)
                ("Structs" "\\b\\(^struct[ \t]+\\([A-Za-z0-9_]+$\\)\\)" 1)))
  ;; consult imenu config
  (eval-after-load 'consult-imenu
    '(add-to-list 'consult-imenu-config
                  '(nwscript-mode :toplevel "Functions"
                                  :types ((?f "Function declarations" font-lock-function-name-face)
                                          (?d "Function definitions" font-lock-function-name-face)
                                          (?s "Structs" font-lock-type-face)
                                          (?c "Constants" font-lock-constant-face)))))

  (setq-local font-lock-defaults '(nwscript-font-lock-keywords))
  (setq-local indent-line-function 'nwscript-indent-line)
  (setq-local comment-start "// "))

(provide 'nwscript-mode)
