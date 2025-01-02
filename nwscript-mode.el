;;; nwscript-mode.el --- major mode for editing Neverwinter Script -*- lexical-binding: t -*-
;; Author: Błażej Niewiadomski - github.com/implicit-image
;; Maintainer: Błażej Niewiadomski
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/implicit-image/nwscript-mode.el
;; Keywords: nwscript

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


;;; Commentary:
;; This is a major mode for editing Neverwinter Script files, based on simpc-mode by Alexey Kutepov aka. rexim
;;; Code:
(require 'subr-x)

(defvar nwscript-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C/C++ style comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table))

;; mode hook
(defvar nwscript-mode-hook nil "Hook for function `nwscript-mode-hook'.")

(defvar nwscript-mode-indent-level 4)

(defun nwscript-types ()
  '("int" "float" "object" "itemproperty" "effect" "talent" "location" "command" "action" "cassowary" "event" "json" "sqlquery" "string" "vector" "void"))

(defun nwscript-operators ()
  '("=" "==" "!=" "<" ">" ">=" "<=" "&&" "||" "%" "%=" "+" "+=" "-" "-=" "*" "*=" "/" "/=" "--" "++" "|" "|=" "&" "&=" "~" "~=" "^" "^=" ">>" ">>=" "<<" "<<=" ">>>" ">>>="))

(defun nwscript-keywords ()
  '("for" "while" "do" "if" "else" "struct" "return" "const" "switch" "case" "default" "break"))

(defvar nwscript--directive-regex
  "\\(#define\\|#include\\b\\)"
  "Regex to parse compiler directives.")

(defvar nwscript--struct-member-regex
  "\\(\\b[A-Za-z]+[A-Za-z0-9_]*\\)\\.\\([A-Za-z]+[A-Za-z0-9_]*\\b\\)"
  "Regex to parse struct members.")

(defvar nwscript--struct-def-regex
  "\\(\\bstruct\\b\\) \\(\\b[A-Za-z]+[A-Za-z0-9_]*\\b\\)"
  "Regex to parse struct definitions.")

(defvar nwscript--constant-regex
  "\\b[A-Z][A-Z_0-9]+\\b"
  "Regex to parse constants.")

;; (defvar nwscript--keyword-regex
;;   (regex-opt (nwscript-keywords) 'symbols)
;;   "Regex to parse keywords.")

;; (defvar nwscript--type-regex
;;   (regexp-opt (nwscript-types) 'symbols)
;;   "Regex to parse types.")

(defvar nwscript--function-regex
  "\\b\\(struct \\b[A-Za-z0-9_]+\\b\\|int\\|void\\|float\\|object\\|itemproperty\\|effect\\|talent\\|location\\|command\\|action\\|cassowary\\|event\\|json\\|sqlquery\\|vector\\|string\\) +\\([A-Za-z_]+[A-Za-z_0-9]*\\)\\((\\)"
  "Regex to parse function declaracions.")


;; (defun nwscript-font-lock-keywords ()
;;   "Font Lock keywords for nwscript-mode."
;;   (list
;;    ;; directives
;;    `("\\(#define\\|#include\\b\\)" . font-lock-keyword-face)
;;    ;; struct members highlighting
;;    `("\\(\\b[A-Za-z]+[A-Za-z0-9_]*\\)\\.\\([A-Za-z]+[A-Za-z0-9_]*\\b\\)" . (2 font-lock-property-name-face))
;;    ;; struct definition highlighting
;;    `("\\(\\bstruct\\b\\) \\(\\b[A-Za-z]+[A-Za-z0-9_]*\\b\\)" . (2 font-lock-type-face))
;;    ;; constants
;;    `("\\b[A-Z][A-Z_0-9]+\\b" . font-lock-constant-face)
;;    ;; keywords
;;    `(,(regexp-opt (nwscript-keywords) 'symbols) . font-lock-keyword-face)
;;    ;; types
;;    `(,(regexp-opt (nwscript-types) 'symbols) . font-lock-type-face)
;;    ;; function declarations
;;    `("\\b\\(struct \\b[A-Za-z0-9_]+\\b\\|int\\|void\\|float\\|object\\|itemproperty\\|effect\\|talent\\|location\\|command\\|action\\|cassowary\\|event\\|json\\|sqlquery\\|vector\\|string\\) +\\([A-Za-z_]+[A-Za-z_0-9]*\\)\\((\\)" . (2 font-lock-function-name-face))
;;    ))


(defun nwscript-font-lock-keywords ()
  "Font Lock keywords for nwscript-mode."
  (list
   ;; directives
   `(,nwscript--directive-regex . font-lock-keyword-face)
   ;; struct members highlighting
   `(,nwscript--struct-member-regex . (2 font-lock-property-name-face))
   ;; struct definition highlighting
   `(,nwscript--struct-def-regex . (2 font-lock-type-face))
   ;; constants
   `(,nwscript--constant-regex . font-lock-constant-face)
   ;; keywords
   `(,(regexp-opt (nwscript-keywords) 'symbols) . font-lock-keyword-face)
   ;; types
   `(,(regexp-opt (nwscript-types) 'symbols) . font-lock-type-face)
   ;; function declarations
   `(,nwscript--function-regex . (2 font-lock-function-name-face))
   ;; operators
   `(,(regexp-opt (nwscript-operators)) . font-lock-operator-face)))

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

(defun nwscript--previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

(defun nwscript--indentation-of-previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (current-indentation)))




(defun nwscript--desired-indentation ()
  (let* ((cur-line (string-trim-right (thing-at-point 'line t)))
         (prev-line (string-trim-right (nwscript--previous-non-empty-line)))
         (indent-len 4)
         (prev-indent (nwscript--indentation-of-previous-non-empty-line)))
    (cond
     ((string-suffix-p "," prev-line)
      (if (string-match-p "\\(.+(.+,\\)" prev-line)
          (- (length (string-trim-right prev-line "\\(.+(\\)"))
             (length (string-trim)))
        prev-indent))
     ((string-match-p "^\\s-*switch\\s-*(.+)" prev-line)
      prev-indent)
     ((and (string-suffix-p "{" prev-line)
           (string-prefix-p "}" (string-trim-left cur-line)))
      prev-indent)
     ((string-suffix-p "{" prev-line)
      (+ prev-indent indent-len))
     ((string-prefix-p "}" (string-trim-left cur-line))
      (max (- prev-indent indent-len) 0))
     ((string-suffix-p ":" prev-line)
      (if (string-suffix-p ":" cur-line)
          prev-indent
        (+ prev-indent indent-len)))
     ((string-suffix-p ":" cur-line)
      (max (- prev-indent indent-len) 0))
     (t prev-indent))))




(define-derived-mode nwscript-mode prog-mode "NWScript"
  "Simple major mode for editing Neverwinter Script files."
  :syntax-table nwscript-mode-syntax-table
  (setq-local font-lock-defaults '(nwscript-font-lock-keywords))
  (setq-local indent-line-function 'nwscript-indent-line)
  (setq-local comment-start "// ")
  (add-to-list 'auto-mode-alist '("\\.nss\\'" . nwscript-mode))
  (run-hooks 'nwscript-mode-hook))

(provide 'nwscript-mode)
