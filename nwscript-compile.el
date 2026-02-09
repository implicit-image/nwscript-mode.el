;;; nwscript-compile.el --- M-x compile support for nwscript-mode -*- lexical-binding: t -*-

;; Author: Błażej Niewiadomski
;; Maintainer: Błażej Niewiadomski
;; Version: 0.1
;; Package-Requires: ((emacs . "29.1"))
;; Homepage: https://github.com/implicit-image/nwscript-mode.el
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:
(require 'cl-lib)
(require 'ffap)

(defcustom nwscript-compiler-use-wine (eq system-type 'gnu/linux)
  "doc" )

(defcustom nwscript-compiler-executable (if nwscript-compiler-use-wine
                                            (executable-find "nwnsc.exe")
                                          (executable-find "nwnsc"))
  "doc" )

(defcustom nwscript-compile-build-options '() "doc" )

(defcustom nwscript-compile-check-options '("-q") "doc" )

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

(defun nwscript--find-root ()
  "Find root for finding include files."
  (cond ((eq nwscript-include-root 'project)
         (require 'project)
         (if-let ((cur (project-current)))
             (project-root cur)
           nil))
        ((eq nwscript-include-root 'projectile)
         (when (bound-and-true-p projectile-mode)
           (projectile-project-root)))
        ((functionp nwscript-include-root) (funcall nwscript-include-root))
        ((stringp nwscript-include-root) nwscript-include-root)
        (t default-directory)))

(defun nwscript--find-include-dirs ()
  "Return local include dirs."
  (let* ((dirs (cl-remove-if-not (lambda (inc-dir)
                                   (or (file-name-absolute-p inc-dir)
                                       (file-directory-p (expand-file-name
                                                          inc-dir nwscript--local-include-root))))
                                 nwscript-include-dirs))
         (dirs (if (null dirs) '(".") dirs)))
    (mapcar (lambda (dir)
              (expand-file-name dir nwscript--local-include-root))
            dirs)))

(defun nwscript-setup-compilation-options ()
  "Setup compilation options for `nwscript-mode' buffers."
  (add-to-list 'compilation-error-regexp-alist 'nwscript-error)
  (add-to-list 'compilation-error-regexp-alist 'nwscript-warning)
  (add-to-list 'compilation-error-regexp-alist-alist nwscript--compilation-error-regexp-alist)
  (add-to-list 'compilation-error-regexp-alist-alist nwscript--compilation-warning-regexp-alist))


(defun nwscript--get-includes ()
  "Return a list of includes for the compile command."
  (let ((include-dirs (nwscript--find-include-dirs))
        (includes))
    (message "%S" include-dirs)
    (dolist (dir include-dirs (flatten-list includes))
      (when (file-directory-p dir)
        (push (ffap-all-subdirs dir 5) includes)))))

(defun nwscript--temp-outfile (infile)
  (tmp (expand-file-name (replace-regexp-in-string "\.nss" "\.ncs"
                                                   (file-name-nondirectory (buffer-file-name)))
                         (temporary-file-directory))))

(defun nwscript--temp-outdir ()
  (let ((dir (expand-file-name "nwnsc-compile" (temporary-file-directory))))
    (when (not (file-exists-p dir))
      (make-directory dir))
    dir))

(defun nwscript--get-check-command (infiles includes)
  `(,@(list (when nwscript-compiler-use-wine (executable-find "wine"))
            nwscript-compiler-executable)
    ,@nwscript-compile-build-options
    "-c"
    ,@(list "-i" (string-join includes ";"))
    ,@(list "-b" (nwscript--temp-outdir))
    ,(string-join infiles " ")))

(defun nwscript--ensure-compiler ()
  (pcase system-type
    ('gnu/linux (and (executable-find nwscript-compiler-executable)
                     (if nwscript-compiler-use-wine
                         (executable-find "wine")
                       t)))
    (_ (user-error "platforms other than gnu/linux are not supported at his moment"))))


(provide 'nwscript-compile)

;;; nwscript-compile.el ends here
