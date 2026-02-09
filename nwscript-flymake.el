;;; nwscript-flymake.el --- flymake backend for Neverwinter Script -*- lexical-binding: t -*-

;; Author: Błażej Niewiadomski
;; Maintainer: Błażej Niewiadomski
;; Version: 0.1
;; Package-Requires: (dependencies)
;; Homepage: homepage
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

(require 'nwscript-compile)
(require 'ansi-color)

(defvar-local nwscript--flymake-buffer nil)

(defvar-local nwscript--flymake-process nil)

(defvar-local nwscript--diagnostics nil)

(defvar-local nwscript-flymake-diagnostics-regex-format "^\\(.*%s\.nss\\) *\(\\([0-9]+\\)\):.*?NSC[0-9]+: \\(.*\\)\$"
  "TODO")

(defun nwscript-get-includes ()
  (require 'ffap)
  (let ((include-dirs (nwscript--find-include-dirs))
        (includes))
    (message "%S" include-dirs)
    (dolist (dir include-dirs (flatten-list includes))
      (when (file-directory-p dir)
        (push (ffap-all-subdirs dir 5) includes)))))


(defun nwscript--temp-outfile (infile)
  "Return a temporary output file corresponding to INFILE."
  (expand-file-name (replace-regexp-in-string "\.nss" "\.ncs"
                                              (file-name-nondirectory infile))
                    (temporary-file-directory)))

(defun nwscript--script-buffer-p (&optional buffer)
  "Return Non-nil if BUFFER is an nwscript script buffer, that is whether is contains void main() function. When BUFFER is nil, it defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (save-mark-and-excursion
        (goto-char (point-min))
        (re-search-forward "void[ \t]+main[ \t]+([^{\)])[ \t][^;]" (point-max) t 1)))))

(defun nwscript-flymake (report-fn &rest args)
  "Flymake backend for nwscript-mode. For REPORT-FN and ARGS see `flymake-diagnostic-functions'."
  (unless (nwscript--ensure-compiler)
    (error "Cannot find a suitable compiler."))
  (let* ((source (current-buffer))
         (source-file-name (buffer-file-name source))
         (cmd (nwscript--get-check-command (list (buffer-file-name source))
                                           (nwscript-get-includes))))
    (message "Checking %s" source-file-name)
    (when (and (processp nwscript--flymake-process)
               (process-live-p nwscript--flymake-process))
      (kill-process nwscript--flymake-process))
    (when (buffer-live-p nwscript--flymake-buffer)
      (with-current-buffer nwscript--flymake-buffer
        (erase-buffer)))
    (setq-local nwscript--flymake-process nil)
    (save-mark-and-excursion
      (let* ((buf (or (and (bufferp nwscript--flymake-buffer)
                           (buffer-live-p nwscript--flymake-buffer)
                           nwscript--flymake-buffer)
                      (get-buffer-create
                       (format " *nwnsc-flymake[%s]"
                               (buffer-name source)))))
             (proc (apply 'start-process
                          "nwscript-flymake"
                          buf
                          cmd)))
        (setq-local nwscript--flymake-process proc
                    nwscript--flymake-buffer buf)
        (set-process-coding-system nwscript--flymake-process 'utf-8-unix)
        (set-process-sentinel nwscript--flymake-process
                              (lambda (process change)
                                (let ((inhibit-message t))
                                  (message "sentinel received: %s" change)
                                  (message "source in sentinel is %S" source))
                                (when (and (process-buffer process))
                                  (with-current-buffer (process-buffer process)
                                    (ansi-color-filter-region (point-min) (point-max))
                                    (replace-regexp-in-region "" "" (point-min) (point-max))
                                    (replace-regexp-in-region "\\([^\n]+\\)\n" "\\1" (point-min) (point-max))
                                    (goto-char (point-min))
                                    (cl-loop
                                     while (search-forward-regexp
                                            (format nwscript-flymake-diagnostics-regex-format
                                                    (file-name-base (buffer-file-name source)))
                                            nil t)
                                     for msg = (match-string 3)
                                     for (beg . end) = (flymake-diag-region
                                                        (get-buffer (or (match-string 1)
                                                                        source))
                                                        (string-to-number (match-string 2)))
                                     for type = (if (string-match ".*Warning.*$" (thing-at-point 'line))
                                                    :warning
                                                  :error)
                                     when (and beg end)
                                     collect (flymake-make-diagnostic (get-buffer (or (match-string 1)
                                                                                      source))
                                                                      beg end type msg)
                                     into diags
                                     finally (progn (with-current-buffer source
                                                      (setq-local nwscript--diagnostics diags))
                                                    (message "finished checking %s" source-file-name)
                                                    (funcall report-fn diags)))))))))))

(defun nwscript-flymake-setup ()
  "Setup flymake backend."
  (interactive)
  ;; check flymake only on save since we are compiling the file on disk
  (setq-local flymake-no-changes-timeout nil
              flymake-start-on-save-buffer t)
  ;; add new backend to local diagnostic functions
  (add-hook 'flymake-diagnostic-functions 'nwscript-flymake nil t))

(provide 'nwscript-flymake)

;;; nwscript-flymake.el ends here
