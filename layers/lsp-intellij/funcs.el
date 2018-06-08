;;; funcs.el --- lsp-intellij layer packages file for Spacemacs.
;;
;; Copyright (c) 2018 Richard Jones
;;
;; Author:  <joneseh25@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code:

;; shared functions

;; The following is adapted from Spacemacs' Eclim completion functions:
;;  https://github.com/syl20bnr/spacemacs/blob/3731d0d/layers/%2Blang/java/funcs.el
(defun spacemacs//java-lsp-delete-horizontal-space ()
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t)))

(defun spacemacs/java-lsp-completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (spacemacs//java-lsp-delete-horizontal-space)
  (insert ".")
  (company-lsp 'interactive))

(defun spacemacs/java-lsp-completing-double-colon ()
  "Insert double colon and show company completions."
  (interactive "*")
  (spacemacs//java-lsp-delete-horizontal-space)
  (insert ":")
  (let ((curr (point)))
    (when (s-matches? (buffer-substring (- curr 2) (- curr 1)) ":")
      (company-lsp 'interactive))))

(if (version< spacemacs-version "0.300")
    (setq spacemacs//intellij-lsp-version-dir-name "spacemacs-0.200")
  (setq spacemacs//intellij-lsp-version-dir-name "spacemacs-0.300"))

(setq spacemacs//intellij-lsp-version-dir-fullpath
      (expand-file-name spacemacs//intellij-lsp-version-dir-name
                        (file-name-directory (or load-file-name buffer-file-name))))

;; Load the relevant `funcs.el` based on Spacemacs version
(load (expand-file-name "funcs" spacemacs//intellij-lsp-version-dir-fullpath) t)

;;; funcs.el ends here
