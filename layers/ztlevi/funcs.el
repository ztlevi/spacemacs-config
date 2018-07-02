;;; funcs.el --- ztlevi layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2016-2018 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Utility functions
(defun bb/define-key (keymap &rest bindings)
  (declare (indent 1))
  (while bindings
    (define-key keymap (pop bindings) (pop bindings))))

(defun insert-4-spaces ()
  (interactive)
  (insert "    "))

(defun ztlevi/toggle-major-mode ()
  (interactive)
  (if (eq major-mode 'fundamental-mode)
      (set-auto-mode)
    (fundamental-mode)))
(spacemacs/set-leader-keys "otm" 'ztlevi/toggle-major-mode)

;; (defadvice quit-window (before quit-window-always-kill)
;;   "When running `quit-window', always kill the buffer."
;;   (ad-set-arg 0 t))
;; (ad-activate 'quit-window)

;; Delete frame if solo window
(defun delete-window-or-frame (&optional window frame force)
  (interactive)
  (if (= 1 (length (window-list frame)))
      (delete-frame frame force)
    (spacemacs/delete-window)))

;; override check-large-file function
;; do not prompt message and use fundamental-mode as default
;; check when opening large files - literal file open
(defun spacemacs/check-large-file ()
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and
           (not (memq major-mode spacemacs-large-file-modes-list))
           size (> size (* 1024 1024 dotspacemacs-large-file-size)))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

;; get nth line length
(defun spacemacs/get-nth-line-length (n)
  "Length of the Nth line."
  (save-excursion
    (goto-char (point-min))
    (if (zerop (forward-line (1- n)))
        (- (line-end-position)
           (line-beginning-position)))))

;; check if the file has been minified
;; disable for text-mode files
(defun spacemacs/check-minified-file ()
  (and
   (not (member (file-name-extension (buffer-file-name))
                '("org" "md" "markdown" "txt" "rtf")))
   (> (spacemacs/get-nth-line-length 1) 500)))
(add-to-list 'magic-mode-alist (cons #'spacemacs/check-minified-file 'fundamental-mode))

;; evil switch to insert before ivy-yasnippet
(defun my-ivy-yasnippet ()
  (interactive)
  (evil-insert-state)
  (ivy-yasnippet))
