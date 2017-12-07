;;; funcs.el --- ztlevi layer packages file for Spacemacs.
;;
;; Copyright (c) 2016-2017 ztlevi
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

(defun js-indent-line ()
  "Indent the current line as JavaScript."
  (interactive)
  (let* ((parse-status
          (save-excursion (syntax-ppss (point-at-bol))))
         (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (if (nth 3 parse-status)
        'noindent
      (indent-line-to (js--proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

;; (defadvice quit-window (before quit-window-always-kill)
;;   "When running `quit-window', always kill the buffer."
;;   (ad-set-arg 0 t))
;; (ad-activate 'quit-window)
