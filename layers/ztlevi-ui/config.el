;;; config.el --- ztlevi layer packages file for Spacemacs.
;;
;; Copyright (c) 2016-2017 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; http://emacsredux.com/blog/2014/04/05/which-function-mode/
(which-function-mode)
;; when editing js file, this feature is very useful
;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))

(defun zt/set-header-line ()
  (interactive)
  (which-function-mode t)
  (setq header-line-format
        '((which-func-mode ("" which-func-format " ")))))
(add-hook 'prog-mode-hook 'zt/set-header-line)
(add-hook 'text-mode-hook 'zt/set-header-line)

;; set scrolling speed
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; all-the-icons add hook
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b01110000
   #b00010000
   #b00010000
   #b00000000])

(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00001000
   #b00001000
   #b00001110
   #b00000000
   #b00000000
   #b00000000
   #b00000000])

;; set initl screen size
(setq initial-frame-alist
      '(
        (width . 86) ; character
        (height . 40) ; lines
        ))
