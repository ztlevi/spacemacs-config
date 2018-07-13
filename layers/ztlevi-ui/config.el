;;; config.el --- ztlevi layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2016-2018 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; set scrolling speed
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; set initl screen size
(setq initial-frame-alist
      '((width . 110)
        (height . 65)))

;; set evil cursors colors
(setq spacemacs-evil-cursors '(("normal"       "#4078f2" box)
                               ("insert"       "#BFD641" (bar . 2))
                               ("emacs"        "#a0bcf8" box)
                               ("hybrid"       "#ff007f" (bar . 2))
                               ("replace"      "#F7786B" (hbar . 2))
                               ("evilified"    "#F6D155" box)
                               ("visual"       "gray"    (hbar . 2))
                               ("motion"       "#B565A7" box)
                               ("lisp"         "#6B5B95" box)
                               ("iedit"        "#E94B3C" box)
                               ("iedit-insert" "#E94B3C" (bar . 2))))

;; set faces after doom-one-light
(if (eq spacemacs--cur-theme 'doom-one-light)
    (custom-set-faces
     ;; set spaceline faces
     '(powerline-active1 ((t (:inherit mode-line :background "#e7e7e7"))))
     '(powerline-active2 ((t (:inherit mode-line :background "#c8c8c8"))))
     '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "#e1e1e1"))))
     '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "#e3e3e3"))))
     ;; others
     '(show-paren-match ((t (:background "#d7d7d7" :foreground "#e45649" :weight ultra-bold))))))

;; set faces for others
(custom-set-faces
 '(origami-fold-replacement-face ((t (:inherit (quote font-lock-keyword-face)))))
 '(term-bold ((t (:weight bold :height 1.1 :family "Ubuntu Mono derivative Powerline")))))

;; set faces after doom-themes
;; (custom-set-faces
;;  '(doom-modeline-error ((t (:background "#e45649" :foreground "#f0f0f0")))))

;; set flycheck faces underline style
(custom-set-faces
 '(flycheck-error ((t (:underline "#e45649"))))
 '(flycheck-info ((t (:underline "#50a14f"))))
 '(flycheck-warning ((t (:underline "#986801")))))

;; set markdown faces
(custom-set-faces
 '(markdown-header-face-1 ((t (:inherit org-level-1))))
 '(markdown-header-face-2 ((t (:inherit org-level-2))))
 '(markdown-header-face-3 ((t (:inherit org-level-3)))))

;; set lsp-intellij face
(custom-set-faces
 '(lsp-intellij-face-code-lens-run ((t (:background "honeydew")))))

;; set imenu faces
(custom-set-faces
 '(imenu-list-entry-face-0 ((t (:inherit font-lock-keyword-face))))
 '(imenu-list-entry-face-1 ((t (:inherit font-lock-string-face))))
 '(imenu-list-entry-face-2 ((t (:inherit font-lock-preprocessor-face))))
 '(imenu-list-entry-face-3 ((t (:inherit font-lock-function-name-face)))))

;; enable ligatures support (emacs mac port only)
;; others here: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
(cond
 ((string-equal system-type "darwin")   ; Mac OS X
  (mac-auto-operator-composition-mode))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                   (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                   (36 . ".\\(?:>\\)")
                   (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                   (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                   (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                   (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                   (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                   ;; (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                   (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                   (48 . ".\\(?:x[a-zA-Z]\\)")
                   (58 . ".\\(?:::\\|[:=]\\)")
                   (59 . ".\\(?:;;\\|;\\)")
                   (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                   (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                   (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                   (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                   (91 . ".\\(?:]\\)")
                   (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                   (94 . ".\\(?:=\\)")
                   (119 . ".\\(?:ww\\)")
                   (123 . ".\\(?:-\\)")
                   (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                   (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                   )
                 ))
      (dolist (char-regexp alist)
        (set-char-table-range composition-function-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring])))))))

;; set the wrap line symbol
(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b11110000
   #b00010000
   #b00010000
   #b00010000
   #b00000000
   #b00000000
   #b00000000
   #b00000000])
(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00001000
   #b00001000
   #b00001000
   #b00001111
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000])
(setq visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
