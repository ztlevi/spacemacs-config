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

;; =============== doom-themes post init ===============
;; Enable flashing mode-line on errors
;; (doom-themes-visual-bell-config)

;; Corrects (and improves) org-mode's native fontification.
(with-eval-after-load 'org
  (doom-themes-org-config))

;; set scrolling speed
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; set initl screen size
(setq initial-frame-alist
      '((width . 110)
        (height . 65)))

;; set evil cursors colors
(setq spacemacs-evil-cursors '(("normal"       "#4078F2" box)
                               ("insert"       "#BFD641" (bar . 2))
                               ("emacs"        "#A0BCF8" box)
                               ("hybrid"       "#FF007F" (bar . 2))
                               ("replace"      "#F7786B" (hbar . 2))
                               ("evilified"    "#DAA520" box)
                               ("visual"       "gray"    (hbar . 2))
                               ("motion"       "#9370DB" box)
                               ("lisp"         "#6B5B95" box)
                               ("iedit"        "#E94B3C" box)
                               ("iedit-insert" "#E94B3C" (bar . 2))))

;; set faces after doom-one-light
(if (eq spacemacs--cur-theme 'doom-one-light)
    (custom-set-faces
     ;; set spaceline faces
     `(powerline-active1 ((t (:inherit mode-line :background ,(doom-color 'base1)))))
     `(powerline-active2 ((t (:inherit mode-line :background ,(doom-color 'base3)))))
     `(powerline-inactive1 ((t (:inherit mode-line-inactive :background ,(doom-color 'base2)))))
     `(powerline-inactive2 ((t (:inherit mode-line-inactive :background ,(doom-color 'base2)))))
     ;; others
     '(tide-hl-identifier-face ((t (:inherit lsp-face-highlight-read))))
     `(show-paren-match ((t (:background ,(doom-color 'base2) :foreground ,(doom-color 'red) :weight ultra-bold))))))

;; set faces for others
(custom-set-faces
 '(origami-fold-replacement-face ((t (:inherit (quote font-lock-keyword-face)))))
 '(term-bold ((t (:weight bold :height 1.1 :family "Ubuntu Mono derivative Powerline")))))

;; set faces after doom-themes
;; (custom-set-faces
;;  '(doom-modeline-error ((t (:background "#e45649" :foreground "#f0f0f0")))))

;; set flycheck faces underline style
(custom-set-faces
 `(flycheck-error ((t (:underline ,(doom-color 'red)))))
 `(flycheck-info ((t (:underline ,(doom-color 'green)))))
 `(flycheck-warning ((t (:underline ,(doom-color 'yellow))))))

;; set markdown faces
(custom-set-faces
 '(markdown-header-face-1 ((t (:inherit org-level-1))))
 '(markdown-header-face-2 ((t (:inherit org-level-2))))
 '(markdown-header-face-3 ((t (:inherit org-level-3)))))

;; set lsp-intellij face
(custom-set-faces
 '(lsp-intellij-face-code-lens-run ((t (:background "honeydew")))))

;; enable natural title bar for emacs-plus
(if (is-emacs-plus)
    (progn
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . light))))

;; enable ligatures support
;; details here: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
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
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

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
