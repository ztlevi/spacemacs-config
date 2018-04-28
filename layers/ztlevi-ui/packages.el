;;; packages.el --- ztlevi-ui layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2016-2018 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ztlevi-ui-packages
  '(
    all-the-icons
    all-the-icons-dired
    ;; doom modeline needs all-the-icons, shrink-path
    (doom-modeline :location local)
    shrink-path
    ;; (ztlevi-modeline :location local)
    popwin
    (ivy-posframe :toggle (version<= "26" emacs-version))
    (whitespace :location built-in)
    doom-themes
    ;; To use local repo, update the packages to clean up the cache
    ;; (doom-themes :location "~/Developer/Github/emacs-doom-themes")
    )
  )

(defun ztlevi-ui/init-doom-themes ()
  (use-package doom-themes
    :init
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    :config
    ;; Enable flashing mode-line on errors
    ;; (doom-themes-visual-bell-config)

    ;; Enable custom neotree theme
    (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)

    ;; custom doom-one-light-theme
    (with-eval-after-load 'doom-one-light-theme
      (custom-set-faces
       '(doom-modeline-error ((t (:background "#e45649" :foreground "#f0f0f0"))))
       '(ivy-posframe ((t (:background "white smoke"))))
       '(lsp-ui-doc-background ((t (:background "white smoke"))))
       '(origami-fold-replacement-face ((t (:inherit (quote font-lock-keyword-face)))))
       '(show-paren-match ((t (:background "light gray"))))
       '(tooltip ((t (:background "#dfdfdf" :foreground "#383a42"))))))))

(defun ztlevi-ui/init-doom-modeline ()
  (use-package doom-modeline
    :init
    (use-package all-the-icons)

    ;; set doom-modeline height
    (setq +doom-modeline-height 36)

    ;; file-name style
    (setq +doom-modeline-buffer-file-name-style 'relative-to-project)

    (add-hook 'after-init-hook #'+doom-modeline|init)))

(defun ztlevi-ui/init-shrink-path ()
  (use-package shrink-path
    :defer t
    :commands (shrink-path-prompt shrink-path-file-mixed)))

(defun ztlevi-ui/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :defer t
    :init
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
    ))

(defun ztlevi-ui/init-all-the-icons ()
  (use-package all-the-icons
    :defer t))

(defun ztlevi-ui/init-ivy-posframe ()
  (use-package ivy-posframe
    :defer t
    :init
    (setq ivy-posframe-parameters
          '((left-fringe . 10)
            (right-fringe . 10)))

    ;; https://github.com/tumashu/ivy-posframe#how-to-enable-ivy-posframe
    (setq ivy-display-function #'ivy-posframe-display)

    (ivy-posframe-enable)))

(defun ztlevi-ui/post-init-pangu-spacing ()
  (progn
    ;; add toggle options
    (spacemacs|add-toggle toggle-pangu-spaceing
      :status pangu-spacing-mode
      :on (global-pangu-spacing-mode)
      :off (global-pangu-spacing-mode -1)
      :documentation "Toggle pangu spacing mode"
      :evil-leader "ots")
    (add-hook 'markdown-mode-hook
              #'(lambda ()
                  (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))))

(defun ztlevi-ui/post-init-popwin ()
  (progn
    (push "*ztlevi/run-current-file output*" popwin:special-display-config)
    (delete "*Async Shell Command*" popwin:special-display-config)))

(defun ztlevi-ui/post-init-whitespace ()
  (progn
    ;; ;; http://emacsredux.com/blog/2013/05/31/highlight-lines-that-exceed-a-certain-length-limit/
    (setq whitespace-line-column fill-column) ;; limit line length
    ;;https://www.reddit.com/r/emacs/comments/2keh6u/show_tabs_and_trailing_whitespaces_only/
    ;; (setq whitespace-style '(face lines-tail))
    ;; show tab;  use untabify to convert tab to whitespace
    (setq spacemacs-show-trailing-whitespace nil)

    (setq-default tab-width 4)
    ;; set-buffer-file-coding-system -> utf8 to convert dos to utf8
    ;; (setq inhibit-eol-conversion t)
    ;; (add-hook 'prog-mode-hook 'whitespace-mode)

    ;; (global-whitespace-mode +1)

    (with-eval-after-load 'whitespace
      (progn
        (set-face-attribute 'whitespace-trailing nil
                            :inherit font-lock-keyword-face
                            :underline t)
        (set-face-attribute 'whitespace-tab nil
                            :inherit font-lock-string-face
                            :underline t
                            :weight 'bold)))
    (diminish 'whitespace-mode)))

(defun ztlevi-ui/init-ztlevi-modeline ()
  (use-package ztlevi-modeline))
