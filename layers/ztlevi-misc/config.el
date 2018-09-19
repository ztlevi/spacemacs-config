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

;; flyspell
(spacemacs/add-to-hooks #'flyspell-mode '(org-mode-hook
                                          markdown-mode-hook))

;; disable spacemacs clean table, using prettier instead
(remove-hook 'markdown-mode-hook 'spacemacs//cleanup-org-tables-on-save)

;; confirmation is requested before visiting a new file or buffer.
(setq confirm-nonexistent-file-or-buffer t)

;; customize helm
(with-eval-after-load 'helm-make
  (progn
    ;; limit max number of matches displayed for speed
    (setq helm-candidate-number-limit 100)
    ;; ignore boring files like .o and .a
    (setq helm-ff-skip-boring-files t)
    ;; replace locate with spotlight on Mac
    (setq helm-locate-command "mdfind -name %s %s")
    (push "\\.emlx$" helm-boring-file-regexp-list)))

(define-abbrev-table 'global-abbrev-table '(
                                            ;; math/unicode symbols
                                            ("8in" "∈")
                                            ("8nin" "∉")
                                            ("8inf" "∞")
                                            ("8luv" "♥")
                                            ("8smly" "☺")
                                            ("8en" "@~english")
                                            ("8zh" "@~chinese")
                                            ("8sp" "spacemacs")

                                            ;; email
                                            ("8me" "zhouting@umich.com")

                                            ;; computing tech
                                            ("8wp" "Wikipedia")
                                            ("8ms" "Microsoft")
                                            ("8g" "Google")
                                            ("8it" "IntelliType")
                                            ("8msw" "Microsoft Windows")
                                            ("8win" "Windows")
                                            ("8ie" "Internet Explorer")
                                            ("8ahk" "AutoHotkey")

                                            ;; signature
                                            ("8zt" "ztlevi")
                                            ;; emacs regex
                                            ("8d" "\\([0-9]+?\\)")
                                            ("8str" "\\([^\"]+?\\)\"")))


(setq user-mail-address "zhouting@umich.com")
