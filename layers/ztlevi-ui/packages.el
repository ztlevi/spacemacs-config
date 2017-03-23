;;; packages.el --- ztlevi-ui layer packages file for Spacemacs.
;;
;; Copyright (c) 2016-2017 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/Spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ztlevi-ui-packages
  '(
    ;; all-the-icons-dired
    all-the-icons
    )
  )

;; (defun ztlevi-ui/init-all-the-icons-dired ()
;;   (use-package all-the-icons-dired))

(defun ztlevi-ui/init-all-the-icons ()
  (use-package all-the-icons
    :init
    (progn
      (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
      (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
)))
