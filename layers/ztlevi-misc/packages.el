;;; packages.el --- ztlevi-misc layer packages file for Spacemacs.
;;
;; Copyright (c) 2016-2017 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/Spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ztlevi-misc-packages
  '(
    ranger
    )
  )

(defun ztlevi-misc/post-init-ranger ()
  (progn
    ;; (use-package bookmark
    ;;   :defer)
    ;; ranger replace dired-mode
    (ranger-override-dired-mode t)
    ))
