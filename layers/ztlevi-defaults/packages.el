;;; packages.el --- ztlevi layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2016-2018 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ztlevi-defaults-packages
  '(
    ;; (auto-save :location local)
    ranger
    helpful
    (profiler :location built-in)
    (recentf :location built-in)
    ))

(defun ztlevi-defaults/post-init-ranger ()
  ;; ranger and deer
  (spacemacs/set-leader-keys "fj" 'deer)
  (with-eval-after-load 'ranger
    (define-key ranger-normal-mode-map (kbd "f") 'counsel-find-file)
    (define-key ranger-normal-mode-map (kbd "C-<tab>") 'ranger-next-tab)
    (define-key ranger-normal-mode-map (kbd "C-S-<tab>") 'ranger-prev-tab)
    (define-key ranger-normal-mode-map (kbd "U") 'dired-unmark-all-files)
    (define-key ranger-normal-mode-map (kbd "u") 'dired-unmark)
    (define-key ranger-normal-mode-map (kbd "(") 'dired-hide-details-mode)
    (define-key ranger-normal-mode-map (kbd "+") 'dired-create-directory))
  (setq ranger-omit-regexp "^\.DS_Store$")
  (setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
  (setq ranger-deer-show-details nil)
  (setq ranger-max-preview-size 10)
)

(defun ztlevi-defaults/init-helpful ()
  (use-package helpful
    :defer t
    :init
    (global-set-key (kbd "C-h f") #'helpful-callable)
    (global-set-key (kbd "C-h C") #'helpful-command)
    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key)

    (spacemacs/declare-prefix "hh" "helpful")
    (spacemacs/set-leader-keys
      "hhh" #'helpful-at-point
      "hhc" #'helpful-command
      "hhf" #'helpful-callable
      "hhk" #'helpful-key
      "hhm" #'helpful-macro
      "hhv" #'helpful-variable)
    :config
    (evil-set-initial-state 'helpful-mode 'normal)
    (evil-make-overriding-map helpful-mode-map 'normal)))

(defun ztlevi-defaults/init-auto-save ()
  (use-package auto-save
    :config
    (auto-save-enable)
    (setq auto-save-slient t)))

(defun ztlevi-defaults/post-init-recentf ()
  (progn
    (setq recentf-exclude
          '("COMMIT_MSG"
            "COMMIT_EDITMSG"
            "github.*txt$"
            "/tmp/"
            "/ssh:"
            "/sudo:"
            "/TAGS$"
            "/GTAGS$"
            "/GRAGS$"
            "/GPATH$"
            "\\.mkv$"
            "\\.mp[34]$"
            "\\.avi$"
            "\\.pdf$"
            "\\.sub$"
            "\\.srt$"
            "\\.ass$"
            ".*png$"))
    (setq recentf-max-saved-items 2048)))

(defun ztlevi-defaults/init-profiler ()
  (use-package profiler
    :defer t
    :init
    (evilified-state-evilify profiler-report-mode profiler-report-mode-map)))

;;; packages.el ends here
