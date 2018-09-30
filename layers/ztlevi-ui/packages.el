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
    spaceline-all-the-icons
    popwin
    (ivy-posframe :toggle (version<= "26" emacs-version))
    (company-box :toggle (version<= "26" emacs-version))

    ;; To use local repo, update the packages to clean up the cache
    ;; (doom-themes :location "~/Developer/Github/emacs-doom-themes")

    ;; (ztlevi-modeline :location local)
    )
  )

(defun ztlevi-ui/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :defer t
    :hook (dired-mode . all-the-icons-dired-mode)))

(defun ztlevi-ui/init-all-the-icons ()
  (use-package all-the-icons
    :defer t))

(defun ztlevi-ui/post-init-spaceline-all-the-icons ()
  (setq spaceline-all-the-icons-clock-always-visible nil
        spaceline-all-the-icons-window-number-always-visible t
        spaceline-all-the-icons-flycheck-alternate t
        spaceline-all-the-icons-hide-long-buffer-path t
        spaceline-all-the-icons-highlight-file-name t)

  (setq spaceline-all-the-icons-icon-set-git-ahead (quote commit)))

;; copy from doom-emacs ivy-posframe
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/completion/ivy/config.el#L175
(defun ztlevi-ui/init-ivy-posframe ()
  (use-package ivy-posframe
    :defer t
    :preface
    ;; This function searches the entire `obarray' just to populate
    ;; `ivy-display-functions-props'. There are 15k entries in mine! This is
    ;; wasteful, so...
    (advice-add #'ivy-posframe-setup :override #'ignore)
    :init
    (ivy-posframe-enable)
    (defun set-ivy-posframe-width ()
      (setq ivy-posframe-width (round (* 0.75 (frame-width)))))
    (add-hook 'minibuffer-inactive-mode-hook 'set-ivy-posframe-width)
    :config
    (setq ivy-fixed-height-minibuffer nil
          ivy-posframe-parameters
          `((min-width . 90)
            (min-height . ,ivy-height)
            (internal-border-width . 10)))

    ;; ... let's do it manually instead
    (unless (assq 'ivy-posframe-display-at-frame-bottom-left ivy-display-functions-props)
      (dolist (fn (list 'ivy-posframe-display-at-frame-bottom-left
                        'ivy-posframe-display-at-frame-center
                        'ivy-posframe-display-at-point
                        'ivy-posframe-display-at-frame-bottom-window-center
                        'ivy-posframe-display
                        'ivy-posframe-display-at-window-bottom-left
                        'ivy-posframe-display-at-window-center
                        '+ivy-display-at-frame-center-near-bottom))
        (push (cons fn '(:cleanup ivy-posframe-cleanup)) ivy-display-functions-props)))
    ;; default to posframe display function
    (setf (alist-get t ivy-display-functions-alist) #'+ivy-display-at-frame-center-near-bottom)))

(defun ztlevi-ui/init-company-box ()
  (use-package company-box
    :defer t
    :hook (company-mode . company-box-mode)
    :config
    (setq company-box-backends-colors nil
          company-box-max-candidates 50
          company-box-icons-yasnippet (all-the-icons-material "short_text" :height 0.8 :face 'all-the-icons-green)
          company-box-icons-unknown (all-the-icons-material "find_in_page" :height 0.8 :face 'all-the-icons-cyan)
          company-box-icons-elisp
          (list (all-the-icons-material "code"         :height 0.8 :face 'all-the-icons-red)    ;method
                (all-the-icons-material "check_circle" :height 0.8 :face 'all-the-icons-blue)   ;field
                (all-the-icons-material "stars"        :height 0.8 :face 'all-the-icons-orange) ;class
                (all-the-icons-material "format_paint" :height 0.8 :face 'all-the-icons-pink))  ;color palette
          company-box-icons-lsp
          `((1  . ,(all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green)) ; text
            (2  . ,(all-the-icons-material "code"                     :height 0.8 :face 'all-the-icons-red))   ; method
            (3  . ,(all-the-icons-material "code"                     :height 0.8 :face 'all-the-icons-red))   ; function
            (4  . ,(all-the-icons-material "code"                     :height 0.8 :face 'all-the-icons-red))   ; constructor
            (5  . ,(all-the-icons-material "code"                     :height 0.8 :face 'all-the-icons-red))   ; field
            (6  . ,(all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))  ; variable
            (7  . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))   ; class
            (8  . ,(all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-red))   ; interface
            (9  . ,(all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-red))   ; module
            (10 . ,(all-the-icons-material "settings"                 :height 0.8 :face 'all-the-icons-red))   ; property
            (11 . ,(all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-red))   ; unit
            (12 . ,(all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-red))   ; value
            (13 . ,(all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-red))   ; enum
            (14 . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-red))   ; keyword
            (15 . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-red))   ; snippet
            (16 . ,(all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-red))   ; color
            (17 . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-red))   ; file
            (18 . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-red))   ; reference
            (19 . ,(all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-red))   ; folder
            (20 . ,(all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-red))   ; enumMember
            (21 . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-red))   ; constant
            (22 . ,(all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-red))   ; struct
            (23 . ,(all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-red))   ; event
            (24 . ,(all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-red))   ; operator
            (25 . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))))

    ;; Until sebastiencs/company-box#40 is merged
    (defun +company*box-frontend-even-if-single (command)
      (cond ((eq command 'hide)
             (company-box-hide))
            ((equal company-candidates-length 0)
             (company-box-hide))
            ((eq command 'update)
             (company-box-show))
            ((eq command 'post-command)
             (company-box--post-command))))
    (advice-add #'company-box-frontend :override #'+company*box-frontend-even-if-single)
    ))

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

(defun ztlevi-ui/init-ztlevi-modeline ()
  (use-package ztlevi-modeline))
