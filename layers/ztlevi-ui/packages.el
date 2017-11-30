;;; packages.el --- ztlevi-ui layer packages file for Spacemacs.
;;
;; Copyright (c) 2016-2017 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ztlevi-ui-packages
  '(
    all-the-icons-dired
    ;; doom modeline needs all-the-icons, shrink-path enabled
    (doom-modeline :location local)
    shrink-path
    all-the-icons
    ;; if you wnat to use spaceline, please comment out ztlevi-mode-line
    ;; (ztlevi-mode-line :location built-in)
    ;; spaceline
    diminish
    popwin
    (whitespace :location built-in)
    doom-themes
    ;; To use local repo, update the packages to clean up the cache
    ;; (doom-themes :location "~/Developer/Github/emacs-doom-themes")
    ;; hl-anything performance is very slow...
    ;; hl-anything
    ;; beacon
    ;; evil-vimish-fold
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
    ))

(defun ztlevi-ui/init-doom-modeline ()
  (use-package doom-modeline
    :init
    (defun t/project-root ()
      "Get project root without throwing"
      (let (projectile-require-project-root strict-p)
        (projectile-project-root)))

    (defun t/init-modeline () (+doom-modeline|init))
    (add-hook 'after-init-hook #'t/init-modeline)
    (use-package all-the-icons)
    ))

(defun ztlevi-ui/init-shrink-path ()
  (use-package shrink-path
    :commands (shrink-path-prompt shrink-path-file-mixed)))

(defun ztlevi-ui/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :defer t))

(defun ztlevi-ui/init-all-the-icons ()
  (use-package all-the-icons
    :defer t))

(defun ztlevi-ui/init-ztlevi-mode-line ()
  (defun ztlevi/display-mode-indent-width ()
    (let ((mode-indent-level
           (catch 'break
             (dolist (test spacemacs--indent-variable-alist)
               (let ((mode (car test))
                     (val (cdr test)))
                 (when (or (and (symbolp mode) (derived-mode-p mode))
                           (and (listp mode) (apply 'derived-mode-p mode))
                           (eq 't mode))
                   (when (not (listp val))
                     (setq val (list val)))
                   (dolist (v val)
                     (cond
                      ((integerp v) (throw 'break v))
                      ((and (symbolp v) (boundp v))
                       (throw 'break (symbol-value v))))))))
             (throw 'break (default-value 'evil-shift-width)))))
      (concat "TS:" (int-to-string (or mode-indent-level 0)))))

  ;; set state tag
  (setq evil-normal-state-tag    (propertize " 𝗡 " 'face '((:inherit spacemacs-normal-face)))
        evil-emacs-state-tag     (propertize " 𝗘 " 'face '((:inherit spacemacs-emacs-face)))
        evil-hybrid-state-tag    (propertize " 𝗛 " 'face '((:inherit spacemacs-hybrid-face)))
        evil-insert-state-tag    (propertize " 𝗜 " 'face '((:inherit spacemacs-insert-face)))
        evil-motion-state-tag    (propertize " 𝗠 " 'face '((:inherit spacemacs-motion-face)))
        evil-visual-state-tag    (propertize " 𝗩 " 'face '((:inherit spacemacs-visual-face)))
        evil-evilified-state-tag (propertize " 𝗙 " 'face '((:inherit spacemacs-evilified-face)))
        evil-replace-state-tag   (propertize " 𝗥 " 'face '((:inherit spacemacs-replace-face)))
        evil-operator-state-tag  (propertize " 𝗢 " 'face '((:background "purple" :foreground "white"))))

  (setq my-flycheck-mode-line
        '(:eval
          (pcase flycheck-last-status-change
            ((\` not-checked) nil)
            ((\` no-checker) (propertize " -" 'face 'warning))
            ((\` running) (propertize " ✷" 'face 'success))
            ((\` errored) (propertize " !" 'face 'error))
            ((\` finished)
             (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                    (no-errors (cdr (assq 'error error-counts)))
                    (no-warnings (cdr (assq 'warning error-counts)))
                    (face (cond (no-errors 'error)
                                (no-warnings 'warning)
                                (t 'success))))
               (propertize (format "[%s/%s]" (or no-errors 0) (or no-warnings 0))
                           'face face)))
            ((\` interrupted) " -")
            ((\` suspicious) '(propertize " ?" 'face 'warning)))))

  (setq-default mode-line-misc-info
                (assq-delete-all 'which-func-mode mode-line-misc-info))

  (setq modeline-height 1.35)
  (setq-default mode-line-format
                (list
                 ;; evil state
                 '(:eval evil-mode-line-tag)

                 ;; set the modeline height
                 '(:eval (propertize
                          " %1"
                          'face '(:height `modeline-height)))

                 ;; linum
                 ;; '(:eval (propertize
                 ;;          (window-number-mode-line)
                 ;;          'face
                 ;;          'font-lock-keyword-face))
                 ;; " "
                 '(:eval (ztlevi/update-persp-name))

                 "%1 "
                 ;; the buffer name; the file name as a tool tip
                 '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                                     'help-echo (buffer-file-name)))

                 ;; '(:eval (propertize
                 ;;          (if overwrite-mode "OVR" "INS")
                 ;;          'face '(:inherit font-lock-preprocessor-face
                 ;;                           :underline t :overline t)
                 ;;          'help-echo (concat "Buffer is in "
                 ;;                             (if overwrite-mode
                 ;;                                 "overwrite"
                 ;;                               "insert") " mode")))

                 ;; was this buffer modified since the last save?
                 '(:eval (if (buffer-modified-p)
                             (propertize
                              "M"
                              'face '(:inherit font-lock-string-face
                                               :weight bold :underline t :overline t)
                              'help-echo "Buffer has been modified")
                           " "))

                 " "

                 ;; is this buffer read-only?
                 '(:eval (if buffer-read-only
                             (propertize
                              "R"
                              'face '(:inherit font-lock-type-face
                                               :weight bold :underline t :overline t)
                              'help-echo "Buffer is read-only")
                           " "))

                 " "
                 ;; anzu
                 anzu--mode-line-format

                 ;; the current major mode for the buffer.
                 '(:eval (propertize "%m" 'face 'font-lock-string-face
                                     'help-echo buffer-file-coding-system))

                 "%1 "
                 my-flycheck-mode-line
                 "%1"

                 ;; minor modes
                 '(:eval (when (> (window-width) 90)
                           minor-mode-alist))
                 " "
                 ;; git info
                 '(:eval (when (> (window-width) 120)
                           `(vc-mode vc-mode)))

                 " "

                 ;; global-mode-string goes in mode-line-misc-info
                 '(:eval (when (> (window-width) 120)
                           mode-line-misc-info))

                 (mode-line-fill 'mode-line 22)

                 '(:eval (propertize (ztlevi/display-mode-indent-width)))

                 ;; relative position, size of file
                 (propertize " [%p" 'face 'font-lock-constant-face) ;; % above top
                 "/"
                 (propertize "%I] " 'face 'font-lock-constant-face) ;; size

                 ;; ;; line and column
                 ;; '(:eval (propertize
                 ;;          (concat
                 ;;           " (" ;; '%02' to set to 2 chars at least; prevents flickering
                 ;;           (propertize "%l") ","
                 ;;           (propertize "%c") ") ")
                 ;;          'face
                 ;;          'font-lock-type-face))

                 '(:eval (when (> (window-width) 80)
                           (buffer-encoding-abbrev)))
                 mode-line-end-spaces
                 ;; add the time, with the date and the emacs uptime in the tooltip
                 ;; '(:eval (propertize (format-time-string "%H:%M")
                 ;;                     'help-echo
                 ;;                     (concat (format-time-string "%c; ")
                 ;;                             (emacs-uptime "Uptime:%hh"))))
                 )))

(defun ztlevi-ui/post-init-diminish ()
  (progn
    (with-eval-after-load 'all-the-icons-dired
      (diminish 'all-the-icons-dired-mode))
    (with-eval-after-load 'whitespace
      (diminish 'whitespace-mode))
    (with-eval-after-load 'smartparens
      (diminish 'smartparens-mode))
    (with-eval-after-load 'which-key
      (diminish 'which-key-mode))
    (with-eval-after-load 'hungry-delete
      (diminish 'hungry-delete-mode))))


(defun ztlevi-ui/post-init-spaceline ()
  (use-package spaceline-config
    :config
    (progn
      (defvar spaceline-org-clock-format-function
        'org-clock-get-clock-string
        "The function called by the `org-clock' segment to determine what to show.")

      (spaceline-define-segment org-clock
                                "Show information about the current org clock task.  Configure
`spaceline-org-clock-format-function' to configure. Requires a currently running
org clock.

This segment overrides the modeline functionality of `org-mode-line-string'."
                                (when (and (fboundp 'org-clocking-p)
                                           (org-clocking-p))
                                  (substring-no-properties (funcall spaceline-org-clock-format-function)))
                                :global-override org-mode-line-string)

      (spaceline-compile
       'ztlevi
       ;; Left side of the mode line (all the important stuff)
       '(((persp-name
           workspace-number
           window-number
           )
          :separator "|"
          :face highlight-face)
         ((buffer-modified buffer-size input-method))
         anzu
         '(buffer-id remote-host buffer-encoding-abbrev)
         ((point-position line-column buffer-position selection-info)
          :separator " | ")
         major-mode
         process
         (flycheck-error flycheck-warning flycheck-info)
         ;; (python-pyvenv :fallback python-pyenv)
         ((minor-modes :separator spaceline-minor-modes-separator) :when active)
         (org-pomodoro :when active)
         (org-clock :when active)
         nyan-cat)
       ;; Right segment (the unimportant stuff)
       '((version-control :when active)
         battery))

      (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ztlevi))))
      )))

(defun ztlevi-ui/init-beacon ()
  (use-package beacon
    :init
    (progn
      (spacemacs|add-toggle beacon
        :status beacon-mode
        :on (beacon-mode)
        :off (beacon-mode -1)
        :documentation "Enable point highlighting after scrolling"
        :evil-leader "otb")

      (spacemacs/toggle-beacon-on))
    :config (spacemacs|hide-lighter beacon-mode)))

(defun ztlevi-ui/init-evil-vimish-fold ()
  (use-package evil-vimish-fold
    :init
    (vimish-fold-global-mode 1)
    :config
    (progn
      (define-key evil-normal-state-map (kbd "zf") 'vimish-fold)
      (define-key evil-visual-state-map (kbd "zf") 'vimish-fold)
      (define-key evil-normal-state-map (kbd "zd") 'vimish-fold-delete)
      (define-key evil-normal-state-map (kbd "za") 'vimish-fold-toggle))))

(defun ztlevi-ui/post-init-hl-anything ()
  (progn
    (defun my-inhibit-globalized-hl-highlight-mode ()
      "Counter-act a globalized hl-highlight-mode."
      (set (make-local-variable 'hl-highlight-mode) nil))

    (add-hook 'org-agenda-mode-hook 'my-inhibit-globalized-hl-highlight-mode)
    (hl-highlight-mode -1)
    (spacemacs|add-toggle toggle-hl-anything
      :status hl-highlight-mode
      :on (hl-highlight-mode)
      :off (hl-highlight-mode -1)
      :documentation "Toggle highlight anything mode."
      :evil-leader "ths")))

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
