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
(setq evil-normal-state-tag    (propertize " N " 'face '((:inherit spacemacs-normal-face)))
      evil-emacs-state-tag     (propertize " E " 'face '((:inherit spacemacs-emacs-face)))
      evil-hybrid-state-tag    (propertize " H " 'face '((:inherit spacemacs-hybrid-face)))
      evil-insert-state-tag    (propertize " I " 'face '((:inherit spacemacs-insert-face)))
      evil-motion-state-tag    (propertize " M " 'face '((:inherit spacemacs-motion-face)))
      evil-visual-state-tag    (propertize " V " 'face '((:inherit spacemacs-visual-face)))
      evil-evilified-state-tag (propertize " F " 'face '((:inherit spacemacs-evilified-face)))
      evil-replace-state-tag   (propertize " R " 'face '((:inherit spacemacs-replace-face)))
      evil-operator-state-tag  (propertize " O " 'face '((:background "purple" :foreground "white"))))

(setq my-flycheck-mode-line
      '(:eval
        (pcase flycheck-last-status-change
          ((\` not-checked) nil)
          ((\` no-checker) (propertize " -" 'face 'warning))
          ((\` running) (propertize " *" 'face 'success))
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

(setq modeline-height 1)

(setq-default mode-line-format
              (list
               ;; evil state
               '(:eval evil-mode-line-tag)

               ;; set the modeline height
               `(:eval (propertize
                        " %1"
                        'face '(:height ,modeline-height)))

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
               '(:eval
                 (if (not (and (buffer-modified-p) buffer-read-only))
                     (cond ((buffer-modified-p)
                            (propertize
                             "Mod"
                             'face '(:inherit font-lock-preprocessor-face)
                             'help-echo "Buffer has been modified"))
                           (buffer-read-only
                            (propertize
                             "RO"
                             'face '(:inherit font-lock-type-face :weight bold)
                             'help-echo "Buffer is read-only")))
                   ))

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
               ))

(provide 'ztlevi-modeline)
