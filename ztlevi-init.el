;; =============================================================================
;; ztlevi's init settings here
;; =============================================================================

;; set perspective auto save
(setq persp-auto-save-persps-to-their-file nil)
(setq persp-auto-save-num-of-backups 0)
(setq persp-auto-resume-time 0)

;; set default open scratch
(when (string= "*scratch*" (buffer-name))
  (spacemacs/switch-to-scratch-buffer))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" " ztlevi - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; set others
(setq evil-insert-state-cursor '("chartreuse3" (bar . 2)))
(setq zilongshanren-programming/post-init-js-doc nil)

