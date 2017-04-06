;; set initl screen size
(setq initial-frame-alist
      '(
        (width . 86) ; character
        (height . 45) ; lines
        ))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" " ztlevi - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; set scrolling speed
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; File colum indicator 80 chars
(setq-default fci-rule-column 81)
(setq fci-rule-color "grey80")

;; SPC t l // toggle-truncate-lines
(add-hook 'text-mode-hook 'toggle-truncate-lines)

;; all-the-icons add hook
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
