(defconst ztlevi-ui-packages
  '(
    all-the-icons
    )
  )

(defun ztlevi-ui/init-all-the-icons ()
  (use-package all-the-icons
    :init
    (progn
      (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
      (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
)))
