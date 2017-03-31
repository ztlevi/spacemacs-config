;; Flyckeck add hook to other modes
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'json-mode-hook 'flycheck-mode)
(add-hook 'web-mode-hook 'flycheck-mode)

;; ctags add hook
(add-hook 'java-mode-hook 'my-setup-develop-environment)

;; yasnippet fix tab
(eval-after-load 'prog-mode
  '(progn
     (define-key prog-mode-map (kbd "TAB")
       (lambda()
         (interactive)
         (let ((yas/fallback-behavior 'return-nil))
           (unless (yas/expand)
             (indent-for-tab-command)
             (if (looking-back "^\s*")
                 (back-to-indentation))))))))
