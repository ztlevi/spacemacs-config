;; Flyckeck add hook to other modes
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'json-mode-hook 'flycheck-mode)
(add-hook 'web-mode-hook 'flycheck-mode)

;; ctags add hook
(add-hook 'java-mode-hook 'my-setup-develop-environment)
