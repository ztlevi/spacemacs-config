;; c++
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

;; Enable for other modes
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'web-mode-hook 'flycheck-mode)
(add-hook 'json-mode-hook 'flycheck-mode)
