(spacemacs|define-jump-handlers rjsx-mode)

;; comment jsx region
(add-hook 'rjsx-mode-hook (lambda ()
                            (push '(?/ . ("{/*" . "*/}")) evil-surround-pairs-alist)))
