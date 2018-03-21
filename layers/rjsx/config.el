(spacemacs|define-jump-handlers rjsx-mode)

;; comment jsx region
(add-hook 'rjsx-mode-hook (lambda ()
                            (push '(?/ . ("{/*" . "*/}")) evil-surround-pairs-alist)))

;; (add-to-list 'auto-mode-alist '("\\.react.js\\'" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("\\index.android.js\\'" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . rjsx-mode))
;; (add-to-list 'magic-mode-alist '("/\\*\\* @jsx .*\\*/" . rjsx-mode))
;; (add-to-list 'magic-mode-alist '(".*import\s+.+\s+from\s+['\"]react['\"]" . rjsx-mode))
