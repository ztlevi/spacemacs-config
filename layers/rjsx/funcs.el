(defun spacemacs//setup-rjsx-mode ()
  "Adjust yas and emmet to accommodate rjsx-mode"
  (emmet-mode 0)
  ;; See https://github.com/CestDiego/emmet-mode/commit/3f2904196e856d31b9c95794d2682c4c7365db23
  (setq-local emmet-expand-jsx-className? t)
  ;; Enable js-mode snippets
  (yas-activate-extra-mode 'js-mode)
  ;; See https://github.com/syl20bnr/spacemacs/issues/8222
  (set (make-local-variable 'company-minimum-prefix-length) 2))
