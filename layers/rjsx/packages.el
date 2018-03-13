(defconst rjsx-packages
  '(
    rjsx-mode
    company-tern
    emmet-mode
    evil-matchit
    flycheck
    js-doc
    smartparens
    tern
    ))

(defun rjsx/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init
    (progn
      ;; ;; company-tern
      ;; (spacemacs|add-company-backends :backends company-tern :modes rjsx-mode)

      ;; ;; emmet
      ;; (add-hook 'rjsx-mode-hook 'emmet-mode)

      ;; ;; evil-matchit
      ;; (with-eval-after-load 'evil-matchit
      ;;   (plist-put evilmi-plugins 'rjsx-mode
      ;;              '((evilmi-simple-get-tag evilmi-simple-jump)
      ;;                (evilmi-javascript-get-tag evilmi-javascript-jump)
      ;;                (evilmi-html-get-tag evilmi-html-jump))))

      ;; ;; flycheck
      ;; (with-eval-after-load 'flycheck
      ;;   (dolist (checker '(javascript-eslint javascript-standard))
      ;;     (flycheck-add-mode checker 'rjsx-mode)))
      ;; (spacemacs/enable-flycheck 'rjsx-mode)

      ;; ;; js doc
      ;; (add-hook 'rjsx-mode-hook 'spacemacs/js-doc-require)
      ;; (spacemacs/js-doc-set-key-bindings 'rjsx-mode)

      ;; ;; smartparens
      ;; (if dotspacemacs-smartparens-strict-mode
      ;;     (add-hook 'rjsx-mode-hook #'smartparens-strict-mode)
      ;;   (add-hook 'rjsx-mode-hook #'smartparens-mode))

      ;; ;; tern
      ;; (add-hook 'rjsx-mode-hook 'tern-mode)
      ;; ;; (spacemacs//set-tern-key-bindings 'rjsx-mode)

      ;; (add-hook 'rjsx-mode-hook 'spacemacs//setup-rjsx-mode)
      )
    :config
    (with-eval-after-load 'rjsx-mode
      (define-key rjsx-mode-map (kbd "C-d") nil))))

(defun rjsx/post-init-company-tern ()
  (spacemacs|add-company-backends :backends company-tern :modes rjsx-mode))

(defun rjsx/post-init-emmet-mode ()
  (add-hook 'rjsx-mode-hook 'emmet-mode))

(defun rjsx/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'rjsx-mode
               '((evilmi-simple-get-tag evilmi-simple-jump)
                 (evilmi-javascript-get-tag evilmi-javascript-jump)
                 (evilmi-html-get-tag evilmi-html-jump)))))

(defun rjsx/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (dolist (checker '(javascript-eslint javascript-standard))
      (flycheck-add-mode checker 'rjsx-mode)))
  (spacemacs/enable-flycheck 'rjsx-mode))

(defun rjsx/post-init-js-doc ()
  (add-hook 'rjsx-mode-hook 'spacemacs/js-doc-require)
  (spacemacs/js-doc-set-key-bindings 'rjsx-mode))

(defun rjsx/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (add-hook 'rjsx-mode-hook #'smartparens-strict-mode)
    (add-hook 'rjsx-mode-hook #'smartparens-mode)))

(defun rjsx/post-init-tern ()
  (add-hook 'rjsx-mode-hook 'tern-mode)
  (spacemacs//set-tern-key-bindings 'rjsx-mode))
