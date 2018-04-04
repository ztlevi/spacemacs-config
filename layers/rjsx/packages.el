(defconst rjsx-packages
  '(
    add-node-modules-path
    rjsx-mode
    emmet-mode
    evil-matchit
    flycheck
    js-doc
    smartparens
    ))

(defun rjsx/post-init-add-node-modules-path ()
  (add-hook 'rjsx-mode-hook #'add-node-modules-path))

(defun rjsx/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init
    (defun +javascript-jsx-file-p ()
      (and buffer-file-name
           (equal (file-name-extension buffer-file-name) "js")
           (re-search-forward "\\(^\\s-*import React\\|\\( from \\|require(\\)[\"']react\\)"
                              magic-mode-regexp-match-limit t)
           (progn (goto-char (match-beginning 1))
                  (not (sp-point-in-string-or-comment)))))

    (push (cons #'+javascript-jsx-file-p 'rjsx-mode) magic-mode-alist)

    :config
    ;; declare prefix
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mr" "refactor")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrr" "rename")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mh" "documentation")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mg" "goto")

    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "rrt" 'rjsx-rename-tag-at-point)

    (with-eval-after-load 'rjsx-mode
      (define-key rjsx-mode-map (kbd "C-d") nil))))

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

(defun rjsx/init-smartparens ()
  (use-package smartparens
    :commands sp-point-in-string-or-comment
    :defer t))
