;;; packages.el --- lsp-intellij layer packages file for Spacemacs.
;;
;; Copyright (c) 2018 Ian Pickering
;;
;; Author:  <ipickering2@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst lsp-intellij-packages
  '(
    lsp-mode
    kotlin-mode
    flycheck
    (lsp-ui :toggle (configuration-layer/package-usedp 'flycheck))
    company
    (company-lsp :toggle (configuration-layer/package-usedp 'company))
    (lsp-intellij :location (recipe :fetcher github :repo "Ruin0x11/lsp-intellij"))
    ))

(defun lsp-intellij/init-lsp-mode ()
  (use-package lsp-mode))

(defun lsp-intellij/init-kotlin-mode ()
  (use-package kotlin-mode))

(defun lsp-intellij/init-lsp-ui ()
  (use-package lsp-ui))

(defun lsp-intellij/init-company-lsp ()
  (use-package company-lsp)

  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t)
  (push 'company-lsp company-backends))

(defun lsp-intellij/init-lsp-intellij ()
  (with-eval-after-load 'lsp-mode
    (use-package lsp-intellij
      :config
      (progn
        ;; key bindings
        (dolist (prefix '(
                          ("mc" . "configuration")
                          ("mg" . "goto")
                          ("mh" . "help/doc")
                          ("mp" . "project")
                          ("mr" . "refactor")
                          ("mI" . "IDEA")
                          ))
          (progn
            (spacemacs/declare-prefix-for-mode
              'java-mode (car prefix) (cdr prefix))
            (spacemacs/declare-prefix-for-mode
              'kotlin-mode (car prefix) (cdr prefix))))
        (spacemacs//lsp-intellij-setup-leader-keys 'java-mode)
        (spacemacs//lsp-intellij-setup-leader-keys 'kotlin-mode)
        (evil-define-key 'insert java-mode-map
          (kbd ".") 'spacemacs/java-lsp-completing-dot
          (kbd ":") 'spacemacs/java-lsp-completing-double-colon
          (kbd "M-.") 'xref-find-definitions
          (kbd "M-,") 'pop-tag-mark)
        (evil-define-key 'insert kotlin-mode-map
          (kbd ".") 'spacemacs/java-lsp-completing-dot
          (kbd ":") 'spacemacs/java-lsp-completing-double-colon
          (kbd "M-.") 'xref-find-definitions
          (kbd "M-,") 'pop-tag-mark))

      (add-hook 'java-mode-hook #'lsp-intellij-enable)
      (add-hook 'kotlin-mode-hook #'lsp-intellij-enable))))

(defun lsp-intellij/post-init-lsp-intellij ()
  (add-hook 'java-mode-hook #'lsp-intellij-enable)
  (add-hook 'kotlin-mode-hook #'lsp-intellij-enable))

(defun lsp-intellij/post-init-company ()
  (spacemacs|add-company-hook java-mode)
  (spacemacs|add-company-hook kotlin-mode)
  (push 'company-lsp company-backends-java-mode)
  (push 'company-lsp company-backends-kotlin-mode))

(defun lsp-intellij/post-init-flycheck ()
  (add-hook 'java-mode-hook 'flycheck-mode)
  (add-hook 'kotlin-mode-hook 'flycheck-mode))

(defun lsp-intellij/post-init-lsp-ui ()
  (add-hook 'lsp-after-open-hook 'lsp-ui-mode))

;;; packages.el ends here
