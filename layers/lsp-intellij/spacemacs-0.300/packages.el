;;; packages.el --- lsp-intellij layer packages file for Spacemacs.
;;
;; Copyright (c) 2018 Richard Jones
;;
;; Author:  <richajn@amazon.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code:

(defconst lsp-intellij-packages
  '(company
    flycheck
    (lsp-intellij :location (recipe :fetcher github :repo "Ruin0x11/lsp-intellij"))))

(defun lsp-intellij/post-init-company ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs//lsp-intellij-setup-company))

(defun lsp-intellij/post-init-flycheck ()
  (add-hook 'java-mode-hook 'flycheck-mode)
  (add-hook 'kotlin-mode-hook 'flycheck-mode))

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

;;; packages.el ends here
