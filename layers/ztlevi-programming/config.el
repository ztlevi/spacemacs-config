;;; config.el --- ztlevi layer packages file for Spacemacs.
;;
;; Copyright (c) 2016-2017 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Flyckeck disable and enable
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))

;; add to mode alist
(add-to-list 'auto-mode-alist '("Capstanfile\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.xtpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode))

(add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt '("xml" "xsd" "rng" "xslt" "xsl") t) "\\'") 'nxml-mode))
(setq nxml-slash-auto-complete-flag t)

;; prettier js
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'react-mode-hook 'prettier-js-mode)
(add-hook 'json-mode-hook 'prettier-js-mode)
(add-hook 'css-mode-hook 'prettier-js-mode)
(add-hook 'markdown-mode-hook 'prettier-js-mode)
(add-hook 'gfm-mode-hook 'prettier-js-mode)

;; only enable prettier for js and jsx if in web-mode
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))))

(spacemacs|add-toggle iimage
  :status iimage-mode
  :on (iimage-mode)
  :off (iimage-mode -1)
  :documentation "Enable iimage mode"
  :evil-leader "oti")

(add-hook 'term-mode-hook 'ztlevi/ash-term-hooks)

;; js2 mode hooks
;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook
          (lambda ()
            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;; turn on react mode recursively in some directories
;; this hook needs to be added before others to take effect
(defun turn-on-react-mode-for-js2 ()
  (interactive)
  (cond ((string-match "/Developer/react_github/" buffer-file-name)
         (react-mode))))
(add-hook 'js2-mode-hook 'turn-on-react-mode-for-js2)

;; ============= Use textlint ============
;; npm i -g textlint textlint-rule-spellchecker textlint-rule-common-misspellings
;; (flycheck-define-checker textlint
;;   "A linter for prose."
;;   :command ("textlint" "--format" "unix" "--rule" "textlint-rule-spellchecker" "--rule" "common-misspellings" source-inplace)
;;   :error-patterns
;;   ((warning line-start (file-name) ":" line ":" column ": "
;;             (id (one-or-more (not (any " "))))
;;             (message (one-or-more not-newline)
;;                      (zero-or-more "\n" (any " ") (one-or-more not-newline)))
;;             line-end))
;;   :modes (text-mode markdown-mode gfm-mode))

;; (add-to-list 'flycheck-checkers 'textlint)

;; (add-hook 'markdown-mode-hook 'flycheck-mode)
;; (add-hook 'gfm-mode-hook 'flycheck-mode)

;; return nil to write content to file
(defun ztlevi/untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max)) nil))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (add-hook 'write-contents-hooks
                        'ztlevi/untabify-buffer nil t)))

(setq auto-mode-alist
      (append
       '(("\\.mak\\'" . makefile-bsdmake-mode))
       auto-mode-alist))

(defmacro ztlevi|toggle-company-backends (backend)
  "Push or delete the backend to company-backends"
  (let ((funsymbol (intern (format "ztlevi/company-toggle-%S" backend))))
    `(defun ,funsymbol ()
       (interactive)
       (if (eq (car company-backends) ',backend)
           (setq-local company-backends (delete ',backend company-backends))
         (push ',backend company-backends)))))
