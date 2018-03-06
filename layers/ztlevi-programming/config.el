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
(spacemacs/add-to-hooks (lambda () (flycheck-mode -1))
                        '(emacs-lisp-mode-hook
                          text-mode-hook))

;; js2 mode hook
(defun my-js2-mode-hook ()
  (progn
    (setq imenu-create-index-function 'js2-imenu-make-index)

    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)

    (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc-snippet)
    (define-key js2-mode-map "@" 'js-doc-insert-tag)
    (modify-syntax-entry ?_ "w")

    (define-key js2-mode-map   (kbd "s-.") 'company-tern)
    (spacemacs/toggle-syntax-checking-on)
    (setq forward-sexp-function nil)
    (set (make-local-variable 'semantic-mode) nil)))
(add-hook 'js2-mode-hook 'my-js2-mode-hook)
;; (add-hook 'js2-mode-hook (lambda () (react-mode)))

;; turn on react mode recursively in some directories
;; this hook needs to be added before others to take effect
(defun turn-on-react-mode-for-js2 ()
  (interactive)
  (if (or
       (my-project-name-contains-substring "react_github")
       (my-project-name-contains-substring "learn-redux")
       (my-project-name-contains-substring "cryptocurrency_exchange_app/frontend")
       )
      (react-mode)))
(add-hook 'js2-mode-hook 'turn-on-react-mode-for-js2)

;; c++ hook
(add-hook 'c++-mode-hook
          (lambda ()
              (add-hook 'write-contents-hooks
                        'ztlevi/untabify-buffer nil t)))

;; java hook
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

;; term hook
(add-hook 'term-mode-hook 'ztlevi/ash-term-hooks)

;; css mode
(add-hook 'css-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'css-imenu-make-index)))

(dolist (hook '(css-mode-hook sass-mode-hook less-mode-hook))
  (add-hook hook 'rainbow-mode))

;; prettier js
(spacemacs/add-to-hooks 'prettier-js-mode '(js2-mode-hook
                                            typescript-mode-hook
                                            react-mode-hook
                                            json-mode-hook
                                            css-mode-hook
                                            markdown-mode-hook
                                            gfm-mode-hook))

;; only enable prettier for js and jsx if in web-mode
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))))
;; spacemacs disables smartparens in web mode
;; (add-hook 'web-mode-hook 'spacemacs/toggle-smartparens-on)

(spacemacs|add-toggle iimage
  :status iimage-mode
  :on (iimage-mode)
  :off (iimage-mode -1)
  :documentation "Enable iimage mode"
  :evil-leader "oti")

;; grep ignore files
(eval-after-load 'grep
  '(progn
     (dolist (v '("auto"
                  "target"
                  "node_modules"
                  "bower_components"
                  "*dist"
                  ".sass_cache"
                  ".cache"
                  ".npm"
                  "elpa"))
       (add-to-list 'grep-find-ignored-directories v))

     (dolist (v '("*.min.js"
                  "*.map"
                  "*.bundle.js"
                  "*.min.css"
                  "tags"
                  "TAGS"
                  "GTAGS"
                  "GRTAGS"
                  "GPATH"
                  "cscope.files"
                  "*.json"
                  "*.log"))
       (add-to-list 'grep-find-ignored-files v))))

;; =============== Use textlint ===============
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

;; add to mode alist
(dolist (m '(("Capstanfile\\'" . yaml-mode)
             ("\\.mm\\'" . objc-mode)
             ("\\.c\\'" . c++-mode)
             ("\\.zsh\\'" . shell-script-mode)
             ("\\.xtpl\\'" . web-mode)
             ("\\.vue\\'" . web-mode)
             ("\\.mak\\'" . makefile-bsdmake-mode)
             ("\\.blade.php\\'" . web-mode)))
  (add-to-list 'auto-mode-alist m))

(add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt '("xml" "xsd" "rng" "xslt" "xsl") t) "\\'") 'nxml-mode))
(setq nxml-slash-auto-complete-flag t)

;; variables are considered undeclared for purposes of highlighting
(setq-default js2-additional-externs
              '("$"
                "$A" ; salesforce lightning component
                "$LightningApp" ; salesforce
                "AccessifyHTML5"
                "Blob"
                "FormData"
                "KeyEvent"
                "Raphael"
                "React"
                "URLSearchParams"
                "__dirname" ; Node
                "_content" ; Keysnail
                "after"
                "afterEach"
                "angular"
                "app"
                "assert"
                "assign"
                "before"
                "beforeEach"
                "browser"
                "by"
                "clearInterval"
                "clearTimeout"
                "command" ; Keysnail
                "content" ; Keysnail
                "define"
                "describe"
                "documentRef"
                "global"
                "display" ; Keysnail
                "element"
                "expect"
                "ext" ; Keysnail
                "fetch"
                "gBrowser" ; Keysnail
                "goDoCommand" ; Keysnail
                "hook" ; Keysnail
                "inject"
                "isDev"
                "it"
                "jQuery"
                "jasmine"
                "key" ; Keysnail
                "ko"
                "log"
                "module"
                "plugins" ; Keysnail
                "process"
                "require"
                "setInterval"
                "setTimeout"
                "shell" ; Keysnail
                "tileTabs" ; Firefox addon
                "util" ; Keysnail
                "utag"))
