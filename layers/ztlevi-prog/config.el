;;; config.el --- ztlevi layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2016-2018 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Flyckeck disable and enable
(defun disable-flycheck-mode ()
  (flycheck-mode -1))
(spacemacs/add-to-hooks 'disable-flycheck-mode
                        '(emacs-lisp-mode-hook
                          text-mode-hook))

;; ===================== tide start =====================
(setq company-tooltip-align-annotations t)

(defun setup-tide-mode ()
  (tide-setup)
  (unless (tide-current-server)
    (tide-restart-server))

  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)

  (define-key tide-mode-map [remap xref-find-definitions] #'tide-jump-to-definition)
  (define-key tide-mode-map [remap xref-find-references] #'tide-references))
(dolist (hook (list 'js2-mode-hook 'rjsx-mode-hook))
  (add-hook hook 'setup-tide-mode))

(defun spacemacs//javascript-setup-tide-company ()
  "Setup tide auto-completion."
  (spacemacs|add-company-backends
    :backends company-tide
    :modes js2-mode rjsx-mode
    :variables
    company-minimum-prefix-length 2)
  (company-mode))
(spacemacs/add-to-hooks #'spacemacs//javascript-setup-tide-company '(js2-mode-local-vars-hook rjsx-mode-local-vars-hook))

;; copy from doom, fix project root
(defun +javascript*tide-project-root ()
  "Resolve to `doom-project-root' if `tide-project-root' fails."
  (or tide-project-root
      (or (locate-dominating-file default-directory "tsconfig.json")
          (locate-dominating-file default-directory "jsconfig.json"))
      (projectile-project-root)))
(advice-add #'tide-project-root :override #'+javascript*tide-project-root)
;; ===================== tide end =====================

(defun my-lsp-mode-hook ()
  ;; disable lsp-highlight-symbol
  ;; (setq lsp-highlight-symbol-at-point nil)

  ;; toggle off lsp-ui-doc by default
  (toggle-lsp-ui-doc)

  ;; overwrite s-j key for toggle-lsp-ui-doc
  (global-set-key (kbd "s-j") #'toggle-lsp-ui-doc))
(add-hook 'lsp-mode-hook #'my-lsp-mode-hook)

;; js2 mode hook
(defun my-js2-mode-hook ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)

  (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc-snippet)
  (define-key js2-mode-map "@" 'js-doc-insert-tag)
  (modify-syntax-entry ?_ "w")

  (spacemacs/toggle-syntax-checking-on)
  (setq forward-sexp-function nil)
  (set (make-local-variable 'semantic-mode) nil))
(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;; web mode hook
(defun my-web-mode-hook ()
  ;; (add-hook 'before-save-hook 'web-beautify-html-buffer t t)
  )
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; c++ hook
(defun my-c++-mode-hook ()
  (add-hook 'write-contents-hooks
            'ztlevi/untabify-buffer nil t))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; java hook
(defun my-java-mode-hook ()
  ;; meghanada-mode on
  ;; use code format
  (add-hook 'before-save-hook #'lsp-format-buffer)
  (setq c-basic-offset 2))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; term hook
(defun my-term-mode-hook ()
  (ztlevi/ash-term-hooks)
  (spacemacs/toggle-truncate-lines-on))
(add-hook 'term-mode-hook 'my-term-mode-hook)

;; css mode
(defun my-css-mode-hook ()
  (setq imenu-create-index-function 'css-imenu-make-index))
(add-hook 'css-mode-hook 'my-css-mode-hook)

;; enable rainbow-mode
(spacemacs/add-to-hooks #'rainbow-mode '(css-mode-hook
                                         sass-mode-hook
                                         less-mode-hook))

;; nxml mode
(setq nxml-child-indent 4 nxml-attribute-indent 4)

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
