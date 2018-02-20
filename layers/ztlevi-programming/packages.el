;;; packages.el --- ztlevi layer packages file for Spacemacs.
;;
;; Copyright (c) 2016-2017 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ztlevi-programming-packages
  '(
    prettier-js
    (stylus-mode :location (recipe :fetcher github :repo "vladh/stylus-mode"))
    ;; rjsx-mode
    xref-js2
    css-mode
    lispy
    ;; racket-mode
    cmake-font-lock
    cmake-mode
    flycheck
    ;; lsp-javascript-typescript
    js2-mode
    eacl
    js2-refactor
    json-mode
    web-mode
    ;; web-beautify
    js-doc
    ;; lua-mode
    (cc-mode :location built-in)
    ;; flycheck-clojure
    counsel-etags
    (python :location built-in)
    ;; (pipenv :location (recipe :fetcher github :repo "pwalsh/pipenv.el"))
    (emacs-lisp :location built-in)
    ;; clojure-mode
    company
    (eldoc :location built-in)
    dumb-jump
    graphviz-dot-mode
    ;; cider
    robe
    )
  )

;; configuration scheme
;; https://prettier.io/docs/en/configuration.html#configuration-schema
(defun ztlevi-programming/init-prettier-js ()
  (use-package prettier-js
    :defer t
    :commands (prettier-js)
    :config
    (setq prettier-js-show-errors (quote echo))))

(defun ztlevi-programming/init-stylus-mode ()
  (use-package stylus-mode
    :defer t))

(defun ztlevi-programming/post-init-react-mode ()
  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

  (setq js2-basic-offset 2)
  (setq css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq react-indent-level 2))

(defun ztlevi-programming/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :config
    (setq js2-basic-offset 2)
    (setq css-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)

    (with-eval-after-load 'rjsx-mode
      (define-key rjsx-mode-map (kbd "C-d") nil))
    ))
;; (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

(defun ztlevi-programming/post-init-robe ()
  (progn
    (add-hook 'inf-ruby-mode-hook 'spacemacs/toggle-auto-completion-on)
    (defun ztlevi/ruby-send-current-line (&optional print)
      "Send the current line to the inferior Ruby process."
      (interactive "P")
      (ruby-send-region
       (line-beginning-position)
       (line-end-position))
      (when print (ruby-print-result)))

    (defun ztlevi/ruby-send-current-line-and-go ()
      (interactive)
      (ztlevi/ruby-send-current-line)
      (ruby-switch-to-inf t))

    (defun ztlevi/start-inf-ruby-and-robe ()
      (interactive)
      (when (not (get-buffer "*ruby*"))
        (inf-ruby))
      (robe-start))

    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "sb" 'ruby-send-block
        "sB" 'ruby-send-buffer
        "sl" 'ztlevi/ruby-send-current-line
        "sL" 'ztlevi/ruby-send-current-line-and-go
        "sI" 'ztlevi/start-inf-ruby-and-robe))))

(defun ztlevi-programming/post-init-cider ()
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

  (defun ztlevi/cider-figwheel-repl ()
    (interactive)
    (save-some-buffers)
    (with-current-buffer (cider-current-repl-buffer)
      (goto-char (point-max))
      (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
      (cider-repl-return)))

  (global-set-key (kbd "C-c C-f") #'ztlevi/cider-figwheel-repl))

(defun ztlevi-programming/post-init-graphviz-dot-mode ()
  (with-eval-after-load 'graphviz-dot-mode
    (require 'company-keywords)
    (push '(graphviz-dot-mode  "digraph" "node" "shape" "subgraph" "label" "edge" "bgcolor" "style" "record") company-keywords-alist)))

(defun ztlevi-programming/post-init-dumb-jump ()
  (setq dumb-jump-selector 'ivy)
  (defun my-dumb-jump ()
    (interactive)
    (evil-set-jump)
    (dumb-jump-go))
  (global-set-key (kbd "C-s-g") 'my-dumb-jump))

(defun ztlevi-programming/post-init-clojure-mode ()
  )

(defun ztlevi-programming/post-init-emacs-lisp ()
  (remove-hook 'emacs-lisp-mode-hook 'auto-compile-mode))

(defun ztlevi-programming/post-init-python ()
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  ;; if you use pyton2, then you could comment the following 3 lines
  ;; (setq python-shell-interpreter "python2")
  ;; (setq python-shell-interpreter-args "-i")
  ;; (setq flycheck-python-pylint-executable "/usr/local/bin/pylint")
  )

;; (defun ztlevi-programming/init-pipenv ()
;;   (use-package pipenv
;;     :after python))
;; (add-hook 'projectile-after-switch-project-hook #'pipenv-activate)

(defun ztlevi-programming/post-init-js-doc ()
  (setq js-doc-mail-address "zhouting@umich.edu"
        js-doc-author (format "ztlevi Qu <%s>" js-doc-mail-address)
        js-doc-url "http://www.ztlevi.com"
        js-doc-license "MIT"))

(defun ztlevi-programming/init-ctags-update ()
  (use-package ctags-update
    :init
    :defer t
    :config
    (spacemacs|hide-lighter ctags-auto-update-mode)))

(defun ztlevi-programming/post-init-web-mode ()
  (with-eval-after-load 'web-mode
    ;; for react mode html indentation
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))

    (web-mode-toggle-current-element-highlight)
    (web-mode-dom-errors-show)

    ;; live server
    (spacemacs/set-leader-keys-for-major-mode 'web-mode "l" 'live-server-preview)))

(defun ztlevi-programming/post-init-racket-mode ()
  (progn
    (eval-after-load 'racket-repl-mode
      '(progn
         (define-key racket-repl-mode-map (kbd "]") nil)
         (define-key racket-repl-mode-map (kbd "[") nil)))

    (add-hook 'racket-mode-hook #'(lambda () (lispy-mode 1)))
    (add-hook 'racket-repl-mode-hook #'(lambda () (lispy-mode t)))
    ;; (add-hook 'racket-repl-mode-hook #'(lambda () (smartparens-mode t)))
    ))

(defun ztlevi-programming/post-init-json-mode ()
  ;; set indent for json mode
  (setq js-indent-level 2)
  ;; set indent for json-reformat-region
  (setq json-reformat:indent-width 2)
  (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.fire\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.fire.meta\\'" . json-mode)))

;; (defun ztlevi-programming/post-init-web-beautify ()
;;   (spacemacs/set-leader-keys-for-major-mode 'json-mode
;;     "=" 'json-reformat-region-or-buffer))

(defun ztlevi-programming/init-lispy ()
  (use-package lispy
    :defer t
    :init
    (spacemacs/add-to-hooks (lambda () (lispy-mode)) '(emacs-lisp-mode-hook
                                                       ielm-mode-hook
                                                       inferior-emacs-lisp-mode-hook
                                                       clojure-mode-hook
                                                       scheme-mode-hook
                                                       cider-repl-mode-hook))
    :config
    (progn
      (define-key lispy-mode-map (kbd "C-a") 'mwim-beginning-of-code-or-line)

      (push '(cider-repl-mode . ("[`'~@]+" "#" "#\\?@?")) lispy-parens-preceding-syntax-alist)

      (spacemacs|hide-lighter lispy-mode)
      (define-key lispy-mode-map (kbd "s-j") 'lispy-splice)
      (define-key lispy-mode-map (kbd "s-k") 'paredit-splice-sexp-killing-backward)

      (with-eval-after-load 'cider-repl
        (define-key cider-repl-mode-map (kbd "C-s-j") 'cider-repl-newline-and-indent))

      (add-hook
       'minibuffer-setup-hook
       'conditionally-enable-lispy))))

(defun ztlevi-programming/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))

(defun ztlevi-programming/init-google-c-style ()
  (use-package google-c-style
    :init (add-hook 'c-mode-common-hook 'google-set-c-style)))

(defun ztlevi-programming/post-init-cmake-mode ()
  (progn
    (spacemacs/declare-prefix-for-mode 'cmake-mode
      "mh" "docs")
    (spacemacs/set-leader-keys-for-major-mode 'cmake-mode
      "hd" 'cmake-help)
    (add-hook 'cmake-mode-hook (function cmake-rename-buffer))))

(defun ztlevi-programming/post-init-flycheck ()
  (progn
    ;; disable jshint since we prefer eslint checking
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))

    ;; customize flycheck temp file prefix
    (setq-default flycheck-temp-prefix ".flycheck")

    ;; disable json-jsonlist checking for json files
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(json-jsonlist)))))

(defun ztlevi-programming/post-init-eldoc ()
  (setq eldoc-idle-delay 0.4))

(defun ztlevi-programming/post-init-js2-refactor ()
  (progn
    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "r>" 'js2r-forward-slurp
      "r<" 'js2r-forward-barf)))

(defun ztlevi-programming/init-eacl ()
  (use-package eacl
    :defer t
    :config
    ;; only for mac
    (setq eacl-grep-program "ggrep")))

(defun ztlevi-programming/init-lsp-javascript-typescript ()
  (use-package lsp-javascript-typescript
    :commands (lsp-javascript-typescript-enable)
    :defer t))

;; (add-hook 'js2-mode-hook #'lsp-javascript-typescript-enable)
;; (add-hook 'react-mode-hook #'lsp-javascript-typescript-enable)
;; (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable)

(defun ztlevi-programming/post-init-js2-mode ()
  (spacemacs|define-jump-handlers js2-mode)
  (add-hook 'spacemacs-jump-handlers-js2-mode 'etags-select-find-tag-at-point)

  ;; add your own keywords highlight here
  (font-lock-add-keywords 'js2-mode
                          '(("\\<\\(cc\\)\\>" 1 font-lock-type-face)))

  (spacemacs/declare-prefix-for-mode 'js2-mode "ms" "repl")

  ;; js default variables
  ;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-javascript.el
  (setq-default js2-strict-inconsistent-return-warning nil ; return <=> return null
                js2-skip-preprocessor-directives t
                js2-bounce-indent-p t
                ;; Let flycheck handle parse errors
                js2-strict-trailing-comma-warning nil
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil
                js2-highlight-external-variables t)

  (evilified-state-evilify js2-error-buffer-mode js2-error-buffer-mode-map))

(defun ztlevi-programming/post-init-css-mode ()
  (progn
    (dolist (hook '(css-mode-hook sass-mode-hook less-mode-hook))
      (add-hook hook 'rainbow-mode))

    (defun css-imenu-make-index ()
      (save-excursion
        (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

    (add-hook 'css-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'css-imenu-make-index)))))

(defun ztlevi-programming/post-init-lua-mode ()
  (progn
    (add-hook 'lua-mode-hook 'evil-matchit-mode)
    ;; (add-hook 'lua-mode-hook 'smartparens-mode)
    (setq lua-indent-level 2)

    ;; add lua language, basic, string and table keywords.
    ;; (with-eval-after-load 'lua-mode
    ;;   (require 'company-keywords)
    ;;   (push '(lua-mode  "setmetatable" "local" "function" "and" "break" "do" "else" "elseif" "self" "resume" "yield"
    ;;                     "end" "false" "for" "function" "goto" "if" "nil" "not" "or" "repeat" "return" "then" "true"
    ;;                     "until" "while" "__index" "dofile" "getmetatable" "ipairs" "pairs" "print" "rawget" "status"
    ;;                     "rawset" "select" "_G" "assert" "collectgarbage" "error" "pcall" "coroutine"
    ;;                     "rawequal" "require" "load" "tostring" "tonumber" "xpcall" "gmatch" "gsub"
    ;;                     "rep" "reverse" "sub" "upper" "concat" "pack" "insert" "remove" "unpack" "sort"
    ;;                     "lower") company-keywords-alist))

    ))

(defun ztlevi-programming/post-init-cc-mode ()
  ;; http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
  (defadvice c-lineup-arglist (around my activate)
    "Improve indentation of continued C++11 lambda function opened as argument."
    (setq ad-return-value
          (if (and (equal major-mode 'c++-mode)
                   (ignore-errors
                     (save-excursion
                       (goto-char (c-langelem-pos langelem))
                       ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                       ;;   and with unclosed brace.
                       (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
              0                       ; no additional indent
            ad-do-it)))               ; default behavior

  (setq c-default-style "linux") ;; set style to "linux"
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(defun ztlevi-programming/init-flycheck-clojure ()
  (use-package flycheck-clojure
    :defer t
    :init
    (eval-after-load 'flycheck '(flycheck-clojure-setup))))

(defun ztlevi-programming/init-counsel-etags ()
  (use-package counsel-etags
    :defer t
    :config
    ;; Don't ask before rereading the TAGS files if they have changed
    (setq tags-revert-without-query t)
    ;; Don't warn when TAGS files are large
    (setq large-file-warning-threshold nil)
    ;; Setup auto update now
    (add-hook 'prog-mode-hook
              (lambda ()
                (add-hook 'after-save-hook
                          'counsel-etags-virtual-update-tags 'append 'local)))))

(defun ztlevi-programming/post-init-company ()
  (progn
    ;; set the company minimum prefix length and idle delay
    (defvar ztlevi/company-minimum-prefix-length 1
      "my own variable for company-minimum-prefix-length")
    (add-hook 'company-mode-hook #'ztlevi/company-init)

    (when (configuration-layer/package-usedp 'company)
      (spacemacs|add-company-backends :modes shell-script-mode makefile-bsdmake-mode sh-mode lua-mode nxml-mode conf-unix-mode json-mode graphviz-dot-mode))

    ;; define company-mode keybindings
    (with-eval-after-load 'company
      (progn
        (bb/define-key company-active-map (kbd "C-f") nil)
        (bb/define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
        (bb/define-key company-active-map (kbd "C-j") 'company-show-location)))
    ))

(defun ztlevi-programming/post-init-company-c-headers ()
  (progn
    (setq company-c-headers-path-system
          (quote
           ("/usr/include/" "/usr/local/include/" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")))
    (setq company-c-headers-path-user
          (quote
           ("/Users/guanghui/cocos2d-x/cocos/platform" "/Users/guanghui/cocos2d-x/cocos" "." "/Users/guanghui/cocos2d-x/cocos/audio/include/")))))

(defun ztlevi-programming/init-xref-js2 ()
  (use-package xref-js2
    :defer t))
