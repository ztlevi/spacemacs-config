;;; packages.el --- ztlevi-ui layer packages file for Spacemacs.
;;
;; Copyright (c) 2016-2017 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/Spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ztlevi-programming-packages
  '(
    flycheck
    yasnippet
    )
  )

(defun ztlevi-programming/post-init-yasnippet ()
  ;; remove yas-installed-snippets-dir from yas-snippet-dirs
  (with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs (remq 'yas-installed-snippets-dir yas-snippet-dirs)))
  )

(defun ztlevi-programming/post-init-flycheck ()
  (progn
    ;; use web-mode for .jsx files
    (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

    ;; disable jshint since we prefer eslint checking
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))

    ;; use eslint with web-mode for jsx files
    (flycheck-add-mode 'javascript-eslint 'web-mode)

    ;; customize flycheck temp file prefix
    (setq-default flycheck-temp-prefix ".flycheck")

    ;; disable json-jsonlist checking for json files
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(json-jsonlist)))

    ;; add c++ flycheck standard
    (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

    ;; for better jsx syntax-highlighting in web-mode
    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (if (equal web-mode-content-type "jsx")
          (let ((web-mode-enable-part-face nil))
            ad-do-it)
        ad-do-it))
    )
  )
