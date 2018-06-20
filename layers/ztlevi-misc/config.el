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

;; flyspell
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; customize helm
(with-eval-after-load 'helm-make
  (progn
    ;; limit max number of matches displayed for speed
    (setq helm-candidate-number-limit 100)
    ;; ignore boring files like .o and .a
    (setq helm-ff-skip-boring-files t)
    ;; replace locate with spotlight on Mac
    (setq helm-locate-command "mdfind -name %s %s")
    (push "\\.emlx$" helm-boring-file-regexp-list)))

(define-abbrev-table 'global-abbrev-table '(

                                            ;; math/unicode symbols
                                            ("8in" "∈")
                                            ("8nin" "∉")
                                            ("8inf" "∞")
                                            ("8luv" "♥")
                                            ("8smly" "☺")
                                            ("8en" "@~english")
                                            ("8zh" "@~chinese")
                                            ("8sp" "spacemacs")
                                            ;; email
                                            ("8me" "zhouting@umich.com")

                                            ;; computing tech
                                            ("8wp" "Wikipedia")
                                            ("8ms" "Microsoft")
                                            ("8g" "Google")
                                            ("8it" "IntelliType")
                                            ("8msw" "Microsoft Windows")
                                            ("8win" "Windows")
                                            ("8ie" "Internet Explorer")
                                            ("8ahk" "AutoHotkey")
                                            ("82dx" "Cocos2D-X")

                                            ;; signature
                                            ("8zl" "ztlevi")
                                            ;; emacs regex
                                            ("8d" "\\([0-9]+?\\)")
                                            ("8str" "\\([^\"]+?\\)\"")))


(setq user-mail-address "zhouting@umich.com")

;; ================== Chenbin's flyspell setting for camel case ==================
;; http://blog.binchen.org/posts/how-to-spell-check-functionvariable-in-emacs.html

(defun flyspell-detect-ispell-args (&optional run-together)
  "If RUN-TOGETHER is true, spell check the CamelCase words.
Please note RUN-TOGETHER will make aspell less capable. So it should only be used in prog-mode-hook."
  ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
  (let* ((args (list "--sug-mode=ultra" "--lang=en_US"))args)
    (if run-together
        (setq args (append args '("--run-together" "--run-together-limit=16"))))
    args))

(setq ispell-program-name "aspell")
(setq-default ispell-extra-args (flyspell-detect-ispell-args t))

(defvar extra-flyspell-predicate '(lambda (word) t)
  "A callback to check WORD.  Return t if WORD is typo.")

(defun my-flyspell-predicate (word)
  "Use aspell to check WORD.  If it's typo return true."
  (if (string-match-p (concat "^& " word)
                      (shell-command-to-string (format "echo %s | %s %s pipe"
                                                       word
                                                       ispell-program-name
                                                       (mapconcat 'identity
                                                                  (flyspell-detect-ispell-args t)
                                                                  " "))))
      t))

(defmacro my-flyspell-predicate-factory (preffix)
  `(lambda (word)
     (let* ((pattern (concat "^\\(" ,preffix "\\)\\([A-Z]\\)"))
            rlt)
       (cond
        ((string-match-p pattern word)
         (setq word (replace-regexp-in-string pattern "\\2" word))
         (setq rlt (my-flyspell-predicate word)))
        (t
         (setq rlt t)))
       rlt)))

(defun js-flyspell-verify ()
  (let* ((case-fold-search nil)
         (font-matched (memq (get-text-property (- (point) 1) 'face)
                             '(js2-function-call
                               js2-function-param
                               js2-object-property
                               font-lock-variable-name-face
                               font-lock-string-face
                               font-lock-function-name-face
                               font-lock-builtin-face
                               rjsx-tag
                               rjsx-attr)))
         word
         (rlt t))
    (cond
     ((not font-matched)
      (setq rlt nil))
     ((not (string-match-p "aspell$" ispell-program-name))
      ;; Only override aspell's result
      (setq rlt t))
     ((string-match-p "^[a-zA-Z][a-zA-Z]$"
                      (setq word (thing-at-point 'word)))
      (setq rlt nil))
     ((string-match-p "\\([A-Z][a-z]\\|^[a-z][a-z]\\)[A-Z]\\|[a-z][A-Z][a-z]$"
                      word)
      ;; strip two character interior words
      (setq word (replace-regexp-in-string "\\([A-Z][a-z]\\|^[a-z][a-z]\\)\\([A-Z]\\)" "\\2" word))
      (setq word (replace-regexp-in-string "\\([a-z]\\)[A-Z][a-z]$" "\\1" word))
      ;; check stripped word
      (setq rlt (my-flyspell-predicate word)))
     (t
      (setq rlt (funcall extra-flyspell-predicate word))))
    rlt))
(put 'js2-mode 'flyspell-mode-predicate 'js-flyspell-verify)
(put 'rjsx-mode 'flyspell-mode-predicate 'js-flyspell-verify)

(defun prog-mode-hook-setup ()
  ;; remove namespace "MS" and "X"
  (setq-local extra-flyspell-predicate (my-flyspell-predicate-factory "MS\\|X")))
(add-hook 'prog-mode-hook 'prog-mode-hook-setup)

;; ========================================================================
