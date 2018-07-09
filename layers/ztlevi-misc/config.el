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
(spacemacs/add-to-hooks #'flyspell-mode '(org-mode-hook
                                          markdown-mode-hook))

;; confirmation is requested before visiting a new file or buffer.
(setq confirm-nonexistent-file-or-buffer t)

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

                                            ;; signature
                                            ("8zt" "ztlevi")
                                            ;; emacs regex
                                            ("8d" "\\([0-9]+?\\)")
                                            ("8str" "\\([^\"]+?\\)\"")))


(setq user-mail-address "zhouting@umich.com")

;; ================== Chenbin's flyspell setting for camel case ==================
;; http://blog.binchen.org/posts/how-to-spell-check-functionvariable-in-emacs.html

(defun split-camel-case (word)
  "Split camel case WORD into a list of strings.
Ported from 'https://github.com/fatih/camelcase/blob/master/camelcase.go'."
  (let* ((case-fold-search nil)
         (len (length word))
         ;; ten sub-words is enough
         (runes [nil nil nil nil nil nil nil nil nil nil])
         (runes-length 0)
         (i 0)
         ch
         (last-class 0)
         (class 0)
         rlt)

    ;; split into fields based on class of character
    (while (< i len)
      (setq ch (elt word i))
      (cond
       ;; lower case
       ((and (>= ch ?a) (<= ch ?z))
        (setq class 1))
       ;; upper case
       ((and (>= ch ?A) (<= ch ?Z))
        (setq class 2))
       ((and (>= ch ?0) (<= ch ?9))
        (setq class 3))
       (t
        (setq class 4)))

      (cond
       ((= class last-class)
        (aset runes
              (1- runes-length)
              (concat (aref runes (1- runes-length)) (char-to-string ch))))
       (t
        (aset runes runes-length (char-to-string ch))
        (setq runes-length (1+ runes-length))))
      (setq last-class class)
      ;; end of while
      (setq i (1+ i)))

    ;; handle upper case -> lower case sequences, e.g.
    ;;     "PDFL", "oader" -> "PDF", "Loader"
    (setq i 0)
    (while (< i (1- runes-length))
      (let* ((ch-first (aref (aref runes i) 0))
             (ch-second (aref (aref runes (1+ i)) 0)))
        (when (and (and (>= ch-first ?A) (<= ch-first ?Z))
                   (and (>= ch-second ?a) (<= ch-second ?z)))
          (aset runes (1+ i) (concat (substring (aref runes i) -1) (aref runes (1+ i))))
          (aset runes i (substring (aref runes i) 0 -1))))
      (setq i (1+ i)))

    ;; construct final result
    (setq i 0)
    (while (< i runes-length)
      (when (> (length (aref runes i)) 0)
        (setq rlt (add-to-list 'rlt (aref runes i) t)))
      (setq i (1+ i)))
    rlt))

(defun flyspell-detect-ispell-args (&optional run-together)
  "If RUN-TOGETHER is true, spell check the CamelCase words.
Please note RUN-TOGETHER will make aspell less capable. So it should only be used in prog-mode-hook."
  ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
  (let* ((args (list "--sug-mode=ultra" "--lang=en_US"))args)
    (if run-together
        (setq args (append args '("--run-together" "--run-together-limit=16"))))
    args))

;; {{ for aspell only, hunspell does not need setup `ispell-extra-args'
(setq ispell-program-name "aspell")
(setq-default ispell-extra-args (flyspell-detect-ispell-args t))
;; }}

;; ;; {{ hunspell setup, please note we use dictionary "en_US" here
;; (setq ispell-program-name "hunspell")
;; (setq ispell-local-dictionary "en_US")
;; (setq ispell-local-dictionary-alist
;;       '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
;; ;; }}

(defvar extra-flyspell-predicate '(lambda (word) t)
  "A callback to check WORD.  Return t if WORD is typo.")

(defun my-flyspell-predicate (word)
  "Use aspell to check WORD.  If it's typo return t."
  (let* ((cmd (cond
               ;; aspell: `echo "helle world" | aspell pipe`
               ((string-match-p "aspell$" ispell-program-name)
                (format "echo \"%s\" | %s pipe"
                        word
                        ispell-program-name))
               ;; hunspell: `echo "helle world" | hunspell -a -d en_US`
               (t
                (format "echo \"%s\" | %s -a -d en_US"
                        word
                        ispell-program-name))))
         (cmd-output (shell-command-to-string cmd))
         rlt)
    ;; (message "word=%s cmd=%s" word cmd)
    ;; (message "cmd-output=%s" cmd-output)
    (cond
     ((string-match-p "^&" cmd-output)
      ;; it's a typo because at least one sub-word is typo
      (setq rlt t))
     (t
      ;; not a typo
      (setq rlt nil)))
    rlt))

(defun js-flyspell-verify ()
  (let* ((case-fold-search nil)
         (font-matched (memq (get-text-property (- (point) 1) 'face)
                             '(js2-function-call
                               js2-function-param
                               js2-object-property
                               js2-object-property-access
                               font-lock-variable-name-face
                               font-lock-string-face
                               font-lock-function-name-face
                               font-lock-builtin-face
                               rjsx-text
                               rjsx-tag
                               rjsx-attr)))
         subwords
         word
         (rlt t))
    (cond
     ((not font-matched)
      (setq rlt nil))
     ;; ignore two character word
     ((< (length (setq word (thing-at-point 'word))) 2)
      (setq rlt nil))
     ;; handle camel case word
     ((and (setq subwords (split-camel-case word)) (> (length subwords) 1))
      (let* ((s (mapconcat (lambda (w)
                             (cond
                              ;; sub-word wholse length is less than three
                              ((< (length w) 3)
                               "")
                              ;; special characters
                              ((not (string-match-p "^[a-zA-Z]*$" w))
                               "")
                              (t
                               w))) subwords " ")))
        (setq rlt (my-flyspell-predicate s))))
     (t
      (setq rlt (funcall extra-flyspell-predicate word))))
    rlt))

(put 'js2-mode 'flyspell-mode-predicate 'js-flyspell-verify)
(put 'rjsx-mode 'flyspell-mode-predicate 'js-flyspell-verify)

;; ========================================================================
