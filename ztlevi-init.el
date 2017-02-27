;; =============================================================================
;; ztlevi's init settings here
;; =============================================================================

;; ==============================UI settings start==============================
;; set initl screen size
(setq initial-frame-alist
      '(
        (width . 85) ; character
        (height . 45) ; lines
        ))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" " ztlevi - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; set scrolling speed
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; set evil state cursor
(setq evil-normal-state-cursor '("#ff007f" box))
(setq evil-insert-state-cursor '("#ff007f" (bar . 2)))

;; File colum indicator 80 chars
(setq-default fci-rule-column 80)
(setq fci-rule-color "grey80")
(add-hook 'prog-mode-hook 'fci-mode)

;; set all-the-icons
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; ==============================UI settings end================================

;; set perspective auto save
(setq persp-auto-save-persps-to-their-file nil)
(setq persp-auto-save-num-of-backups 0)
(setq persp-auto-resume-time 0)

;; set default open scratch
(when (string= "*scratch*" (buffer-name))
  (spacemacs/switch-to-scratch-buffer))

;; add dash
(autoload 'dash-at-point "dash-at-point"
  "search the word at point with dash." t nil)

;; ranger mode fix
(require 'bookmark)
(ranger-override-dired-mode t)

;; set org notes dir
(setq-default
 org-agenda-dir "~/Documents/Org-notes"
 deft-dir "~/Documents/Org-notes"
 blog-admin-dir "")

;; ===================flycheck settings start====================
;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

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

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; c++
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

;; Enable for other modes
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'web-mode-hook 'flycheck-mode)
(add-hook 'json-mode-hook 'flycheck-mode)
;; ===================flycheck settings end======================

;; set others
(setq zilongshanren-programming/post-init-js-doc nil)
(setq tab-always-indent 'complete)
