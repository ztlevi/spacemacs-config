;; =============================================================================
;; ztlevi's init settings here
;; =============================================================================

;; ==============================UI settings start==============================

;; settings for transparent
(spacemacs/toggle-transparency)

;; set initl screen size
(setq initial-frame-alist
      '(
        (width . 86) ; character
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
(setq-default fci-rule-column 81)
(setq fci-rule-color "grey80")
(add-hook 'prog-mode-hook 'fci-mode)

;; set all-the-icons
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; SPC t l // toggle-truncate-lines
(add-hook 'org-mode-hook 'toggle-truncate-lines)

;; ==============================UI settings end================================

;; set perspective auto save
(setq persp-auto-save-persps-to-their-file nil)
(setq persp-auto-save-num-of-backups 0)
(setq persp-auto-resume-time 0)

;; add dash
(autoload 'dash-at-point "dash-at-point"
  "search the word at point with dash." t nil)

;; ranger mode fix
(require 'bookmark)
(ranger-override-dired-mode t)

;; set org notes dir
(setq-default
 org-agenda-dir "~/Developer/Org-notes"
 deft-dir "~/Developer/Org-notes"
 blog-admin-dir "~/Developer/Wordpress")
(setq org-blog-dir blog-admin-dir)

;; occur non ascii, used to check non-ascii in Wordpress
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (deactivate-mark)
  (occur "[^[:ascii:]]"))

;; ===================Wordpress Org2Blog setting start====================
(setq load-path (cons "~/.spacemacs.d/layers/org2blog/" load-path))
(require 'org2blog-autoloads)
(require 'auth-source) ;; or nothing if already in the load-path
(setq org2blog/wp-blog-alist
      '(("my-blog"
         :url "https://ztlevi.wordpress.com/xmlrpc.php"
         :username "ztlevi")))
(let (credentials)
  ;; only required if your auth file is not already in the list of auth-sources
  (add-to-list 'auth-sources "~/.netrc")
  (setq credentials (auth-source-user-and-password "wp-ztlevi"))
  (setq org2blog/wp-blog-alist
        `(("my-blog"
           :url "https://ztlevi.wordpress.com/xmlrpc.php"
           :username ,(car credentials)
           :password ,(cadr credentials)))))
;; ===================Wordpress Org2Blog setting end======================

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

;; flyspell
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; yasnippet fix tab
(eval-after-load 'prog-mode
  '(progn
     (define-key prog-mode-map (kbd "TAB")
       (lambda()
         (interactive)
         (let ((yas/fallback-behavior 'return-nil))
           (unless (yas/expand)
             (indent-for-tab-command)
             (if (looking-back "^\s*")
                 (back-to-indentation))))))))

;; Layout
(defun ztlevi/load-my-layout ()
  (interactive)
  (persp-load-state-from-file (concat persp-save-dir "ztlevi")))

(defun ztlevi/save-my-layout ()
  (interactive)
  (persp-save-state-to-file (concat persp-save-dir "ztlevi")))

;; remove yas-installed-snippets-dir from yas-snippet-dirs
(with-eval-after-load 'yasnippet
  (setq yas-snippet-dirs (remq 'yas-installed-snippets-dir yas-snippet-dirs)))

;; define replace-dos-eol
(defun ztlevi/replace-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "
")))

;; set other
(setq zilongshanren-programming/post-init-js-doc nil)
(setq tab-always-indent 'complete)
