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

;; SPC t l // toggle-truncate-lines
(add-hook 'text-mode-hook 'toggle-truncate-lines)

;; ==============================UI settings end================================

;; set perspective auto save
(setq persp-auto-save-persps-to-their-file nil)
(setq persp-auto-save-num-of-backups 0)
(setq persp-auto-resume-time 0)

;; ranger mode fix
(use-package bookmark)
(ranger-override-dired-mode t)

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
