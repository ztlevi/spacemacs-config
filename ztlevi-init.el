;; =============================================================================
;; ztlevi's init settings here
;; =============================================================================

;; set perspective auto save
(setq persp-auto-save-persps-to-their-file nil)
(setq persp-auto-save-num-of-backups 0)
(setq persp-auto-resume-time 0)

;; set default open scratch
(when (string= "*scratch*" (buffer-name))
  (spacemacs/switch-to-scratch-buffer))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" " ztlevi - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; add dash
(autoload 'dash-at-point "dash-at-point"
  "search the word at point with dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)

;; set scrolling speed
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; set evil surround
(evil-define-key 'visual evil-surround-mode-map "Cs" 'evil-surround-change)
(evil-define-key 'visual evil-surround-mode-map "Ds" 'evil-surround-delete)

;; get safari url
(defun insert-safari-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (insert (retrieve-safari-current-tab-url)))

(defun retrieve-safari-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (let ((result (do-applescript
                 (concat
                  "set frontmostApplication to path to frontmost application\n"
                  "tell application \"Safari\"\n"
                  "	set theUrl to get URL of active tab of first window\n"
                  "	set theResult to (get theUrl) \n"
                  "end tell\n"
                  "activate application (frontmostApplication as text)\n"
                  "set links to {}\n"
                  "copy theResult to the end of links\n"
                  "return links as string\n"))))
    (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))

;; set evil state cursor
(setq evil-normal-state-cursor '("#ff007f" box))
(setq evil-insert-state-cursor '("#ff007f" (bar . 2)))

;; ranger mode fix
(require 'bookmark)
(ranger-override-dired-mode t)

;; set initl screen size
(setq initial-frame-alist
      '(
        (width . 83) ; character
        (height . 45) ; lines
        ))

;; set others
(setq zilongshanren-programming/post-init-js-doc nil)
(setq tab-always-indent 'complete)
