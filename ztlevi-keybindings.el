;; ================================Evil Mode Start===============================
;; define the emacs move keys in evil-mode
(define-key evil-insert-state-map (kbd "C-d") 'evil-delete-char)
;; (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line) ;; was 'evil-paste-last-insertion
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)    ;; was 'evil-copy-from-below
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)      ;; was 'evil-complete-next
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)  ;; was 'evil-complete-previous
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)

;; window split and focus
(define-key evil-normal-state-local-map (kbd "SPC w v") 'split-window-right-and-focus)
(define-key evil-normal-state-local-map (kbd "SPC w s") 'split-window-below-and-focus)

;; set evil surround
(evil-define-key 'visual evil-surround-mode-map "Cs" 'evil-surround-change)
(evil-define-key 'visual evil-surround-mode-map "Ds" 'evil-surround-delete)
;; ================================Evil Mode END=================================

;; define company-mode keybindings
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))
(define-key global-map (kbd "M-/") nil)
(define-key global-map (kbd "M-/") 'company-complete)

;; bind help keybindings
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; helm bookmark keybindings
(define-key spacemacs-default-map (kbd "h b") 'helm-bookmarks)
(with-eval-after-load 'helm-bookmark
  (progn
    (bb/define-key helm-bookmark-map (kbd "C-o") 'helm-bookmark-run-jump-other-window)
    (bb/define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete)
    (bb/define-key helm-bookmark-map (kbd "C-f") 'helm-bookmark-toggle-filename)
    (bb/define-key helm-bookmark-map (kbd "C-e") 'helm-bookmark-run-edit))
  )

;; comment
(global-set-key (kbd "s-/") 'comment-dwim)

;; occur
(bind-key* "M-s o" 'occur-dwim)

;; ranger
(with-eval-after-load 'ranger
  (progn
    (define-key ranger-normal-mode-map (kbd "q") 'ranger-close)
    (define-key evil-normal-state-local-map (kbd "SPC f j") 'deer)))

;; dash
(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)

;; yasnippet fix tab
(eval-after-load 'js2-mode
  '(progn
     (define-key js2-mode-map (kbd "TAB")
       (lambda()
         (interactive)
         (let ((yas/fallback-behavior 'return-nil))
           (unless (yas/expand)
             (indent-for-tab-command)
             (if (looking-back "^\s*")
                 (back-to-indentation))))))))
(eval-after-load 'css-mode
  '(progn
     (define-key css-mode-map (kbd "TAB")
       (lambda()
         (interactive)
         (let ((yas/fallback-behavior 'return-nil))
           (unless (yas/expand)
             (indent-for-tab-command)
             (if (looking-back "^\s*")
                 (back-to-indentation))))))))
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
(define-key evil-normal-state-local-map (kbd "SPC y r") 'yas-reload-all)
(define-key evil-normal-state-local-map (kbd "SPC y d") 'yas-describe-tables)

;; fix yasnippet with unbinding emmet keys
(eval-after-load 'emmet-mode
  '(progn
     (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") nil)
     (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") nil)
     (evil-define-key 'emacs emmet-mode-keymap (kbd "TAB") nil)
     (evil-define-key 'emacs emmet-mode-keymap (kbd "<tab>") nil)
     (evil-define-key 'hybrid emmet-mode-keymap (kbd "TAB") nil)
     (evil-define-key 'hybrid emmet-mode-keymap (kbd "<tab>") nil)))
     
;; layout keybindings
(spacemacs/set-leader-keys "oll" 'ztlevi/load-my-layout)
(spacemacs/set-leader-keys "ols" 'ztlevi/save-my-layout)
