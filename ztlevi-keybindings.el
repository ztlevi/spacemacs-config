;; define the emacs move keys
(define-key evil-insert-state-map (kbd "C-d") 'evil-delete-char)
(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line) ;; was 'evil-paste-last-insertion
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)    ;; was 'evil-copy-from-below
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)      ;; was 'evil-complete-next
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)  ;; was 'evil-complete-previous
(setq-default evil-escape-key-sequence "jk")

;; define company-mode keybindings
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; bind help keybindings
(global-set-key (kbd "C-h C-f") `find-function)
(global-set-key (kbd "C-h C-v") `find-variable)
(global-set-key (kbd "C-h C-k") `find-function-on-key)

;; window split and focus
(define-key evil-normal-state-local-map (kbd "SPC w v") `split-window-right-and-focus)
(define-key evil-normal-state-local-map (kbd "SPC w s") `split-window-below-and-focus)
