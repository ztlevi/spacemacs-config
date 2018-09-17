;;; keybindings.el --- ztlevi-ui layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2016-2018 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; window split and focus
(spacemacs/set-leader-keys "wv" 'split-window-right-and-focus)
(spacemacs/set-leader-keys "ws" 'split-window-below-and-focus)
(define-key evil-normal-state-map (kbd "C-w v") 'split-window-right-and-focus)
(define-key evil-normal-state-map (kbd "C-w s") 'split-window-below-and-focus)
(define-key evil-visual-state-map (kbd "C-w v") 'split-window-right-and-focus)
(define-key evil-visual-state-map (kbd "C-w s") 'split-window-below-and-focus)

;; evil hybrid
(define-key evil-hybrid-state-map (kbd "C-r") 'evil-paste-from-register)
(define-key evil-hybrid-state-map (kbd "C-z") 'evil-emacs-state)

;; jump-to-definitions
(define-key evil-normal-state-map (kbd "gd") #'xref-find-definitions)
(define-key evil-normal-state-map (kbd "gD") #'xref-find-definitions-other-window)

;; ex completion
(define-key evil-ex-completion-map (kbd "C-d") 'delete-forward-char)

;; use black hole register
(define-key evil-visual-state-map (kbd "<del>") (kbd "\"_d"))
(define-key evil-visual-state-map (kbd "<backspace>") (kbd "\"_d"))

;; evil inc-num, dec-num, find-char-reverse, insert space before and after
(define-key evil-normal-state-map (kbd "-") nil)
(bb/define-key evil-normal-state-map
  "+" 'evil-numbers/inc-at-pt
  "-" 'evil-numbers/dec-at-pt
  "\\" 'evil-repeat-find-char-reverse
  (kbd "DEL") 'evil-repeat-find-char-reverse
  "[s" (lambda (n) (interactive "p") (dotimes (c n nil) (insert " ")))
  "]s" (lambda (n) (interactive "p")
         (forward-char) (dotimes (c n nil) (insert " ")) (backward-char (1+ n))))

;; counsel etags
(define-key evil-normal-state-map (kbd "gF") 'counsel-etags-find-tag-at-point)
(define-key evil-normal-state-map (kbd "gb") 'pop-tag-mark)
(define-key evil-normal-state-map (kbd "gR") 'counsel-etags-recent-tag)
(spacemacs/set-leader-keys "ou" 'counsel-etags-update-tags-force)

;; stack exchange
(define-prefix-command 'launcher-map)
(bind-key* "s-l" #'launcher-map)
(define-key launcher-map "q" #'sx-tab-all-questions)
(define-key launcher-map "i" #'sx-inbox)
(define-key launcher-map "o" #'sx-open-link)
(define-key launcher-map "u" #'sx-tab-unanswered-my-tags)
(define-key launcher-map "a" #'sx-ask)
(define-key launcher-map "s" #'sx-search)

;; company complete
(bind-key* "C-SPC" #'company-complete)

;; comment
(bind-key* "s-/" #'spacemacs/comment-or-uncomment-lines)

;; macro
(bind-key* "s-m" #'call-last-kbd-macro)
(spacemacs/set-leader-keys "om" 'kmacro-edit-macro)

;; git
(spacemacs/set-leader-keys "gT" 'my-git-timemachine)
(spacemacs/set-leader-keys "gR" 'magit-list-repositories)

;; expand region
(bind-key* "C-s-." #'er/expand-region)
(bind-key* "C-s-," #'er/contract-region)

;; redefine C-i and S-tab
(bind-key* "<C-i>" #'evil-shift-right-line)
(bind-key* "<S-tab>" #'evil-shift-left-line)

;; helm bookmark keybindings
(spacemacs/set-leader-keys (kbd "fb") 'helm-filtered-bookmarks)
(with-eval-after-load 'helm-bookmark
  (progn
    (bb/define-key helm-bookmark-map (kbd "C-o") 'helm-bookmark-run-jump-other-window)
    (bb/define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete)
    (bb/define-key helm-bookmark-map (kbd "C-f") 'helm-bookmark-toggle-filename)
    (bb/define-key helm-bookmark-map (kbd "C-e") 'helm-bookmark-run-edit)))

;; yasnippet
(bind-key* "s-i"   #'yas-expand)
(spacemacs/set-leader-keys "yr" 'yas-reload-all)
(spacemacs/set-leader-keys "yd" 'yas-describe-tables)

;; fix yasnippet with unbinding emmet keys
(eval-after-load 'emmet-mode
  '(progn
     (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") nil)
     (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") nil)
     (evil-define-key 'emacs emmet-mode-keymap (kbd "TAB") nil)
     (evil-define-key 'emacs emmet-mode-keymap (kbd "<tab>") nil)
     (evil-define-key 'hybrid emmet-mode-keymap (kbd "TAB") nil)
     (evil-define-key 'hybrid emmet-mode-keymap (kbd "<tab>") nil)))

;; buffer key
(spacemacs/set-leader-keys "bl" 'lsp-ui-imenu)
(spacemacs/set-leader-keys "bf" 'ztlevi/open-finder-in-current-dir)
(spacemacs/set-leader-keys "bt" 'ztlevi/open-terminal-in-project-root)
(spacemacs/set-leader-keys "bT" 'ztlevi/open-terminal-in-current-dir)
(spacemacs/set-leader-keys "bc" 'ztlevi/open-vscode-in-project-root)
(spacemacs/set-leader-keys "bC" 'ztlevi/open-current-file-in-vscode)
(spacemacs/set-leader-keys "bM" 'ztlevi/open-markdown-in-typora)

;; importmagic
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "i" 'importmagic-fix-symbol-at-point)

;; ein jupyter
(spacemacs/set-leader-keys "ays" 'ein:jupyter-server-start)
(spacemacs/set-leader-keys-for-major-mode 'ein:notebook-multilang-mode "c" 'ein:worksheet-change-cell-type)
(spacemacs/set-leader-keys-for-major-mode 'ein:notebook-multilang-mode "a" 'ein:worksheet-execute-all-cell)

;; remap C-h with delte, C-M-h with help-command
;; (define-key evil-hybrid-state-map (kbd "C-h") 'delete-backward-char)
;; (bind-key* "C-M-h" #'help-command)
;; disable view-hello-file key binding
(global-set-key (kbd "C-h h") nil)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)

(spacemacs|add-toggle toggle-shadowsocks-proxy-mode
  :status shadowsocks-proxy-mode
  :on (global-shadowsocks-proxy-mode)
  :off (global-shadowsocks-proxy-mode -1)
  :documentation "Toggle shadowsocks proxy mode."
  :evil-leader "ots")

;; (spacemacs/set-leader-keys "op" 'ztlevi/org-save-and-export)
(spacemacs/set-leader-keys "fR" 'ztlevi/rename-file-and-buffer)

;;Must set key to nil to prevent error: Key sequence b m s starts with non-prefix key b m
(spacemacs/set-leader-keys "bD" 'spacemacs/kill-other-buffers)
(spacemacs/declare-prefix "B" "bookmark")
(spacemacs/set-leader-keys "Bs" 'bookmark-set)
(spacemacs/set-leader-keys "Br" 'bookmark-rename)
(spacemacs/set-leader-keys "Bd" 'bookmark-delete)
(spacemacs/set-leader-keys "Bj" 'counsel-bookmark)
(spacemacs/set-leader-keys "Bb" 'bookmark-bmenu-list)

;; custom key
(spacemacs/declare-prefix "ot" "Toggle")
(spacemacs/set-leader-keys "otf" 'focus-mode)
(spacemacs/set-leader-keys "oo" 'ztlevi/helm-hotspots)
(spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+)
(spacemacs/set-leader-keys "ok" 'helm-show-kill-ring)
(spacemacs/set-leader-keys "o(" 'ielm)
(spacemacs/set-leader-keys "oc" 'eacl-complete-tag)
(spacemacs/set-leader-keys "od" 'occur-dwim)
(spacemacs/set-leader-keys "on" 'occur-non-ascii)
(spacemacs/set-leader-keys "ox" 'org-open-at-point-global)
(spacemacs/set-leader-keys "or" 'revert-buffer-no-confirm)
(spacemacs/set-leader-keys "oR" 'ztlevi/browser-refresh--chrome-applescript)
(spacemacs/set-leader-keys "oT" 'ztlevi/untabify-buffer)
(spacemacs/set-leader-keys "of" 'counsel-git)

;; rjsx-mode
(spacemacs/set-leader-keys "rr" 'rjsx-mode)

;; resume
(spacemacs/set-leader-keys "rh" 'helm-resume)
(spacemacs/set-leader-keys "ri" 'ivy-resume)
(spacemacs/set-leader-keys "rb" 'popwin:display-last-buffer)

;; flycheck & flyspell
(spacemacs/set-leader-keys "en" 'flycheck-next-error)
(spacemacs/set-leader-keys "ep" 'flycheck-previous-error)
(spacemacs/set-leader-keys "os" 'flyspell-buffer)

(spacemacs/set-leader-keys "gL" 'magit-log-buffer-file)

;; dash find
(spacemacs/set-leader-keys "fd" 'dash-at-point)
(spacemacs/set-leader-keys "fD" 'dash-at-point-with-docset)

;; deal with BOM
(spacemacs/set-leader-keys "fl" 'find-file-literally-at-point)
(spacemacs/set-leader-keys "fh" 'ffap-hexl-mode)

;; layout
(spacemacs/declare-prefix "ol" "layout")
(spacemacs/set-leader-keys "oll" 'ztlevi/load-my-layout)
(spacemacs/set-leader-keys "ols" 'ztlevi/save-my-layout)

;; global key binding
(bind-key* "<f1>"  #'ztlevi/helm-hotspots)
(bind-key* "<f5>"  #'ztlevi/run-current-file)
(bind-key* "<f8>"  #'ztlevi/show-current-buffer-major-mode)
(bind-key* "<f9>"  #'org-capture)
(bind-key* "C-`"   #'toggle-input-method)
(bind-key* "C-c /" #'company-files)
(bind-key* "C-c a" #'org-agenda)
(bind-key* "C-c b" #'org-iswitchb)
(bind-key* "C-c k" #'which-key-show-top-level)
(bind-key* "C-c l" #'ztlevi/insert-chrome-current-tab-url)
(bind-key* "C-c t" #'org-capture)
(bind-key* "C-c y" #'youdao-dictionary-search-at-point+)
(bind-key* "C-s-;" #'ztlevi/delete-semicolon-at-the-end-of-this-line)
(bind-key* "M--"   #'ztlevi/goto-match-paren)
(bind-key* "s-,"   #'ztlevi/insert-comma-at-the-end-of-this-line)
(bind-key* "s-;"   #'ztlevi/insert-semicolon-at-the-end-of-this-line)
(bind-key* "s-g"   #'goto-line)
(bind-key* "s-r"   #'mc/reverse-regions)
(bind-key* "s-y"   #'aya-expand)
(global-set-key [(shift return)] #'ztlevi/smart-open-line)
(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)

;; keybindings for linux and macOS
(when (or (spacemacs/system-is-linux) (spacemacs/system-is-mac))
  (bind-key* "s-p"   #'counsel-git)
  (bind-key* "s-e"   #'ivy-switch-buffer)
  (bind-key* "C-s"   #'my-swiper-search)
  (bind-key* "s-f"   #'my-swiper-search)
  (bind-key* "s-F"   #'spacemacs/search-project-auto)
  (bind-key* "s-s"   #'evil-write-all)
  (bind-key* "s-w"   #'delete-window-or-frame)
  (bind-key* "s-o"   #'spacemacs/jump-to-last-layout)
  (bind-key* "C-s-o" #'other-frame)
  (bind-key* "<C-s-268632079>" #'other-frame)
  (bind-key* "s-="   #'spacemacs/scale-up-font)
  (bind-key* "s--"   #'spacemacs/scale-down-font)
  (bind-key* "s-0"   #'spacemacs/reset-font-size)
  (bind-key* "s-q"   #'save-buffers-kill-terminal)
  (bind-key* "s-v"   #'yank)
  (bind-key* "s-c"   #'evil-yank)
  (bind-key* "s-a"   #'mark-whole-buffer)
  (bind-key* "s-x"   #'kill-region)
  (bind-key* "s-n"   #'switch-to-buffer-other-frame)
  (bind-key* "s-z"   #'undo-tree-undo)
  (bind-key* "s-Z"   #'undo-tree-redo)
  ;; hungry delete
  (bind-key* "s-<backspace>" #'hungry-delete-backward)
  ;; iterm
  (spacemacs/set-leader-keys "o!" 'ztlevi/iterm-shell-command))
;; keybindings for Windows
(when (spacemacs/system-is-mswindows)
  (bind-key* "C-S-p" #'counsel-git)
  (bind-key* "C-S-e" #'ivy-switch-buffer)
  (bind-key* "C-f"   #'my-swiper-search)
  (bind-key* "C-S-f" #'spacemacs/search-project-auto)
  (bind-key* "C-S-s" #'evil-write-all)
  (bind-key* "C-S-w" #'delete-window-or-frame)
  (bind-key* "C-S-o" #'other-frame)
  (bind-key* "C-="   #'spacemacs/scale-up-font)
  (bind-key* "C--"   #'spacemacs/scale-down-font)
  (bind-key* "C-0"   #'spacemacs/reset-font-size)
  (bind-key* "C-S-q" #'save-buffers-kill-terminal)
  (bind-key* "C-v"   #'yank)
  (bind-key* "C-S-v" #'yank)
  (bind-key* "C-S-c" #'evil-yank)
  (bind-key* "C-S-a" #'mark-whole-buffer)
  (bind-key* "C-S-x" #'kill-region)
  (bind-key* "C-S-n" #'switch-to-buffer-other-frame)
  (bind-key* "C-S-z" #'undo-tree-undo)
  ;; hungry delete
  (bind-key* "C-<backspace>" #'hungry-delete-backward))
