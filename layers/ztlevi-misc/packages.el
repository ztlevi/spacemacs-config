;;; packages.el --- ztlevi-misc layer packages file for Spacemacs.
;;
;; Copyright (c) 2016-2017 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ztlevi-misc-packages
  '(
    sx
    sos
    helm-github-stars
    atomic-chrome
    projectile
    prodigy
    find-file-in-project
    multiple-cursors
    visual-regexp
    visual-regexp-steroids
    command-log
    evil
    discover-my-major
    ace-window
    avy
    ;; 4clojure
    persp-mode
    tiny
    ;; smartparens
    flyspell-correct
    peep-dired
    markdown-mode
    ivy
    swiper
    magit
    git-messenger
    gist
    hydra
    wrap-region
    golden-ratio
    (highlight-global :location (recipe :fetcher github :repo "glen-dai/highlight-global"))
    browse-at-remote
    )
  )

(defun ztlevi-misc/init-sx ()
  (use-package sx
    :defer t
    :config
    (with-eval-after-load 'sx-question-list
      (progn
        (define-key sx-question-list-mode-map "j" 'sx-question-list-next)
        (define-key sx-question-list-mode-map "k" 'sx-question-list-previous)
        (define-key sx-question-list-mode-map "n" 'sx-question-list-view-next)
        (define-key sx-question-list-mode-map "p" 'sx-question-list-view-previous)))))

(defun ztlevi-misc/init-sos ()
  (use-package sos
    :defer t))

(defun ztlevi-misc/init-atomic-chrome ()
  (use-package atomic-chrome
    :ensure t                           ; To install its dependencies
    :preface
    (defun ztlevi-atomic-chrome-server-running-p ()
      (cond ((executable-find "lsof")
             (zerop (call-process "lsof" nil nil nil "-i" ":64292")))
            ((executable-find "netstat") ; Windows
             (zerop (call-process-shell-command "netstat -aon | grep 64292")))))
    :config
    (setq atomic-chrome-buffer-open-style 'full) ;; or frame, split
    (setq atomic-chrome-url-major-mode-alist
          '(("github\\.com"        . gfm-mode)
            ("emacs-china\\.org"   . gfm-mode)
            ("stackexchange\\.com" . gfm-mode)
            ("stackoverflow\\.com" . gfm-mode)
            ("lintcode\\.com"      . python-mode)
            ("leetcode\\.com"      . python-mode)))

    (defun ztlevi-atomic-chrome-mode-setup ()
      (setq header-line-format
            (substitute-command-keys
             "Edit Chrome text area.  Finish \
`\\[atomic-chrome-close-current-buffer]'.")))

    (add-hook 'atomic-chrome-edit-mode-hook #'ztlevi-atomic-chrome-mode-setup)
    (add-hook 'atomic-chrome-edit-done-hook (lambda () (shell-command "open -a \"/Applications/Google Chrome.app\"")))

    (if (ztlevi-atomic-chrome-server-running-p)
        (message "Can't start atomic-chrome server, because port 64292 is already used")
      (atomic-chrome-start-server))))

(defun ztlevi-misc/init-browse-at-remote ()
  (use-package browse-at-remote
    :defer t
    :init (spacemacs/set-leader-keys "gho" 'browse-at-remote)))

(defun ztlevi-misc/init-highlight-global ()
  (use-package highlight-global
    :init
    (progn
      (spacemacs/set-leader-keys "hh" 'highlight-frame-toggle)
      (spacemacs/set-leader-keys "hc" 'clear-highlight-frame)
      (setq-default highlight-faces
                    '(('hi-red-b . 0)
                      ('hi-yellow . 0)
                      ('hi-pink . 0)
                      ('hi-blue-b . 0))))))

(defun ztlevi-misc/post-init-golden-ratio ()
  (with-eval-after-load 'golden-ratio
    (dolist (mode '("dired-mode" "occur-mode"))
      (add-to-list 'golden-ratio-exclude-modes mode))
    (dolist (n '("COMMIT_EDITMSG"))
      (add-to-list 'golden-ratio-exclude-buffer-names n))))

(defun ztlevi-misc/post-init-hydra ()
  (progn
    (defhydra hydra-hotspots (:color blue)
      "Hotspots"
      ("b" blog-admin-start "blog")
      ("g" helm-github-stars "helm github stars")
      ("r" ztlevi/run-current-file "run current file"))

    (defhydra multiple-cursors-hydra (:hint nil)
      "
       ^Up^            ^Down^        ^Other^
             ----------------------------------------------
         [_p_]   Next    [_n_]   Next    [_l_] Edit lines
         [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
         [_M-p_] Unmark  [_M-n_] Unmark [_r_] Mark by regexp
         ^ ^             ^ ^ [_q_] Quit
       "
      ("l" mc/edit-lines :exit t)
      ("a" mc/mark-all-like-this :exit t)
      ("n" mc/mark-next-like-this)
      ("N" mc/skip-to-next-like-this)
      ("M-n" mc/unmark-next-like-this)
      ("p" mc/mark-previous-like-this)
      ("P" mc/skip-to-previous-like-this)
      ("M-p" mc/unmark-previous-like-this)
      ("r" mc/mark-all-in-region-regexp :exit t)
      ("q"
       nil))

    (defhydra
      hydra-apropos (:color blue)
      "Apropos"
      ("a" apropos "apropos")
      ("c" apropos-command "cmd")
      ("d" apropos-documentation "doc")
      ("e" apropos-value "val")
      ("l" apropos-library "lib")
      ("o" apropos-user-option "option")
      ("u" apropos-user-option "option")
      ("v" apropos-variable "var")
      ("i" info-apropos "info")
      ("t" tags-apropos "tags")
      ("z" hydra-customize-apropos/body "customize"))

    (defhydra
      hydra-customize-apropos (:color blue)
      "Apropos (customize)"
      ("a" customize-apropos "apropos")
      ("f" customize-apropos-faces "faces")
      ("g" customize-apropos-groups "groups")
      ("o" customize-apropos-options "options"))

    (define-key global-map (kbd "<f1>") 'hydra-hotspots/body)
    (spacemacs/set-leader-keys "oo" 'hydra-hotspots/body)
    ;; (bind-key*  "<f4>" 'hydra-apropos/body)
    (spacemacs/set-leader-keys "oh" 'hydra-apropos/body)

    ))

(defun ztlevi-misc/post-init-gist ()
  (use-package gist
    :defer t
    :init
    (setq gist-list-format
          '((files "File" 30 nil "%s")
            (id "Id" 10 nil identity)
            (created "Created" 20 nil "%D %R")
            (visibility "Visibility" 10 nil
                        (lambda
                          (public)
                          (or
                           (and public "public")
                           "private")))
            (description "Description" 0 nil identity)))
    :config
    (progn
      (spacemacs|define-transient-state gist-list-mode
        :title "Gist-mode Transient State"
        :bindings
        ("k" gist-kill-current "delete gist")
        ("e" gist-edit-current-description "edit gist title")
        ("+" gist-add-buffer "add a file")
        ("-" gist-remove-file "delete a file")
        ("y" gist-print-current-url "print url")
        ("b" gist-browse-current-url "browse gist in browser")
        ("*" gist-star "star gist")
        ("^" gist-unstar "unstar gist")
        ("f" gist-fork "fork gist")
        ("q" nil "quit" :exit t)
        ("<escape>" nil nil :exit t))
      (spacemacs/set-leader-keys-for-major-mode 'gist-list-mode
        "." 'spacemacs/gist-list-mode-transient-state/body))
    ))

(defun ztlevi-misc/init-peep-dired ()
  ;;preview files in dired
  (use-package peep-dired
    :defer t
    :commands (peep-dired-next-file
               peep-dired-prev-file)
    :bind (:map dired-mode-map
                ("P" . peep-dired))))

(defun ztlevi-misc/post-init-flyspell-correct ()
  (progn
    (with-eval-after-load 'flyspell
      (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic))
    (setq flyspell-correct-interface 'flyspell-correct-ivy)))

(defun ztlevi-misc/post-init-smartparens ()
  (use-package smartparens
    :defer t
    :init
    (progn
      (global-set-key (kbd "C-(") 'wrap-sexp-with-new-round-parens))
    :config
    (progn
      (setq sp-highlight-pair-overlay nil)

      (evil-define-key 'normal sp-keymap
        (kbd ")>") 'sp-forward-slurp-sexp
        (kbd ")<") 'sp-forward-barf-sexp
        (kbd "(>") 'sp-backward-barf-sexp
        (kbd "(<") 'sp-backward-slurp-sexp))))

(defun ztlevi-misc/init-tiny ()
  (use-package tiny
    :defer t
    :init
    (spacemacs/set-leader-keys "oe" 'tiny-expand)))

(defun ztlevi-misc/init-helm-github-stars ()
  (use-package helm-github-stars
    :commands (helm-github-stars)
    :init
    (setq helm-github-stars-username "ztlevi")))

(defun ztlevi-misc/post-init-command-log ()
  (with-eval-after-load 'global-command-log-mode
    (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                              '(evil-next-visual-line
                                                evil-previous-visual-line)))))

(defun ztlevi-misc/init-litable ()
  (use-package litable
    :init
    :defer t))

(defun ztlevi-misc/init-osx-dictionary ()
  (use-package osx-dictionary
    :init
    (progn
      (evilified-state-evilify osx-dictionary-mode osx-dictionary-mode-map)
      (setq osx-dictionary-use-chinese-text-segmentation t)
      (global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)
      )))


(defun ztlevi-misc/init-4clojure ()
  (use-package 4clojure
    :init
    (progn
      (spacemacs/declare-prefix "o4" "4clojure")
      (spacemacs/set-leader-keys "o4q" '4clojure-open-question)
      (spacemacs/set-leader-keys "o4n" '4clojure-next-question)
      (spacemacs/set-leader-keys "o4p" '4clojure-previous-question)
      (spacemacs/set-leader-keys "o4c" '4clojure-check-answers)
      )))

(defun ztlevi-misc/post-init-avy ()
  (progn
    (global-set-key (kbd "C-s-'") 'avy-goto-char-2)
    (global-set-key (kbd "M-'") 'avy-goto-char-2)))

(defun ztlevi-misc/post-init-ace-window ()
  (global-set-key (kbd "C-x C-o") #'ace-window))

(defun ztlevi-misc/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys (kbd "mhm") 'discover-my-major)
      (evilified-state-evilify makey-key-mode makey-key-mode-get-key-map)
      )))


(defun ztlevi-misc/post-init-elfeed ()
  (use-package elfeed
    :init
    (global-set-key (kbd "C-x w") 'elfeed)
    :defer t
    :config
    (progn

      (setq elfeed-feeds
            '("http://nullprogram.com/feed/"
              "http://z.caudate.me/rss/"
              "http://irreal.org/blog/?feed=rss2"
              "http://feeds.feedburner.com/LostInTheTriangles"
              "http://tonybai.com/feed/"
              "http://planet.emacsen.org/atom.xml"
              "http://feeds.feedburner.com/emacsblog"
              "http://blog.binchen.org/rss.xml"
              "http://oremacs.com/atom.xml"
              "http://blog.gemserk.com/feed/"
              "http://www.masteringemacs.org/feed/"
              "http://t-machine.org/index.php/feed/"
              "http://gameenginebook.blogspot.com/feeds/posts/default"
              "http://feeds.feedburner.com/ruanyifeng"
              "http://coolshell.cn/feed"
              "http://blog.devtang.com/atom.xml"
              "http://emacsist.com/rss"
              "http://puntoblogspot.blogspot.com/feeds/2507074905876002529/comments/default"
              "http://angelic-sedition.github.io/atom.xml"))

      ;; (evilify elfeed-search-mode elfeed-search-mode-map)
      (evilified-state-evilify-map elfeed-search-mode-map
        :mode elfeed-search-mode
        :bindings
        "G" 'elfeed-update
        "g" 'elfeed-search-update--force)

      (defun ztlevi/elfeed-mark-all-as-read ()
        (interactive)
        (mark-whole-buffer)
        (elfeed-search-untag-all-unread))

      (define-key elfeed-search-mode-map (kbd "R") 'ztlevi/elfeed-mark-all-as-read)

      (defadvice elfeed-show-yank (after elfeed-show-yank-to-kill-ring activate compile)
        "Insert the yanked text from x-selection to kill ring"
        (kill-new (x-get-selection)))

      (ad-activate 'elfeed-show-yank))))

(defun ztlevi-misc/post-init-evil ()
  (progn
    (push "TAGS" spacemacs-useless-buffers-regexp)

    (adjust-major-mode-keymap-with-evil "git-timemachine")
    (adjust-major-mode-keymap-with-evil "tabulated-list")

    (define-key evil-visual-state-map "p" 'evil-paste-after-from-0)

    ;;mimic "nzz" behaviou in vim
    (defadvice evil-search-next (after advice-for-evil-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (defadvice evil-search-previous (after advice-for-evil-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

    ;; indent buffer
    (define-key evil-normal-state-map (kbd ",=") 'spacemacs/indent-region-or-buffer)
    (define-key evil-visual-state-map (kbd ",=") 'spacemacs/indent-region-or-buffer)

    ;; bind [, ] functions
    (define-key evil-normal-state-map (kbd "[ SPC") (lambda () (interactive) (evil-insert-newline-above) (forward-line)))
    (define-key evil-normal-state-map (kbd "] SPC") (lambda () (interactive) (evil-insert-newline-below) (forward-line -1)))
    (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
    (define-key evil-normal-state-map (kbd "] b") 'next-buffer)

    (define-key evil-normal-state-map (kbd "M-y") 'counsel-yank-pop)

    (define-key evil-motion-state-map "\C-e" 'mwim-end-of-code-or-line)

    (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
    (define-key evil-ex-completion-map "\C-b" 'backward-char)
    (define-key evil-ex-completion-map "\C-k" 'kill-line)
    (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

    ;; visual-state-map
    (define-key evil-visual-state-map (kbd "C-r") 'ztlevi/evil-quick-replace)
    (define-key evil-visual-state-map (kbd "ml") 'mc/edit-lines)
    (define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
    (define-key evil-visual-state-map (kbd "m M-n") 'mc/unmark-next-like-this)
    (define-key evil-visual-state-map (kbd "mp") 'mc/mark-previous-like-this)
    (define-key evil-visual-state-map (kbd "m M-p") 'mc/unmark-previous-like-this)
    (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this)
    (define-key evil-visual-state-map (kbd "mf") 'mc/mark-all-like-this-in-defun)

    ;; in spacemacs, we always use evilify miscro state
    (evil-add-hjkl-bindings package-menu-mode-map 'emacs)

    ;; set C-w as delete word backward
    (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)
    (evil-define-key 'emacs term-raw-map (kbd "C-w") 'evil-delete-backward-word)

    ;; set state tag
    (setq evil-normal-state-tag    (propertize " 𝗡 " 'face '((:inherit spacemacs-normal-face)))
          evil-emacs-state-tag     (propertize " 𝗘 " 'face '((:inherit spacemacs-emacs-face)))
          evil-hybrid-state-tag    (propertize " 𝗛 " 'face '((:inherit spacemacs-hybrid-face)))
          evil-insert-state-tag    (propertize " 𝗜 " 'face '((:inherit spacemacs-insert-face)))
          evil-motion-state-tag    (propertize " 𝗠 " 'face '((:inherit spacemacs-motion-face)))
          evil-visual-state-tag    (propertize " 𝗩 " 'face '((:inherit spacemacs-visual-face)))
          evil-evilified-state-tag (propertize " 𝗙 " 'face '((:inherit spacemacs-evilified-face)))
          evil-replace-state-tag   (propertize " 𝗥 " 'face '((:inherit spacemacs-replace-face)))
          evil-operator-state-tag  (propertize " 𝗢 " 'face '((:background "purple" :foreground "white"))))

    ;; set evil state cursor
    (setq evil-normal-state-cursor '("#ff007f" box))
    (setq evil-insert-state-cursor '("#ff007f" (bar . 2)))
    (setq evil-hybrid-state-cursor '("#ff007f" (bar . 2)))
    ))

(defun ztlevi-misc/init-visual-regexp ()
  (use-package visual-regexp
    :commands (vr/replace vr/query-replace)))

(defun ztlevi-misc/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :commands (vr/select-replace vr/select-query-replace)
    :init
    (progn
      (define-key global-map (kbd "C-c r") 'vr/replace)
      (define-key global-map (kbd "C-c q") 'vr/query-replace))))

(defun ztlevi-misc/init-multiple-cursors ()
  (use-package multiple-cursors
    :init
    (progn
      (bind-key* "C-s-l" 'mc/edit-lines)
      (bind-key* "C-s-f" 'mc/mark-all-dwim)
      (bind-key* "C-s-." 'mc/mark-next-like-this)
      (bind-key* "C-s-," 'mc/mark-previous-like-this)
      (bind-key* "s->" 'mc/unmark-next-like-this)
      (bind-key* "s-<" 'mc/unmark-previous-like-this)
      (bind-key* "C-c C-s-." 'mc/mark-all-like-this)

      ;; http://endlessparentheses.com/multiple-cursors-keybinds.html?source=rss
      (define-prefix-command 'endless/mc-map)
      ;; C-x m is usually `compose-mail'. Bind it to something
      ;; else if you use this command.
      (define-key ctl-x-map "m" 'endless/mc-map)
;;; Really really nice!
      (define-key endless/mc-map "i" #'mc/insert-numbers)
      (define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
      (define-key endless/mc-map "a" #'mc/mark-all-like-this)

;;; Occasionally useful
      (define-key endless/mc-map "d" #'mc/mark-all-symbols-like-this-in-defun)
      (define-key endless/mc-map "r" #'mc/reverse-regions)
      (define-key endless/mc-map "s" #'mc/sort-regions)
      (define-key endless/mc-map "l" #'mc/edit-lines)
      (define-key endless/mc-map "\C-a" #'mc/edit-beginnings-of-lines)
      (define-key endless/mc-map "\C-e" #'mc/edit-ends-of-lines)
      )
    :config
    (setq mc/always-repeat-command t)
    (setq mc/always-run-for-all t)

    (define-key mc/keymap (kbd "<return>") nil)
    (global-unset-key (kbd "M-<down-mouse-1>"))
    (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
    ))

(defun ztlevi-misc/post-init-persp-mode ()
  (setq persp-kill-foreign-buffer-action 'kill)
  (setq persp-lighter nil)
  (when (fboundp 'spacemacs|define-custom-layout)
    (spacemacs|define-custom-layout "@Hexo-Blog"
      :binding "h"
      :body
      (find-file "~/Developer/Github/hexo_blog/_config.yml")
      (split-window-right)
      (find-file "~/Developer/Github/hexo_blog/package.json"))))

(defun ztlevi-misc/post-init-chinese-wbim ()
  (progn
    (bind-key* ";" 'chinese-wbim-insert-ascii)
    (setq chinese-wbim-punc-translate-p nil)
    (spacemacs/declare-prefix "ot" "Toggle")
    (spacemacs/set-leader-keys
      "otp" 'chinese-wbim-punc-translate-toggle)
    (setq chinese-wbim-wb-use-gbk t)
    (add-hook 'chinese-wbim-wb-load-hook
              (lambda ()
                (let ((map (chinese-wbim-mode-map)))
                  (define-key map "-" 'chinese-wbim-previous-page)
                  (define-key map "=" 'chinese-wbim-next-page))))
    ))

(defun ztlevi-misc/init-find-file-in-project ()
  (use-package find-file-in-project
    :defer t
    :config
    (progn
      ;; If you use other VCS (subversion, for example), enable the following option
      ;;(setq ffip-project-file ".svn")
      ;; in MacOS X, the search file command is CMD+p
      ;; for this project, I'm only interested certain types of files
      (setq-default ffip-patterns '("*.html" "*.js" "*.css" "*.java" "*.xml" "*.cpp" "*.h" "*.c" "*.mm" "*.m" "*.el"))
      ;; if the full path of current file is under SUBPROJECT1 or SUBPROJECT2
      ;; OR if I'm reading my personal issue track document,
      (defadvice find-file-in-project (before my-find-file-in-project activate compile)
        (when (ffip-current-full-filename-match-pattern-p "\\(/fireball\\)")
          ;; set the root directory into "~/projs/PROJECT_DIR"
          (setq-local ffip-project-root "~/Github/fireball")
          ;; well, I'm not interested in concatenated BIG js file or file in dist/
          (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
          ;; do NOT search files in below directories, the default value is better.
          (dolist (item '("*/docs/html/*" "*.meta" "*/cocos2d-x/*" "*.asset" "*/visual-tests/res/*"))
            (push item  ffip-prune-patterns)))
        (when (ffip-current-full-filename-match-pattern-p "\\(/cocos2d-x\\)")
          ;; set the root directory into "~/projs/PROJECT_DIR"
          (setq-local ffip-project-root "~/cocos2d-x")
          ;; well, I'm not interested in concatenated BIG js file or file in dist/
          (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
          ;; do NOT search files in below directories, the default value is better.
          ;; (setq-default ffip-prune-patterns '(".git" ".hg" "*.svn" "node_modules" "bower_components" "obj"))
          ))
      (ad-activate 'find-file-in-project))))

(defun ztlevi-misc/post-init-projectile ()
  (progn
    (with-eval-after-load 'projectile
      (progn
        (setq projectile-completion-system 'ivy)
        (add-to-list 'projectile-other-file-alist '("html" "js"))
        (add-to-list 'projectile-other-file-alist '("js" "html"))))

    (defvar my-simple-todo-regex "\\<\\(FIXME\\|TODO\\|BUG\\):")

    (defun my-simple-todo ()
      "When in a project, create a `multi-occur' buffer matching the
  regex in `my-simple-todo-regex' across all buffers in the
  current project. Otherwise do `occur' in the current file."
      (interactive)
      (if (projectile-project-p)
          (multi-occur (projectile-project-buffers) my-simple-todo-regex)
        (occur my-simple-todo-regex)))
    (spacemacs/set-leader-keys "pf" 'ztlevi/open-file-with-projectile-or-counsel-git)
    (spacemacs/set-leader-keys "pt" 'my-simple-todo)))

(defun ztlevi-misc/post-init-prodigy ()
  (progn
    (prodigy-define-tag
      :name 'jekyll
      :env '(("LANG" "en_US.UTF-8")
             ("LC_ALL" "en_US.UTF-8")))
    ;; define service
    (prodigy-define-service
      :name "Debug TRI Demo"
      :command "npm"
      :args '("start")
      :cwd "~/Developer/Github/TRI_demo"
      :tags '(work)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Leetcode Solution Website"
      :command "python"
      :args '("-m" "SimpleHTTPServer" "6005")
      :cwd "~/Developer/Github/leetcode"
      :tags '(leetcode)
      ;; if don't want to browse instantly, delete the following line
      :init (lambda () (browse-url "http://localhost:6005"))
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Hexo Blog Server"
      :command "hexo"
      :args '("server" "-p" "4000")
      :cwd "~/Developer/Github/hexo_blog"
      :tags '(hexo server)
      :init (lambda () (browse-url "http://localhost:4000"))
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Hexo Blog Deploy"
      :command "hexo"
      :args '("deploy" "--generate")
      :cwd "~/Developer/Github/hexo_blog"
      :tags '(hexo deploy)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    ;; (prodigy-define-service
    ;;   :name "Debug Fireball"
    ;;   :command "npm"
    ;;   :args '("start" "--" "--nologin" "/Users/guanghui/Github/example-cases")
    ;;   :cwd "~/Github/fireball/"
    ;;   :tags '(work)
    ;;   :kill-signal 'sigkill
    ;;   :kill-process-buffer-on-stop t)

    (defun refresh-chrome-current-tab (beg end length-before)
      (call-interactively 'ztlevi/browser-refresh--chrome-applescript))
    ;; add watch for prodigy-view-mode buffer change event
    (add-hook 'prodigy-view-mode-hook
              #'(lambda() (set (make-local-variable 'after-change-functions) #'refresh-chrome-current-tab)))
    ))

(defun ztlevi-misc/init-moz-controller ()
  (use-package moz-controller
    :init
    (progn
      (moz-controller-global-mode t)
      (spacemacs|hide-lighter moz-controller-mode))))

(defun ztlevi-misc/init-ag ()
  (use-package ag
    :init))

(defun ztlevi-misc/post-init-erc ()
  (progn
    (add-hook 'erc-text-matched-hook 'my-erc-hook)
    (spaceline-toggle-erc-track-off)))

(defun ztlevi-misc/init-wrap-region ()
  (use-package wrap-region
    :init
    (progn
      (wrap-region-global-mode t)
      (wrap-region-add-wrappers
       '(("$" "$")
         ("{-" "-}" "#")
         ("/" "/" nil ruby-mode)
         ("/* " " */" "#" (java-mode javascript-mode css-mode js2-mode))
         ("`" "`" nil (markdown-mode ruby-mode))))
      (add-to-list 'wrap-region-except-modes 'dired-mode)
      (add-to-list 'wrap-region-except-modes 'web-mode)
      )
    :defer t
    :config
    (spacemacs|hide-lighter wrap-region-mode)))

(defun ztlevi-misc/init-keyfreq ()
  (use-package keyfreq
    :init
    (progn
      (keyfreq-mode t)
      (keyfreq-autosave-mode 1))))

(defun ztlevi-misc/post-init-ivy ()
  (progn
    (setq ivy-display-style 'fancy)
    (setq ivy-initial-inputs-alist nil)
    (setq ivy-wrap t)
    (setq confirm-nonexistent-file-or-buffer t)

    (define-key ivy-minibuffer-map (kbd "<C-i>") 'ivy-call)
    (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial-or-done)
    (define-key ivy-minibuffer-map (kbd "C-<return>") 'ivy-immediate-done)
    ))

(defun ztlevi-misc/post-init-swiper ()
  (define-key global-map (kbd "C-s") 'my-swiper-search))

(defun ztlevi-misc/post-init-magit ()
  (progn
    (with-eval-after-load 'magit
      (progn

        (add-to-list 'magit-no-confirm 'stage-all-changes)
        (define-key magit-log-mode-map (kbd "W") 'magit-copy-section-value)
        (define-key magit-status-mode-map (kbd "s-1") 'magit-jump-to-unstaged)
        (define-key magit-status-mode-map (kbd "s-2") 'magit-jump-to-untracked)
        (define-key magit-status-mode-map (kbd "s-3") 'magit-jump-to-staged)
        (define-key magit-status-mode-map (kbd "s-4") 'magit-jump-to-stashes)
        (setq magit-completing-read-function 'magit-builtin-completing-read)

        (magit-define-popup-switch 'magit-push-popup ?u
          "Set upstream" "--set-upstream")
        ))

    ;; prefer two way ediff
    (setq magit-ediff-dwim-show-on-hunks t)

    (setq magit-push-always-verify nil)

    (eval-after-load 'magit
      '(define-key magit-mode-map (kbd "C-c g")
         #'ztlevi/magit-visit-pull-request))

    (setq magit-process-popup-time 10)))

(defun ztlevi-misc/post-init-git-messenger ()
  (use-package git-messenger
    :defer t
    :config
    (progn
      (define-key git-messenger-map (kbd "f") 'ztlevi/github-browse-commit))))

(defun ztlevi-misc/post-init-markdown-mode ()
  (progn
    (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

    ;; use remark to format markdown
    (defun remark/format-markdown ()
      (interactive)
      (save-buffer)
      (cond
       ((string-equal system-type "darwin")
        (shell-command (concat "remark --silent --no-color --setting listItemIndent:1 '" buffer-file-name "' -o '" buffer-file-name "'")))
       ((string-equal system-type "gnu/linux")
        (let ((process-connection-type nil))
          (start-process "" nil "remark" (concat " --silent --no-color --setting listItemIndent:1 '" buffer-file-name "' -o '" buffer-file-name)))))
      (deactivate-mark)
      (revert-buffer :ignore-auto :noconfirm))

    ;; define markdown keys
    (with-eval-after-load 'markdown-mode
      (progn
        ;; (when (configuration-layer/package-usedp 'company)
        ;;   (spacemacs|add-company-hook markdown-mode))
        (dolist (mode markdown--key-bindings-modes)
          (spacemacs/set-leader-keys-for-major-mode mode
            "=" 'remark/format-markdown)
          (spacemacs/set-leader-keys-for-major-mode mode
            "o" 'markdown-follow-link-at-point)
          (spacemacs/set-leader-keys-for-major-mode mode
            "D" 'my-flymd-delete-tmp-file)
          (spacemacs/set-leader-keys-for-major-mode mode
            "p" 'ztlevi/markdown-to-html)
          (spacemacs/set-leader-keys-for-major-mode mode
            "H" 'markdown-hide-body)
          (spacemacs/set-leader-keys-for-major-mode mode
            "S" 'markdown-show-all)
          (spacemacs/set-leader-keys-for-major-mode mode
            "dd" 'org-deadline)
          (spacemacs/set-leader-keys-for-major-mode mode
            "ds" 'org-schedule)
          (spacemacs/set-leader-keys-for-major-mode mode
            "dt" 'org-time-stamp)
          (spacemacs/set-leader-keys-for-major-mode mode
            "dT" 'org-time-stamp-inactive))

        (evil-define-key 'normal markdown-mode-map (kbd "TAB") 'markdown-cycle)
        (evil-define-key 'normal gfm-mode-map (kbd "TAB") 'markdown-cycle)

        (spacemacs//evil-org-mode)
        (evil-define-key 'normal markdown-mode-map (kbd "o") 'evil-org-open-below)
        (evil-define-key 'normal gfm-mode-map (kbd "o") 'evil-org-open-below)
        (evil-define-key 'normal markdown-mode-map (kbd "O") 'evil-org-open-above)
        (evil-define-key 'normal gfm-mode-map (kbd "O") 'evil-org-open-above)
        ))
    ))
