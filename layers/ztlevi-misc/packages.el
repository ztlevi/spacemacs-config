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
    ;; sx
    ;; sos
    helm-github-stars
    atomic-chrome
    projectile
    prodigy
    find-file-in-project
    multiple-cursors
    visual-regexp
    visual-regexp-steroids
    command-log
    origami
    evil
    evil-surround
    discover-my-major
    ;; 4clojure
    persp-mode
    focus
    flyspell-correct
    peep-dired
    markdown-mode
    (live-server :location local)
    edit-indirect
    ivy
    swiper
    magit
    github-browse-file
    git-messenger
    hydra
    wrap-region
    golden-ratio
    (highlight-global :location (recipe :fetcher github :repo "glen-dai/highlight-global"))
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
    :defer 3
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
            ;; jupyter notebook
            ("localhost\\:8888"    . python-mode)
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
      (atomic-chrome-start-server))

    ;; bind keys
    (spacemacs/set-leader-keys "cc" 'atomic-chrome-close-current-buffer)))

(defun ztlevi-misc/init-highlight-global ()
  (use-package highlight-global
    :defer t
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

(defun ztlevi-misc/post-init-origami ()
  (add-to-list 'origami-parser-alist `(js2-mode . ,(origami-markers-parser "//region" "//endregion")))
  (add-to-list 'origami-parser-alist `(java-mode . ,(origami-markers-parser "//region" "//endregion")))
  (add-to-list 'origami-parser-alist `(python-mode . ,(origami-markers-parser "# region" "# endregion")))
  (add-to-list 'origami-parser-alist `(ruby-mode . ,(origami-markers-parser "#region" "#endregion")))
  )

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

(defun ztlevi-misc/init-focus ()
  (use-package focus
    :defer t))

(defun ztlevi-misc/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :commands (helm-github-stars)
    :init
    (setq helm-github-stars-username "ztlevi")))

(defun ztlevi-misc/post-init-command-log ()
  (with-eval-after-load 'global-command-log-mode
    (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                              '(evil-next-visual-line
                                                evil-previous-visual-line)))))

(defun ztlevi-misc/init-4clojure ()
  (use-package 4clojure
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "o4" "4clojure")
      (spacemacs/set-leader-keys "o4q" '4clojure-open-question)
      (spacemacs/set-leader-keys "o4n" '4clojure-next-question)
      (spacemacs/set-leader-keys "o4p" '4clojure-previous-question)
      (spacemacs/set-leader-keys "o4c" '4clojure-check-answers))))

(defun ztlevi-misc/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys (kbd "mhm") 'discover-my-major)
      (evilified-state-evilify makey-key-mode makey-key-mode-get-key-map))))

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

    ;; visual-state-map
    (define-key evil-visual-state-map (kbd "C-r") 'ztlevi/evil-quick-replace)

    ;; set evil state cursor
    ;; (setq evil-normal-state-cursor '("#ff007f" box))
    ;; (setq evil-insert-state-cursor '("#ff007f" (bar . 2)))
    ;; (setq evil-hybrid-state-cursor '("#ff007f" (bar . 2)))
    ))

(defun ztlevi-misc/post-init-evil-surround ()
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "Cs" 'evil-surround-change)
  (evil-define-key 'visual evil-surround-mode-map "Ds" 'evil-surround-delete))

(defun ztlevi-misc/init-visual-regexp ()
  (use-package visual-regexp
    :defer t
    :commands (vr/replace vr/query-replace)))

(defun ztlevi-misc/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :defer t
    :commands (vr/select-replace vr/select-query-replace)
    :init
    (progn
      (define-key global-map (kbd "C-c r") 'vr/replace)
      (define-key global-map (kbd "C-c q") 'vr/query-replace))))

(defun ztlevi-misc/init-multiple-cursors ()
  (use-package multiple-cursors
    :defer t
    :init
    (progn
      (bind-key* "C-s-l" 'mc/edit-lines)
      (bind-key* "C-s-g" 'mc/mark-all-like-this)
      (bind-key* "C->" 'mc/mark-next-like-this)
      (bind-key* "C-<" 'mc/mark-previous-like-this)
      (bind-key* "s->" 'mc/unmark-next-like-this)
      (bind-key* "s-<" 'mc/unmark-previous-like-this)

      ;; add mouse click
      (global-unset-key (kbd "M-<down-mouse-1>"))
      (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

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
      (define-key endless/mc-map "\C-e" #'mc/edit-ends-of-lines))
    :config
    (setq mc/always-repeat-command t)
    (setq mc/always-run-for-all t)

    (define-key mc/keymap (kbd "<return>") nil)))

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
      :cwd blog-admin-dir
      :tags '(hexo server)
      :init (lambda () (browse-url "http://localhost:4000"))
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Hexo Blog Deploy"
      :command "hexo"
      :args '("deploy" "--generate")
      :cwd blog-admin-dir
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

(defun ztlevi-misc/post-init-erc ()
  (progn
    (add-hook 'erc-text-matched-hook 'my-erc-hook)
    (spaceline-toggle-erc-track-off)))

(defun ztlevi-misc/init-wrap-region ()
  (use-package wrap-region
    :defer t
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
      (add-to-list 'wrap-region-except-modes 'web-mode))
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

    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-call)
    (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial-or-done)
    (define-key ivy-minibuffer-map (kbd "C-<return>") 'ivy-immediate-done)
    (define-key ivy-minibuffer-map (kbd "C-c C-e") 'spacemacs//counsel-edit)
    ))

(defun ztlevi-misc/post-init-swiper ()
  (define-key global-map (kbd "C-s") 'my-swiper-search))

(defun ztlevi-misc/post-init-magit ()
  (progn
    (with-eval-after-load 'magit
      ;; set magit display buffer way, need to be eval after magit
      (setq magit-display-buffer-function (quote magit-display-buffer-same-window-except-diff-v1))

      (add-to-list 'magit-no-confirm 'stage-all-changes)
      (setq magit-completing-read-function 'magit-builtin-completing-read)

      (magit-define-popup-switch 'magit-push-popup ?u
        "Set upstream" "--set-upstream"))

    ;; prefer two way ediff
    (setq magit-ediff-dwim-show-on-hunks t)

    (setq magit-push-always-verify nil)

    (setq magit-process-popup-time 10)))

(defun ztlevi-misc/post-init-browse-at-remote ()
  (add-to-list 'browse-at-remote-remote-type-domains '("isl-122-ubuntu" . "gitlab")))

(defun ztlevi-misc/post-init-git-messenger ()
  (use-package git-messenger
    :defer t
    :config
    (define-key git-messenger-map (kbd "o") 'ztlevi/github-browse-commit)
    (setq git-messenger:func-prompt (cons '(ztlevi/github-browse-commit . "Browse online") git-messenger:func-prompt ))))

(defun ztlevi-misc/init-github-browse-file ()
  (use-package github-browse-file
    :defer t
    :commands (github-browse-file--relative-url)))

(defun ztlevi-misc/init-edit-indirect ()
  (use-package edit-indirect
    :defer t))

(defun ztlevi-misc/init-live-server ()
  (use-package live-server
    :defer t
    :commands (live-server-preview)))

(defun ztlevi-misc/post-init-markdown-mode ()
  (progn
    (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

    (setq markdown-italic-underscore t)

    ;; set markdown inline iamge max size
    (setq markdown-max-image-size '(800 . 800))

    ;; define markdown keys
    (with-eval-after-load 'markdown-mode
      (progn
        ;; (when (configuration-layer/package-usedp 'company)
        ;;   (spacemacs|add-company-hook markdown-mode))
        (dolist (mode markdown--key-bindings-modes)
          (spacemacs/set-leader-keys-for-major-mode mode
            "o" 'markdown-follow-link-at-point)
          (spacemacs/set-leader-keys-for-major-mode mode
            "D" 'my-flymd-delete-tmp-file)
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

        ;; bind key for edit code block
        (define-key markdown-mode-map (kbd "C-c '") 'markdown-edit-code-block)
        ))
    ))
