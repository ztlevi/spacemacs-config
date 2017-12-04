;;; packages.el --- ztlevi layer packages file for Spacemacs.
;;
;; Copyright (c) 2016-2017 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ztlevi-better-defaults-packages
  '(
    dired+
    (dired-mode :location built-in)
    (profiler :location built-in)
    (recentf :location built-in)
    ))

(defun ztlevi-better-defaults/post-init-recentf ()
  (progn
    (setq recentf-exclude
          '("COMMIT_MSG"
            "COMMIT_EDITMSG"
            "github.*txt$"
            "/tmp/"
            "/ssh:"
            "/sudo:"
            "/TAGS$"
            "/GTAGS$"
            "/GRAGS$"
            "/GPATH$"
            "\\.mkv$"
            "\\.mp[34]$"
            "\\.avi$"
            "\\.pdf$"
            "\\.sub$"
            "\\.srt$"
            "\\.ass$"
            ".*png$"))
    (setq recentf-max-saved-items 2048)))

(defun ztlevi-better-defaults/init-dired+ ()
  (use-package dired+
    :defer t
    :config (diredp-toggle-find-file-reuse-dir 1)))

(defun ztlevi-better-defaults/init-dired-mode ()
  (use-package dired-mode
    :defer t
    :init
    (progn
      (require 'dired-x)
      (require 'dired-aux)
      (setq insert-directory-program "gls" dired-use-ls-dired t)
      (setq dired-listing-switches "-aBhl  --group-directories-first")
      (setq dired-guess-shell-alist-user
            '(("\\.pdf\\'" "open")
              ("\\.docx\\'" "open")
              ("\\.\\(?:djvu\\|eps\\)\\'" "open")
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
              ("\\.\\(?:xcf\\)\\'" "open")
              ("\\.csv\\'" "open")
              ("\\.tex\\'" "open")
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
               "open")
              ("\\.\\(?:mp3\\|flac\\)\\'" "open")
              ("\\.html?\\'" "open")
              ("\\.md\\'" "open")))

      (setq dired-omit-files
            (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|\\.js\\.meta$\\|\\.meta$"))

      ;; always delete and copy recursively
      (setq dired-recursive-deletes 'always)
      (setq dired-recursive-copies 'always)

      (defun ora-ediff-files ()
        (interactive)
        (let ((files (dired-get-marked-files))
              (wnd (current-window-configuration)))
          (if (<= (length files) 2)
              (let ((file1 (car files))
                    (file2 (if (cdr files)
                               (cadr files)
                             (read-file-name
                              "file: "
                              (dired-dwim-target-directory)))))
                (if (file-newer-than-file-p file1 file2)
                    (ediff-files file2 file1)
                  (ediff-files file1 file2))
                (add-hook 'ediff-after-quit-hook-internal
                          (lambda ()
                            (setq ediff-after-quit-hook-internal nil)
                            (set-window-configuration wnd))))
            (error "no more than 2 files should be marked"))))

      (define-key dired-mode-map "E" 'ora-ediff-files)

      (defvar dired-filelist-cmd
        '(("vlc" "-L")))

      ;; FIXME: evilify dired mode will lead to startup warnings
      (define-key dired-mode-map "G" nil)
      (evilified-state-evilify-map dired-mode-map
        :mode dired-mode
        :bindings
        "j"         'vinegar/move-down
        "k"         'vinegar/move-up
        "h"         'dired-up-directory
        "l"         (if vinegar-reuse-dired-buffer
                        'dired-find-alternate-file
                      'dired-find-file)
        (kbd "C-w j") 'evil-window-down
        (kbd "C-w k") 'evil-window-up
        (kbd "C-w h") 'evil-window-left
        (kbd "C-w l") 'evil-window-right
        (kbd "RET") (if vinegar-reuse-dired-buffer
                        'dired-find-alternate-file
                      'dired-find-file)
        "f"         (if (configuration-layer/layer-used-p 'ivy)
                        'counsel-find-file
                      'helm-find-files)
        "J"         'ztlevi/open-file-with-projectile-or-counsel-git
        "I"         'vinegar/dotfiles-toggle
        (kbd "~")   '(lambda ()(interactive) (find-alternate-file "~/"))
        "e"         'dired-toggle-read-only
        (kbd "C-r") 'dired-do-redisplay
        "C"         'dired-do-copy
        "c"         'dired-copy-file-here
        "<mouse-2>" 'my-dired-find-file
        "`"         'dired-open-term
        "p"         'peep-dired-prev-file
        "n"         'peep-dired-next-file
        "v"         'dired-view-file
        "z"         'dired-get-size
        "gg"        'vinegar/back-to-top
        "G"         'vinegar/jump-to-bottom
        ")"         'dired-omit-mode)
      )))

(defun ztlevi-better-defaults/init-profiler ()
  (use-package profiler
    :init
    (evilified-state-evilify profiler-report-mode profiler-report-mode-map)))

;;; packages.el ends here
