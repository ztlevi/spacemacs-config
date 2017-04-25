;; ctags setting, need to be implemented for personal useage
;; (defun my-setup-develop-environment ()
;;   (interactive)
;;   (when (my-project-name-contains-substring "ztlevi")
;;     (cond
;;      ((my-project-name-contains-substring "cocos2d-x")
;;       (setq tags-table-list (list (my-create-tags-if-needed "~/cocos2d-x/cocos"))))
;;      ((my-project-name-contains-substring "Leetcode/Code/Java")
;;       (message "load tags for Leetcode Java repo...")
;;       (setq tags-table-list (list (my-create-tags-if-needed "~/Developer/Leetcode/Code/Java"))))
;;      )))

(defun ztlevi-programming/post-init-js-doc ()
  (setq js-doc-mail-address "ztlevi1993@gmail.com"
        js-doc-author (format "Ting Zhou <%s>" js-doc-mail-address)
        js-doc-url "http://ztlevi.wordpress.com"
        js-doc-license "MIT"))

;; Environment setting - Ctags here
(defun my-setup-develop-environment ()
  (interactive)
  (when (my-project-name-contains-substring "ztlevi")
    (cond
     ((my-project-name-contains-substring "Sites/TRIdemo")
      (message "load tags for TRIdemo...")
      (setq tags-table-list
            (list (my-create-tags-if-needed "/Users/ztlevi/Sites/TRIdemo/app"))))
     ((my-project-name-contains-substring "Leetcode/Code/Java")
      (message "load tags for Leetcode Java repo...")
      (setq tags-table-list (list (my-create-tags-if-needed "~/Developer/Leetcode/Code/Java"))))
     )))
