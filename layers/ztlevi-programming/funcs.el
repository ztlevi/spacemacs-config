;; ctags setting, need to be implemented for personal useage
(defun my-setup-develop-environment ()
  (interactive)
  (when (my-project-name-contains-substring "ztlevi")
    (cond
     ((my-project-name-contains-substring "cocos2d-x")
      (setq tags-table-list (list (my-create-tags-if-needed "~/cocos2d-x/cocos"))))
     ((my-project-name-contains-substring "Leetcode/Code/Java")
      (message "load tags for Leetcode Java repo...")
      (setq tags-table-list (list (my-create-tags-if-needed "~/Developer/Leetcode/Code/Java"))))
     )))
