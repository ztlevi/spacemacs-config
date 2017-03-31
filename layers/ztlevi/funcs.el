;; Layout
(defun ztlevi/load-my-layout ()
  (interactive)
  (persp-load-state-from-file (concat persp-save-dir "ztlevi")))

(defun ztlevi/save-my-layout ()
  (interactive)
  (persp-save-state-to-file (concat persp-save-dir "ztlevi")))

;; define replace-dos-eol
(defun ztlevi/replace-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "
")))
