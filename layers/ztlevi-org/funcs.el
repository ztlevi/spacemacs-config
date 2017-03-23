;; occur non ascii, used to check non-ascii in Wordpress
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (deactivate-mark)
  (occur "[^[:ascii:]]"))
