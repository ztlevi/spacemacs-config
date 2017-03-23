
;;;### (autoloads nil "ox-wp" "ox-wp.el" (22726 531 0 0))
;;; Generated autoloads from ox-wp.el

(autoload 'org-wp-export-as-wordpress "ox-wp" "\
Export current buffer to a text buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

Export is done in a buffer named \"*Org WP Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP EXT-PLIST)" t nil)

;;;***

;;;### (autoloads nil nil ("org2blog-pkg.el") (22726 531 0 0))

;;;***

;;;### (autoloads nil "org2blog" "org2blog.el" (22726 531 0 0))
;;; Generated autoloads from org2blog.el

(autoload 'org2blog/wp-mode "org2blog" "\
Toggle org2blog/wp mode.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{org2blog/wp-entry-mode-map}

Entry to this mode calls the value of `org2blog/wp-mode-hook'.

\(fn &optional ARG)" t nil)

(autoload 'org2blog/wp-login "org2blog" "\
Logs into the blog. Initializes the internal data structures.

\(fn &optional BLOG-NAME)" t nil)

(autoload 'org2blog/wp-new-entry "org2blog" "\
Creates a new buffer for a blog entry.

\(fn)" t nil)

(autoload 'org2blog/wp-post-subtree "org2blog" "\
Post the current entry as a draft. Publish if PUBLISH is non-nil.

\(fn &optional PUBLISH)" t nil)

(autoload 'org2blog/wp-post-subtree-as-page "org2blog" "\
Post the current entry as a draft. Publish if PUBLISH is non-nil.

\(fn &optional PUBLISH)" t nil)

(autoload 'org2blog/wp-post-subtree-as-page-and-publish "org2blog" "\
Publish the current subtree as a page.

\(fn)" t nil)

(autoload 'org2blog/wp-track-buffer "org2blog" "\
Save details of current buffer in the tracking file.

\(fn)" t nil)

(autoload 'org2blog/wp-track-subtree "org2blog" "\
Save details of current subtree in the tracking file.

\(fn)" t nil)

(autoload 'org2blog/wp-preview-buffer-post "org2blog" "\
Preview the present buffer in browser, if posted.

\(fn)" t nil)

(autoload 'org2blog/wp-preview-subtree-post "org2blog" "\
Preview the present subtree in browser, if posted.

\(fn)" t nil)

;;;***

(provide 'org2blog-autoloads)

;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8
;; End:
