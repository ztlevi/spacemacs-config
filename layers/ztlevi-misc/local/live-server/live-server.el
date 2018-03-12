;;; live-server.el --- live server support for Emacs.

;; Copyright (C) 2018 Ting Zhou

;; Author: Ting Zhou <zhouting@umiche.edu>
;; Version: 1.0.0
;; Keywords: web, live
;; URL: https://github.com/ztlevi/emacs-live-server

;;; Commentary:

;; Realtime html previews for Emacs.

;;; Code:

(defgroup live-server nil
  "Realtime Markdown previews"
  :group 'live-server
  :prefix "live-server-")

(defcustom live-server-port 5050
  "Port on which live-server server will run."
  :type 'integer
  :group 'live-server)

;;;###autoload
(defun live-server-kill ()
  "Stops the live-server process."
  (interactive)
  ;; delete process if existed
  (let ((emacs-live-server-process-name (get-buffer-process (format "emacs-live-server-buffer"))))
    (if emacs-live-server-process-name
        (delete-process emacs-live-server-process-name))))

;;;###autoload
(defun live-server-preview ()
  "Preview the current file in live-server."
  (interactive)
  (live-server-kill)

  (start-process-shell-command
   (format "emacs-live-server")
   (format "emacs-live-server-buffer")
   (format "live-server %s --port=%s"
           buffer-file-name
           live-server-port))
  (print (format "%s serverd @ %s" buffer-file-name live-server-port) (get-buffer "emacs-live-server-buffer")))

(provide 'live-server)
;;; live-server.el ends here
