;; =============================================================================
;; ztlevi's org settings here
;; =============================================================================

;; set org notes dir
(setq-default
 org-agenda-dir "~/Developer/Org-notes"
 deft-dir "~/Developer/Org-notes"
 blog-admin-dir "~/Developer/Wordpress")
(setq org-blog-dir blog-admin-dir)

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

;; ===================Wordpress Org2Blog setting start====================
(setq load-path (cons "~/.spacemacs.d/layers/org2blog/" load-path))
(use-package org2blog-autoloads)
(use-package auth-source) ;; or nothing if already in the load-path
(setq org2blog/wp-blog-alist
      '(("my-blog"
         :url "https://ztlevi.wordpress.com/xmlrpc.php"
         :username "ztlevi")))
(let (credentials)
  ;; only required if your auth file is not already in the list of auth-sources
  (add-to-list 'auth-sources "~/.netrc")
  (setq credentials (auth-source-user-and-password "wp-ztlevi"))
  (setq org2blog/wp-blog-alist
        `(("my-blog"
           :url "https://ztlevi.wordpress.com/xmlrpc.php"
           :username ,(car credentials)
           :password ,(cadr credentials)))))
;; ===================Wordpress Org2Blog setting end======================

;; setting to solve the non-ascii problem
;; found at http://blog.somof.net/?p=1310
(advice-add 'url-http-create-request :override
            'url-http-create-request-debug)
(defun url-http-create-request-debug (&optional ref-url)
  "Create an HTTP request for <code>url-http-target-url', referred to by REF-URL."
  (let* ((extra-headers)
         (request nil)
         (no-cache (cdr-safe (assoc "Pragma" url-http-extra-headers)))
         (using-proxy url-http-proxy)
         (proxy-auth (if (or (cdr-safe (assoc "Proxy-Authorization"
                                              url-http-extra-headers))
                             (not using-proxy))
                         nil
                       (let ((url-basic-auth-storage
                              'url-http-proxy-basic-auth-storage))
                         (url-get-authentication url-http-proxy nil 'any nil))))
         (real-fname (url-filename url-http-target-url))
         (host (url-http--encode-string (url-host url-http-target-url)))
         (auth (if (cdr-safe (assoc "Authorization" url-http-extra-headers))
                   nil
                 (url-get-authentication (or
                                          (and (boundp 'proxy-info)
                                               proxy-info)
                                          url-http-target-url) nil 'any nil))))
    (if (equal "" real-fname)
        (setq real-fname "/"))
    (setq no-cache (and no-cache (string-match "no-cache" no-cache)))
    (if auth
        (setq auth (concat "Authorization: " auth "\r\n")))
    (if proxy-auth
        (setq proxy-auth (concat "Proxy-Authorization: " proxy-auth "\r\n")))
 
    ;; Protection against stupid values in the referrer
    (if (and ref-url (stringp ref-url) (or (string= ref-url "file:nil")
                                           (string= ref-url "")))
        (setq ref-url nil))
 
    ;; We do not want to expose the referrer if the user is paranoid.
    (if (or (memq url-privacy-level '(low high paranoid))
            (and (listp url-privacy-level)
                 (memq 'lastloc url-privacy-level)))
        (setq ref-url nil))
 
    ;; url-http-extra-headers contains an assoc-list of
    ;; header/value pairs that we need to put into the request.
    (setq extra-headers (mapconcat
                         (lambda (x)
                           (concat (car x) ": " (cdr x)))
                         url-http-extra-headers "\r\n"))
    (if (not (equal extra-headers ""))
        (setq extra-headers (concat extra-headers "\r\n")))
 
    ;; This was done with a call to </code>format'.  Concatenating parts has
    ;; the advantage of keeping the parts of each header together and
    ;; allows us to elide null lines directly, at the cost of making
    ;; the layout less clear.
    (setq request
          (concat
             ;; The request
             (or url-http-method "GET") " "
             (url-http--encode-string
              (if using-proxy (url-recreate-url url-http-target-url) real-fname))
             " HTTP/" url-http-version "\r\n"
             ;; Version of MIME we speak
             "MIME-Version: 1.0\r\n"
             ;; (maybe) Try to keep the connection open
             "Connection: " (if (or using-proxy
                                    (not url-http-attempt-keepalives))
                                "close" "keep-alive") "\r\n"
                                ;; HTTP extensions we support
             (if url-extensions-header
                 (format
                  "Extension: %s\r\n" url-extensions-header))
             ;; Who we want to talk to
             (if (/= (url-port url-http-target-url)
                     (url-scheme-get-property
                      (url-type url-http-target-url) 'default-port))
                 (format
                  "Host: %s:%d\r\n" host (url-port url-http-target-url))
               (format "Host: %s\r\n" host))
             ;; Who its from
             (if url-personal-mail-address
                 (concat
                  "From: " url-personal-mail-address "\r\n"))
             ;; Encodings we understand
             (if (or url-mime-encoding-string
                     ;; MS-Windows loads zlib dynamically, so recheck
                     ;; in case they made it available since
                     ;; initialization in url-vars.el.
                     (and (eq 'system-type 'windows-nt)
                          (fboundp 'zlib-available-p)
                          (zlib-available-p)
                          (setq url-mime-encoding-string "gzip")))
                 (concat
                  "Accept-encoding: " url-mime-encoding-string "\r\n"))
             (if url-mime-charset-string
                 (concat
                  "Accept-charset: "
                  (url-http--encode-string url-mime-charset-string)
                  "\r\n"))
             ;; Languages we understand
             (if url-mime-language-string
                 (concat
                  "Accept-language: " url-mime-language-string "\r\n"))
             ;; Types we understand
             "Accept: " (or url-mime-accept-string "*/*") "\r\n"
             ;; User agent
             (url-http-user-agent-string)
             ;; Proxy Authorization
             proxy-auth
             ;; Authorization
             auth
             ;; Cookies
             (when (url-use-cookies url-http-target-url)
               (url-http--encode-string
                (url-cookie-generate-header-lines
                 host real-fname
                 (equal "https" (url-type url-http-target-url)))))
             ;; If-modified-since
             (if (and (not no-cache)
                      (member url-http-method '("GET" nil)))
                 (let ((tm (url-is-cached url-http-target-url)))
                   (if tm
                       (concat "If-modified-since: "
                               (url-get-normalized-date tm) "\r\n"))))
             ;; Whence we came
             (if ref-url (concat
                          "Referer: " ref-url "\r\n"))
             extra-headers
             ;; Length of data
             (if url-http-data
                 (concat
                  "Content-length: " (number-to-string
                                      (length url-http-data))
                  "\r\n"))
             ;; End request
             "\r\n"
             ;; Any data
             url-http-data))
    ;; Bug#23750
    ;;(unless (= (string-bytes request)
    ;;           (length request))
    ;;  (message "   text byte %d vs %d length" (string-bytes request) (length request)))
      ;;(message "===============================")
      ;;(error "Multibyte text in HTTP request: %s" request))
    (url-http-debug "Request is: \n%s" request)
    request))
