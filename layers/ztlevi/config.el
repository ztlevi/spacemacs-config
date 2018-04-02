;;; config.el --- ztlevi layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2016-2017 ztlevi
;;
;; Author: ztlevi <zhouting@umich.edu>
;; URL: https://github.com/ztlevi/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar org-agenda-dir ""
  "gtd org files location")

(defvar deft-dir ""
  "deft org files locaiton")

(defvar blog-admin-dir ""
  "blog-admin files location")

(setq-default
 org-agenda-dir "~/Dropbox/Org-Notes"
 deft-dir "~/Dropbox/Org-Notes"
 blog-admin-dir "~/Developer/Github/hexo_blog")
