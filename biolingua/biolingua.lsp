;;; Top level loader for biolingua; Assumes that biolisp is available in the same directory.

;;; Available from ftp://ftp.franz.com/pub/misc/xmlutils/xmlutils.zip
(require :pxml)

;;; Available from http://prdownloads.sourceforge.net/allegroserve/aserve-1.2.1.tgz
(require :aserve)

(defpackage :biolisp (:use :user :net.aserve :excl :common-lisp))
(defpackage :biolingua (:use :biolisp :user :net.aserve :net.html.generator :excl :common-lisp))

(load (merge-pathnames "biolisp.lsp" *load-truename*))

(in-package :biolingua)

(defvar *biolingua-files*
  '(disco
    gokb
    pathway
    biotelligence
    mycogs
    ))

(dolist (file *biolingua-files*)
  (compile-file-if-needed (merge-pathnames (format nil "~a.lsp" file) *load-truename*))
  (load (merge-pathnames (format nil "~a.fasl" file) *load-truename*)))

(load (merge-pathnames "web-publish.lsp" *load-truename*))

