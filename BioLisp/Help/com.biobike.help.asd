;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(defpackage :com.biobike.help-system (:use :asdf :cl))
(in-package :com.biobike.help-system)

(defsystem com.biobike.help
  :name "com.biobike.help"
  :author "Peter Seibel <peter@gigamonkeys.com>, JP Massar"
  :components	
  ((:file "dummy"))
  :depends-on (
	       :com.gigamonkeys.foo
	       :com.gigamonkeys.twas
	       :com.gigamonkeys.pathnames
	       :split-sequence
	       :url-function
	       :com.gigamonkeys.macro-utilities 
	       :com.gigamonkeys.utilities))

