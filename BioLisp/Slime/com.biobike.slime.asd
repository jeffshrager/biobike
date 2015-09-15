;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(defsystem com.biobike.slime
  :name "com.biobike.slime"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :components	
  ((:file "slime"))
  :depends-on (:swank))