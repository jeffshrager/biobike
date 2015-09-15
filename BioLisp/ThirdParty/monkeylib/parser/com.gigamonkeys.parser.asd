;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(defpackage :com.gigamonkeys.parser-system (:use :asdf :cl))
(in-package :com.gigamonkeys.parser-system)

(defsystem com.gigamonkeys.parser
  :name "com.gigamonkeys.parser"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :components
  ((:file "packages")
   (:file "parser" :depends-on ("packages")))
  :depends-on (:com.gigamonkeys.macro-utilities 
	       :com.gigamonkeys.utilities))

