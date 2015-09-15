;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(defpackage :com.gigamonkeys.markup-system (:use :cl :asdf))
(in-package :com.gigamonkeys.markup-system)

(defsystem com.gigamonkeys.markup
  :components
  ((:file "packages")
   (:file "parameters" :depends-on ("packages"))
   (:file "parser" :depends-on ("packages" "parameters"))
   (:file "render" :depends-on ("packages" "parameters"))
   (:file "pdf"    :depends-on ("packages" "render"))
   (:file "html"   :depends-on ("packages" "render")))
  :depends-on (:cl-typesetting :com.gigamonkeys.foo :test-framework :com.gigamonkeys.pathnames))