;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(defpackage :com.gigamonkeys.foo-system (:use :asdf :cl))
(in-package :com.gigamonkeys.foo-system)

(defsystem com.gigamonkeys.foo
  :name "com.gigamonkeys.foo"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "FOO Outputs Output"
  :components
  ((:file "packages")
   (:file "text-output"      :depends-on ("packages"))
   (:file "language"         :depends-on ("packages" "text-output"))
   (:file "file-compiler"    :depends-on ("packages" "language" "text-output"))
   (:file "html"             :depends-on ("packages" "language" "text-output"))
   (:file "css"              :depends-on ("packages" "language" "html"))
   (:file "html-macros"      :depends-on ("packages" "html" "css"))
   (:file "javascript"       :depends-on ("packages" "language"))
   (:file "lispscript"       :depends-on ("packages" "language" "javascript" "html"))
   (:file "lispscript-tests" :depends-on ("packages" "lispscript" "html" "html-macros")))
  :depends-on (:com.gigamonkeys.macro-utilities  :com.gigamonkeys.utilities))

