;;
;; Copyright (c) 2006, Gigamonkeys Consulting All rights reserved.
;;

(defsystem com.gigamonkeys.asdf-extensions
  :name "com.gigamonkeys.asdf-extensions"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Extensions to ASDF."
  :components
  ((:file "packages")
   (:file "asdf-extensions" :depends-on ("packages"))
   (:file "concat-fasls" :depends-on ("packages")))
  :depends-on ())


