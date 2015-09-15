;;
;; Copyright (c) 2005-2006, Gigamonkeys Consulting All rights reserved.
;;

(defsystem com.gigamonkeys.pathnames
  :name "com.gigamonkeys.pathnames"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Portable pathname manipulation functions."
  :long-description ""
  :components
  ((:file "packages")
   (:file "pathnames" :depends-on ("packages"))))


        
