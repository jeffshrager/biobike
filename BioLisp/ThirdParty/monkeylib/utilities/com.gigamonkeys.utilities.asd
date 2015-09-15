;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(defpackage :com.gigamonkeys.utilities-system (:use :cl :asdf))
(in-package :com.gigamonkeys.utilities-system)

(defsystem com.gigamonkeys.utilities
  :components
  ((:file "packages")
   (:file "heap" :depends-on ("packages"))
   (:file "files" :depends-on ("packages"))
   (:file "randomization" :depends-on ("packages"))
   #+allegro(:file "timer" :depends-on ("packages"))
   (:file "with-time" :depends-on ("packages")))
  :depends-on (:com.gigamonkeys.macro-utilities))