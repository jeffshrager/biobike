;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sax))

#.(when (cl:find-package :swank) '(pushnew :swank *features*))

(defsystem com.gigamonkeys.ajax
  :name "com.gigamonkeys.ajax"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Lisp and Javascript infrastructure for Ajax."
  :components
  ((:file "packages")
   (:file "ajax-server"        :depends-on ("packages" "queue" "channel"))
   (:file "ajax-server-tests"  :depends-on ("packages" "ajax-server"))
   (:file "channel"            :depends-on ("packages"))
   (:file "json"               :depends-on ("packages"))
   (:file "json-builder"       :depends-on ("packages"))
   (:file "message-dispatcher" :depends-on ("packages"))
   (:file "queue"              :depends-on ("packages"))

   ;; AJAX applications
   (:file "ping"               :depends-on ("packages" "message-dispatcher"))
   (:file "repl"               :depends-on ("packages" "message-dispatcher"))
   (:file "json-test"          :depends-on ("packages" "message-dispatcher")))

  :depends-on 
  (:com.gigamonkeys.foo
   :com.gigamonkeys.pathnames
   :com.gigamonkeys.parser
   ;; #+swank :swank ;; not really necessary as we only use it if it's already loaded.
   :com.gigamonkeys.utilities
   :com.gigamonkeys.twas
   :test-framework))


