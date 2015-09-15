(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :aserve)
  (require :process)
  (require :sax))

(defpackage :com.gigamonkeys.ajax
  (:nicknames :cg-ajax)
  (:use :cl
	:com.gigamonkeys.foo
	:com.gigamonkeys.pathnames
	:com.gigamonkeys.utilities
	:com.gigamonkeys.twas
	:com.gigamonkeys.test
	:net.aserve
	:net.uri)
  (:shadow :sequence)
  (:export 
   :register-message-handler
   :send
   :start-ajax-server
   :stop-ajax-server
   :*ajax-directories*
   :*regenerate-if-newer*
   ))

(defpackage :com.gigamonkeys.ajax.json
  (:use :cl :com.gigamonkeys.foo :com.gigamonkeys.foo.javascript)
  (:import-from :com.gigamonkeys.foo.javascript 
		:new-env
		:statement-or-expression
		:top-level-environment)
  (:import-from :com.gigamonkeys.foo.text-output :new-pretty-printer)
  (:export :array :object :json))


(defpackage :com.gigamonkeys.ajax.json-parser
  (:use :cl :com.gigamonkeys.parser)
  (:shadow :string :exp))