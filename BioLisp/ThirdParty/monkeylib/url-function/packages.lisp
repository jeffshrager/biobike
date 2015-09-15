;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :cl-user)

(defpackage :com.gigamonkeys.url-function
  (:use :common-lisp 
        :net.aserve 
        :com.gigamonkeys.foo
        :com.gigamonkeys.macro-utilities)
  (:export :define-url-function
	   :define-url-function/no-publish
	   :define-redirect
	   :define-redirect/no-publish
           :string->type))

