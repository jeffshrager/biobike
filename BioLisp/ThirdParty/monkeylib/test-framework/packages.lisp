;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :cl-user)

(defpackage :com.gigamonkeys.test
  (:use :common-lisp :com.gigamonkeys.macro-utilities)
  (:export :deftest :check :test :with-test-cases :test-package :clear-package-tests))

(defpackage :com.gigamonkeys.test-tests
  (:use :common-lisp :com.gigamonkeys.test))