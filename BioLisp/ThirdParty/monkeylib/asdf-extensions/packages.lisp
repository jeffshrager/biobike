(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(defpackage :com.gigamonkeys.asdf-extensions
  (:use :cl :asdf)
  (:export :register :include :register-and-include :build-one-fasl))

