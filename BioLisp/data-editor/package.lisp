;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :aserve))

(defpackage :data-editor
  (:nicknames :de)
  (:use :wlisp :utils :frames :net.html.generator :net.aserve)
  )

(in-package :de)

