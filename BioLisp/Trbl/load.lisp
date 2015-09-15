;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :cl-user)

(defparameter *pb-files*
  '(
    "package"
    "utils"
    "converter"
    "lang"
    "pb"
    ;; "poset"
    "poset"
    "jpcode"
    "go-analysis"
    "jamesgen"
    ))

(load-system* "biol:Trbl;" *pb-files*)
