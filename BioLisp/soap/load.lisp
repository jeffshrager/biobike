;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :cl-user)


(defparameter *soap-files* 
  '(
    "interface-tools"
    "kegg-goo"
    ))


(load-system* "websrc:soap;" *soap-files*)

(when (fboundp 'provides) (funcall 'provides :soap-tools))


