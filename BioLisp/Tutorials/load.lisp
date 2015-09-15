;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :cl-user)

(defparameter *biolisp-tutorial-code-files*
  '(
    "biolisp1"
    "biolisp2"
    "go"
    "anno1"
    "meta1"
    "array1"
    ;; go2.lisp 
    ;; go2.lisp is not loaded due to redefinition problems
    "cardisco"
    ;; arraydisco.lisp
    ;; arraydisco.lisp is not loaded due to redefinition problems
    ))

(load-system* "biol:Tutorials;" *biolisp-tutorial-code-files*)

(when (fboundp 'provides) (funcall 'provides :Tutorials))
