;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE :bbi)

(defmethod vpl-data-menu-variables 
           ((instance cl-user:biolingua)
            (organism-descriptor (eql :parasites)))
  (append 
   *common-vpl-data-menu-variables* 
   ;; put any variables specific to parasites in a quoted list here 
   nil
   ))

(defmethod organism-subset-variables ((descriptor (eql :parasites)))
  (copy-list
   '(*all-parasites*
     )))

(define-symbol-macro *all-parasites* (biolisp::loaded-organisms))