;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE :bbi)

(defmethod vpl-data-menu-variables 
           ((instance cl-user:biolingua)
            (organism-descriptor (eql :streptococcus)))
  (append 
   *common-vpl-data-menu-variables* 
   ;; put any variables specific to streptococcus in a quoted list here 
   nil
   ))

(defmethod organism-subset-variables ((descriptor (eql :streptococcus)))
  (copy-list
   '(*all-streptococci*     
     )))

(define-symbol-macro *all-streptococci* (biolisp::loaded-organisms))