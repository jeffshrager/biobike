;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE :bbi)

(defmethod vpl-data-menu-variables 
           ((instance cl-user:biolingua)
            (organism-descriptor (eql :no-organism)))
  (append 
   *common-vpl-data-menu-variables* 
   ;; put any variables specific to no-organism in a quoted list here 
   nil
   ))

(defmethod organism-subset-variables ((descriptor (eql :no-organism)))
  (copy-list
   '( )))


;;; Stuff specific to the :no-organism *organisms-descriptor* value




 