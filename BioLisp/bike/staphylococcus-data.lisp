;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE :bbi)

(defmethod vpl-data-menu-variables 
           ((instance cl-user:biolingua)
            (organism-descriptor (eql :staphylococcus)))
  (append 
   *common-vpl-data-menu-variables* 
   ;; put any variables specific to staphylococcus in a quoted list here 
   nil
   ))

(defmethod organism-subset-variables ((descriptor (eql :staphylococcus)))
  (copy-list
   '(*all-staphylococci* *all-staphylococcus-aureus*    
     )))

(define-symbol-macro *all-staphylococci* (biolisp::loaded-organisms))

(define-symbol-macro *all-staphylococcus-aureus* 
    (compute-staphyloccocus-aureus))

(defun compute-staphyloccocus-aureus ()
  (organism-symbol-values 
    '(:NC_002951 :NC_009632 :NC_009487 :NC_002952 :NC_002953
      :NC_003923 :NC_002758 :NC_002745 :NC_007795 :NC_007793
      :NC_010079 :NC_009641 :NC_004461)))




