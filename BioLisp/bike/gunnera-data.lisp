;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE :bbi)

(defmethod vpl-data-menu-variables 
           ((instance cl-user:biolingua)
            (organism-descriptor (eql :gunnera)))
  (append 
   *common-vpl-data-menu-variables* 
   ;; put any variables specific to staphylococcus in a quoted list here 
   nil
   ))

(defmethod organism-subset-variables ((descriptor (eql :gunnera)))
  (copy-list
   '(*all-gunnera*    
     )))

(define-symbol-macro *all-gunnera* (biolisp::loaded-organisms))

#|
(define-symbol-macro *all-gunnera* (compute-all-gunnera))

(defun compute-all-gunnera ()
  (organism-symbol-values 
    '(:gunnera_manicata_cdna_sanger :gunnera_manicata_cdna_454
	  :gunnera_manicata_cdna_illumina_without_primers
	  :gunnera_manicata_cdna_illumina_with_primers)))
|#



