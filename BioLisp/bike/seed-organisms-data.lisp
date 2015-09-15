;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi)

(defmethod vpl-data-menu-variables 
           ((instance cl-user:biolingua)
            (organism-descriptor (eql :seed-organisms)))
  (append 
   *common-vpl-data-menu-variables* 
   ;; put any variables specific to viruses in a quoted list here 
   ;; E.g., '(*frob* *virus-goo*) 
   ))
   
(defmethod organism-subset-variables ((descriptor (eql :seed-organisms)))
  (COPY-LIST
    '(*all-phage* *all-bacteria* *mycobacteriophage* *mycobacteria*)
  ))
  
(define-symbol-macro *all-phage* (compute-all-phage))

(define-symbol-macro *all-bacteria* (compute-all-bacteria))

(define-symbol-macro *mycobacteriophage* (compute-mycobacteriophage))

(define-symbol-macro *mycobacteria* (compute-mycobacteria))

(defun compute-all-phage ()
  (LOOP FOR organism IN *all-organisms*
        AS type = (REF organism #$real-domain)
        WHEN (EQUAL type "Bacteriophage")
            COLLECT organism)
    )
	
(defun compute-all-bacteria ()
  (LOOP FOR organism IN *all-organisms*
        AS type = (REF organism #$real-domain)
        WHEN (EQUAL type "Eubacteria")
            COLLECT organism)
    )
  
(defun compute-mycobacteriophage ()
  (LOOP FOR organism IN *all-phage*
        AS name = (REF organism #$fname)
        WHEN (SEARCH "ycobacterium-phage" name)
            COLLECT organism)
    )
	
(defun compute-mycobacteria ()
  (LOOP FOR organism IN *all-bacteria*
        AS name = (REF organism #$fname)
        WHEN (SEARCH "ycobacteri" name)
            COLLECT organism)
    )


