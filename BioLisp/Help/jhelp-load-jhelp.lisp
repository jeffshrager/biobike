;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

;;  May 29 '12  Strict JPages don't support extra help URLs yet, it seems.



(in-package :help)

;; NOTE LOADING THE VPL SYMBOLS INTO JHELP IS CURRENTLY DONE IN 
;; jhelp-handle-box-wksp-req.lisp.

(defun jhelp-load-jhelp ()
  (format t "~%===============================================~%")
  (format t "============== Loading JHelp ==================~%")
    (format   t "~&jpages: ") (finish-output nil)  ;;Flushes standard output.
  (loop for parensDoc in *jpages* 
        for doc = (make-JPage
                   :title        (first parensDoc)
                   :subtitle     (second parensDoc)
                   :keywords     (third parensDoc)
                   :logical-form (fourth parensDoc)
                   :summary      (fifth parensDoc)
                   :text         (sixth parensDoc)
                   )
        do
	(Enter-document-Into-JHelp   
         doc  'jpage
         `',(format nil "/jpage/~a" (first parensDoc))  ;;This now needs to be eval'd at run-time, so we need to quote it like this.  May 29 '12.
         ;; URL.  Back end not really there yet, but when it is, this is where it will go to.
         ;;'(make-help-documentation-file-url :name (name doc))
         (JPage-title doc)   (JPage-subtitle  doc)
         (JPage-keywords doc)   (JPage-summary doc)   (JPage-text doc)  
         )
	(incf *jpage-count*)
        )
	
  (format t " ~a of ~a~%" *jpage-count* (length *jpages*))
  (format t "~&===============================================~%")

  )
