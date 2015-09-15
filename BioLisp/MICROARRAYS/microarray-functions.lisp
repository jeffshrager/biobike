;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi)

(DEFCONSTANT *microarrays* 
  (handler-case 
      (with-bbl-form
        (Load-microarray-experiments))
    (error 
     (c) 
     (formatt "*** Load microarray experiments failed!~%")
     (formatt "*** Actual error: ~A~%" c)
     nil
     )))
	
