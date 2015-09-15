;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

;;; Copyright (c) 2002-2003 by Jeff Shrager, Mike Travers, JP Massar.
;;; All rights reserved.

(in-package :bio)

;;; This software is made avilable for EDUCATIONAL PURPOSES
;;; ONLY, and WITHOUT ANY WARRANTY, express or implied, of
;;; its merchantability or fitness for a particular purpose. 


(defun do-xml-blast-sequence ()
  (blast-sequence 
   "tccagataatatagtgaaagacacagatgtactagcactattccgtgtatctcctcaaccaggagtaga"))

;;; XML code by Mike Travers:

(defun get-blast-xml (rid)
  (web-page-contents
   (one-string
    "http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?"
    "CMD=get"
    "&FORMAT_TYPE=XML"
    "&RID="  (symbol-name rid))))

;;; Utilities for navigating an LXML structure

(defun lxml-subnode (lxml subnode-type)
  (dolist (sub lxml)
    (if (and (listp sub) (eq (car sub) subnode-type))
	(return sub))))

(defun lxml-subnodes (lxml subnode-type)
  (let ((result nil))
    (dolist (sub lxml (nreverse result))
      (if (and (listp sub)
        (eq (car sub) subnode-type))
   (push sub result)))))

;; Go down several levels
(defun lxml-descend (lxml &rest subnode-types)
  (if (null subnode-types)
      lxml
    (apply #'lxml-descend 
	   (lxml-subnode lxml (car subnode-types)) 
	   (cdr subnode-types))))


;;; JP Massar.  Simple parser to convert CLOCC's CLLIB XML format into
;;; something resembling LXML, at least sufficiently for the
;;; example to work.


;;; Hack alert.  Prevent the necessity of having the CLLIB package
;;; defined when we compile and load this file.  Also done around
;;; XML-STRING-TO-CLXML below.

(defun cllib-symbol (s) (intern (symbol-name s) (find-package :cllib)))

(let ((xml-obj-symbol nil))
  (defun clxml-obj? (x) 
    (when (null xml-obj-symbol) 
      (setq xml-obj-symbol (cllib-symbol :xml-obj)))
    (and (listp x) (eq (first x) xml-obj-symbol))
    ))

;;; These two functions work on XML-OBJ lists, which are conversions
;;; from the CLLIB::XML-OBJ structure into a list, done using the
;;; CLLIB::XML-OBJ-TO-LISP-FORM function.

(defun clxml-obj-data-values (xml-obj) (second (fourth xml-obj)))

(defun clxml-obj-name-string (xml-obj)
  (let* ((name (second xml-obj))
	 (xml-name (second name))
	 (local-name (second xml-name))
	 (local-name-string (second local-name)))
    local-name-string
    ))


;;; Toplevel routine XML-STRING-TO-CLXML which takes a string of XML 
;;; and converts it into LXML in three stages:
;;; 1. the string gets converted in CLOCC CLLIB XML format
;;; 2. That format gets converted into CXML format, which basically
;;;    changes the DEFSTRUCT structures into list structure.
;;; 3. CXML gets parsed into a very simple LXML structure, which
;;;    throws away a lot of the information in CXML.


(defparameter *xml-temp-dir*
    (namestring
     (make-pathname
      :host (pathname-host *load-truename*)
      :device (pathname-device *load-truename*)
      :directory (pathname-directory *load-truename*)
      :name nil :type nil :version nil)))

(let ((xml-read-from-file-symbol nil)
      (xml-obj-to-lisp-form-symbol nil))
  (defun xml-string-to-clxml 
      (xml-string &optional (name-package (find-package :cl-user)))
    (when (null xml-read-from-file-symbol)
      (setq xml-read-from-file-symbol (cllib-symbol :xml-read-from-file)))
    (when (null xml-obj-to-lisp-form-symbol)
      (setq xml-obj-to-lisp-form-symbol (cllib-symbol :xml-obj-to-lisp-form)))
    ;; I can't figure out how to use the damned CLLIB XML package to read
    ;; the XML directly from a string, or string stream.  But this works...
    (let ((file (concatenate 'string *xml-temp-dir* "temp.xml")))
      (with-open-file (f file :direction :output :if-exists :supersede)
	(with-standard-io-syntax (format f "~A" xml-string)))
      (let* ((clxml (funcall xml-read-from-file-symbol file))
	     (clxml (funcall xml-obj-to-lisp-form-symbol clxml)))
	(delete-file file)
	(if (clxml-obj? clxml)
	    (simple-clxml-parse-obj clxml name-package)
	  (mapcar #'(lambda (x) (simple-clxml-parse-obj x name-package))
		  (remove-if-not #'clxml-obj? clxml)
		  ))))))

(defun simple-clxml-parse-obj (xml-obj name-package)
  (let ((name (clxml-obj-name-string xml-obj))
	(data (clxml-obj-data-values xml-obj)))
    (cons (intern name name-package) 
	  (simple-clxml-data-values-parse data name-package))))

(defun simple-clxml-data-values-parse (data-list name-package)
  (flet ((one-space-string? (x) (and (stringp x) (string= x " "))))
    (if (and (= (length data-list) 1) (one-space-string? (first data-list)))
	data-list
      (let ((despaced-list (remove-if #'one-space-string? data-list)))
	(mapcan
	 #'(lambda (datum)
	     (cond
	      ((stringp datum) (list datum))
	      ((clxml-obj? datum) 
	       (list (simple-clxml-parse-obj datum name-package)))
	      (t
	       (warn "Found ~S in XML-OBJ data.  It will be ignored." datum)
	       nil
	       )))
	 despaced-list
	 )))))

;;; Now back to your regularly scheduled tutorial.

;;; This produces something like my original extract-blast-hits:

(defun extract-blast-hits-from-xml (xml)
  (let ((hits (lxml-subnodes
	       (lxml-descend xml '|BlastOutput| '|BlastOutput_iterations|
			     '|Iteration| '|Iteration_hits|) 
	       '|Hit|)))
    (mapcar 
     #'(lambda (hit)
	 (list 
	  (lxml-subnode hit '|Hit_id|)
	  (lxml-subnode hit '|Hit_def|)
	  (let ((hsps (lxml-subnodes (lxml-subnode hit '|Hit_hsps|)
				     '|Hsp|)))
	    (mapcar 
	     #'(lambda (hsp)
		 (list 
		  (lxml-subnode hsp '|Hsp_evalue|) 
		  (lxml-subnode hsp '|Hsp_hit-from|) 
		  (lxml-subnode hsp '|Hsp_hit-to|) ))
	     hsps))))
     hits)))

;;; New version, via XML:

(defun blast-sequence-xml 
    (sequence &key (db "nr") (expect 0.00001) (program "blastn"))
  (submit-blast-query-xml 
   (one-string
    "QUERY=" 
    sequence
    "&DATABASE=" 
    db
    "&HITLIST_SIZE=10"
    "&FORMAT_TYPE=HTML"
    "&FILTER=L"
    "&EXPECT=" 
    (format nil "~A" expect)
    "&PROGRAM=" 
    program
    "&CLIENT=web"
    "&SERVICE=plain"
    "&NCBI_GI=on"
    "&PAGE=Nucleotides"
    "&CMD=Put")))


(defun submit-blast-query-xml (query)
  ;; Submit the query and parse-out the RID from the reply.
  (let ((rid (assocadr 
	      'rid 
	      (extract-blast-info (web-page-contents (blast-query query))))))
    (format t (one-string "The query has been assigned RID ~a... "
			  "Waiting for it to become ready!") rid)
    ;; Wait for ready, sleeping a little each time so as not to overload NCBI!
    (loop until 
	  (not (string-equal 
		"WAITING" 
		(assocadr 
		 'status 
		 (extract-blast-info 
		  (web-page-contents 
		   (blast-query 
		    (format nil "CMD=Get&RID=~a&ALIGNMENTS=0" rid)))))))
	    do (format t "#") (sleep 5))
    ;; Okay, should be ready; Let's get the results.
    (extract-blast-hits-from-xml
     (parse-xml 
      (web-page-contents 
       (blast-query 
	(format nil "CMD=Get&RID=~a&ALIGNMENTS=0&FORMAT_TYPE=XML" rid)))))))




#|

;;; This code uses the results of the portable XML parser found
;;; in CLOCC's CLLIB libary

;;; We are given a a list of listified XML data as per
;;; our function CLLIB::XML-OBJ-TO-LISP-FORM.

;;; Find an XML-OBJ in the input list whose name is the first SUBNODE-NAME.
;;; Then look over the data in that XML-OBJ's DATA slot for other
;;; XML-OBJ's with names on the SUBNODE-NAMES list.
;;; When we've found the innermost XML-OBJ corresponding to the
;;; last SUBNODE-NAME return it.


(defun clxml-descend (xml &rest subnode-names)
  (block exit
    (when (or (null xml) (null subnode-names)) (return-from exit nil))
    (let ((subnode (clxml-subnode xml (car subnode-names))))
      (when subnode
	(when (null (cdr subnode-names)) (return-from exit subnode))
	(let* ((obj-data (clxml-obj-data-values subnode))
	       (result (apply #'clxml-descend obj-data (cdr subnode-names))))
	  result
	  )))))

;;; Find an XML-OBJ in the list of nodes whose name is SUBNODE-NAME

(defun clxml-subnode (xml subnode-name)
  (find-if #'(lambda (obj) (clxml-obj-named? obj subnode-name)) xml))

(defun clxml-subnodes (xml subnode-name)
  (remove-if-not #'(lambda (obj) (clxml-obj-named? obj subnode-name)) xml))

(defun clxml-obj-named? (node subnode-name)
  (and (listp node) 
       (eq (car node) 'cllib::xml-obj)
       (string= subnode-name (clxml-obj-name-string node))
       ))

(defun testd (lxml)
  ;; Get all the hits
  (let* ((iteration-hits-node
	  (clxml-descend 
	   Lxml
	   "BlastOutput" "BlastOutput_iterations" 
	   "Iteration" "Iteration_hits"))
	 (hits
	  (clxml-subnodes (clxml-obj-data-values iteration-hits-node) "Hit")))
    (print (list 'nhits (length hits)))
    (mapcar
     #'(lambda (hit)
	 (let* ((hit-data-nodes (clxml-obj-data-values hit))
		(hit-id-node (clxml-subnode hit-data-nodes "Hit_id"))
		(hit-def-node (clxml-subnode hit-data-nodes "Hit_def"))
		(hit-hsps-node (clxml-subnode hit-data-nodes "Hit_hsps"))
		(hit-hsps-data-nodes (clxml-obj-data-values hit-hsps-node))
		(hsps (clxml-subnodes hit-hsps-data-nodes "Hsp"))
		)
	   (print (list 'nhsps (length hsps)))
	   (list 
	    hit-id-node
	    hit-def-node
	    (mapcar
	     #'(lambda (hsp)
		 (let ((hsp-data-nodes (clxml-obj-data-values hsp)))
		   (list
		    (clxml-subnode hsp-data-nodes "Hsp_evalue")
		    (clxml-subnode hsp-data-nodes "Hsp_hit-from")
		    (clxml-subnode hsp-data-nodes "Hsp_hit-to")
		    )))
	     hsps
	     ))))
     hits
     )))

|#
