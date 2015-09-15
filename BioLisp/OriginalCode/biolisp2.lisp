;;; Copyright (c) 2002-2003 by Jeff Shrager and Mike Travers; All rights reserved.

;;; This software is made avilable for EDUCATIONAL PURPOSES
;;; ONLY, and WITHOUT ANY WARRANTY, express or implied, of
;;; its merchantability or fitness for a particular purpose. 

(in-package USER)
(setq *print-pretty* nil) ; Keep long lines from being wrapped around!
(import 'cg::web-page-contents)
(import 'cg::with-open-web-page)

;;; LSETQ gives the length instead of the value, and is good for
;;; setting long strings.

(defmacro lsetq (var value)
 `(length (setq ,var ,value)))

;;; Get the NCBI standard info out of a web page's contents, represented
;;; by a single long string.  Note that if there is more than one occurrance
;;; of "QBlastInfoBegin", etc., this will only get the first one.  
;;; Call it as:
;;;   (extract-blast-info the-web-page-contents)
;;; It returns this sort of thing:
;;;   ((STATUS "WAITING") (RID "1044584572-020243-29658"))

(defun extract-blast-info (web-page-contents)
  ;; Find the position of the surrounding markers (QBlastInfoBegin, etc.)
  (let* ((QBlastInfoBegin-position (search "QBlastInfoBegin" web-page-contents))
	 (QBlastInfoEnd-position (search "QBlastInfoEnd" web-page-contents)))
    ;; If either one is missing, skip it and retrun nothing!
    (when (and QBlastInfoBegin-position QBlastInfoEnd-position)
      ;; Extract the part between the markers, bind it to a stream
      ;; and read each line out of it, until you hit the end.
      (loop with stream = (make-string-input-stream (subseq web-page-contents QBlastInfoBegin-position
                                                            QBlastInfoEnd-position))
            as line = (read-line stream nil nil)
            with result = nil ; This will collect the resulting parsed values.
            until (null line)
            ;; This turns this: "<TAB>LABEL=VALUE" to this: (LABEL "VALUE")
            do (let ((=pos (position #\= line)))
                 (when =pos (push (list (read-from-string (subseq line 1 =pos))
                                        (string-trim " " (subseq line (1+ =pos))))
                                  result)))
            finally (return result)
            ))))

(defun broket-positions (string)
  (loop for current-position from 0 by 1
	with <-pos = nil
	as char across string
	when (char-equal #\< char)
	do (setq <-pos current-position)
	when (char-equal #\> char)
	collect (list <-pos current-position)))

;;; The very small e-values get converted to 0.0 by read-from-string, so
;;; we have our own (mantissa exponent) notation.

(defun read-e-value (s)
  (let ((e-pos (position #\e s))) ; look for a exponential (scientific) form.
    (cond ((null e-pos) ; no e... just read the number straight!
	   (list (read-from-string s) 0.0)) ; Return the "weird" form
	  (t ; otherwise we have to produce the fancy parse...
	   (list (read-from-string (subseq s 0 e-pos))
		 (read-from-string (subseq s (1+ e-pos))))))))

(defun get-e-value (s)
  (read-e-value (subseq s (1+ (second (first (last (broket-positions s)))))))
  )

(defun parse-blast-match (s)
  (let* ((brokets (broket-positions s))
	 (e-value (read-e-value (subseq s (1+ (second (first (last brokets)))))))
	 (accession-start (+ 1 (second (first brokets))))
	 (accession-end (first (second brokets)))
	 (accession (subseq s accession-start accession-end))
	 (match-start (+ 1 (second (second brokets))))
	 (match-end (first (third brokets)))
	 (match (subseq s match-start match-end))
	 )
    (list 
     (list 'accession accession)
     (list 'match match)
     (list 'e-value e-value)
     )))

#| This is the version that you pass the web page contents into.  It is
   superseded by the new one, which you just give a URL.

(defun extract-blast-hits (web-page-contents)
  (let ((stream (make-string-input-stream web-page-contents)))
    ;; Skip until we find the target line: "Sequences producing significant alignments:"
  (loop as line = (read-line stream nil nil)
	until (or (null line)
		  (and (> (length line) 43)
		       (string-equal (subseq line 0 43) "Sequences producing significant alignments:")))
	)
  ;; Okay, now skip one (the blank line!) -- stop here if we hit the end of the file!
  (when (read-line stream nil nil)
  ;; And start parsing until we hit the </pre> line:
	(loop as line = (read-line stream nil nil)
	      until (string-equal "</PRE>" line)
	      collect (parse-blast-match line)
	      ))))

|#

(defun extract-blast-hits (url)
  (CG::with-open-web-page (stream url)
    ;; Skip until we find the target line: "Sequences producing significant alignments:"
    (loop as line = (read-line stream nil nil)
	  until (or (null line)
		  (and (> (length line) 43)
		         (string-equal (subseq line 0 43) "Sequences producing significant alignments:")))
	)		 
  ;; Okay, now skip one (the blank line!) -- stop here if we hit the end of the file!
  (when (read-line stream nil nil)
  ;; And start parsing until we hit the </pre> line:
	(loop as line = (read-line stream nil nil)
	      until (string-equal "</PRE>" line)
	      collect (parse-blast-match line)
	      ))))

;;; First a couple of little helping functions.

;;; This just makes a string with the url on the front for convenience.
(defun blast-query (query)
  (format nil "http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?~a" query))

;;; This is a shorthand for going through alists.  (If we were being
;;; real Lisp engineers, we'd use object or defstructs, instead of alists
;;; but....
(defmacro assocadr (key alist)
  `(cadr (assoc ,key ,alist)))

;;; This does the real work, submitting the query, getting the RID, waiting for
;;; ready from NCBI and then getting the results!  

(defun submit-blast-query (query)
  ;; Submit the query and parse-out the RID from the reply.
  (let ((rid (assocadr 'rid (extract-blast-info (cg::web-page-contents (blast-query query))))))
    (format t "The query has been assigned RID ~a... Waiting for it to become ready!" rid)
    ;; Wait for ready, sleeping a little eeach time so as not to overload NCBI!
    (loop until (not (string-equal "WAITING" 
				      (assocadr 'status (extract-blast-info (cg::web-page-contents (blast-query (format nil "CMD=Get&RID=~a&ALIGNMENTS=0" rid)))))))
	    do (format t "#") 
	      (sleep 5))
    ;; Okay, should be ready; Let's get the results.
    (extract-blast-hits (blast-query (format nil "CMD=Get&RID=~a&ALIGNMENTS=0" rid)))))

(defun blast-sequence (sequence &key (db "nr") (expect 0.00001) (program "blastn"))
  (submit-blast-query (format nil "QUERY=~a&DATABASE=~a&HITLIST_SIZE=10&FORMAT_TYPE=HTML&FILTER=L&EXPECT=~a&PROGRAM=~a&CLIENT=web&SERVICE=plain&NCBI_GI=on&PAGE=Nucleotides&CMD=Put" 
                       sequence
                       db
                       expect
                       program
                       )))


;;; XML code by Mike Travers:

;;; Uses Franz-ACL specific calls:

(require "pxml")
(import 'NET.XML.PARSER::PARSE-XML)

(defun get-blast-xml (rid)
  (cg:web-page-contents
   (format nil "http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=get&FORMAT_TYPE=XML&RID=~A" rid)))

;;; Utilities for navigating an LXML structure
(defun lxml-subnode (lxml subnode-type)
  (dolist (sub lxml)
    (if (and (listp sub)
      (eq (car sub) subnode-type))
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
    (apply #'lxml-descend (lxml-subnode lxml (car subnode-types)) (cdr
    subnode-types))))

;;; This produces something like my original extract-blast-hits:

(defun extract-blast-hits-from-xml (xml)
  (let ((hits (lxml-subnodes
        (lxml-descend xml '|BlastOutput| '|BlastOutput_iterations|
        '|Iteration| '|Iteration_hits|) '|Hit|)))
    (mapcar #'(lambda (hit)
  (list (lxml-subnode hit '|Hit_id|)
        (lxml-subnode hit '|Hit_def|)
        (let ((hsps (lxml-subnodes (lxml-subnode hit '|Hit_hsps|)
        '|Hsp|)))
   (mapcar #'(lambda (hsp)
        (list 
         (lxml-subnode hsp '|Hsp_evalue|) 
         (lxml-subnode hsp '|Hsp_hit-from|) 
         (lxml-subnode hsp '|Hsp_hit-to|) ))
    hsps))))
     hits)))

;;; New version, via XML:

(defun blast-sequence-xml (sequence &key (db "nr") (expect 0.00001) (program "blastn"))
  (submit-blast-query-xml (format nil "QUERY=~a&DATABASE=~a&HITLIST_SIZE=10&FORMAT_TYPE=HTML&FILTER=L&EXPECT=~a&PROGRAM=~a&CLIENT=web&SERVICE=plain&NCBI_GI=on&PAGE=Nucleotides&CMD=Put" 
                       sequence
                       db
                       expect
                       program
                       )))

(defun submit-blast-query-xml (query)
  ;; Submit the query and parse-out the RID from the reply.
  (let ((rid (assocadr 'rid (extract-blast-info (cg::web-page-contents (blast-query query))))))
    (format t "The query has been assigned RID ~a... Waiting for it to become ready!" rid)
    ;; Wait for ready, sleeping a little eeach time so as not to overload NCBI!
    (loop until (not (string-equal "WAITING" 
				      (assocadr 'status (extract-blast-info (cg::web-page-contents (blast-query (format nil "CMD=Get&RID=~a&ALIGNMENTS=0" rid)))))))
	    do (format t "#") 
	      (sleep 5))
    ;; Okay, should be ready; Let's get the results.
    (extract-blast-hits-from-xml
     (parse-xml (cg::web-page-contents (blast-query (format nil "CMD=Get&RID=~a&ALIGNMENTS=0&FORMAT_TYPE=XML" rid)))))))
