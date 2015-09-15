;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

;; Copyright (c) 2002-2003 by Jeff Shrager, Mike Travers and JP Massar; 
;;; All rights reserved.

(in-package :bio)

;;; This software is made avilable for EDUCATIONAL PURPOSES
;;; ONLY, and WITHOUT ANY WARRANTY, express or implied, of
;;; its merchantability or fitness for a particular purpose. 

(defparameter *mystery-sequence* 
    (one-string
     "MYTAKGLASANVDVTLIDKRNFHLFQPLLYQVATGTLSPSDISAPLRSI"
     "LSKSKNTKVLLGEVKDVDTKTQEVILNDRVVRYDTLVVATGANHSYFGN"
     "DEWKEVAPGLKTIEDALEIRSRIFNAFEAAEKETDPQKRRALLTFVIVG"
     "GGPTGVELSGAIA"
     ))

(defparameter mystery-sequence-query-result nil)

(defun do-mystery-sequence-query ()
  (setq mystery-sequence-query-result
    (blast-sequence-xml *mystery-sequence* :program "blastp")))

(defun get-xml-blast-defns (hits)
   (loop for hit in hits
	 collect (assocadr '|Hit_def| hit)))

(defun all-ordered-sublists (set)
  (loop for n from 1 to (length set)
	append (contiguous-sublists-of-length n set)))

;; FIRST-N is defined in UTILS.LISP

(defun contiguous-sublists-of-length (n set)
  (loop for set+ on set
	when (>= (length set+) n)
	collect (first-n n set+)))

(defvar *ordered-sublist-table* (make-hash-table :test #'equal))

(defun enter-ordered-sublists (list)
  (loop for sublist in (all-ordered-sublists list)
	as value = (gethash sublist *ordered-sublist-table*)
	when value 
	do (incf (gethash sublist *ordered-sublist-table*))
	else do (setf (gethash sublist *ordered-sublist-table*) 1)))

;;; This will enter all of the descriptions into the table:

(defun enter-blast-descriptions (blast-results)
  (clrhash *ordered-sublist-table*)
  (loop for defn in (get-xml-blast-defns blast-results)
	do (enter-ordered-sublists (string-split defn))))

;;; Now let's get all the results, sorted by number of occurrances:

(defun frequency-ranked-sublists ()
 (sort 
  (loop for list being the hash-keys of *ordered-sublist-table*
	using (hash-value count)
	collect (cons count list))
  #'(lambda (a b) (> (car a) (car b)))))

;;; Now put this together with frequency-ranked-sublists and the GO 
;;; definition function, and we're set:

(defun extract-go-matches ()
  (loop for (frequency . words) in (frequency-ranked-sublists)
	as match = (define (string-join words))
	when match
	collect (cons frequency match)))

;;; And here's the whole thing put together:

(defun annotate-to-gene-ontology (mystery-sequence &key (program "blastn"))
  (enter-blast-descriptions 
   (blast-sequence-xml mystery-sequence :program program))
  (extract-go-matches))

(defun do-annotation-example ()
  (annotate-to-gene-ontology *mystery-sequence* :program "blastp"))

#|
(defun all-contiguous-sublists (seq)
  (let ((sequence-len (length seq)))
    (labels 
	((all-contiguous-subsets-of-length (seq seqlen subsets-len)
	   (cond
	    ((< seqlen subsets-len) nil)
	    ((= seqlen subsets-len) (list seq))
	    (t 
	     (cons (first-n subsets-len seq)
		   (contiguous-subsets-of-length 
		    (cdr seq) (1- seqlen) subsets-len)))))
	 (contiguous-sublists (seq n)
	   (cond 
	    ((zerop n) nil)
	    ((append 
	      (contiguous-subsets-of-length seq sequence-len n)
	      (contiguous-sublists seq (1- n))
	      )))))
      (contiguous-sublists seq sequence-len)
      )))
|#
