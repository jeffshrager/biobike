;;; Copyright (c) 2002-2003 by Jeff Shrager and Mike Travers; All rights reserved.

;;; This software is made avilable for EDUCATIONAL PURPOSES
;;; ONLY, and WITHOUT ANY WARRANTY, express or implied, of
;;; its merchantability or fitness for a particular purpose. 

(compile
(defun string-split (string &optional (delimiter #\space))
  (let ((substrings '())
        (length (length string))
        (last 0))
    (flet ((add-substring (i)
	     (push (make-array (- i last)
			       :element-type 'character
			       :displaced-to string
			       :displaced-index-offset last)
		   substrings)))
      (dotimes (i length)
        (when (eq (char string i) delimiter)
          (add-substring i)
          (setq last (1+ i))))
      (add-substring length)
      (nreverse substrings)))))

(defun get-xml-blast-defns (hits)
   (loop for hit in hits
	 collect (assocadr '|Hit_def| hit)))

(defun all-ordered-sublists (set)
  (loop for n from 1 to (length set)
	append (n-length-ordered-sublists n set)))

(defun n-length-ordered-sublists (n set)
  (loop for set+ on set
	when (>= (length set+) n)
	collect (first-n n set+)))

(defun first-n (n list)
  (loop for k from 1 to n
	as item in list
	collect item))


(defvar *ordered-sublist-table* (make-hash-table :test #'equal))

(compile
(defun enter-ordered-sublists (list)
  (loop for sublist in (all-ordered-sublists list)
	as value = (gethash sublist *ordered-sublist-table*)
	when value 
	do (incf (gethash sublist *ordered-sublist-table*))
	else do (setf (gethash sublist *ordered-sublist-table*) 1))))

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

;;; This will turn '("this" "is" "a" "test") into "this is a test"
;;; (There's a fancier way to do this all with one format, which we
;;;  shall carefully avoid here!  Some Lisps also have a concatenate
;;;  function, but the format nil will do just as well for the nonce.)

(compile
(defun recompose-list-into-GO-name (list)
  (string-trim " " 
    (loop with result = ""
	  as word in list
	  do (setq result (format nil "~a ~a" result word))
	  finally (return result)))))
 
;;; Now put this together with frequency-ranked-sublists and the GO defin function, and 
;;; we're set:

(defun extract-go-matches ()
  (loop for (frequency . words) in (frequency-ranked-sublists)
	as match = (define (recompose-list-into-GO-name words))
	when match
	collect (cons frequency match)))

;;; And here's the whole thing put together:

(defun annotate-to-gene-ontology (mystery-sequence &key (program "blastn"))
  (enter-blast-descriptions (blast-sequence-xml mystery-sequence :program program))
  (extract-go-matches))

