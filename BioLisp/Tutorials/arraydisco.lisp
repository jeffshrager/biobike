;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

;;; Copyright (c) 2002-2003 by Jeff Shrager, Mike Travers, and JP Massar.
;;;  All rights reserved.

(in-package :bio)

;;; This software is made avilable for EDUCATIONAL PURPOSES
;;; ONLY, and WITHOUT ANY WARRANTY, express or implied, of
;;; its merchantability or fitness for a particular purpose. 

;;;

;;; Conversion of Hihara data to symbolic labels.

(defun massage-hihara-data ()
  (loop for entry in *hihara-results*
      do (setf (car entry) (read-from-string (car entry))))
  (tablify-hihara-results))

(defvar *correlation-cache* (make-hash-table :test #'equal))

(defun correlate-genes (a b)
  (or (gethash (cons a b) *correlation-cache*)
      (gethash (cons b a) *correlation-cache*)
      (let ((r (correlate-fast (gethash a *hihara-table*) 
                               (gethash b *hihara-table*))))
	(setf (gethash (cons a b) *correlation-cache*) r)
	r)))

(defstruct model ngenes genes edge-types path-cache table)

(defun create-empty-model (genes &optional (edge-types '(+ - nil)))
  (let ((ngenes (length genes)))
    (make-model :ngenes ngenes
    :genes genes 
    :edge-types edge-types
    :path-cache (make-hash-table :test #'equal)
    :table (make-blank-model-table genes)
    )))

(defun make-blank-model-table (vertices)
  ;; Create the main (from-)table indexes being vertexes...
  (let ((table (make-hash-table :test #'equal)))
    ;; Fill it with sub (to-)tables for each vertex...
    (loop for vertex-from in vertices
          as to-table = (make-hash-table :test #'equal)
          do 
          (setf (gethash vertex-from table) to-table))
    table))

(defun code-model (model edges)
  (let ((table (model-table model)))
    (loop for (edge-type from to) in edges
        do (setf (gethash to (gethash from table)) edge-type))))

;;; This is the original version of FIND-ALL-PATHS used in the
;;; tutorial.  The improved version is below.

#|
(defun find-all-paths (model)
  (let ((genes (model-genes model))
	(table (model-table model)))
    (loop for start in genes
	append 
	  ;; We have a special case for self-edges because otherwise we'll
	  ;; always get them (since the test in trace-paths just looks for 
	  ;; (eq start target)
	  ;; which is always true for self-edges!)
	  (loop for target in genes
	      if (eq start target)
	      append (when (gethash target (gethash start table)) 
		       (list (list start)))
	      else append (trace-paths table start target)))))
|#


;;; This is the heart of the operation.  Just follow all the ways out of every
;;; node until you reach the target in all possible ways, collecting paths 
;;; in the optional collector var (path) as you go. When you hit the end, 
;;; return the collector.

;;; This is the first of multiple versions of TRACE-PATHS.

#|
(defun trace-paths (table here target &optional path)
  (cond 
   ((eq here target) (list (cons here path)))
   (t (loop for next being the hash-keys of (gethash here table)
	  using (hash-value +-)
		;; Otherwise we'll be including entries in the 
		;; hash table that contain nil!
	  when +- 
	  append (trace-paths table next target (cons here path))))))
|#

;;; Here's a test function:

(defun test ()
  (let ((m (create-empty-model '(a b c d e f g))))
    (code-model m '((+ a b) (- a c) (+ b d) (- c e) 
		    (+ c f) (- b f) (- e f) (+ f g)))
    (mapcar #'print (find-all-paths m))))


;;; The second of multiple versions of TRACE-PATHS

;;; Records edge types in paths:

#|
(defun trace-paths (table here target &optional path)
  (cond 
   ((eq here target) (list (cons here path)))
   (t (loop for next being the hash-keys of (gethash here table)
	  using (hash-value +-)
		;; Otherwise we'll be including entries in the 
		;; hash table that contain nil!
	  when +- 
	       ;; *********** Cons the edge polarity in as well. ***********
	  append (trace-paths table next target (cons (cons +- here) path))))))
|#

;;; The third of multiple versions of TRACE-PATHS

;;; Handles loops:

(defun trace-paths (table here target &optional path)
  (cond 
   ((eq here target) (list (cons here path)))
   ;; *********** If we've been here before, stop here! ***********
   ((member here path :test #'(lambda (a b) (eq a (cdr b))))
    (list (cons here path)))
   (t (loop for next being the hash-keys of (gethash here table)
	  using (hash-value +-)
		;; Otherwise we'll be including entries in the 
		;; hash table that contain nil!
	  when +- 
	  append (trace-paths table next target (cons (cons +- here) path))))))

(defun test2 ()
  (let (( m (create-empty-model '(a b c d e f g))))
    (code-model m '((+ a b) (- a c) (+ b d) (- c e) (+ c f) 
		    (- c c) (- b f) (- e f) (+ e b) (+ f g)))
    (mapcar #'print (find-all-paths m))))

(defun improve (model-expression)
  (let* ((model (create-empty-model 
		 ;; All unique vertex names.
                 (loop for node in model-expression
		     with names = nil
		     do 
		       (let ((from (second node)) (to (third node)))
			 (pushnew from names)
			 (pushnew to names))
		     finally (return names)))))
    (code-model model model-expression)
    ;; Iterate on calling IMPROVE-MODEL until we can't 
    ;; make the score any better, then stop.
    (loop with current-score = (score-model model)
	as round from 1 by 1
	do 
	  (format t "~a: Current score is ~a~%" round current-score)
	  (improve-model model)
	  (let ((new-score (score-model model)))
	    (format t "Resulting model score is ~a~%" new-score)
	    (when (= current-score new-score)
	      (format t "Done:~%")
	      (display-model model)
	      (return nil))
	    (setq current-score new-score))
	  )))

;;; Here's a convenience that we should have had ages ago!

(defun display-model (model &optional text)
  (when text (format t text))
  (loop for expression in (read-model (model-table model))
	do (print expression)))

;;; This needs a special case for self-paths, 
;;; which look like this: ((-+ . gene))

;;; This is just for fun, so that we count up the number
;;; of models that have been tried.

(defvar *n-models-scored* 0)

;;; The score is just going to be the fraction of paths that
;;; are right (match data correlation polarity).

(defun score-model (model)
  (incf *n-models-scored*)
  (when (zerop (mod *n-models-scored* 1000))
    (format t "~%Scored ~a models~%" *n-models-scored*))
  (loop for path in (find-all-paths model)
      with sum-scores = 0.0
      with n-paths = 0.0
      do 
	(incf n-paths)
	(when (score-path path)
	  (incf sum-scores))
      finally (return (/ sum-scores n-paths))))

(defun score-path (path)
  (cond ((null (cdr path)) '(:r 1.0 :r2 1.0))
	(t (let* ((corr (corr-path path))
		  (r (getf corr :r))
		  (r2 (getf corr :r2))
		  (path-is-+? (path-polarity path))
		  (corr-is-+? (> r 0)))
	     (declare (ignore r2))
	     (or (and path-is-+? corr-is-+?)
		 (and (not path-is-+?) (not corr-is-+?)))))))

(defun corr-path (path)
  (correlate-genes (car path) (cdar (last path))))

(defun path-polarity (path)
  (let ((polarity t))
    (dolist (node (cdr path))
      (let ((edge-polarity (first node)))
        (when (eq '- edge-polarity) (setq polarity (not polarity)))))
    polarity
    ))

#|
(defun path-polarity (path)
  (loop for (edge-polarity . name) in (cdr path)
	with v = t
	when (eq '- edge-polarity)
	do (setq v (not v))
	finally (return v)))
|#

(setq *model-1* '((+ slr1520 slr1519) 
		  (- slr1520 slr1518)
		  (+ slr1519 slr1517)
		  (- slr1518 slr1516)
		  (+ slr1518 slr1515)
		  (- slr1518 slr1518)
		  (- slr1519 slr1515)
		  (- slr1516 slr1515)
		  (+ slr1516 slr1519)
		  (+ slr1515 slr1513)
		  (- slr1517 slr1520)))

(defun improve-model (model)
  (let* ((edge-types (model-edge-types model))
	 (model-table (model-table model))
	 (vertices (model-genes model))
	 (initial-score (score-model model)))
    (format t "Initial score is ~a~%" initial-score)
    ;; First two loops run through every cell in the table.
    (loop with current-score = initial-score
	  for from-vertex in vertices
	  as to-table = (gethash from-vertex model-table)
	  do 
	  (loop for to-vertex in vertices
	        as current-edge-type = (gethash to-vertex to-table)
	        do 
		;; This one tries every new edge type.
		(loop for new-edge-type in edge-types
                      ;; Don't bother if it's the same
                      ;; as the one we have now!
		      unless (eq current-edge-type new-edge-type)
		      do 
		      ;; Make the current change in the model table...
		      (setf (gethash to-vertex to-table) new-edge-type)
		      ;; score it, then decide whether to leave it, or revert.
		      (let ((new-score (score-model model)))
			(cond 
			 ;; New score is better: Leave new model!
			 ((> new-score current-score) 
			  (format t "~a-[~a]->~a becomes [~a] improving score to ~a~%"
				  from-vertex current-edge-type to-vertex 
				  new-edge-type new-score)
			  ;; Ensure that if we have to reset the edge, 
			  ;; it gets set to the best last type, rather 
			  ;; than a previous, possibly older value.
			  (setq current-edge-type new-edge-type)
			  (setq current-score new-score))
			 ;; Otherwise, put the model back the way we found it.
			 (t (setf (gethash to-vertex to-table) 
			          current-edge-type))))
		      )))))

(defun test3 (model)
  (setq *n-models-scored* 0)
  (improve model))

;;; This version uses the "pushnewt" method to efficiently remove duplicate
;;; paths, plus does a rather hairy version of subpath caching.  Actually, 
;;; it could be made even more efficient by not reporting paths at all, 
;;; but instead just return the polarities and path lengths for scoring!

(defvar *path-pushnew-table* (make-hash-table :test #'equal))

(defun find-all-paths (model)
  (clrhash *path-pushnew-table*)
  (let ((genes (model-genes model))
	(table (model-table model))
	(path-cache (model-path-cache model))
	result)
    (clrhash path-cache)
    (loop for start in genes
	do (loop for target in genes
			       ;; We have a special case for self-edges 
			       ;; because otherwise we'll always get them 
			       ;; (since the test in trace-paths just looks 
			       ;; for (eq start target) which is always true 
			       ;; for self-edges!)
	       as self = (gethash target (gethash start table))
	       as cached-paths = (gethash (cons start target) path-cache)
	       do
		 (loop for path in 
		       (cond 
			(cached-paths)
			((eq start target)
			 (when self (list (list (cons self start)))))
			(t (let ((paths (trace-paths table start target)))
			     (loop for path in paths
				 do (cache-path path path-cache))
			     paths)))
		     do (cond 
			 ((gethash path *path-pushnew-table*))
			 (t (setf (gethash path *path-pushnew-table*) path)
			    (push path result))))))
    result))
	  

;;; This breaks down every subpath in the path and adds them all to the cache. 
;;; Note that you need to extract the terminal element from its polarity 
;;; embedding, that is, (+ . G) -> G if it's the last element in the path.  
;;; Note that the paths are reversed to head are tails, and v.v.!  
;;; This could prob. be improved!

(defun cache-path (path cache)
  (let ((l (length path)))
    (loop for start from 0 to (1- l)
	as tail on path
	do 
	  (loop for end from 1 to (- l start)
	      as path1 = (loop for item in tail
			     as k from 0 to end
			     collect item)
	      as path2 = (cons (if (consp (car path1)) (cdar path1) (car path1)) 
			       (cdr path1))
	      as key = (when (cdr path2) (cons (cdar (last path2)) (car path2)))
	      when (cdr path2)
	      do 
		(pushnew path2 (gethash key cache)
			 :test #'equal)))))

