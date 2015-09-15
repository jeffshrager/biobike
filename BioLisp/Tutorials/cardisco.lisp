;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

;;; Copyright (c) 2002-2003 by Jeff Shrager, Mike Travers, and JP Massar.
;;;  All rights reserved.

(in-package :bio)

;;; This software is made avilable for EDUCATIONAL PURPOSES
;;; ONLY, and WITHOUT ANY WARRANTY, express or implied, of
;;; its merchantability or fitness for a particular purpose. 

(defparameter *model-1* 
 '(
   (+ green accelerator)
   (+ accelerator speed)
   (+ red brake)
   (- brake speed)
   ))

(defparameter *model-1-data* '(   
  (red -) (green +) (red -) (green +) (red -) (red +)
  (red -) (red -) (green +) (red -) (red +) (green +)
  (red -) (red -) (green +)  (red -) (red +) (green +)
  (red -) (green +) (red -)  (red -) (green +) (red -)
  (green +) (red -) (green +) (red -) (green -) (green +)
  (red -) (red -) (green +) (red -) (red +) (green +)
  (red -)  (red -) (red +) (green +) (red -) (green +)
  (red -) (green +) (red -)  (red -) (green +) (red -)
  (green +)  (green -) (green +) (red -) (green +) (red -)
  ))

(defun make-blank-model-table (vertices)
  ;; Create the main (from-)table indexes being vertexes...
  (let ((table (make-hash-table :test #'equal)))
    ;; Fill it with sub (to-)tables for each vertex...
    (loop for vertex-from in vertices
          as to-table = (make-hash-table :test #'equal)
          do 
          (setf (gethash vertex-from table) to-table)
          ;; Set each cell to nil (slightly redundant since that's the default).
          (loop for vertex-to in vertices
                do (setf (gethash vertex-to to-table) nil)))
    table))

(defun code-model (model table)
  (loop for (edge-type from to) in model
        do (setf (gethash to (gethash from table)) edge-type)))

(defun read-model (table)
  (loop for from-vertex being the hash-keys of table
        using (hash-value to-table)
        append (loop for to-vertex being the hash-keys of to-table
                     using (hash-value edge-type)
                     when edge-type
                     collect (list edge-type from-vertex to-vertex))))

;;; This is the first version of EXPLAIN-FROM used by the tutorial.
;;; There is another definition below.   You should cut and paste
;;; each definition into Lisp as the tutorial uses them.

#|
(defun explain-from (from-node table &optional (value '+) (to-node 'speed))
  (let* ((to-table (gethash from-node table))
         (stop? (gethash to-node to-table)))
    (case stop? ; If the target is among the to-edges, report the result
      ;; inverting if necessary along this edge.
      (+ value)
      (- (invert value))
      ;; Otherwise, recursively trace paths through the model,
      ;; appropriately swapping + for - across negative edges, as needed.
      (t (loop for intermediate-node being the hash-keys of to-table
               using (hash-value to-edge-type)
               as result = (when to-edge-type 
                             (explain-from intermediate-node table
                                           (case to-edge-type
                                             (+ value)
                                             (- (invert value)))
                                           to-node))
               ;; We only need the FIRST result we come to, so we don't have
               ;; to gather more than one result that comes back to us.
               when result
               do (return result))))))
|#

(defun invert (value) ; This just saves us some typing 
  (case value (+ '-) (- '+)))

(defun explain-model (model from to)
  (let ((table (make-blank-model-table (find-unique-vertices model))))
    (code-model model table)
    (explain-from from table '+ to)))

;(defun find-unique-vertices (model) (remove-duplicates (mapcan #'cdr model)))

(defun find-unique-vertices (model)
  (loop with result
      for (edge-type from to) in model
      do 
	(pushnew from result)
        (pushnew to result)
	;; Make compiler shut up aout EDGE-TYPE not being used.
        (progn edge-type)
      finally (return result)))

(defun score-traffic-model-table (table data)
  (let* ((red-prediction (explain-from 'red table))
         (green-prediction (explain-from 'green table))
         (red-right (count-prediction-matches data 'red red-prediction))
         (red-wrong (count-prediction-matches data 'red (invert red-prediction)))
         (green-right (count-prediction-matches data 'green green-prediction))
         (green-wrong (count-prediction-matches data 'green (invert green-prediction))))
      (list red-right red-wrong green-right green-wrong)))


;;; This is the initial definition of SCORE-TRAFFIC-MODEL.
;;; There is a latter one below.  You should cut-and-paste each one
;;; into your Lisp as the tutorial uses each one.

#|
(defun score-traffic-model (model data)
  (let ((table (make-blank-model-table (find-unique-vertices model))))
    (code-model model table)
    (score-traffic-model-table table data)))
|#


(defun count-prediction-matches (data color prediction)
  (loop for (data-color observation) in data
        when (and (eq color data-color)
                  (eq prediction observation))
        sum 1))

(defun summary-score (score)
  ;; Note that we're also protecting against division by zero, in case
  ;; there's no prediction at all made by the model!)
  (/ (+ (let ((divisor (+ (first score) (second score))))
          (if (zerop divisor) 0.0 (/ (first score) divisor)))
        (let ((divisor (+ (third score) (fourth score))))
          (if (zerop divisor) 0.0 (/ (third score) divisor))))
     2.0))

(defun score-traffic-model (model data)
  (summary-score 
   (let ((table (make-blank-model-table (find-unique-vertices model))))
    (code-model model table)
    (score-traffic-model-table table data))))

(defun improve-model (model-table 
                      &key (edge-types '(+ - nil))
                      (vertices '(red green brake accelerator speed))
                      &aux (data *model-1-data*))
  (let ((initial-score 
         (summary-score (score-traffic-model-table model-table data))))
    (format t "Initial score is ~a~%" initial-score)
    ;; First two loops run through every cell in the table.
    (loop with current-score = initial-score
          for from-vertex in vertices
          as to-table = (gethash from-vertex model-table)
          do 
          (loop for to-vertex in vertices
                as current-edge-type = (gethash to-vertex to-table)
                ;; This one tries every new edge type.
                do 
                (loop for new-edge-type in edge-types
                      ;; Don't bother if it's the same as the one we have now!
                      unless (eq current-edge-type new-edge-type)
                      do 
                      ;; Make the current change in the model table...
                      (setf (gethash to-vertex to-table) new-edge-type)
                      ;; score it, then decide whether to leave it, or revert.
                      (let ((new-score 
                             (summary-score 
                              (score-traffic-model-table model-table data))))
                        (cond 
                         ;; New score is better: Leave new model!
                         ((> new-score current-score) 
                          (format t "~a-[~a]->~a becomes [~a] improving score to ~a~%"
                                  from-vertex current-edge-type 
                                  to-vertex new-edge-type new-score)
                          ;; This ensures that if we need to set it back, it
                          ;; gets set back to the correct (i.e. new) edge type.
                          (setq current-edge-type new-edge-type)
                          (setq current-score new-score))
                         ;; Otherwise, put the model back the way we found it.
                         (t (setf (gethash to-vertex to-table) 
                                  current-edge-type))))
                      ))))
  model-table)
      
(defun improve 
       (starting-model &key (vertices '(red green brake accelerator speed)))
  (let ((table (make-blank-model-table vertices)))
    (code-model starting-model table)
    (read-model (improve-model table :vertices vertices))))
    
(defparameter bad-model-1
             '((- GREEN ACCELERATOR) (+ ACCELERATOR SPEED)
               (+ RED BRAKE) (- BRAKE SPEED)))

(defun explain-from 
       (from-node table &optional been-there-done-that (value '+) (to 'speed))
  (let* ((to-table (gethash from-node table))
         (stop? (gethash to to-table)))
    ;; If the target is among the to-edges, report the result
    ;; inverting if necessary along this edge.
    (case stop?	
      (+ value)
      (- (invert value))
      ;; Otherwise, recursively trace paths through the model,
      ;; appropriately swapping + for - across negative edges, as needed.
      (t 
       (loop for intermediate-node being the hash-keys of to-table
             using (hash-value to-edge-type)
             as result = 
             (cond 
              ((member 
                (cons from-node intermediate-node) 
                been-there-done-that 
                :test #'equal) 
               nil)
              (t 
               (when to-edge-type 
                 (explain-from intermediate-node table
                               (cons (cons from-node intermediate-node)
                                     been-there-done-that)
                               (case to-edge-type
                                 (+ value)
                                 (- (invert value)))
                               to))))
             ;; We only need the FIRST result we come to, so we don't have
             ;; to gather more than one result that comes back to us.
             when result
             do (return result))))))


(defun explain-causal-chain 
       (from-node table 
                  &optional (more-or-less '+) (to-node 'speed) (verbose t))
  (let ((result 
	 (explain-causal-chain-internal 
          from-node to-node table more-or-less nil verbose)))
    (values
     (caar result)
     (and verbose
          (concatenate 
           'string
           (format nil "The hypothesis is ~A ~A...~%"
                   (ecase more-or-less (+ "increasing") (- "decreasing"))
                   from-node)
           (string-join (mapcar #'second (reverse result)) #\Newline)
           )))))

(defun explanation (to-node more-or-less terminate? verbose)
  (list more-or-less
        (and verbose
	     (format nil "  which causes ~A to ~A~A"
		     to-node
		     (case more-or-less
                       (+ "increase")
                       (- "decrease")
                       (otherwise "not be affected"))
		     (if terminate? "." ",")
		     ))))

(defun explain-causal-chain-internal 
       (from-node to-node table more-or-less been-there-done-that verbose)
  (let* ((to-table (gethash from-node table))
         (stop? (gethash to-node to-table)))
    ;; If the target is among the to-edges, report the result
    ;; inverting if necessary along this edge.
    (if stop?
	(list 
         (explanation 
          to-node (ecase stop? (+ more-or-less) (- (invert more-or-less)))
          t verbose))
      ;; Otherwise, recursively trace paths through the model,
      ;; appropriately swapping + for - across negative edges, as needed.
      (loop for intermediate-node being the hash-keys of to-table
	    using (hash-value edge-type)
	    as result = 
            (cond
             ;; prevent self-pointing-edge infinite recursion
             ((member 
               (cons from-node intermediate-node)
               been-there-done-that
               :test #'equal)
              nil)
             (t
              (when edge-type 
                (explain-causal-chain-internal
                 intermediate-node to-node table
                 (case edge-type
                   (+ more-or-less)
                   (- (invert more-or-less)))
                 (cons (cons from-node intermediate-node) been-there-done-that)
                 verbose
                 ))))
	    when result do
	    ;; We only need the FIRST result we come to, so we don't have
	    ;; to gather more than one result that comes back to us.
	    (return 
             (append
              result
              (list (explanation intermediate-node more-or-less nil verbose))
              ))))))

(defun explain-causal-model (model from to)
  (let ((table (make-blank-model-table (find-unique-vertices model))))
    (code-model model table)
    (explain-causal-chain from table '+ to)))
