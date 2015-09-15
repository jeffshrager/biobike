;;; Copyright (c) 2002-2003 by Jeff Shrager and Mike Travers; All rights reserved.

;;; This software is made avilable for EDUCATIONAL PURPOSES
;;; ONLY, and WITHOUT ANY WARRANTY, express or implied, of
;;; its merchantability or fitness for a particular purpose. 

(in-package USER)

(setq model-1 
 '(
   (+ green accelerator)
   (+ accelerator speed)
   (+ red brake)
   (- brake speed)
   ))

(setq *data* '(   
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

(defun explain-from (from-edge table &optional (value '+) (to 'speed))
  (let* ((to-table (gethash from-edge table))
         (stop? (gethash to to-table)))
    (case  stop? ; If the target is among the to-edges, report the result
     ;; inverting if necessary along this edge.
     (+ value)
     (- (invert value))
     ;; Otherwise, recursively trace paths through the model,
     ;; appropriately swapping + for - across negative edges, as needed.
     (t (loop for to-edge being the hash-keys of to-table
              using (hash-value to-edge-type)
              as result = (when to-edge-type 
                             (explain-from to-edge table
                               (case to-edge-type
                                 (+ value)
                                 (- (invert value)))
                               to))
              ;; We only need the FIRST result we come to, so we don't have
              ;; to gather more than one result that comes back to us.
              when result
              do (return result))))))

(defun invert (value) ; This just saves us some typing 
  (case value (+ '-) (- '+)))

(defun explain-model (model from to)
  (let ((table (make-blank-model-table (find-unique-vertices model))))
    (code-model model table)
    (explain-from from table '+ to)))

(defun find-unique-vertices (model)
  (loop with result
        for (edge-type from to) in model
        do (pushnew from result)
        (pushnew to result)
        finally (return result)))

(defun score-traffic-model-table (table data)
  (let* ((red-prediction (explain-from 'red table))
         (green-prediction (explain-from 'green table))
         (red-right (count-prediction-matches data 'red red-prediction))
         (red-wrong (count-prediction-matches data 'red (invert red-prediction)))
         (green-right (count-prediction-matches data 'green green-prediction))
         (green-wrong (count-prediction-matches data 'green (invert green-prediction))))
      (list red-right red-wrong green-right green-wrong)))

(defun score-traffic-model (model data)
  (let ((table (make-blank-model-table (find-unique-vertices model))))
    (code-model model table)
    (score-traffic-model-table table data)))

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
                      &aux (data *data*))
  (let ((initial-score (summary-score (score-traffic-model-table model-table data))))
    (format t "Initial score is ~a~%" initial-score)
    ;; First two loops run through every cell in the table.
    (loop with current-score = initial-score
          for from-vertex in vertices
          as to-table = (gethash from-vertex model-table)
          do (loop for to-vertex in vertices
                   as current-edge-type = (gethash to-vertex to-table)
                   ;; This one tries every new edge type.
                   do (loop for new-edge-type in edge-types
                            ;; Don't bother if it's the same as the one we have now!
                            unless (eq current-edge-type new-edge-type)
                            do 
                            ;; Make the current change in the model table...
                            (setf (gethash to-vertex to-table) new-edge-type)
                            ;; and score it, then decide whether to leave it, or revert.
                            (let ((new-score (summary-score (score-traffic-model-table model-table data))))
                              (cond ((> new-score current-score) ; New score is better: Leave new model!
                                     (format t "~a-[~a]->~a becomes [~a] improving score to ~a~%"
                                             from-vertex current-edge-type to-vertex new-edge-type new-score)
                                     ;; This ensures that if we need to set it back, it gets set
                                     ;; back to the correct (i.e., new) edge type.
                                     (setq current-edge-type new-edge-type)
                                     (setq current-score new-score))
                                   ;; Otherwise, put the model back the way we found it.
                                   (t (setf (gethash to-vertex to-table) current-edge-type))))
                             ))))
  model-table)
      
(defun improve (starting-model &key (vertices '(red green brake accelerator speed)))
  (let ((table (make-blank-model-table vertices)))
    (code-model starting-model table)
    (read-model (improve-model table :vertices vertices))))
    
(setq bad-model-1 '((- GREEN ACCELERATOR) (+ ACCELERATOR SPEED) (+ RED BRAKE) (- BRAKE SPEED)))

(defun explain-from (from-edge table &optional been-there-done-that (value '+) (to 'speed))
  (let* ((to-table (gethash from-edge table))
         (stop? (gethash to to-table)))
    (case  stop? ; If the target is among the to-edges, report the result
      ;; inverting if necessary along this edge.
      (+ value)
      (- (invert value))
      ;; Otherwise, recursively trace paths through the model,
      ;; appropriately swapping + for - across negative edges, as needed.
      (t (loop for to-edge being the hash-keys of to-table
               using (hash-value to-edge-type)
               as result = (cond ((member (cons from-edge to-edge) been-there-done-that :test #'equal) 
                                  nil)
                                 (t (when to-edge-type 
                                      (explain-from to-edge table
                                                    (cons (cons from-edge to-edge)
                                                          been-there-done-that)
                                                    (case to-edge-type
                                                      (+ value)
                                                      (- (invert value)))
                                                    to))))
               ;; We only need the FIRST result we come to, so we don't have
               ;; to gather more than one result that comes back to us.
               when result
               do (return result))))))
