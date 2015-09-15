;;; -------------------------------------------------- DISCO --------------------------------------------------

;;; ------------------------------------------------------------------------------------------------
;;; Top level, implements greedy search.  We have a single best model,
;;; and replace it only when we find a better one, keeping no backtrack
;;; history.  When there are no models that are better in the set, stop.
;;; This is definitely NOT the best search method, but it'll do for now.

(defvar *target* 'life)

(defun disco-for-web (model &optional (datafile "wc") print? &aux best-model best-score (n-tried 0))
  (chipload (format nil "g:\\jshrager\\biolingua\\code\\~a.cd" datafile))
  (rd) ; Outlier removal
  (setq best-model (parse-model model))
  (setq best-score (analyze-model best-model nil))
  (format t "~%Initial model, at ~a, is:~%" best-score)
  (until (progn 
	  (format t "Next round! (tried ~a so far)....~%" n-tried)
	  (null
	  ;; Runs through all proposed models from the current one, returning
	  ;; nil if we don't replace it (with a better one), t if we do.  If it gets
	  ;; replaced, the UNTIL will run through again, but if not, it won't.
	  (loop with replaced? = nil
		for new-model in (propose-new-models best-model)
		as new-score = (analyze-model new-model print?)
		do (incf n-tried) 
;		(format t "Trying new model with ~a links, score = ~a~%" (length new-model) new-score)
		(cond ((and (< new-score 2) (> new-score best-score))
		       (format t " ****** After ~a tries, replacing at ~a with: ~%" n-tried new-score)
		       (print-model new-model)
		       (setq replaced? t)
		       (setq best-model new-model)
		       (setq best-score new-score)))
		finally (return replaced?)))))
  (format t "Tried ~a models~%" n-tried)
  (format t "Best model analysis:~%")
  (analyze-model best-model nil)
  best-model
  )

;;; The model come in from the web  page as a long string that has to be parsed into lisp.

(defun parse-model (model-as-string)
  (remove nil 
	  (loop with start-at = 0
		with last-entry = t
		until (null last-entry)
		collect (multiple-value-bind (entry next) (read-from-string model-as-string nil nil :start start-at)
					     (setq start-at next) 
					     (setq last-entry entry)))))

(defun print-model (model)
  (loop for x in model do (format t "  ~a~%" x)))

;;; ------------------------------------------------------------------------------------------------
;;; Model modification.

;;; Returns a list of proposed new models, using the favored
;;; model-creation operators du jour.  At the moment this makes
;;; stupid meaningless changes.

(defun propose-new-models (current-model &aux (len-1 (1- (length current-model))))
  (remove 
   nil 
   (append 
    ;; Remove each (single) link.  Problem: This can leave the model disconnected.  That
    ;; is theoretically okay, but you can end up with a model that has very few predictions
    ;; (because of its disconnectedness), and they are all good.  In fact, you can end up
    ;; with a model that makes only one prediction, even though it has many components,
    ;; giving you a false sense of perfection.
    (loop for i from 0 to len-1
	  collect (remove (nth i current-model) current-model)
	  )
    ;; Invert each (single) q direction.
    (loop for i from 0 to len-1
	  as new-model = (copy-tree current-model)
	  as expr = (cdr (nth i new-model))
	  do (rplaca expr (if (eq 'q+ (car expr)) 'q- 'q+))
	  collect new-model)
    ;; Replace each (single) node with one from the data but which is NOT in the 
    ;; current model (no looping problems!)  
    (replace-genes current-model)
#|
    ;; Adding links is complicated because it can cause loops in the model,
    ;; as well as loops in the addition/deletion sequence.  This just adds
    ;; adds single links between existing nodes.
    (loop for i from 0 to len-1
	  as x1 in current-model
	  append (loop for j from i to len-1
		       as x2 = (nth j current-model)
		       append
		       (list (add-link current-model (third x1) (third x2) 'q+)
			     (add-link current-model (third x1) (third x2) 'q-)
			     (add-link current-model (third x1) (fourth x2) 'q+)
			     (add-link current-model (third x1) (fourth x2) 'q-)
			     (add-link current-model (fourth x1) (third x2) 'q+)
			     (add-link current-model (fourth x1) (third x2) 'q-) 
			     (add-link current-model (fourth x1) (fourth x2) 'q+)
			     (add-link current-model (fourth x1) (fourth x2) 'q-)
			     ))
	  )
|#
    )))

;;; Replace uses of a gene everywhere it occurs with one that isn't used anyplace else in the 
;;; model (cannot cause loops, I think!)

(defun replace-genes (model)
  (let ((used-genes (loop for (key type from to) in model 
			  with all = nil 
			  do (pushnew from all) 
			  (pushnew to all)
			  finally (return all))))
    (loop for (new-gene . rest) in *d*
	  unless (member new-gene used-genes)
	  append (loop for gene in used-genes
		       collect (replace-tree model gene new-gene)
		       ))))

(defun replace-tree (tree old new)
  (cond ((null tree) nil)
	((eq tree old) new)
	((atom tree) tree)
	(t (cons (replace-tree (car tree) old new)
		 (replace-tree (cdr tree) old new)
		 ))))

;;; Protects against local loops (having exactly the same from/to)

(defun add-link (model from to type)
  (unless (eq from to)
	  (cons `(xr ,type ,from ,to) (copy-list model))))

;;; This does complete search of all models that have up to DN deletions 
;;; and IN inversions.

(defun record (r m)
  (format t ".")
  (when (< r 2)
	(push (cons r m) *results*)))

(defun all-models-with-n-inversions (n m &aux r)
  (setq r (list m))
  (loop for i from 1 to n
	do (loop for m in r
		 do (setq r (append (loop for j from 0 to (1- (length m))
					  collect (let ((nm (copy-tree m)))
						    (invert (nth j nm))
						    nm))
				    r))))
  (remove-duplicates r :test #'equal))

(defun invert (xpr)
  (setf (second xpr) (if (eq (second xpr) 'q+) 'q- 'q+)))

(defun all-models-with-n-deletions (n m &aux r)
  (setq r (list m))
  (loop for i from 1 to n
	do (loop for m in r
		 do (setq r (append (loop for j from 0 to (1- (length m))
				 collect (remove (nth j m) m))
			   r))))
  (remove-duplicates r :test #'equal))

;;; ------------------------------------------------------------------------------------------------
;;; Model Analysis incl. pathway tracking and qualitative simulation.

(defun analyze-model (model &optional (print? t) &aux results)
  (setq *p* nil)
  (setq *m* model)
  (when print?
	(format t "~%Model is:~%")
	(print-model model)
	(format t "~%~%"))
  (explain *target*)
  (setq combined-predictions nil)
  (loop for pathway in *p*
	as k from 1 by 1
	as predictions = (fpfxrg pathway)
	as integrated-predictions = (QSim predictions)
	with final
	do
	(when print?
	      (format t "Pathway #~a:~%  ~a~%" k pathway)
	      (format t "  Predictions: ~a~%" predictions)
	      )
	(setq combined-predictions (combine-predictions integrated-predictions combined-predictions))
	)
  (when print? (format t "~%Combined analysis:~%"))
  (setq final (analyze-qsim combined-predictions print?))
  (let ((r (summarize-Qresult final)))
    (when print? (format t "~%Score = ~a~%" r))
    r))

;;; Form pathways in the current model (*m*) putting them in *p*
;;; WARNING: This will loop if the model has loops!

(defun explain (x &optional (q-op '+) e)
  (let ((sources (loop for q in *m* 
		       when (and (member (second q) '(q+ q-)) (eq x (fourth q)))
		       collect q))
	(integrators (loop for q in *m* 
		       when (and (eq (second q) 'combine) (eq x (fourth q)))
		       collect q))
	)
    (cond ((and (null sources) (null integrators))
	   (push e *p*))
	  (t 
	   (loop for integrator in integrators
		 do
		 (loop for input in (cdddr integrator)
		       do (cond ((and (listp input) (eq '- (car input)))
				 (explain2 (cadr input) '- (cons integrator e)))
				((atom input) (explain2 input '+ (cons integrator e)))
				(t (error "~a isn't a valid integrator element." input))))
		 )
	   (loop for source in sources
		   do
		   (case q-op 
			 (+ (case (second source)
				  (q+ (explain2 (third source) '+ (cons source e)))
				  (q- (explain2 (third source) '- (cons source e)))
				  ))
			 (- (case (second source)
				  (q+ (explain2 (third source) '- (cons source e)))
				  (q- (explain2 (third source) '+ (cons source e)))))
			 )))
	  )))

(defun explain2 (expr q-op e)
  (cond ((atom expr) (explain expr q-op e))
	(t (case (car expr)
		 (and (loop for qs in (cdr expr)
			    do (cond ((atom qs) (explain qs q-op e))
				     (t (case (car qs)
					      (not (case q-op
							 (+ (explain (second qs) '- e))
							 (- (explain (second qs) '+ e))))))
				     )))
		 (t (error "Can't understand ~a" expr))
		 ))))

(defun fpfxrg (pathway) ; form-predictions-for-transcription-regulation-genes
   (loop for (type q+- . args) in pathway
	 when (eq 'xr type)
	 collect (cons q+- args)))

(defun qsim (predictions &aux terms)
  ;; Collect a unique list of terms
  (loop for (q a b) in predictions do (pushnew a terms) (pushnew b terms))
  (remove-duplicates 
   (q-combine-terms terms terms predictions)
   :test #'(lambda (a b) (or (and (eq (first a) (first b)) (eq (second a) (second b)))
			     (and (eq (first a) (second b)) (eq (second a) (first b))))))
  )
  

(defun q-combine-terms (terms allterms predictions)
  (cond ((null terms) nil)
	(t (append (mapcar #'(lambda (term2) 
			       (when (not (eq (car terms) term2))
				     (qsim-term-to-term (car terms) term2 predictions))) allterms)
		   (mapcar #'(lambda (term2) 
			       (when (not (eq (car terms) term2))
				     (qsim-term-to-term term2 (car terms) predictions))) allterms)
		   (q-combine-terms (cdr terms) allterms predictions)))))

(defun qsim-term-to-term (t1 t2 props)
  (let ((props+ (loop for current-prop on props
		   until (eq t1 (second (car current-prop)))
		   finally (return current-prop))))
    (loop with q+- = 'q+
	  as current-prop in props+
	  with first-prop = (car props+)
	  until (eq t2 (third current-prop))
	  do
	  (when (eq 'q- (first current-prop))
		   (setq q+- (if (eq q+- 'q+) 'q- 'q+)))
	  finally (progn (when (eq t2 (third current-prop)) ; check for running off the end
			       (when (eq 'q- (first current-prop))
				     (setq q+- (if (eq q+- 'q+) 'q- 'q+)))
			       (return (list q+- t1 t2)))))))

(defun combine-predictions (new old &aux result)
  (setq result (remove-duplicates (append new old) :test #'equal))
  (loop for p1 in result
	unless (loop for p2 in result
		     do (when (and (equal (cdr p1) (cdr p2)) (not (eq (car p1) (car p2))))
			      (return 'delete)))
	collect p1))

(defun summarize-qresult (r*)
  (if r*
      (loop with n-made = 0
	    with n-good = 0 
	    with n-bad = 0
	    for ((q+- a b) r) in r*
	    do 
	    (cond ((null r) 0)
	      ((progn (incf n-made) nil)) ; kludge to force incf
	      ((and (eq 'q- q+-)
		    (eq '- r)) (incf n-good))
	      ((and (eq 'q+ q+-)
		    (eq '+ r)) (incf n-good))
	      (t (incf n-bad)))
	    finally (return (/ (float n-good) n-made))
	    )
    0.0))

(defun analyze-qsim (integrated-predictions print?)
  (loop for (q+- a b) in integrated-predictions
	as xpr in integrated-predictions
	as result = (analyze-gene-pair a b)
	do (when print? (format t "~a = ~a~%" xpr result))
	collect (list xpr result)))

;;; Figure the qualitative correlation between a pair of genes.

(defun analyze-gene-pair (aname bname)
  (let ((a (assoc aname *d*))
	(b (assoc bname *d*)))
    (cond ((null a) nil)
	  ((null b) nil)
	  (t (let* ((ra* (correlate-genes a b))
		    (qra (q-correlation-result (getf ra* :r))))
	       qra)))))

(defvar *xcr-cutoff* 0.3) ; this is the wrong threshold (it's based on n=32 which is historical!) but it's
                          ; what was used for pat's results.  Oh well.
;(defvar *xcr-cutoff* 0.81) ; for directional hypotheses at N=5 (from http://faculty.vassar.edu/~lowry/ch4pt1.html)

(defun q-correlation-result (r)
  (cond ((and (> (abs r) *xcr-cutoff*) (> r 0)) '+)
	((and (> (abs r) *xcr-cutoff*) (< r 0)) '-)
	(t '0)))

;;; Gene correlation is cached in the gene-correlations-table, since it never changes!

(defvar *gctbl* (make-hash-table :test #'equal)) ; gene-correlations-table

(defun correlate-genes (g1 g2)
  (let* ((index (cons g1 g2))
	 (cached-value (gethash index *gctbl*))
	 )
    (or cached-value
	(let ((value (correlate (cdr g1) (cdr g2))))
	  (setf (gethash index *gctbl*) value)
	  value))))

;;; ------------------------------------------------------------------------------------------------
;;; Data loading and cleaning.

(defun chipload (filename)
  (setq *d* nil)
  (errorset
   (with-open-file (s filename)
     (loop for entry = (read s nil nil)
	   until (null entry)
	   do (push entry *d*)))))

;;; There's something highly weird in the data, esp. in the second 
;;; and 4th elements, so I'm hacking these to the mean of the other three.
;;; This sounds bogus, but it's the same as outlier replacement,
;;; I just eyeballed the outliers.  Anyhow, this won't be needed
;;; when I put the latest chip data in. 

(defun outlier-filter (d)
  (let ((m (mean (list (first d) (third d) (fifth d)))))
    (list (first d) m (third d) m (fifth d))))

;;; Precalculate the new chip values, meaned and outlier-filtered

(defun rd ()
  (loop for d in *d*
	do (rplacd d (outlier-filter (mapcar #'mean (nthcdr 4 d))))))

;;; ------------------------------------------------------------------------------------------------
;;; Various random utilities.

;;; Find some negative correlations ... please!?

(defun scorr (&aux (+ 0) (- 0) (z 0))
  (setq *-* nil)
  (loop for g1 in (mapcar #'car *d*)
	do (print g1)
	(push (list g1) *-*)
	(loop for g2 in (mapcar #'car *d*)
		 as v = (analyze-gene-pair g1 g2)
		 do 
		 (case v 
		       (+ (incf +)) 
		       (- (push g2 (cdar *-*))
			  (incf -)) 
		       (t (incf z)))))
  (format t "+ = ~a, - = ~a, z = ~a, sum = ~a~%" + - z (+ + - z)))

(defun pd ()
  (with-open-file (o "c:\\temp\\d.txt" :direction :output :if-exists :supersede)
  (loop for d in *d*
	as n = (car d)
	as v = (cdr d)
	do 
	(format o "~a~a" n #\tab)
	(loop for v in v do (format o "~a~a" v #\tab))
	(format o "~%")
	)))

;;; These are required in order for the dicovery engine to be able to do EQ tests on the model, which is understand
;;; all in the user package.

(import 'xr 'user)
(import 'q+ 'user)
(import 'q- 'user)
(import 'life 'user)
