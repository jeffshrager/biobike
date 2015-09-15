(defparameter *model-graph*		; (fig 1b)
  '((size + cln3)
    (cln3 + mbf)
    (cln3 + sbf)
    (sbf + cln12)
    (cln12 - sic1)
    (cln12 - cdh1)
    (sic1 - clb56)
    (sic1 - clb12)
    (mbf + clb56)
    (clb56 + mcm1/sff)		     ; weird yellow/green in the paper
    (clb56 - sic1)
    (clb56 + clb12)		     ; weird yellow/green in the paper
    (clb56 - cdh1)
    (cdh1 - clb12)
    (clb12 - cdh1)
    (clb12 + mcm1/sff)
    (clb12 + cdc20&14)
    (clb12 - sic1)
    (clb12 - swi5)
    (clb12 - mbf)
    (clb12 - sbf)
    (mcm1/sff + cdc20&14)
    (mcm1/sff + swi5)
    (mcm1/sff + clb12)
    (cdc20&14 + swi5)
    (cdc20&14 + cdh1)
    (cdc20&14 - clb12)
    (cdc20&14 + sic1)
    (cdc20&14 - clb56)
    (swi5 + sic1)
    ))

(seegraph *model-graph*)

(defparameter *proteins*
  (loop for (p1 +- p2) in *model-graph*
	with proteins = nil
	do (pushnew p1 proteins)
	(pushnew p2 proteins)
	finally (return proteins)))

(defvar *protein->current-state* (make-hash-table :test #'equal))

(compile
(defun litest (&key (initially-on-proteins '(size))
                    (cycles-to-run 50))

  ;; Initialize the current states of all the proteins:
  (initialize-current-states)

  ;; Turn on a set of initial proteins:
  (set-proteins initially-on-proteins 1)

  ;; Print out a header for the report:
  (format t "~8,f" "Cycle")
  (loop for p in *proteins* do (format t "~9,f" p)) 
  (format t "~%")

  ;; Report the initial state of things:
  (report-states 0)

  ;; Run one cycle of the simulation, and report again:
  (run-one-cycle)
  (report-states 1)

  ;; Turn off the initial proteins:
  (set-proteins initially-on-proteins 0)

  ;; Run the simulation reporting as we go:
  (loop for cycle from 2 to cycles-to-run
	do (run-one-cycle)
	(report-states cycle)
	)				; end loop

  ))					; end defun

(compile
(defun set-proteins (proteins state)
  (loop for protein in proteins
	do (setf (gethash protein *protein->current-state*) state))))

(compile
(defun report-states (cycle)
  (format t "~8,f" cycle)
  (loop for p in *proteins*
        do (format t "~9,f" (gethash p *protein->current-state*)))
  (format t "~%")
  ))

(compile
(defun run-one-cycle ()
  (zero-sum-influences)
  (add-up-influences)
  (change-state)
  (do-self-degradation)
  ))

(defvar *protein->sum-influences* (make-hash-table :test #'equal))

(compile
(defun zero-sum-influences ()
  (loop for p in *proteins*
        do (setf (gethash p *protein->sum-influences*) 0))))

(compile
(defun add-up-influences ()
  (loop for (p1 +- p2) in *model-graph*
        do (case +-
             (+ (incf (gethash p2 *protein->sum-influences*)
		      (gethash p1 *protein->current-state*)))
             (- (decf (gethash p2 *protein->sum-influences*)
		      (gethash p1 *protein->current-state*)))
             ))))

(compile
(defun change-state ()
  (loop for p in *proteins*
        as sum-influence = (gethash p *protein->sum-influences*)
        do (cond ((> sum-influence 0)
                  (setf (gethash p *protein->current-state*) 1))
                 ((< sum-influence 0)
                  (setf (gethash p *protein->current-state*) 0))
                 ))))

(defparameter *self-degrading-proteins*
  '(cln3 cln12 mcm1/sff swi5 cdc20&14))

(compile
(defun do-self-degradation ()
  (do-self-degradations)
  (update-degradation-histories)))

(defvar *protein->history* (make-hash-table :test #'equal))

(compile
(defun do-self-degradations ()
  (loop for p in *self-degrading-proteins*
        when (time-to-degrade? p)
        do (format t "Self-degrading ~a~%" p)
	(setf (gethash p *protein->current-state*) 0))))

(defvar *degrade-after* 5)

(compile
 (defun time-to-degrade? (p)
   "If we find 5 (1 0) at the head of the history, return T, else NIL."
   (let ((history (gethash p *protein->history*)))
     (when (> (length history) 4) ; Make sure that there are at least 5 entries
       (loop for history-entry in history
	     as time from 1 to *degrade-after*
	     when (not (equal '(1 0) history-entry))
	     do (return nil)
	     finally (return t))))))

(compile
(defun update-degradation-histories ()
  "Add each protein's current state to the history."
  (loop for p in *proteins*
        do (push (list (gethash p *protein->current-state*)
                       (gethash p *protein->sum-influences*))
                 (gethash p *protein->history*))
        )))

(compile
(defun initialize-current-states ()
  "This is only done at the beginning of the run."
  (loop for p in *proteins*
        do
        (setf (gethash p *protein->current-state*) 0)
        (setf (gethash p *protein->history*) nil)
        )))


(compile
(defun litest-pumped (&key (initially-on-proteins '(size))
                           (cycles-to-run 50)
                           (pump-frequency 50))
  (initialize-current-states)
  (set-proteins initially-on-proteins 1)
  (format t "~8,f" "Cycle")
  (loop for p in *proteins* do (format t "~9,f" p)) 
  (format t "~%")
  (report-states 0)
  (run-one-cycle)
  (report-states 1)
  (set-proteins initially-on-proteins 0)
  (loop for cycle from 2 to cycles-to-run
	do (run-one-cycle)
	(report-states cycle)
	(when (zerop (mod cycle pump-frequency))
	  (format t "Pumping ~a~%" initially-on-proteins)
	  (set-proteins initially-on-proteins 1)
	  (run-one-cycle)
	  (report-states (incf cycle))
	  (set-proteins initially-on-proteins 0)
	  )				; end when
	)				; end loop

  ))					; end defun

; (litest :cycles-to-run 50)
; (litest-pumped :cycles-to-run 500 :pump-frequency 50)

(print "The code from live tutorial #5: Regulation Simulation, has been loaded and compiled!")
