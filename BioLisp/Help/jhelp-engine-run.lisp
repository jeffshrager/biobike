;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-
(in-package :help)
;;  Copyright (c) 2011, 2012  DragonLord Enterprises, Inc.

;;  User Interface to runtime:
;; (defun  jhelp-run (probe-list)  is a top-level debugging interface that is a synonym for...
;; (defun  JHelp-Run-Engine-Get-JDocuments  ( probe-list )     ;; probe-list is "list" "of" "words" "without" "puctuation" .
;; The Main Guts of the "Google Search" runtime system.
;; Runs the engine.
;; Sucks in a probe, which is the query that the user is coming in with.
;; Currently searches on both words, and significant bi-grams.
;; Comes up with a simple (ranked) list of the top JDocument integer ID codes, with scores above 1.0.
;; (Note that we're now throwing away the scores right before we return the list.)
;; A higher-level system is needed to wrap this, look up the Documents, display each document's summary, and Do The Right Thing.

;; Now in jhelp-control-panel.
;;(defparameter *jhelp-threshold*  0.20)  ;; was 1.0.  Mostly kill off any scores below this, if you have something better than this.
;;(defparameter *jhelp-div-threshold* 4)  ;; also kill anything with score less than top / div.


#|
Jan 27 '12  J.Myers.  truncate-JSummary-results was testing best score nil against threshold number
  and crashing out with numeric test on not-a-number.  Fixed.  threshold must be a number too.
Jan 28 '12  J.Myers.  Hacked in handling of punctuation, then special punctuation, for probe.
Jun 09 '12  J.Myers.  Stem searches.

There are two different ways to handle salience (specificity).  
One way is to compute it as a separate layer on load initialization, 
which gives faster run times, but restricts initialization to be run only once.
The second way is to compute it at run-time as a modification to score addition.
This allows incremental loading of additional help pages, but is more complex
and harder to maintain.
Here we go for ease of maintenance, which also gets us faster run-time too.
Oops.  We need to be able to switch this at run-time, for testing, not just at load-time.
So you get the run-time version.

Displaying of the results is done magically using inductance coils through routine wb::out-record-to-html 
in help-single.lisp, note not jhelp.  This invokes (display-single-word-jhelp-results form).
|#


(defun  jh-salience-adjust  (Barrel)
;; Make and return a temp copy of the barrel that's weighted by the number of entries in the barrel.
  (let* ((count (max 1 (JBarrel-count Barrel)))
           (result-Barrel   (make-JBarrel :entries 
      (loop for entry in (JBarrel-entries Barrel)
	collecting (cons (car entry) (* 25 (/ (cdr entry) count)))  into result
	finally (return result))
 	:count count)))

	result-Barrel
))

(defun merge-JSummary-barrels (summary)
;;  summary is a JSummary object that we're operating on.
;; This is a cute function that MERGES all of the barrels in a JSummary into one effective long barrel list.
;; Input is the list of barrels found inside the given summary.
;; Side-effect output is the results copied into the "results" of the given summary.
;; Remember that the JSummary owns the results, which must be copies, but it does not own the barrels.
;;
;; As an added bonus on the merge, the Barrel-Entries are flipped so that the scores are in front for the results.
;;
;;  This is done in one big whoosh, not incrementally, for efficiency.
;;  A different design would use an anytime algorithm, and then choke it off when out of time;
;;  but no need to get fancy in such a small system here.
;;  There should be only a couple hundred matches at most.

          (when (>= *jhelp-debug* 5)
	  (format T "Merging..."))
          (when (>= *jhelp-debug* 8)
	  (format T "sorting..."))

  ;;First, sort the barrels from shortest barrel to longest barrel. 
  ;;This is important to keep the triangle numbers low on the O(length * 0.5 *barrelscount^2) for the merge,
  ;; which is the reason why the JSummary is a separate object in the first place.
  ;; In practicality, when done this way, it's not all that bad.
  (setf (JSummary-barrels summary)
          (sort (JSummary-barrels summary) #'< :key #'JBarrel-count))  ;;use the length of the Barrel in comparison.
   ;; number of barrels is roughly two or three times word-length of entry probe sentence, maybe around 10,
   ;; so this sort's pretty much trivial.


          (when (>= *jhelp-debug* 8)
	  (format T "sort finished, interleave merging..."))

   ;; Now peel through the barrels.  We're doing the Merge.  ADD similar barrel entries, *interleave* dissimilar barrel entries.
  (loop for realBarrel in (JSummary-barrels summary)     ;;Barrel is a temp var here.  We destructively modify it, but not the original barrel list, which is const.
 ;;    with results = nil	;;can't use this for collecting inside second nested loop, as it gets shadowed.
     for barrel = (JBarrel-entries realBarrel)
     with oldresults = ( )
     with entry = nil
     do     (loop                  ;;this is a primitive tricky loop, we are iterating and merging two lists together and so we loop until done.
	while (or barrel oldresults)
	collecting   ; to suck it onto the back of the list.  Don't use push, it would reverse order.

		;;Note:
		;; barrel is undigested, in the form                             ((JDocIDint . score)  ...)
		;; whereas oldresult is already digested, in the form  ((score . JDocIDint)  ...)
		;; We are going after and merging by *JDOC ID*--both lists have already been sorted.

	     (if (and barrel oldresults)    ;;Are they BOTH still here?     ..Note:  First time round, oldresults will be empty.


		(if   (> (caar barrel)  (cdar oldresults))     ;; oldresults less, pop it off.
			(progn
				;  (setq entry (car oldresults) )   ;;no new memory alloc.  This has been cooked already, don't twiddle it.
				;  (setq oldresults (cdr oldresults))       ;;Pop oldresults.
				;  entry               ;;Return the new cons, to be gathered.
 				(pop oldresults)
			)
		;else
		  (if  (< (caar barrel)  (cdar oldresults))     ;; barrel less, pop IT off.
			(progn
				(setq entry (cons  (cdar barrel)  (caar barrel)))    ;;new memory alloc.  Cook it (swap). Entry:  (score.JDocIDint).
				(setq barrel (cdr barrel))    ;;Pop barrel.
				entry                                 ;;Return the new cons, to be gathered.
			)
		;else
		   (if  (equal (caar barrel)  (cdar oldresults))     ;; Same ID number.  Add results, pop BOTH.
			(progn
				(setq entry (cons   (+ (cdar barrel) (caar oldresults))   (caar barrel)))    ;;new memory alloc.
				(setq oldresults (cdr oldresults))     ;;Pop both oldresults
				(setq barrel (cdr barrel))                   ;;and also pop barrel, eventually down to ().                                                                                                                                                                                                                                                                                                                  
				entry                                                 ;;Return the new cons, to be gathered.
			)
		;else
		 (progn (format T "Bad entries, could not compare IDs for ~a and ~a."  (caar barrel)  (cdar oldresults))
	                             (cons 0 0))    ;error cons to return.  You Should Never See This.
		)))


	    ;else ...Here, we have RUN OUT of either barrel OR oldresults.  Figure out which, then simply copy the other in.
                    ;
                    ; This is slightly not so good because this should probably be outside the loop, not inside it,
                    ; but for the barrel side we have to go through and cons up a swap entry anyway, so we're not losing anything with the loop;
                    ; and for the oldresults side, it's simply a few "ands" and "ifs" more per entry.
                    ; Save the optimization for version six.

	     (if barrel
	   
                           ;; continue through barrel... we are still in the middle of a "collecting"...
	           ;;  on the barrel side, 
		(progn
			(setq entry (cons  (cdar barrel)  (caar barrel)))    ;;new memory alloc.  Cook it (swap). Entry:  (score.JDocIDint).
			(setq barrel (cdr barrel))    ;;Pop barrel.
			entry                                 ;;Return the new cons, to be gathered.
		)

                          ;;else

                            ;;continue through oldresults...we are still in the middle of a "collecting"...
		(pop oldresults)

	  ) ;; end if  of continue through whichever one is left over
             )   ;; end if  of "and barrel oldresults", compute the entry to collect

	into localresults     ;;autodeclared, shadows previous var.  Oops.

	finally (setq oldresults localresults)       ;; we have finished with one barrel.
        )   ;;End of merging / simple loop through one barrel and one oldresults. 


	finally (setf (JSummary-results summary) oldresults)
    )  ;;End of loop through all barrels, 

          (when (>= *jhelp-debug* 5)
	  (format T "Merge finished.~%"))

    ; JSummary object's barrels can be safely GC'd at this point.  Kept around for debugging.

  ;This action function is a side-effect, it returns the new list in the summary's results slot.
	
)



(defun rank-JSummary-results (summary)
;;  summary is a JSummary object that we're operating on.
;; This function ranks the JSummary results by re-sorting its list.
;; Input is the results found inside the given summary.    in the form  ((score . JDocIDint)  ...).
;; Side-effect output is the new results, ordered highest to lowest scores.
;;
          (when (>= *jhelp-debug* 5)
	  (format T "Ranking..."))

  (setf (JSummary-results summary)
          (sort (JSummary-results summary) #'> :key #'car))  ;;use the front of the item in comparison.

          (when (>= *jhelp-debug* 5)
	  (format T "Rank finished.~%"))
)



(defun truncate-JSummary-results (summary)
;;  summary is a JSummary object that we're operating on.
;; Here we winnow the results down to the top best.
;; "WE ALSO CORRECTLY THROW AWAY THE SCORES/ASSOC-LIST STRUCTURE, LEAVING ONLY A LIST OF THE DOC ID CODES.
;; THIS CHANGES THE REPRESENTATION AT THE LAST STEP."  Naw, I want to see the scores.  Take this out again later.
;; ...most of the numbers in this routine are heuristic, feel free to tune these at will.

         (when (>= *jhelp-debug* 5)
	  (format T "Truncating..."))

  (let ((best-result-score (caar (JSummary-results summary)) ))
   (if (and best-result-score *jhelp-threshold* (> best-result-score *jhelp-threshold* ))   ;Is the first result reasonable?    ...note:  caar is nil-safe.

	;Yes.  Return everything above 1.0 threshold, stopping at 100 entries. But at least return the top 5.  Unless ridiculous.
      (loop for i from 1 to 100				; Stop the list at 100 max.
	for entry in (JSummary-results summary)	; Go through the list and grab everything that's > 1.0.
	; "Pick up either everything that's above 1.0, or if really short, the first five that are above 0.1."
	while (or (>  (car entry)  (max (/ best-result-score *jhelp-div-threshold*) *jhelp-threshold*))	
		  (and (< i 6) (>(car entry) (* 0.1 *jhelp-threshold*))) )   ; Kills loop upon first negative, which is desired.
;;	collecting (cdr entry) into newresults
	collecting  entry into newresults
	finally (setf (JSummary-results summary) newresults) )    ; Now save the truncated list.

      ;  Else,
	; No.  We are lost in space, no really good matches.  Simply return the first 100.
      (loop repeat 100				; Stop the list at 100 max.
	for entry in (JSummary-results summary)	; Go through the list and grab everything that's > 1.0.
;;	collecting (cdr entry) into newresults
	collecting entry into newresults
	finally (setf (JSummary-results summary) newresults)     ; Now save the truncated list.
       ) ;loop
      ) ;if
    ) ;let
          (when (>= *jhelp-debug* 5)
	  (format T "Truncate finished.~%"))
)





(defun  JHelp-Run-Engine-Get-JDocuments  ( probe-list )     ;; probe-list is "list" "of" "words," "some-having" "puctuation?" .
;; The Main Guts of the "Google Search" runtime system.
;; Runs the engine.
;; Sucks in a probe, which is the query that the user is coming in with.
;; Currently searches on both WORDS, and also significant BI-GRAMS.
;; Comes up with a simple (ranked) list of the top JDocument integer ID codes, with scores above 1.0.
;; (Note that we're now throwing away the scores right before we return the list.)
;; A higher-level system is needed to wrap this, look up the Documents, display each document's summary, and Do The Right Thing.

;; ...we need to do the cleaning twice, once for the actual run, once for the words used for searching for context,
;; because the bigrams almost certainly need a raw probe-list without the tiny-words and stems corrupting it.
;;  Sorry.

  ;; Create a JSummary object to hold the results.
  	(let ((summary (make-JSummary :results nil  :barrels nil)))

          (when (>= *jhelp-debug* 1)
            (format T "JHelp searching on: "))

	;;First, we fix the probelist so it actually takes out the punctuation it should have already.
	;; Put it all back together again, then tear it apart, correctly this time.
	(setq probe-list (split-at-puctuationwhitespace 
   	 (apply 'concatenate 'string
  		 (loop for x in probe-list 
               		collecting (format nil "~a " x)))))


  ;; Loop through the words first, collecting their barrels
	(loop for big-word in probe-list 
	with barrel do
      	 (let* ((words (split-at-specialpunctuationspace big-word))
	         (tiny-word-stems (loop for tword in words with stem 
				when (and (setq stem (find-stem tword)) (>= (length stem) 3))
				collecting stem)
		)
	         )
	   (when (> (length words) 1)
	     (push big-word words))

	(loop for word in (append words tiny-word-stems) do
	(when    (or (<= (length words) 1)
		(not (isa-jhelp-stop-wordp word))  )
	  ;;actually this should not be necessary, as there should be no barrels for stop words in the first place.
	  ;; For some reason we are getting some leakage so far, though.


                 (when (>= *jhelp-debug* 1)
	    (format T "?~s " word))

	(when (setq barrel (gethash word (JHelp-Engine-wordhash *JHelp-Engine*)) )
		(push 
			(if *jhelp-use-salience*
				(jh-salience-adjust barrel)
			        barrel  )
		  (JSummary-barrels summary)
		)
	)))))

  ;; Now loop through the bigrams, collecting their barrels
	(loop repeat (- (length probe-list) 1 )  ; Fence-post count. Stops it one early.
	for word in probe-list 
	for word2 in (cdr probe-list)
	with barrel with bigram do
	(setq bigram (concatenate 'string   word  " "  word2))
	(when (setq barrel (gethash bigram (JHelp-Engine-wordhash *JHelp-Engine*)) )
                (when (>= *jhelp-debug* 1)
                  (format T "?~s " bigram))
		(push 	(if *jhelp-use-salience*
				(jh-salience-adjust barrel)
			        barrel  )
		  (JSummary-barrels summary)
		)
	))

  ;; Do the same for concepts, etc.
  ;;  Put in expansion here.

          (when (>= *jhelp-debug* 1)
	   (terpri))

  ;; MERGE the results
	(merge-JSummary-barrels summary)

  ;; Rank the results--in an industrial version, these three steps might be combined.  Here maintenance is more important.
	(rank-JSummary-results summary)

  ;; Truncate--skim off the good ones
	(truncate-JSummary-results summary)   ;;Coming into this, the results list is an assoc list of (score . JDocIDint) .

  ;; Return what we've got.
	(JSummary-results summary)    ;;"At this point the list is now a simple list of the top JDocument ID codes." no we're going with (score.ID) pairs.
	) ;let
)

(defun  jhelp-run (probe-list)     ;;Only called by user.  A handy debugging handle.
  (JHelp-Run-Engine-Get-JDocuments   probe-list )
)


;;called from (multiple-word-jhelp words) in jhelp-multiple.lisp
