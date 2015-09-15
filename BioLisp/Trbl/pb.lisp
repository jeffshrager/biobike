(in-package :pb)

(defvar *tmptable* (make-hash-table :test #'equal))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(rank-pub-year-table-by-pub-count
	    rank-pub-year-table-by-edge-abstraction
	    plot-edge-dynamics
	    map-edge-dynamics
	    *go-stops*
	    iterative-cognates-graph
	    report-cognates-iteratively
	    map-cognates-iteratively
	    ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Concepts with more than this number of genes are not analyzed.

(defvar *go-stops-limit* 500) 
(defvar *go-stops* nil)

(defun ensure-go-stops ()
  (if *go-stops* 
      (cformatt "There are ~a entries in the *go-stops* " (length *go-stops*))
    (setup-go-stops)))

(defun setup-go-stops (&key (limit *go-stops-limit*))
  (cformatt "Setting up *GO-STOPS* with limit = ~a" limit)
  (setq *go-stops* 
	(loop for frame in *go-frames*
	      when (> (length (#^go.related-genes frame)) limit)
	      collect frame))
  (cformatt "There are ~a entries in the *go-stops* " (length *go-stops*))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Each entry in the *pairs-table* is a pair of go concepts
;;; represented in a given publication, and the list of those
;;; publications.  The result of this is huge as it is setting up
;;; EVERY pairing!  Be sure to make the *go-stops* limit VERY low,
;;; like in the hundreds, in order to reduce the size of these
;;; analyses!

(defun go-pairs-per-winyears (&key (winsize 3) (winskip 3) (do-only 2))
  (loop for year-frame-set in (windowize *year-frames* winsize winskip)
	as kount from 1 to do-only
	collect 
	(list year-frame-set
	      (sort
	       (loop for key being the hash-keys of (go-pairs-pubs-table
						     (loop as  yf in year-frame-set
							   append (#^pubs yf)))
		     using (hash-value pubs)
		     collect (cons (length pubs) key))
	       #'> :key #'car))))

;;; (go-pairs-pubs-table (loop as k from 1 to 6 for  yf in  *year-frames* append (#^pubs yf)) :really-run-without-a-target? t)
;;; Results in a hash table containing 13,000 entries.

(defun go-pairs-pubs-table (pub-list
				&key (go-target-concept nil) 
				&aux (table (make-hash-table :test #'equal :size 10000)))
  #.(one-string-nl
     "Given a pub-list this will return a table where the keys are consed pairs of go nodes (limited to the go fringe)"
     "and the value is the set of pubs that refer to that pair.  NOTE THAT THE PAIRS ARE NON-REDUNDANT -- THAT IS, YOU'LL"
     "ONLY SEE (A . B) not both that and (B . A)"
     )
  (unless (listp go-target-concept) (setq go-target-concept (list go-target-concept)))
  (loop for pub in pub-list
	as genes = (#^genes pub)
	when (> (length genes) 1) ; Don't bother unless there's a pair.
	do 
	(loop for (g1 g2) in (all-unordered-pairs genes)
	      as go1s = (#^fringe-go-nodes g1)
	      as go2s = (#^fringe-go-nodes g2)
	      do 
	      (loop for go1 in go1s
		    when (not (member go1 *go-stops* :test #'eq))
		    do 
		    (loop for go2 in go2s
			  when 
			  (and (not (member go2 *go-stops* :test #'eq))
			       (not (eq go1 go2))
			       ;; If there is a target concept, one of the gos must be in the list,
			       ;; otherwise (if no taget is given) just return t.
			       (if go-target-concept
				   (or (member go1 go-target-concept :test #'eq)
				       (member go2 go-target-concept :test #'eq))
				 t)
			       )
			  do
			  (let ((key (or (let ((key (cons go1 go2))) (when (gethash key table) key))
					 (cons go2 go1))))
			    (pushnew pub (gethash key table)))
			  ))))
  table)

;;; User-callable fns.

(defun rank-pub-year-table-by-pub-count (year-frame)
  (sort (lmaphash (lambda (k v) (list (length v) k)) (#^go-pairs-table year-frame))
	#'> :key #'car))

(defun rank-pub-year-table-by-edge-abstraction (year-frame) 
  (sort (lmaphash (lambda (k v) (list (+ (length (#^go.related-genes (car k)))
					 (length (#^go.related-genes (cdr k))))
				      (list k v)) )
		    (#^go-pairs-table year-frame))
	#'> :key #'car))

(defun test ()
  (trace-single-go-edge-dynamics
   #$GO.ProteinSerine/ThreonineKinaseActivity
   #$GO.UbiquitinLigaseComplex
   #'(lambda (go1 go2 publist year-frame)
       (declare (ignore go1 go2))
       (float (/ (length publist) (length (#^pubs year-frame)))))
   ))

;;; The value extractor must accept four args: Two go frames, a
;;; list of publications related to the given edge.  Whatever it
;;; returns is collected and returned.

(defun trace-single-go-edge-dynamics (go1 go2 value-extractor)
   (loop for year-frame in *year-frames*
	 as pubs = (go-edge-in-year go1 go2 year-frame)
	 collect (funcall value-extractor go1 go2 pubs year-frame)))

(defun go-edge-in-year (go1 go2 year-frame)
  (let ((table (#^go-pairs-table year-frame)))
    (and table
	 (or (gethash (cons go1 go2) table)
	     (gethash (cons go2 go1) table)))))

;;; Give this a year that serves as the source model.  Returns, e.g.:
;;; ((#$GO.Arp2/3ProteinComplex . #$GO.StructuralConstituentOfCytoskeleton)
;;;    (0.0 0.0 8.05153e-4 0.0))
;;; Where the car is the edge, and the list of numbers is the
;;; fraction of papers in each year (using pb::*year-frames*)
;;; sourced for this edge.

(defun plot-edge-dynamics (go-target-concept &key (n-to-plot 5))
  #.(one-string-nl 
     "Plots the result of the top n-to-plot (default 5) from map-edge-dynamics"
     "Example: (plot-edge-dynamics #$GO.ResponseToInsects)"
     )
  (let ((result (first-n n-to-plot (map-edge-dynamics go-target-concept))))
    (line-plot (mapcar #'second result) 
	       :labels 
	       (loop for (pair nil) in result
		     collect (if (eq (car pair) go-target-concept)
				 (cdr pair)
			       (car pair))))))

(defun report-edge-dynamics (go-target-concept &key (n-to-report 10))
  #.(one-string-nl 
     "Returns the result of the top n-to-plot (default 10) from map-edge-dynamics"
     "for excel plotting"
     "Example: (pretty-edge-dynamics #$GO.ResponseToInsects :n-to-report 30)"
     )
  (format t "go1	go2")
  (loop for yf in *year-frames*
	do (format t "~a	" (#^year yf)))
  (format t "~%")
  (loop for ((go1 . go2) map) in (first-n n-to-report (map-edge-dynamics go-target-concept))
	do (format t "~a	~a" go1 go2)
	(loop for value in map
	      do (format t "	~a" value))
	(format t "~%")))
    
#+wont-work-any-longer-bcs->set-up-go-pairs-per-year-tables<-has-changed
(defun map-edge-dynamics (go-target-concept)
  #.(one-string-nl 
     "Maps the cognates to the target concept(s) over time."
     "Example: (map-edge-dynamics #$GO.ResponseToInsects)."
     "Results are ranked by the sum of the relative number of entries in each year."
     "(Note that this could miss the absolute max!)"
     )
  (set-up-go-pairs-per-year-tables :go-target-concept go-target-concept)
  (let ((year-pub-counts (loop for year-frame in *year-frames*
			       collect (cons (#^year year-frame)
					     (length (#^pubs year-frame))))))
    (sort 
     (lmaphash (lambda (pair year-pub-structs) 
		 (list pair
		       (loop for year-frame in *year-frames*
			     as year = (#^year year-frame)
			     as year-pub-struct = (find (#^year year-frame)
							year-pub-structs
							:key #'pair-year-year)
			     collect (if year-pub-struct
					 (float (/ (length (pair-year-pubs year-pub-struct)) (cdr (assoc year year-pub-counts))))
				       '0.0))))
	       *pairs-table*)
     #'> :key #'(lambda (key/map) (apply #'+ (second key/map))))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; How many new GO concepts are referred to in each year?

(defun count-new-go-concepts (&optional (year-frames *year-frames*))
  #.(one-string-nl 
     "Create #^first-pub-year in the go frames in the #^go-id slots of the genes represented by genes"
     "in the pubs for that year. If the go frame already has a #^first-pub-year entry, then it is NOT"
     "replaced.  You end up with each go concept being annotated with its first year of mention."
     "This returns the list of years and counts of go frames thusly annotated for that year."
     "This is like historicalize-go-per-gene, but does not recurse up the GO hierarchy."
     )
  (loop for frame in *go-frames* do (setf (#^first-pub-year frame) nil))
  (loop for year-frame in year-frames
	as year = (#^year year-frame)
	do (loop for pub in (#^pubs year-frame)
		 do (loop for gene-model in (#^genes pub)
			  do (loop for go-concept in (remove nil (#^go-id gene-model))
				   unless (#^first-pub-year go-concept)
				   do (setf (#^first-pub-year go-concept) year)))))
  (loop for year-frame in year-frames
	as year = (#^year year-frame)
	collect (print (list year (count-if (lambda (gof) (vwhen (x (#^first-pub-year gof))
						     (= year x)))
				*go-frames*))))
  )
	      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 20050212 Per Gene Discovery

(defun historicalize-go-per-gene ()
  #.(one-string-nl
     "Walk up the GO from each gene's linked GOIDs, and assign the #^per-gene-discovered-year in the node and all"
     "its ISA parents, unless is's been assigned a year already.  This is like count-new-go-concepts, but recurses"
     "up through the GO hiearchy."
     )
  ;; Remove previous assignments, if any.  (Pre cleaning for multiple calls.)
  (loop for go in *go-frames*
	do (setf (#^per-gene-discovered-year go) nil))
  (loop for year-frame in *year-frames*
	do (setf (#^gos-discovered-this-year-per-gene year-frame) nil))
  ;; Do this for each year.
  (loop for year-frame in *year-frames*
	do (loop for pub in (#^pubs year-frame)
		 do (loop for gene in (#^genes pub)
			  do (loop for go in (#^go-id gene)
				   when go
				   do (recursively-assign-year go year-frame))))))

(defun recursively-assign-year (go year-frame)
  #.(one-string-nl
     "If the given GO frame already has an assigned #^per-gene-discovered-year, then"
     "we're done.  Otherwise, assign the given GO frame the given year in that slot,"
     "and then recuse up the #^IsA slot contents. The result is that all previously"
     "unassigned concepts up the IsA hierarchy are assigned the given year.")
  (unless (#^per-gene-discovered-year go)
    (setf (#^per-gene-discovered-year go) year-frame)
    (push go (#^gos-discovered-this-year-per-gene year-frame))
    (mapcar (lambda (parent) (recursively-assign-year parent year-frame))
	    (#^isa go))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Calculate the first year that a particular "true" pairing was
;;; mention in the literature.  A "true" pairing is defined as a pair
;;; of genes who share their deepest GO assocation's #^parents That
;;; is, suppose for example that GeneA has this #^parent slot in one
;;; of its GO bindings: (#$GO.Biological_Process
;;; #$GO.PhysiologicalProcess #$GO.Metabolism
;;; #$GO.Nucleobase,Nucleoside,NucleotideAndNucleicAcidMetabolism
;;; #$GO.DnaMetabolism), and that this is the longest (i.e., deepest)
;;; GO #^parents path of all its GO bindings.  If this is also the
;;; longest of another gene in the same paper, these are called
;;; "siblings."

;;; First we go through and assign each gene's longest GO path to the
;;; #^longest-go-path slot in the gene.  Actually, only the lastelt is
;;; assigned to make comparisons faster.  This is called the genes'
;;; #^defacto-go-assignments.  If there is a tie for longest, all of
;;; the tieants are included (therefore, #^defacto-go-assignments is a
;;; list!)

(defun setup-defacto-go-assigments ()
  #.(one-string-nl 
     "Concetually this is supposed to find the 'nearest' GO slots to this gene; That is,"
     "those that are only the immediate parents of the gene.  However, the heuristic that"
     "is used (last element of the #^parent field) isn't right."
     )
  (loop for gene in (#^gene-models #$arabidopsis_thaliana)
	as longest-parent-elts = nil 
	as length-of-longest-path = 0
	do 
;	(print gene)
	(loop for go in (remove nil (#^go-id gene))
	      as parent-path = (#^parents go)
	      as length-of-parent-path = (length parent-path)
	      do 
;	      (format t "Go = ~a, Parent-path = ~s, length-of-parent-path = ~a~%" go
;		      Parent-path length-of-parent-path)
	      (cond ((> length-of-parent-path length-of-longest-path)
;		     (format t "Replacing~%")
		     (setf longest-parent-elts (list (car (last parent-path)))
			   length-of-longest-path length-of-parent-path))
		    ((= length-of-parent-path length-of-longest-path)
;		     (format t "Pushing~%")
		     (pushnew (car (last parent-path)) longest-parent-elts))))
;	(format t "Assignment = ~s~%" longest-parent-elts)
	(setf (#^defacto-go-assignments gene) longest-parent-elts)
	(loop for go in longest-parent-elts
	      when go
	      do (push gene (#^defacto-assigned-genes go)))
	))

;;; Once we've done the above, we can ask whether a give paper refers to 
;;; a sibling pair -- that is, a pair that are co-parented.  This isn't a
;;; simple calculation!  This gives you a list of the coparents, if any.

(defun refers-to-any-sibling-pairs? (pub)
  (clrhash *tmptable*)
  (loop for goset in (mapcar #'#^defacto-go-assignments (#^genes pub))
	with coparents
	do (loop for elt in goset
		 if (gethash elt *tmptable*)
		 do (pushnew elt coparents)
		 else do (setf (gethash elt *tmptable*) t))
	finally (return coparents)))

(defun coparented-publications (pubs)
  (loop for pub in pubs
	as sib = (pb::refers-to-any-sibling-pairs? pub)
	when sib 
	collect (list pub sib)))

;;; And finally we're done... although, actually, we need to get the actual co-parented 
;;; pair, which means modifying the above!  UUUU

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-immediate-go-bindings ()
  #.(one-string-nl 
     "Sets the #^immediate-go-bindings slot in the given gene to"
     "contain only elements of GO-IDs from that gene that are not"
     "parents of other members.")
  (loop for gene in (#^gene-models #$arabidopsis_thaliana)
	do (let ((bindings (copy-list (#^go-id gene))))
	     (loop for go in bindings
		   as parents = (remove go (compute-transitive-slot go #$isa))
		   do (setf bindings (set-difference bindings parents)))
	     (setf (#^immediate-go-bindings gene) (remove-duplicates bindings)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Using cognate dynamics (map-edge-dynamics) to identify a constellation of 
;;; related "hot" go concepts.  The idea here is to begin with a seed concept
;;; and find the highest cognate concepts to it, and then use those to find
;;; the highest cognates again and again, and finally reporting a set of highly
;;; reporting cognate pairs.

(defun ic-graph (&key (seed-concept #$go.responsetoinsects) ; ic = iterative cognates
		      (n-iterations 10)
		      (n-to-keep-per-iteration 10)
		      (n-edges 20)
                      (year-frames *year-frames*)
		      )
  (let ((*year-frames* year-frames))
    (loop for ((go1 . go2)) in (map-ics :seed-concept seed-concept
                                        :n-iterations n-iterations
                                        :n-to-keep-per-iteration n-to-keep-per-iteration)
          with result = nil
          until (>= (length result) n-edges)
          do (pushnew `(,go1 -> ,go2) result :test #'equal)
          finally (return result))))

(defun report-ics (&key (n-to-report 10) ; iterative cogante set
			(seed-concept #$go.responsetoinsects)
			(n-iterations 10)
			(n-to-keep-per-iteration 10))
  (format t "go1	go2")
;  (loop for yf in *year-frames*
;	do (format t "~a	" (#^year yf)))
;  (format t "~%")
  (loop for ((go1 . go2) map) in 
        (first-n n-to-report 
                 (map-ics  :seed-concept seed-concept
                           :n-iterations n-iterations
                           :n-to-keep-per-iteration n-to-keep-per-iteration)
                 )
	do (format t "~a	~a" go1 go2)
	(loop for value in map
	      do (format t "	~a" value))
	(format t "~%")))

(defun map-ics (&key (seed-concept #$go.responsetoinsects)
		     (n-iterations 10)
		     (n-to-keep-per-iteration 10))
  (loop for it below n-iterations
	with seen-seeds = (list seed-concept)
	with results = nil
	until (null seed-concept)
	do 
	(let ((new-entries (first-n n-to-keep-per-iteration (map-edge-dynamics seed-concept))))
;	  (print (list it seed-concept))
	  (setq results (append results new-entries))
	  (setq seed-concept (find-new-seed new-entries seen-seeds))
	  (when seed-concept
	    (push seed-concept seen-seeds)
;	    (cformatt "New seed is ~a~%" seed-concept)
            ))
	finally (return (sort results #'> :key #'(lambda (key/map) (apply #'+ (second key/map)))))
	))

(defun find-new-seed (new-entries seen-seeds)
  (loop for ((new1 . new2)) in new-entries
	do 
	(cond ((and (not (member new1 seen-seeds))
		    (not (member new1 *go-stops*)))
	       (return new1))
	      ((and (not (member new2 seen-seeds))
		    (not (member new2 *go-stops*)))
	       (return new2))
	      )
	finally (progn (print "Can't find a new seed.")
		       (return nil))))

;;; Using iterative-cognates-graph and random GO concepts, report an average graph.

(defvar *go-activity-frames*
  (set-difference 
   (loop for frame in *go-frames*
	 when (and (search "activity" (#^fname frame) :test #'char-equal)
		   (#^Go.Related-Genes frame))
	 collect frame)
   *go-stops*))

(defun n-ic-graphs (&key 
		    (n-concepts-to-average 10)
		    (n-iterations 10)
		    (n-to-keep-per-iteration 10))
  (let ((nframes (length *go-activity-frames*)))
    (avgraphs (loop for seed-concept = (nth (random nframes) *go-activity-frames*)
		    as i from 1 to n-concepts-to-average
		    collect 
		    (ic-graph :n-iterations n-iterations 
			      :n-to-keep-per-iteration n-to-keep-per-iteration 
			      :seed-concept seed-concept)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface to Kazumi's FET code, and runs some simple experiments.  FET requires
;;; a set of tables in flat files (Ugh); These are mostly in the biotools/fet
;;; subdir, but we might possibly want to create them on the fly.  The first example
;;; does create such a file -- the URLID.genes file -- in order to run an experiment
;;; that fakes out FET to try different date combinations.

(defvar *years* (loop for i from 1991 to 2003 collect i))

(defun fet-with-date-xlations
       (alpha &optional (xlation (mapcar #'list *years* *years*)))
  (ensure-fet-url-masterlist)
  (with-temp-directory 
   (dir "tmp:" :delete? nil)
   (with-temp-file-in 
    (urlfile dir :name "URLS" :delete? nil)                
    (with-temp-file-in 
     (outfile dir :name "out" :delete? nil)           
     (with-open-file 
         (urlstream urlfile :direction :output)                  
       (print urlfile)
       (print outfile)
       (loop for (a b c) in (xlate-urllist xlation)
             do (format urlstream "~a ~a ~a~%" a b c)))
     (let ((fetdir (cl-user::translate-simple-lp "biotools:fet;"))
           (fet-command 
            (formatn "./fet ~a label-list.genes ~a WORDID.genes > ~a" 
                     alpha (namestring urlfile) (namestring outfile))))
       (case (protected-shell-command fet-command :directory fetdir)
         (:timeout nil)
         (otherwise
          (parse-fet-output outfile)
          )))))))
        
(defun parse-fet-output (outfile)
  (with-open-file 
   (i outfile)
   (loop for line = (read-line i nil nil)
	 until (search "Table prepared" line))
   (read-line i)
   (loop for line = (read-line i nil nil)
	 until (null line)
	 collect (let* ((split (string-split line #\tab)))
		   (list (read-from-string (first split)) ; id
			 (second split)	; gene name
			 (remove nil 
				 (loop for result in (cddr split)
				       collect (read-from-string (format nil "(~a)" 
									 (substitute #\space #\- (substitute #\space #\: result)))))
				 )))
	 )
   ))
			 
(defvar *urllist* nil)

(defun ensure-fet-url-masterlist ()
  (unless *urllist*
    (setq *urllist*
	  (with-open-file 
	   (i "/usr/local/biotools/fet/URLID.genes")
	   (loop for line = (read-line i nil nil)
		 until (null line)
		 collect (mapcar #'parse-integer (string-split line #\tab))
		 )))))

(defun xlate-urllist (xlations)
  (ensure-fet-url-masterlist)
  (loop for (a b date) in *urllist*
	as new = (assoc date xlations)
	unless new
	do (error "Missing xlation for ~a" date)
	collect (list a b (second new))))

;;; This is conceptually what you want to do, but the list is 
;;; way too big, so we use a random sampling aproach instead

#+nil
(defun all-year-combinations (&optional (years *years*))
  (cond ((null years) (list nil))
	(t (loop for year in years
		 append (mapcar #'(lambda (yl) (cons year yl))
				(all-year-combinations (remove year years)))))))

(defun n-random-year-combinations (n)
  (loop for i below n
	collect (loop with l = (length *years*)
		      for i below l
		      as j from l by -1
		      with list = *years*
		      as next = (nth (random j) list)
		      do (setq list (remove next list))
		      collect next)))

(defvar *fet-exp1-results-by-gene* (make-hash-table :test #'equal))
(defvar *fet-exp1-results-by-xlation* (make-hash-table :test #'equal))

(defun fet-run-xlation-or-n-combinations (&optional (xlation-or-n-combinations 10) (fet-alpha 0.01))
  (clrhash *fet-exp1-results-by-gene*)
  (clrhash *fet-exp1-results-by-xlation*)
  (loop for combination in (if (numberp xlation-or-n-combinations)
			       (n-random-year-combinations xlation-or-n-combinations)  
			     (list xlation-or-n-combinations))
	as xlation = (mapcar #'list *years* combination)
	as result = (fet-with-date-xlations fet-alpha xlation)
	do 
	(setf (gethash xlation *fet-exp1-results-by-xlation*) result)
	(loop for (nil gene results) in result
	      do 
	      (loop for result in results
		    do (pushnew (dexlate-fet-result xlation result) 
                                (gethash gene *fet-exp1-results-by-gene*)
				:test #'(lambda (a b) (or (and (= (car a) (car b)) (= (second a) (second b)))
							  (and (= (car a) (second b)) (= (second a) (car b))))))
	      ))))

(defun dexlate-fet-result (xlation result)
  (flet ((dexlate (year) (car (find year xlation :key #'second))))
    (list (dexlate (first result))
	  (dexlate (second result))
	  (third result)
	  (fourth result)
	  (car (fifth result)))))

;;; Now the hard part: Results analysis.

;;; Warming study:  Find genes that have progressive positive
;;; significant runs -- ie., they are warming.

(defun run-warming-study (limit)
  (fet-run-xlation-or-n-combinations *years* limit)
  (sort (find-warming-genes)  
	#'> :key #'second))

(defun find-warming-genes ()
  (let ((year-pairings (loop for (y1 y2) on *years* until (null y2) collect (cons y1 y2))))
    (loop for gene being the hash-key of *fet-exp1-results-by-gene*
	  using (hash-value results)
	  collect (list gene 
			(xlate-trend
			 (loop for y1.y2 in year-pairings
			       collect 
			       (find y1.y2 results
				     :test (lambda (a b) (or (and (= (car a) (car b)) (= (cdr a) (second b)))
							     (and (= (car a) (second b)) (= (cdr a) (car b)))))))))
	  )))

;;; ((1991 1992 14 3 0.001466) NIL NIL NIL NIL (1996 1997 9 1 0.006207) NIL NIL NIL NIL NIL NIL)
;;; ->
;;; (- NIL NIL NIL NIL - NIL NIL NIL NIL NIL NIL)

(defun xlate-trend (trend) 	  
  (max-run-length
   (loop for (from to nf nt nil) in trend 
	 collect
	 (when from
	   (let ((ddif (- to from))
		 (ndif (- nt nf)))
	     (if (plusp ddif)
		 (if (plusp ndif) '+ nil)
	       (if (plusp ndif) nil '+)))))))
				   
;;; Possibly useful utility to find runs of non-nils, and sort by
;;; their length.

(defun max-run-length (l)
  (reduce #'max (run-map l)))

(defun run-map (l)
  (loop with k = 0 for elt in
        l when elt collect
        (progn (incf k) k) else
        collect (progn (setq k 0) k)))


;;; List of links output format

;;; =====================================================
;;; Each entry in the links slot is a pair: (url display-string ...)
;;; anything in ... is ignored.

(defstruct listolinks title links)

(defmethod wb::out-record-to-html 
           ((obj listolinks) (string string) &key (pkg nil))
  (declare (ignore pkg))
  (html
   :br
   (:table
    (:tr
     (:th (:princ-safe (listolinks-title obj)))
     )
    (loop for (url string) in (listolinks-links obj)
          do
          (html 
           (:tr
            (:td
             ((:a :href url :target "_blank")
              (:princ-safe string))))))
    )))

(defun wicg (window &key (seed-concept #$go.responsetoinsects)) ; windowed ic graph
  (make-listolinks 
   :title (formatn "Windowed IC graphs from ~a" seed-concept)
   :links
   (loop for year+ on *year-frames*
         as wyears = (first-n window year+)
         as graph = (ic-graph :seed-concept seed-concept :year-frames wyears)
         until (< (length year+) window)
         collect (list (wb::jpg-path (seegraph graph))
                       (formatn "~a - ~a" 
                                (#^year (first wyears))
                                (#^year (lastelem wyears)))
                       graph
                       ))))

(defvar *author-color-tables* nil)

;;; This isn't so easy becase it has to be constrained not
;;; to walk through pubs that aren't in the given list.
;;; To avoid this, we premark all the value pubs' auth-search-tag
;;; and then use that both to indicate that a pub is :usable
;;; or :used

;;; Produces a set of hash tables.
;;; Each hash table contains a set of authors, all of whom are
;;; 'connected' to each other directly (by being co-authors of some
;;; publication in the input publications list) or indirectly,
;;; transitively through this co-author relationship.  (That is, if X and Y
;;; are co-authors on paper P1, and Y and Z are co-authors on paper P2,
;;; and X and Z are not co-authors on any paper, X and Z are connected
;;; to each other indirectly.

;;; So each hash table represents a cluster of authors who are related
;;; to each other as above.  The number of hash tables is the number
;;; of independent clusters of authors.

;;; (BTW, this has nothing in particular to do with colors!)

(defun color-authors (pubs)
  (setq *author-color-tables* nil)
  ;; Clear everything
  (loop for pub in pubs
        do 
        (setf (#^color-author pub) :useable)
        (loop for auth in (#^authors pub)
              do (setf (#^color-author auth) nil)))
  ;; Processes all pubs that haven't been touched
  (loop for pub in pubs
        if (eq :useable (#^color-author pub))
        do 
        (let ((curtab (make-hash-table :test #'eq)))
          (color-auths-2 (list pub) curtab)
          (push curtab *author-color-tables*)))
  ;; Clear out the temp slots 
  (loop for pub in pubs
        do 
        (delete-slot pub #$color-author)
        (loop for auth in (#^authors pub)
              do (delete-slot auth #$color-author)))
  (setq *author-color-tables*
	(sort (remove-if (lambda (tbl) (zerop (hash-table-count tbl))) *author-color-tables*)
	      #'>
	      :key #'hash-table-count
	      )))

(defun color-auths-2 (pubs curtab)
  (loop for pub in pubs
        when (eq (#^color-author pub) :useable)
        do
        (setf (#^color-author pub) :used)
        (loop for auth in (#^authors pub)
              unless (#^color-author auth)
              do 
              (setf (#^color-author auth) :used)
              (setf (gethash auth curtab) t)
              (color-auths-2 (#^pubs auth) curtab)
              )))

(defun slot-elts-in-year-set (frame slot year-set)
  (remove-if-not #'(lambda (thing) (member (#^year thing) year-set))
		 (slotv frame slot)))

(defun describe-author-cluster (ac year-set)
  (sort (loop for a being the hash-keys of ac
	      as pubs = (slot-elts-in-year-set a #$pubs year-set)
              collect (list (#^name a) 
			    (length pubs)
			    pubs))
	#'> :key #'second))
			    
(defun author-clusters (year-set)
  "Return the set of author clusters for the given year set (a list of numerical years)."
  (color-authors (loop for year in year-set append (#^pubs (year->year-frame year)))))

(defun top-author-cluster (author-clusters year-set)
  (describe-author-cluster (car author-clusters) year-set))

(defun author-frames (author-string)
  "Find the author frames where the name of the author contains AUTHOR-STRING"
  (search-frames author-string :frames (all-authors-list) :slots (list #$name)))

#+screwed-up
;;; This is never called, and *author-clusters* is not defined anywhere.
(defun find-author-cluster (author-frame &key (clusters *author-clusters*))
  (find author-frame clusters :test #'(lambda (a c) (gethash a c))))

(defun author-cluster-authors (ac)
  (hash-table-keys ac))

(defun report-author-clusters (&key (stream t) (top-n-to-report 10) (window-size 1) (window-move 1) (years (mapcar #'#^year *year-frames*)))
  (format stream "Top author cluster sizes:~%")
  (format stream "Year set	Total number of clusters	Total author sum	Un- and Normalized Cluster sizes of top ~a clusters~%" top-n-to-report)
  (loop for year-set in (windowize years window-size window-move)
	as clusters = (author-clusters year-set)
	as sum = (loop for table in clusters sum (hash-table-count table))
	do (format stream "~a	~a	~a	" year-set (length clusters) sum)
	(loop for c in clusters
	      as n below top-n-to-report
	      as count = (hash-table-count c)
	      do (format stream "~a	~a	" count (/ (float count) sum))
	      )
	(format stream "~%")
	))

(defun windowize (list window-size window-move)
  (all-contiguous-sublists list window-size window-move))


(defun go-node-freqs-for-author-cluster (cluster year-range)
  (let* ((pubs (pubs-from-authors (author-cluster-authors cluster) year-range)))
    (freqsort 
     (loop for pub in pubs 
	   append 
	   (minimal (purge-duplicates (loop for gene in (#^genes pub)
					    append (#^fringe-go-nodes gene)))
		    'go-children-f1 #'eq)))))

(defun pubs-from-authors (authors year-range)
  (purge-duplicates
   (loop for author in authors 
	 append (slot-elts-in-year-set author #$pubs year-range))))

(defun print-filtered-author-cluster-gos-for-year-set (ys &optional (n 10))
  (print ys)
  (loop for cluster in (author-clusters ys) as k below n
	do
	(format t "~%~%Cluster ~a: ~%" (1+ k))
	collect
	(print (first-n 10
			(loop for (count concept) in (go-node-freqs-for-author-cluster cluster ys)
			      unless (member concept *go-stops*) collect
			      (list count concept))))))

(defun ar1 (size skip)
  "Given a window-size and how far to shift each time (skip), reports the go descriptions of the author clusters."
  (loop for window in (windowize (mapcar #'#^year *year-frames*) size skip)
	do (print window)
	(print (first-n 10 (loop for (count concept) in (go-node-freqs-for-author-cluster (car (author-clusters window)) window)
				 unless (member concept *go-stops*) collect
				 (list count concept))))))

(defun report-author-cluster-conceptual-models (&key (outfile "acr.tbl") (sets-to-report 25))
 (let ((*print-pretty* nil))
  (with-open-file (o outfile :direction :output :if-exists :supersede)
    (loop for (window clusters) in (ar2) ; Need to change this for args.
	  do 
	  (loop for (authtable sets) in clusters
		as nauth = (hash-table-count authtable)
		do (loop for (n a . b) in sets
			 as k from 1 to sets-to-report
			 do (format o "~a~c~a~c~a~c~a~c~a~c~a~c~%" 
				    window #\tab
				    k #\tab
				    nauth #\tab
				    n #\tab
				    a #\tab
				    b #\tab)))))))

(defun ar2 (&key (winsize 3) (winskip 3) (do-only 2) (clusters-to-analyze 3))
  "Given a window-size and how far to shift each time (skip), analyzes the go pairings for the pubs related to each author cluster."
  (loop for window in (windowize (mapcar #'#^year *year-frames*) winsize winskip)
	as k from 1 to do-only
	collect 
	(list window
	      (loop for cluster in (author-clusters window)
		    as k from 1 to clusters-to-analyze
		    collect (list cluster 
				  (sort
				   (loop for key being the hash-keys of (go-pairs-pubs-table
									 (purge-duplicates 
									  (loop for authframe being the hash-keys of cluster
										append (loop for pub in (#^pubs authframe)
											     ;; Only want pubs from the relevant years for the author!!
											     when (member (#^year pub) window)
											     collect pub))))
					 using (hash-value pubs)
					 collect (cons (length pubs) key))
				   #'> :key #'car))))))

;;; Create a table of all combinationial pairs of LEAF go nodes mentioned in the combination
;;; of all combinatiorial pairs of GENE (models) mentions in all publications in a year set.
;;; E.g., Pub1 -> Gm1, Gm2, Gm3
;;;       Gm1 -> Go1, Go2*
;;;       Gm2 -> Go3*, Go4
;;;       Gm3 -> Go5*, Go1
;;; * indicates leaf go nodes.
;;; These are the pairs: ((2 3) (2 5) (3 5))

;;; JP's concepts of FRINGE, as opposed to LEAF, is that a Gm could refer to a Go node
;;; that is non-LEAF, but which nonetheless is not an acestor of any of the other Go nodes
;;; also refered by this Gm.  Since we're NOT going to expand all the children, we DO want to 
;;; include these.  The trick is to avoid the situation where we're include Gox and it's own
;;; parent!  But the fringe concept avoids this. 

;;; Then, once we can compute this combination of Go pairs, we can
;;; count them per time period (year set).

(defun set-fringe-of-pub (pub)
  (setf (#^go-fringe pub) 
	(minimal (purge-duplicates (loop for gene in (#^genes pub)
					 append (#^fringe-go-nodes gene)))
		 'go-children-f3 #'eq)))

(defun go-leaf-node? (go-node &key (children-function #'go-children-f3))
  (null (funcall children-function go-node)))

(defun set-all-pub-fringes ()
  (loop for year-frame in *year-frames*
	do (loop for pub in (#^pubs year-frame)
		 do (set-fringe-of-pub pub))))

(defun pubs-in-year-set (ys)
  (loop for y in ys
	append (#^pubs (year->year-frame y))))

(defun fringe-concepts-in-year-set (ys)
  (freqsort (loop for pub in (pubs-in-year-set ys)
		  append (#^go-fringe pub))))

(defun leaf-concepts-in-year-set (ys)
  (freqsort (remove-if-not #'go-leaf-node? (loop for pub in (pubs-in-year-set ys)
						 append (#^go-fringe pub)))))

