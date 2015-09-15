(load "c:/biolingua/utils.lsp")

(setq *initial* '(6 8 9 12 16 17))
(setq *target* '(1))
(setq *reactions* '(((13 14) (11))
		    ((15 16) (11))
		    ((11 12) (7))
		    ((6 7) (2))
		    ((8 9) (3))
		    ((6 10) (4))
		    ((2 3) (1))
		    ((4 5) (1))))

(defun bridge ()
  (bridge-reactions *initial* *target* *reactions*))

;;; This is the top-level function to determine the fewest compounds needed to complete
;;; the pathway from the 'initial' compounds to the 'target' compounds using 'reactions

(defun bridge-reactions (initial target reactions)
  (let* ((environment (apply-reactions initial reactions))
	 (reactions (simplify-reactions environment reactions))
	 (all-reactants (find-all-basic-reactants target reactions))
	 (solutions (find-all-solutions all-reactants initial target reactions)))
    (format t "Initial environment: ~W~%" environment)
    (format t "Target environment: ~W~%" target)
    (format t "Compounds needed: ~W~%" solutions)
    solutions))

;;; Step 1: Fill initial environment by running all possible reactions on existing compounds.
;;; This will apply all the appropriate reactions on the 'initial' set of compounds
;;; until no more reactions can be run. This will recursively call itself until done.

(defun apply-reactions (initial reactions)
  (let ((final initial))
    (loop for (substrates products) in reactions
	  when (and (subset substrates initial)
		    (not (equal (intersection initial products) products)))
	  do
	  (loop for product in products
		do 
		(setq final (adjoin product final))))
    (if (equal initial final) 
	final
      (apply-reactions final reactions))))

;;; Step 2: Eliminate all compounds that are in the initial environment from the 
;;; reactants side of each reaction.

(defun simplify-reactions (environment reactions)
  (loop for (reactants products) in reactions
	collect (list (set-difference reactants environment) products)))
	
;;; Step 3: Start from the target environment and step back through the reaction 
;;; tree level by level from product to all its reactants until there are no more 
;;; new reactants.

(defun find-all-basic-reactants (target reactions)
  (loop as reactants = (step-back-reactions target reactions)
	until (equal target reactants)
	do (setq target reactants)
	finally (return reactants)))

;;; Step back one level of compounds to their reactants

(defun step-back-reactions (products reactions)
  (loop for compound in products
	as reactants = (all-reactants-for-product compound reactions)
	append (if reactants reactants (list compound))))

;;; Find all the reactants that can create a given product

(defun all-reactants-for-product (product reactions)
  (loop for (reactants products) in reactions
	when (member product products)
	append reactants))

;;; Step 4: Pick groups from the final level until you have a working set of compounds 
;;; to get to the target environment.

(defun find-all-solutions (all-reactants initial target reactions)
  (let ((possible-solutions (choose-from-list all-reactants)))
    (loop for solution in possible-solutions
	  when (valid-solution solution initial target reactions)
	  collect solution)))

;;; This will return true if 'bridged-compounds' will be enough to produce the 'target'

(defun valid-solution (bridged-compounds initial target reactions)
  (subset target (apply-reactions (append bridged-compounds initial) reactions)))


;;; Assorted Utility Functions

;;; Given a list of any length this will return all groupings of elements in that list.
;;; For example (1 2 3 4), will return:
;;; (1) (2) (3) (4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4)

(defun choose-from-list (input-list) ; Use a call back function?
  (let* ((list (choose-from-list-rec input-list
				     'nil 
				     (- (length input-list) 1)))
	 (ordered-list (loop for item in list
			     collect (reverse item)))
	 (sorted-list (sort ordered-list #'< :key #'length)))
    (append sorted-list (list input-list))))

(defun choose-from-list-rec (input-list output-list num)
  (if (> num 0)
      (loop for item in input-list
	    with remaining = (cons 'nil input-list)
	    do 
	    (setq remaining (cdr remaining))
	    when (not (member item output-list))
	    append (cons (remove nil (cons item output-list))
			 (choose-from-list-rec remaining (cons item output-list) (- num 1))))))

(defun subset (set superset)
  (not (set-difference set (intersection set superset))))
                                                               
