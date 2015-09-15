;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

;;; Copyright (c) 2002-2003 by Jeff Shrager, Mike Travers, and JP Massar.
;;;  All rights reserved.

(in-package :bio)

;;; This software is made avilable for EDUCATIONAL PURPOSES
;;; ONLY, and WITHOUT ANY WARRANTY, express or implied, of
;;; its merchantability or fitness for a particular purpose. 


(defun extract-reaction-from-defn (definition)
  (when definition
    (let* ((search-string "Catalysis of the reaction:")
           (catpos (search search-string definition :test #'char-equal)))
      (when catpos 
        (let ((dotpos (position #\. definition :start (1+ catpos))))
          (when dotpos
            (subseq definition (+ (length search-string) catpos)  dotpos)))))))

(defun +split-reaction-half (part)
  ;; This both parses a half reaction, and fixes the H+ problem by looking
  ;; for cases where a term is followed by a null string (""), so anywhere we
  ;; find something like: "H" "", it goes to "H+", etc.  At the same time this
  ;; will skip over null strings.
  ;; Gather the terms in the +-split of the half reaction.                
  (loop for molecule+ on 
	(loop for term in (string-split part #\+)
			  ;; Downcase for case folding and remove spaces.
	    collect (string-downcase (string-trim " " term)))
      when (string-equal "" (cadr molecule+))
      collect (format nil "~a+" (car molecule+))
      else unless (zerop (length (car molecule+)))
      collect (car molecule+)))

;;; Another version of +SPLIT-REACTION-HALF which doesn't have the H+
;;; problem, because it splits the string by looking for " + ", not just
;;; '+'.  If you have trouble following the above this might be easier
;;; to understand.

#|
(defun split-reaction-half-aux (part)
  (unless (string= part "")
    (let ((pos (search " + " part)))
      (if (null pos)
          (list part)
        (cons (subseq part 0 pos) 
              (split-reaction-half-aux (subseq part (+ pos 3))))))))

(defun +split-reaction-half (part)
  (mapcar #'(lambda (s) (string-trim " " (string-downcase s)))
          (split-reaction-half-aux part)))
|#


(defvar *molecules->goids* (make-hash-table :test #'equal))
(defvar *goid->reactions* (make-hash-table :test #'equal))

(defun find-reactions-in-go ()
  (clrhash *molecules->goids*)
  (clrhash *goid->reactions*)
  (loop for (id . entry) in *go* ; Scan the GO
        ;; Extract-reaction.... returns nil if there's no reaction.
	as reaction = (extract-reaction-from-defn (assocadr 'definition entry))
	when reaction 
	do 
	(setf (gethash id *goid->reactions*)
              ;; Split into two halves...
	      (loop for part in (string-split reaction #\=) 
                    ;; ... and here's the hard work!
		    collect (loop for mol in (+split-reaction-half part) 
				  do (push id (gethash mol *molecules->goids*))
				  collect mol)))))

(defun display-reactions-involving (molecule)
  (loop for goid in (gethash (string-downcase molecule) *molecules->goids*)
	as object = (gethash goid *go-id->plist-table*)
	as name = (assocadr 'name object)
	as reaction = (gethash goid *goid->reactions*)
	do (format t "~s --[~a]--> ~s~%" (car reaction) name (cadr reaction))
	))

(defvar *reactants->reactions* (make-hash-table :test #'equal))
(defvar *molecule-names->molecules* (make-hash-table :test #'equal))
(defvar *goids->reactions* (make-hash-table :test #'equal))

;;; Create the molecule and reaction structures required by the various 
;;; Pathway Logic functions.  There's some fancy footwork near the end 
;;; where we create a reaction structure, and then fill in the reactant
;;; and product fields. This enables us to both fill in the reaction, 
;;; and pass it in to CONVERT-MOLECULES-FOR-PATHWAY-SEARCH.

(defun FIND-REACTIONS-IN-GO-FOR-PWYS ()
  (clrhash *reactants->reactions*)
  (clrhash *molecule-names->molecules*)
  (clrhash *goids->reactions*)
  (loop for (id . entry) in *go* ; Scan the GO
	as reaction = (extract-reaction-from-defn (assocadr 'definition entry))
	when reaction ; Extract-reaction... returns nil if there's no reaction.
	do 
	(setf (gethash id *goids->reactions*)
	      (let* ((reactants+products
                      ;; Split into two halves...
		      (loop for part in (string-split reaction #\=)
			    collect 
                            ;; ... and here's the hard work!
                            (loop for mol in (+split-reaction-half part)
                                  collect mol)))
                     ;; Create the reaction; Fill it in later!
		     (reaction (make-reaction :catalyst id)) 
		     )
                ;; Make real molecule structures for reactants and products.
		(setf (reaction-reactants reaction) 
		      (convert-molecules-for-pathway-search 
                       (car reactants+products) reaction))
		(setf (reaction-products reaction)
		      (convert-molecules-for-pathway-search 
                       (cadr reactants+products)))
		reaction
		))))
	
;;; These replace the N/NS/INS types that we were able to create when we were
;;; manually creating things in the original Pathway Logic section.  (See INIT-OBJECTS)

(defvar *INS-mol-names* '("atp" "adp" "h2o" "phosphate" "nadh" "h+"))
(defvar *NS-mol-names* '())

;;; Create Pathway-Logic compatible molecule structures, and, in the case of reactants
;;; put pointers in for the reactants -> reactions table.

(defun convert-molecules-for-pathway-search (mol-names &optional reaction)
  (loop for mol-name in mol-names
	as mol = (or (find-molecule mol-name)
		     (make-molecule 
                      :abbrev mol-name
                      :fullname mol-name
                      :type (cond ((member mol-name *INS-mol-names* :test #'string-equal) 'ins)
                                  ((member mol-name *NS-mol-names* :test #'string-equal) 'ns)
                                  (t 's))))
	do 
	(setf (gethash mol-name *molecule-names->molecules*) mol)
	(when reaction (push reaction (gethash mol *reactants->reactions*)))
	collect mol))

;;; Versions that use names instead of abbreviations:

(defun find-molecules (names)
  (loop for name in names 
        collect (find-molecule name)))

(defun find-molecule (name)
  (gethash name *molecule-names->molecules*))

;;; Initing the envionment has to be done by name now also.  Sometimes the
;;; GO isn't consistent about the names of things (like CO2 is sometimes 
;;; called "Carbon Dioxide").  Oh well.

(defun init-env ()
  (setq *env* 
	(find-molecules 
         '("atp" "adp" "d-glucose" "phosphatate" "co2" "h2o" "phosphate" "nadh"
                 "d-fructose" "fad" "nad+" "o2" "gdp" "gtp")))
  (format t "~&;; ~D molecules seeded into the environment.~%" (length *env*))
  (values)
  )


;;; Oh, and the catalyst is listed as a GO ID now, so we have to 
;;; dereference it to its name for pretty printing.

(defun pp-reaction (r)
  (let* ((catalyst (reaction-catalyst r))
	 (reactants (reaction-reactants r))
	 (products (reaction-products r))
	 )
    (pp-mols reactants)
    ;; Here's the only change in this function:
    (format t " --[~a]--> " 
            (assocadr 'name (gethash catalyst *go-id->plist-table*)))
    (pp-mols products)
    (format t "~%"))
  r)

;;; Exactly like Pathway Logic version, except that we've
;;; added a test for null products, which are a side effect of 
;;; the heuristic parsing of the GO, and we don't want them
;;; in the environment!

(defun apply-reaction (reaction env)
  (append env
	  (loop for product in (reaction-products reaction)
                ;; Here's the modified test:
		unless (or (null product) (member product env))
		collect product)))

;;; Exactly like Pathway Logic version, except that we use
;;; the *reactants->reactions* table instead of searching the
;;; list of all reactions.

(defun possible-reactions-from (from env)
  (loop for reaction in (gethash from *reactants->reactions*)
	if (loop for mol in (reaction-reactants reaction)
		 if (not (member mol env))
		 do (return nil)
		 finally (return t))
	collect reaction))

;;; Excatly like the old one, except that we'll add a string-downcase
;;; because we're using names now instead of abbreviations (which were symbols),
;;; and we are conventionally downcasing all the names for consistency.

(defmacro fpathsto (from to)
  `(length (setq *pathways* 
                 (remove-duplicate-paths 
		  (paths-between (find-molecule (string-downcase ',from))
				 (find-molecule (string-downcase ',to))
				 *env*)))))

;;; This is the improved PP-REACTION defined in the tutorial text.
;;; It's commented out here because the tutorial previously relies
;;; on an alternative definition of PP-REACTION, defined by an
;;; earlier tutorial.  So cut and paste this definition when it is
;;; called for.

#|

(defun pp-reaction (r)
  (let* ((catalyst (reaction-catalyst r))
	 (reactants (reaction-reactants r))
	 (products (reaction-products r))
         (catalyst (gethash catalyst *go-id->plist-table*))
         )
    (pp-mols reactants)
    ;; Not only print the reaction...
    (format t " --[~a]--> " (assocadr 'name catalyst))
    (pp-mols products)
    (format t "~%") ; <br>
    ;; But give us a little explanation:
    (loop for (key value) in catalyst
          when (eq 'isa key)
	do (format t "  This is a ~a~%" 
		   (assocadr 'name (gethash value *go-id->plist-table*))))
    (format t "~%"))
  r)

|#
