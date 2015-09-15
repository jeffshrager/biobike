(in-package :biolisp-library)

(defstruct enzyme
  EC
  NAME
  CLASS
  SYSNAME
  REACTION
  SUBSTRATE
  PRODUCT
  INHIBITOR
  COFACTOR
  EFFECTOR
  COMMENT
  PATHWAY
  GENES
  DISEASE
  MOTIF
  STRUCTURES
  DBLINKS
  DB-POINTERS)

(defstruct molecule
  ID
  NAME
  FORMULA
  PATHWAY
  ENZYME
  STRUCTURES
  DBLINKS
  DB-POINTERS)

(defstruct reaction 
  LEFT-SIDE
  RIGHT-SIDE
  ENZYME
  DB-POINTERS)

(defun init ()
  (kegg::import-db)
  (go::import-db)
  (transfac::import-db)
  (synbase::import-db)

  (in-package :biolisp-library)
  (setq *trace-output* *html-stream*)
  (setq *user-id* 0)
  (setf (gethash 0 biolisp::*user-id-to-display*) biolisp::*display-normal*)
  (biolisp::declare-dispatch-functions)
  (create-inverted-tables)
  (start))


;;; This might have to be updated.  I'm not 100% sure.

(defmethod print-object ((object enzyme) stream)
  (if (or (not (biolisp::get-display-method-from-current-package))
	  (eq (biolisp::get-display-method-from-current-package) biolisp::*display-expanded*))
      (call-next-method) 
    (funcall #'enzyme-print-function object stream)))

;;; This has to be updated to use the new global values.

(defun enzyme-print-function (struct stream)
  (if (and (biolisp::get-display-method-from-current-package) 
	   (eq (biolisp::get-display-method-from-current-package) biolisp::*display-html*) 
	   *html-stream*)
      (html ((:table border "2")
	     ((:tr)
	      (:td ((:a 
		     :target "new"
		     :href (format nil "http://www.expasy.ch/cgi-bin/nicezyme.pl?~A" (kegg::enzyme-ec struct))) 
		    (:b (:princ-safe (second (assoc :kegg (enzyme-ec struct))))))
		   (:i " " (:princ-safe (car (second (assoc :kegg (enzyme-name struct))))))))
	     ((:tr)
	      (:td (:princ-safe (concatenate-string-list (second (assoc :kegg (enzyme-reaction struct)))))))))
    (format stream "#E~A" (second (assoc :kegg (enzyme-ec struct))))))

(defmethod print-object ((object molecule) stream)
  (if (or (not (biolisp::get-display-method-from-current-package)) 
	  (eq (biolisp::get-display-method-from-current-package) biolisp::*display-expanded*))
      (call-next-method) 
    (funcall #'molecule-print-function object stream)))

(defun molecule-print-function (struct stream)
  (if (and (biolisp::get-display-method-from-current-package)
	   (eq (biolisp::get-display-method-from-current-package) biolisp::*display-html*) 
	   *html-stream*)
      (html ((:table border "2")
	     ((:tr)
	      (:td (:b (:princ-safe (second (assoc :kegg (molecule-id struct)))))
		   (:i " " (:princ-safe (car (second (assoc :kegg (molecule-name struct))))))))))
    (format stream "#M~s" (second (assoc :kegg (molecule-name struct))))))

;;; Used in the init process to create or update global table entries
;;; by unfolding and externalizing db-specific information.  It has to decide whether
;;; to create a new struct, or update an existing one.

(defmacro globalize (to-table make-fn db-pointers-field-accessor
		     from-db-name from-table-or-expr to-key-extractor from-extractor 
		     &rest property-extractors)
  `(loop for id being the hash-keys of ,from-table-or-expr
	 using (hash-value data)
	 do (let ((to-keys (funcall ,to-key-extractor data)))
	      (loop for to-key in (if (listp to-keys) to-keys (list to-keys))
		    do 
		    (let ((record (or (gethash (string-downcase to-key) ,to-table)
				      (funcall ,make-fn))))
		      (push (list ,from-db-name ,from-extractor id)
			    (funcall ,db-pointers-field-accessor record))
		      ,@(loop for (to-slot-accessor from-extractor-fn) in property-extractors
			      collect `(push (list ,from-db-name (funcall ,from-extractor-fn data))
					     (,to-slot-accessor record))
			      )
		      (setf (gethash (string-downcase to-key) ,to-table) record)
		      )
		    ))
	      ))

(defun create-inverted-tables ()

  ;; ----- Molecules -----
  (clrhash *molecules*)

  ;; KEGG molecules.
  (globalize *molecules* #'make-molecule #'molecule-db-pointers
	     :kegg kegg::*molecules* #'kegg::molecule-name 
	     #'kegg::lookup-molecule-in-kegg-*molecules*-table
	     ;; *************** Fill in for externalized contents *******************
	     (molecule-formula #'kegg::molecule-formula)
	     (molecule-name #'kegg::molecule-name)
	     (molecule-id #'kegg::molecule-id)
	     )

  ;; TransPath molecules.
  (globalize *molecules* #'make-molecule #'molecule-db-pointers
	     :tx (transfac::abbreviated-records-from-table 'transfac::molecule)
	     #'(lambda (data) (string-downcase (transfac::field-value 'transfac::na data)))
	     #'transfac::ac-lookup
	     ;; *************** Fill in for externalized contents *******************
	     )

  ;; ----- Enzymes ------

  (clrhash *enzymes*)

  ;; KEGG enzymes.
  (globalize *enzymes* #'make-enzyme #'enzyme-db-pointers
	     :kegg kegg::*enzymes* #'(lambda (e) (cons (kegg::enzyme-ec e) (kegg::enzyme-name e)))
	     #'kegg::lookup-enzyme-in-kegg-*enzymes*-table
	     ;; *************** Fill in for externalized contents *******************
	     (enzyme-substrate #'kegg::enzyme-substrate) ; This is going to be wrong -- should be a global pointer!
	     (enzyme-reaction #'kegg::enzyme-reaction)
	     (enzyme-ec #'kegg::enzyme-ec)
	     (enzyme-name #'kegg::enzyme-name)
	     )

  ;; ----- Reactions -----

  (clrhash *reactions*)

  ;; Transpath reactions.
  (globalize *reactions* #'make-reaction #'reaction-db-pointers
	     :tx (transfac::abbreviated-records-from-table 'transfac::reaction)
	     #'(lambda (data) (string-downcase (transfac::field-value 'transfac::na data)))
	     #'transfac::ac-lookup
	     ;; *************** Fill in for externalized contents *******************
	     (reaction-left-side #'(lambda (data) (transfac::get-all-fields-of-type 'mb data))) ; these need to refer globally!
	     (reaction-right-side #'(lambda (data) (transfac::get-all-fields-of-type 'ma data)))
	     )
  )


;;; Sometimes the user will want to create their own table, indexed in accord with their
;;; own needs.  This does that, taking one table and an accessor fn, and producing a table
;;; whose keys are the results of the accessor fn (key-creator-fn), and whose value is either 
;;; the original table, where the value comes from the value-creator-fn.  ORIG-KEY is the key from
;;; the orignal table. You can choose either :PUSH or :SETF. PUSH is useful in a many::many relationship,
;;; do you don't lose information randomly.  Both the key creator and value creator
;;; fns are given the keys and data from the table being inverted.

;;; Usage: (invert-a-table *molecules* 
;;;                        #'(lambda (key value) 
;;;                             (second (first value))) 
;;;                        #'(lambda (key value) key))

(defun invert-a-table (from-table key-creator-fn value-creator-fn 
                       &key (push-or-setf? :push) (to-table nil) (clear-table? t))
  (unless to-table (setq to-table (make-hash-table :test #'equal)))
  (when clear-table? (clrhash to-table))
  (loop for key being the hash-keys of from-table
	using (hash-value value)
	do (case push-or-setf? 
		 (:push 
		  (push (funcall value-creator-fn key value)
			(gethash (funcall key-creator-fn key value) to-table)))
		 (:setf 
		  (setf (gethash (funcall key-creator-fn key value) to-table)
			(funcall value-creator-fn key value)
			))
		 (t (Error "invert-a-table got ~a instead of either :PUSH or :SETF in :PUSH-OR-SETF? keyword argument!"))
		 ))
  to-table)

(defun clear ()
  (setf (gethash (biolisp::get-user-id-from-current-package) biolisp::*web-interactions*) nil))

(defun get-go-from-ec (enzyme)
  (go::get-go-from-ec (second (assoc :kegg (enzyme-ec enzyme)))))

(defun find-pathways (initial-ec final-ec &key (cutoff 5) (shortest nil))
  (let* ((initial-string (second (assoc :kegg (enzyme-ec initial-ec))))
	 (final-string (second (assoc :kegg (enzyme-ec final-ec))))
	 (pathways (if shortest
		       (find-shortest-pathway-between-ec-strings initial-string
								 final-string :cutoff-max cutoff)
		     (find-pathway-between-ec-strings initial-string
						      final-string :cutoff cutoff))))
    (excl::list-remove-duplicates-equal
     (loop for pathway in pathways
	   collect
	   (loop for enzyme-string in pathway
		 as enzyme = (gethash (remove #\* enzyme-string) *enzymes*)
		 collect enzyme)))))

(defun find-pathway-between-ec-strings (initial-ec final-ec &key (cutoff 5))
  (let (results)
    (declare (special results))
    (traverse-pathway-recursively (list initial-ec) final-ec (- cutoff 1))
    (traverse-pathway-recursively (list (concatenate 'string initial-ec "*")) final-ec (- cutoff 1))
    results))

(defun find-shortest-pathway-between-ec-strings (initial-ec final-ec &key (cutoff-max 5))
  (loop for cutoff from 1 to cutoff-max
	as pathways = (find-pathway-between-ec-strings initial-ec final-ec :cutoff cutoff)
	until pathways
	finally (return pathways)))

(defun traverse-pathway-recursively (pathway final-ec cutoff)
  (if (equal (car pathway) final-ec)
      (push (reverse pathway) results))
  (if (not (zerop cutoff))
      (loop for ec in (gethash (car pathway) kegg::*ec-pathways*)
	    when (not (or (member ec pathway :test #'equal) 
			  (member (concatenate 'string ec "*") pathway :test #'equal)
			  (member (string-right-trim "*" ec) pathway :test #'equal)))		      
	    do (traverse-pathway-recursively (cons ec pathway) final-ec (- cutoff 1)))))

;;; Flatten GO-hieracy entries into a list of parent types that 
;;; you can easily search w/member...

(defun flatten (l)
  (cond ((null l) nil)
	((listp l) (append (flatten (car l))
			   (flatten (cdr l))))
	(t (list l))))

(defun go-type? (type enzyme)
  (find type (flatten (get-go-from-ec enzyme)) :test #'string-equal))

;;; =======================================================================
;;; This all has to go in the BioCyc package:

(defvar *syncyc-table* (make-hash-table :Test #'equal))

(defun biocyc-hash ()
  (loop for object in *synbase*
	as name = (first object)
	do 
	(setf (gethash name *syncyc-table*) object)))
	
;;; Finds the type path of an object
;;; Chache the pathways for efficiency.

(defvar *sc-object-paths* (make-hash-table :Test #'equal))

(defun biocyc-type-path (object &optional path)
  (or (gethash (car object) *sc-object-paths*)
      (setf (gethash (car object) *sc-object-paths*) 
	  (cond ((null object) path)
		(t (mapcan #'(lambda (parent) (biocyc-type-path (gethash parent *syncyc-table*) (cons parent path)))
			   (cdr (assoc 'OCELOT::PARENTS (third object)))))))))

;;; Find all of the pathways in the syscyc db:

(defun all-cyc-pwys ()
  (loop for object being the hash-values of *syncyc-table*
	when (member '|Pathways| (biocyc-type-path object))
	collect object))

; (remhash :OCELOT-KB *syncyc-table*)

;;; =======================================================================
;;; Simulation of the SMD

(defvar *smd-data* (make-hash-table :test #'equal))

(defun get-smd-data (&rest params)
  ;; Eventually this will connect to the SMD, for now, it just loads the indicated file.
  (load (getf params :file))
  (setf (gethash (getf params :name) *smd-data*) *202-data*)
  (format t "~a entries loaded from ~a, called ~s in *smd-data* table.~%"
	  (length *202-data*)
	  (getf params :file)
	  (getf params :name)))

(defstruct smd-data params data)

#|
(get-smd-data :uid :public
	:name "test"
	:file "c:/temp/28148_pcl.lsp"
	:expt-name "Schaffer R et al.(2001) Plant Cell 13:113-23" 
	:exptid 202
	:genes/clones :all
	:by :suid
	:annotations '(:suid :cloneid :description :TIGR-name :accession)
	:label :both
	:filter '(> regression-correlation 0.6)
	)

|#

;;; Find all the acetlytransferases on a chip dataset and return their clone IDs and EC numbers, if any are specified.

(defun chip-clones-by-substring (target dataset-name)
  (loop for entry in (gethash dataset-name *smd-data*)
	as (clone description . rest) = entry
	when (search target description :test #'char-equal)
	collect (list entry
		      (loop for ec in (unembed-EC-numbers description)
			    collect (ec-lookup ec)))))
		      
;;; =======================================================================
;;; General user-level utilities.

;;; Find EC numbers embedded in a string

(defun unembed-EC-numbers (string &aux ecs)
  (loop for pos from 0 to (- (length string) 5)
	when (and (char-equal #\e (aref string pos))
		  (char-equal #\c (aref string (1+ pos)))
		  )
	do (let ((pn1 (+ pos 2)))
	     (when (when (char-equal #\space (aref string pn1))
		   (incf pn1)))
	     (let ((n1code (char-code (aref string pn1))))
	       (when (and (>= n1code 48)
			  (<= n1code 57))
		     (let ((endpos (or (position #\space string :start pn1 :test #'(lambda (a b) (member b '(#\space #\( #\) #\]) :test #'char-equal)))
				       (length string))))
			   (push (subseq string pn1 endpos) ecs)	
			   )))))
  ecs)

(defun ec-lookup (ec-string)
  (gethash ec-string *enzymes*))

(defun nte (table &optional (n 10)) ; Just give you the first n table entries, which is sort of 
                                    ; random, but useful for debugging. (n-table-entries...)
  (loop for i from 1 to n
	as key being the hash-keys of table
	using (hash-value value)
	collect (list :key key :value value)))

;;; Try your best to track down objects through pointer blocks of all sorts.

(defvar *dbg* nil)

(defmacro string-search (for in)
  `(search ,for ,in :test #'char-equal))

(defun lookup (pb)
  (cond ((null pb) nil)
	((and (listp pb) (member (car pb) '(:tx :kegg)))
	 (lookup2 (car pb) (cdr pb)))
	((listp pb)
	 (cons (lookup (car pb))
	       (lookup (cdr pb))))
	(t pb)))

(defun lookup2 (type pb)
  (case type 
	(:tx (tx-lookup pb))
	(:kegg (kegg-lookup pb))))

(defun kegg-lookup (pb)
  (cond ((null pb) nil)
	((stringp pb) pb) ; This is wrong; Shouldn't ever end up with a string in kegg.
	((listp pb)
	 (cond ((functionp (car pb))
		(apply (car pb) (cdr pb)))
	       (t (cons (kegg-lookup (car pb))
			(kegg-lookup (cdr pb))))))
	(t pb)))

(defun tx-lookup (pb)
  (cond ((null pb) nil)
	((stringp pb)
	 (cond ((position #\< pb) ; This isn't an absolutely accurate test.
		(transfac::ac-lookup (transfac::strip-tp-accession-code pb)))
	       (t (transfac::ac-lookup pb))))
	((listp pb)
	 (cond ((functionp (car pb))
		(apply (car pb) (cdr pb)))
	       (t (cons (tx-lookup (car pb))
			(tx-lookup (cdr pb))))))
	(t pb)))

;;; Shorthands that make certain common operations look a little nicer!  Use KEY and VALUE

(defmacro test-any (list test)
  `(loop for each-element in ,list 
	 when ,test
	 do (return t)))

(defmacro test-all (list test)
  `(loop for each-element in ,list 
	 when (not ,test)
	 do (return nil)
	 finally (return t)))

(defmacro fun (args &rest body)
  `(function (lambda ,args ,@body)))

(defmacro loop-over-table (table &rest exprs)
  `(loop for key being the hash-key of ,table
	 using (hash-value value)
	 ,@exprs))

;;; If you don't like setq:

(defmacro assign (&rest ments)
  `(setq ,@ments))
  
;;; Used to display the values of things for debugging.
;;; Returns the last value in the list.

(defmacro display (&rest names)
  `(progn 
     ,@(loop for name in names
	     collect `(format t "~a = ~a~%" ',name ,name))
     ,(car (last names))))

;;; Some table utilities:

(defmacro tableset (table key value)
  `(setf (gethash ,key ,table) ,value))
(defmacro tableget (table key) ; This is trivial but make the balance with SETHASH
  `(gethash ,key ,table))

;;; Tries to fill in the various fields in the tables with the appropriate
;;; objects from other global tables.

(defun refine-global-tables ()
  ;; Make the reactions in *reactions* point to global molecules through transpath 
  ;; molecules that have been promoted into the *molecules* table.
  (loop-over-table *reactions*
		   do
		   (setf (reaction-left-side value) 
			 (loop for entry in (lookup (reaction-left-side value))
			       collect (tableget *molecules* (transfac::field-value 'na entry))))
		   (setf (reaction-right-side value) 
			 (loop for entry in (lookup (reaction-right-side value))
			       collect (tableget *molecules* (transfac::field-value 'na entry))))
		   ))

;;; This specifically restricts a list of structs by applying the test to the value
;;; of the struct.  If slot is an atom, you just use its name as the var, and if its
;;; a list, you get all of them.  

(defmacro restrict (list slot* constraint)
  `(loop for each-element in ,list
	 ,@(cond ((null slot*) nil)
		 ((symbolp slot*) `(as ,slot* = (funcall ',slot* each-element)))
		 (t (loop for slot in slot*
			  append `(as ,slot = (funcall ',slot each-element)))))
	 when ,constraint
	 collect each-element))

(export '(MAKE-ENZYME
	  ENZYME-P
	  ENZYME-EC
	  ENZYME-NAME
	  ENZYME-CLASS
	  ENZYME-SYSNAME
	  ENZYME-REACTION
	  ENZYME-SUBSTRATE
	  ENZYME-PRODUCT
	  ENZYME-INHIBITOR
	  ENZYME-COFACTOR
	  ENZYME-EFFECTOR
	  ENZYME-COMMENT
	  ENZYME-PATHWAY
	  ENZYME-GENES
	  ENZYME-DISEASE
	  ENZYME-MOTIF
	  ENZYME-STRUCTURES
	  ENZYME-DBLINKS
	  ENZYME-DB-POINTERS

	  MAKE-MOLECULE
	  MOLECULE-P
	  MOLECULE-ID
	  MOLECULE-NAME
	  MOLECULE-FORMULA
	  MOLECULE-PATHWAY
	  MOLECULE-ENZYME
	  MOLECULE-STRUCTURES
	  MOLECULE-DBLINKS
	  MOLECULE-DB-POINTERS
	  
	  MAKE-REACTION
	  REACTION-P
	  REACTION-LEFT-SIDE
	  REACTION-RIGHT-SIDE
	  REACTION-ENZYME
	  REACTION-DB-POINTERS

	  CLEAR
	  GET-GO-FROM-EC
	  FIND-PATHWAYS

	  *DBG*
	  LOOKUP
	  LOOKUP2
	  TX-LOOKUP
	  LOOP-OVER-TABLE
	  ASSIGN
	  DISPLAY
	  TABLESET
	  REFINE-GLOBAL-TABLES
	  RESTRICT

	  KEY
	  VALUE
	  EACH-ELEMENT
	  ENTRY

	  INVERT-A-TABLE
	  NTE
	  TABLEGET
	  STRING-SEARCH
	  FLATTEN
	  GO-TYPE?

	  UNEMBED-EC-NUMBERS
	  ))


