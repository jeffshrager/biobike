;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

;;; Based upon code originally written (for us) by Sumudu Watugala of MIT

;;; Now exported in library-api.lisp

(defparameter *crossmetric-key-max-length* 128)


(defun all-db-table-names () 
  (mapcar #'first (dosql "show tables")))

(defun create-crossmetric-table 
       (function data1 data2 table-name 
                 &key
                 (value-field-name "distance")
                 (value-field-type "float")
                 (field1-name "id_from")
                 (field2-name "id_to")
                 (field1-type "varchar")
                 (field2-type "varchar")
                 (field1-size *crossmetric-key-max-length*)
                 (field2-size *crossmetric-key-max-length*)
                 (if-exists :error)
                 (commutative? nil)
                 (do-identity? t)
                 (verbose? t)
                 (print-dot-every 500)
                 )

  #.(one-string-nl
     "CREATE-CROSSMETRIC-TABLE takes a function, two lists of data, and the name"
     "of a table to be created permanently in the database.  The table that it creates"
     "will have three columns: two keys, representing the compared items, and a value,"
     "reflecting a 'distance', or 'measure' between elements of the data lists."
     "The function (first argument) takes two arguments, which will be elements"
     "of the two data lists (described in more detail below).  It muust return a list of"
     "three elements: two strings, representing keys for the new table record,"
     "and a third element, being the value. DATA1 and DATA2 are list of the "
     "objects that are intended to be 'cross-measured' by the function, and"
     "whose 'cross-measures' are to be installed in the table.  The function "
     "is called with pairs of elements of DATA1 and DATA2.  For example, if DATA1"
     "is the list (A B C), and DATA2 is the list '(D E F), (and assuming for the "
     "moment that :COMMUTATIVE? is T), then the function will be called with arguments: "
     "A D, A E, A F, B D, B E, B F, and finally C D, C E, and C F.  The resturned keys"
     "and values from these calls will be installed in the cross table."
     "If :COMMUTATIVE? is NIL, the function will also be called with all the opposite"
     "pairs: D A, D B, D C, E A, E B, E C, F A, F B, and F C."
     ""
     ":IF-EXISTS may be :WARN-AND-SUPERSEDE, :ERROR (the default)"
     ":SUPERSEDE, :APPEND or NIL. If it is NIL and the table exists the function does nothing."
     "If :APPEND, no new table is created, but the values are added to the old table.  (We"
     "assume, in this case, that the old table has the appropriate fields.  If it doesn't,"
     "Probably the function will simply break with a database error.)"
     ""
     "Since CREATE-CROSSMETRIC-TABLE is frequently called with the same data argument twice,"
     "resulting in the self-cross-product of the data, it is sometimes convenient to avoid "
     "testing the identity. If :DO-IDENTITY? is NIL, then elements that are EQ to one another"
     "are not entered into the cross metric table."
     ""
     "Note that the function must be able to return unique keys to identify each element of"
     "each data list.  Often this will require pre-consing identifying tags onto the elements"
     "before passing them into CREATE-CROSSMETRIC-TABLE."
     ""
     "Example:"
     ""
     "(setq d1 '((a 1 2 3 4) (b 4 3 2 1) (c 4 2 3 5)))"
     "(setq d2 '((d 3 2 4 1) (e 4 3 4 2) (f 2 1 3 2)))"
     ""
     "(defun xfun (a b)"
     "  (list (car a) (car b)"
     "        (getf (correlate (cdr a) (cdr b)) :r)))"
     ""
     "(bio::create-crossmetric-table #'xfun d1 d2 \"test\")"
     ""
     "This creates a table called TEST with all the the cross product correlations between the"
     "lists of values in d1 and d2.  There will be 18 records (3x3x2, including the reverse)."
     "(In this case, probably commutative should be T because correlation is commutative.)"
     ""
     "For uses of crossmetric tables, see the function CROSSMETRIC-APPLY, for example."
     )

  (with-organisms-db (db)
    ;; Create the new table if requested.
    (unless (eq :append if-exists)
     (let ((existing-tables (all-db-table-names)))
      (when (member table-name existing-tables :test #'string=)
        (ecase if-exists
          ((:warn-and-supersede :warn-and-supercede)
           (format t "~&;; *** Table ~A exists.  Deleting it...~%" table-name)
           (esql (format nil "drop table ~A" table-name)))
          ((:supersede :supercede)
           (esql (format nil "drop table ~A" table-name)))
          (:error (error "Table ~A already exists." table-name))
          ((nil) (return-from create-crossmetric-table nil))
          ))
      (when verbose? (cformatt "Creating cross metric table ~A" table-name))
      (esql
       (formatn
        (one-string
         "create table ~A ( "
         " ~A ~A(~D) not null, "
         " ~A ~A(~D) not null, "
         " ~A ~A not null, "
         " index id (id_from, id_to)"
         ")"
         )
        table-name 
        field1-name field1-type field1-size
        field2-name field2-type field2-size
        value-field-name value-field-type
        ))))
        
    ;; Fill it in, record by record, calling FUNCTION to get
    ;; the distance measure.

    (let ((count 1) (*db-verbose* nil))
      (flet ((insert (from to value)
               (esql
                (formatn "insert into ~A ( ~A, ~A, ~A) values (~S,~S,~A)"
		table-name field1-name field2-name value-field-name 
		(fstring from) (fstring to)
		(cond
		 ((string-equal "float" value-field-type)
		  (float-to-mysql-float-string (float value)))
		 (t value)
		 )))
	     (incf count)
	     ))
	;; Forward direction:
	(do ((xlist data1 (cdr xlist)))
	    ((null xlist))
	  (do ((ylist data2 (cdr ylist)))
	      ((null ylist))
	    (let ((x (first xlist)) (y (first ylist)))
	      (unless (and (not do-identity?) (eq x y))
		(apply #'insert (funcall function x y))
		(when (and verbose? (zerop (mod count print-dot-every)))
		  (format t "."))
		))))
	;; Reverse if desired (could be flet'ed -- but whatever...):
	(when (not commutative?)
	  (do ((xlist data2 (cdr xlist)))
	      ((null xlist))
	    (do ((ylist data1 (cdr ylist)))
		((null ylist))
	      (let ((x (first xlist)) (y (first ylist)))
		(unless (and (not do-identity?) (eq x y))
		  (apply #'insert (funcall function x y))
		  (when (and verbose? (zerop (mod count print-dot-every)))
		    (format t "."))
		  )))))
	;; That's all, folks!)
        (when verbose?
          (cformatt "~D records entered into table ~A" (1- count) table-name))
        ))
    ))
       
    
(defun crossmetric-apply (table-name fn)
  ;; FFF %%% Do this without having to return every entry in the table by using
  ;; the methods that map over results from selects sent off, and then reading row-by-row.
  #.(one-string-nl
     "Apply FN to each element in a crossmetric table. FN must take three agruments: "
     "the two keys, and the cross metric value.  No result is returned, so presumably"
     "the function has some useful side effect.  The order in which rows are returned"
     "is undefined."
     )
 (with-organisms-db (db)
   (mapcar #'(lambda (v) (apply fn v))
	   (esql
	    (formatn "select * from ~A" table-name)
	    ))))

;;; auxiliary functions for CROSSMATCHED-GENES-OF


(defun comparison-symbol-to-string (cs)
  (if (stringp cs)
      (if (member cs '("<" ">" "<=" ">=" "=" "/=") :test 'string=) 
          cs
        (error "Invalid comparison indicator: ~A" cs))
    (case cs
      ((< :lt :less-than) "<")
      ((> :gt :greater-than) ">")
      ((<= :le :less-than-or-equal) "<=")
      ((>= :ge :greater-than-or-equal) ">=")
      ((= :eq :equal :equal-to :equals) "=")
      ((/= :ne :not-equal :not-equal-to) "/=")
      (otherwise (error "Invalid comparison indicator: ~A" cs))
      )))

(defun verify-organisms (organisms)
  (or (eq organisms :all)
      (let* ((known-organisms (available-organisms)))
        (dolist (org organisms)
          (unless (every 
                   (lambda (x) 
                     (member (fstring x) known-organisms :test 'string-equal))
                   organisms)
            (error "Unknown organism: ~A" org)
            )))))
   
(defun find-gene-organism-from-db (gene)
  (let* ((name (fstring gene))
         (query (format nil "select organism from genes where id ='~A'" name))
         (query-result (esql query)))
    (when (> (length query-result) 1)
      (format t "~&;; Warning: duplicate gene ~A in GENES table~%" gene))
    (caar query-result)
    ))

(defun find-gene-organism-frame (gene)
  (cond
   ((framep gene)
    (or (slotv gene #$organism)
        (let ((organism-string (find-gene-organism-from-db gene)))
          (and organism-string (frame-fnamed organism-string t))
          )))
   (t (find-gene-organism-frame (->frames gene t)))
   ))
   


(defun crossmatched-entities-of
       (entity table-name 
               &key 
               (field1-name "id_from")
               (field2-name "id_to")
               (threshold-field "evalue")
               (threshold :all) 
               (comparison :lt)
               (entities nil) 
               (organisms :all)
               (commutative? nil)
               (safely? nil)
	       &aux slots
	       )

  #.(one-string-nl
     "Return a list of frames, each of which represents the contents of the"
     "crossmatch records from TABLE-NAME that are selected by the given criteria."
     "The given :FIELD1-NAME is used to match the given ENTITY.  If :COMMUTATIVE?"
     "is T, then ENTITY is also matched against the FIELD2-NAME of the table."
     "Values returned can be further restricted by :THRESHOLD (a comparison), "
     ":ENTITIES (a membership test) and :ORGANISMS (a membership test)."
     "The :THRESHOLD is compared against :THRESHOLD-FIELD in the table "
     "using a test designated by :COMPARISON.  For example: "
     "(crossmatched-entities-of \"slr1894\" \"crossblast\" "
     "   :threshold 1.0e-10 :comparison :le :organisms '(#$promed4)) "
     "See the documentation for additional examples."
     )

  (with-organisms-db (db)

    ;; Canonicalize the input arguments.

    (setq entity (fstring entity))
    (setq table-name (fstring table-name))
    (setq comparison (comparison-symbol-to-string comparison))
    (when (and (atom organisms) (not (eq :all organisms)))
      (setq organisms (list organisms)))
    (setq entities (mapcar 'fstring entities))

    ;; Verify the validity of the input arguments if asked to.

    (when safely?
      (unless (member table-name (esql "show tables"))
        (error "Unknown table ~A.  Not in database." table-name))
      (unless (or (eql threshold :all) (numberp threshold))
        (error "Invalid threshold value: ~A" threshold))
      (verify-organisms organisms))
    (setq organisms (->frames organisms t))

    ;; Get the table cols; This will dictate the frame contents.

    (setq slots (mapcar #'(lambda (name) (frame-fnamed (car name) t))
			(bio::esql (format nil "describe ~a" table-name))))

    ;; Select those entities from the crosstable which satisfy
    ;; the threshold

    (let* ((basic-query-string "select * from ~A where ~A = '~A'")
           (basic-forward-query
            (formatn 
             basic-query-string
             table-name field1-name entity))
           (basic-backward-query
            (when commutative?
              (formatn
               basic-query-string
               table-name field2-name entity)))
           (compare-phrase
            (if (eq threshold :all)
                ""
              (formatn " and ~A ~A ~A" 
                       threshold-field comparison 
                       (cond
                        ((floatp threshold)
                         (float-to-mysql-float-string threshold))
                        (t threshold)
                        ))))
           (forward-query 
            (formatn "~A~A" basic-forward-query compare-phrase))
           (backward-query 
            (when basic-backward-query
              (formatn "~A~A" basic-backward-query compare-phrase)))
           (forward-results (esql forward-query))
           (backward-results (when backward-query (esql backward-query)))
           (results (nconc forward-results backward-results))
           )

      ;; Turn the results into frames, translating everything inside
      ;; into frames as well.  

      (loop for item in results
	    as frame = (frame-fnamed (format nil "~a::~a" (car item) (cadr item)) t)
	    do (loop for value in item
		     as slot in slots
		     do (setf (slotv frame slot) (->frames value)))
	    collect frame)

      )))


;;; Auxiliary data and routines for BLASTGENE and GIVE-GRAPH


(defun hash-the-entities (ht entity1 entity-evalue-pairs)
  (loop for (entity2 evalue) in entity-evalue-pairs do
        (push (list entity1 entity2) (gethash evalue ht))))


(defun remove-subsets (list &key (element-test 'equal))
  (one-string-nl
   "LIST is a list of lists.  Any toplevel element which is a subset"
   "of another toplevel element is removed, and each toplevel element"
   "has any elements which are NIL removed.")
  (mapcar 
   (lambda (y) (remove-if #'null y))
   (remove-if 
    (lambda (x) 
        (loop for i in list do
              (when (not (eq x i)) 
                (when (subsetp x i :test element-test) (return t))
                )))
    list)))

(defun clust (list)
  (if (null list) 
      nil
    (let* ((gene (caar list))
           ;; PAIRS becomes all the elements of LIST that have
           ;; GENE as one of their two elements.
	   (pairs (loop for pair in list
			as found1 = (find gene pair :test #'equal) 
                        when found1 collect pair))
           ;; LIST becomes all elements the original LIST that do not
           ;; have GENE as one of their two elements.
	   (list (set-difference list pairs))
           ;; MEMBERS becomes a list of the other elements of each
           ;; element of PAIRS: i.e, each element of PAIRS has GENE as
           ;; one of its two elements.  This is a list of the other elements.
	   (members 
            (loop for pair in pairs 
                  as found2 = (find gene pair :test-not #'equal)
                  when found2 collect found2)))
      (concatenate 
       'list 
       (loop for m in members
             ;; Each M is an element paired with GENE.
             ;; If M is also paired with some other element collect that pair.
             ;; FOUND3 is a list of such pairs.
             as found3 = (loop for pair in list 
                               as f = (find m pair :test #'equal) 
                               when f collect pair)
             collect 
             (cons 
              gene 
              (cons 
               m 
               (loop for n in (cdr (member m members))
                     collect
                     (loop for pair in found3 
                           when (find n pair :test #'equal) 
                           return n)))))
       (clust list)))))


(defun blastgene (protein threshold)
  (let* ((ht (make-hash-table :test 'equal))
         (protein-frame (canonicalize-protein-designator protein t))
         (protein-name (#^Fname protein-frame))
         (threshold-string
          (cond
           ((numberp threshold) 
            (float-to-mysql-float-string (float threshold)))
           (t threshold)
           )))
    (with-organisms-db (db)
      (let* ((query1
              (formatn
               (one-string 
                "select id_to, evalue from crossblast "
                "where id_from = '~A' and evalue <= ~A")
               protein-name 
               threshold-string))
             (proteins 
              (remove (list protein-name 0) (esql query1) :test #'equal)))
        (hash-the-entities ht protein-name proteins)
        (loop for proteins-list on proteins
	      as id_from = (caar proteins-list) do
	      (hash-the-entities
               ht
               id_from 
               (loop for j in (cdr proteins-list)
                     as id_to = (car j)
                     as query2 = 
                     (formatn
                      (one-string 
                       "select id_to, evalue from crossblast "
                       "where id_from = '~A' "
                       "and id_to = '~A' "
                       "and evalue <= ~A")
                      id_from id_to threshold-string
                      )
                     as list = (car (esql query2))
                     when list collect list
                     )))))
    ht))

;;threshold as number
(defun give-graph (ht threshold)
  (->frames
   (let* ((s nil))
     (maphash 
      #'(lambda (evalue pairs) 
          (if (<= evalue threshold) 
              (setf s (concatenate 'list s pairs))))
      ht)
     (remove-subsets (clust s)))))
