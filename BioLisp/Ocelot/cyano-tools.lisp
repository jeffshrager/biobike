(in-package :cl-user)

(defpackage :ocelot (:use :common-lisp))

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

;;; This stuff is not used currently.

(defun cyano-path ()
  (cl-user:translate-simple-lp "bioetc:data;ocelot;cyano6803base.ocelot"))

;;;; Useful routines:

;;;; WITH-EACH-CYANODB-RECORD
;;;;  -- Iterate over each record in a file.  
;;;;     Each record is slurped in using READ.
;;;;     By default ignores header record.

;;;; CLEAN-UP-CYANO-RECORDS (output-file)
;;;;  -- Writes a cleaned-up version of the DB to a file.
;;;;     See documentation just above this function for specifics
;;;;     of what exactly it does.

;;;; *ALL-CYANO-THIRD-HEADERS*, *ALL-CYANO-FOURTH-HEADERS*
;;;; These are lists of all the KEYS for the third and fourth
;;;; elements of each record.  They can be recreated by calling
;;;; ALL-CYANO-HEADERS.


(defmacro with-cyanodb ((s file) &body body)
  `(with-open-file (,s ,file)
     (unless (read ,s nil nil nil) (error "Oops.  EOF at start!"))
     (flet ((next-cyano-record () (read ,s nil nil nil)))
       ,@body
       )))

(defmacro with-each-cyanodb-record 
          ((rec file 
                &optional (record-count nil)
                &key 
                (header-present t)
                (return-header nil)
                (show-progress-every nil))
           &body body)
  (let ((rcount (gensym "RCOUNT-"))
        (stream (gensym "STREAM-"))
        (exit (gensym "EXIT-")))
    `(let ((,rcount 0) (,rec nil))
       (with-open-file (,stream ,file :direction :input)
         ,@(when (and header-present (null return-header))
             `((unless (read ,stream nil nil nil) (error "EOF immediately!"))))
         (block ,exit
           (loop
            (when (null (setq ,rec (read ,stream nil nil nil)))
              (return-from ,exit))
            (incf ,rcount)
            ,@(when record-count `((setq ,record-count ,rcount)))
            ,@(when show-progress-every
                `((when (and ,show-progress-every
                             (zerop (mod ,rcount ,show-progress-every)))
                    (format t ".") (force-output t))))
            ,@body
            ))))))
             

(defun all-cyano-headers (&key (verbose t))
  (let ((distinct-third-headers nil)
        (distinct-fourth-headers nil)
        (progress (if verbose 200 nil))
        (record-count 0))
    (when verbose (format t "~&~%;; Reading ~A~%" (cyano-path)))
    (with-each-cyanodb-record 
        (next-record (cyano-path) record-count :show-progress-every progress)
      (let ((third-header-value-pairs (third next-record))
            (fourth-header-value-pairs (fourth next-record)))
        (dolist (hvp third-header-value-pairs)
          (pushnew (first hvp) distinct-third-headers))
        (dolist (hvp fourth-header-value-pairs)
          (pushnew (first hvp) distinct-fourth-headers))
        ))
    (when verbose (terpri))
    (setq distinct-third-headers 
          (sort distinct-third-headers #'string-lessp :key #'symbol-name))
    (setq distinct-fourth-headers 
          (sort distinct-fourth-headers #'string-lessp :key #'symbol-name))
    (list distinct-third-headers distinct-fourth-headers record-count)
    ))
	      

;; Does the following:
;;   -- removes :CREATOR and :CREATION-DATE slots
;;   -- removes NIL if it is the last element in the record.
;;   -- changes all header symbols to keywords.
;;   -- changes all symbols in OCELOT package to keywords
;;   -- gets rid of the first record (the metainformation)

(defun clean-up-cyano-records (output-file)
  (format t "~&~%;; Writing cleaned-up Cyano db to ~A~%" output-file)
  (with-open-file (out output-file :direction :output :if-exists :supersede)
    (let ((c 0))
      (with-each-cyanodb-record 
          (record (cyano-path) c :show-progress-every 200)
        (terpri out)
        (pprint (clean-up-cyano-record record '(:creator :creation-date)) out))
      c
      )))

(defun clean-up-cyano-record (r excise-list)
  (labels ((to-keyword (x) (intern (symbol-name x) #.(find-package :keyword)))
           (header-list-to-keyword-header (x)
             (if (and (listp x) 
                      (symbolp (first x))
                      (not (keywordp (first x))))
                 (cons (to-keyword (first x)) (rest x))
               x))
           (excise (pairs)
             (remove-if 
              #'(lambda (x) (and (listp x) (member (first x) excise-list)))
              pairs)))
    (death-to-ocelot
     (if (fourth r)
         (list (first r) (second r) 
               (mapcar #'header-list-to-keyword-header (excise (third r)))
               (mapcar #'header-list-to-keyword-header (excise (fourth r))))
       (list (first r) (second r)
             (mapcar #'header-list-to-keyword-header (excise (third r))))))))
	    
(defun death-to-ocelot (tree)
  (cond
   ((and (symbolp tree) (eq #.(find-package :ocelot) (symbol-package tree)))
    (intern (symbol-name tree) #.(find-package :keyword)))
   ((not (listp tree)) tree)
   ((null tree) nil)
   ;; Dotted list test
   ((not (null (cdr (last tree))))
    (cons (death-to-ocelot (car tree)) (death-to-ocelot (cdr tree))))
   (t (mapcar #'death-to-ocelot tree))
   ))


(defparameter *all-cyano-third-headers*
  '(APPEARS-IN-LEFT-SIDE-OF
    APPEARS-IN-RIGHT-SIDE-OF
    AROMATIC-RINGS
    ASSUME-UNIQUE-ENZYMES
    ATOM-CHARGES
    ATOM-CHIRALITY
    ATOMIC-NUMBER
    ATOMIC-WEIGHT
    BASE-URL
    :BOOKKEEPING-SLOT?
    CACHED-STATISTICS
    CANONICALIZE-FOR-INDEX?
    CARDINALITY
    :CARDINALITY
    :CARDINALITY-MAX
    CAS-REGISTRY-NUMBERS
    CATALYZES
    CHARGE
    CHEMICAL-FORMULA
    CIRCULAR?
    CITATIONS
    :COLLECTION-TYPE
    COMMENT
    COMMENT-INTERNAL
    COMMON-NAME
    COMPONENT-OF
    COMPONENTS
    CONTACT-EMAIL
    :CREATION-DATE
    :CREATOR
    DBLINKS
    DELTAG0
    DIGITS-OF-PRECISION
    DISPLAY-COORDS-2D
    :DOCUMENTATION
    :DOMAIN
    EC-LIST
    EC-NUMBER
    EC-NUMBER-OLD
    ENZYMATIC-REACTION
    ENZYME
    GENE
    GENOME
    :GET-METHODS
    :HIDE-SLOT?
    HISTORY
    IN-PATHWAY
    INHERITANCE-TYPE
    :INHERITANCE-TYPE
    :INSTANCE-NAME-TEMPLATE
    :INVERSE
    :KEY-SLOT
    LAYOUT-ADVICE
    LEFT
    LEFT-END-POSITION
    LOCATIONS
    :MAXIMUM-CARDINALITY
    :MEMBER-SORT-FN
    :MINIMUM-CARDINALITY
    MODIFIED-FORM
    MOLECULAR-WEIGHT
    N+1-NAME
    N-1-NAME
    N-NAME
    NET-REACTION-EQUATION
    :NUMERIC-MAXIMUM
    :NUMERIC-MINIMUM
    OFFICIAL-EC?
    OPTIONAL-ANNOTATIONS
    ORIGINAL-FRAME-TYPE
    OVERVIEW-NODE-SHAPE
    OCELOT::PARENTS
    PATHWAY-LINKS
    PGDB-AUTHORS
    PGDB-HOME-PAGE
    PGDB-NAME
    PI
    PKA1
    PKA2
    PKA3
    POLYMERIZATION-LINKS
    PREDECESSORS
    PRIMARIES
    :PRIVATE?
    PRODUCT
    QUERYABLE?
    REACTION
    REACTION-LIST
    :READ-ONLY
    REFERENT-FRAMES
    RIGHT
    RIGHT-END-POSITION
    SCHEMA?
    SEARCH-OBJECT-CLASS
    SENDER
    SMILES
    SPECIES
    SPONTANEOUS?
    STATIC-SEARCH-URL
    STRAIN-NAME
    STRUCTURE-ATOMS
    STRUCTURE-BONDS
    SUB-PATHWAYS
    SUBREACTIONS
    SUPER-PATHWAYS
    SUPERATOMS
    SYNONYMS
    SYSTEMATIC-NAME
    TAXONOMIC-DOMAIN
    :TEMPLATE-FORMATING-FN
    TEXT
    UNITS
    UNMODIFIED-FORM
    VALENCE
    :VALUE-TYPE
    VARIANTS?
    ))
	    
(defparameter *all-distinct-fourth-headers*
  '(ACTIVATORS
    COMPONENT-OF
    COMPONENTS
    DELTAG0
    EC-NUMBER
    INHIBITORS
    LEFT
    MOLECULAR-WEIGHT
    NEIDHARDT-SPOT-NUMBER
    POLYMERIZATION-LINKS
    RIGHT
    SUPERATOMS
    ))


