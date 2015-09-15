;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi) 

;;; +=========================================================================+
;;; | Copyright (c) 2013 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

;;; Author: JP Massar, Jeff Elhai, Arnaud Taton. 


(DEFUN BB-Trim-blanks (string &KEY left right)
  "Removes spaces from left and/or right"
  (LET ((result string)
        (point))
    (IF (OR left (NOT right))
        (PROGN
          (SETF point (POSITION #\space result :TEST-NOT 'EQUAL))
          (SETF result (SUBSEQ result point))))
    (IF (OR right (NOT left))
        (PROGN 
          (SETF point 
             (1+ (POSITION #\space result :TEST-NOT 'EQUAL :FROM-END T)))
          (SETF result (SUBSEQ result 0 point))))
    result))


(DEFUN BB-Break-string (string at exact)
  (LET* ((string+ (CONCATENATE 'STRING string " "))
         (length (LENGTH string))
         (coord 0)
         (breakpoint at)
         (r-coord (MIN breakpoint length))
         (r-pos)
         (segment)
         (broken-string))

    (LOOP UNTIL (>= coord length)
          DO
          (COND 
           (exact
            (PUSH (SUBSEQ string+ coord r-coord) broken-string)
            (SETF coord (+ coord breakpoint)))
           (T
            (SETF segment (SUBSEQ string+ coord (+ r-coord 1)))
            (SETF r-pos (POSITION #\space segment :FROM-END T))
            (IF-TRUE r-pos
                     THEN (SETF r-coord (+ coord r-pos))
                     ELSE (SETF r-coord (MIN (+ coord breakpoint -1) length))
                     (SETF r-pos (- r-coord coord -1)))
            (PUSH (SUBSEQ segment 0 r-pos) broken-string)
            (SETF coord (+ r-coord 1))))
          (SETF r-coord (MIN (+ coord breakpoint) length))
          FINALLY (RETURN-FROM BB-Break-string (REVERSE broken-string)))))


(DEFUN Convert-to-number-maybe (entity &KEY else-nil)
  (IF (AND (TYPEP entity 'String) (> (LENGTH entity) 0))
      (LET* ((contains-colon (FIND #\: entity))
             (contains-quote (FIND #\" entity))
             (contains-bar (FIND #\| entity))
             (contains-pound (FIND #\# entity))
             (contains-space (FIND #\space entity))
             (contains-comma (FIND #\, entity))
             (contains-single-quote (FIND #\' entity))
             (contains-left-par (FIND #\( entity))
             (contains-e (FIND #\e entity)) 
                 ; converts single-precision float to double if necessary
             (item 
                (COND 
                   ((AND (NOT contains-colon) (NOT contains-quote)
                         (NOT contains-bar) (NOT contains-pound)
                         (NOT contains-space) (NOT contains-comma)
                         (NOT contains-single-quote) (NOT contains-left-par))
                         (IF (NOT contains-e)
                             (STRING-TO-LISP-OBJECT entity)
                             (LET* ((temp (SUBSTITUTE #\d #\e entity))
                                    (temp-item (STRING-TO-LISP-OBJECT temp)))
                               (IF (AND (NUMBERP temp-item)
                                        (< temp-item *big-number*)
                                        (> temp-item (/ *big-number*)))
                                   (STRING-TO-LISP-OBJECT entity)
                                   temp-item))))
                   ((< (LENGTH entity) 9) NIL)
                   ((EQUAL "#.EXCL::*" (SUBSEQ entity 0 9))
                       (STRING-TO-LISP-OBJECT entity)))))
        (IF (AND item (OR (TYPEP item 'Number) (TYPEP item 'List)))
            item
            (IF ELSE-NIL NIL entity)))
      (IF ELSE-NIL NIL entity))
)

(DEFUN Convert-to-list-maybe (entity)
  (IF (AND (TYPEP entity 'String) 
           (> (LENGTH entity) 1)
           (EQUAL (REF entity 1) "(")
           (EQUAL (REF entity (LENGTH entity)) ")"))
      (STRING-TO-LIST entity)
	  entity)
)

(defun convert-aux (entity type each? if-possible)
  (if each?
      (if (listp entity)
          (loop for item in entity
                collect (bb-convert item type))
          (IF (AND if-possible (SYMBOL= type 'Number) (TYPEP entity 'STRING))
              (CONVERT-TO-NUMBER-MAYBE entity)
              (bb-convert entity type)))
      (IF (AND if-possible (SYMBOL= type 'Number) (TYPEP entity 'STRING))
          (CONVERT-TO-NUMBER-MAYBE entity)
          (bb-convert entity type))))


(DEFUN Is-X-map (function list)
  (UNLESS (LISTP list)
    (ERROR "EACH option must be followed by a list!"))
  (LOOP FOR item IN list
        COLLECT (FUNCALL function item)))

(DEFUN Maybe-labeled-sequence (seq label labeled?)
   (IF labeled?
       (MAKE-LABELED-SEQUENCE 
             :LABEL label
             :SEQUENCE seq)
       seq))

(DEFUN Description-of-full-frame-aux (entity)
  (LET* ((*suppress-warnings* T)
         (slot-width 30)
         (fields-in-order 
          (LIST
           #$fname #$isa #$organism-entity-type #$genetic-name
           #$annotation #$description #$best-hit-descr #$best-hit-id-pct
           #$best-hit-accession
           #$cog-id #$go-id #$go-guesses
           #$organism #$taxonomy #$contiguous-sequence
           #$completed #$date #$genbank-accession-number #$NCBI-accession-number 
           #$version #$Pubmed-pubid #$reference #$links 
           #$sequence-length #$circular
           #$from #$to #$direction #$architecture
           #$transmembrane-regions
           #$encodes-protein
           #$start-unknown #$end-unknown
           #$contiguous-sequences
           #$habitat #$filamentous #$heterocystous
           #$organism-prefix #$nicknames #$organism-symbols #$alternative-names
           #$organism-loaded? #$organism-info-loaded
           #$genes-sorted-by-position #$fragments-sorted-by-position
           #$genes
           #$noncoding-genes
           #$fragments
           #$proteins
           #$sequencer #$submitter
           #$organism-data-directory
           #$protein-sequence-file
           #$genome-sequence-stream
           #$genome-sequence-file
           #$internal-sequence-info))
         (slots (FRAME-SLOTS-OF entity)))

    (without-code-walker
      (LOOP FOR field IN fields-in-order
            WHEN (MEMBER field slots)
            DO (FORMAT T "~&~A~A"
                       (BB-FIT (BB-STRING-OF field) slot-width)
                       (SLOTV entity field))
            (ASSIGN slots = (DELETE field slots :TEST 'EQUAL)))
      (LOOP FOR slot IN slots
            DO (FORMAT T "~&~A~A"
                       (BB-FIT (BB-STRING-OF slot) slot-width)
                       (SLOTV entity slot))))))

(bbi::define-function bbi::edit-object
  summary "Brings up the object in a data editor"
  required obj
  body 
  (multiple-value-bind (editable? why-not)
      (forward-package-funcall :vpl :de-edit-object? obj)
    (if (not editable?)
        (formatt "~A" why-not)
      (forward-package-funcall :wb :make-data-editor-object :data obj)
      )))
      
(DEFUN Description-of-aux (entity length display labeled full
                                  &KEY from to direction description)
  (without-code-walker
    (LET ((*suppress-warnings* T)
          (descriptors
           (LIST #$Annotation #$Best-hit-descr #$EC-description 
                 #$COG-description #$Description)))

      (TYPECASE entity
        ((OR gene protein)
         (IF-TRUE 
          full
          THEN 
          (IF (EQUAL cl-user::*blast-lookup-database* :SEED)
              (VIEW-GENE entity)
            (EDIT-OBJECT entity)
            ;; (DESCRIPTION-OF-FULL-FRAME-aux entity)
            )
          ELSE 
          (LET* ((item (IF (TYPEP entity 'Protein)
                           (SLOTV entity #$Gene)
                         entity))
                 (result
				  (LET ((pre-result NIL))
                    (WHEN (OR from to direction)
                        (IF direction 
                            (SETF
                             pre-result
                             (CONS (REF item #$direction) pre-result)))
                        (IF to 
                            (SETF pre-result (CONS (REF item #$To) pre-result))
                          )
                        (IF from 
                            (SETF
                             pre-result (CONS (REF item #$From) pre-result)))
                        (IF (= (LENGTH pre-result) 1)
                            (FIRST pre-result))
                          )
					(IF (OR description (NOT (OR from to direction)))
                      (LOOP FOR descriptor IN descriptors
                            AS description = (SLOTV item descriptor)
                            AS min-length = (MIN length (LENGTH description))
                            DO (IF description
                                   (RETURN (SUBSEQ description 0 min-length)))))
					))
                )
            (IF (AND display (NOT full))
                (FORMAT T "~&~A~,10T~A" entity result))
            (IF labeled
                (APPEND (LIST entity) (ENSURE-LIST result))
                result))))
        
        (organism (EDIT-OBJECT entity)
                  ;; (DESCRIPTION-OF-FULL-FRAME-aux entity)
                  )				 
        (frame ;; (IF (bbl-toplevel?) ...) 
               (DESCRIPTION-OF-FULL-FRAME-aux entity)
               )
        (list 
         (LET ((list-type
                (IF (EVERY 'NULL (MAPCAR 'BB-IS-LIST? entity))
                    "Simple" "Complex")))
           (FORMAT NIL "~A list of ~A elements" 
                   list-type (LENGTH entity))))
        (string (FORMAT NIL "String of ~A characters" 
                        (LENGTH entity)))
        (table  (DESCRIBE-GARRAY entity :type "TABLE")
                "TABLE")
        (symbol "SYMBOL")
        (number (FORMAT NIL "Number of type ~A"
                        (TYPE-OF entity)))
        (OTHERWISE 
         (ERROR "Type of entity ~A not describable"
                (TYPE-OF entity))))
)))

; ============= ELEMENT/S-OF auxiliary functions ===========
(DEFUN Complex-list-element (list indices &REST more-indices)
  (IF (AND (LISTP indices) more-indices)
      (ERROR (S+ "COMPLEX-LIST-ELEMENT cannot have both a list "
                 "of indices and additional indices")))
  (LET* ((indices
           (IF (LISTP indices)
               indices
               (CONS indices more-indices)))
         (first-index (POP indices)))
    (LOOP WHILE list
       DO (IF (NOT indices)
              (RETURN (REF list first-index)))
          (SETF list (REF list first-index))
          (SETF first-index (FIRST indices))
          (SETF indices (REST indices)))
))

(DEFUN Element/s-of-turntable (entity item/s +label)
  (SETF item/s (ENSURE-LIST item/s))
  (IF (TYPEP entity 'Labeled-sequence)
      (SETF entity (LABELED-SEQUENCE-SEQUENCE entity)))
  (TYPECASE entity
     (List (ELEMENT/S-OF-LIST-aux entity item/s :LABELED +label))
     (Table (ELEMENT/S-OF-TABLE-aux entity item/s :LABELED +label))
     (String (ELEMENT/S-OF-STRING-aux entity item/s :LABELED +label))
        ; Needs error catching above
     (Frame (ELEMENT/S-OF-FRAME-aux entity item/s :LABELED +label))
     (OTHERWISE 
        (ERROR (S+ "INTERNAL ERROR! argument passed to "
                   "ELEMENT/S-OF-turntable is of the illegal type "
                   "~a.")
               (TYPE-OF entity))))
)

(DEFUN Element/s-of-string-aux (target item-list &KEY labeled)
  (LET* ((all-indices (ILIST 1 (1+ (LENGTH target))))
         (items (SUBST all-indices '@ (FLATTEN item-list) :TEST 'SYMBOL=)))
                 
    (SETF items (SUBST all-indices @ (FLATTEN items) :TEST 'EQUAL))
    (SETF items
      (LOOP FOR index IN (FLATTEN items)
        DO (IF (NOT (IS-POSITIVE-NUMBER? index))
               (ERR+ Problem "Indices to ELEMENT/S-OF for a string "
                     Indent  "must be positive numbers, not ~A."
                     Format-args index))
           (IF (> (ROUND index) (LENGTH target))
               (ERR+ Problem "Indices to ELEMENT/S-OF for a string "
                     Indent  "must be no greater than the length of "
                     Indent  "the string (~A), not ~A."
                     Format-args (LENGTH target) index))                     
           COLLECT (IF (< index 0.5)
                       1
                       (ROUND index))))
    (LOOP FOR i IN items
          COLLECT
            (IF labeled 
                (LIST i (REF target i))
                (REF target i)))
))

(DEFUN Element/s-of-table-aux (table item/s &KEY labeled)
  (LET ((index-sets (ALL-SETS-IN item/s))
        (no-more-@))
    (LOOP UNTIL no-more-@
          DO (SETF no-more-@ T)
             (SETF index-sets
               (LOOP FOR old-indices IN index-sets
                     AS @-position = (OR (POSITION '@ old-indices) 
                                         (POSITION @ old-indices))
                     AS expanded-labels =
                        (IF @-position 
                            (IF (= @-position 0)
                                (GARRAY-COMPONENT-INDICES table)
                                (APPLY 'GARRAY-COMPONENT-INDICES table  
                                      (SUBSEQ old-indices 0 @-position))))
                     AS new-indices =
                        (IF expanded-labels
                            (PROGN
                               (SETF (NTH @-position old-indices) expanded-labels)
                               (ALL-SETS-IN old-indices))
                            (LIST old-indices))
                     DO (IF @-position (SETF no-more-@ NIL))
                     APPEND new-indices)))

    (LOOP FOR index-set IN index-sets
          AS value = (APPLY 'REF table index-set)
          COLLECT 
            (IF labeled
                (JOIN index-set value)
                value))
))

          
(DEFUN Element/s-of-list-aux (list indices &KEY labeled)
  (LET* ((index-sets (ALL-SETS-IN indices))
         (no-more-@))
    (LOOP UNTIL no-more-@
          DO (SETF no-more-@ T)
             (SETF index-sets
                (LOOP FOR old-indices IN index-sets
                      AS @-position = (OR (POSITION '@ old-indices) 
                                          (POSITION @ old-indices))
                      AS list-length =
                         (IF @-position 
                             (IF (= @-position 0)
                                 (LENGTH list)
                                 (LENGTH (COMPLEX-LIST-ELEMENT list (SUBSEQ old-indices 0 @-position)))))
                      AS new-indices =
                         (IF list-length
                             (PROGN
                                (SETF (NTH @-position old-indices)
                                      (ILIST 1 (1+ list-length)))
                                (ALL-SETS-IN old-indices))
                             (LIST old-indices))
                      DO (IF @-position (SETF no-more-@ NIL))
                      APPEND new-indices)))
         
    (LOOP FOR index-set IN index-sets
          AS value = (COMPLEX-LIST-ELEMENT list index-set)
          COLLECT 
            (IF labeled
                (JOIN index-set value)
                value))
))


(DEFUN Element/s-of-frame-aux (target item-list &KEY labeled)
  (LET* ((all-slots (FRAME-SLOTS-OF target))
         (item/s
            (LOOP FOR item IN (FLATTEN item-list)
                 COLLECT
                   (COND 
                      ((NULL item-list) NIL)
                      ((OR (SYMBOL= item '@) (EQUAL item @)) all-slots)
                      ((SYMBOLP item) (FRAME-FNAMED (SYMBOL-NAME item)))
                      ((STRINGP item) (FRAME-FNAMED item))
                      ((IS-FRAME? item) item)
                      (T (ERROR (S+ "INTERNAL ERROR in ELEMENT-OF-FRAME! " 
                                    "~A is not a legal type!" *newline*
                                    "Please report circumstances to authorities")
                             item)))))
         )
    (LOOP FOR i IN (FLATTEN item/s)
          COLLECT
            (IF labeled 
                (LIST i (SLOTV target i))
                (SLOTV target i)))
))

; ============= end ELEMENT/S-OF auxiliary functions ===========

(DEFMACRO FOR-EACH (&REST forms)
  #.(one-string-nl
     "A variation on LOOP.  Ask for help about LOOP to understand FOR-EACH."
     "Instead of saying (loop for j from 1 to 10 ...)" 
     "you say (for-each j from 1 to 10 ...)"
     "There are some other stylistic differences as well but the functionality"
     "is equivalent"
     )
  (LET ((1st-pass
         (LOOP 
             ; FOR i FROM 1 TO 4   ; WHY DO WE NEED THIS LIMITATION ON INITS?
          AS form-length = (LENGTH forms)
          AS pre-init = (POSITION 'INIT forms :TEST 'SYMBOL=)
          AS init = (AND pre-init  
                         (< pre-init (- form-length 3))
                         (SYMBOL= (Nth (+ pre-init 2) forms) '=)
                         pre-init)
          AS pre-initialize = (POSITION 'INITIALIZE forms :TEST 'SYMBOL=)
          AS initialize = (AND pre-initialize  
                               (< pre-initialize (- form-length 3))
                               (SYMBOL= (Nth (+ pre-initialize 2) forms) '=)
                               pre-initialize)
          AS next-init
          = (COND
             ((NOT init) initialize)
             ((NOT initialize) init)
             (T (MIN init initialize)))
          WHEN next-init
          APPEND (LIST (Nth next-init forms) 
                       (Nth (+ 1 next-init) forms) 
                       (Nth (+ 2 next-init) forms) 
                       (Nth (+ 3 next-init) forms))
          WHEN next-init
          DO (SETF forms 
                   (CONCATENATE 'LIST 
                                (biolisp::FIRST-N next-init forms)
                                (LAST-N (- form-length next-init 4) forms)))
          UNTIL (NOT next-init)))
        )
    (SETF forms (CONCATENATE 'LIST 1st-pass '(FOR) forms))
    (SETF forms (SUBSTITUTE 'FOR 'FOR-EACH forms :TEST 'SYMBOL=))
    `(BBL:LOOP ,@forms)
    ))


(DEFUN Forget-aux (symbol contents? kill-variable kill-function)
  (LET ((no-instruction (NOT (OR kill-variable kill-function))))
    (IF-TRUE
     contents?
     THEN (LOOP FOR s IN (ENSURE-LIST symbol)
                COLLECT (FORWARD-FUNCALL 
                         'FORGET-aux s NIL kill-variable kill-function))
     ELSE (LET ((symbol-type
                 (COND
                  ((AND (BOUNDP symbol) (FBOUNDP symbol) 
                        (AND (NOT kill-variable) (NOT kill-function)))
                   (ERROR (ONE-STRING 
                           "'~A' is both a variable and a function. "
                           "Specify one or both.")
                          symbol))
                  ((AND (BOUNDP symbol) (FBOUNDP symbol) 
                        kill-variable kill-function)
                   (MAKUNBOUND symbol)
                   (FMAKUNBOUND symbol)
                   "variable and function")
                  ((AND (BOUNDP symbol) (OR kill-variable no-instruction))
                   (SETF kill-variable T)
                   (MAKUNBOUND symbol) 
                   "variable")
                  ((AND (FBOUNDP symbol) (OR kill-function no-instruction))
                   (SETF kill-function T)
                   (FMAKUNBOUND symbol)
                   "function")
                  (kill-variable
                   (ERROR "Variable '~A' doesn't exist" symbol))
                  (kill-function
                   (ERROR "Function '~A' doesn't exist" symbol))
                  (T (ERROR "Symbol '~A' doesn't exist" symbol)))))
            (IF-TRUE (CALLED-FROM-VPL) 
                     THEN (IF kill-variable
                              (FORWARD-PACKAGE-FUNCALL
                               'VPL 
                               :REMOVE-ITEM-FROM-MY-VARIABLES-MENU symbol))
                     (IF kill-function
                         (FORWARD-PACKAGE-FUNCALL 
                          'VPL 
                          :REMOVE-ITEM-FROM-MY-FUNCTIONS-MENU symbol)))
            (FORMAT T "~A ~A forgotten~&" symbol-type symbol)
            T))))



(DEFUN Gene-left-or-right-of (entity replicon function)
  (LET ((to-left (SYMBOL= function 'GENE-LEFT-OF)))
    (TYPECASE entity
      (Gene
       (IF replicon 
           (ERR+
            problem "You may only use the IN keyword when you pass a coordinate."
            advice  "Do not use this keyword if you pass a gene or a protein."
            indent  "You must pass a coordinate as entity, for example:"
            indent  "(~A 104000 IN A7120.chromosome)."
            help~A
            format-args
            function function)
         )            
       (LET ((direction (SLOTV entity #$DIRECTION)))
         (IF-TRUE (OR (AND to-left (EQUAL direction :F))
                      (AND (NOT to-left) (EQUAL direction :B)))
                  THEN (GENE-UPSTREAM-FROM entity)
                  ELSE (GENE-DOWNSTREAM-FROM entity))))
      (Number
       (IF (NOT replicon)
           (ERR+ problem "Coordinate cannot be interpreted." 
                 advice  "Use the keyword IN, for example:"
                 indent  "(~A coordinate IN replicon)."
                 help~A
                 format-args
                 function function)
         )
       (IF (TYPEP replicon 'Organism)
           (SETF replicon (LONE-REPLICON-OR-ERROR replicon))) 
     
       ;; Changed last argument to context-of-coord-aux (no-display) 
       ;; to T to prevent spurious output. -- JP

       (LET* ((context-info 
               (FORWARD-FUNCALL 
                'CONTEXT-OF-coord-aux entity replicon NIL NIL t)) 
              (context (biolisp::FIRST context-info)))
         (COND
          ((EQUALP context "I")
           (FORWARD-FUNCALL 'GENE-LEFT-OR-RIGHT-OF
                            (biolisp::SECOND context-info) NIL function))
          (to-left (biolisp::SECOND context-info))
          (T (biolisp::THIRD context-info)))))
      )))

(DEFUN Get-genes-by-pathway-bypass (old-kegg-gene)
  (LET* ((kegg-gene (SUBSEQ old-kegg-gene 5))
         (kegg-path1 "http://www.kegg.jp/kegg-bin/download?entry=")
         (kegg-path3 "&format=kgml")
         (kegg-path (FORWARD-FUNCALL 'BB-JOIN (LIST kegg-path1 kegg-gene kegg-path3)))
         (kegg-file (bio::WEB-PAGE-CONTENTS kegg-path))
         (kegg-prefix (FORWARD-FUNCALL 'MATCH-OF-PATTERN-ONE-ITEM-aux
                         "^[^\\d]*" kegg-gene 1 '(NIL NIL T NIL) NIL NIL NIL NIL))
         (pattern (FORWARD-FUNCALL 'BB-JOIN (LIST kegg-prefix ":([a-zA-z0-9]*)")))
         (gene-names (FORWARD-FUNCALL 'MATCH-OF-PATTERN-ONE-ITEM-aux
               pattern kegg-file *big-number* '(NIL NIL NIL T) NIL NIL NIL NIL))
        )
  gene-names
))

(DEFUN Genes-in-pathway/s-aux (query entity)
  (LET* ((organism 
            (IF (TYPEP entity 'Organism)
                entity
                (SLOTV entity #$Organism )))
         (kegg-org (FORWARD-FUNCALL 'KEGG-ORG-OF organism))
         (organism-OK (<= (LENGTH kegg-org) 5))
         (query-list (IF organism-OK
            (COND 
               ((STRINGP query) (LIST query))
               ((AND (= (LENGTH query) 2) ; e.g. ("00010" "glycolysis")
                     (STRINGP (SECOND query))
                     (ONLY-NUMERALS (FIRST query))
                     (NOT (ONLY-NUMERALS (SECOND query))))
                   (LIST (FIRST query)))
               ((AND (= (LENGTH query) 2) 
                        ; e.g. ("cho metab" (("00010" "glycolysis"))...)
                     (LISTP (SECOND query))
                     (LISTP (FIRST (SECOND query))))
                  (SECOND query))
               (T query)))) ; e.g. (("00010" "glycolysis")("00040" "PG pathway")...)
         (pathways (IF organism-OK
            (LOOP FOR item IN query-list
                  AS pathway 
                   = (IF (LISTP item) 
                         (FIRST item) 
                         item)
                 COLLECT (S+ "path:" kegg-org pathway))))
         (pathway-genes-per-kegg (IF organism-OK
           (REMOVE-DUPLICATES
#|             (FLATTEN
                  (LOOP FOR pathway IN pathways
                      AS path-array 
                        = (SECOND 
                            (SECOND 
                             ;; soap call
                             (kegg::GET-GENES-BY-PATHWAY :PATHWAY_ID pathway)
                             ))
                      COLLECT (ARRAY-TO-LIST path-array))) |#
                  (LOOP FOR pathway IN pathways
                        AS genes = (GET-GENES-BY-PATHWAY-BYPASS pathway)
                        APPEND genes)
                 :TEST 'EQUAL)))
#|
         (pathway-genes (IF organism-OK
            (LOOP FOR kegg-gene IN pathway-genes-per-kegg
                 AS gene-name1 = (REMOVE #\_ (SUBSEQ kegg-gene 4) :TEST 'EQUAL)
                 AS gene-name2 = (SUBSEQ kegg-gene 4)
                 AS gene = (OR (BB-GENE-NAMED gene-name1) (BB-GENE-NAMED gene-name2))
                 COLLECT gene)))
|#
         (pathway-genes (IF organism-OK
            (LOOP FOR kegg-gene IN pathway-genes-per-kegg
                  AS gene = (BB-GENE-NAMED kegg-gene)
                  WHEN gene COLLECT gene)))
         )
    (COND
       ((NOT organism-OK) kegg-org)
       ((EQUAL organism entity)
           pathway-genes)
       (T (INTERSECTION pathway-genes (GENES-OF-CONTIG entity))))))


(defvar *simple-genes-of-search* t)

(DEFUN Genes-of-one-segment (contig from-coord to-coord wrap 
                                    truncate exclude-overlaps)
  "(INTERNAL USE) Finds all genes within a segment of a contig"

  (LET* ((contig-size (SLOTV contig #$SEQUENCE-LENGTH))
         (circular? (SLOTV contig #$CIRCULAR))
         (sorted-fragments (SLOTV contig #$Fragments-sorted-by-position))
         (last-index-of-contig (- (LENGTH sorted-fragments) 1))
         (from-left) (from-right) (to-left) (to-right)
         (from-in-gene) (to-in-gene)
         (index-list)
         )

    ;; ************ INTERNAL FUNCTIONS ************

    (LABELS
        ((In-gene? (coord index)
           ;; Returns T if coord within a gene, otherwise NIL
           (LET* ((gene (AREF sorted-fragments index))
                  (start-coord (SLOTV gene #$From ))
                  (end-coord (SLOTV gene #$To )))
             (COND
              ((<= coord end-coord) T)
              ((<= end-coord start-coord) T)
              (T NIL))))
       
         (Make-range (coord)
           ;; Returns indices of genes on either side of coord as a list
           ;;    and T if coord within a gene, otherwise NIL
           (LET* ((in-gene "unknown")
                  (index1) (indexa) (indexb) (range))

             (MULTIPLE-VALUE-SETQ (index1 indexa indexb) 
                 (BINSEARCH coord sorted-fragments 
                            :KEY '#^from :IF-NOT-FOUND :range))
             (SETF range
                   (COND
                    (index1 
                     (SETF in-gene T)
                     (LIST index1 NIL))
                    ((NOT indexa)
                     (SETF in-gene NIL)
                     (LIST last-index-of-contig indexb))
                    ((NOT indexb)
                     (LIST indexa 0))
                    (T (LIST indexa indexb))))
              
             (IF (EQUAL in-gene "unknown")
                 (SETF in-gene (IN-GENE? coord indexa)))
             (LIST (biolisp::FIRST range) (biolisp::SECOND range) in-gene)))

         (Non-circular-wrap-error ()
           (ERR+ problem "Invalid coordinate range (~A to ~A) in "
                 indent  "noncircular contig ~A."
                 help~A
                 format-args from-coord to-coord contig "GENE-OF")
           )

         (Gene-list-with-possible-wrap (index1 index2)
           ;; Returns full list, with wrap around circular replicon
           ;;    or partial list that wraps
           ;; Handles special case where full list doesn't wrap (starts at 0)   
           (IF (= index1 index2)
               (SETF index2 (- index2 1)))
           (CONCATENATE 'LIST
                        (FROM-AUX index1 last-index-of-contig)
                        (IF (>= index2 0)
                            (FROM-AUX 0 index2))))  

         (List-within-same-unit (relation index)
           ;; Returns full list if:
           ;;    to < from  (if coords on same side of origin)
           ;;    from < to  (if coords on opposite sides of origin)
           ;; Otherwise returns one gene if either coord is within a gene
           (IF-TRUE 
            (FUNCALL relation from-coord to-coord) ; wrap?
            THEN (IF-TRUE (OR circular? (EQUAL relation '<))
                          THEN (GENE-LIST-WITH-POSSIBLE-WRAP index index)
                          ELSE (NON-CIRCULAR-WRAP-ERROR))
            ELSE (IF-TRUE (OR from-in-gene to-in-gene)
                          THEN
                          (LIST index)
                          ELSE 
                          (IF-TRUE (AND (not circular?) (EQUAL relation '<))
                                   THEN (NON-CIRCULAR-WRAP-ERROR)
                                   ELSE NIL))))
                 
         (Construct-list (first-index last-index &KEY same-unit)
           ;; Returns list of genes to be returned by main function
           (LET ((full-list 
                  (COND
                   ((< first-index last-index)   ; normal case: from < to
                    (FROM-AUX first-index last-index))
                   ((AND (= first-index last-index) same-unit)
                    (LET* ((gene (AREF sorted-fragments first-index))
                           (ref-coord 
                            (IF to-in-gene (SLOTV gene #$To )
                              (SLOTV gene #$From ))))
                      (IF-TRUE (EXCLUSIVE-OR (< from-coord ref-coord)
                                             (< to-coord ref-coord))
                               THEN (LIST-WITHIN-SAME-UNIT '< first-index)
                               ELSE (LIST-WITHIN-SAME-UNIT '> first-index))))
                   ((= first-index last-index) (LIST first-index))
                   (circular? ; spans end of circular contig
                    (GENE-LIST-WITH-POSSIBLE-WRAP first-index last-index))
                   (T ; spans end of noncircular contig
                    (NON-CIRCULAR-WRAP-ERROR))))) 
             (IF-TRUE Exclude-overlaps 
                      THEN (IF from-in-gene 
                               (SETF full-list (REMOVE first-index full-list)))
                      (IF to-in-gene 
                          (SETF full-list (REMOVE last-index full-list))))
             full-list)))

      ;; ************ MAIN PROGRAM ************

      (SETF from-coord 
            (1+ (HUMAN-BASE-TO-0-BASE (OR from-coord 1) contig-size contig
                                      :WRAP wrap :TRUNCATE truncate)))
      (SETF to-coord 
            (1+ (HUMAN-BASE-TO-0-BASE (OR to-coord contig-size) contig-size contig
                                      :WRAP wrap :TRUNCATE truncate)))

      (if *simple-genes-of-search* 
          
          (genes-in-contig-range contig from-coord to-coord exclude-overlaps)
        
        (progn

          ;; BINSEARCH doesn't use human coordinates. Function is used just to
          ;;    deal with wrapping, so add one to get back to human coordinates.

          (MULTIPLE-VALUE-SETQ (from-left from-right from-in-gene)
              (VALUES-LIST (MAKE-RANGE from-coord)))

          (MULTIPLE-VALUE-SETQ (to-left to-right to-in-gene)
              (VALUES-LIST (MAKE-RANGE to-coord)))
          (SETF index-list
                (COND
                 ((AND (NOT from-in-gene) (NOT to-in-gene))   ; both intergenic 
                  (IF-TRUE (= from-right to-right)    ; same intergenic
                           THEN (CONSTRUCT-LIST from-right to-right :SAME-UNIT T)
                           ELSE (CONSTRUCT-LIST from-right to-left)))
                 ((NOT from-in-gene)     ; from intergenic
                  (CONSTRUCT-LIST from-right to-left))
                 ((NOT to-in-gene)       ; to intergenic
                  (CONSTRUCT-LIST from-left to-left))
                 (T                      ; both in genes
                  (CONSTRUCT-LIST from-left to-left 
                                  :SAME-UNIT (= from-left to-left)))))
           
          (LOOP FOR index IN index-list
                AS gene = (AREF sorted-fragments index)
                COLLECT gene))))))

(defun genes-in-contig-range (contig from to exclude-overlaps?)
  (let ((genevec (#^genes-sorted-by-position contig))
        (genes nil)
        (contig-length (#^sequence-length contig)))
    (COND
       ((NOT genevec) NIL)
       ((not (#^circular contig))
        ;; Optimize case of non-circular contig by terminating search
        ;; once genes we're looking at are beyond the end of the interval
          ;; Sanity check.  Coordinates should have been transformed 
          ;; in higher level code.  
          (unless (and (<= from to) (>= from 1) (<= to contig-length))
            (error "Internal error!  From/To not within contig!  ~A ~D ~D"
                   contig from to))
          (lisp:loop
           for g across genevec 
           as gf = (#^from g)
           as gt = (#^to g)
           do
           (if exclude-overlaps? 
               ;; completely within range
               (when (and (>= gf from) (<= gt to))
                 (push g genes))
             ;; some part of gene within range
             (when (and (>= gt from) (not (> gf to)))
               (push g genes)
               ))
           (when (> gf to) (return nil)))
          (reverse genes)
          )
      (T
      (lisp:loop 
       for g across genevec 
       as gf = (#^from g)
       as gt = (#^to g)
       with interval-length = (#^sequence-length contig)
       do
       (multiple-value-bind (relationship order)
           (bio::positional-relationship-between-two-intervals
            t interval-length from to gf gt)
         ;; if the gene contains the interval then 
         ;; we simply define it as overlapping the interval
         ;; and hence since we are excluding overlaps we don't include
         ;; that gene.  (since we basically want to exclude any gene
         ;; that has any part of itself outside of the interval).  
         (if exclude-overlaps? 
             (if (or (eq relationship :identical) 
                     (and (eq relationship :within) 
                          (not (eq order :order-backward))))
                 (push g genes))
           (when (member relationship '(:identical :within :overlapped))
             (push g genes)
             )))
       finally (return (reverse genes))
       )))))

(DEFUN Maybe-position (give-position? result)
  (IF-TRUE give-position? 
           THEN (IF result (1+ result))
           ELSE (IF result T NIL)))


(DEFUN BB-X-than
       (x y numeric-comp string-comp-cs string-comp char-comp-cs char-comp 
          case-sensitive? position?)
  "Function serving BB-LESS-THAN and BB-GREATER-THAN"
  (COND
   ((AND (NUMBERP x) (NUMBERP y))
    (FUNCALL numeric-comp x y))
   ((AND (STRINGP x) (STRINGP y))
    (MAYBE-POSITION 
     position?
     (FUNCALL (IF case-sensitive? string-comp-cs string-comp) x y)))
   ((AND (CHARACTERP x) (CHARACTERP y))
    (FUNCALL (IF case-sensitive? char-comp-cs char-comp) x y))
   ((AND (STRING-OR-SYMBOL? x) (STRING-OR-SYMBOL? y))
                          ; Both are strings or symbols
    (MAYBE-POSITION position? (FUNCALL string-comp x y)))
   ((NULL x) NIL)
   ((NULL y) T)   
   ((AND (OR (AND (IS-NON-NIL-LIST? x) (IS-NON-NIL-LIST? y))
             (AND (VECTORP x) (VECTORP y))))
          ; Compare list/vector item by item
    (MAYBE-POSITION 
     position?
     (LOOP FOR x1 IN x
       FOR y1 IN y
       FOR i FROM 0
       WHEN (FORWARD-FUNCALL 
             'BB-X-THAN x1 y1 numeric-comp string-comp-cs string-comp
             char-comp-cs char-comp case-sensitive? NIL)
       RETURN i
       WHEN (NOT (BB-SAME x1 y1 :CASE-SENSITIVE case-sensitive?))
       RETURN NIL
       FINALLY 
       (RETURN 
        (IF (FUNCALL numeric-comp (LENGTH x) (LENGTH y))
            (1+ i)
          NIL)))))
   ((OR (IS-NON-NIL-LIST? X) (IS-NON-NIL-LIST? y))
    (ERR+ problem "Individual value '~A' cannot be compared "
          "to aggregate '~A' as a whole"
          advice  "Perhaps you should be using the EACH option"
          format-args
          (OR (AND (NOT (IS-NON-NIL-LIST? x)) x) y) 
          (OR (AND (NOT (IS-NON-NIL-LIST? y)) x) y))
    ) 
   (T (MAYBE-POSITION 
       position? (FUNCALL string-comp (BB-STRING-OF x) (BB-STRING-OF y))))))

(DEFUN BB-greater-than (x y &KEY case-sensitive? position?)
  (BB-X-THAN x y '> 'STRING> 'STRING-GREATERP 'CHARACTER> 'CHARACTER-GREATERP 
         case-sensitive? position?))

(DEFUN BB-LESS-than (x y &KEY case-sensitive? position?)
  (BB-X-THAN x y '< 'STRING< 'STRING-LESSP 'CHARACTER< 'CHARACTER-LESSP 
         case-sensitive? position?))

(DEFUN *Set-operation-of (function set-list)
  (LET ((single-argument NIL))
    (IF-TRUE (= (LENGTH set-list) 1)
             THEN (SETF set-list (FIRST set-list))
             (SETF single-argument T))

    (IF-TRUE (MEMBER NIL (MAPCAR 'LISTP set-list))
             THEN (LET ((function-name (S+ function "-OF")))
                    (IF single-argument
                        (ERROR "~A requires at least two sets"
                               function-name)
                      (ERROR "All arguments to ~A must be lists. '~A' is not a list"
                             function-name
                             (LOOP FOR arg IN set-list
                                   WHEN (NOT (LISTP arg))
                                   DO (RETURN arg))))))
    ; *** If a list is not a proper set because it contains duplicates,
	;     duplicates are removed
    (LET ((common-set (REMOVE-DUPLICATES (FIRST set-list))))
      (LOOP FOR set IN (REST set-list)
            DO (SETF common-set (FUNCALL function common-set 
			         (REMOVE-DUPLICATES set) :TEST 'BB-SAME)))
      ;; (FORMAT T "~&~A elements in ~A" (LENGTH common-set) function)
      common-set)))


(defun my-functions-aux ()
  (let ((functions nil)
        (package (find-package wb::*username*)))
    (do-symbols (x package)
      (when (and (fboundp x) 
                 (eq (symbol-package x) package)
                 (not 
                  (utils::initial-subsequence-of?
                   (symbol-name x) 
                   "DF-FUNCTION-FOR-"
                   :element-test 'char=
                   )))                   
        (push x functions)))
    functions
    ))


(defun my-variables-aux ()
  (let ((symbols nil)
        (package (find-package wb::*username*)))
    (do-symbols (x package)
      (when (and (boundp x)
                 (eq (symbol-package x) package)
                 (not (string-equal (symbol-name x) "*MY-CURRENT-DB*")))
        (push x symbols)))
    symbols
    ))


(DEFUN Make-sort-condition (criteria)
  (LET* ((fun-key (POP criteria))
         (fun (FIRST fun-key))
         (case-sensitive (SECOND fun-key))
         (key (THIRD fun-key))
         (put-in-cs-key 
          (AND case-sensitive
               (MEMBER fun `(BBI::BB-GREATER-THAN BBI::BB-LESS-THAN) :TEST 'SYMBOL=)))
         (put-in-cs-flag 
          (AND case-sensitive
               (MEMBER fun `(GREATER-THAN LESS-THAN) :TEST 'SYMBOL=))))
    `(COND
      ((,fun
        (FUNCALL ,key x)
        (FUNCALL ,key y)
        ,@(IF put-in-cs-key `(:CASE-SENSITIVE? T))
        ,@(IF put-in-cs-flag `(CASE-SENSITIVE)))
       T)
      ((BBI::BB-SAME
        (FUNCALL ,key x)
        (FUNCALL ,key y)
        ,@(IF case-sensitive `(:CASE-SENSITIVE T)))
       ,(WHEN criteria
          (FORWARD-FUNCALL 'BBI::MAKE-SORT-CONDITION criteria)))
      (T NIL))))


(DEFUN Shuffle-Aux (sequence &Key (In-place? nil))
  (IF (LISTP sequence)
      (LET* ((v (COERCE sequence 'VECTOR))
             (sv (FORWARD-FUNCALL 'SHUFFLE-AUX v :In-place? T)))
        (IF In-place?
            (LOOP FOR sublist ON sequence 
                  FOR elem ACROSS sv 
                  DO
                  (SETF (CAR sublist) elem)
                  FINALLY (RETURN sequence))
          (COERCE sv 'LIST)))
    (LET* ((sh (IF In-place? 
                   sequence
                 (COPY-SEQ sequence))))
      (cond 
       ((simple-string-p sh) 
        (shuffle-simple-string sh))
       (t
        (LOOP FOR j FIXNUM FROM (LENGTH sequence) DOWNTO 1 
              DO
              (LET ((r (RANDOM j)) 
                    (i (1- j)))
                (PSETF (AREF sh r) (AREF sh i) (AREF sh i) (AREF sh r)))
              FINALLY (RETURN sh)
              ))))))

(defun shuffle-simple-string (sh)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (simple-string sh))
  (loop for j fixnum from (length sh) downto 1 
        do
        (let ((r (random j)) 
              (i (the fixnum (1- j))))
          (declare (fixnum r i))
          (psetf (schar sh r) (schar sh i) (schar sh i) (schar sh r)))
        finally (return sh)))


(DEFUN String-number-conversion ()
  "Returns official method to convert number to string, without going through CONVERT"
  (LET* ((possible-conversions (GETHASH 'number *defconversion-hash-table*)))
     (CADR (find-if 
             (lambda (x) 
                 (equal (caar x) 'string))
              possible-conversions))))

(DEFUN Flat (obj)
   #.(one-string-nl
      "Remove all the nested structure in a list."
      "Same as FLATTEN except NILs retained") ; *** But not internal NILs!
   #.(optimization-declaration)
    (COND
     ((NULL obj) NIL)
     ((LISTP obj)
        (LOOP FOR list = obj THEN list
              AS elem = (FIRST list)
              AS rest = (REST list)
              UNTIL (NULL list)
              NCONC
                (IF (LISTP rest)
                    (PROGN
                       (POP list)
                       (COND
    ;                     ((NULL elem)
    ;                          (IF keep-nils (LIST elem) elem))
                          ((ATOM elem) (LIST elem))
                          (T (FLATTEN elem))  ; *** (FORWARD-FUNCALL 'FLAT elem) to retain internal NILs
                       ))
                    (PROGN
                       (SETQ list NIL)
                       (NCONC
                          (COND
                          ;  ((null elem) elem)
    ;                        ((NULL elem)
    ;                            (IF keep-nils (LIST elem) elem))
                             ((ATOM elem) (LIST elem))
                             (T (FLATTEN elem))
                          )
                          (LIST rest)
                       )))))
     (T (LIST obj))
     ))



(DEFUN Repeat-aux (string-or-item until-length number as-list as-string as-unit)
  (when (typep string-or-item 'bbi::labeled-sequence)
    (setf string-or-item (bbi::labeled-sequence-sequence string-or-item)))
  (without-code-walker
    (IF (AND until-length 
             (OR (NULL string-or-item)
                 (EQUAL string-or-item "")))
        (ERR+ PROBLEM "String or list of zero length given to be "
                      "repeated until a positive length. "
                      "Can't be done!"))
                
    (UNLESS (OR as-string (STRINGP string-or-item))
       (SETF as-list T))
    (COND
       ((AND as-list as-unit)
          (LOOP FOR i FROM 1 TO number
                COLLECT string-or-item))
       ((AND as-list until-length)
          (LET* ((unit (ENSURE-LIST string-or-item))
                 (unit-length (LENGTH unit)))
            (SUBSEQ 
               (LOOP WITH length = 0
                     UNTIL (>= length number)
                     DO (INCF length unit-length)
                     APPEND unit)
                0 number)))
       (as-list
          (LOOP FOR i FROM 1 TO number
                APPEND (ENSURE-LIST string-or-item)))
       (until-length
          (LET ((unit-length (LENGTH (BB-STRING-OF string-or-item))))
            (SUBSEQ
                (apply 'S+
                    (LOOP WITH length = 0
                          UNTIL (>= length number)
                          DO (INCF length unit-length)
                          COLLECT string-or-item))
                0 number)))
       (T  ; times
          (apply 'S+
              (LOOP FOR i FROM 1 TO number
                    COLLECT string-or-item))))
  ))

(DEFUN BB-Split (string &KEY at every between-words no-compress before after)
  (LET* ((substrings '())                ; Holds split up string
         (string-length (Length string))
         (break-point (OR at every))
         (segment-length 
          (IF (NUMBERP break-point) 
              (bbi::ROUND break-point)))
         (delimiter                      ; Make delimiter a character or NIL
          (COND ((NUMBERP break-point) NIL)
                ((CHARACTERP break-point) (STRING break-point))
                ((AND between-words (NOT break-point)) " ")
                ((= (LENGTH break-point) 0) NIL)
                (T break-point)))
         (delimiter-length
          (COND
           ((NOT delimiter) NIL)
           ((NOT (BB-IS-LIST? break-point)) 
            (LENGTH delimiter))
           ((AND (EVERY 'STRINGP delimiter)
                 (FORWARD-FUNCALL 'DF-FUNCTION-FOR-ALL-SAME 
                                  (MAPCAR 'LENGTH delimiter) T))
            (LENGTH (FIRST delimiter)))
           (T (ERROR (S+ "If AT or EVERY is followed by a list, "
                         "then every element of the list must be "
                         "a string of the same length"))))))
      
    (FLET ((Add-substring (start end)
             (LET* ((c1 (IF before (MAX 0 (- start 1)) start))
                    (c2 (IF after (MIN string-length (+ end 1)) end))
                    (substring (SUBSEQ string c1 c2)))
               (IF (OR no-compress  (> (LENGTH substring) 0))
                   (PUSH substring substrings))))
                
           (Next-delim (string char offset &KEY backwards?)
             (LET* ((offset1)(offset2))
               (IF backwards?
                   (ASSIGN (offset1 offset2) = (list 0 offset))
                 (ASSIGN (offset1 offset2) = (list offset (LENGTH string))))
               (IF (LISTP char)
                   (PROVIDED-VALUE
                    (MIN-OF (MAPCAR 
                             (LAMBDA (C) 
                               (OR (SEARCH c string :START2 offset1 
                                           :END2 offset2 :FROM-END backwards?)
                                   *UNPROVIDED+*))
                             char)))  
                 (SEARCH char string :START2 offset1 :END2 offset2 
                         :FROM-END backwards?)))))
                  
      (COND
       ((AND segment-length at between-words)
        (LET* ((delim-point 
                (NEXT-DELIM string " " segment-length :BACKWARDS? T))
               (point (IF delim-point
                          (MIN delim-point segment-length)
                        segment-length)))
          (LIST (BB-TRIM-BLANKS (SUBSEQ string 0 point) :RIGHT T)
                (BB-TRIM-BLANKS (SUBSEQ string point) :LEFT T))))
       ((AND segment-length at before)
        (LIST (SUBSEQ string 0 (1- segment-length))
              (SUBSEQ string (1- segment-length))))
       ((AND segment-length at)
        (LIST (SUBSEQ string 0 segment-length)
              (SUBSEQ string segment-length)))
       (segment-length
        (LET* ((exact? (NOT between-words))
               (segments (BB-BREAK-STRING string segment-length exact?))
               (first-segment (FIRST segments)))
          (IF (EQUAL (LAST segments) "")
              (ASSIGN segments (SUBSEQ segments 0 (1- (LENGTH segments)))))
          (IF every
              segments
            (LIST first-segment 
                  (string-join (rest segments) "")
                  ))))
       ((NOT delimiter)
        (LOOP FOR char ACROSS string
              COLLECT (STRING char)))
       (T (LOOP WITH start = 0
                WITH end = (NEXT-DELIM string delimiter 0)
                FOR segment FROM 1
                UNTIL (OR (NOT end) 
                          (AND at (> segment 1)))
                DO (ADD-SUBSTRING start end)
                (SETF start (+ end delimiter-length))
                (SETF end (NEXT-DELIM string delimiter start))
                FINALLY (ADD-SUBSTRING start string-length))
          (OR (NREVERSE substrings) '("")))))))

(defun is-list?-aux (each list? nil-ok)
  (COND 
   ((AND each (LISTP list?))
    (LOOP FOR item IN list?
          COLLECT (BB-IS-LIST? item :NIL-OK nil-ok)))
   (each (ERROR "EACH option requires that argument is a list"))
   (T (BB-IS-LIST? list? :NIL-OK nil-ok))
   ))

(defun trim-aux (string cutting left right pattern)
  (when (typep string 'bbi::labeled-sequence)
    (setf string (labeled-sequence-sequence string)))
  (without-code-walker
    (IF cutting
        (SETF pattern 
              (CL-PPCRE:REGEX-REPLACE-ALL "]" cutting "\\]")))
    (IF (OR left (NOT right))
        (SETF string (CL-PPCRE:REGEX-REPLACE-ALL 
                      (S+ "^[" pattern "]*") string "")))
    (IF (OR right (NOT left))
        (SETF string (CL-PPCRE:REGEX-REPLACE-ALL 
                      (S+ "[" pattern "]*$") string "")))
    string))


(defun new-table-aux (specs Initialize not-adjustable)
  (when (null specs)
    (error 
     "Can't create table with no dimensions! (e.g., (NEW-TABLE NIL))"))
  (LET* ((parsed-specs (PARSE-TABLE-SPECS specs))
         (spec-list (list parsed-specs ':ADJUSTABLE (NOT not-adjustable)
                          ':INITIAL-ELEMENT initialize)))
    (IF (NUMBERP initialize)
        (SETQ spec-list 
              (APPEND spec-list 
                      (list :IF-ACCESSED-LOCATION-DOES-NOT-EXIST initialize))))
    (APPLY 'MAKE-GARRAY spec-list)))
  
(DEFUN all-same-aux (thing case-sensitive first)
   (WHEN (TYPEP thing 'bbi::labeled-sequence)
         (SETF thing (bbi::labeled-sequence-sequence thing)))
   (FORWARD-FUNCALL 
     'DF-FUNCTION-FOR-ALL-TRUE 
       (MAPCAR (LAMBDA (c) 
          (BB-SAME first c :CASE-SENSITIVE case-sensitive)) 
          thing)
       NIL)
)

		
(defun common-orthlogs-of-aux (organisms primary not-in no-display)
  (let* ((revised-list
          (INTERSECTION 
           organisms 
           (intersection (common-ortholog-organisms) (bio::available-organisms))
           ))
         (primary-organism 
          (OR primary (FIRST revised-list) 
              (ERROR (S+ "BioBIKE does not know the orthologs of any "
                         "organism within the set ~A")
                     organisms)))
         (not-in-list (ENSURE-LIST not-in))
         (excluded-organisms
          (INTERSECTION not-in-list (COMMON-ORTHOLOG-ORGANISMS)))
         (common-ortholog-set (PROTEINS-OF-ORGANISM primary-organism)))
    ;; added length around lists because INTERSECTION can return 
    ;; list in any order, so two lists are not necessarily EQUAL,
    ;; but are still set-equal -- JP
    (IF (NOT (EQUAL (length organisms) (length revised-list)))
        (WARN (S+ "BioBIKE does not know the orthologs of some "
                  "organisms in the list. The following were "
                  "ignored: ~A")
              (SET-DIFFERENCE organisms revised-list)))
    (IF (NOT (EQUAL (length not-in-list) (length excluded-organisms)))
        (WARN (S+ "BioBIKE does not know the orthologs of some "
                  "organisms in the excluded organisms. The "
                  "following were ignored: ~A")
              (SET-DIFFERENCE not-in-list excluded-organisms)))
   
    (LET* ((excluded-organisms (ENSURE-LIST excluded-organisms)))     
     
      (LOOP 
       FOR organism IN (REMOVE primary-organism revised-list :TEST #'BB-SAME)
       DO (SETF common-ortholog-set
                (INTERSECTION 
                 common-ortholog-set
                 (GREF *ortholog-table* primary-organism organism))))
      (LOOP FOR organism IN excluded-organisms
            DO (SETF common-ortholog-set
                     (SET-DIFFERENCE
                      common-ortholog-set
                      (GREF *ortholog-table* primary-organism organism))))
      (UNLESS no-display
        (FORMAT T "~&~A common orthologs found" (LENGTH common-ortholog-set)))
      common-ortholog-set)))

(defun fit-aux
       (pre-string columns with if-too-big center centered
                   flush-right flush-left center?)
  (declare (ignore center centered))
  (when (typep pre-string 'bbi::labeled-sequence)
    (setf pre-string (labeled-sequence-sequence pre-string)))
  (COND
   ((NOT (OR center? flush-right))
    (SETF flush-left T))
   ((OR (AND center? flush-right) flush-left)
    (ERR+ problem "FLUSH-LEFT, FLUSH-RIGHT, and CENTERED are "
          indent  "mutually exclusive choices."
          advice  "Use only one of these flags at the time."
          help~A format-args *df-name*))) 
  (BB-FIT pre-string columns :WITH with :IF-TOO-BIG if-too-big
          :FLUSH-LEFT flush-left :CENTER? center? :FLUSH-RIGHT flush-right
          ))

(DEFUN from-aux (start end &optional (by 1) &KEY (limit 10000000))
  (SETQ by (ABS by))
  (IF (= by 0)
      (ERR+ PROBLEM "The BY value cannot be 0"))
  (LET ((n (/ (- end start -1) by)))
    (IF (> n limit)
        (ERR+ 
         PROBLEM
         (S+ 
          "The list you are trying to create would have more values (~A) than "
          "the maximum allowed (~A)")
         ADVICE 
         (S+ "You can change the maximum allowed using the LIMIT keyword. "
             "However, it is likely that there is a more efficient way of "
             "doing what you are trying to do.")
         FORMAT-ARGS n limit)))
  (COND
   ((AND (= by 1) (<= start end))
    (LOOP FOR j FROM start TO end COLLECT j))
   ((<= start end)
    (LOOP FOR j FROM start TO end BY by COLLECT j))
   ((= by 1)
    (LOOP FOR j FROM start DOWNTO end COLLECT j))
   (T
    (LOOP FOR j FROM start DOWNTO end BY by COLLECT j))
   ))

(defun from-pre-aux (start end by limit)
  (COND
   ((AND (NUMBERP start) (NUMBERP end))
    (FROM-AUX start end by :LIMIT limit))
   ((OR (NUMBERP start) (NUMBERP end))
    (ERR+ PROBLEM 
          "If either the start or the end is a number then both must be."
          INDENT  "'~A' is not a number"
          ADVICE  "Make both numbers, or make both strings."
          FORMAT-ARGS (IF (NUMBERP start) end start) ))
   (T (UNLESS (CHARACTERP start)
        (IF (NOT (= (LENGTH start) 1))
            (ERR+ 
             PROBLEM 
             "A string used as the start must be one character long, not '~A'"
             FORMAT-ARGS start)
          (SETF start (CHAR start 0))))
      (UNLESS (CHARACTERP end)
        (IF (NOT (= (LENGTH end) 1))
            (ERR+
             PROBLEM
             "A string used as the end must be one character long, not '~A'"
             FORMAT-ARGS end)
          (SETF end (CHAR end 0))))
      (LET ((codes (FROM-aux (CHAR-CODE start) 
                             (CHAR-CODE end) by :LIMIT limit)))
        (MAPCAR (LAMBDA (x) (STRING (CODE-CHAR x))) codes)))))


(defun genes-of-aux (entity from to length wrap truncate exclude-overlaps)
  (let* ((from-coord (PROVIDED from))
         (to-coord 
          (COND      
           ((ERROR-IF-MORE-THAN-ONE to length) (PRINT "ERROR")) 
           ((PROVIDED to) to)
           ((PROVIDED length) 
            (IF-TRUE 
             from-coord
             THEN (+ from-coord length -1)
             ELSE
             (ERR+
              problem "Invalid use of the keyword 'LENGTH'."
              advice 
              "The keyword 'LENGTH' can only be used if a coordinate"
              indent  "is provided to the keyword 'FROM', for example:"
              indent  "(GENE-OF pnpe FROM 12345 LENGTH 3000)"
              help~A format-args *df-name*
              )))
           (T NIL))))
    (TYPECASE entity
      (Gene entity)
      (Contiguous-sequence
       (IF-TRUE
        (OR from-coord to-coord)
        THEN 
        (GENES-OF-ONE-SEGMENT
         entity from-coord to-coord wrap truncate exclude-overlaps)
        ELSE (GENES-OF-CONTIG entity)))
      (Organism
       (IF-TRUE from-coord
                THEN (SETF entity (LONE-REPLICON-OR-ERROR entity))
                (GENES-OF-ONE-SEGMENT 
                 entity from-coord to-coord wrap truncate exclude-overlaps)
                ELSE (GENES-OF-ORGANISM entity)))
      ;;(gene-part (SLOTV entity #$gene))  <<< gene-part not available yet >>>
      )))

(defun mw-of-aux 
       (entity nowarnings sequence amino-acid No-sequence->error 
               No-sequence->0 No-sequence->ignore labeled label)
  (ERROR-IF-MORE-THAN-ONE No-sequence->error
                          No-sequence->0 No-sequence->ignore)
  (IF-TRUE 
   (AND (AMINO-ACID-DESIGNATOR? entity) (NOT sequence))
   THEN 
   (IF-TRUE 
    amino-acid
    THEN (aa-to-mw entity)
    ELSE  
    (WARN
     (S+
      "~&Given string, corresponded to Amino-acid designation of: ~A."
      "~&To obtain the molecular weight of the amino-acid sequence ~S,"
      "~&use the flag SEQUENCE."
      "~&If you do not want this warning use the flag AMINO-ACID."
      )
     (AA-TO-LONG-NAME entity) entity)
    (aa-to-mw entity))      
   ELSE                      
   (LET* ((raw-sequence
           (TYPECASE entity            
             (Protein 
              (SETF label entity)
              (EXTRACT-SEQUENCE entity))
             (Gene
              (SETF label entity)
              (IF-TRUE 
               (SLOTV entity #$ENCODES-PROTEIN)
               THEN 
               (EXTRACT-SEQUENCE (FIRST (SLOTV entity #$PROTEINS)))
               ELSE 
               (IF-TRUE 
                nowarnings
                THEN  NIL
                ELSE 
                (WARN 
                 (S+
                  "~&You are trying to obtain the molecular weight of"
                  "~&the noncoding gene ~S!"
                  "~&The function returns NIL."
                  "~&If you do not want this warning use the flag NOWARNINGS."
                  )
                 entity
                 )
                NIL )))
             (String 
              (SETF label (FORWARD-FUNCALL 'DF-FUNCTION-FOR-NAME-OF entity T))
              entity)
             (Labeled-sequence
              (SETF label (FORWARD-FUNCALL 'DF-FUNCTION-FOR-NAME-OF entity T))
              (LABELED-SEQUENCE-SEQUENCE entity)))) 
          (sequence 
           (IF-TRUE (OR (EQUAL raw-sequence NIL) (= (LENGTH raw-sequence) 0))
                    THEN NIL
                    ELSE
                    (IF-TRUE 
                     (AMINO-ACID-DESIGNATOR? 
                      (SUBSEQ raw-sequence 
                              (- (LENGTH raw-sequence) 1) 
                              (LENGTH raw-sequence)))    
                     THEN raw-sequence
                     ELSE 
                     (SUBSEQ raw-sequence 0 (- (LENGTH raw-sequence) 1)))))) 
     (IF (AND No-sequence->error (= (LENGTH raw-sequence) 0))
         (ERROR "Zero-length sequence encountered in execution of MW-OF"))
                  
     (IF-TRUE (EQUAL sequence NIL)
              THEN (IF no-sequence->0 0 NIL)
              ELSE (IF labeled
                       (LIST label (MOLECULAR-WEIGHT-OF sequence))
                     (MOLECULAR-WEIGHT-OF sequence))))))

(defun name-of-aux (entity short)
  (TYPECASE entity
    ((OR Gene Protein Contiguous-sequence)
     (LET ((name (BB-STRING-OF entity)))
       (IF-TRUE short
                THEN (LET* ((period (POSITION #\. name)))
                       (SUBSEQ name (1+ period)))
                ELSE name)))
    (Organism
     (IF-TRUE short
              ;; removed [] notation to allow without-code-walker
              THEN (LET ((prefix (slotv entity #$organism-prefix)))
                     ;; prefix[1 -> (1- (LENGTH prefix))]
                     (subseq prefix 0 (1- (length prefix)))
                     )
              ELSE (BB-STRING-OF entity)))
    (Labeled-sequence
     (LABELED-SEQUENCE-LABEL entity))
    (string
     (LET ((string
            (IF-TRUE short
                     THEN (LET* ((period (POSITION #\. entity)))
                            (IF period
                                (SUBSEQ entity (1+ period))
                              entity))
                     ELSE entity)))

      ;    (IF ; (AND (NOT short) (> (LENGTH string) 10))
       (IF (> (LENGTH string) 10)
           ;; (S+ string[1 -> 10] "...")
           (s+ (subseq string 0 10) "...")
         string)))
    (OTHERWISE
     (ERROR "INTERNAL ERROR in NAME-OF (type): Please report"))))

(defun nucleotide-distance-aux (start end contig truncate wrap)
  (let ((length 0)
        (circular nil))
    (IF (TYPEP contig 'Organism)
        (LET* ((contigs (SLOTV contig #$Contiguous-sequences))
               (contig-number (LENGTH contigs)))
          (IF-TRUE (= contig-number 1)
                   THEN (SETF contig (biolisp::FIRST contigs))
                   ELSE (ERR+
                         problem "Organism ~A has more than" 
                         indent  "one contiguous-sequence:"
                         indent  "~A."
                         advice  "Specify which one you mean."
                         help~A
                         format-args
                         contig
                         (IF-TRUE (< contig-number 10)
                                  THEN contigs
                                  ELSE
                                  ;; (FORMAT NIL "~A ..." contigs[1 -> 9] )
                                  (format nil "~A ..." (subseq contigs 0 9))
                                  )
                         *df-name*)
                   )))

    (IF (AND wrap truncate)
        (ERR+ problem "WRAP and TRUNCATE are mutually exclusive!"
              advice  "Specify only one at the time."
              help~A
              format-args *df-name*)
      )   
    (SETF length (SLOTV contig #$Sequence-length))
    (SETF circular (SLOTV contig #$Circular ))
    (IF (AND wrap (NOT circular))
        (ERR+ problem "Can't wrap around the noncircular contig ~A."
              advice  "You may try the flag TRUNCATE."
              help~A
              format-args contig *df-name*)
      )
    (SETF
     start
     (Human-base-to-0-base start length contig :WRAP wrap :TRUNCATE truncate))
    (SETF
     end
     (Human-base-to-0-base end length contig :WRAP wrap :TRUNCATE truncate))

    (COND 
     ((<= start end) (- end start -1))
     (circular (+ (- end start -1) length))
     (T (ERR+ problem "Given end '~A' precedes given start '~A' of"
              indent  "noncircular contig ~A."
              help~A
              format-args end start contig *df-name*)
        ))))

;;; Find an organism frame given a name.  The name can be a string, a
;;; symbol, or a frame.  

;;; If NO-ALIASES is false, the organism's nicknames and alternative names 
;;; are searched if and only if no complete match is found.  
;;; If IN-PART is true, the name can be a substring of the organism's name.
;;; Partial matches are only searched for if no complete match is found and 
;;; no aliases are found.  

;;; NIL is returned if no match is found.  If more than one match is found,
;;; the list of matching organism frames is returned, otherwise a single
;;; organism frame is returned.  


(DEFUN Organism-named-aux (given-name in-part exact-only 
                  phage-only bacteria-only no-aliases display-off)
  (DECLARE (SPECIAL cl-user::*blast-lookup-database*))
  (SETQ in-part (NOT exact-only))
  (LET* ((using-SEED (EQUAL cl-user::*blast-lookup-database* :SEED))
         (name (SUBSTITUTE #\- #\SPACE given-name))
         (domain 
           (COND
		   ;; *all-phage* is only defined in seed instances and would
           ;; cause a compiler warning if used directly.  
           ;; furthermore, it is a symbol-macro, so we can't just use 
           ;; (symbol-value '*all-phage*).  same for *all-bacteria*
              ((AND using-SEED phage-only) (eval '*all-phage*))
              ((AND using-SEED bacteria-only) (eval '*all-bacteria*))
              (T *all-organisms*))))
  (BLOCK exit
    (COND
      ((ISFRAME? name) (SETQ name (#^Fname name)))
      ((SYMBOLP name) (SETQ name (SYMBOL-NAME name)))
      ((STRINGP name) nil)
      (T (ERROR "Don't know how to convert ~A to an organism!" name)))
	
	; **** First pass: Exact match to organism name  
    (LOOP FOR organism IN domain 
          AS orgname = (#^Fname organism)
          DO (WHEN (STRING-EQUAL orgname name) 
		           (RETURN-FROM EXIT organism)))
				   
    (LET ((results nil))
    ; **** Second pass: Exact match to alias  
      (UNLESS no-aliases 
        (LOOP FOR organism IN domain 
              AS nicknames = (#^Nicknames organism)
              AS alternative-names = (#^Alternative-names organism)
              AS organism-symbols = (#^Organism-symbols organism)
              AS all-aliases = 
                  (APPEND nicknames alternative-names organism-symbols)
              DO (WHEN (FIND name all-aliases :TEST 'STRING-EQUAL)
                       (PUSH organism results)
                ))
        (WHEN results 
             (RETURN-FROM EXIT (IF (NULL (CDR results)) (FIRST results) results))
          ))

    ; **** Third pass: Partial match to organism name  
      (WHEN in-part 
        (LOOP FOR organism IN domain 
              AS orgname = (#^Fname organism)
              DO
                (WHEN (SEARCH name orgname :TEST 'STRING-EQUAL)
                      (PUSH organism results))
                ))
				
       (WHEN results 
          (RETURN-FROM EXIT 
              (IF (NULL (CDR results)) 
		  (FIRST results)
                  (LET ((sorted-results (SORT results 'BB-LESS-THAN)))
                    (IF (AND (BBL-TOPLEVEL?) (NOT display-off))
                       (LET ((name-list
                               (LOOP FOR org IN sorted-results
                                     AS prefix = (#^Organism-prefix org)
                                     AS nickname = (SUBSEQ prefix 0 (1- (LENGTH prefix)))
                                     COLLECT (LIST nickname org))))
                               (FORWARD-FUNCALL 'DISPLAY-LIST-aux 'EACH name-list
                                  NIL 3 NIL NIL T NIL)))
                    sorted-results))
       		
          ))
      NIL
      ))))

      
(defun repeat-pre-aux
       (each string-or-item times until-length number as-list as-string as-unit)
  (declare (ignore times))
   (ERROR-IF-MORE-THAN-ONE (LIST as-list as-string))
   (COND
      ((AND EACH (LISTP string-or-item))
         (LOOP FOR item IN string-or-item
               COLLECT 
               (REPEAT-aux item until-length number as-list as-string as-unit)))
      (EACH
         (ERR+ PROBLEM "The EACH option must be followed by a list"))
      (T 
       (REPEAT-aux 
        string-or-item until-length number as-list as-string as-unit))))


(defun sort-vpl-aux
       (list by-position then-sort-ascending-by then-sort-descending-by 
             ascending descending case-sensitive)
  (let* ((1st-position (PROVIDED by-position))
         (1st-test
          (IF descending 'BB-GREATER-THAN 'BB-LESS-THAN))
         (2nd-position
          (OR (PROVIDED then-sort-ascending-by)
              (PROVIDED then-sort-descending-by)))
         (2nd-test
          (IF 
              (PROVIDED then-sort-ascending-by)
              'BB-LESS-THAN
            'BB-GREATER-THAN))
         (extents (EXTENTS-OF-COMPLEX-LIST list))
         )
       
    ;; Check for suitability of POSITIONS
    ;; (DECLARE (IGNORE ascending))  ; Doesn't work
    (AND ascending "This is done just to squelch a warning")
    (WHEN 1st-position
       (LET* ((max-pos (MAX 1st-position (OR 2nd-position (- *big-number*)))))
         (IF (> max-pos (SECOND extents))
             (WARN "Position ~A used in SORT, but the list goes up to only position ~A"
                  max-pos (SECOND extents)))
         (IF (= (FIRST extents) 0)
             (ERR+ PROBLEM "BY-POSITION used in SORT, but some elements of list"
                   INDENT "are not lists and therefore have no positions"
                   HELP~A 
                   FORMAT-ARGS "SORT"
                   ))))
    (COND
     ((AND (NOT 2nd-position) 1st-position)
      (SORT (COPY-LIST list)
            (LAMBDA (x y) (FUNCALL 1st-test x y
                                   :CASE-SENSITIVE? case-sensitive))
            :KEY (LAMBDA (i) (Nth (1- 1st-position) i))))
     ((NOT 2nd-position)
      (SORT (COPY-LIST list)
            (LAMBDA (x y) (FUNCALL 1st-test x y
                                   :CASE-SENSITIVE? case-sensitive))))
     (T (IF (NOT 1st-position)
            (ERROR "Can't specify a second sort position without specifying a first"))
        (SORT 
         (COPY-LIST list)
         (LAMBDA (x y)
           (COND
            ((FUNCALL 1st-test (Nth (1- 1st-position) x) (Nth (1- 1st-position) y)
                      :CASE-SENSITIVE? case-sensitive)
             T)
            ((BB-SAME (Nth (1- 1st-position) x) (Nth (1- 1st-position) y)
                      :CASE-SENSITIVE case-sensitive)
               (COND
                ((FUNCALL 2nd-test (Nth (1- 2nd-position) x)
                          (Nth (1- 2nd-position) y)
                          :CASE-SENSITIVE? case-sensitive)
                 T)
                (T NIL)))
            (T NIL))))))))



(DEFUN Variable-in-package (package var-string)
   (LET* ((var (FIND-SYMBOL var-string package))
          (var-package (SYMBOL-PACKAGE var)))
     (AND var (EQ (FIND-PACKAGE package) var-package) var)
   ))

(DEFUN everyones-aux (var function labeled)
   (FUNCALL function
     (LOOP FOR package IN wb::*logins*
           WITH string = (STRING var)
           AS package-var = (VARIABLE-IN-PACKAGE package string)
           WHEN (AND package-var (BOUNDP package-var))
             COLLECT 
               (IF labeled 
                   (LIST package (SYMBOL-VALUE package-var))
                   (SYMBOL-VALUE package-var))))
  )
  
(DEFUN Replace-string-with-frame (string frame &KEY separate)
   (LET* ((boundary (CODE-CHAR 129))
          (buffer (CODE-CHAR 130))
          (frame-string (BB-STRING-OF frame))
          (parts (BB-SPLIT (FORWARD-FUNCALL 'BB-JOIN 
                              (LIST boundary string boundary))
                                :EVERY frame-string))
          )
    (IF (= (LENGTH parts) 1)
        (LIST string)
        (LET ((result
                (FLATTEN (FORWARD-FUNCALL 'BB-JOIN 
                   parts :BY (LIST buffer frame buffer):AS-LIST T))))
           (UNLESS separate
              (SETF result (REMOVE buffer result)))
           (MAPCAR (LAMBDA (i) (IF (STRINGP i)(REMOVE boundary i) i)) result)))
))

; ============ Permutation and Combination functions =============
(DEFUN Permute-list (list length with-repeats &KEY stack-count)
  (IF stack-count
      (INCF stack-count)
      (SETF stack-count 1)
      )
  (UNLESS (> stack-count length) 
    (LOOP FOR item IN list
          FOR i FROM 0
          AS rest-of-list 
            = (IF with-repeats
                   list
                   (REMOVE-nTH-ELEMENT i list))
          AS subpermutations 
            = (FORWARD-FUNCALL 'permute-list rest-of-list length with-repeats 
                    :STACK-COUNT stack-count)
           APPEND (MAPCAR (LAMBDA(x) (CONS item x)) 
                     (OR subpermutations (LIST NIL)))))
)
      
(DEFUN Permute-string (string length with-repeats &KEY stack-count)
  (IF stack-count
      (INCF stack-count)
      (SETF stack-count 1)
      )
  (UNLESS (> stack-count length)
      (LOOP FOR char ACROSS string
            FOR i FROM 0
            AS rest-of-string 
              = (IF with-repeats
                     string
                     (REMOVE-nTH-CHARACTER i string))
            AS subpermutations 
              = (FORWARD-FUNCALL 'permute-string rest-of-string length with-repeats 
                      :STACK-COUNT stack-count)
             APPEND (MAPCAR (LAMBDA(x) (S+ char x)) 
                       (OR subpermutations (LIST "")))))
)

(DEFUN Combine-list (list length &KEY stack-count)
  (IF stack-count
      (INCF stack-count)
      (SETF stack-count 1)
      )
  (UNLESS (> stack-count length) 
    (LOOP FOR item IN list
          FOR i FROM 1 TO (- (LENGTH list) length (- stack-count))
          AS rest-of-list = (SUBSEQ list i)
          AS subpermutations 
            = (FORWARD-FUNCALL 'combine-list rest-of-list length  
                    :STACK-COUNT stack-count)
           APPEND (MAPCAR (LAMBDA(x) (CONS item x)) 
                     (OR subpermutations (LIST NIL)))))
)

(DEFUN Combine-string (string length &KEY stack-count)
  (IF stack-count
      (INCF stack-count)
      (SETF stack-count 1)
      )
  (UNLESS (> stack-count length)
      (LOOP FOR char ACROSS string
            FOR i FROM 1 TO (- (LENGTH string) length (- stack-count))
            AS rest-of-string = (SUBSEQ string i)
            AS subpermutations 
              = (FORWARD-FUNCALL 'combine-string rest-of-string length 
                      :STACK-COUNT stack-count)
             APPEND (MAPCAR (LAMBDA(x) (S+ char x)) 
                       (OR subpermutations (LIST "")))))
)

(DEFUN Permutation-count-of-aux (n k with-repeats)
   "Calculates permutations of k items from n total items"
   ;  N!/(N-K)! in a semi-efficient manner"
   #.(optimization-declaration)
   (unless (and (integerp n) (integerp k) (<= 0 n) (<= 0 k) (<= k n))
     (error "Invalid arguments: ~S, ~S" n k))
   (locally
     (declare (fixnum n k))
     (cond
      (with-repeats (EXPT n k))
      ((= k 0) 1)
      ((= k 1) n)
      ((= k 2) (* n (the fixnum (1- n))))
      (t
       (let ((result 1))
         (loop for j fixnum from n downto (the fixnum (- n k -1))
               do
               (setq result (* result j))
               finally (return result)
               ))))
))