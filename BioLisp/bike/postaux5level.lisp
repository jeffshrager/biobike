;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi) 

;;; +=========================================================================+
;;; | Copyright (c) 2005 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

(DEFUN SEED-x-blast-aux (protein)
  (LET* ((raw-crossblast 
          (forward-package-funcall :bio :CROSSBLAST-MATCHES protein)))
    (IF (SECOND raw-crossblast)
        (FOR-EACH (pre-ids results) IN (REST raw-crossblast)
                  APPEND
                  (FOR-EACH 
                   pre-id IN pre-ids
                   ASSIGN hit-type = (FIRST pre-id)
                   ASSIGN hit-gene-frame = (OR (FOURTH pre-id) (LIST NIL))
                   ASSIGN hit-organism = (FIFTH pre-id)
                   WHEN (OR (EQUAL hit-type :MASTER-LIST) 
                            (EQUAL hit-type :LOADED))
                   COLLECT
                   (JOIN hit-gene-frame hit-organism (bio::SUBSEQ results 2)))))
    ))

(DEFUN Filter-blast-result (blast-list targets)
  (LET* ((org-list
            (REMOVE-DUPLICATES
               (FOR-EACH target IN (ENSURE-LIST targets)
                    AS org =
                       (IF (IS-ORGANISM? target) 
                           target
                           (ORGANISM-OF target))
                    COLLECT org)))
         )
    (IF targets
        (FOR-EACH hit IN blast-list
             AS target-org = (SECOND hit)
             WHEN (MEMBER target-org org-list) 
                COLLECT hit)
        blast-list)
  ))

(DEFUN SEED-X-BLAST (protein target return display-off return-targets)
  (LET* ((gene (IF (IS-PROTEIN? protein)
                   (REF protein #$gene)
                   protein))
         (blast-table (NEW-TABLE '(1 $)))
         (last-line (OR return *big-number*))
         (crossblast-list (SEED-X-BLAST-aux gene))
        )
     (UNLESS (NOT crossblast-list)
         (SETF crossblast-list (FILTER-BLAST-RESULT crossblast-list target))
         (SETF crossblast-list 
          (FOR-EACH hit IN crossblast-list
		       WHEN (FIRST hit)
                 COLLECT
                    (JOIN (FIRST hit) (SECOND hit)
                          (APPLY-FUNCTION 
                           (CONVERT item TO Number IF-POSSIBLE)
                           REPLACING item WITH (SUBLIST hit FROM 3)) AS-LIST)))
         (SETF crossblast-list
            (BBL::SORT crossblast-list BY-POSITION 11))
         (FOR-EACH hit IN crossblast-list
          FOR-EACH line FROM 1 TO last-line
              INITIALIZE headers 
                = (STRING-OF '(target t-organism %ID align-length mismatches gaps 
                         q-start q-end t-start t-end e-value bit-score 
                         query-length target-length))
			  (ASSIGN (REF blast-table line "QUERY") 
			         = (PROTEIN-OF protein))
			  (FOR-EACH header IN headers
               FOR-EACH value IN hit
                   (IF (EQUAL header "TARGET")      
				       (ASSIGN (REF blast-table line "TARGET")
                            = (PROTEIN-OF value))
                       (ASSIGN (REF blast-table line header) = value)))))
	 (IF-TRUE crossblast-list
	     THEN 
		   (UNLESS display-off
               (DISPLAY-BLAST-aux blast-table
                  '("QUERY" "Q-START" "Q-END" "Q-FRAME" "TARGET" "T-START"
                      "T-END" "E-VALUE" "%ID" "T-ORGANISM" "T-DESCRIPTION")
                  NIL NIL))
           (IF return-targets
               (FOR-EACH line IN (LABELS-OF blast-table DIMENSION 1)
                  COLLECT (REF blast-table line "TARGET"))
             blast-table)
		 ELSE
		   (UNLESS display-off
		      (DISPLAY "NO MATCHES FOUND")))
))

(DEFUN SEED-ortholog-aux (gene/protein target &KEY CROSSBLAST-LIST)
   (LET* ((gene (IF (IS-PROTEIN? gene/protein)
                   (REF gene/protein #$gene)
                   gene/protein))
          (primary-org (ORGANISM-OF gene))
          (crossblast-list (OR crossblast-list (SEED-X-BLAST-aux gene)))
          (candidate-info
              (FIRST (FILTER-BLAST-RESULT crossblast-list target)))
          (candidate (FIRST candidate-info))
          (crossblast-list2 (IF candidate (SEED-X-BLAST-aux candidate)))
          (back-candidate-info (IF candidate
              (FIRST (FILTER-BLAST-RESULT crossblast-list2 primary-org))))
          (back-candidate (FIRST back-candidate-info))
          )
    (IF (EQUAL gene back-candidate) candidate)
))

#|
(DEFUN SEED-ortholog (gene/protein target/s)
   ;; *** Uses SEED lookup table ***
   (LET* ((gene (IF (IS-PROTEIN? gene/protein)
                   (REF gene/protein #$gene)
                   gene/protein))
          (primary-org (ORGANISM-OF gene))
          (primary-org-is-target 
              (COND
                 ((EQUAL primary-org target/s)
                    (RETURN-FROM SEED-ORTHOLOG gene/protein))
                 ((LISTP target/s)
                    (MEMBER primary-org target/s))
                 (T NIL)))
          (targets
             (IF primary-org-is-target
                 (REMOVE primary-org target/s)
                 (ENSURE-LIST target/s)))
          (crossblast-list (SEED-X-BLAST-aux gene))
          (result
              (FOR-EACH target IN targets
                   COLLECT (SEED-ORTHOLOG-aux gene target 
                         :CROSSBLAST-LIST crossblast-list)))
          )
      (IF primary-org-is-target
          (SETF result (APPEND (LIST gene) result)))  
      (IF (LISTP target/s)
          result
          (FIRST result))
 ))
|#

(DEFUN SEED-ortholog (gene/protein target/s)
  ;; *** Uses Blast instead of lookup table ***
   (LET* ((protein (IF (IS-PROTEIN? gene/protein)
                   gene/protein
                   (FIRST (REF gene/protein #$proteins))))
          (primary-org (ORGANISM-OF protein))
          (primary-org-is-target 
              (COND
                 ((EQUAL primary-org target/s)
                    (RETURN-FROM SEED-ORTHOLOG gene/protein))
                 ((LISTP target/s)
                    (MEMBER primary-org target/s))
                 (T NIL)))
          (targets
             (IF primary-org-is-target
                 (REMOVE primary-org target/s)
                 (ENSURE-LIST target/s)))
          (crossblast-list 
              (BBL::FIRST IN-EACH
                (APPLY-FUNCTION 
                   (FORWARD-FUNCALL 'SEQUENCES-SIMILAR-TO-aux
                          protein target "blastp" *unprovided+*
                          *unprovided+* *unprovided+* *unprovided+*
                          NIL :BYPASS-LOOKUP T :NO-DISPLAY T :RETURN-TARGETS T)
                   REPLACING target WITH targets)))
          (result
              (FOR-EACH hit IN crossblast-list
                   AS back-hit =
                      (FIRST (FORWARD-FUNCALL 'SEQUENCES-SIMILAR-TO-aux
      				   protein primary-org "blastp" *unprovided+*
				   *unprovided+* *unprovided+* *unprovided+*
				   NIL :BYPASS-LOOKUP T :NO-DISPLAY T :RETURN-TARGETS T))
                   WHEN (EQUAL back-hit protein)
                     COLLECT hit))
         )
      (IF primary-org-is-target
          (SETF result (APPEND (LIST protein) result)))  
      (IF (LISTP target/s)
          result
          (FIRST result))
 ))
