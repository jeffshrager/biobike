;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package bbi)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 by the BioBike teams                            |
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

(DEFPARAMETER *microarray-directory*
  (FORMAT NIL "~a~a" cl-user::*source-root* "data/microarray/"))

(DEFVAR *microarray-symbols* (make-hash-table :test 'equal))

(LET ((symbol-pairs
        '((ratio ratio) (ratio-log2 ratio2) 
          (target-values TSIG)(target-values TVALUES) (target-background TBG)
          (control-values CSIG)(control-values CVALUES) (control-background CBG)
          (target-mean TMEAN) (target-stdev TSD) 
          (control-mean CMEAN) (control-stdev CSD)
          (t-test-score TTEST))))
  (LOOP FOR (symbol1 symbol2) IN symbol-pairs
        AS string1 = (STRING symbol1)
        AS string2 = (STRING symbol2)
        DO (SETF (GETHASH string1 *microarray-symbols*) string2)
           (SETF (GETHASH string2 *microarray-symbols*) string1)))

(DEFUN Write-microarray-table (table path)
  (LET ((first-line
           (LIST 
             (LIST "  LABELS" 
                 (GARRAY-COMPONENT-INDICES table
                       (FIRST (GARRAY-COMPONENT-INDICES table)) 1)
                 ))))
  (BBL::WRITE
        (JOIN first-line
          (BBL::SORT 
             (INTERLEAVE (NAMES-OF (GARRAY-COMPONENT-INDICES table) SHORT)
                         (GMAP 'IDENTITY table)))
          AS-LIST)
          TO path TEXT)))


(DEFUN microarray-file-path-of (expt type &KEY create)
  (LET* ((organism-name (BB-STRING-OF (SLOTV expt #$organism))) 
         (expt-name (FNAME expt))
         (path (S+ *microarray-directory* organism-name
                   "/" expt-name "/" expt-name "-" type ".txt"))
        )
    (IF (OR (DIRECTORY path) create)
        path)))


(DEFUN Raw-microarray-table-from-replicates
  (expt &KEY (column-order '(1 2 3 4 5 6 7)) (skip 0))
  (IF (NOT (IsFrame? expt))
      (ERROR "Argument to RAW-MICROARRAY-TABLE-FROM-REPLICATES must be a frame, not ~A" expt))
  (LET* ((conditions (SLOTV expt #$Conditions))
         (table (NEW-TABLE '($ $ (xpos ypos csig cbg tsig tbg))))
         (key-labels '("CSIG" "CBG" "TSIG" "TBG"))
         (all-labels (APPEND '("XPOS" "YPOS") key-labels))
         (organism (SLOTV expt #$organism))
         (bad-genes (NEW-TABLE '($)))
         )
       (display-data organism)
    (DISPLAY-LINE "Processing " expt)
    (LOOP 
       FOR (condition condition-descr) IN conditions
       FOR cond-number FROM 1
       AS replicates = (SLOTV condition #$Replicates)
       AS last-replicate = (FIRST (LAST replicates))
       AS dye-flips = (REF condition #$dye-flip)
       DO (DISPLAY-LINE *tab* condition-descr)
          (LOOP
            FOR replicate IN replicates
         ;  FOR flip? IN dye-flips
            AS flip? = (POP dye-flips)
            AS lines = (BBL::READ replicate TABBED CONVERT-NUMBERS)
            DO (LOOP 
                  FOR line in lines
                  FOR line# FROM 1
                  AS first-field = (FIRST line)
                  AS comment? = (AND (STRINGP first-field)
                                  (EQUAL (SUBSEQ first-field 0 1) "#"))
                  AS data = (AND (NOT comment?) (> line# skip)
                                 (MAPCAR (LAMBDA (n) (NTH (- n 1) line)) column-order))
                  AS gene = (GENE-NAMED (FIRST data) IN organism)
                  AS csig = (REF table gene cond-number "csig")
                  DO (COND
                        ((OR comment? (<= line# skip) ))
                        ((NOT gene) (SETF (REF bad-genes (FIRST data)) T))
                        (T (IF flip?
                               (SETF data (REF data '(1 2 3 6 7 4 5))))
                           (IF-TRUE csig
                               THEN (LOOP 
                                       FOR type IN key-labels
                                       FOR function IN '(FOURTH FIFTH SIXTH SEVENTH)
                                       DO (SETF (REF table gene cond-number type)
                                                (CONS (FUNCALL function data)
                                                      (REF table gene cond-number type)))
                                          (IF-TRUE (EQUAL replicate last-replicate)
                                              THEN (SETF (REF table gene cond-number type)
                                                         (REVERSE (REF table gene
                                                                       cond-number type)))))
                               ELSE (LOOP 
                                       FOR type IN all-labels
                                       FOR function
                                           IN '(SECOND THIRD FOURTH FIFTH SIXTH SEVENTH)
                                       DO (SETF (REF table gene cond-number type)
                                                (LIST (FUNCALL function data)))
                                       )))))))
    (LOOP FOR gene IN (LABELS-OF bad-genes DIMENSION 1)
          DO (DISPLAY-LINE "Gene '" gene "' not found! Data ignored."))
    table
    ))


(DEFUN Make-microarray-table (data-file-or-list organism &KEY normalized)
  ; If argument is NIL, returns NIL
  ; If argument is filename, reads list within file
  ; If argument is list, converts to table
  ; 23 seconds to run Ehira_2006 with preexisting file
  (LET* ((org (SLOTV organism #$organism-prefix))
         (data-list
           (COND
             ((STRINGP data-file-or-list)
              ; 1.3 sec
                (WITH-INPUT-FROM-STRING
                  (s (FILE-TO-STRING data-file-or-list :MAX 10000000))
                  (READ s)))
             ((LISTP data-file-or-list) data-file-or-list)
             (T (ERROR (S+ "Argument to MAKE-MICROARRAY-TABLE must "
                         "be either a filename or a microarray list, "
                         "not ~A")
                  (TYPE-OF data-file-or-list)))))
         (label-list 
            (STRING-OF 
              (IF normalized 
;                '(ratio ratio2 Tvalues Tmean TSD Cvalues Cmean CSD ttest)
                 '(Tmean CSD ratio TSD CNG ratio2 ttest Cvalues Cmean TNG Tvalues)
                 '(xpos ypos csig cbg tsig tbg))))
         (table (NEW-TABLE (LIST $ $ label-list)))
         (label-line (IF (SAME (REF (FIRST data-list) 1) 'LABELS)
                         (POP data-list)))
         )

    ;; Remove this declaration if you uncomment out the next line
  ;  (declare (ignore label-line))
    (IF label-line (SETF label-list (STRING-OF (SECOND label-line))))
    (LOOP FOR (gene-symbol-or-string data) IN data-list
          AS gene-name
          = (IF (STRINGP gene-symbol-or-string)
                (REMOVE #\" gene-symbol-or-string)
                (SYMBOL-NAME gene-symbol-or-string))
          AS gene = (OR (FRAME-FNAMED (S+ org gene-name)) gene-name)
          DO (LOOP FOR cond-data IN data
                   FOR cond-number FROM 1
                   DO (LOOP FOR type IN label-list
                            FOR value IN cond-data
                            FOR type-number FROM 1
                            DO (IF (< type-number -1) ; 3)
                                   (SETF (REF table gene cond-number type)
                                     (FIRST value))
                                   (SETF (REF table gene cond-number type)
                                     value)))))
    table))


(DEFUN Raw-microarray-of (expt &KEY (column-order '(1 2 3 4 5 6 7)) (skip 0))
  "Provides preexisting raw microarray for given expt or constructs one"
  (IF (NOT (IsFRAME? expt))
      (ERROR (S+ "Argument to RAW-MICROARRAY-OF must be a microarray "
               "experiment frame, not ~A")
        (TYPE-OF expt)))
  ; (IF (NOT (TYPEP expt 'Microarray-expt)) (ERROR  ))

  (LET* ((table (SLOTV expt #$raw-data))
         (read-path (MICROARRAY-FILE-PATH-OF expt "raw-data"))
         (write-path (OR read-path (MICROARRAY-FILE-PATH-OF expt "raw-data" :CREATE T)))
         (organism (SLOTV expt #$organism))
        )         

    (COND
       (table table)
       (read-path 
          (SETF table (MAKE-MICROARRAY-TABLE read-path organism))
          (SETF (SLOTV expt #$raw-data) table))
       (T (SETF table (RAW-MICROARRAY-TABLE-FROM-REPLICATES expt
                           :COLUMN-ORDER column-order
                           :SKIP skip))
          (SETF (SLOTV expt #$raw-data) table)
          (WRITE-MICROARRAY-TABLE table write-path)
          table))
   ))

(DEFUN Normalize-Table-Medians (raw-table)
  "Normalizes a 3-d table of microarray data using the median values of the signals.  The background is
        subtracted from the raw signal before the medians are computed."
 (LET* ((condition-list (LABELS-OF raw-table DIMENSION 2))
        (normalized-data (NEW-TABLE (LIST $ condition-list $)))
        (genes (LABELS-OF raw-table DIMENSION 1))
       )
           ;creates a table of background-subtracted signals
  ; (SETF (REF normalized-data "  LABELS" 1 "CVALUES") "CVALUES"
  ;       (REF normalized-data "  LABELS" 1 "TVALUES") "TVALUES")
  (FOR-EACH condition IN condition-list
       AS csig-values-by-gene
         = (FOR-EACH gene IN genes
                AS csig = (REF raw-table gene condition "CSIG")
                AS cbg  = (REF raw-table gene condition "CBG")
                AS cvalues = (MAPCAR (LAMBDA (sig bg) (- sig bg))
                              csig cbg)
                COLLECT cvalues)
       AS csig-values-by-replication = (TRANSPOSE-LIST csig-values-by-gene)
       AS csig-medians = (APPLY-FUNCTION (MEDIAN values)
                            REPLACING values WITH csig-values-by-replication)
       AS corrections = (DIVIDE 1 BY csig-medians) 

       ;normalizes the column by the medians and creates a new table with the normalized values    
       (FOR-EACH gene IN genes
        FOR-EACH cvalues IN csig-values-by-gene
            AS cvalues-corrected = (PRODUCT-OF corrections cvalues)
            AS tsig = (REF raw-table gene condition "TSIG")
            AS tbg  = (REF raw-table gene condition "TBG")
            AS tvalues = (MAPCAR (LAMBDA (sig bg) (- sig bg))
                              tsig tbg)
            AS tvalues-corrected = (PRODUCT-OF corrections tvalues)
            (SETF (REF normalized-data gene condition "CVALUES") cvalues-corrected)
            (SETF (REF normalized-data gene condition "TVALUES") tvalues-corrected)
       ) ;end for-each gene    
  ) ;end for-each condition

  normalized-data   
)) ;end define-function Normalize-Table-Medians


(DEFUN Above-bg? (sigs bgs threshold)
  (LET ((count
          (FOR-EACH sig IN sigs
               FOR bg IN bgs
               WHEN (< sig (* 1.5 bg))
                 COUNT sig)))
    (IF (>= count threshold)
        NIL T)
  ))

(DEFUN Add-analysis-slots-to (table expt)
  (LET* ((raw (REF expt #$raw-data))
         (genes (BBL::SORT (LABELS-OF table DIMENSION 1)))
         (conditions (LABELS-OF table DIMENSION 2))
         (label-names 
            (STRING-OF 
    ;          '(c-OK t-OK cvalues tvalues cmean tmean csd tsd ttest ratio ratio2)))
               '(cng tng cvalues tvalues cmean tmean csd tsd ttest ratio ratio2)))
        )
    (LOOP
       FOR condition IN conditions
       AS replicates = (LENGTH (REF raw (FIRST genes) condition "CSIG"))
       AS threshold = (FLOOR (/ replicates 3))
       DO (LOOP
             FOR gene IN genes
             AS c-sig = (REF raw gene condition "CSIG")
             AS c-bg = (REF raw gene condition "CBG")
             AS t-sig = (REF raw gene condition "TSIG")
             AS t-bg = (REF raw gene condition "TBG")
   ;         AS c-OK = (Above-bg? c-sig c-bg threshold)
   ;         AS t-OK = (Above-bg? t-sig t-bg threshold)
             AS cng = (NOT (Above-bg? c-sig c-bg threshold))
             AS tng = (NOT (Above-bg? t-sig t-bg threshold))
             AS cvalues = (REF table gene condition "CVALUES")
             AS cmean = (MEAN cvalues)
             AS csd = (STD-DEV cvalues)         
             AS tvalues = (REF table gene condition "TVALUES")
             AS tmean = (MEAN tvalues)
             AS tsd = (STD-DEV tvalues)
             AS ttest = (T-TEST Tvalues Cvalues PAIRED)         
             AS ratio = (IF (NOT (= cmean 0)) (/ tmean cmean))
             AS ratio2 = (IF (AND ratio (> ratio 0)) (LOG2 ratio))
             DO (LOOP
                   FOR value IN (LIST cng tng cvalues tvalues cmean tmean csd tsd ttest ratio ratio2)
                   FOR label IN label-names
                   DO (SETF (REF table gene condition label) value))))
   ))


(DEFUN Normalized-microarray-of (expt &KEY type)
  "Provides preexisting normalized microarray of a given type for given expt or constructs one"
  (IF (NOT (IsFRAME? expt))
      (ERROR (S+ "Argument to NORMALIZED-MICROARRAY-OF must be a microarray "
               "experiment frame, not ~A")
        (TYPE-OF expt)))
  (LET* ((type (OR type "median"))
         (organism (SLOTV expt #$organism))
         (normalized-slot 
           (FRAME-FNAMED (JOIN "normalized-table-" type) T))
         (read-path (MICROARRAY-FILE-PATH-OF expt normalized-slot))
         (write-path (OR read-path (MICROARRAY-FILE-PATH-OF expt normalized-slot :CREATE T)))
         (table (SLOTV expt normalized-slot))
         (raw-data (RAW-MICROARRAY-OF expt))
        )

    (COND
       (table table)
       (read-path
          (SETF table (MAKE-MICROARRAY-TABLE read-path organism :NORMALIZED T))
          (SETF (SLOTV expt normalized-slot) table))
       ((EQUAL type "median")
          (SETF table (NORMALIZE-TABLE-MEDIANS raw-data))
          (ADD-ANALYSIS-SLOTS-TO table expt)
          (SETF (SLOTV expt normalized-slot) table)
          (WRITE-MICROARRAY-TABLE table write-path)
          table))
  ))

(DEFUN Inside-microarray-aux (table gene condition return 
       &KEY gene-list condition-list -labels)
  (COND
     ((LISTP gene)
        (LOOP FOR item IN 
          (APPLY-FUNCTION 
             (FORWARD-FUNCALL 'INSIDE-MICROARRAY-aux table g condition 
                   return :GENE-LIST T :-LABELS -labels)
             REPLACING g WITH gene)
          WHEN (AND (BBL::LAST item) (LISTP condition))
            APPEND item
          WHEN (AND (BBL::LAST item) (NOT (LISTP condition)))
            COLLECT item))

     ((LISTP condition)
        (LOOP FOR item IN 
          (APPLY-FUNCTION 
             (FORWARD-FUNCALL 'INSIDE-MICROARRAY-aux table gene c 
                   return :GENE-LIST gene-list :CONDITION-LIST T  :-LABELS -labels)
             REPLACING c WITH condition)
          WHEN (BBL::LAST item)
            COLLECT item))

     (T (LET* ((values
                  (LOOP FOR item IN return
                        AS value 
                          = (COND
                               ((EQUAL item "DESCR-L") 
                                  (DESCRIPTION-OF gene LENGTH 10000))
                               ((EQUAL item "DESCR-S") 
                                  (DESCRIPTION-OF gene LENGTH 30))
                               (T (REF table gene condition item)))
                        COLLECT value))
              )

          (COND
             ((AND gene-list condition-list (NOT -labels))
                (JOIN gene condition values))
             ((AND gene-list (NOT -labels)) (JOIN gene values))
             ((AND condition-list (NOT -labels)) (JOIN condition values))
             ((= (LENGTH return) 1)
                (FIRST values))
             (T values)))))

  )


(DEFUN Search-microarray-aux (table gene condition search-item 
                 operator value return controls
       &KEY gene-list condition-list)
  (COND
     ((LISTP gene)
        (LOOP FOR item IN 
          (APPLY-FUNCTION 
             (FORWARD-FUNCALL 'SEARCH-MICROARRAY-aux table g condition search-item
                 operator value return controls :GENE-LIST T)
             REPLACING g WITH gene)
           WHEN (AND item (LISTP condition))
            APPEND item
          WHEN (AND item (NOT (LISTP condition)))
            COLLECT item))

     ((LISTP condition)
        (LOOP FOR item IN 
          (APPLY-FUNCTION 
             (FORWARD-FUNCALL 'SEARCH-MICROARRAY-aux table gene c search-item
                 operator value return controls 
                             :GENE-LIST gene-list :CONDITION-LIST T)
             REPLACING c WITH condition)
          WHEN (BBL::LAST item)
            COLLECT item))
        
     (T (LET* ((if-target-above-bg (FIRST controls))
               (if-control-above-bg (SECOND controls))
               (target (REF table gene condition search-item))
               (target-passes
                  (AND
                    target
                    (FUNCALL operator target value)
                    (NOT (AND if-target-above-bg
                              (REF table gene condition "TNG")))
                    (NOT (AND if-control-above-bg
                              (REF table gene condition "CNG")))))
               (return-values
                  (IF target-passes
                      (LOOP FOR item IN return
                            AS return-value 
                              = (COND
                                   ((EQUAL item "DESCR-L") 
                                      (DESCRIPTION-OF gene LENGTH 10000))
                                   ((EQUAL item "DESCR-S") 
                                      (DESCRIPTION-OF gene LENGTH 30))
                                   (T (REF table gene condition item)))
                            COLLECT return-value)))
              )
          (COND
             ((NOT return-values) NIL) 
             ((AND gene-list condition-list)
                (JOIN gene condition return-values))
             (gene-list (JOIN gene return-values))
             (condition-list (JOIN condition return-values))
             ((= (LENGTH return) 1)
                (FIRST return-values))
             (T return-values)))))

  )
