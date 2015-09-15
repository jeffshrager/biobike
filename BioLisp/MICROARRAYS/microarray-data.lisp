;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bbi)

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

(DEFUN Make-raw-microarray-list
  (expt &KEY (column-order '(1 2 3 4 5 6 7)) (skip 0))
  (IF (NOT (IsFrame? expt))
      (ERROR "Argument to MAKE-RAW-MICROARRAY-TABLE must be a frame, not ~A" expt))
  (LET* ((conditions (SLOTV expt #$Conditions))
         (table (NEW-TABLE '($ $ (xpos ypos csig cbg tsig tbg))))
         (key-labels '("CSIG" "CBG" "TSIG" "TBG"))
         (all-labels (APPEND '("XPOS" "YPOS") key-labels))
         )
    (DISPLAY "Processing " expt)
    (LOOP FOR (condition condition-descr) IN conditions
          FOR cond-number FROM 1
          AS replicates = (SLOTV condition #$Replicates)
          AS last-replicate = (FIRST (LAST replicates))
          AS dye-flips = (REF condition #$dye-flip)
          DO (DISPLAY-LINE *tab* condition-descr)
          (LOOP
            FOR replicate IN replicates
            FOR flip? IN dye-flips
            AS lines = (BBL::READ replicate TABBED CONVERT-NUMBERS)
            DO (LOOP FOR line in lines
                     FOR line# FROM 1
                     AS first-field = (FIRST line)
                     AS comment? = (AND (STRINGP first-field)
                                     (EQUAL (SUBSEQ first-field 0 1) "#"))
                     AS data = (AND (NOT comment?)(> line# skip)
                                 (MAPCAR (LAMBDA (n) (NTH (- n 1) line)) column-order))
                     AS gene = (FIRST data)
                     AS csig = (REF table gene cond-number "csig")
                     DO (UNLESS (OR comment? (<= line# skip))
                          (IF flip?
                              (SETF data (REF data '(1 2 3 6 7 4 5))))
                          (IF-TRUE csig
                            THEN (LOOP FOR type IN key-labels
                                       FOR function IN '(FOURTH FIFTH SIXTH SEVENTH)
                                       DO (SETF (REF table gene cond-number type)
                                            (CONS (FUNCALL function data)
                                              (REF table gene cond-number
                                                
                                                type)))
                                       (IF-TRUE (EQUAL replicate last-replicate)
                                         THEN (SETF (REF table gene cond-number
                                                      
                                                      type)
                                                (REVERSE (REF table gene
                                                           
                                                           cond-number type)))))
                            ELSE (LOOP FOR type IN all-labels
                                       FOR function
                                       IN '(SECOND THIRD FOURTH FIFTH SIXTH SEVENTH)
                                       DO (SETF (REF table gene cond-number type)
                                            (LIST (FUNCALL function data)))
                                       ))))))
    (INTERLEAVE (GARRAY-COMPONENT-INDICES table)
      (GMAP 'IDENTITY table))
    ))



(DEFUN Make-raw-microarray-table (raw-data-file-or-list organism)
  "Converts microarray raw data in file in form of a list into a table"
  ; If argument is NIL, returns NIL
  ; If argument is filename, reads list within file
  ; If argument is list, converts to table
  ; 23 seconds to run Ehira_2006 with preexisting file
  (LET* ((org (SLOTV organism #$organism-prefix))
         (raw-list
           (COND
             ((STRINGP raw-data-file-or-list)
              ; 1.3 sec
              (WITH-INPUT-FROM-STRING
                (s (FILE-TO-STRING raw-data-file-or-list :MAX 10000000))
                (READ s)))
             ((LISTP raw-data-file-or-list) raw-data-file-or-list)
             (T (ERROR (S+ "Argument to MAKE-RAW-MICROARRAY-TABLE must "
                         "be either a filename or a microarray list, "
                         "not ~A")
                  (TYPE-OF raw-data-file-or-list)))))
         (table (NEW-TABLE '($ $ (xpos ypos csig cbg tsig tbg))))
         )
    (LOOP FOR (gene-symbol-or-string data) IN raw-list
          AS gene-name
          = (IF (STRINGP gene-symbol-or-string)
                (REMOVE #\" gene-symbol-or-string)
                (SYMBOL-NAME gene-symbol-or-string))
          AS gene = (OR (FRAME-FNAMED (S+ org gene-name)) gene-name)
          DO (LOOP FOR cond-data IN data
                   FOR cond-number FROM 1
                   DO (LOOP FOR type IN '("XPOS" "YPOS" "CSIG" "CBG" "TSIG" "TBG")
                            FOR value IN cond-data
                            FOR type-number FROM 1
                            DO (IF (< type-number 3)
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
  
  (OR (SLOTV expt #$raw-data)    ; table already exists
    (LET* ((expt-name (SLOTV expt #$Fname))
           (organism (SLOTV expt #$organism))
           (raw-data-file (SLOTV expt #$raw-data-file))
           (table
             (IF-TRUE raw-data-file
               THEN (MAKE-RAW-MICROARRAY-TABLE raw-data-file organism)
               ELSE (SETF raw-data-file
                      (FORMAT NIL "~a~a~a~a~a~a~a" *microarray-directory*
                        (SLOTV organism #$FName)
                        "/" expt-name "/" expt-name "-raw-data.txt"))
               (IF-TRUE (DIRECTORY raw-data-file)
                 THEN (SETF (SLOTV expt #$raw-data-file) raw-data-file)
                 (MAKE-RAW-MICROARRAY-TABLE raw-data-file organism)
                 ELSE (LET* ((microarray-list
                               (MAKE-RAW-MICROARRAY-LIST expt
                                 :COLUMN-ORDER column-order
                                 :SKIP skip)))
                        (BBL::WRITE FROM microarray-list
                          TO raw-data-file TEXT)
                        (SETF (SLOTV expt #$raw-data-file) raw-data-file)
                        (MAKE-RAW-MICROARRAY-TABLE microarray-list
                          organism))))))
      (SETF (SLOTV expt #$raw-data) table)
      table)))

