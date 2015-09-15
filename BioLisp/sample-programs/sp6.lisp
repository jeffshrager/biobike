;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

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

;;; Authors:  Jeff Elhai, JP Massar.

#|

6. Make a 2-dimensional table of protein molecular weights You're
interested in a protein that you've identified biochemically. You know
its size and you know that it appears only when the organism is
exposed to heavy metal stress. You'd like to identify the gene that
encodes it, which might give you a clue as to what it does and thereby
gain insight as to how organisms resist heavy metals. Your clue is the
size of the protein. How many proteins in the organism have similar
sizes? This is a common questions, so you might want to generate
graphs of size vs frequency for all available organisms. The routine
below gets size vs counts, binning the data every 1000 g/mol (units of
molecular weight). I'll leave it to others to figure out how to graph
the data.
   
(LOOP FOR organism IN *loaded-organisms*
      DO (LOOP FOR protein IN (PROTEINS-OF organism)
               DO (ASSIGN mw (MW-OF protein))
                  (ASSIGN bin (ROUND rough-bin 1000
                  (IF-EXISTS (VALUE-OF mw-bin (organism bin))
                        THEN (INCREMENT-ARRAY mw-bin (organism bin))
                        ELSE (ASSIGN-ARRAY mw-bin (organism bin) 1))))
(DISPLAY-TABLE-IN-SOME-MAGICAL-WAY mw-bin)

Proteins-of (organism-frame): Returns list of protein-frames in given
organism.  

MW-of (protein-frame-or-amino-acid-sequence): Returns
molecular weight calculated from given amino acid sequence or from the
amino acid sequence of the given protein.  

Increment-array (array
(indices) [increment-amount]): Adds the increment-amount (default 1)
to the array element specified by the given indices. If the element
does not exist or is NIL, it is assigned a value of 1. The array may
be any array/hash acceptable to Assign-array. (The above program was
written by a person who did not know that the function can deal with
uninitialized elements).

|#

(defvar *molecular-weight-distributions*)

(defun protein-molecular-weight-distribution 
       (weight-delta-per-bin &key (max-weight 200000.0))
  (setq *molecular-weight-distributions* nil)
  (let ((minbin most-positive-fixnum) (maxbin 0))
    (loop for orgf in *loaded-organisms* do
          (loop for protein in (proteins-of-organism orgf) 
                as weight = (molecular-weight-of protein)
                as bin = (floor weight weight-delta-per-bin)
                as current-count =
                (gref *molecular-weight-distributions* orgf bin)
                do
                (when (<= weight max-weight)
                  (setq minbin (min minbin bin))
                  (setq maxbin (max maxbin bin))
                  (setf (gref *molecular-weight-distributions* orgf bin)
                        (if (integerp current-count) (1+ current-count) 1)
                        ))))
    (display-mw-table-in-ascii 
     *molecular-weight-distributions* minbin maxbin weight-delta-per-bin
     ))
  *molecular-weight-distributions*
  )

(defun display-mw-table-in-ascii (table minindex maxindex delta)
  (format t "~17@A " "ORGANISM")
  (loop for j from minindex to maxindex do (format t "~6D+  " (* j delta)))
  (terpri)
  (format t "~17A " "")
  (loop for j from minindex to maxindex do (format t "-------  "))
  (terpri)
  (loop for orgf in *loaded-organisms* do
        (let ((name (#^Fname orgf)))
          (format t "~17@A| " (subseq name 0 (min 17 (length name)))))
        (loop for j from minindex to maxindex do
              (let ((entry (gref table orgf j)))
                (when (null entry) (setq entry 0))
                (let* ((output (formatn "~D" entry)))
                  (format t "  ~4@A   " output)
                  )))
        (terpri))
  (terpri)
  )

(defun molecular-weight-of-protein-sequence (seq)
  (declare (simple-string seq))
  (let ((weight-sum 0))
    (declare (fixnum weight-sum))
    (loop for ch across seq do
          (if (amino-acid-designator? ch)
              (setq weight-sum 
                    (the fixnum (+ weight-sum (the fixnum (aa-to-mw ch)))))
            (progn
              (warn "invalid amino acid designator in protein sequence: ~S" ch)
              0
              )))
    weight-sum
    ))


                  
