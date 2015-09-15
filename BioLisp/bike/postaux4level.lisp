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

;;; Author: JP Massar, Jeff Elhai.  

(DEFUN Random-DNA-aux (like frequencies length)
  (IF (AND like 
           (OR (NOT (PROVIDED length)) 
               (AND (> length 100000)
                    (>= (LENGTH like) length))))
        ; If a random sequence is desired like an existing
        ; sequence, with the same or a long length, simply shuffle
        ; the existing sequence
      (LET* ((sequence (string-join (ENSURE-LIST (SEQUENCE-OF like)) ""))
             #+oops
             (sequence (APPLY 'S+ (ENSURE-LIST (SEQUENCE-OF like))))
             (shuffled-sequence (SHUFFLE sequence)))
        (IF (PROVIDED length)
            (SUBSEQ shuffled-sequence 0 length)
            shuffled-sequence))
               ; Takes into account possibility of LIKE organism

      ; otherwise... 
      (LET* ((length (UNLESS-PROVIDED length 1000))
             (frequencies 
               (COND
                  (like (BACKGROUND-FREQUENCIES-OF like))
                  (frequencies frequencies)
                  (T '(.25 .25 .25 .25))))
             (normalized-frequencies 
               (DIVIDE frequencies BY (SUM-OF frequencies)))
            )
         (IF (AND like (> length 100000))
             ; Save time by making string with set frequencies 
             ; and shuffling it
             (LET ((nucleotide-numbers
                    (MAPCAR 'ROUND 
                      (MULTIPLY normalized-frequencies BY length)))
                  )
               (INCF (FIRST nucleotide-numbers)
                     (- length 
                        (reduce '+ nucleotide-numbers)
                        #+oops
                        (APPLY '+ nucleotide-numbers)
                        ))
               (SHUFFLE
                 (string-join
                   (MAPCAR (LAMBDA (len letter)
                             (MAKE-STRING len :INITIAL-ELEMENT letter))
                           nucleotide-numbers (list #\A #\C #\G #\T))
                   "")))
             ; Or calculate the string one nucleotide at a time    
             (LOOP FOR i FROM 0 TO (1- length)
                   WITH a-freq = (FIRST normalized-frequencies)
                   WITH c-freq = (+ a-freq (SECOND normalized-frequencies))
                   WITH g-freq = (+ c-freq (THIRD normalized-frequencies))
                   WITH string = (MAKE-STRING length :INITIAL-ELEMENT #\T)
                   AS n = (RANDOM 1.0)
                   AS letter = 
                      (COND
                         ((< n a-freq) #\A)
                         ((< n c-freq) #\C)
                         ((< n g-freq) #\G)
                         (T NIL))
                   DO (IF letter
                          (SETF (AREF string i) letter))
                   FINALLY (RETURN string))))))

(defun background-frequencies-of-aux 
       (sequence-source dna protein both-strands)
  (let* ((dna? (OR DNA (NOT Protein)))
         (alphabet (IF-TRUE DNA? THEN *nucleotides* ELSE *amino-acids*))
         (sequence-set nil)
         (bg-counts nil)
         (total nil)
         (answer nil))
    (ERROR-IF-MORE-THAN-ONE both-strands protein)
    (SETF sequence-set 
          (COND
           ((AND DNA? (TYPEP sequence-source 'Organism))
            (REPLICONS-OF sequence-source))
           ((TYPEP sequence-source 'Organism) 
            (PROTEINS-OF sequence-source))
           ((AND (LISTP sequence-source)
                 (EVERY 'LISTP sequence-source))
            (MAPCAR 'biolisp::SECOND sequence-source))
           (T sequence-source)))
    (SETF bg-counts 
          (LOOP FOR seq IN (ENSURE-LIST sequence-set)
            WITH totals = (MAKE-LIST (LENGTH alphabet) :INITIAL-ELEMENT 0)
            AS bg-count-set = (COUNTS-OF alphabet IN seq)
            DO (SETF totals (ADD totals TO bg-count-set))
            FINALLY (RETURN totals)))
    (SETF total (reduce '+ bg-counts))
    (SETF answer
          (LOOP FOR count IN bg-counts
            COLLECT (bbl::/ count total)))
    (WHEN both-strands
      (SETF (FIRST answer) 
            (bbl::/ (+ (FIRST answer) (FOURTH answer)) 2))
      (SETF (SECOND answer) 
            (bbl::/ (+ (SECOND answer) (THIRD answer)) 2))
      (SETF (THIRD answer) (SECOND answer))
      (SETF (FOURTH answer) (FIRST answer)))
    answer))
