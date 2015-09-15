;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

;;; Authors:  Jeff Elhai, JP Massar.

(in-package :bio)

;;; +============================================================================+
;;; | Copyright (c) 2001, 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers |
;;; |                                                                            |
;;; | Permission is hereby granted, free of charge, to any person obtaining      |
;;; | a copy of this software and associated documentation files (the            |
;;; | "Software"), to deal in the Software without restriction, including        |
;;; | without limitation the rights to use, copy, modify, merge, publish,        |
;;; | distribute, sublicense, and/or sell copies of the Software, and to         |
;;; | permit persons to whom the Software is furnished to do so, subject to      |
;;; | the following conditions:                                                  |
;;; |                                                                            |
;;; | The above copyright notice and this permission notice shall be included    |
;;; | in all copies or substantial portions of the Software.                     |
;;; |                                                                            |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,            |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF         |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.     |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY       |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,       |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE          |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                     |
;;; +============================================================================+


#|

7. Create a nucleotide frequency table You've identified a handful of
genes that respond to the same stimulus (maybe defense against viral
attack). If you can find a pattern in the regulatory region preceding
the genes, you might begin to understand how the stimulus evokes
increased expression of these genes. One approach is to try to find
positions upstream from the gene where certain nucleotides are
favored. The routine below takes a collection of sequences and
determines nucleotide counts at each position.

(DEFUN Count-motifs (strings)
  ;; Takes array of aligned sequences (in strings)
  ;; Returns count of each nucleotide at each position
  ;; Strings must be of same length for results to make sense
  (LET ((motif-length (LENGTH (FIRST strings)))
        (nucleotides (LIST "A" "C" "G" "T")))
     ; Create table
     (LOOP FOR string IN strings
           DO (LOOP FOR pos FROM 0 BELOW motif-length
                    DO (ASSIGN nuc (EXTRACT-STRING string pos))
                       (INCREMENT-ARRAY counts (pos nuc))))
     ; Display table
     (LOOP FOR nuc (LIST "A" "C" "G" "T")
           DO (DISPLAY nuc ": ")
              (LOOP FOR pos from 0 BELOW motif-length
                    DO (DISPLAY (VALUE-OF counts (pos nuc))" ")
                    FINALLY (DISPLAY *return*)))
     (RETURN-FROM Count-motifs counts)))

Display (&REST text-strings): Prints to screen the given text strings
concatenated together. *return* and *tab* are global constants
specifying appropriate control characters.  

|#


(defun display (&rest things)
  (format t "~A" (string-join (mapcar (lambda (x) (formatn "~A" x)) things))))

(defconstant *return* #\newline)

(defun count-motifs (strings &key (domain "ACGT"))

  (let* ((motif-len (length (first strings)))
         (frequency-array nil))

    (loop for string in (rest strings) do
          (when (/= (length string) motif-len)
            (error "All the strings are not the same length!")))

    ;; Initialize frequency counts.

    (loop for ch across domain do
          (loop for pos from 0 below motif-len do
                (setf (gref frequency-array ch pos) 0)))

    ;; Get the frequency distribution

    (loop for string in strings do
          (loop for ch across string 
                for pos below motif-len do
                (incf (gref frequency-array ch pos))))

    (loop for base across domain do
          (display base ": ")
          (loop for pos below motif-len do
                (display (gref frequency-array base pos) " ")
                finally (display *return*)
                ))

    frequency-array
    
    ))
    
    
