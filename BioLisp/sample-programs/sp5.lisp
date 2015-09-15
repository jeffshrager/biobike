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

5. Display alternate translations of a gene. The starting points of
proteins are often misidentified by automated methods. It would be
useful to have a routine that identified alternative start sites in
protein sequences and reported their sequences and lengths.

(ASSIGN gene-sequence (EXTRACT-GENE-SEQUENCE gene))
(ASSIGN start-codons ("ATG" "GTG" "TTG"))
(LOOP FOR pos FROM 0 BELOW (LENGTH gene-sequence) BY 3
      DO 
         (ASSIGN codon (EXTRACT-FROM gene-sequence pos LENGTH 3))
         (IF-EXISTS (FIND-SEQUENCE codon start-codons)
               THEN (ASSIGN protein 
                          (TRANSLATE (EXTRACT-STRING gene-sequence *end*)))
                    (DISPLAY-FORMATTED "Alternative protein: ~S" protein)
                    (DISPLAY-FORMATTED "Length: ~S" (LENGTH protein))))

Extract-string (string begin [[TO] end | LENGTH length]): Returns
substring of string, extending from the given beginning coordinate to
either the given end coordinate or (if the 3rd argument is LENGTH) the
number of specified elements. If no 3rd argument is provided, then a
single element is extracted.  Find-sequence (query-sequence [FROM|IN]
target-sequence): Returns beginning and end coordinates of
query-sequence within target-sequence or NIL if not found.
Query-sequence may follow target-sequence in argument list so long as
FROM or IN appears before the target sequence.  Display-formatted
(text-string &KEY (continue F) &REST interpolations): A gentler
version of FORMAT. Sends to screen (FORMAT T...) the text-string after
performing interpolations. Precedes text string with "~&" unless
:continue is set to T.

|#


(defun alternative-protein-sites (gene)
  (let* ((gene-sequence (extract-gene-sequence gene))
         (seqlen (length gene-sequence))
         (start-codons '("ATG" "GTG" "TTG")))
    (unless (zerop (mod seqlen 3)) (error "Gene length not a multiple of 3!"))
    (loop for pos from 0 below seqlen by 3 
          as next-codon = (subseq gene-sequence pos (+ pos 3)) 
          nconc
          (when (member next-codon start-codons :test 'string-equal)
            (list pos)
            ))))


(defun alternative-protein-sequences (gene-sequence sites)
  (loop for site in sites collect
        (list 
         site 
         (translate-gene-sequence-to-protein (subseq gene-sequence site))
         )))

;;; This is all obsolete.  Need to replace soon.

(defparameter *dna<->amino-acid*
  '(
    ("TTT" Phe "F") ("TTC" Phe "F") ("TTA" Leu "L") ("TTG" Leu "L")
    ("TCT" Ser "S") ("TCC" Ser "S") ("TCA" Ser "S") ("TCG" Ser "S")
    ("TAT" Tyr "Y") ("TAC" Tyr "Y") ("TAA" Ter "!") ("TAG" Ter "!")
    ("TGT" Cys "C") ("TGC" Cys "C") ("TGA" Ter "!") ("TGG" Trp "W")
    ("CTT" Leu "L") ("CTC" Leu "L") ("CTA" Leu "L") ("CTG" Leu "L")
    ("CCT" Pro "P") ("CCC" Pro "P") ("CCA" Pro "P") ("CCG" Pro "P")
    ("CAT" His "H") ("CAC" His "H") ("CAA" Gln "Q") ("CAG" Gln "Q")
    ("CGT" Arg "R") ("CGC" Arg "R") ("CGA" Arg "R") ("CGG" Arg "R")
    ("ATT" Ile "I") ("ATC" Ile "I") ("ATA" Ile "I") ("ATG" Met "M")
    ("ACT" Thr "T") ("ACC" Thr "T") ("ACA" Thr "T") ("ACG" Thr "T")
    ("AAT" Asn "N") ("AAC" Asn "N") ("AAA" Lys "K") ("AAG" Lys "K")
    ("AGT" Ser "S") ("AGC" Ser "S") ("AGA" Arg "R") ("AGG" Arg "R")
    ("GTT" Val "V") ("GTC" Val "V") ("GTA" Val "V") ("GTG" Val "V")
    ("GCT" Ala "A") ("GCC" Ala "A") ("GCA" Ala "A") ("GCG" Ala "A")
    ("GAT" Asp "D") ("GAC" Asp "D") ("GAA" Glu "E") ("GAG" Glu "E")
    ("GGT" Gly "G") ("GGC" Gly "G") ("GGA" Gly "G") ("GGG" Gly "G")
    ))

(let ((codon (make-string 3)))
  (defun translate-gene-sequence-to-protein (sequence)
    (declare (simple-string sequence))
    (let* ((seqlen (length sequence))
           (protein-string (make-string (/ seqlen 3))))
      (declare (fixnum seqlen))
      (declare (simple-string protein-string codon))
      (loop for pos fixnum from 0 below seqlen by 3
            for j fixnum from 0 do
            (loop for i fixnum from pos 
                  for k fixnum from 0 below 3 do
                  (setf (schar codon k) (char-upcase (schar sequence i))))
            (setf (schar protein-string j)
                  (schar
                   (third 
                    (find codon *dna<->amino-acid* :test 'string= :key 'car))
                   0
                   )))
      protein-string
      )))
          

