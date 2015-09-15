;;; -*- Package: bbi; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bbi)

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

;;; Author:  Michiko Kato, Jeff Elhai

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(
            com.biobike.help:document-module
               com.biobike.help:document-function) :bbi))


(DOCUMENT-MODULE input-output
  "Communication with the screen, files, and the world."
  (:keywords :read :file :write :input :output :IO)
  (:display-modes :bbl)
  #.`(:functions display display-line display-data display-table
                bbl::Read bbl::Write email-me Message-to Run-file Plot))

(DOCUMENT-FUNCTION DISPLAY
(:SUMMARY "Displays given text on screen with optional formatting ." )
(:SYNTAX (display text))
(:PARAMETERS
(text :VALUE-TYPE number\,string\,list  :DOCSTRING " the text to be displayed " )
)
(:RETURNS nil)
(:EXAMPLES
" 1. (DISPLAY 1 2 3)

	-->123"
" 2. (DISPLAY 1 *tab* 2 *tab 3)

	-->1   2   3 "
" 3. (DISPLAY (PROTEIN-OF A7120))

	-->will display all proteins of A7120 "
)
(:TEXT
(:p " The displayed elements are displayed without intervening spaces . ")
(:p " *tab* can be used to display the elements with tab between them .")
(:p " *newline* puts a line between elements .")
)
(:SEE-ALSO DISPLAY-LINE DISPLAY-DATA))


(DOCUMENT-FUNCTION READ-FASTA-FILE
  (:SUMMARY "Reads text file in FastA format")
  (:RETURNS "list")
  (:PARAMETERS 
        (path :docstring "Filename or filename preceded by the directory name. If directory is not specified, the function looks for the file in the user's directory." :parameter-type required :value-type string)
        (shared :docstring "If specified, reads the file in shared-file directory" :parameter-type flag :value-type boolean))
  (:EXAMPLES "1.
 (READ-FASTA-FILE \"example-seq.txt\") 
 --> (\"Example Sequence\"
          \"GTTACTCATTCTTGGCAGACCGCGCTGGGTCGGAATCGCACGAACAAATCTTATCAGGTAGACGTAAGGCGGTGCTGCCCTCTGTTGTGACTAGCAATATCAAACACATTCTGCGGATCA\")"
"2. To display the header and the sequence separately:
(DEFINE (HEADER SEQ) AS (READ-FASTA-FILE \"example-seq.txt\"))
-->\"GTTACTCATTCTTGGCAGACCGCGCTGGGTCGGAATCGCACGAACAAATCTTATCAGGTAGACGTAAGGCGGTGCTGCCCTCTGTTGTGACTAGCAATATCAAACACATTCTGCGGATCA\"
  Then,
(DISPLAY-DATA HEADER SEQ)
-->HEADER      Example Sequence
   SEQ	       GTTACTCATTCTTGGCAGACCGCGCTGGGTCGGAATCGCACGAACAAATCTTATCAGGTAGACGTAAGGCGGTGCTGCCCTCTGTTGTGACTAGCAATATCAAACACATTCTGCGGATCA
"

"3. Displaying the header and the sequence separately for a file containing multiple sequences:
  (FOR-EACH list in (READ-FASTA-FILE \"ntca-binding-sites-gapped.txt\" shared)
     AS HEADER = list [1] ;; extracts the first element of each list
     AS SEQUENCE = list [2] ;; extracts the second element of each list
    DO (DISPLAY-DATA HEADER SEQUENCE))

--->
HEADER	S7942:nir operon
SEQUENCE	AAAGTTGTAGTTTCTGTTACCAATTGCGAATCGAGAACTGCC--TAATCTGCCGA

HEADER	S7942:nirB-ntcB
SEQUENCE	TTTTTAGTAGCAATTGCTACAAGCCTTGACTCTGAAGCCCGC--TTAGGTGGAGC

HEADER	S7942:ntcA	
SEQUENCE	GAAAAAGTAGCAGTTGCTACAAGCAGCAGCTAGGCTAGGCCG--TACGGTAACGA

HEADER	S7942:glnB	 
SEQUENCE	-TTGCTGTAGCAGTAACTACAACTGTGGTCTAGTCAGCGGTGT-TACCAAAGAGT
.
.
.")
   (:TEXT
    (:p "Returns header and sequence in list")
    (:p "Extracts from FastA-formatted file one or more header/sequence pairs")
    (:ul 
       (:li "See this " ((:a href "/weblistenerdocs/mini-tutorial-read-fasta.html") "tutorial page ") "to learn how to upload files")    
       (:li "What is "((:a href "http://en.wikipedia.org/wiki/FASTA_format") "FASTA Format")"?"))
  )

)


 
