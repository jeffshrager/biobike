;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Peter Seibel           |
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

;;; Authors:  JP Massar, Peter Seibel, Jeff Elhai.

(def-topic "Contiguous Sequences"
  (:summary "A continuous segment of DNA.")
  (:text
   (:p 
    #.(one-string-sp
       "An organism may consist of one or more contiguous sequences,"
	   "(or contigs)." 
       "A contiguous DNA sequence is one known to arise from a single"
	   "molecule. If the entire sequence of a genome is known, then"
       "contigs correspond to natural entities, replicons, i.e. chromosomes and"
       "plasmids. Otherwise, they merely reflect our level of ignorance"
       "and have no exact physical correlates."
       "In either case, only one strand of the DNA is shown, and the"
	   "complementary strand is implied but not shown."
	   ))
  (:p 
    #.(one-string-sp
 	   "The contigs of a genome can be retrived using the ")
   ((:A :HREF (:PRINT (MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
		       :NAME "CONTIGS-OF" :PACKAGE "bbi")))
      "CONTIGS-OF")
   " function."
   "This function is also called "
   ((:A :HREF (:PRINT (MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
		       :NAME "REPLICON/S-OF" :PACKAGE "bbi")))
      "REPLICON/S-OF")
   #.(one-string-sp
       " and either function can be used"
       "equivalently with any genome, regardless of whether the genome sequence"
       "is completely known. The completeness of a genome sequence can be"
       "determined using the ")
   ((:A :HREF (:PRINT (MAKE-HELP-FUNCTION-DOCUMENTATION-URL 
		       :NAME "INFORMATION-ABOUT-GENOME/S" :PACKAGE "bbi")))
      "INFORMATION-ABOUT-GENOME/S")
   " function and its Completed option."
   )
  (:p 
    #.(one-string-sp
	   "A contiguous sequence is either CIRCULAR or LINEAR. If CIRCULAR,"
       "the nucleotide with the greatest coordinate is considered to lie"
	   "adjacent to the nucleotide with a coordinate of 1."
	   "Most contigs of completely sequenced bacteria and bacteriophages"
	   "are circular. Contigs of all incompletely sequenced genomes are"
	   "considered to be linear."
       ))
    )
  (:see-also 
   ; (topic "Upstream/Downstream Sequences")
    (df bbl::CONTIGS-OF)
   bbi::REPLICON/S-OF
   bbi::CHROMOSOME/S-OF
   (DF bbi::INFORMATION-ABOUT-GENOME/S)
   ))


