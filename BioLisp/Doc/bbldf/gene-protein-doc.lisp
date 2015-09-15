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

;;; Author:  JP Massar, Arnaud Taton, Jeff Elhai.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(com.biobike.help:document-module
               com.biobike.help:document-function) :bbi))


(DOCUMENT-MODULE genes-proteins
  "Extract gene and protein information"
  (:submodules 
      description-analysis 
      gene-neighborhood 
      translation
      gene-protein-types)
  (:functions Sequence-of Description-of Gene-of Ortholog-of Protein-of
       Display-sequence-of Genes-described-by Domains-of
	   Information-about-gene/s)
  (:keywords :description :gene :protein :sequence
   :genes :upstream :downstream :left :right :coordinate 
   :coordinates :|5'direction| :|3'direction| :forward :backward 
   :segment :encode :ortholog :cog :molecular-weight :hydrophobicity)
  (:display-modes :bbl))

(DOCUMENT-MODULE description-analysis
"Properties of genes and proteins"
(:keywords :description :gene :protein :sequence
:genes :encode :ortholog :cog :molecular-weight :hydrophobicity)
(:display-modes :bbl)
(:toplevel? nil)
#.`(:FUNCTIONS
description-of Gene/s-named chromosome-named contig-named
replicon-named Cog-id/s-of Gene/s-described-by
ortholog/s-of ortholog-of* homolog-of*
Hydrophobicity-of MW-of protein/s-of Protein-of Length-of Lengths-of
Encodes-protein? Name-of Protein/s-named
Amino-acid-counts-of Amino-acid-frequencies-of
Dinucleotide-biases-of Dinucleotide-comparison
Codon-frequencies-of Codon-frequency-comparison
Kegg-id-of Highlight-gene-in-pathway
Domains-of))


(DOCUMENT-MODULE gene-neighborhood
  "Genes and sequences around a given gene or coordinate"
  (:keywords :gene :protein :sequence :genes :upstream :downstream :left :right 
   :coordinate :coordinates :|5'direction| :|3'direction| :forward :backward 
   :segment)
  (:display-modes :bbl) 
  (:toplevel? nil)  
  #.`(:FUNCTIONS Gene/s-upstream-of
      Gene/s-downstream-of Gene/s-left-of Gene/s-right-of 
         Sequence-upstream-of Sequence-downstream-of Sequence-left-of
         Sequence-right-of Context/s-of Gene/s-of))
		 

(DOCUMENT-MODULE translation  
   "Codon <--> amino acid conversions and properties of amino acids"  
   (:keywords :genetic :code :codon :translate)  
   (:display-modes :bbl)  
   (:toplevel? nil)
   #.`(:FUNCTIONS Codons-of Convert-amino-acid/s  mw-of reading-frames-of
         translation-of hydrophobicity-of orfs-in))


(DOCUMENT-MODULE gene-protein-types
  "Type checks related to genes and proteins"
  (:keywords :gene :protein :sequence :genes)
  (:display-modes :bbl)
  (:toplevel? nil)
  #.`(:FUNCTIONS Is-gene? Is-protein? Is-DNA-sequence? Is-protein-sequence? 
         Is-RNA-sequence?))

