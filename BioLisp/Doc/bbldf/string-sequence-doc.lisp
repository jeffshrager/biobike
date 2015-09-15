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

;;; Author:  Michiko Kato, Arnaud Taton, Bogdan Mihai, Hien Truong, Jeff Elhai


(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(com.biobike.help:document-module
               com.biobike.help:document-function) :bbi))

(DOCUMENT-MODULE strings-sequences 
  "Functions dealing with strings"
  (:keywords :text :string :length :letters :character :random)
  (:display-modes :bbl)
  (:submodules string-analysis
               string-extraction
               string-production
               bioinformatic-tools
               search/compare
               string-type-checks
               phylogenetic-tree)
 #.`(:functions LENGTH-OF SUBSTRING JOIN LABELED-SEQUENCE-FROM
        SEQUENCE-OF
        SEQUENCE-SIMILAR-TO
        make)
     )
	 
	(DOCUMENT-MODULE STRING-ANALYSIS 
  "Functions that analyze the properties of strings"
  (:keywords :string :character :size :search :find)
  (:display-modes :bbl)
  (:toplevel? nil)  
#.`(:functions alphabet-of count-of counts-of length-of lengths-of  
        nucleotide-distance GC-fraction-of background-frequencies-of
        Dinucleotide-biases-of Dinucleotide-comparison))
		
 
(DOCUMENT-MODULE STRING-EXTRACTION 
  "Functions that deliver parts of strings"
  (:keywords :string :extract :subseq :substring :part)
  (:display-modes :bbl)
  (:alpha-listing? nil) 
  (:toplevel? nil)  
#.`(:functions first second third fourth fifth sixth seventh eighth ninth tenth last substring-of item items split))


(DOCUMENT-MODULE STRING-PRODUCTION 
  "Functions that manipulate and produce strings"
  (:keywords :string :convert :replace :permute :combinations :combine :break)
  (:display-modes :bbl)
  (:toplevel? nil)  
#.`(:functions insert replace split join make randomize
    string/s-of item items
    inversion-of all-DNA-sequences all-protein-sequences reverse repeat
    random-dna sequence-of Fit Transliterate Trim substring
    labeled-sequence-from Permutations-of Combinations-of all-strings))
	
(DOCUMENT-MODULE BIOINFORMATIC-TOOLS
  "Bioinformatic tools for sequence analysis"
  (:keywords :search :align :alignment :PSSM :information :micro-RNA)
  (:display-modes :bbl)
  (:toplevel? nil)  
#.`(:functions ;; blast 
    apply-pssm-to information-of Make-pssm-from 
        Find-conserved-RNA-in Sequence-similar-to alignment-of Motifs-in
        Align-blast-result Fold-RNA))

(DOCUMENT-MODULE SEARCH/COMPARE
  "Tools to compare and search for sequences and strings"
  (:keywords :blast :search :align :alignment :mismatches)
  (:display-modes :bbl)
  (:toplevel? nil)  
#.`(:functions alignment-of ;; blast 
    blast-values digestion-of display-blast protein/s-similar-to 
     gene/s-similar-to
     match-of matches-of matches-of-pattern matches-of-item  
     Sequence/s-similar-to Align-blast-result))

	
(DOCUMENT-MODULE STRING-TYPE-CHECKS 
  "Functions that determine the type of a string"
  (:keywords :string)
  (:display-modes :bbl)
  (:toplevel? nil)  
#.`(:functions is-string? is-DNA-sequence? Is-protein-sequence? Is-RNA-sequence? 
            sequence-type-of))
			
(DOCUMENT-MODULE PHYLOGENETIC-TREE
  "Implementation of the PHYLIP software package to build phylogenetic trees"
  (:keywords :phylip :tree :phylogeny)
  (:display-modes :bbl)
  (:toplevel? nil)  
#.`(:functions Tree-of Create-a-tree-project-from Resample-alignment
               Build-distance-tree-protein Build-parsimony-tree-protein Build-maximum-likelihood-tree-protein
               Build-distance-tree-dna Build-parsimony-tree-dna Build-maximum-likelihood-tree-dna
               Consensus-tree Reroot-and-return-tree 
               Join-neighbors))




