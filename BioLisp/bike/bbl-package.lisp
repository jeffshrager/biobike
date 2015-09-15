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

;;; Author: JP Massar. 

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (DEFPARAMETER 
      *shadowed-functions*
    '(:First :Second :Third :Fourth :Fifth :Sixth 
      :Seventh :Eighth :Ninth :Tenth :Last
      :Mean :Loop :Xloop :Round :Sort :Condition :Write
      :Read :Filter :Abs :Log :Replace :Mod :Exp :Sqrt
      :Combinations-of 
     )))

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (DEFPARAMETER 
      *completed-functions*
    (CONCATENATE 
     'LIST
     '(
      ;; PARAMETERS
       *bbl-time-stamp* $ TRUE FALSE *big-number* *small-number* *tab* 
       *newline* *CR* *nucleotides* *amino-acids* *all-organisms* 
       *microarray-tables* *division-converts-rationals-to-floats*
       *end* *viral-dinucleotides* *bacterial-codons* *genbank* @
       )

     #+moved-to-org-specific-file
     ;; ((EQL cl-user::*organisms-descriptor* :cyanobacteria)
     '(*all-cyanobacteria*
       *complete-genomes*
       ;; Marine-cyanobacteria
       *marine-cyanobacteria*
       *marine-unicellular-cyanobacteria*
       *marine-Prochlorococcus-and-Synechococcus*
       *marine-Prochlorococcus*
       *marine-Synechococcus*
       *marine-filamentous-cyanobacteria*
       *marine-n-fixing-cyanobacteria*
       ;; Terrestrial-and-limnetic-cyanobacteria
       *terrestrial-and-limnetic-cyanobacteria*
       *terrestrial-and-limnetic-unicellular-cyanobacteria*
       *terrestrial-and-limnetic-Synechococcus*
       *terrestrial-and-limnetic-filamentous-cyanobacteria*
       *terrestrial-and-limnetic-n-fixing-cyanobacteria*
       ;; Unicellular-cyanobacteria
       *unicellular-cyanobacteria*
       ;;    *marine-unicellular-cyanobacteria*
       ;;    *marine-Prochlorococcus-and-Synechococcus*
       ;;    *marine-Prochlorococcus*
       ;;    *marine-Synechococcus*
       ;;    *terrestrial-and-limnetic-unicellular-cyanobacteria*
       ;;    *terrestrial-and-limnetic-Synechococcus*
       *unicellular-n-fixing-cyanobacteria*
       ;; Filamentous-cyanobacteria
       *filamentous-cyanobacteria*
       ;;    *marine-filamentous-cyanobacteria*
       ;;    *terrestrial-and-limnetic-filamentous-cyanobacteria*
       *filamentous-n-fixing-cyanobacteria*
       *heterocystous-cyanobacteria*
       ;; N-fixing-cyanobacteria
       *n-fixing-cyanobacteria*
       ;;    *marine-n-fixing-cyanobacteria*
       ;;    *terrestrial-and-limnetic-n-fixing-cyanobacteria*
       ;;    *unicellular-n-fixing-cyanobacteria*
       ;;    *filamentous-n-fixing-cyanobacteria*
       ;;    *heterocystous-cyanobacteria*
       ) ;;)

     #+moved-to-org-specific-file
     ;;  ((EQL cl-user::*organisms-descriptor* :photosythetic-bacteria)
     '(*all-cyanobacteria*
       *complete-genomes*
       *marine-cyanobacteria*
       *marine-unicellular-cyanobacteria*
       *marine-Prochlorococcus-and-Synechococcus*
       *marine-Prochlorococcus*
       *marine-Synechococcus*
       *marine-filamentous-cyanobacteria*
       *marine-n-fixing-cyanobacteria*
       *terrestrial-and-limnetic-cyanobacteria*
       *terrestrial-and-limnetic-unicellular-cyanobacteria*
       *terrestrial-and-limnetic-Synechococcus*
       *terrestrial-and-limnetic-filamentous-cyanobacteria*
       *terrestrial-and-limnetic-n-fixing-cyanobacteria*
       *unicellular-cyanobacteria*
       *unicellular-n-fixing-cyanobacteria*
       *filamentous-cyanobacteria*
       *filamentous-n-fixing-cyanobacteria*
       *heterocystous-cyanobacteria*
       *n-fixing-cyanobacteria*
       *all-photosynthetic-bacteria*
       *all-green-bacteria*
       *Chlorobi*
       *Chloroflexi*
       *purple-bacteria*
       ) ;;)

     #+moved-to-org-specific-file
     ;;  ((EQL cl-user::*organisms-descriptor* :viruses)
     '(
       *all-sequences* 
       *metagenomes* *marine-metagenomes* *known-viruses*
       *eukaryotic-viruses* *prokaryotic-viruses*
       *dsDNA-viruses* *ssRNA-viruses* *ssDNA-viruses* *dsRNA-viruses* ) ;) )


     ;; from bioutils      
     '(aa-to-1-letter-code 
       aa-to-3-letter-code aa-to-codons aa-to-long-name 
       aa-to-mw codon-to-aa

       ;; bbl-instance.lisp
       is-gene? is-protein?

       ;; bbi-definitions.lisp
       IS-SIMPLE-LIST? IS-NONNEGATIVE? Is-organism-list? Is-positive-integer?
       Is-positive-number? is-frame? Is-integer? Is-even? Is-odd? 
       Is-negative-number?

       ;; assign.lisp
       Assign Define

       ;; LEVEL I
       If-true If-false If-not-true Last-N Max-of Min-of Sum-of Product-of
       Internal-error Either Both Order Interleave Program
       Bbl-code Everyones negation-of quotient-of difference-of
       when-value-of calc True?
       ;; These must stay commented out!!!!!!!!!!!!!!!!!!!!
       ;; Condition Abs Log Mod Exp Sqrt


       ;; LEVEL II
       Table-format New-Table Break-string From Same
       Read-FastA-file Increment Decrement
       Split string/s-of String-of Strings-of
       For-each Enter Nucleotide-distance BBL-Version
       All-DNA-sequences All-protein-sequences
       Sequence-type-of Fit
       gene/s-upstream-of Gene-upstream-of Genes-upstream-of
       gene/s-downstream-of Gene-downstream-of Genes-downstream-of
       gene/s-left-of Gene-left-of Genes-left-of
       gene/s-right-of Gene-right-of Genes-right-of
       gene/s-of Gene-of Genes-of
       Gene/s-named Gene-named Genes-named
       protein/s-named Protein-named Proteins-named
       Swap Convert Slots-of
       ^ ! 
       Inversion-of 
       Opposite-Strand/s-of Opposite-Strand-of Opposite-Strands-of
       Noncoding-Genes-of Coding-Genes-of
       protein/s-of Protein-of Proteins-of 
       Name-of
       replicon/s-of Replicon-of Replicons-of
       contig/s-of Contig-of Contigs-of
       organism/s-of Organism-of Organisms-of
       Common-Orthologs-of
       cog-id/s-of Cog-ID-of Cog-IDs-of
       mw/s-of MW-of MWs-of
       Transpose-List Log2 Log10 Shuffle Who-is-here? Who-is-here
       All-true None-false None-NIL All-false All-NIL None-true Any-true 
       Any-false Any-NIL
       Alphabet-of Apply-function Apply-function-of 
       Sequence-Downstream-Of Sequence-Left-Of 
       Sequence-Right-Of Sequence-Upstream-Of
       Intersection-of Union-of Greater-than Less-than Simplify-List Join
       Encodes-protein? Encodes-protein Encode-protein? Encode-protein
       ortholog/s-of Ortholog-of Orthologs-of
       Ortholog-of* Homolog-of* Add-set Subtract-set Labels-of	
       Previous-result Repeat Forget Forget-all My-functions
       Table Element/s-of Raise-error
       Permutations-of Permutation-counts-of Combination-counts-of
       Time-space-usage all-strings
       
       ;; My-symbols
       Trim Run-file Labeled-sequence-from Organism-named 
       Organism/s-named All-same 
       Repeat-function My-session 
       chromosome-named contig-named replicon-named
       number-list Display-list

         ; Type checks
       Is-DNA-sequence? Is-DNA-sequence Is-protein-sequence? Is-protein-sequence
       Is-RNA-sequence? Is-RNA-sequence Is-string Is-string? Is-table? Is-table
       Is-number Is-number? Is-list? Is-list Is-NIL?

       ;; These must stay commented out!!!!!!!!!!!!!!!!!!!!
       ;; Round Sort Combinations-of 

       ;; LEVEL III
       Choose-from Intergenic-sequences-of Upstream-sequences-of
       length/s-of Length-of Lengths-of
       sequence/s-of Sequence-of Sequences-of
       Add Subtract Multiply Divide Negative Negate
       Average Std-dev Standard-deviation 
       item/s Item Items
       Reading-frames-of 
       chromosome/s-of Chromosome-of Chromosomes-of
       gene/s-described-by Gene-described-by Genes-described-by
       Hydrophobicity-of
       T-Test Random-integer Random-number 
       item/s-of-rank Item-of-rank Items-of-rank
       median
       downstream-sequence/s-of Downstream-sequences-of Downstream-sequence-of
       Upstream-sequence/s-of Upstream-sequences-of Upstream-sequence-of
       Transliterate 
       Sublist Inside-list Sublist-of
       Substring Inside-string Substring-of
       element/s-of-table Element-of-table Elements-of-table
       element/s-of-frame Element-of-frame Elements-of-frame
       Bin-data-of 
       Description-of Descriptions-of 
       Genes-in-pathway/s Genes-in-pathway gene-in-pathway
       Kegg-organisms-of Randomize
       Convert-amino-acid/s Codons-of
       Information-about-gene/s information-about-gene information-about-genes
       Information-about-genome/s information-about-genome 
       information-about-genomes
       Organism/s-in-group organism-in-group organisms-in-group
       ;; These must stay commented out!!!!!!!!!!!!!!!!!!!!
       ;; First Second Third Fourth Fifth Sixth Seventh Eighth Ninth Tenth
       ;; Last Mean 
      
       ;; LEVEL IV
       Position-of Positions-of
       translation/s-of Translation-of Translations-of
       count/s-of Count-of Counts-of
       Count-of-each Counts-of-each
       match/es-of Match-of Matches-of
       Random-DNA GC-fraction-of Header-of
       Display Display-line Display-data old-Display-table display-table
       matches-of-pattern match-of-pattern matches-of-patterns match-of-patterns
       match-of-item matches-of-item match-of-items
       matches-of-items
       Display-sequence-of Display-sequences-of
       Alignment-of Motifs-in
       Orfs-in make
       ;; These must stay commented out!!!!!!!!!!!!!!!!!!!!
       ;; Read Write
      
       ;; LEVEL V
       
       make-circular-map 
       Group Microarray-table-from Inside-microarray Search-microarray
       Align-blast-result
       view-seed-gene Domains-of

       ;; Level VI
       sequence/s-similar-to Sequence-similar-to Sequences-similar-to
       gene/s-similar-to Genes-similar-to Gene-similar-to
       protein/s-similar-to Proteins-similar-to Protein-similar-to 
       Amino-acid-counts-of Amino-acid-frequencies-of
       Dinucleotide-biases-of Dinucleotide-comparison
       Codon-frequencies-of Codon-frequency-comparison
     

       ;; Level VII
       Plot
       SAM
       Write-Microarray-description-file
       Display*
       Display-blast
       Blast-value Blast-values
       annotate-seed-feature
       change-subsystem-role
       send-message Digestion-of Delete-file/s My-files My-variables 
       ;; These must stay commented out!!!!!!!!!!!!!!!!!!!
       ;; Filter

       ;; DISPLAY-CONTEXT-OF
       context/s-of context-of contexts-of

       ;; LOAD-PRIVATE-ORGANISM
       Upload-private-organism-from Load-private-organism

       ;; DATA-TABLE-FUNCTIONS
       All-data-tables Data-table-info-of 
       Ratio-of Ratios-of

       ;; PSSM-FUNCTIONS
       Make-PSSM-from Information-of Background-frequencies-of Apply-PSSM-to 
       Scan ; deprecated

       ;; MARKOV-FUNCTIONS
       Make-Markov-from Apply-Markov-to

       ;; RNAZ-FUNCTIONS
       Find-conserved-RNA-in Fold-RNA
 
       ;; PHYLIP-FUNCTIONS
       Tree-of Reroot-and-return-tree Consensus-tree
       Build-maximum-likelihood-tree-dna Build-parsimony-tree-dna
       Build-maximum-likelihood-tree-protein Build-parsimony-tree-protein
       Join-neighbors Build-distance-tree-dna Build-distance-tree-protein
       Resample-alignment Create-a-tree-project-from

       ;; BBI-KEGG-APIs-FUNCTIONS
       Kegg-id-of  
       Highlight-gene-in-pathway Kegg-org-of

 
       ;; BIOLISP FUNCTIONS
       Flatten

       ;; IN REPLACE.LISP
       ;; must stay commented out!
       ;; Replace
       Insert

       ;; share.lisp
       share
       unshare
       
       ;; Random Goo
       Comment sequence-viewer edit-gene edit-object

       ;; Annotation functions
       ALL-ROLES-IN-SUBSYSTEM GENES-WITHIN-ROLE GENES-WITHIN-SUBSYSTEM SUBSYSTEM-ROLE-OF
       SUBSYSTEM/S-OF VIEW-GENE HOST-OF-PHAGE
       CHANGE-SUBSYSTEM-ROLE/S-OF 

       )))
  
  (DEFPARAMETER *BBL-types*
    '(Gene Protein Contiguous-sequence Organism Labeled-sequence DNA RNA Table
           Frame Simple-List Organism-list Positive-number Positive-integer
           Nonnegative-number Nonnegative-integer Unprovided))

  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export *BBL-types* :bbi)
  (export *completed-functions* :bbi)
  )
 
     
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *useful-cl-symbols* 
    (append cl-user::*useful-cl-special-ops* 
            cl-user::*useful-cl-macros*
            cl-user::*useful-cl-functions*
            cl-user::*useful-cl-constants*
            cl-user::*useful-cl-variables*
            cl-user::*useful-cl-types*
            cl-user::*useful-cl-other-symbols*
            )))


(defpackage :bbl (:use :bbl-internals :webuser)
  (:nicknames :biobike)
  ;; get SORT and STABLE-SORT 
  ;;  REMOVE? #.`(:shadowing-import-from :shadowlisp 
  ;;            ,@shadowlisp:*symbols-to-shadowing-import*)
  ;; everything else from common lisp we want biobike users to be able to use
  ;; (wlisp redefines SETF, PUSH, PUSHNEW, etc, otherwise all its symbols are
  ;; all the common lisp exported symbols)
  #.`(:import-from :wlisp ,@*useful-cl-symbols*)
  (:shadowing-import-from 
   :bbi :define-function :for-each :description-of :same :unprovided)
  (:import-from :bioutils :xloop)
  (:import-from :utils :ref :-> :*safety* :*suppress-warnings*)
  ;; jp for jshrager 11/05/08
  (:import-from :wb :create-vpl-web-service :help-me-program-this
   :in-sequence :in-parallel :exit-workflow)
  #.`(:shadow :SORT)  ; <-- put in per JP to allow BBL::SORT
  #.`(:export ,@wb::*shadowing-import-def-form-symbols*)
  #.`(:export ,@*useful-cl-symbols*)
  #.`(:export ,@(let ((vars nil)) 
                  (do-external-symbols (v :webuser) (pushnew v vars))
                  vars))
  (:export :ref :-> :*safety* :*suppress-warnings*)
  ;; jp for jshrager 11/05/08
  (:export :wb :create-vpl-web-service :help-me-program-this
   :in-sequence :in-parallel :exit-workflow)
  ;; (:export :loop :xloop)
  ;; (:export :gene :protein :contiguous-sequence :organism :labeled-sequence) 
  (:export :define-function :defconversion :bbload :unprovided)
  ;; (:export :for-each :description-of :same)
  #.`(:shadowing-import-from :bbi ,@*completed-functions*)
  #.`(:export ,@*bbl-types*)
  #.`(:export ,@*completed-functions*)
  #.`(:export ,@*shadowed-functions*)
  )

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter bbi::*bbl-loop-tokens*
    '(
      bbl::init
      bbl::in 
      bbl::on
      bbl::for 
      bbl::from
      bbl::to
      bbl::by
      bbl::below
      bbl::downto
      bbl::then 
      bbl::while
      bbl::until
      bbl::collect
      bbl::sum
      ))

  (defparameter bbi::*bbl-define-function-tokens*
    '(
      bbl::required
      bbl::flags
      bbl::keys
      bbl::body
      ))

  (defparameter bbi::*bbl-ref-tokens* 
    '(
      bbl::[
      bbl::]
      ))

  (defparameter bbi::*bbl-pseudo-functions*
    '(
      bbl::{}
      bbl::[]
      bbl::e
      ))
  
  (defparameter bbi::*bbl-random-tokens*
    '(
      bbl::then
      bbl::else
      bbl::<none>
      bbl::ignore-below
      bbl::in-each
      ))

  )




(defmacro bbl::loop (&body body)
  #.(one-string-nl
     "A variation on the LISP:LOOP macro.  For examples, type"
     "(help bioutils::xloop). For a detailed description, click"
     "on the link produced by (help biotuils::xloop)."
     ""
     "Revised features include simplified syntax and semantics, the"
     "ability to loop over hash tables, vectors, and arrays using"
     "'in' syntax, and elimination of WITH in favor of INIT to"
     "avoid confusion with AS and FOR.")
  `(bioutils:xloop ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export bbi::*bbl-loop-tokens* :bbl)
  (export bbi::*bbl-define-function-tokens* :bbl)
  (export bbi::*bbl-ref-tokens* :bbl)
  (export bbi::*bbl-pseudo-functions* :bbl)
  (export bbi::*bbl-random-tokens* :bbl)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro whereis (function-name)
  (if (member :alias-of (symbol-plist function-name))
      `(get (get ',function-name :alias-of) :df-source-file)
    `(get ',function-name :df-source-file)
    ))





