;;; -*- Package: cl-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :cl-user) 

;;; +=========================================================================+
;;; | copyright (c) 2005 jp massar, jeff elhai, mark slupesky, peter seibel   |
;;; |                                                                         |
;;; | permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "software"), to deal in the software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the software, and to      |
;;; | permit persons to whom the software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | the above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the software.                  |
;;; |                                                                         |
;;; | the software is provided "as is", without warranty of any kind,         |
;;; | express or implied, including but not limited to the warranties of      |
;;; | merchantability, fitness for a particular purpose and noninfringement.  |
;;; | in no event shall the authors or copyright holders be liable for any    |
;;; | claim, damages or other liability, whether in an action of contract,    |
;;; | tort or otherwise, arising from, out of or in connection with the       |
;;; | software or the use or other dealings in the software.                  |
;;; +=========================================================================+

;;; author: jp massar, mark slupesky.  

(defpackage :bbl-test-user 
  (:nicknames :btu)
  (:use :bbl :tests)
  (:import-from :utils :cl :c/l)
  (:import-from :bbi :with-bbl-form))

;; Warning.  You cannot run an individual test that uses any of the 
;; defconversion stuff.  The defconversion definitions only are in effect
;; when you do (run-chapter-tests :df), and this is caused by the 
;; :before-code and :after-code below, which stash away the current 
;; defconversion table and replace it with the tests conversion table.

(tests::define-chapter 
 :df
 :package :bbi 
 :directory #.(cl-user:translate-simple-lp "biol:Bbltests;")
 :files ("df-tests.lisp")
 :before-code 
 (progn 
   (setq *saved-defconversion-hash* bbi::*defconversion-hash-table*)
   (setq bbi::*defconversion-hash-table* 
         (make-hash-table :test 'equal))
   (bbi::execute-df-tests-defconversion-forms)
   )
 :after-code
 (progn
   (clrhash bbi::*defconversion-hash-table*)
   (setq bbi::*defconversion-hash-table* *saved-defconversion-hash*)
   (setq *saved-defconversion-hash* nil)
   )
 )

(tests::define-chapter 
 :bbl-basic
 :package :bbl-test-user 
 :directory #.(cl-user:translate-simple-lp "biol:Bbltests;")
 :files ("bbl-basic-tests.lisp")
 )

(tests::define-chapter 
 :bbl-replace 
 :package :bbl-test-user 
 :directory #.(cl-user:translate-simple-lp "biol:Bbltests;")
 :files ("bbl-replace-tests.lisp")
 )


;; a random loaded organism
(defvar btu::some-organism nil)
;; SO's chromosome, MAY BE NIL
(defvar btu::some-chromosome nil)
;; one of SO's's contigs, chosen randomly
(defvar btu::some-contig nil)
;; one of that contig's genes, randomly
(defvar btu::some-gene nil)
;; SG's protein
(defvar btu::some-corresponding-protein nil)
;; another of the org's genes, from a dif. contig is possible
(defvar btu::another-gene nil)
;; another protein; its gene is on the same contig as AG
;; MAY BE NIL
(defvar btu::another-protein nil)
;; MAY BE NIL
(defvar btu::first-circular-contig nil)

;;; these four MAY BE NIL
(defvar btu::some-gene-with-overlaps-in-circular nil)
(defvar btu::some-gene-with-no-overlaps-in-circular nil)
(defvar btu::some-gene-with-overlaps-in-non-circular nil)
(defvar btu::some-gene-with-no-overlaps-in-non-circular nil)


;; These are variables to force the tests to use certain data.  
(defvar btu::use-this-organism nil)
(defvar btu::use-this-chromosome nil) ;; based on organism
(defvar btu::use-this-contig nil) ;; based on chromosome and organism
(defvar btu::use-this-gene nil) ;; based on organism
(defvar btu::use-this-corresponding-protein nil) ;; based on organism
;; if we ever need to investigate a gene and protein, 
;; we just use the above variables
;; (defvar btu::use-this-other--gene nil) ;; based on some-gene
;; (defvar btu::use-this-other-protein nil) ;; based on some-gene
(defvar btu::use-this-circular-contig nil)
(defvar btu::use-this-gene-with-overlaps-in-circular nil)
(defvar btu::use-this-gene-with-no-overlaps-in-circular nil)
(defvar btu::use-this-gene-with-overlaps-in-non-circular nil)
(defvar btu::use-this-gene-with-no-overlaps-in-non-circular nil)


;; this is the random seed, and RANDOM can take a second optional argument
(defparameter *rs* (make-random-state t))

;; something to use to test REPEAT-FUNCTION
(defparameter *repeat-function-value* 0)
(defun repeat-function-aux () 
  (setq *repeat-function-value* (1+ *repeat-function-value*)))

(defun btu::seqsim-return-type-check (result condition)
  (lisp:typep 
   result
   (cond
    ((eq condition :list-of-garray) 
     (and result (every (lambda (x) (typep x 'utils::garray)) result)))
    ((eq condition :list-of-garray-or-list-of-nil)
     (or 
      (every 'null result)
      (every (lambda (x) (typep x 'utils::garray)) result)))
    (t 
     (ecase condition
       (:garray-or-nil '(or null utils::garray))
       (:list-or-nil '(or null lisp:cons))
       (:garray 'utils::garray)
       (:list 'lisp:cons)
       )))))
      

(defmacro btu::seqsim-wrapper ((&optional (must-match? nil)) &body body)
  `(ecase (user::os?)
     (:unix
      (let ((result nil) (match? ,must-match?))
        (lisp:with-output-to-string (p)
          (let ((*standard-output* p))
            (setq result ,@body)
            ))
        (btu::seqsim-return-type-check result match?)
        ))
     (:windows t)
     ))

(defun btu::loaded-organisms-by-total-sequence-size ()
  (flet ((orgsize (orgf) 
           (loop for c in (#^contiguous-sequences orgf)
                 sum
                 (#^sequence-length c)
                 )))
    (let ((list 
           (mapcar
            (lambda (orgf) (list (orgsize orgf) orgf))
            bio::*loaded-organisms*
            )))
      (mapcar 'second (sort list '< :key 'first))
      )))
   
   
(defun get-me-some-chromosome (org)
  (unless (#^contiguous-sequences org)
    (warn "The organism ~A has no contiguous sequences!" org))
  (let ((chromosome 
         (loop
          for contig in (#^contiguous-sequences org) 
          when (lisp:search "chromosome" (#^fname contig) :test 'string-equal)
          do (return contig))))
    (unless chromosome 
      (warn "The organism ~A has no contig designated as CHROMOSOME!" org))
    chromosome
    ))

;; Find a contig that has genes.  
;; If possible, return the contig other than the CHROMOSOME but if not
;; possible, use CHROMOSOME.
(defun get-me-some-contig (chr org)
  (block exit
    ;; make sure at least one contig actually has a gene
    (unless 
        (some
         (lambda (c) (plusp (length (#^genes-sorted-by-position c))))
         (#^contiguous-sequences org))
      (warn "Organism ~A has no contigs that actually have genes!" org)
      (return-from exit nil))
    (let* ((contig-list (remove chr (#^contiguous-sequences org))))
      ;; if there aren't any other contigs, use CHROMOSOME.  Since we know
      ;; at least one contig has genes and CHROMOSOME is the only contig, 
      ;; it must have genes.  
      (if (null contig-list) 
          chr
        ;; otherwise remove from consideration any contigs that don't have genes
        (let ((contigs-with-genes 
               (loop for c in contig-list 
                     as len = (length (#^genes-sorted-by-position c))
                     do 
                     (unless (plusp len)
                       (warn 
                        "Contiguous sequence ~A of organism ~A has no genes!"
                        c org
                        ))
                     when (plusp len) collect c
                     )))
          ;; If none of the other contigs have genes, use CHROMOSOME,
          ;; again with the same logic as above.  
          (if (null contigs-with-genes)
              chr
            ;; otherwise pick a random non-CHROMOSOME contig
            (elt contig-list (random (length contig-list) *rs*))
            ))))))

;; modified to take an org instead of a contig
(defun get-a-gene-and-corresponding-protein (org)  
  (block exit
    (let* ((protein nil) 
           (gene nil)
           (genes-list (#^genes org))
           (genes-with-proteins 
            (remove-if-not (lambda (g) (#^proteins g)) genes-list))
           (len (length genes-with-proteins)))
      (unless genes-with-proteins 
        (warn "The org ~A contains no gene with a corresponding protein!" org)
        (return-from exit nil))
      (loop 
       until protein
       do
       (setq gene (elt genes-list (random len *rs*)))
       (setq protein (first (#^proteins gene))))
      (values gene protein)
      )))

(tests::define-chapter 
 :bbl-org 
 :package :bbl-test-user 
 :directory #.(cl-user:translate-simple-lp "biol:Bbltests;")
 :files ("bbl-org-tests.lisp")
 :before-code 
 (org-tests-before-code)
 :after-code 
 (progn 
   (utils::cformatt "Clearing 'use-this-' variables.")
   (clean-up-use-these)
   ))

(tests::define-chapter 
 ;; sequence-similar-to (blast) tests
 :bbl-sst
 :package :bbl-test-user 
 :directory #.(cl-user:translate-simple-lp "biol:Bbltests;")
 :files ("bbl-sst-tests.lisp")
 :before-code 
 ;; org tests will usually be run prior to these and so 
 ;; the set up will not be necessary in that case
 (unless btu::some-organism (org-tests-before-code))
 :after-code 
 nil
 )

(defun set-up-use-these 
       (&key 
        use-this-organism 
        use-this-chromosome
        use-this-contig
        use-this-gene
        use-this-corresponding-protein
        use-this-circular-contig
        use-this-gene-with-overlaps-in-circular 
        use-this-gene-with-no-overlaps-in-circular 
        use-this-gene-with-overlaps-in-non-circular 
        use-this-gene-with-no-overlaps-in-non-circular)
 
  (when use-this-organism
    (setq btu::use-this-organism use-this-organism))
  (when use-this-chromosome
    (setq btu::use-this-chromosome use-this-chromosome))
  (when use-this-contig
    (setq btu::use-this-contig use-this-contig))
  (when use-this-gene
    (setq btu::use-this-gene use-this-gene))
  (when use-this-corresponding-protein
    (setq btu::use-this-corresponding-protein use-this-corresponding-protein))
  (when use-this-circular-contig
    (setq btu::use-this-circular-contig use-this-circular-contig))
  (when use-this-gene-with-overlaps-in-circular
    (setq btu::use-this-gene-with-overlaps-in-circular
          use-this-gene-with-overlaps-in-circular))
  (when use-this-gene-with-no-overlaps-in-circular
    (setq btu::use-this-gene-with-no-overlaps-in-circular
          use-this-gene-with-no-overlaps-in-circular))
  (when use-this-gene-with-overlaps-in-non-circular
    (setq btu::use-this-gene-with-overlaps-in-non-circular
          use-this-gene-with-overlaps-in-non-circular))
  (when use-this-gene-with-no-overlaps-in-non-circular
    (setq btu::use-this-gene-with-no-overlaps-in-non-circular
          use-this-gene-with-no-overlaps-in-non-circular))
  )

(defun clean-up-use-these ()
  (setq btu::use-this-organism nil)
  (setq btu::use-this-chromosome nil)
  (setq btu::use-this-contig nil)
  (setq btu::use-this-gene nil)
  (setq btu::use-this-corresponding-protein nil)
  (setq btu::use-this-circular-contig nil)
  (setq btu::use-this-gene-with-overlaps-in-circular nil)
  (setq btu::use-this-gene-with-no-overlaps-in-circular nil)
  (setq btu::use-this-gene-with-overlaps-in-non-circular nil)
  (setq btu::use-this-gene-with-no-overlaps-in-non-circular nil)
  )
       

(defun org-tests-before-code ()
  (let* ((orgs (bio::loaded-organisms))
         (x (length orgs)))
    (when (= x 0)
      (error "The organisms are not loaded!"))
    (unless (= x 0)
      ;; some-organism
      (setq btu::some-organism 
            (or btu::use-this-organism (elt orgs (random x *rs*))))

      ;; some-chromosome
      (if (not btu::use-this-chromosome)
          (setq btu::some-chromosome
                (get-me-some-chromosome btu::some-organism))
        (progn
          (unless btu::use-this-organism 
            (error 
             (one-string-nl
              "If you are going to select use-this-chromosome,"
              "you must also select use-this-organism."))
            (setq btu::some-chromosome btu::use-this-chromosome)
            )))
          
      ;; some-contig
      (if (not btu::use-this-contig)
          (setq btu::some-contig 
                (get-me-some-contig 
                 btu::some-chromosome btu::some-organism))
        (progn
          (unless (and btu::use-this-organism btu::use-this-chromosome)
            (error 
             (one-string-nl
              "If you are going to select use-this-contig,"
              "you must also select use-this-chromsome and use-this-organism."
              )))
          (setq btu::some-contig btu::use-this-contig)
          ))
             
      ;; some-gene and some-corresponding-protein
      (cond
       ((not (or btu::use-this-gene btu::use-this-corresponding-protein))
        (multiple-value-setq 
            (btu::some-gene btu::some-corresponding-protein)
            (get-a-gene-and-corresponding-protein 
             btu::some-organism)))
       ((and btu::use-this-gene (not btu::use-this-corresponding-protein))
        (unless btu::use-this-organism 
          (error 
           (one-string-nl
            "If you are going to select use-this-gene,"
            "you must also select use-this-organism."
            )))
        (utils::vif (prot (first (#^proteins btu::some-gene)))
                    (setq btu::some-corresponding-protein prot)
                    (error
                     (one-string-nl
                      "The gene you have selected, ~A, does not have"
                      "a corresponding protein!")
                     btu::some-gene
                     ))
        (setq btu::some-gene btu::use-this-gene)
        )
      
       ((and btu::use-this-corresponding-protein (not btu::use-this-gene))
        (unless btu::use-this-organism
          (error
           (one-string-nl
            "If you are going to select use-this-corresponding-protein,"
            "you must also select use-this-organism."
            )))
        (setq btu::some-gene (#^gene btu::use-this-corresponding-protein))
        (setq btu::some-corresponding-protein 
              btu::use-this-corresponding-protein)
        )

       ((and btu::use-this-gene btu::use-this-corresponding-protein)
        (unless btu::use-this-organism
          (error
           (one-string-nl
            "If you are going to select use-this gene"
            "and use-this-corresponding-protein, you must also"
            "select use-this-organism."
            )))
        (setq btu::some-gene btu::use-this-gene)
        (setq btu::some-corresponding-protein 
              btu::use-this-corresponding-protein)
        ))
         
      ;; we need a use-this var here
      (setq btu::another-gene
            (get-a-different-gene btu::some-gene))
     
      ;; we need a use-this var here
      (setq btu::another-protein
            (get-a-different-protein btu::another-gene))

      (setq btu::first-circular-contig
            (or btu::use-this-circular-contig
                (btu::first-circular-contig-in-system)))

      (setq btu::some-gene-with-no-overlaps-in-circular
            (or btu::use-this-gene-with-overlaps-in-circular
                (when btu::first-circular-contig
                  (loop for g across 
                        (#^genes-sorted-by-position btu::first-circular-contig) 
                        do 
                        (when (null (bio::overlapping-genes-of g)) 
                          (return g))))))
     
      (setq btu::some-gene-with-overlaps-in-circular
            (or btu::use-this-gene-with-no-overlaps-in-circular
            (when btu::first-circular-contig
              (loop for g across 
                    (#^genes-sorted-by-position btu::first-circular-contig) 
                    do 
                    (when (bio::overlapping-genes-of g) (return g))))))

      (setq btu::some-gene-with-no-overlaps-in-non-circular
            (or btu::use-this-gene-with-overlaps-in-non-circular
                (utils::vwhen 
                    (contig 
                     (btu::first-non-circular-contig-in-system))
                  (loop 
                   for g across (#^genes-sorted-by-position contig)
                   do 
                   (when (null (bio::overlapping-genes-of g)) (return g))))))
     
      (setq btu::some-gene-with-overlaps-in-non-circular
            (or btu::use-this-gene-with-no-overlaps-in-non-circular 
                (utils::vwhen 
                 (contig (btu::first-non-circular-contig-in-system))
                 (loop for g across (#^genes-sorted-by-position contig)
                       do 
                       (when (bio::overlapping-genes-of g) (return g))))))
      (utils::cformatt "Organism = ~A" btu::some-organism)
      (utils::cformatt "Chromosome = ~A" btu::some-chromosome)
      (utils::cformatt "Contig = ~A" btu::some-contig)
      (utils::cformatt "Some Gene = ~A" btu::some-gene)
      (utils::cformatt "Some corresponding protein = ~A" 
                       btu::some-corresponding-protein)
      (utils::cformatt "Another Gene = ~A" btu::another-gene)
      (utils::cformatt "Another protein = ~A" btu::another-protein)
      (utils::cformatt "first circular contig = ~A" btu::first-circular-contig)
      (utils::cformatt "Some gene with no overlaps in circular = ~A" 
                       btu::some-gene-with-no-overlaps-in-circular)
      (utils::cformatt "Some gene with overlaps in circular = ~A" 
                       btu::some-gene-with-overlaps-in-circular)
      (utils::cformatt
       "Some gene with no overlaps in non-circ = ~A"
       btu::some-gene-with-no-overlaps-in-non-circular)
      (utils::cformatt
       "some gene with overlaps in non-circ = ~A" 
       btu::some-gene-with-overlaps-in-non-circular)       
      )))
       
(defun get-a-different-gene (thegene)
  (block exit
    (let* ((org (#^organism thegene))
           (contig (#^contiguous-sequence thegene))
           (contigs (#^contiguous-sequences org)))
      (if (= 1 (length contigs))
          (let ((other-genes (remove thegene (#^genes org))))
            (when (null other-genes) 
              (warn
               (one-string-nl
                "No other genes in get-a-different-gene!  Using SOME-GENE"
                "Organism ~A, contig ~A"
                )
               org contig
               )
              (return-from exit thegene)
              )
            (elt other-genes (random (length other-genes) *rs*)))
        (let* ((other-contigs (remove contig contigs))
               (genes-on-other-contigs
                (loop for c in other-contigs nconc
                      (coerce (#^genes-sorted-by-position c) 'list))))
          (when (null genes-on-other-contigs)
            (error
             (one-string-nl
              "Almost impossible condition in get-a-different-gene!"
              "No genes on other contigs of organism ~A"
              )
             org
             ))
          (elt genes-on-other-contigs 
               (random (length genes-on-other-contigs) *rs*))
          )))))

(defun get-a-different-protein (thegene)
  (block exit
    (let* ((org (#^organism thegene))
           (contig (#^contiguous-sequence thegene))
           (contigs (#^contiguous-sequences org)))
      (if (= 1 (length contigs))
          (let* ((other-genes (remove thegene (#^genes org)))
                 (genes-with-proteins 
                  (remove-if-not (lambda (g) (#^proteins g)) other-genes)))
            (when (null genes-with-proteins)
              (warn
               (one-string-nl
                "No other genes with proteins in get-a-different-protein."
                "Organism ~A, contig ~A"
                )
               org contig
               )
              (return-from exit (first (#^proteins thegene)))
              )
            ;; get a random gene that has a protein 
            ;; get the protein associated with that gene
            (first 
             (#^proteins
              (elt
               genes-with-proteins
               (random (length genes-with-proteins) *rs*)))))
        (let* ((other-contigs (remove contig contigs))
               (genes-on-other-contigs
                (loop for c in other-contigs nconc
                      (coerce (#^genes-sorted-by-position c) 'list)))
               (genes-with-proteins 
                (remove-if-not 
                 (lambda (g) (#^proteins g)) genes-on-other-contigs))) 
          (when (null genes-with-proteins)
            (error
             (one-string-nl
              "No genes with proteins on other contigs of organism ~A"
              )
             org
             ))
          (first 
           (#^proteins 
            (elt genes-with-proteins
                 (random (length genes-with-proteins) *rs*))))

          )))))
         
 
(defun bbi::run-bbl-tests (&key (verbose t))
  (tests:run-chapter-tests :df :verbose verbose)
  (tests:run-chapter-tests :bbl-basic :verbose verbose)
  (tests:run-chapter-tests :bbl-replace :verbose verbose)
  (if (eq *organisms-descriptor* :seed-organisms)
      (bbi::run-bbl-org-tests :verbose verbose)
    ;; (format t ";; *** Not running org tests because of memory problem!")
    (bbi::run-bbl-org-tests :verbose t)
    ))

(defun bbi::run-bbl-org-tests (&key (verbose t))
  (if (bio::loaded-organisms)
      (if (organisms-suitable-for-bbl-tests?) 
          (progn 
            (tests:run-chapter-tests :bbl-org :verbose verbose :time? nil)
            #+not-yet
            (tests:run-chapter-tests :bbl-sst :verbose verbose :time? :room)
            )
        (when verbose 
          (utils::cformatt "Skipping BBL org tests, organisms not suitable.")))
    (when verbose 
      (utils::cformatt "Skipping BBL org tests, no organisms loaded.")
      )))

(defun organisms-suitable-for-bbl-tests? ()
  (every 
   (lambda (orgf) 
     (and (null (#^no-genes-or-proteins? orgf)) (null (#^no-proteins? orgf))))
   (bio::loaded-organisms)))
          
          
#||
tests not requiring a loaded organism for bbl functions:

if-true
last-n
max-of
min-of
assign
increment
decrement
sequence-of
define
new-table
table-format
split
from 
same
string-of
description-of
enter
fit
all-combinations-of
choose-from
length-of
is-nonnegative?
is-simple-list?
add
subtract
multiply
divide
negate
sum-of
product-of 
is-list?
is-string? 
swap
mean
all-true
none-true
all-false
some-true
some-false
inversion-of / opposite-strand-of / opposite-strands-of

Tests requiring loaded organisms for BBL functions

sequence-of
description-of
read-fasta-file
sequence-type-of
nucleotide-distance
length-of

intergenic-sequences-of
upstream-sequences-of
gene-left-of
gene-right-of

gene-named

-- finish going through spec
gene-of

-- substitute U and T
is-dna-sequence?
is-rna-sequence? 
is-protein-sequence?

-- translation-of? 


-- from elhai 6/5 email move to org-test as appropriate
 ^ ! 
Noncoding-Gene-of / Noncoding-Genes-of 
Coding-gene-of  / Coding-Genes-of
Protein-of /  Proteins-of
Replicon-of  / Replicons-of Contig-of / Contigs-of 
Organism-of / Organisms-of
;; Common-Orthologs-of
Cog-ID-of  / Cog-IDs-of
MW-of / MWs-of
Transpose-List 
Log2
Log10 
Shuffle 

;;;;;;;;;;;;;; 

Tests requiring loaded organisms for BBL functions

sequence-of
description-of
read-fasta-file
sequence-type-of
nucleotide-distance
length-of

intergenic-sequences-of
upstream-sequences-of
gene-left-of
gene-right-of

gene-named

-- finish going through spec
gene-of

-- substitute U and T
is-dna-sequence?
is-rna-sequence? 
is-protein-sequence?

-- translation-of? 




||#