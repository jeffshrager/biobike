;;; -*- Package: bbl-test-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bbl-test-user) 

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

;;; Author: JP Massar, Mark Slupesky.  

(defbblorgtest= 
 duplicate-genes-1
 (let ((orgs 
        (if (> (length bio::*loaded-organisms*) 20)
            (lisp:subseq bio::*loaded-organisms* 0 19)
          bio::*loaded-organisms*
          )))
   (lisp:loop for org in orgs do
     (let ((namehash (make-hash-table :test 'lisp:eq))
           (infohash (make-hash-table :test 'lisp:equal)))
       (lisp:loop for g in (#^genes org) 
         as found = nil
         do
         (if (gethash g namehash)
             (progn
               (setq found t)
               (utils::formatt 
                "~%The genes list for ~A has gene ~A duplicated!" org g))
           (setf (gethash g namehash) g))
         (let* ((from (#^from g))
                (to (#^to g))
                (dir (#^direction g))
                (contig (#^contiguous-sequence g))
                (info (list from to dir contig)))
           (if (gethash info infohash)
               (progn
                 (setq found t)
                 (utils::formatt 
                  "~%The gene ~A has the same CONTIG, FROM, TO & DIRECTION as ~A"
                  g (gethash info infohash)))
             (setf (gethash info infohash) g)
             ))
         finally (when found (terpri))
         ))
     finally (return t)
     ))
 t
 )

;;; SEQUENCE-OF
;; invert both inverts and complements
;; from-end affects the start while to-end affects the end 
;; as far as we can determine, from and from-end are mutually exclusive
;; as are to, to-end, and length 
;; if mutually exclusive flags are provided, no warning or error is signalled
;; from takes priority over from-end while to takes priority over to-end
;; which takes priority over length


;; sequence-of a gene
(defbblorgtest= 
 seqof-14
 (sequence-of some-gene)
 (bio::extract-sequence 
  (#^contiguous-sequence some-gene)
  :from (#^from some-gene) :to (#^to some-gene)
  :direction (#^direction some-gene)))

;; this fails because of the trailings *
;; ...of an ordinary protein
(defbblorgtest= 
 seqof-15
 (sequence-of some-corresponding-protein)
 (bio::extract-protein-sequence some-corresponding-protein))

;; ...of an ordinary contig
(defbblorgtest= 
 seqof-16
 (sequence-of some-contig)
 (bio::extract-contig-sequence 
  some-contig 1 (#^sequence-length some-contig) :f))

;; (sequence-of org) --> list of string being the seq of the contigs,
;; except when theres one contig it just returns the string
(defbblorgtest= 
 seqof-17
 (sequence-of some-organism)
 (let ((ncontigs (length (#^contiguous-sequences some-organism))))
   (if (= 1 ncontigs)
       (let ((cont (first (#^contiguous-sequences some-organism))))
         (bio::extract-contig-sequence cont 1 (#^sequence-length cont) :f))
     (lisp:loop for cont in (#^contiguous-sequences some-organism)
                collect
                (bio::extract-contig-sequence 
                 cont 1 (#^sequence-length cont) :f)))))

;; from-end, to-end, and length keywords
(defbblorgtest=
 seqof-18
 (list (length (sequence-of some-gene from-end -5))
       (length (sequence-of some-gene to-end -4))
       (length (sequence-of some-gene from-end -8 length 4)))
 (list 5 (- (bbi::with-bbl-form (length-of some-gene)) 3) 4))
 

;;; DESCRIPTION-OF

;; checking for any description in a gene
;; 30 is hard-coded in the function (?)
(defbblorgtest=
 org-description-1
 (description-of some-gene)
 (let* ((desc (or (#^annotation some-gene)
                  (#^best-hit-descr some-gene)
                  (#^ec-description some-gene)
                  (#^cog-description some-gene)
                  (#^description some-gene)))
        (real-desc 
         (when desc (lisp:subseq desc 0 (min 30 (length desc))))))
   real-desc))

;; ...on a protein (so testing protein --> gene conversion),
;; and test the length keyword
(defbblorgtest=
 org-description-2
 (let* ((proteins (#^proteins (utils::lastelem bio::*loaded-organisms*)))
        (last-protein (utils::lastelem proteins)))
   (descriptions-of last-protein length cl::most-positive-fixnum))
 (let* ((proteins (#^proteins (utils::lastelem bio::*loaded-organisms*)))
        (last-protein (utils::lastelem proteins))
        (last-gene (#^gene last-protein))
        (desc (or (#^annotation last-gene)
                  (#^best-hit-descr last-gene)
                  (#^ec-description last-gene)
                  (#^cog-description last-gene)
                  (#^description last-gene)
                  )))
   desc))

;; ...on a protein, and using a different length and labeled keys
(defbblorgtest=
 org-description-3
 (let* ((proteins (#^proteins some-organism))
        (2last-protein (if (> (length proteins) 1) 
                           (lisp:second (reverse proteins))
                         (lisp:car proteins))))
   (description-of 2last-protein length 58 +label))
 (let* ((proteins (#^proteins some-organism))
        (2last-protein (if (> (length proteins) 1) 
                           (cadr (reverse proteins))
                         (lisp:car proteins)))
        (2last-gene (#^gene 2last-protein))
        (desc (or (#^annotation 2last-gene)
                  (#^best-hit-descr 2last-gene)
                  (#^ec-description 2last-gene)
                  (#^cog-description 2last-gene)
                  (#^description 2last-gene)
                  )))
   (list 2last-protein 
         (when desc (lisp::subseq desc 0 (min 58 (length desc)))))))


;; READ-FASTA-FILE

;; read a simple fasta
#-:sframes
(defbblorgtest=
 rff1
 (let* ((o (#^organism some-corresponding-protein))
        (proteins-dir 
         (utils::s+ (#^organism-data-directory o) "proteins/"))
        (fasta (utils::s+ proteins-dir "proteins.fasta"))
        (first-record (lisp:car (read-fasta-file fasta)))
        (fasta-header (bbl:name-of first-record))
        (fasta-obj (lisp:car (utils:string-split fasta-header)))
        (prefixed (utils::s+ (#^organism-prefix o) fasta-obj)))
   (list
    (bbi::is-protein? (frames::frame-fnamed prefixed))
    (cl-user::stringp (bbl:sequence-of first-record))))
 '(t t))


;; SEQUENCE-TYPE-OF
;; gene is either dna or rna, and protein is protein
(defbblorgtest=
 seqtypeof-1
 (let ((g1 some-gene)
       (p1 some-corresponding-protein))
   (setq g1 (sequence-type-of 
             (bio::extract-sequence g1)))
   (setq p1 (sequence-type-of 
             (bio::extract-sequence p1)))
   (and 
    (or (equal 'dna g1) (equal 'rna g1))
    (equal p1 'protein)))
 t)     


;; NUCLEOTIDE-DISTANCE
;; simple case
(defbblorgtest= 
 nd1
 (nucleotide-distance 2 4 some-contig)
 3)

;; give it one organism with a single contig
(defbblorgtest= 
 nd2
 (let ((o (first-organism-with-one-contig-in-system)))
   (if o (nucleotide-distance 1 5 o) 5))
 5)

;; test truncate keyword on a circular (?)
(defbblorgtest=
 nd3
 (let ((c btu::first-circular-contig))
   (and c (nucleotide-distance 1 cl::most-positive-fixnum c truncate)))
 (let ((c btu::first-circular-contig))
   (and c (#^sequence-length c))))

;; wrap keyword on a circular contig
(defbblorgtest=
 nd4 
 (let ((c btu::first-circular-contig))
   (and c (nucleotide-distance 1 (+ (#^sequence-length c) 10) c wrap)))
 (and btu::first-circular-contig 10))

;; test truncate keyword on a non-circular
(defbblorgtest=
 nd5 
 (let* ((c (first-non-circular-contig-in-system)))
   (when c (nucleotide-distance 1 cl::most-positive-fixnum c truncate)))
 (let ((c (first-non-circular-contig-in-system)))
   (when c (#^sequence-length c))))
    
 
;; error, passing it an organism with more than one contig
(defbblorgerrortest=
 e-nd1 
 (let ((o (first-organism-with-multiple-contigs-in-system)))
   (nucleotide-distance 1 2 o))
 error)


;; error, both truncate and wrap keyword
(defbblorgerrortest= 
 e-nd2 
 (nucleotide-distance 1 2 some-contig wrap truncate)
 error)
 
;; error, wrap keyword on a non-circular
(defbblorgerrortest= 
 e-nd3 
 (utils:vif (c (first-non-circular-contig-in-system))
            (nucleotide-distance 1 2 c wrap)
            (error ""))
 error)

;;; LENGTH-OF

;; of a gene
(defbblorgtest= 
 length3 
 (length-of some-gene)
 (length (bio::extract-sequence some-gene)))

;; of a protein
(defbblorgtest= 
 length4 
 (length-of some-corresponding-protein)
 (length (bio::extract-sequence some-corresponding-protein)))

;; contig
(defbblorgtest= 
 length5 
 (length-of some-contig)
 (length (bio::extract-sequence some-contig)))

;; INTERGENIC-SEQUENCES-OF
;; checking the length when min-size is 0
(defbblorgtest=
 iso1 
 (let ((seqs (intergenic-sequences-of some-organism minimum-size 0)))
   (length seqs))
 (length (#^genes some-organism)))

;; returns a list of strings
(defbblorgtest=
 iso2 
 (let ((i (intergenic-sequences-of some-contig)))
   (every 'stringp i))
 t) 


;; GENE-LEFT-OF/GENE-RIGHT-OF
;; the only gene left of the 2nd is the first
(defbblorgtest=
 gene-left-of-1 
 (gene-left-of 
  (lisp:elt (#^genes-sorted-by-position (first-large-contig-in-system)) 2))
 (lisp:elt (#^genes-sorted-by-position (first-large-contig-in-system)) 1))

;; and to the right is the 3rd
(defbblorgtest=
 gene-right-of-1 
 (gene-right-of 
  (lisp:elt (#^genes-sorted-by-position (first-large-contig-in-system)) 2))
 (lisp:elt (#^genes-sorted-by-position (first-large-contig-in-system)) 3))



;; UPSTREAM-SEQUENCES-OF
;; basic sanity tests
(defbblorgtest=
 uso1 
 (progn (upstream-sequences-of some-organism minimum-size 0) t)
 t
 )

(defbblorgtest=
 uso1a
 (progn (upstream-sequences-of some-contig minimum-size 0) t)
 t
 )

(defbblorgtest=
 uso1b
 (progn (upstream-sequences-of some-gene minimum-size 0) t)
 t
 )

(defbblorgtest=
 uso1c
 (lisp:typep 
  (upstream-sequences-of some-gene minimum-size 0 labeled)
  '(or bbl::labeled-sequence null)
  )
 t)

(defbblorgtest=
 dso1 
 (progn (downstream-sequences-of some-organism minimum-size 0) t)
 t)

(defbblorgtest=
 dso1a
 (progn (downstream-sequences-of some-contig minimum-size 0) t)
 t)

(defbblorgtest=
 dso1b
 (progn (downstream-sequences-of some-gene minimum-size 0) t)
 t
 )

(defbblorgtest=
 dso1c
 (lisp:typep 
  (downstream-sequences-of some-gene minimum-size 0 labeled)
  '(or bbl::labeled-sequence null)
  )
 t)



;; all elems in the returned list are null or string
(defbblorgtest=
 uso2 
 (let ((seqs (upstream-sequences-of some-organism)))
   (every (lambda (x) (or (null x) (stringp x))) seqs))
 t)
(defbblorgtest=
 dso2 
 (let ((seqs (downstream-sequences-of some-organism)))
   (every (lambda (x) (or (null x) (stringp x))) seqs))
 t)

;; GENE-NAMED
;; use an fname (a string)
(defbblorgtest=
 gene-named-1 
 #-:sframes
 (gene-named (#^fname some-gene))
 #+:sframes
 (gene-named (#^fname some-gene) in some-organism)
 some-gene)
 
;; GENE-OF

;; simple gene
(defbblorgtest=
 gene-of-1 
 (gene-of some-gene)
 some-gene)

;; protein
(defbblorgtest=
 gene-of-2 
 (gene-of some-corresponding-protein)
 (#^gene some-corresponding-protein))

;; organism and using an alias
(defbblorgtest=
 gene-of-3 
 (genes-of some-organism)
 (#^genes some-organism))

;; Genes-of returns gene fragments as well as genes.  This is why
;; the number of items returned by genes-of below isn't necessarily
;; equal to the number of genes on the contig
(defbblorgtest=
 gene-of-4a
 (let ((c (first-non-circular-contig-in-system)))
   (if c
       (let* ((clen (#^sequence-length c))
              (n (length (genes-of c from 1 to clen)))
              (m (length (#^genes-sorted-by-position c))))
         (>= n m))
     t))
 t
 )

;; Genes-of returns gene fragments as well as genes.  This is why
;; the number of items returned by genes-of below isn't necessarily
;; equal to the number of genes on the contig
(defbblorgtest=
 gene-of-5a
 (let ((c btu::first-circular-contig))
   (if c
       (let* ((clen (#^sequence-length c))
              (n (length (genes-of c from 1 to clen)))
              (m (length (#^genes-sorted-by-position c))))
         (>= n m))
     t))
 t)

;; test that every index for a gene returns that gene 
(defbblorgtest= 
 gene-of-6
 (let ((c (first-contig-in-system)))
   (if c
       (let ((gene (lisp:elt (#^genes-sorted-by-position c) 0)))
         (lisp:loop for index from (#^from gene) to (#^to gene)
           for j from 1 to 1000
           do 
           (unless (member gene (genes-of c from index to index))
             (return nil))
           finally (return t)
           ))
     t
     ))
 t
 )

;; find a intergenic position, and call genes-of on that coordinate,
;; which should return NIL (no genes there)
(defbblorgtest=
 gene-of-7
 (let ((gs (#^genes-sorted-by-position some-contig)))
   (when (>= (length gs) 2)
     (let* ((g1 (lisp:elt gs 0))
            (g2 (lisp:elt gs 1))
            (g1to (#^to g1))
            (g2from (#^from g2))
            (gdif (- g2from g1to)))
       ;; when this is T, the genes are not overlapping
       (when (> gdif 1)
         (let ((ip (+ g1to 1)))
           (genes-of some-contig from ip to ip)
           )))))
 nil
 )

;; coordinates FROM before start of 1st gene and after end of 2nd gene, 
;; genes-of with those FROM AND TO should return a list with those 2 genes
(defbblorgtest=
 gene-of-8
 (let ((gs (#^genes-sorted-by-position some-contig)))
   (when (>= (length gs) 2)
     (let* ((g1 (lisp:elt gs 0))
            (g2 (lisp:elt gs 1))
            (g1from (#^from g1))
            (g2to (#^to g2)))
       (if (and (> g1from 1) (< (+ g2to 1) (#^sequence-length some-contig)))
           (let ((r (genes-of some-contig from 1 to (+ g2to 1))))
             (if (and (member g1 r) (member g2 r)) nil :failed))
         nil
         ))))
 nil
 )

;; genes-of using coordinates within the 2 genes should still return 
;; both genes 
(defbblorgtest=
 gene-of-9
 (let ((gs (#^genes-sorted-by-position some-contig)))
   (when (>= (length gs) 2)
     (let* ((g1 (lisp:elt gs 0))
            (g2 (lisp:elt gs 1))
            (g1from (#^from g1))
            (g2to (#^to g2)))
       (let ((r (genes-of some-contig from (1+ g1from) to (1- g2to))))
         (if (and (member g1 r) (member g2 r)) nil :failed)
         ))))
 nil
 )

;; genes-of using coordinates within the 2 genes but with the EXCLUDE-OVERLAPS
;; flag, so make sure the result does NOT have those genes (in some 
;; degenerate case, some other gene could be in that range).  
(defbblorgtest=
 gene-of-10
 (let ((gs (#^genes-sorted-by-position some-contig)))
   (when (>= (length gs) 2)
     (let* ((g1 (lisp:elt gs 0))
            (g2 (lisp:elt gs 1))
            (g1from (#^from g1))
            (g2to (#^to g2)))
       (let ((r (genes-of 
                  some-contig from (1+ g1from) to (1- g2to) exclude-overlaps)))
         (if (and (not (member g1 r)) (not (member g2 r))) nil :failed)
         ))))
 nil
 )
       

;; nonsense TO value
(defbblorgerrortest=
 e-geneof-1 
 (let* ((c btu::first-circular-contig))
   (if c (genes-of c from -10 to 2) (error "No circular contig")))
 error)

;;; IS-DNA-SEQUENCE
;; with a gene
(defbblorgtest= 
 dnaseq-1 
 (is-dna-sequence (sequence-of some-gene))
 t)

;; with a protein
(defbblorgtest= 
 dnaseq-2 
 (is-dna-sequence (sequence-of some-corresponding-protein))
 nil)

;; IS-PROTEIN-SEQUENCE

;; Most genes have stop codons, which get translated to '*'
;; and is-protein-sequence doesn't like the trailing *.  
;; But we've discovered a few genes which translation-of doesn't
;; put '*' on the end.
(defbblorgtest= 
 protseq-11 
 (let ((translation (translation-of (sequence-of some-gene))))
   (if (lisp:char-equal (utils::lastelem translation) #\*)
       (setq translation (utils::sbutlast translation))
     (utils::cformatt "*** Gene ~A does not have a stop codon!"))
   (is-protein-sequence translation))
 t)

;; this succeeds because we strip off the end codon
(defbblorgtest= 
 protseq-1a
 (let ((gl (length-of some-gene)))
   (is-protein-sequence (translation-of (sequence-of some-gene to (- gl 3)))))
 t)

(defbblorgtest= 
 protseq-2
 (is-protein-sequence (sequence-of some-corresponding-protein))
 t)

;;; IS-RNA-SEQUENCE

;; with a gene
(defbblorgtest= 
 rnaseq-1
 (is-rna-sequence (sequence-of some-gene))
 nil)


;; coding-genes-of 

;; gene
(defbblorgtest=
 coding-genes-1 
 (coding-genes-of some-gene)
 some-gene)

;; protein
(defbblorgtest=
 coding-genes-2 
 (coding-genes-of some-corresponding-protein)
 some-gene)

;; contig
(defbblorgtest=
 coding-genes-3 
 (let ((count
        (lisp:loop for gene across (#^genes-sorted-by-position some-contig) 
          with xcount = 0
          as encodes? = (let ((utils::*suppress-warnings* t)) 
                          (#^encodes-protein gene))
          when encodes? do (incf xcount)
          finally (return xcount))))
   (= (length (coding-genes-of some-contig)) count))
 t)

;; check NIL
(defbblorgtest=
 coding-genes-4 
 (coding-genes-of nil)
 nil)

;; noncoding-genes-of

;; gene
(defbblorgtest=
 noncoding-genes-1 
 (noncoding-genes-of some-gene) 
 (#^encoodes-protein some-gene))

; protein
(defbblorgerrortest=
 e-noncoding-genes 
 (noncoding-genes-of some-corresponding-protein)
 error)

;; contig
(defbblorgtest=
 noncoding-genes-2 
 (let* ((genevec (#^genes-sorted-by-position some-contig))
        (noncount
         (lisp:loop for gene across genevec
           with xcount = 0
           as encodes? = (let ((utils::*suppress-warnings* t)) 
                          (#^encodes-protein gene))
           when (null encodes?) do (incf xcount)
           finally (return xcount))))
   (= (length (noncoding-genes-of some-contig)) noncount))
 t)

;; check NIL
(defbblorgtest=
 noncoding-genes-3 
 (noncoding-genes-of nil)
 nil)

;; with a contig
(defbblorgtest=
 noncoding-genes-4 
 (let ((g (noncoding-genes-of some-contig)))
   (every (lambda (x) (typep x 'gene)) g))
 t)

;; protein-of
;; with a contig
(defbblorgtest=
 protof1 
 (let ((c (protein-of some-contig)))
   (every (lambda (x) (typep x 'protein)) c))
 t)


;; replicon-of
;; with a gene, and also check nil
(defbblorgtest=
 repliconof1 
 (typep 
  (or (replicon-of nil)
      (replicon-of some-gene))
  'contiguous-sequence)
 t)

;; make sure we get back an org from this random check
(defbblorgtest=
 organismof1  
 (and (null (organism-of nil))
      (every (lambda (x) (typep x 'organism))
             (list
              (organism-of some-corresponding-protein)              
              (organism-of some-organism)           
              (organism-of some-gene)
              (organism-of some-contig))))
 t)

;; check that it returns a null or string
(defbblorgtest=
 cogid1 
 (let ((x (cog-id-of some-gene)))
   (or (null x) (stringp x)))
 t)

;; check it can handle nil and that no warnings works, 
;; then make sure the molec weight is a number
(defbblorgtest=
 mwof1 
 (numberp 
  (or (mw-of nil nowarnings) (mw-of some-gene)))
 t)

;; position-of
;; < shouldnt show up in in a gene seq.
(defbblorgtest=
 pos1  
 (position-of "<" (sequence-of some-gene))
 nil)

;; should have a C in a gene, but maybe its possible it couldnlt? xxx
(defbblorgtest=
 pos2  
 (let ((k (position-of "C" (sequence-of some-gene))))
   (or (null k) (integerp k)))
 t)

(defbblorgtest= 
 pos3 
 (position-of 
  some-gene
  (lisp:coerce 
   (#^genes-sorted-by-position (#^contiguous-sequence some-gene)) 'list))
 (lisp:1+ 
  (lisp:position 
   some-gene (#^genes-sorted-by-position (#^contiguous-sequence some-gene)))))

;; pick something from the entity, then <=
;; make sure it returns an integer with a gene
(defbblorgtest=
 pos4 
 (let ((pos (position-of "c" in some-gene)))
   (unless (integerp pos) 
     (cl:find "c" (sequence-of some-gene))))
 nil) 
 
;; same with a contig
(defbblorgtest=
 pos5 
 (let ((pos (position-of "C" in some-contig)))
   (integerp pos))
 t) 
 
;; same with a proteinb
(defbblorgtest=
 pos6 
 (lisp:loop for aa in *amino-acids* do
   (when (position-of aa in some-corresponding-protein)
     (return t)))
 t)


;; count-of
;; should only find first-gene once in both first-contig and genes by pos
(defbblorgtest=
 count0 
 (let* ((g some-gene)
        (c (#^contiguous-sequence g))
        (s (lisp:coerce (#^genes-sorted-by-position c) 'list)))
   (count-of g in-each `(,s ,(list g))))
 '(1 1))

;; calling C-O on an org that has no chromosome returns NIL and on a 
;; g p or o with a chromosome returns that chromosome.  While an
;; organism can have multiple contigs, it has 1 or 0 chromosomes, and 
;; if the chromosome exists, it is one of the org's contigs.  
(defbblorgtest=
 chrom1 
 (let ((c (chromosome-of some-organism nowarnings)))
   (or (null c) 
       (and (frames::isframe? c)
            #-:weblistener-aframes
            (eq (#^organism-entity-type c) #$contiguous-sequence)
            #+:weblistener-aframes
            (typep c (read-from-string "frames::bio.gene"))
            )))
 t)

(defbblorgtest=
 chrom2 
 (let ((thing 
        (list
         some-gene some-corresponding-protein
         (#^contiguous-sequence some-gene))))
   (let ((c-list 
          (lisp:mapcar 
           (lambda (x) (chromosome-of x nowarnings)) 
           thing)))
     (or 
      (every 'null c-list)
      (and 
       (eq (lisp:first c-list) (lisp:second c-list))
       (eq (lisp:first c-list) (lisp:third c-list))
       (every 
        (lambda (x) 
          (and (frames::isframe? x)
               #-:weblistener-aframes
               (eq (#^organism-entity-type x) #$contiguous-sequence)
               #+:weblistener-aframes
               (typep x (read-from-string "frames::bio.contiguous-sequence"))
               ))
        c-list)))))
 t)

;; string
(defbblorgtest= 
 inversion0
 (inversion-of "aaaaa")
 "ttttt")

;; gene
(defbblorgtest=
 inversion1 
 (let ((seq1 (reverse (sequence-of some-gene)))
       (seq2 (inversion-of some-gene)))
   (every 
    (lambda (x y) 
      (let ((ux (lisp:char-upcase x)) (uy (lisp:char-upcase y)))
        (case ux 
          (#\A (eql uy #\T))
          (#\T (eql uy #\A))
          (#\G (eql uy #\C))
          (#\C (eql uy #\G))
          (otherwise nil))))
    seq1 seq2
    ))
 t)

;; invalid argument
(defbblorgtest=
 inversion2 
 (let ((seq1 (reverse (sequence-of (#^gene another-protein))))
       (seq2 (inversion-of another-protein)))
   (every 
    (lambda (x y) 
      (let ((ux (lisp:char-upcase x)) (uy (lisp:char-upcase y)))
        (case ux 
          (#\A (eql uy #\T))
          (#\T (eql uy #\A))
          (#\G (eql uy #\C))
          (#\C (eql uy #\G))
          (otherwise nil))))
    seq1 seq2
    ))
 t)  

;; protein
(defbblorgerrortest= 
 e-inversion1 
 (inversion-of 23)
 error)    


;; together 1a and 1b confirm the first and last genes of circ 
;; and noncirc contigs to the left of the first gene and right of
;; the last gene = NIL in a non-circular
;; also checks protein conversion
(defbblorgtest=
 leftandright1a 
 (let ((contig (first-non-circular-contig-in-system)))
   (if contig
       (let* ((genevec (#^genes-sorted-by-position contig))
              (first (lisp:elt genevec 0))
              (last (utils::lastelem genevec))
              (lefty (gene-left-of first))
              (righty (gene-right-of last)))
         (and (null lefty) (null righty)))
     t))
 t)

;; test same edge cases on a circular contig 
(defbblorgtest=
 leftandright1b 
 (utils::vif (c btu::first-circular-contig)
             (let* ((genevec (#^genes-sorted-by-position c))
                    (first (lisp:elt genevec 0))
                    (last (utils::lastelem genevec)))
               (and 
                (eq (gene-left-of first) last)
                (eq (gene-right-of last) first)))
             t)
 t)

                     
;;; 2a-d allow for some random checks

(defbblorgtest=
 leftandright2a 
 (let ((g some-gene-with-no-overlaps-in-non-circular))
   (if g
       (let* ((contig (#^contiguous-sequence g))
              (genevec (#^genes-sorted-by-position contig))
              (pos (lisp:position g genevec))
              (prevgene (if (zerop pos) nil (lisp:elt genevec (1- pos)))))
         (eql prevgene (gene-left-of g)))
     t))
 t)

(defbblorgtest=
 leftandright2b 
 (let ((g some-gene-with-no-overlaps-in-circular))
   (if g 
       (let* ((contig (#^contiguous-sequence g))
              (genevec (#^genes-sorted-by-position contig))
              (pos (lisp:position g genevec))
              (lefty (gene-left-of g))
              (leftpos (lisp:position lefty genevec)))
         (if (zerop pos)
             (= leftpos (1- (length genevec)))
           (= (1- pos) leftpos)))
     t))
 t)

(defbblorgtest=
 leftandright2c 
 (let ((g some-gene-with-no-overlaps-in-non-circular))
   (if g
       (let* ((contig (#^contiguous-sequence g))
              (genevec (#^genes-sorted-by-position contig))
              (pos (lisp:position g genevec))
              (lastindex (1- (length genevec)))
              (nextgene 
               (if (= pos lastindex) nil (lisp:elt genevec (1+ pos)))))
         (eql nextgene (gene-right-of g)))
     t))
 t)

(defbblorgtest=
 leftandright2d 
 (let ((g some-gene-with-no-overlaps-in-circular))
   (if g 
       (let* ((contig (#^contiguous-sequence g))
              (genevec (#^genes-sorted-by-position contig))
              (pos (lisp:position g genevec))
              (righty (gene-right-of g))
              (rightpos (lisp:position righty genevec))
              (lastindex (1- (length genevec))))
         (if (= pos lastindex) 
             (= rightpos 0)
           (= (1+ pos) rightpos)))
     t))
 t)


;; check that the gene G1 left of a gene G2 has a FROM, F1, that is 
;; less than the FROM, F2, of G2.
;; lopping off the ends deals with the circ problem, so we can choose a random contig 

;; this failed with some-contig = #$S6803.pCA2.4
(defbblorgtest=
 leftandright3 
 (let* ((long-genevec (#^genes-sorted-by-position some-contig)))
   (if (> (length long-genevec) 2)
       (let ((genevec (lisp::subseq long-genevec 1 (1- (length long-genevec)))))
         (= (length genevec)
            (lisp:loop for gene across genevec
                       for count from 1
                       as leftgene = (gene-left-of gene)
                       ;; should always be true
                       while (<= (#^from leftgene) (#^from gene))
                       finally (return count)
                       )))
     t
     ))
 t)
 

;; test that GLO and GRO can take coordinates
;; coords need IN and an org exactly one chrom

;; first in non-circular contigs...

(defbblorgtest=
 leftandright4a 
 (let ((g some-gene-with-no-overlaps-in-non-circular))
   (if g
       (let* ((contig (#^contiguous-sequence g))
              (genevec (#^genes-sorted-by-position contig))
              (pos (lisp:position g genevec))
              (coord (middle-coordinate g))
              (prevgene (if (zerop pos) nil (lisp:elt genevec (1- pos)))))
         (eql prevgene (gene-left-of coord in contig)))
     t))
 t)

(defbblorgtest=
 leftandright4b 
 (let ((g some-gene-with-no-overlaps-in-non-circular))
   (if g
       (let* ((contig (#^contiguous-sequence g))
              (genevec (#^genes-sorted-by-position contig))
              (pos (lisp:position g genevec))
              (coord (middle-coordinate g))
              (lastindex (1- (length genevec)))
              (nextgene 
               (if (= pos lastindex) nil (lisp:elt genevec (1+ pos)))))
         (eql (gene-right-of coord in contig) nextgene))
     t))
 t)
       
;; ... then in circular contigs
(defbblorgtest=
 leftandright4c 
 (let ((g some-gene-with-no-overlaps-in-circular))
   (if g 
       (let* ((contig (#^contiguous-sequence g))
              (genevec (#^genes-sorted-by-position contig))
              (pos (lisp:position g genevec))
              (coord (middle-coordinate g))
              (lefty (gene-left-of coord in contig)))
         (or (and (= pos 0) (eql lefty (utils::lastelem genevec)))
             (= (1- pos) (lisp:position lefty genevec))))
     t))
 t)

(defbblorgtest=
 leftandright4d 
 (let ((g some-gene-with-no-overlaps-in-circular))
   (if g 
       (let* ((contig (#^contiguous-sequence g))
              (genevec (#^genes-sorted-by-position contig))
              (lastindex (1- (length genevec)))
              (pos (lisp:position g genevec))
              (coord (middle-coordinate g))
              (righty (gene-right-of coord in contig)))
         (or (and (= pos lastindex) (eql righty (lisp:elt genevec 0)))
             (= (1+ pos) (lisp:position righty genevec))))
     t))
 t)

;; in a noncirc, the first gene --> :f-up or :b-down = nil
;; in a noncirc, the last gene --> :f-down or :b-up = nil
(defbblorgtest=
 updown1-1 
 (let ((c (first-non-circular-contig-in-system)))
   (if (not c) 
       t 
     (let* ((genevec (#^genes-sorted-by-position c))
            (gene1 (lisp:elt genevec 0))
            (gene1dir (#^direction gene1)))
       (ecase gene1dir
         (:f 
          (null (gene-upstream-of gene1)))
         (:b
          (null (gene-downstream-of gene1)))))))
 t)

(defbblorgtest=
 updown1-2 
 (let ((c (first-non-circular-contig-in-system)))
   (if (not c)
       t
     (let* ((genevec (#^genes-sorted-by-position c))
            (genen (utils::lastelem genevec))
            (genendir (#^direction genen)))
       (ecase genendir
         (:f 
          (null (gene-downstream-of genen)))
         (:b
          (null (gene-upstream-of genen)))))))
 t)

;; in a circ, the first gene --> :f-up or :b-down = the last gene
;; in a circ, the last gene --> :f-down or :b-up = the first gene
(defbblorgtest=
 updown2-1 
 (let ((c btu::first-circular-contig))
   (if (not c)
       t
     (let* ((genevec (#^genes-sorted-by-position c))
            (gene1 (lisp:elt genevec 0))
            (gene1dir (#^direction gene1))
            (genen (utils::lastelem genevec)))
       (lisp:ecase gene1dir
         (:f (eql (gene-upstream-of gene1) genen))
         (:b (eql (gene-downstream-of gene1) genen))))))
 t)
(defbblorgtest=
 updown2-2 
 (let ((c btu::first-circular-contig))
   (if (not c)
       t
     (let* ((genevec (#^genes-sorted-by-position c))
            (lastgene (utils::lastelem genevec))
            (lastgenedir (#^direction lastgene)) 
            (gene1 (lisp:elt genevec 0)))
       (lisp:ecase lastgenedir
         (:f (eql (gene-downstream-of lastgene) gene1))
         (:b (eql (gene-upstream-of lastgene) gene1))))))
 t) 

;; random checks in non-circs
(defbblorgtest=
 updown3-1 
 (let ((g some-gene-with-no-overlaps-in-non-circular))
   (if g
       (let* ((gdir (#^direction g))
              (contig (#^contiguous-sequence g))
              (genevec (#^genes-sorted-by-position contig))
              (lgenevec (length genevec))
              (pos (lisp:position g genevec))
              (prevgene (if (zerop pos) nil (lisp:elt genevec (1- pos))))
              (nextgene 
               (if (= pos (1- lgenevec)) nil (lisp:elt genevec (1+ pos))))
              )
         (lisp:ecase gdir
           (:f (eql nextgene (gene-downstream-of g)))
           (:b (eql prevgene (gene-downstream-of g)))))
     t))
 t)

(defbblorgtest=
 updown3-2 
 (let ((g some-gene-with-no-overlaps-in-non-circular))
   (if g
       (let* ((gdir (#^direction g))
              (contig (#^contiguous-sequence g))
              (genevec (#^genes-sorted-by-position contig))
              (lgenevec (length genevec))
              (pos (lisp:position g genevec))
              (prevgene (if (zerop pos) nil (lisp:elt genevec (1- pos))))
              (nextgene 
               (if (= pos (1- lgenevec)) nil (lisp:elt genevec (1+ pos)))))
         (ecase gdir
           (:f (eql prevgene (gene-upstream-of g)))
           (:b (eql nextgene (gene-upstream-of g)))))
     t))
 t)

;; random checks in circs
(defbblorgtest=
 updown3-3 
 (let ((g some-gene-with-no-overlaps-in-circular))
   (if g 
       (let* ((gdir (#^direction g))
              (contig (#^contiguous-sequence g))
              (genevec (#^genes-sorted-by-position contig))
              (lastindex (1- (length genevec)))
              (pos (lisp:position g genevec))
              (down (gene-downstream-of g)))
         (ecase gdir
           (:f 
            (eql down (lisp:elt genevec (if (= pos lastindex) 0 (1+ pos)))))
           (:b 
            (eql down (lisp:elt genevec (if (zerop pos) lastindex (1- pos))))
            )))
     t))
 t)
          
(defbblorgtest=
 updown3-4 
 (let ((g some-gene-with-no-overlaps-in-circular))
   (if g 
       (let* ((gdir (#^direction g))
              (contig (#^contiguous-sequence g))
              (genevec (#^genes-sorted-by-position contig))
              (lastindex (1- (length genevec)))
              (pos (lisp:position g genevec))
              (up (gene-upstream-of g)))
         (ecase gdir
           (:f 
            (eql up (lisp:elt genevec (if (= pos 0) lastindex (1- pos)))))
           (:b 
            (eql up (lisp:elt genevec (if (= pos lastindex) 0 (1+ pos))))
            )))
     t))
 t)
          


;; DESC tests test GENES-DESCRIBED-BY
(defbblorgtest= 
 desc1 
 (let ((descs
        '(description best-hit-descr annotation
                      cog-description ec-description gene-name))
       (first-desc nil))
   (block exit
     (lisp:loop for desc in descs
                as fdesc = (frames::frame-fnamed desc) 
                do
                (when (frames::slotv some-gene fdesc)
                  (return-from exit (setq first-desc fdesc)))))
   (if first-desc   
       (let* ((description (frames::slotv some-gene first-desc))
              (subseq-desc
               (lisp:subseq description 0 (min (length description) 10)))
              (glist 
               (utils::flatten
                #-:sframes
                (genes-described-by subseq-desc display-off)
                #+:sframes
                (genes-described-by 
                 subseq-desc in (#^organism btu::some-gene) display-off)
                )))
         (not (null (member some-gene glist))))
     t))
 t)

(defbblorgtest=
 desc2
 #-:sframes
 (genes-described-by "xyzzyxyzzy")
 #+:sframes
 (genes-described-by "xyzzyxyzzy" in btu::some-organism)
 nil)

(defbblorgtest=
 desc3
 (let* ((g btu::some-gene)
        (desc (#^description g)))
   (if desc 
       ;; grab first third of description
       (let ((s (lisp:subseq desc 0 (floor (/ (length desc) 3)))))
         (not
          (null
           (member g (genes-described-by s in (#^organism g) display-off))
           )))
     t))
 t
 )

(defbblorgtest=
 desc4
 (let* ((g btu::some-gene)
        (desc (#^description g)))
   (if desc
       ;; grab last third of description
       (let* ((len (length desc))
              (third (floor (/ len 3)))
              (s (lisp:subseq desc (- len third))))
         (not 
          (null 
           (member 
            g (genes-described-by s in (#^contiguous-sequence g) display-off)
            ))))
     t
     ))
 t
 )

(defbblorgtest=
 desc5
 (let* ((g btu::some-gene)
        (desc (#^description g)))
   (if desc 
       (not
        (null
         (member 
          g (genes-described-by desc in (list btu::another-gene g) display-off)
          )))
     t
     ))
 t
 )

#+tofix
(defbblorgtest=
 desc6
 (let* ((g btu::some-gene)
        (desc (#^description g)))
   (if desc 
       (let ((result 
              (genes-described-by
                (list "xyzzyxyzzy" desc) in (#^organism g) display-off)))
         (and (null (first result)) (not (null (member g (second result))))))
     t))
 t
 )



(defbblorgtest= 
 is-orglist-1  
 (is-organism-list? (bio::available-organisms))
 t)

(defbblorgtest= 
 is-orglist-2 
 (is-organism-list? bio::*loaded-organisms*)
 t)


(defbblorgtest=
 common-orthologs-1 
 (progn
   (bbi::load-ortholog-tables)
   (when (> (length (bio::crossblast-organisms)) 1)
     (let* ((orglist (lisp:subseq (bio::crossblast-organisms) 0 2))
            (commons (common-orthologs-of orglist)))
       (if (null commons)
           t
         (every 
          (lambda (x) 
            #-:weblistener-aframes 
            (eql #$protein (#^organism-entity-type x))
            #+:weblistener-aframes 
            (typep x (read-from-string "frames::bio.protein")))
          commons)))))
 (unless (<= (length (bio::crossblast-organisms)) 1) t))

;; some-protein --> corresponding gene --> sequence, thanks to RFO
(defbblorgtest=
 readframe1 
 (let ((read (reading-frames-of some-corresponding-protein do-not-display)))
   (or (null read)
       (and (listp read) (every 'stringp read))))
 t)

(defbblorgtest=
 make-markov-1 
 (let* ((genevec (#^genes-sorted-by-position some-contig))
        (gvlen (length genevec))
        (ngenes (min 20 gvlen))
        (genes
         (lisp:coerce
          (lisp:subseq genevec 0 (- ngenes 1)) 'list))
        (markov (make-markov-from genes)))
   (typep markov 'utils::garray))
 t)

(defbblorgtest=
 make-markov-2 
 (let* ((genevec (#^genes-sorted-by-position some-contig))
        (gvlen (length genevec))
        (ngenes (min 20 gvlen))
        (genes 
         (lisp:coerce 
          (lisp:subseq genevec 0 (- ngenes 1)) 'list))
        (proteins (mapcan (lambda (x) (copy-list (#^proteins x))) genes))
        (markov (make-markov-from proteins)))
   (typep markov 'utils::garray))
 t)

(defbblorgtest=
 make-markov-3                
 (let* ((genevec (#^genes-sorted-by-position some-contig))
        (gvlen (length genevec))
        (ngenes (min 20 gvlen))
        (genes (lisp:coerce 
                (lisp:subseq genevec 0 (- ngenes 1))
                'list))
        (seqs (mapcar 'bio::extract-sequence genes))
        (markov (make-markov-from seqs)))
   (typep markov 'utils::garray))
 t)

(defbblorgtest=
 make-pssm-from-1  
 (block exit
   (let* ((genevec (#^genes-sorted-by-position some-contig))
          (ngenes (length genevec)))
     (when (< ngenes 2) (return-from exit t))
     (let* ((genes (lisp:coerce 
                    (lisp:subseq genevec 0 (min 3 ngenes))
                    'list))
            (seqs (mapcar 'bio::extract-sequence genes))
            (minlen (lisp:reduce 'min (mapcar 'lisp:length seqs)))
            (truncated-seqs 
             (mapcar (lambda (x) (lisp:subseq x 0 minlen)) seqs))
            (pssm (make-pssm-from truncated-seqs)))
       (typep pssm 'utils::garray))))
 t)

;; if we know its noncoding and still want it translated, use noncoding
;; if we know its noncoding and just want nil, use nowarnings

;; find a nonencoding gene,
;; make sure nil is returned when nowarnings is selected
(defbblorgtest=
 trans3
 (let ((utils::*suppress-warnings* t))
   (cond
    ((null (#^encodes-protein some-gene)) 
     (translation-of some-gene nowarnings))
    ((null (#^encodes-protein another-gene))
     (translation-of another-gene nowarnings))
    (t
     ;; if our random genes both encode
     (let ((orgs 
            (if (> (length bio::*loaded-organisms*) 20)
                (lisp:subseq bio::*loaded-organisms* 0 19)
              bio::*loaded-organisms*
              )))
       (utils::vwhen
        (genex
         (lisp::loop
           named orgloop for org in orgs do 
           (lisp::loop for g in (#^genes org) 
             unless (#^encodes-protein g)
             do (return-from orgloop g))))
        (translation-of genex nowarnings)
        )))))
 nil
 )

;; again find a nonencoding gene, but this time translate it anyway
(defbblorgtest=
 trans4
 
 (let* ((utils::*suppress-warnings* t)
        (non-encoding-gene
         (cond
          ((null (#^encodes-protein some-gene)) some-gene)
          ((null (#^encodes-protein some-gene)) another-gene)
          (t
           (let ((orgs 
                  (if (> (length bio::*loaded-organisms*) 20)
                      (lisp:subseq bio::*loaded-organisms* 0 19)
                    bio::*loaded-organisms*
                    )))
             (lisp:loop
               named orgloop for org in orgs
               do
               (lisp:loop for g in (#^genes org)
                 unless (#^encodes-protein g)
                 do (return-from orgloop g)
                 )))))))
   ;; if there are none, then we're just gonna fall through
   (when (null non-encoding-gene) (setq non-encoding-gene "AAA"))
   (stringp (translation-of non-encoding-gene noncoding)))
 t
 )
   
   

(defbblorgtest=
 encodes1
 (let ((utils::*suppress-warnings* t))
   (cond
    ((#^encodes-protein some-gene)
     (encodes-protein some-gene))
    ((#^encodes-protein another-gene)
     (encodes-protein another-gene))
    (t
     ;; if our random genes both encode
     (let ((orgs 
            (if (> (length bio::*loaded-organisms*) 20)
                (lisp:subseq bio::*loaded-organisms* 0 19)
              bio::*loaded-organisms*
              )))
       (utils::vwhen
        (genex 
         (lisp::loop
           named orgloop for org in orgs do 
           (lisp::loop for g in (#^genes org) 
             when (#^encodes-protein g)
             do (return-from orgloop g))))
        (encode-protein? genex)
        )))))
 t)

(defbblorgtest=
 encodes2
 (let ((utils::*suppress-warnings* t))
   (cond
    ((null (#^encodes-protein some-gene))
     (encodes-protein some-gene))
    ((null (#^encodes-protein another-gene))
     (encodes-protein another-gene))
    (t
     ;; if our random genes both encode
     (let ((orgs 
            (if (> (length bio::*loaded-organisms*) 20)
                (lisp:subseq bio::*loaded-organisms* 0 19)
              bio::*loaded-organisms*
              )))
       (utils::vwhen
        (genex 
         (lisp::loop
           named orgloop for org in orgs do 
           (lisp::loop for g in (#^genes org) 
             unless (#^encodes-protein g)
             do (return-from orgloop g))))
        (encode-protein? genex)
        )))))
 nil)
     
(defbblorgtest=
 protnamed1
 (let ((prot (lisp:first (#^proteins some-gene))))
   (every
    (lambda (x) (eql x prot))
    (protein-named 
      (list (#^fname some-gene) 
            some-gene 
            (lisp:car (#^proteins some-gene)
                      )))))
 t) 

(defbblorgtest= 
 context1
 (utils::vwhen 
     (genex some-gene-with-no-overlaps-in-circular)
   (let* ((contig (#^contiguous-sequence genex))
          (genevec (#^genes-sorted-by-position contig))
          (len (length genevec)))
     (cond
      ((> len 4) 
       (append 
        (context-of (lisp::aref genevec 0) :gene-width 2 no-display)
        (context-of (lisp::aref genevec 2) :gene-width 2 no-display)
        (context-of (lisp::aref genevec (1- len)) :gene-width 2 no-display)))
      (t nil)
      )))
 (utils::vwhen 
     (genex some-gene-with-no-overlaps-in-circular)
   (let* ((contig (#^contiguous-sequence genex))
          (genevec (#^genes-sorted-by-position contig))
          (len (length genevec)))
     (cond
      ((> len 4)
       (let ((last-index (1- len)))
         (list
          ;; 5 genes surrounding first gene (because circular)
          (lisp:aref genevec (1- last-index))
          (lisp:aref genevec last-index)
          (lisp::aref genevec 0)
          (lisp:aref genevec 1)
          (lisp:aref genevec 2)
          ;; 5 genes surrounding third gene 
          (lisp:aref genevec 0)
          (lisp:aref genevec 1)
          (lisp:aref genevec 2)
          (lisp:aref genevec 3)
          (lisp:aref genevec 4)
          ;; 5 genes surrounding last gene
          (lisp:aref genevec (- last-index 2))
          (lisp:aref genevec (- last-index 1))
          (lisp:aref genevec last-index)
          (lisp:aref genevec 0)
          (lisp:aref genevec 1)
          )))
      (t nil)
      ))))


(defbblorgtest= 
 context2
 (utils::vwhen 
     (genex some-gene-with-no-overlaps-in-non-circular)
   (let* ((contig (#^contiguous-sequence genex))
          (genevec (#^genes-sorted-by-position contig))
          (len (length genevec)))
     (cond
      ((> len 4) 
       (append 
        (context-of (lisp::aref genevec 0) :gene-width 2 no-display)
        (context-of (lisp::aref genevec 2) :gene-width 2 no-display)
        (context-of (lisp::aref genevec (1- len)) :gene-width 2 no-display)))
      ((= len 1)
       (context-of (lisp::aref genevec 0) :gene-width 2 no-display))
      ((= len 2)
       (append 
        (context-of (lisp::aref genevec 0) :gene-width 4 no-display)
        (context-of (lisp::aref genevec 1) :gene-width 3 no-display)))
      (t nil)
      )))
 (utils::vwhen 
     (genex some-gene-with-no-overlaps-in-non-circular)
   (let* ((contig (#^contiguous-sequence genex))
          (genevec (#^genes-sorted-by-position contig))
          (len (length genevec)))
     (cond
      ((> len 4)
       (list (lisp::aref genevec 0) (lisp:aref genevec 1) (lisp:aref genevec 2)
             (lisp:aref genevec 0) (lisp:aref genevec 1) (lisp:aref genevec 2)
             (lisp:aref genevec 3) (lisp:aref genevec 4)
             (lisp:aref genevec (- len 3)) (lisp:aref genevec (- len 2)) 
             (lisp:aref genevec (- len 1))  
             ))
      ((= len 1) (coerce genevec 'list))
      ((= len 2) (append (coerce genevec 'list) (coerce genevec 'list)))
      (t nil)
      ))))

(defbblorgtest=
 context5
 ;; only on completed organisms does context-of work.  
 ;; (it works on any contig, but if you give it an organism, 
 ;; the organism has to be completed, as per Jeff E)
 (or (not (#^completed some-organism))
     (let ((x (context-of 1 in some-organism no-display)))
       (and 
        (listp x)
        (or (member :b x) (member :f x))
        (some 'is-gene? x)
        )))
 t
 )

(defbblorgtest=
 context6
 (let ((x 
        (context-of 
         1 in (lisp:first 
               (#^contiguous-sequences btu::some-organism)) no-display)))
   (destructuring-bind (string frame1 frame2 number1 number2 dir) 
       x
     (and 
      ;; string
      (member string '("I" "P" "C" "D" "U" "N") :test 'lisp:string-equal)
      ;; frame1
      (or (null frame1)
          (and 
           (frames::isframe? frame1)
           (eql (#^organism-entity-type frame1) #$gene)))
      ;; frame2
      (or (null frame2)
          (and (frames::isframe? frame2)
               (eql (#^organism-entity-type frame2) #$gene)))
      ;; number1
      (integerp number1)
      ;; number2
      (integerp number2)
      ;; dir
      (or (eq dir :f) (eq dir :b))
      )))
 t
 )

(defbblorgtest=
 context7
 (let* ((start (#^from some-gene))
        (end (#^to some-gene))
        (contig (#^contiguous-sequence some-gene))
        (context1 (context-of start in contig no-display))
        (context2 (context-of end in contig no-display)))
   (every
    (lambda (context)
      (destructuring-bind (string frame1 frame2 number1 number2 dir) 
          context
        (and 
         ;; string
         (member string '("I" "P" "C" "D" "U" "N") :test 'lisp:string-equal)
         ;; frame1
         (or (null frame1)
             (and 
              (frames::isframe? frame1)
              (eql (#^organism-entity-type frame1) #$gene)))
         ;; frame2
         (or (null frame2)
             (and (frames::isframe? frame2)
                  (eql (#^organism-entity-type frame2) #$gene)))
         ;; number1
         (integerp number1)
         ;; number2
         (integerp number2)
         ;; dir
         (or (eq dir :f) (eq dir :b))
         )))
    (list context1 context2)
    ))
 t
 )

;; add a context test to test the very first and very last coordinate of a gene

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; on a gene
(defbblorgtest=
 header1
 (let* ((header (header-of some-gene))
        (ss (utils::string-split header))
        (h1 (lisp:first ss))
        (h3 (lisp:third ss))
        (h6 (lisp:sixth ss))
        (h8 (lisp:eighth ss))
        (h9 (lisp:ninth ss))
        (fname (#^fname some-gene))
        (org (#^organism some-gene)))
   (and 
    ;; first component of header is organism name
    (string-equal h1 (utils::subseq fname (+ 1 (lisp:position #\. fname))))
    ;; third component is organism prefix
    (string-equal (utils::s+ h3 ".") (#^organism-prefix org))
    ;; sixth is from
    (= (parse-integer h6) (#^from some-gene))
    ;; eighth is to
    (= (parse-integer h8) (#^to some-gene))
    ;; ninth is direction
    (string-equal h9 (#^direction some-gene))))
 t)

;; on a protein
(defbblorgtest=
 header2
 (let* ((header (header-of some-corresponding-protein))
        (ss (utils::string-split header))
        (h1 (lisp:first ss))
        (h3 (lisp:third ss))
        (h4 (lisp:fourth ss))
        (fname (#^fname some-corresponding-protein))
        (org (#^organism some-corresponding-protein)))
   (and (string-equal h1 (utils::subseq fname (+ 1 (lisp:position #\. fname))))
        (string-equal (utils::s+ h3 ".") (#^organism-prefix org))
        (= (parse-integer h4) (#^sequence-length some-corresponding-protein))
        ))
 t)

;; on a contig
(defbblorgtest=
 header3
 (let* ((header (header-of some-contig))
        (ss (utils::string-split header))
        (h1 (lisp:first ss))
        (h3 (lisp:third ss))
        (h4 (lisp:fourth ss))
        (fname (#^fname some-contig))
        (org (#^organism some-contig)))
   (and (string-equal h1 (utils::subseq fname (+ 1 (lisp:position #\. fname))))
        (string-equal (utils::s+ h3 ".") (#^organism-prefix org))
        (= (parse-integer h4) (#^sequence-length some-contig))
        ))
 t)

;; on an organism
(defbblorgtest=
 header4
 (let* ((header (header-of some-organism))
        (ss (utils::string-split header))
        (h1 (lisp:first ss))
        (h2 (lisp:second ss))
        )
   (and (string-equal (utils::s+ h1 ".") (#^organism-prefix some-organism))
        (= (parse-integer h2) (length-of some-organism))
        ))
 t
 )

(defbblorgtest=
 gcfrac1
 (gc-fraction-of some-gene)
 (with-bbl-form
   (let* ((seq (sequence-of some-gene))
          (len (lisp:length seq)))
     (lisp:float (/ (+ (lisp:count #\G seq :test 'lisp:char-equal)
                       (lisp:count #\C seq :test 'lisp:char-equal)) 
                    len
                    )))))

;; test LIKE with a gene
(defbblorgtest=
 randomdna1
 (length (random-dna like some-gene))
 (with-bbl-form 
   (length (sequence-of btu::some-gene))))

;; test LIKE with an organism
(defbblorgtest=
 randomdna2
 (length (random-dna like some-organism))
 (let ((x 0))
   (lisp:loop for c in (#^contiguous-sequences some-organism)
     do
     (setq x (+ x (#^sequence-length c)))
     )
   x
   ))

;; test frequencies and length
(defbblorgtest=
 randomdna3
 (let ((seq (random-dna frequencies '(.5 .3 .1 .1) length 10)))
   (and 
    (= (length seq) 10)
    (= 10
       (+ (lisp:count #\A seq) (lisp:count #\C seq)
          (lisp:count #\G seq) (lisp:count #\T seq)
          ))))
 t)

;; test LIKE with a string
(defbblorgtest=
 randomdna4
 (let ((seq (random-dna like "acgg" length 99)))
   (and
    (= (length seq) 99)
    (= 0 (lisp:count #\T seq))
    (= 99 
       (+ (lisp:count #\A seq) (lisp:count #\C seq)
          (lisp:count #\G seq) (lisp:count #\T seq)
          ))))
 t)

;; make sure LIKE and FREQUENCIES yields an error
(defbblorgerrortest=
 e-randomdna1
 (random-dna like "ACGT" frequencies '(.25 .25 .25 .25))
 error)

(defbblorgtest=
 hydrophob1
 (let ((nums (hydrophobicity-of some-gene nowarnings)))
   (every (lambda (x) (and (>= x -4.6) (<= x 4.6))) nums))
 t)

;; make sure H-O can take a list
(defbblorgtest=
 hydrophob2
 (let ((nums 
        (flatten 
         (hydrophobicity-of
          (list some-gene "ACD" some-corresponding-protein) nowarnings))))
   (every (lambda (x) (and (>= x -4.6) (<= x 4.6))) nums))
 t)

(defbblorgtest=
 hydrophob3
 (let ((nums (hydrophobicity-of "ala" sequence nowarnings)))
   (every (lambda (x) (and (>= x -4.6) (<= x 4.6))) nums))
 t)

(defbblorgtest=
 hydrophob4
 (hydrophobicity-of "ala" amino-acid nowarnings)
 1.8)

(defbblorgerrortest=
 e-hydrophob1
 (let ((g (first-non-protein-encoding-gene)))
   (if g (hydrophobicity-of g) (error "No non protein encoding genes")))
 error)

;; sanity tests for amino-acid-counts-of
(defbblorgtest=
 aacounts1
 (let ((counts (amino-acid-counts-of some-gene)))
   (every (lambda (x) (>= x 0)) counts))
 t
 )

(defbblorgtest=
 aacounts2
 (let ((counts (amino-acid-counts-of some-corresponding-protein)))
   (every (lambda (x) (>= x 0)) counts))
 t
 )

;; sanity tests for amino-acid-frequences-of
(defbblorgtest=
 aafreqsof1
 (let ((freqs (amino-acid-frequencies-of some-gene)))
   (every (lambda (x) (and (>= x 0) (<= x 1))) freqs))
 t)


(defbblorgtest=
 aafreqsof2
 (let ((freqs (amino-acid-frequencies-of some-corresponding-protein)))
   (every (lambda (x) (and (>= x 0) (<= x 1))) freqs))
 t)

(defbblorgtest=
 isgene1
 (is-gene? "h")
 nil)

(defbblorgtest=
 isgene2
 (is-gene? some-gene)
 t)

(defbblorgtest=
 isprot1
 (is-protein? "h")
 nil)

(defbblorgtest=
 isprot2
 (is-protein? some-corresponding-protein)
 t)

(defbblorgtest=
 info-about-genes-1
 (information-about-genes some-gene from to)
 (list (#^from some-gene) (#^to some-gene)))

(defbblorgtest=
 info-about-genome-1
 (information-about-genome some-organism chromosome size)
 (list (#^chromosome some-organism) (#^szdna some-organism))
 )




  
