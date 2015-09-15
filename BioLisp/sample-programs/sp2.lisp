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

2. Find region upstream from genes that may regulate differentiation
Cyanobacteria of the genus Nostoc and Anabaena were capable of
multicellular differentiation long before any animal walked the earth.
Their critical problem of development is the same as ours: how to
determine when and where critical genes are expressed to ensure that a
cell properly differentiates at the right time and the right place. No
mechanism is known to explain the regulated expression of genes during
cyanobacterial differentiation of heterocysts, but here's a BioLingua
program that would address this problem. The user could execute it as
a single program or line-by-line (or as a less legible, but more
concise, combined program.)

  (ASSIGN het-organisms (Ana7120 Npun Avar))
  (ASSIGN het-genes ("nifH" "hepA" "hetR" "nifB"))
  (LOOP FOR organism IN het-organisms
        DO (LOOP FOR gene IN (Get-genes organism het-genes)
                 COLLECT (Sequence-upstream-of gene)))
  (ASSIGN candidate-regions *) ;(* gives the result of the last calculation.)
  (MEME candidate-regions :OPTION "zoops")

Translation: Go through every available heterocyst-producing organism
and within those organisms every gene in my list of genes I know to be
developmentally regulated. Take the upstream intergenic region of
each. Take the collection of upstream regions and call it
"candidate-regions". Give these to MEME with the specification to
permit Zero Or One hit Per Sequence.  MEME is a widely used program
that looks for statistically over-represented segments in a collection
of sequences.  

(defbio align-upstream-regions-of-heterocyst-genes-with-meme
        ((het-orgs (:set :organism))
         (meme-options :string))
  (call-meme-tool       
   (loop for organism in het-orgs
         as het-genes = (get-genes organism 'is-a-heterocyst-gene) nconc
         (remove-if 
          'null
          (loop for gene in het-genes collect (sequence-upstream-of gene))
          ))
   meme-options
   ))

(defun align-upstream-regions-of-heterocyst-genes-with-meme
       (het-orgs meme-options)
  (setq het-orgs (loop for orgd in het-orgs collect
                       (canonicalize-organism-designator orgd)))
  (setq meme-options (string meme-options))
  (call-meme-tool       
   (loop for organism in het-orgs
         as het-genes = (get-genes organism 'is-a-heterocyst-gene) nconc
         (remove-if 
          'null
          (loop for gene in het-genes collect (sequence-upstream-of gene))
          ))
   meme-options
   ))

;;; See documentation for JP's version of these functions
;;; in the documentation strings of the functions.
                 
GET-GENES ([FROM|IN] organism-frame gene-descriptors ):
Returns gene-frames of genes in organism as described by descriptors.
XFMacro looks for descriptors through an ordered sequence of searches
through different slots of gene-frames. Gene-descriptors (either a
list or a single value) may precede organism-frame in argument list so
long as FROM or IN appears before the organism-frame.

SEQUENCE-UPSTREAM-OF (gene-frame [number]): Returns sequence frame of
given gene, extending back to the previous gene unless a number is
given as the second argument. In that case, the upstream sequence of a
length given by the number is returned.  

MEME (list-of-sequences):
Returns output from the program Meme in some appropriate format.

|#

(defun get-genes (frame &optional (test 'identity))
  #.(one-string-nl
     "Returns all the genes of the #$Genes slot of FRAME that satisfy "
     "the TEST function.  By default all the genes of FRAME are returned.")
  (cond 
   ((or (eq test 'identity) (eq test #'identity))
    (slotv frame #$Genes))
   (t
    (loop for gene in (slotv frame #$Genes) 
          when (funcall test gene) collect gene
          ))))

(defparameter *heterocyst-gene-labels* '("nifH" "hepA" "hetR" "nifB"))

(defun is-a-heterocyst-gene (gene)
  (member (slotv gene #$Gene-Labels) *heterocyst-gene-labels* 
          :test 'string-equal))


;;;; CODE TO DEAL WITH UPSTREAM AND DOWNSTREAM REGIONS.
                       
;;; Conceptually a contig runs from left to right, with lower positions
;;; more to the left.

;;; A critical observation for this code is that on any one contig there
;;; can be at most one wrapped gene.

;;; The genes are sorted (using the genes-sorted-by-position-key-function,
;;; defined below) such that if any part of the gene occupies the
;;; leftmost part of the contig it is the very first gene in the sort order.

(defun reversed-gene-direction (dir)
  (ecase (canonicalize-direction dir) (:f :b) (:b :f)))
(defun gene-is-wrapped? (gene) 
  (< (slotv gene #$To) (slotv gene #$From)))
(defun contig-is-circular? (gene) 
  (slotv (slotv gene #$Contiguous-Sequence) #$Circular))

(defmacro with-gene-direction-temporarily-reversed ((gene) &body body)
  (let ((dirsym (gensym "DIR-"))
        (genesym (gensym "GENE-")))
    `(let* ((,genesym ,gene)
            (,dirsym (slotv ,genesym #$Direction)))
       (setf (slotv ,genesym #$Direction) (reversed-gene-direction ,dirsym))
       (unwind-protect
           (progn ,@body)
         (setf (slotv gene #$Direction) ,dirsym)
         ))))

(defun sequence-upstream-of (gene &optional (size nil))
  #.(one-string-nl
     "Find the upstream region of GENE.  Returns a list of the GENE "
     "and the upstream sequence, or NIL if no such region exists. "
     "The upstream region is conceptually to the left of GENE if GENE"
     "goes FORWARD, and to the right of GENE if GENE goes BACKWARDS."
     "If SIZE is provided, that many base pairs upstream are returned"
     "in the sequence, or as many base pairs as exist before hitting the"
     "edge of the sequence, or NIL if no such base pairs exist"
     "If SIZE is NIL, the intergenic region between"
     "GENE and the gene immediately prior or subsequent to GENE is returned.")
  (cond
   ((integerp size) 
    (let ((result (sequence-upstream-of-by-size gene size)))
      (and result (list gene result))))
   ((null size)
    (maybe-sort-genes-by-position-per-contig (slotv gene #$Organism))
    (let* ((contig (slotv gene #$Contiguous-Sequence))
           (sorted-genes (slotv contig #$Genes-Sorted-by-Position))
           (gene-index 
            (binsearch 
             gene sorted-genes
             :key 'genes-sorted-by-position-key-function
             ))
           (result (intergenic-upstream-region gene gene-index sorted-genes))
           )
      (and result (list gene result))
      ))
   (t (error "Invalid SIZE argument: ~A" size))
   ))

(defun sequence-downstream-of (gene &optional (size nil))
  "Same as SEQUENCE-UPSTREAM-OF, but logically reversed."
  (with-gene-direction-temporarily-reversed (gene)
    (ncomplement-base-pairs (sequence-upstream-of gene size) :reverse? nil)))

(defun sequence-upstream-of-by-size (gene size)
  #.(one-string-nl
     "Return the SIZE base-pairs upstream of GENE, or as many as possible"
     "before running into the edge of GENE's contig, or NIL is the gene is"
     "on the contig's edge in the upstream direction.")
  (let* ((contig (slotv gene #$Contiguous-Sequence))
         (contig-size (slotv contig #$Sequence-Length))
         (from (slotv gene #$From))
         (to (slotv gene #$To))
         (dir (slotv gene #$Direction)))
    (when (plusp size)
      (flet ((ecs-or-nil (from to dir)
               (and (<= from to) 
                    (extract-contig-sequence contig from to dir))))
        (if (gene-is-wrapped? gene)
            (ecase dir
              (:f (extract-contig-sequence contig (- from size) (1- from) :f))
              (:b (extract-contig-sequence contig (1+ to) (+ to size) :b)))
          ;; Limit region to edges of contig if not wrapped.
          (ecase dir
            (:f (ecs-or-nil (max 1 (- from size)) (1- from) :f))
            (:b (ecs-or-nil (1+ to) (max contig-size (+ to size)) :b))
            ))))))


(defun maybe-sort-genes-by-position-per-contig (organism)
  ;; Unless they're already sorted...
  (unless (slotv organism #$Genes-Sorted-by-Position?)
    ;; Clear out the sorted list on each contig just in case
    (let ((organism-contigs (slotv organism #$Contiguous-Sequences)))
      (loop for contig in organism-contigs do
            (setf (slotv contig #$Genes-Sorted-by-Position) nil))
      ;; Assign each gene to its contig list
      (loop for gene in (genes-of-organism organism) do
            (push gene 
                  (slotv 
                   (slotv gene #$Contiguous-Sequence)
                   #$Genes-Sorted-By-Position
                   )))
      ;; Sort each contig list
      ;; Note that it is irrelevant as to whether a gene is backwards
      ;; or forwards or whether we use FROM or TO as the key, the 
      ;; resulting order will be the same modulo the single wrapped
      ;; gene (if any) on a contig that wraps.  We choose to use (MIN FROM TO)
      ;; as the sort key, so that if any part of a gene is at the
      ;; leftmost edge of the contig it will be first in the sort order.
      (loop for contig in organism-contigs do
            (setf (slotv contig #$Genes-Sorted-by-Position)
                  (coerce
                   (sort
                    (slotv contig #$Genes-Sorted-by-Position)
                    '<
                    :key 'genes-sorted-by-position-key-function)
                   'simple-vector
                   ))))
    (setf (slotv organism #$Genes-Sorted-by-Position?) t)
    ))
                            
(defun genes-sorted-by-position-key-function (gene)
  (min (slotv gene #$From) (slotv gene #$To)))


(defun gene-left-or-right-of (gene gene-index sorted-genes left-or-right)
  "Returns gene to :left or :right of GENE, or NIL if there is no such gene."
  (declare (fixnum gene-index) (vector sorted-genes))
  (let* ((len (length sorted-genes))
         (last-index (the fixnum (1- len))))
    (declare (fixnum len last-index))
    (cond
     ((= len 1) (ierror "There is only one gene!"))
     ((= len 0) (ierror "There are no genes!"))
     ((and (plusp gene-index) (/= gene-index last-index))
      (ecase left-or-right
        (:left (aref sorted-genes (the fixnum (1- gene-index))))
        (:right (aref sorted-genes (the fixnum (1+ gene-index))))
        ))
     ((gene-is-wrapped? gene)
      (unless (zerop gene-index)
        (ierror "Wrapped gene is not at index 0 in sorted order!"))
      (ecase left-or-right
        (:left (aref sorted-genes last-index))
        (:right (aref sorted-genes 1))))
     ((zerop gene-index)
      (ecase left-or-right
        (:left (and (contig-is-circular? gene) (aref sorted-genes last-index)))
        (:right (aref sorted-genes 1))
        ))
     ((= gene-index last-index)
      (ecase left-or-right
        (:left (aref sorted-genes (the fixnum (1+ gene-index))))
        (:right (and (contig-is-circular? gene) (aref sorted-genes 0)))
        ))
     (t (ierror "Impossible cond condition!"))
     )))

(defun gene-to-the-right-of (gene gene-index sorted-genes)
  (gene-left-or-right-of gene gene-index sorted-genes :right))
(defun gene-to-the-left-of (gene gene-index sorted-genes)
  (gene-left-or-right-of gene gene-index sorted-genes :left))

(defun intergenic-upstream-region (gene gene-index sorted-genes)

  #.(one-string-nl
     "Returns a sequence from the GENE's Contiguous Sequence which "
     "is adjacent to GENE, and extends to the next gene or the end of "
     "the contig, or it returns NIL."
     "If the GENE is FORWARD, the upstream region extends conceptually"
     "to the left of the GENE, and if BACKWARD, to the right."
     "The function returns NIL if: "
     "  -- The sequence is not circular and the gene is FORWARD and it"
     "     abuts the beginning of the contig."
     "  -- The sequence is not circular and the gene is BACKWARD and it"
     "     abuts the end of the contig."
     "  -- The GENE abuts the next gene is the appropriate direction."
     "If GENE is BACKWARD, then the sequence returned is reversed and "
     "complemented base-pair-wise."
     ""
     "DIAGRAMS OF SOME POSSIBLE CONFIGURATIONS "
     "AND RESULTING INTEGENIC UPSTREAM REGIONS(***)"
     ""
     "Simplest case for forward gene:"
     " ufrom    uto  from    to"
     "---|-------|-----|-----|-------------------------------------"
     "            *****"
     ""
     "Simplest case for backward gene:"
     " from      to  ufrom  uto"
     "---|-------|-----|-----|-------------------------------------"
     "            *****"
     ""
     "Forward gene where intergenic region is wrapped:"
     " from      to                                    ufrom    uto"
     "---|-------|---------------------------------------|-------|--"
     "***                                                         **"
     ""
     "Case where upstream gene is wrapped (uto < ufrom), gene is forward:"
     "  uto     from  to                                       ufrom"
     "---|-------|-----|-----------------------------------------|--"
     "    *******"
     ""
     "Case where upstream gene is wrapped (uto < ufrom), gene is backward:"
     "  uto                                     from      to   ufrom"
     "---|----------------------------------------|--------|-----|--"
     "                                                      *****"
     ""
     "Case where gene is wrapped (to < from) and gene is forward:"
     "  to     ufrom  uto                                       from"
     "---|-------|-----|-----------------------------------------|--"
     "                  *****************************************"
     ""
     "Case where gene is wrapped (to < from) and gene is backward:"
     "  to     ufrom  uto                                       from"
     "---|-------|-----|-----------------------------------------|--"
     "    *******"
     ""
     )

  (let* ((from (slotv gene #$From))
         (to (slotv gene #$To))
         (dir (slotv gene #$Direction))
         (contig (slotv gene #$Contiguous-Sequence))
         (contig-size (slotv contig #$Sequence-Length))
         (which-way (ecase dir (:f :left) (:b :right)))
         (upstream-gene
          (ecase which-way
            (:left (gene-to-the-left-of gene gene-index sorted-genes))
            (:right (gene-to-the-right-of gene gene-index sorted-genes))
            ))
         (result nil))

    (flet ((ecs-safe (contig from to)
             (and (<= from to) (extract-contig-sequence contig from to :f)))
           (ecs-wrapped-safe (contig from to)
             (and (>= from to) (extract-contig-sequence contig from to :f))))
      
      (setq
       result
       (cond
        ((null upstream-gene)
         (when (gene-is-wrapped? gene) 
           (ierror "No upstream gene but gene is wrapped!"))
         (ecase which-way
           ;; From the left edge of the contig to the beginning of the gene
           (:left (ecs-safe contig 1 (1- from)))
           ;; From the end of the gene to the end of the contig
           (:right (ecs-safe contig (1+ to) contig-size))))
        (t
         (let ((gene-wrapped? (gene-is-wrapped? gene))
               (upstream-gene-wrapped? (gene-is-wrapped? upstream-gene))
               (ufrom (slotv upstream-gene #$From))
               (uto (slotv upstream-gene #$To))
               )
           (cond 
            ((and (not gene-wrapped?) (not upstream-gene-wrapped?))
             (ecase which-way
               ;; From the end of the upgene to the start of the gene
               (:left 
                (if (> ufrom from)
                    ;; The intergenic region itself is wrapped
                    (ecs-wrapped-safe contig (1+ uto) (1- from))
                  (ecs-safe contig (1+ uto) (1- from))))
               ;; From the end of the gene to the start of the upgene
               (:right 
                (if (> from ufrom)
                    ;; The intergenic region itself is wrapped.
                    (ecs-wrapped-safe contig (1+ to) (1- ufrom))
                  (ecs-safe contig (1+ to) (1- ufrom))))))
            (gene-wrapped?
             (ecase which-way
               ;; From the end of upgene to the edge of gene nearest the
               ;; end of the contig
               (:left (ecs-safe contig (1+ uto) (1- from)))
               ;; From the edge of gene nearest the contig start
               ;; to the start of upgene
               (:right (ecs-safe contig (1+ to) (1- ufrom)))
               ))
            (upstream-gene-wrapped?
             (ecase which-way
               ;; From the edge of upgene nearest the contig start 
               ;; to the start of gene
               (:left (ecs-safe contig (1+ uto) (1- from)))
               ;; From the end of gene to the edge of upgene nearest
               ;; the end of the contig.
               (:right (ecs-safe contig (1+ to) (1- ufrom)))
               ))
            (t (ierror "They both can't be wrapped!!"))
            )))))

      (when (and result (eq dir :b))
        (setq result (ncomplement-base-pairs result)))

      result

      )))


;;; CODE THAT DEALS WITH CALLING MEME

(defun sequences-to-fasta-file (label-sequence-list fasta-file)
  (with-open-file (p fasta-file :direction :output :if-exists :supersede)
    (loop for (label sequence) in label-sequence-list do
          (format p ">~A~%" (string label))
          (format p "~A~%" sequence)
          )))


(defun new-biowebtmp-file 
       (&key (name "temp") (type "html") (user-prefix t) (if-exists nil))
  "See documentation  for MAKE-NEW-TEMP-FILE-PATH"
  (make-new-temp-file-path "biowebtmp:" user-prefix name type if-exists))
    
(defun new-biotmp-file
       (&key (name "temp") (type "html") (user-prefix t) (if-exists nil))
  "See documentation  for MAKE-NEW-TEMP-FILE-PATH"
  (make-new-temp-file-path "biotmp:" user-prefix name type if-exists))

(defun call-meme-tool (fasta-or-seqs meme-options)
  #.(one-string-nl
     "Calls the Unix MEME program with an input file SEQS and command line"
     "arguments MEME-OPTIONS.  Returns an object which contains the filepath"
     "of the output file, and which when displayed in the Bioweblistener"
     "will display a hyperlink to that file whose title is 'MEME results'."
     "If SEQS is a list it must be in the form "
     "((<gene1> <sequence1>) (<gene2> <sequence2>) ...)"
     "The sequence data is written out to a temporary FASTA format file"
     "which is then given to the MEME program as input."
     )
    (cond
     ((stringp fasta-or-seqs)
      (let ((full-output-path (new-biowebtmp-file :name "meme"))
            (full-input-path (merge-pathnames fasta-or-seqs)))
        (cl-user::run-shell-command 
         ;; meme <filename> <options> > <tempfile>
         (one-string 
          "meme " full-input-path " " 
          meme-options " > " full-output-path))
        (wb::make-biowebtmp-file full-output-path "MEME results")
        ))
      ((pathnamep fasta-or-seqs) 
       (call-meme-tool (namestring fasta-or-seqs) meme-options))
      ((listp fasta-or-seqs)
       (let ((temp-fasta-file (new-biotmp-file :type "fasta")))
         (unwind-protect
             (progn
               (sequences-to-fasta-file fasta-or-seqs temp-fasta-file)
               (call-meme-tool temp-fasta-file meme-options))
           (when (probe-file temp-fasta-file)
             (delete-file temp-fasta-file)
             ))))
      (t (error "Invalid SEQS argument, ~A, to CALL-MEME" fasta-or-seqs))
      ))



