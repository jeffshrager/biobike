;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bio; -*-

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

;;; Authors:  Jeff Elhai, JP Massar. Foo.

#|

  This is the toplevel function that gets called after LOAD-ORGANISM
  has done all the rest of its work.  It does further processing on
  the organism, arranging the genes into ordered vectors, etc.

CREATE-ORGANISM-DATA-STRUCTURES
 
  These functions are called by CREATE-ORGANISM-DATA-STRUCTURES

CREATE-GENE-FRAGMENTS
SORT-GENES-BY-POSITION-PER-CONTIG 
MAKE-NONCODING-GENE-SLOTS

  These are other functions which work with gene fragments
  or are needed to work with gene fragments.  These are exported functions.

FRAGMENT-OF-GENE
OVERLAP-BETWEEN2 
GENE-LENGTH

|#

;;; The idea is to call this function from LOAD-ORGANISM
;;; after the postload processing has been done (which is, I admit,
;;; somewhat paradoxical, but avoids having to call it from every
;;; postload file.

(defun create-organism-data-structures 
       (&key (organisms *loaded-organisms*) (verbose? t))
  (when verbose? (cformatt "Creating gene fragments..."))
  (create-gene-fragments organisms)
  (when verbose? (cformatt "Creating sorted gene and fragment vectors"))
  (sort-genes-by-position-per-contig organisms)
  (when verbose? (cformatt "Creating list of non-coding genes"))
  (make-noncoding-gene-slots organisms)
  )


(defun fragment-frame-name (gene fragment-number)
  (formatn "~A-~D" (fname gene) fragment-number))

;;; This should be done for each individual organism and be done at
;;; LOAD-ORGANISM time.

(defun maybe-transform-architecture-slot (gene)
  (let ((arch (slotv gene #$architecture)))
    (setf (slotv gene #$architecture)
          (cond
           ((stringp arch) (string-to-list arch))
           ((listp arch) arch)
           (t
            (error 
             "Architecture value, ~A, is neither a string nor a list!"
             arch
             ))))))
      

(defun create-gene-fragments
       (&optional (organisms *loaded-organisms*))
  #.(one-string-nl
     "For every gene in each ORGANISM, if it has an ARCHITECTURE slot,"
     "process that slot, creating FRAGMENT frames for each piece of"
     "architecture defined in the architecture slot, and add this list"
     "of fragment frames to the gene's #$fragments slot, and each new"
     "fragment frame to the gene's organism's #$fragments slot."
     "In addition, each fragment frame points back at the gene it was"
     "derived from."
     "Further, if the architecture information specified that the gene"
     "starts and/or ends at a different place than what is specified in the"
     "original annotation's FROM and TO, the #$FROM and/or #$TO slots of the"
     "gene are modified to reflect the architecture information, while the"
     "orginal FROM and/or TO information is stored into #$RECORDED-FROM and/or"
     "#$RECORDED-TO slots.")
  (loop for organism in organisms do
        (setf (slotv organism #$fragments) nil)
        (loop for gene in (genes-of-organism organism)
              AS arch-list = (maybe-transform-architecture-slot gene)
              when arch-list
              do 
              (setf (#^fragments gene) nil)
              (let* ((far-left (first (first arch-list)))
                     (far-right (second (lastelem arch-list)))
                     (from (slotv gene #$from))
                     (to (slotv gene #$to))
                     (organism (slotv gene #$organism)))
                (unless (= from far-left)
                  (error 
                   "In gene ~A, #$From not same as first fragment's start."
                   gene
                   ))
                (unless (= to far-right)
                  (error
                   "In gene ~A, #$To not same as last fragment's end." 
                   gene
                   ))
                (loop for pair in arch-list
                      for part-index fixnum from 1 to (length arch-list)
                      as left = (first pair)
                      as right = (second pair)
                      as name = (fragment-frame-name gene part-index)
                      as fragment-frame = (frame-fnamed name t)
                      do 
                      (push fragment-frame (slotv organism #$fragments))
                      (push fragment-frame (slotv gene #$fragments))
                      (def-frame 
                       fragment-frame
                       #$organism-entity-type #$gene-part
                       #$from left
                       #$to right
                       #$gene gene
                       )))
              (setf (#^fragments gene) (nreverse (#^fragments gene)))
              )
        (setf (slotv organism #$fragments) 
              (nreverse (slotv organism #$fragments))
              )))



(defun fragment-indicator-to-fragment-index (indicator gene)
  (cond
   ((integerp indicator) indicator)
   ((symbolp indicator)
    (let ((key (keywordize indicator)))
      (cond
       ((equal key :first) 1)
       ((equal key :last) (length (slotv gene #$architecture-list)))
       (t (error "FRAGMENT-OF-GENE: Invalid PART DESIGNATOR: ~A" 
                 indicator
                 )))))
   (t (error "FRAGMENT-OF-GENE: Invalid PART DESIGNATOR: ~A" 
             indicator
             ))))


(defun fragment-of-gene 
       (gene 
        given-part 
        &key
        (if-no-architecture? nil)
        (if-no-such-fragment? :error)
        &aux n-fragments index part-name part-frame)
  #.(one-string-nl
     "(FRAGMENT-OF-GENE gene given-part)"
     "Returns the frame designating a gene fragment, given the gene frame"
     "and the fragment desired, which may be either FIRST, LAST or an integer"
     "which indexes the desired fragment. The index is 1-based, not 0-based."
     "E.g., (FRAGMENT-OF-GENE <gene> 1) returns the first fragment."
     "-- If the gene has no architecture:"
     "   -- By default NIL is returned."
     "   -- If IF-NO-ARCHITECTURE? is :ERROR, an error is signalled."
     "   -- otherwise the value of IF-NO-ARCHITECTURE? is returned."
     "-- If GIVEN-PART indexes a non-existent fragment:"
     "   -- By default, an error is signalled."
     "   -- otherwise, the value of IF-NO-SUCH-FRAGMENT? is returned.")
  (block exit
    (let ((architecture-list (slotv gene #$architecture))
          (fragment-list (slotv gene #$fragments)))
      (when (null architecture-list)
        (case if-no-architecture?
          (:error (error "No architecture exists for gene ~A" gene))
          (otherwise (return-from exit if-no-architecture?))
          ))
      (when (null fragment-list)
        (case if-no-such-fragment?
          (:error (error "No #$fragments slot for gene ~A" gene))
          (otherwise (return-from exit if-no-such-fragment?))
          ))
      (unless (= (length architecture-list) (length fragment-list))
        (ierror "For gene ~A, architecture list not same length as fragments"
                gene))
      (setq n-fragments (length fragment-list))
      (setq index (fragment-indicator-to-fragment-index given-part gene))
      (setq part-name (fragment-frame-name gene index))
      (setq part-frame (frame-fnamed part-name))
      (cond
       ((or (< index 1) (> index n-fragments))
        (cond
         (part-frame
          (error "Ruh roh. Fragment ~D for gene ~D should NOT exist but does!"
                 index gene))
         (t
          (case if-no-such-fragment?
            (:error (error "No fragment ~A exists for gene ~A" part-name gene))
            (otherwise (return-from exit if-no-such-fragment?))
            ))))
       (t
        (cond
         ((null part-frame)
          (error "Ruh roh. Fragment ~D for gene ~D should exist but does not!"
                 index gene))
         ((not (member part-frame fragment-list))
          (ierror 
           (one-string
            "Ruh roh. Fragment ~A for gene ~D is not in fragment list"
            "yet it exists!!"
            )))
         (t part-frame)
         ))))))



; =============== SORT-GENES-BY-POSITION-PER-CONTIG ===============

;;; Modified from original in lines related to fragments
;;; Creates #$Fragments-sorted-by-position in #$CONTIGUOUS-SEQUENCE frames
;;;   allowing other functions to handle split genes
 
(defun sort-genes-by-position-per-contig 

       (&optional (organisms *loaded-organisms*))

  #.(one-string-nl
     "For every organism in ORGANISMS, for every Contiguous Sequence frame"
     "of that organism, creates a vector of gene frames in the"
     "#$Genes-sorted-by-position slot of the contig, and another vector"
     "consisting of gene frames without fragments and the fragment frames of"
     "genes with fragments. These vectors of frames are sorted by their"
     "#$FROM slot, i.e., by their starting position on the contig.")
  
  (dolist (organism organisms)

    ;; Clear out the sorted list on each contig just in case

    (let ((organism-contigs (slotv organism #$Contiguous-Sequences)))
      
      (loop for contig in organism-contigs do
            (setf (slotv contig #$Genes-Sorted-by-Position) nil)
            (setf (slotv contig #$Fragments-sorted-by-position) nil))
    
      ;; Assign each gene to the #$Genes-Sorted-By-Position slot
      ;; of the contig the gene is part of (the genes aren't sorted yet...)
      (loop for gene in (genes-of-organism organism) do
            (push gene 
                  (slotv 
                   (Slotv gene #$Contiguous-Sequence)
                   #$Genes-Sorted-By-Position
                   )))

      ;; Copy the gene list of each contig into
      ;; #$Fragments-sorted-by-position so we can later create a sorted
      ;; genes and fragments vector (consisting of genes with no fragments
      ;; and the fragments of genes with fragments).
      (loop for contig in organism-contigs do
            (setf (slotv contig #$Fragments-Sorted-by-position)
                  (copy-list (slotv contig #$Genes-Sorted-by-Position))))

      ;; Sort the genes based on their FROM coordinate along the contig.
      ;; Wrapped genes will end up at the high end of the sort.
      (loop for contig in organism-contigs do
            (setf (slotv contig #$Genes-Sorted-by-Position)
                  (coerce
                   (sort (slotv contig #$Genes-Sorted-by-Position)
                         '< :key (lambda (gene) (slotv gene #$from)))
                   'simple-vector
                   )))
   
      ;; Create a list combining non-fragmented genes (i.e.,
      ;; 'degenerate' fragments) and real fragments,
      ;; and sort that list according to the FROM coordinate.
      (loop for contig in organism-contigs do
            ;; Divide the genes for this contig into two lists,
            ;; those with fragments and those without.
            (multiple-value-bind 
                (genes-with-fragments genes-without-fragments)
                (separate-into-lists 
                 (slotv contig #$Fragments-sorted-by-position)
                 (lambda (gene) (slotv gene #$Fragments)))
              ;; Create a flattened list of all fragments
              (let ((all-fragments
                     (mapcan
                      (lambda (gene) (copy-list (slotv gene #$Fragments)))
                      genes-with-fragments
                      )))
                (setf (slotv contig #$Fragments-sorted-by-position)
                      (coerce
                       (sort (nconc all-fragments genes-without-fragments)
                             '< :key (lambda (x) (slotv x #$From)))
                       'simple-vector
                       )))))
                           

      )))


; =============== OVERLAP-BETWEEN2 ===============
;;; Differs from original in:
;;;   1. Creates new keyword CONTIG allowing user to specify
;;;        contig when both inputs are (FROM TO) lists
;;;   2. Made LISTS-CAN-BE-CIRCULAR? a keyword (used to be OPTIONAL)
;;; These changes allow user to find overlap between segments
;;;   of DNA (e.g. segments of split genes)

(defun overlap-between2 (input1 input2 &KEY contig (lists-can-be-circular? nil))

  #.(one-string-nl
     "Returns a list (FROM TO) which defines the overlap between"
     "INPUT1 and INPUT2, which may either be genes or (FROM TO) lists."
     "If no overlap exists NIL is returned."
     "FROM and TO must both be positive integers."
     "If double overlap exists :double-overlap is returned as the first"
     "value and two (FROM TO) lists are returned as the 2nd and 3rd values."
     "If two genes are provided and they are from different contigs then"
     "NIL is returned."
     "If both inputs are (FROM TO) lists, then a contig must be specified"
     "through the CONTIG key")

  (block exit

    (labels 
        ((is-a-gene? (input)
           (subtypep (type-of input) 'aframes::bio.gene))
         (Input-to-coordinate-list (input)
           (cond
            ((is-a-gene? input) 
             (list (#^from input) (#^to input)))
            ((listp input) input)
            ((integerp input) (list input input))
            (t (error "Bad input, ~A, to function OVERLAP-BETWEEN" input))
            )))

      (let* ((sequence1 (input-to-coordinate-list input1))
             (from1 (first sequence1))
             (to1 (second sequence1))
             (sequence2 (input-to-coordinate-list input2))
             (from2 (first sequence2))
             (to2 (second sequence2))
             (contig
              (cond
               ((and (is-a-gene? input1) (is-a-gene? input2))
                (let ((c1 (#^contiguous-sequence input1))
                      (c2 (#^contiguous-sequence input2)))
                  (if (eq c1 c2) 
                      c1 
                    (return-from exit nil))))
               ((is-a-gene? input1) (#^contiguous-sequence input1))
               ((is-a-gene? input2) (#^contiguous-sequence input2))
               (contig contig)
               (t nil)))
             (circular?
              (or lists-can-be-circular? (and contig (#^circular contig))))
             wrap1?
             wrap2?
             )

        ;; Canonicalize order so FROM1 <= FROM2
        (when (> from1 from2)
          (psetq from1 from2 to1 to2 from2 from1 to2 to1))

        (setq wrap1? (> from1 to1) wrap2? (> from2 to2))

        (cond
         ((and (not wrap1?) (not wrap2?))
          ;; no wrapping -- no overlap or simple overlap.
          (if (< to1 from2) 
              nil 
            (list (max from1 from2) (min to1 to2))))
         ((not circular?)
          (error
           (one-string
            "You provided wrapped coordinates for one or both inputs but "
            "either both coordinates were provided as lists or the contig "
            "designated by the genes provided was not circular.")))
         (t 
          (cond
           ;; both wrap.
           ((and wrap1? wrap2?) 
            (list from2 (min to1 to2)))
           ;; only the one with the smaller from1 wraps.
           (wrap1? (list from2 to2))
           ;; only the one with the bigger from1 wraps
           (wrap2?
            (cond
             ((and (<= from2 to1) (>= to2 from1))
              (values :double-overlap 
                      (list from1 to2) (list from2 to1)))
             ((>= to2 from1) (list from1 (min to1 to2)))
             ((>= to1 from2) (list from2 to1))
             (t nil)))
           ;; neither wrap
           (t (ierror "this case should already have been covered.")
              (if (<= from2 to1) (list from2 to1) nil)))))
        ))))


(defun gene-length 
       (gene &key (ignore-architecture? nil) (if-not-known? :error))
  #.(one-string-nl
     "(GENE-LENGTH GENE &KEY (IGNORE-ARCHITECTURE? NIL) (IF-NOT-KNOWN? :ERROR))"
     "Returns the number of base pairs in gene sequence."
     "-- By default, if the gene has architecture, the spaces between the"
     "fragments defined by the architecture are not counted."
     "-- If IGNORE-ARCHITECTURE? is T, then those spaces are counted."
     "-- If the length cannot be determined because the gene is insufficiently"
     "annotated, then an error is signalled by default. If IF-NOT-KNOWN? is"
     "some other value than :error this value is returned in this case"
     "rather than signalling an error.")
     (let* ((contig (#^Contiguous-Sequence gene))
            (circular? (#^Circular contig))
            (arch (#^Architecture gene))
            (from (#^From gene))
            (to (#^To gene))
            )
       (if (or (#^Start-Unknown gene) (#^End-Unknown gene) (= -1 from))
           (cond
            ((eq :error if-not-known?)
             (error 
              (one-string
               "Gene ~A's sequence is not defined sufficiently"
               "to compute its length")
              gene))
            (t if-not-known?)
            )
         (if (or (not circular?) (<= from to))
             (if (or ignore-architecture? (null arch))
                 (1+ (- (#^To gene) (#^From gene)))
               (loop for fragment in arch sum
                     (1+ (- (second fragment) (first fragment)))
                     ))
           ;; This is sort of cheating, but almost no genes are wrapped
           ;; so this makes life a lot easier at theoretically very small
           ;; amortized cost.
           (length 
            (extract-gene-sequence 
             gene :ignore-architecture? ignore-architecture?)
            )))))
                

; =============== MAKE-NONCODING-GENE-SLOTS ===============
;;; Adds field of noncoding genes to each genome frame
;;; Field makes it much easier to use noncoding genes as an
;;;   aggregate (i.e., genes like those determining tRNAs)
;;; I currently run it by hand after every reboot

(defun make-noncoding-gene-slots (&optional (organisms *loaded-organisms*))
  (loop for organism in organisms do
        (setf (slotv organism #$Noncoding-genes)
              (loop for gene in (genes-of-organism organism)
                    when (not (slotv gene #$Encodes-protein))
                    collect gene
                    ))))

