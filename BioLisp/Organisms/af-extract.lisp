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

;;; Author:  JP Massar

;;;; INTERFACE: 

;;;; EXTRACT-CONTIG-SEQUENCE
;;;; EXTRACT-GENE-SEQUENCE
;;;; EXTRACT-PROTEIN-SEQUENCE
;;;; EXTRACT-SEQUENCE
;;;; EXTRACTION-TEST

(defvar *current-extraction-organism* nil)
(defvar *current-extraction-file* nil)

;;; THESE TWO DEFINITIONS NEED TO STAY IN SYNC!

(defun xtract-contig-sequence 
       (contig from to direction
               &key
               (complement-backwards? t)
               (if-wrapped-but-not-circular :error) 
               (safely? t)
               &allow-other-keys)
  (extract-contig-sequence 
   contig from to direction
   :complement-backwards? complement-backwards?
   :if-wrapped-but-not-circular if-wrapped-but-not-circular
   :safely? safely?))

(defun extract-contig-sequence 
       (contig from to direction 
               &key
               (complement-backwards? t)
               (if-wrapped-but-not-circular :error) 
               (safely? t))
  #.(one-string-nl
     "Extract a subsequence from a contig using standard FROM, TO and "
     "DIRECTION semantics, except that neither FROM nor TO may be -1. "
     "If DIRECTION is B (read backwards) and COMPLEMENT-BACKWARDS? is T "
     "(the default) the returned sequence is base-pair complemented. "
     "If FROM > TO this indicates wrapping. IF-WRAPPED-BUT-NOT-CIRCULAR "
     "controls what happens if wrapping is indicated but the CONTIG is not "
     "circular.  Its possible values are :ERROR, :WARN, or NIL.  (See "
     "EXTRACT-GENE-SEQUENCE to get sequence information specific to an "
     "individual GENE.)  Note that FROM and TO are 1-based, not zero-based, "
     "and FROM is inclusive, not exclusive.")
  (when safely?
    (unless (and (integerp from) (plusp from))
      (error "Invalid FROM value: ~A" from))
    (unless (and (integerp to) (plusp to))
      (error "Invalid TO value: ~A" from)))
  (setq contig (canonicalize-frame-designator contig))
  (let ((type (type-of contig)))
    (unless (eq 'aframes::bio.contiguous-sequence type)
      (error "Ruh roh. ~A is not a contiguous sequence!" contig)))
  (setq direction (canonicalize-direction direction))
  (let* ((organism (slotv contig #$Organism))
        (*current-extraction-organism* organism)
        (*current-extraction-file* (#^genome-sequence-file organism)))
    (when (null organism) (ierror "Gene has no organism!!"))
    (let* ((seqlen (slotv contig #$Sequence-Length))
           (seqstart (internal-sequence-data-start contig))
           (seqstream (genome-sequence-stream organism))
           (circular? (slotv contig #$Circular)))
      (when safely?
        (unless (and seqlen (integerp seqlen))
          (ierror "Sequence length null or invalid: ~A" seqlen))
        (unless (and seqstart (integerp seqstart))
          (ierror "Sequence start null or invalid: ~A" seqstart))
        (unless (and (<= from seqlen) (<= to seqlen))
          (error "Invalid FROM/TO bounds (~D,~D) with respect to seqlen ~D"
                 from to seqlen))
        )
      (setq circular? (canonicalize-circular? circular?))
      (when safely? (sequence-system-limitations-check from to seqlen))
      (circular-contig-check
       contig from to seqlen circular? if-wrapped-but-not-circular)
      (extract-sequence-substring-using-seq-stream
       from to seqlen seqstart seqstream
       (eq direction :b) complement-backwards?
       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      
(defun xtract-gene-sequence
       (gene
        &key
        (cache? nil)
        (start-offset 0)
        (end-offset 0)
        (complement-backwards? t)
        (if-no-sequence? :null-string)
        (if-wrapped-but-not-circular :error)
        (ignore-architecture? nil)
        (safely? t)
        &allow-other-keys
        )        
  (extract-gene-sequence
   gene
   :cache? cache? 
   :start-offset start-offset
   :end-offset end-offset
   :complement-backwards? complement-backwards?
   :if-no-sequence? if-no-sequence?
   :if-wrapped-but-not-circular if-wrapped-but-not-circular
   :ignore-architecture? ignore-architecture?
   :safely? safely?
   ))


(defun extract-gene-sequence 
       (
        gene
        &key
        (cache? nil)
        (start-offset 0)
        (end-offset 0)
        (complement-backwards? t)
        (if-no-sequence? :null-string)
        (if-wrapped-but-not-circular :error)
        (ignore-architecture? nil)
        (safely? t)
        )

  #.(one-string-nl
     "The GENE belongs to a CONTIG, which is part of an ORGANISM."
     "-- Retrieve the sequence defined by a gene's DIRECTION property,"
     "and either its FROM and TO properties, or its ARCHITECTURE property."
     "The sequence retrieved is a concatentation of one or more subsequences"
     "of the CONTIG's sequence (one, if the gene has no architecture, possibly"
     "many if the gene has an ARCHITECTURE)."
     "-- If COMPLEMENT-BACKWARDS? is T (the default), then if the "
     "gene's DIRECTION is B (backwards) the result sequence is base-pair "
     "complemented (meaning the sequence is reversed, then each base is"
     "complemented)."
     "-- If the gene has ARCHITECTURE it must be a list of (A-FROM A-TO) pairs."
     "The first (A-FROM A-TO) pair must have A-FROM = FROM, and the last"
     "pair must have A-TO = TO.  Each pair represents a subsequence between"
     "FROM and TO, and A-FROM and A-TO are inclusive just as FROM and TO are."
     "If IGNORE-ARCHITECTURE? is NIL, and the gene has an architecture, the"
     "aformentioned extracted subsequences are concatenated together in order"
     "and then COMPLEMENT-BACKWARDS? is applied."
     "-- Extended regions around the gene may be accessed using "
     "START-OFFSET and/or END-OFFSET.  In the forward-reading case, "
     "START-OFFSET is added to FROM and END-OFFSET is added to TO. In the "
     "backward-reading case, START-OFFSET is subtracted from TO, and "
     "END-OFFSET is subtracted from FROM.  If START-OFFSET or END-OFFSET"
     "are provided any ARCHITECTURE data is ignored, only FROM and TO are"
     "used to determine the bases from which the offsets are determined."
     "-- If the gene's FROM > TO, this indicates wrapping."
     "If wrapping is implied but the CONTIG is not specified "
     "as CIRCULAR when it is defined, then an error will be signalled by "
     "default.  Other possible values of IF-WRAPPED-BUT-NOT-CIRCULAR are "
     ":WARN (issue a warning but execute the wrap) or NIL (execute with "
     "no warning)."
     "-- If CACHE? is true the sequence is retrieved from the gene's"
     "#$Sequence slot if there, or stored there once retrieved if not "
     "-- See EXTRACT-CONTIG-SEQUENCE for a way to extract arbitrary regions"
     "of a CONTIG.")

  (block exit

    (setq gene (canonicalize-frame-designator gene))

    ;; Make sure we have an honest-to-goodness gene frame.

    (when safely?
      (let ((type (type-of gene)))
        (unless (eq 'aframes::bio.gene type)
          (error 
           (one-string
            "Ruh roh. ~A is not a gene!" gene)))))

    ;; If caching is enabled and it's cached, we're done.

    (when cache? 
      (let ((sequence (slotv gene #$Sequence)))
        (when sequence (return-from exit sequence))))

    ;; Extract the relevant sequence data out of the gene

    (let* ((direction (slotv gene #$Direction))
           (from (slotv gene #$From))
           (to (slotv gene #$To))
           (architecture (slotv gene #$Architecture))
           (contig (slotv gene #$Contiguous-Sequence))
           (seqlen (slotv contig #$Sequence-Length))
           (circular? (slotv contig #$Circular))
           (organism (slotv gene #$Organism))
           (offsets-used? nil)
           (*current-extraction-organism* organism)
           (*current-extraction-file* (#^genome-sequence-file organism))
           )

      ;; If there's no sequence data (signified by FROM = -1) we're done

      (when (eql from -1)
        (return-from exit
          (case if-no-sequence?
            (:null-string "")
            (otherwise (error "Gene has no sequence data."))
            )))

      ;; Verify all of the data if safety is enabled

      (when safely?
        (when (null contig) (ierror "Gene has no contig!!"))
        (when (null organism) (ierror "Gene has no organism!!"))
        (when (null direction)
          (ierror "DIRECTION slot of gene ~A is invalid: ~A" gene direction))
        (when (or (null seqlen) (not (integerp seqlen)) (not (plusp seqlen)))
          (ierror "Null or bad seqlen, ~A, for contig ~A of gene ~A"
                  seqlen contig))
        (when (or (null from) (not (integerp from)))
          (ierror "FROM slot of gene ~A is invalid: ~A" gene from))
        (when (or (null to) (not (integerp to)))
          (ierror "TO slot of gene ~A is invalid: ~A" gene to))
        (unless (and (plusp from) (<= from seqlen))
          (ierror "FROM slot, ~A, of gene ~A, is not within contig's bounds, ~A" 
                  from gene seqlen))
        (unless (and (plusp to) (<= to seqlen))
          (ierror "TO slot, ~A, of gene ~A, is not within contig's bounds, ~A" 
                  to gene seqlen))
        ;; Verify each fragment against the gene.
        (when (and architecture (not ignore-architecture?))
          (when (stringp architecture)
            (setq architecture (read-from-string architecture)))
          (unless (valid-architecture-list architecture)
            (ierror "Invalid architecture list, ~A, for gene ~A"
                    architecture gene))
          (loop for (a-from a-to) in architecture do
                (validate-fragment-within-gene
                 gene contig from to a-from a-to
                 ))
          (let* ((first-fragment (first architecture))
                 (last-fragment (lastelem architecture))
                 (first-from (first first-fragment))
                 (last-to (second last-fragment)))
            (unless (= first-from from)
              (ierror 
               "First fragment's FROM, ~A, does not match gene ~A's FROM, ~A"
               first-from gene from
               ))
            (unless (= last-to to)
              (ierror 
               "Last fragment's TO, ~A, does not match gene ~A's TO, ~A"
               last-to gene to
               )))))

      (setq direction (canonicalize-direction direction))
      (setq circular? (canonicalize-circular? circular?))
                  
      (let* ((seqstart (internal-sequence-data-start contig))
             (seqstream (genome-sequence-stream organism)))

        (when safely?
          (when (or (null seqstart) (not (integerp seqstart)))
            (ierror 
             "Null or bad seqstart, ~A, for contig ~A !" seqstart contig))
          )

        ;; Apply offsets, if provided and non-zero.

        (unless (and (zerop start-offset) (zerop end-offset))
          (setq offsets-used? t)
          (let (new-from new-to)
            (cond
             ((eq direction :f)
              ;; Start is FROM, End is TO.  
              ;; A positive offset to Start increments FROM, etc.
              (setq new-from (+ from start-offset))
              (setq new-to (+ to end-offset)))
             ((eq direction :b)
              ;; Start is TO, and End is FROM.
              ;; A positive offset to Start decrements TO, etc.
              (setq new-from (- from end-offset))
              (setq new-to (- to start-offset))))
            (circular-contig-check 
             contig new-from new-to seqlen 
             circular? if-wrapped-but-not-circular)
            (setq from (wrap-sequence-index new-from seqlen))
            (setq to (wrap-sequence-index new-to seqlen))
            ))

        ;; Verify that if extraction wraps, it is legal.

        (circular-contig-check 
         contig from to seqlen circular? if-wrapped-but-not-circular)

        ;; Grab the sequence!

        (let ((sequence
               (if (and architecture 
                        (not offsets-used?)
                        (not ignore-architecture?))
                   (extract-gene-sequence-using-architecture
                    architecture seqlen seqstart seqstream
                    (eq direction :b) complement-backwards?)
                 (extract-sequence-substring-using-seq-stream
                  from to seqlen seqstart seqstream
                  (eq direction :b) complement-backwards?
                  ))))
          (when cache? (setf (slotv gene #$Sequence) sequence))
          sequence
          )

        ))))

(defun extract-gene-sequence-using-architecture
       (architecture seqlen seqstart seqstream reverse? complement-if-reverse?)
  ;; Retrieve all the fragments and smash them together.
  (let ((sequence
         (apply
          'concatenate
          'string
          (loop for (from to) in architecture collect
                (extract-sequence-substring-using-seq-stream
                 from to seqlen seqstart seqstream nil nil
                 )))))
    ;; Reverse and complement the entire sequence if specified, not the
    ;; individual fragments.
    (when reverse? (setq sequence (nreverse sequence))
      (when complement-if-reverse?
        (setq sequence (ncomplement-base-pairs sequence :reverse? nil))
        ))
    sequence
    ))

(defun validate-fragment-within-gene
       (gene contig-of-gene gene-from gene-to fragment-from fragment-to)
  #.(one-string-nl
     "Validate fragment coordinates against the enclosing gene coordinates."
     "Errors out if the fragment coordinates are not valid, otherwise"
     "returns T.  The gene coordinates are assumed valid.")
  (declare (fixnum gene-from gene-to fragment-from fragment-to))

  (block exit

    (labels ((oops (label reason)
               (ierror 
                (one-string 
                 "Invalid ~A in fragment (~A ~A) of gene ~A: " reason)
                label fragment-from fragment-to gene))
             (bad-from (&optional (reason "")) (oops "FROM" reason))
             (bad-to (&optional (reason "")) (oops "TO" reason))
             (bad-from-to (&optional (reason "")) (oops "FROM/TO" reason))
             )
  
      (unless (and (integerp fragment-from) (plusp fragment-from)) 
        (bad-from "Not a positive integer"))
      (unless (and (integerp fragment-to) (plusp fragment-to))
        (bad-to "Not a positive integer"))
      
      (let ((circular? (#^Circular contig-of-gene))
            (seqlen (#^Sequence-Length contig-of-gene)))
        (declare (fixnum seqlen))

        (when (> fragment-from seqlen) 
          (bad-from "Coordinate not within contig"))
        (when (> fragment-to seqlen)
          (bad-to "Coordinate not within contig"))

        (cond

         ;; The gene does not wrap (the contig may be circular but this
         ;; gene doesn't wrap)

         ((or (not circular?) (<= gene-from gene-to))
          (when (< fragment-from gene-from)
            (bad-from "Coordinate not within gene and/or contig"))
          (when (> fragment-to gene-to)
            (bad-to "Coordinate not within gene and/or contig"))
          (when (> fragment-from fragment-to)
            (bad-from-to "FROM coordinate is larger than TO coordinate"))
          t)

         (t

          ;; This gene wraps.
          ;; Conceptually divide the gene into two segments, one
          ;; from where the gene starts to the 'wrap point' (aka 'end')
          ;; of the contig (X1), and one from the 'wrap point' (aka 'start')
          ;; of the contig to where the gene ends (X2).

          (let ((fragment-from-location
                 (cond
                  ((>= fragment-from gene-from) :x1)
                  ((<= fragment-from gene-to) :x2)
                  (t (bad-from "Coordinate not within wrapped gene"))
                  ))
                (fragment-to-location
                 (cond
                  ((>= fragment-to gene-from) :x1)
                  ((<= fragment-to gene-to) :x2)
                  (t (bad-to "Coordinate not within wrapped gene"))
                  )))
            ;; Now if the FROM and TO are on the same segment,
            ;; then it better be that FROM < TO.
            ;; If they are on different segments unless FROM is on X1
            ;; and TO is on X2 there is an error because the reverse
            ;; is not possible.
            (cond
             ((eq fragment-from-location fragment-to-location)
              (when (> fragment-from fragment-to) 
                (bad-from-to "TO coordinate is conceptually left of FROM")
                ))
             ((not (and (eq fragment-from-location :x1) 
                        (eq fragment-to-location :x2)
                        ))
              (bad-from-to "TO coordinate is conceptually left of FROM"))
             (t t)
             ))

          ))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; THESE TWO DEFINITIONS NEED TO STAY IN SYNC!

(defun xtract-protein-sequence 
       (protein &key (strip-trailing-star? t) (cache? nil) &allow-other-keys)
  (extract-protein-sequence 
   protein :strip-trailing-star? strip-trailing-star? :cache? cache?))


(defun extract-protein-sequence 
       (protein &key (strip-trailing-star? t) (cache? nil))
  #.(one-string-nl
     "Retrieve the protein sequence of a protein.  If CACHE? is true the "
     "sequence is retrieved from the #$Sequence slot of the protein if "
     "it is there, and stored there once retrieved if not.  If"
     "STRIP-TRAILING-STAR? is true any trailing '*' is removed (but not"
     "from a cached sequence).")
  (block exit
    (setq protein (canonicalize-frame-designator protein))
    (let ((type (type-of protein)))
      (unless (eq 'aframes::bio.Protein type)
        (error "Ruh roh. ~A is not a protein!" protein)))
    (when cache?
      (let ((sequence (slotv protein #$Sequence)))
        (when sequence (return-from exit sequence))))
    
    (let* ((orgf (#^organism protein))
           (seqfile (protein-sequence-file orgf))
           (seqlen (#^sequence-length protein))
           (seqstart (internal-sequence-data-start protein)))
          
      (when (null seqfile)
        (error "No protein sequence file slot in organism ~A!" orgf))
      (when (null seqlen)
        (error "No seqlen slot in protein ~A" protein))
      (when (null seqstart)
        (error "No internal sequecne data for protein ~A" protein))
     
      (with-open-file (p seqfile :direction :input)
        (unless (file-position p seqstart)
          (error "Internal error.  FILE-POSITION returned NIL for file ~A"
                 seqfile))
        (let* ((sequence (make-string seqlen))
               (nchars (read-sequence sequence p))
               (lastpos (1- seqlen)))
          (unless (= seqlen nchars)
            (error
             (one-string 
              "Protein sequence ~A has ~D chars but READ-SEQUENCE"
              "read only ~D chars from sequence file ~A")
             protein seqlen nchars seqfile))

          (when (and strip-trailing-star? 
                     ;; Degenerate case -- null protein sequence.
                     (not (minusp lastpos))
                     (eql #\* (char sequence lastpos)))
            (setq sequence (subseq sequence 0 lastpos)))
          (when cache? (setf (slotv protein #$Sequence) sequence))
          sequence
          )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-sequence (organism-entity &rest keyargs &key &allow-other-keys)
  #.(one-string-nl
     "Retrieves a sequence from a Contig, Gene, Trancript or Protein "
     "depending on what kind of frame ORGANISM-ENTITY is.  To retrieve "
     "a subsequence from a contig use keywords FROM, TO and DIRECTION which "
     "get passed as required arguments to EXTRACT-CONTIG-SEQUENCE. Other "
     "keywords are passed through to the appropriate extraction routine or "
     "ignored if they are not appropriate.  See:"
     "  (EXTRACT-GENE-SEQUENCE ...)"
     "  (EXTRACT-PROTEIN-SEQUENCE ...)"
     "  (EXTRACT-CONTIG-SEQUENCE ...)"
     "for details on allowable KEYARGS for each type of ORGANISM-ENTITY.")
  (let* ((oe (canonicalize-frame-designator organism-entity)))
    (typecase oe
     (aframes::bio.gene (apply 'xtract-gene-sequence oe keyargs))
     (aframes::bio.protein (apply 'xtract-protein-sequence oe keyargs))
     (aframes::bio.contiguous-sequence 
      (flet ((getkeyval (key)
               (loop for keylist on keyargs do 
                     (when (eq key (first keylist)) (return (second keylist)))
                     )))
        (let ((from (or (getkeyval :from) 1))
              (to (or (getkeyval :to) (slotv oe #$Sequence-Length)))
              (direction (or (getkeyval :direction) :f))
              )
          (apply 'xtract-contig-sequence oe from to direction keyargs)
          )))
     (otherwise (error "Don't know how to extract a sequence from ~S" oe))
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Auxiliary routines

;;; Read a subsequence from the organism's sequence file
;;; using the open stream SEQSTREAM, open to that file.

(defun extract-sequence-substring-using-seq-stream
       (from to seqlen seqstart seqstream reverse? complement-if-reverse?)
  #.(optimization-declaration)
  (declare (fixnum from to))
  (let ((sequence
         (if (<= from to)
             (get-contig-subsequence seqstream seqstart seqlen from to)
           (one-string
            (get-contig-subsequence seqstream seqstart seqlen from seqlen)
            (get-contig-subsequence seqstream seqstart seqlen 1 to)
            ))))
    (when reverse? (setq sequence (nreverse sequence))
      (when complement-if-reverse?
        (setq sequence (ncomplement-base-pairs sequence :reverse? nil))))
    sequence
    ))


(defun get-contig-subsequence (organism-sequence-port start-data seqlen from to)
  #.(optimization-declaration)
  (declare (fixnum start-data seqlen from to))
  (when (or (not (plusp from)) (> from seqlen)
            (not (plusp to)) (> to seqlen)
            (> from to))
    (ierror "Bad (FROM,TO) indices: (~D,~D). Sequence length is ~D."
            from to seqlen))
  (flet ((get-it (p)
           (let* ((start-subseq 
                   (the fixnum (+ start-data (the fixnum (1- from)))))
                  (size (the fixnum (1+ (the fixnum (- to from)))))
                  (result (make-string size)))
             (unless (file-position p start-subseq)
               (ierror 
                (one-string
                 "Could not position stream pointer to position ~S,"
                 "for stream ~A, organism sequence file ~A")
                start-subseq p *current-extraction-file*))
             (read-sequence result p)
             result
             )))
    (cond
     ((streamp organism-sequence-port) (get-it organism-sequence-port))
     ((null *keep-sequence-streams-open*)
      (with-open-file (s *current-extraction-file* :direction :input) 
        (get-it s)
        ))
     (t 
      (let ((stream (open *current-extraction-file* :direction :input))) 
        (set-genome-sequence-stream *current-extraction-organism* stream)
        (get-it stream)
        )))))


(defun circular-contig-check (contig from to seqlen circular? action)
  #.(optimization-declaration)
  (declare (fixnum from to seqlen))
  (when (and (or (> from to) (< from 1) (< to 1) (> from seqlen) (> to seqlen))
             (not circular?))
    (case action
      (:error
       (error 
        (one-string
         "You're asking for a wrapped sequence extraction (FROM: ~D, TO: ~A) "
         "but the contiguous segment ~A is not designated as circular.")
        from to contig))
      (:warn "Performing wrapped extraction on non-circular contig ~A!" contig)
      (otherwise nil)
      )))

(defun sequence-system-limitations-check (from to seqlen)
  #.(optimization-declaration)
  (flet ((not-fixnum (x) 
           (or (> x most-positive-fixnum) (< x most-negative-fixnum)))
         (seqlen (from to) (1+ (- to from)))
         (ruh-roh ()
           (error "Ruh roh. Subsequence (~D,~D) exceeds array size limit: ~D"
                  from to array-dimension-limit)))
    (when (or (not-fixnum from) (not-fixnum to) (not-fixnum seqlen))
      (error "Ruh roh. FROM, TO or SEQLEN are not fixnums!"))
    (if (<= from to)
        (when (> (seqlen from to) array-dimension-limit) (ruh-roh))
      (when (> (+ (seqlen from seqlen) (seqlen 1 to)) array-dimension-limit)
        (ruh-roh)
        ))))


(defun extraction-test (orgfs)
  (cformatt "Executing Extraction Test.")
  (flet ((extract-them (organism slot ext-function)
           (cformatt "  Now extracting ~A from organism ~A" slot organism)
           (loop for thing in (slotv organism slot) do
                 (funcall ext-function thing))))
    (loop for org in orgfs do
          (cformatt "Now testing organism ~A" org)
          (extract-them 
           org #$contiguous-sequences 
           (lambda (c) (extract-contig-sequence c 1 (#^sequence-length c) :f)))
          (extract-them org #$genes 'extract-gene-sequence)
          (extract-them org #$proteins 'extract-protein-sequence)))
  (cformatt "All done."))


(defun test-sequence-extraction 
       (organism
        &key 
        (test-file nil)
        (other-genes? t)
        (max-genes most-positive-fixnum)
        (verbose? t)
        &aux
        (genes-prefix (orgf-element-prefix organism :genes))
        genef
        )
  #.(one-string-nl
     "Retrieves gene sequences from a test file and tests them against"
     "gene sequences retrieved from the database.  OTHER-GENES? = T causes"
     "all an organisms genes to be retrieved from the database to verify"
     "the retrieval process.")
  (when test-file
    (when verbose? 
      (cformatt 
       "Testing sequences in file ~A" (namestring (truename test-file))))
    (with-open-file (p test-file :direction :input)
      (do ((test (read p nil nil) (read p nil nil)))
          ((null test))
        (destructuring-bind (gene expected-sequence) test
          ;; Allow test file to have gene name with or without given prefix.
          (setq gene (add-prefix-if-not-present gene genes-prefix))
          (setq genef (frame-fnamed gene))
          (unless genef (error "No gene named ~S exists ???" gene))
          (setq organism (slotv genef #$Organism))
          (setq expected-sequence
                (remove-if
                 (lambda (ch) (or (eql ch #\Newline) (eql ch #\Return)))
                 expected-sequence
                 ))
          (let* ((actual-sequence (extract-gene-sequence genef))
                 (asl (length actual-sequence))
                 (esl (length expected-sequence)))
            (cond 
             ((/= asl esl)
              (cformatt 
               "Ruh roh!  Gene ~A Actual length: ~D, Expected ~D"
               genef asl esl))
             ((not (string-equal actual-sequence expected-sequence))
              (cformatt
               "Ruh roh! Gene ~A actual sequence differs from expected" genef))
             (t (when verbose? (cformatt "Gene ~A OK." genef)))
             ))))))
  (when other-genes?
    (setq max-genes (min (length (slotv organism #$Genes)) max-genes))
    (when verbose? 
      (cformatt 
       "Testing that ~D gene sequences in organism ~A can be extracted."
       max-genes organism))
    (with-organisms-db (db)
      (loop for j fixnum from 1 to max-genes 
            for gene in (slotv organism #$Genes) do
            (when verbose? (when (zerop (mod j 100)) (format t ".")))
            (extract-gene-sequence gene)))
    (when verbose? 
      (cformatt "All genes extracted successfully.")
      (cformatt "(Not necessarily correctly!)")
      )))

