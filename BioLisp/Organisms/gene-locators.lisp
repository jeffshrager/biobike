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

;;; Author:  JP Massar.

(defun reversed-gene-direction (dir)
  (ecase (canonicalize-direction dir) (:f :b) (:b :f)))

(defmacro with-gene-direction-temporarily-reversed ((gene) &body body)
  (let ((dirsym (gensym "DIR-"))
        (genesym (gensym "GENE-")))
    `(let* ((,genesym ,gene)
            (,dirsym (slotv ,genesym #$Direction)))
       (unwind-protect
           (progn 
             (setf (slotv ,genesym #$Direction) 
                   (reversed-gene-direction ,dirsym))
             ,@body)
         (setf (slotv ,genesym #$Direction) ,dirsym)
         ))))

(defun clamp-contig-coordinate (contig coord start-or-end)
  #.(one-string-nl
     "Restrict an index, COORD, into a Contiguous Sequence to be within its"
     "bounds, either with respect to its beginning, if START-OR-END"
     "is :START, or its end, if START-OR-END is :END."
     "If the Contiguous Sequence is circular COORD is returned unchanged.")
  (if (#^Circular contig)
      coord
    (ecase start-or-end
      (:start (max 1 coord))
      (:end (min (#^Sequence-length contig) coord))
      )))
    

(defun sequence-upstream-from (gene &optional (size nil))

  #.(one-string-nl
     "Find the upstream region of GENE."
     "Returns the upstream sequence, or NIL if no such region exists. "
     "The upstream region is conceptually to the left of GENE if GENE"
     "goes FORWARD, and to the right of GENE if GENE goes BACKWARDS."
     "If SIZE is provided, that many base pairs upstream are returned"
     "in the sequence, or as many base pairs as exist before hitting the"
     "edge of the sequence, or NIL if no such base pairs exist."
     "If SIZE is NIL, the intergenic region between GENE and the gene"
     "immediately prior or subsequent to GENE is returned.  If no such"
     "prior or subsequent gene exists then the region from GENE to the"
     "beginning, or end, respectively of the contig, is returned."
     "If the prior or subsequent gene exists but overlaps with GENE, or"
     "GENE is embedded within it, then NIL is returned.")

  (let* ((contig (#^Contiguous-Sequence gene))
         (contig-length (#^Sequence-Length contig))
         (circular? (#^Circular contig))
         (direction (keywordize (#^Direction gene)))
         (forward? (eq direction :f))
         (from (#^From gene))
         (to (#^To gene))
         (downstream-boundary nil)
         (upstream-boundary nil))

    (flet ((safe-extract (from to dir)
             (unless (and (not circular?) (> from to))
               (extract-sequence contig :from from :to to :direction dir)))
           (clamp-start (coord) (clamp-contig-coordinate contig coord :start))
           (clamp-end (coord) (clamp-contig-coordinate contig coord :end))
           )

      (setq downstream-boundary
            (if circular?
                (if forward? 
                    (wrap-gene-index (1- from) gene)
                  (wrap-gene-index (1+ to) gene))
              (if forward? (clamp-start (1- from)) (clamp-end  (1+ to)))))

      (cond 

       ;; SIZE is specified.  Grab that many base pairs.

       ((and (integerp size) (plusp size))
        (when (and circular? (> size contig-length))
          (error "Attempting to extract more base pairs than exist!"))
        (if forward?
            (let ((raw-index (- downstream-boundary (- size 1))))
              (if circular?
                  (setq upstream-boundary (wrap-gene-index raw-index gene))
                (setq upstream-boundary (clamp-start raw-index)))
              (safe-extract upstream-boundary downstream-boundary :f))
          (let ((raw-index (+ downstream-boundary (- size 1))))
            (if circular?
                (setq upstream-boundary (wrap-gene-index raw-index gene))
              (setq upstream-boundary (clamp-end raw-index)))
            (safe-extract downstream-boundary upstream-boundary :b)
            )))

       ;; No SIZE specified.

       ((null size)

        (let* ((upstream-gene (Gene-upstream-from gene)))

          (cond

           ((and (null upstream-gene) circular?)
            (error "No upstream gene for circular contig!?"))

           (upstream-gene
            ;; A gene exists which defines the intergene region.
            (let ((ufrom (#^From upstream-gene)) 
                  (uto (#^To upstream-gene))
                  (rel (positional-relationship-between-two-genes
                        gene upstream-gene)))
              (cond
               ((eq rel :adjacent) "")
               ((eq rel :overlapped) nil)
               ((eq rel :within) nil)
               ((eq rel :standard)
                (cond
                 (circular?
                  (setq 
                   upstream-boundary
                   (if forward? 
                       (wrap-gene-index (1+ uto) upstream-gene)
                     (wrap-gene-index (1- ufrom) upstream-gene))))
                 (t
                  (setq upstream-boundary (if forward? (1+ uto) (1- ufrom)))))
                (if forward?
                    (safe-extract upstream-boundary downstream-boundary :f)
                  (safe-extract downstream-boundary upstream-boundary :b)
                  ))
               ((eq rel :identical) 
                (ierror
                 (one-string-nl
                  "Identical genes detected!  Currently in Biobike this is"
                  "not allowed.  The identical genes are ~A and ~A")
                 gene upstream-gene 
                 ))
               (t (ierror "Impossible condition"))
               )))

           (t
            ;; No gene between this gene and start/end of contig.
            (setq upstream-boundary (if forward? 1 contig-length))
            (if forward?
                (safe-extract upstream-boundary downstream-boundary :f)
              (safe-extract downstream-boundary upstream-boundary :b)
              )))))
       
       (t (error "Invalid SIZE argument: ~A" size))

       ))))


(defun sequence-downstream-from (gene &optional (size nil))
  "Same as (SEQUENCE-UPSTREAM-FROM ...) with direction reversed."
  (let ((seq
         (with-gene-direction-temporarily-reversed (gene)
           (sequence-upstream-from gene size)
           )))
    (setq seq (nreverse seq))
    ;; Might return nil...
    (unless (null seq) (ncomplement-base-pairs seq :reverse? nil))
    seq
    ))


(defun prev-gene-index (index len circular?)
  "Returns NIL if INDEX = 0 and CIRCULAR? is false"
  (declare (fixnum index len))
  (if (zerop index) (if circular? (1- len) nil) (1- index)))
(defun next-gene-index (index len circular?)
  "Returns NIL if INDEX = LEN-1 and CIRCULAR? is false"
  (declare (fixnum index len))
  (if (= index (1- len)) (if circular? 0 nil) (1+ index)))

(defun find-actual-gene 
       (index searched-for-gene sorted-genes circ? direction)
  (let ((ngenes (length sorted-genes))
        (inc (if (eq direction :f) -1 1))
        (gene-index-func 
         (if (eq direction :f) 'prev-gene-index 'next-gene-index))
        (from (#^From searched-for-gene)))
    (loop for j from 0 by inc 
          as next-index = (funcall gene-index-func (+ index j) ngenes circ?)
          ;; Until we run out of genes in the specified direction or
          ;; we wrap completely around...
          until (or (null next-index) (= next-index index))
          do
          (let ((next-gene (aref sorted-genes next-index)))
            ;; If we find it we're done.
            (when (eq next-gene searched-for-gene) (return next-index))
            ;; If FROM is no longer same as gene we are searching for
            ;; no sense continuing.
            (when (not (= from (#^From next-gene))) (return nil))
            ))))

(defun gene-upstream-from (gene)
  #.(one-string-nl
     "Return the gene upstream of GENE, or NIL."
     "If GENE's contig is not circular and GENE goes forward and GENE "
     "is the leftmost gene on the contig then there is no upstream gene."
     "And similarly for backward/rightmost."
     "If there were only one gene on a circular contig then this function"
     "will return GENE itself, not another gene.")
  (let* ((contig (#^Contiguous-Sequence gene))
         (sorted-genes (#^Genes-Sorted-by-Position contig))
         (ngenes (length sorted-genes))
         (from (#^From gene))
         (gene-index (binsearch from sorted-genes :key '#^From))
         (forward? (eq (keywordize (#^Direction gene)) :f))
         (circular? (#^circular contig))
         (upstream-gene-index nil))
    (unless gene-index
      (ierror "Cannot find gene ~A in contig ~A's sorted genes list!"
              gene contig))
    ;; There are occasionally 'genes' (not real genes, but entries in
    ;; the table) with the same 'from'.  Look on either side of the
    ;; entry we found for the real match if we didn't find the real match
    ;; (since the BINSEARCH is not guarenteed to return the least index)
    (unless (eq gene (aref sorted-genes gene-index))
      (setq gene-index
            (or (find-actual-gene gene-index gene sorted-genes circular? :f)
                (find-actual-gene gene-index gene sorted-genes circular? :b)
                (ierror 
                 (formatn
                  (one-string
                   "Cannot find gene ~A in sorted vector of genes for "
                   "contiguous sequence ~A")
                  gene contig
                  )))))
    (setq
     upstream-gene-index
     (if forward? 
         (prev-gene-index gene-index ngenes circular?)
       (next-gene-index gene-index ngenes circular?)
       ))
    (and upstream-gene-index (aref sorted-genes upstream-gene-index))
    ))

(defun gene-downstream-from (gene)
  "Same as (GENE-UPSTREAM-FROM ...) with direction reversed."
  (with-gene-direction-temporarily-reversed (gene)
    (gene-upstream-from gene)))

(defun overlap-between (input1 input2 &optional (lists-can-be-circular? nil))

  #.(one-string-nl
     "Returns a list (FROM TO) which defines the overlap between"
     "INPUT1 and INPUT2, which may either be genes or (FROM TO) lists."
     "If no overlap exists NIL is returned."
     "FROM and TO must both be positive integers."
     "If double overlap exists :double-overlap is returned as the first"
     "value and two (FROM TO) lists are returned as the 2nd and 3rd values."
     "If two genes are provided and they are from different contigs then"
     "NIL is returned.")

  (overlap-between2 
   input1 input2 :lists-can-be-circular? lists-can-be-circular?
   ))


(defun positional-relationship-between-two-genes (g1 g2)
  #.(one-string-nl
     "Returns NIL if the two genes are not on the same contig, otherwise"
     "Returns 2 values.  The first is the relationship between the genes"
     "and is one of: :standard, :adjacent, :identical, :within or :overlapped"
     "where :standard means neither adjacent, identical, within, nor" 
     "overlapping.  The second"
     "value is the relative position of G1 to G2, and is one of"
     ":order-forward, meaning G1 is 'before' G2, or :order-backward, meaning"
     "the opposite.  For genes in which neither wraps 'before' has the"
     "obvious semantics.  For one or more genes which wrap, the smaller"
     "FROM still dictates the relationship.")
  (let ((c1 (#^Contiguous-Sequence g1))
        (c2 (#^Contiguous-Sequence g2)))
    (when (eq c1 c2)
      (let ((circ? (#^Circular c1))
            (len (#^Sequence-Length c1))
            (from1 (#^From g1))
            (from2 (#^From g2))
            (to1 (#^To g1))
            (to2 (#^To g2))
            )
        (positional-relationship-between-two-intervals
         circ? len from1 to1 from2 to2)
        ))))

(defun prbti (circular length f1 t1 f2 t2)
  (multiple-value-list 
   (positional-relationship-between-two-intervals circular length f1 t1 f2 t2)
   ))

;; Assumes index of first element of sequence is 1
(defun positional-relationship-between-two-intervals
       (circular length f1 t1 f2 t2) 
  (declare (fixnum f1 t1 f2 t2))
  (let ((w1 (> f1 t1)) (w2 (> f2 t2)))
    (if (or (not circular) (and (not w1) (not w2)))
        ;; No wrapped intervals.
        (cond
         ;; interval 1 is strictly to the left of interval 2
         ((< t1 f2)
          (if (= t1 (1- f2))
              (values :adjacent :order-forward)
            (values :standard :order-forward)
            ))
         ;; interval 2 is strictly to the left of interval 2
         ((< t2 f1)
          (if (= t2 (1- f1))
              (values :adjacent :order-backward)
            (values :standard :order-backward)
            ))
         ;; identical intervals
         ((and (= f1 f2) (= t1 t2)) (values :identical :order-forward))
         ;; One is within the other
         ((and (= f2 f1) (< t2 t1)) (values :within :order-forward))
         ((and (> f2 f1) (<= t2 t1)) (values :within :order-forward))
         ((and (= f1 f2) (< t1 t2)) (values :within :order-forward))
         ((and (> f1 f2) (<= t1 t2)) (values :within :order-backward))
         ((and (< f1 f2) (>= t1 f2) (< t1 t2)) 
          (values :overlapped :order-forward))
         ((and (< f2 f1) (>= t2 f1) (< t2 t1)) 
          (values :overlapped :order-backward))
         (t (ierror "This should be impossible."))
         )
      ;; One or both intervals are wrapped. 
      (pr-between-wrapped-intervals w1 w2 f1 t1 f2 t2 length)
      )))

(defun pr-between-wrapped-intervals 
       (a-wrapped? b-wrapped? fa ta fb tb interval-length)
  (flet ((entire-interval? (from to) 
           (or (= to (1- from)) (and (= from 1) (= to interval-length)))))
    (let ((ae? (entire-interval? fa ta))
          (be? (entire-interval? fb tb))
          (order (if (<= fa fb) :order-forward :order-backward)))
      (cond
       ((and (= fa fb) (= ta tb)) (values :identical :order-forward))
       ((and ae? be?) (values :within order))
       ((or ae? be?) (values :within order))
       ((and a-wrapped? b-wrapped?) 
        (values (pr-between-two-wrapped-intervals fa ta fb tb) order))
       (a-wrapped? 
        (let ((i1 (list fa interval-length))
              (i2 (list 1 ta))
              (i3 (list fb tb)))
          (values 
           (pr-between-unwrapped-and-nonwrapped-intervals i1 i2 i3)
           order
           )))
       (b-wrapped?
        (let ((i1 (list fb interval-length))
              (i2 (list 1 tb))
              (i3 (list fa ta)))
          (values 
           (pr-between-unwrapped-and-nonwrapped-intervals i1 i2 i3)
           order
           )))
       (t (error "This is impossible!"))
       ))))

;; (circular length f1 t1 f2 t2)
;; :standard, :adjacent, :identical, :within or :overlapped
(defun pr-between-unwrapped-and-nonwrapped-intervals (i1 i2 i3)
  (let* ((fa (first i1))
         (fb (first i2))
         (fc (first i3))
         (r1 (positional-relationship-between-two-intervals 
              nil nil fa (second i1) fc (second i3)))
         (r2 (positional-relationship-between-two-intervals 
              nil nil fb (second i2) fc (second i3)))
         )
    (when (eq r1 :within) 
      (when (< fc fa) (setq r1 :encompassing)))
    (when (eq r2 :within) 
      (when (> (second i3) (second i2)) (setq r2 :encompassing)))
    (cond
     ((eq r1 :standard) 
      (cond
       ((eq r2 :standard) :standard)
       ((eq r2 :adjacent) :adjacent)
       ((eq r2 :identical) :within)
     ; ((eq r2 :within) :adjacent)
       ((eq r2 :within) :within)
       ((eq r2 :encompassing) :overlapped)
       ((eq r2 :overlapped) :overlapped)
       (t (error "standard,t Impossible"))
       ))
     ((eq r1 :adjacent)
      (cond
       ((eq r2 :standard) :adjacent)
       ((eq r2 :adjacent) :adjacent)
       ((eq r2 :identical) (error "adjacent,identical Impossible"))
       ((eq r2 :within) (error "adjacent,within Impossible"))
       ((eq r2 :encompassing) :overlapped)
       ((eq r2 :overlapped) :overlapped)
       (t (error "adjacent,t Impossible"))
       ))
     ((eq r1 :identical)
      (cond
       ((eq r2 :standard) :within)
       ((eq r2 :adjacent) (error "identical,adjacent Impossible"))
       ((eq r2 :identical) (error "identical,identical Impossible"))
       ((eq r2 :within) (error "identical,within Impossible"))
       ((eq r2 :encompassing) (error "identical,encompassing Impossible"))
       ((eq r2 :overlapped) (error "identical,overlapped Impossible"))
       (t (error "identical,t Impossible"))
       ))
     ((eq r1 :within)
      (cond
       ((eq r2 :standard) :within)
       ((eq r2 :adjacent) (error "within,adjacent Impossible"))
       ((eq r2 :identical) (error "within,identical Impossible"))
       ((eq r2 :within) (error "within,within Impossible"))
       ((eq r2 :encompassing) (error "within,encompassing Impossible"))
       ((eq r2 :overlapped) (error "within,overlapped Impossible"))
       (t (error "within,t Impossible"))
       ))
     ((eq r1 :encompassing)
      (cond
       ((eq r2 :standard) :overlapped)
       ((eq r2 :adjacent) :overlapped)
       ((eq r2 :identical) (error "encompassing,identical Impossible"))
       ((eq r2 :within) (error "encompassing,within Impossible"))
       ((eq r2 :encompassing) (error "encompassing,encompassing Impossible"))
       ((eq r2 :overlapped) :overlapped)
       (t (error "encompassing,t Impossible"))
       ))
     ((eq r1 :overlapped)
      (cond
       ((eq r2 :standard) :overlapped)
       ((eq r2 :adjacent) :overlapped)
       ((eq r2 :identical) (error "overlapped,identical Impossible"))
       ((eq r2 :within) (error "overlapped,within Impossible"))
       ((eq r2 :encompassing) :overlapped)
       ((eq r2 :overlapped) :overlapped)
       (t (error "overlapped,t Impossible"))
       ))
     (t (error "Fell through cond clause"))
     )))
      
(defun pr-between-two-wrapped-intervals (fa ta fb tb)
  (declare (fixnum fa ta fb tb))
  (cond
   ((= fa fb) 
    (cond
     ((= ta tb) (error "Should never get here"))
     ((< ta tb) :within)
     ((> ta tb) :within)
     ))
   ((< fa fb) 
    (cond
     ((= ta tb) :within)
     ((< ta tb) :overlapped)
     ((> ta tb) :within)
     ))
   ((> fa fb) 
    (cond
     ((= ta tb) :within)
     ((< ta tb) :within)
     ((> ta tb) :overlapped)
     ))
   ))
    

        
       

  

       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Code for computing overlap and embeds 


;;; contig coordinates are 1-based. That is, if a gene were to start
;;; at the very start of a contig, its FROM would be 1, not 0, and
;;; if it were to end at the very end, it's TO would be the length
;;; of the contig (not one less than).

(defun overlapping-genes-of (gene)
  #.(one-string-nl
     "Returns a list of all the genes that overlap GENE on GENE's contig."
     "If overlap information about GENE's contig has not yet been computed,"
     "it is computed for all the contig's genes.  (The slot #$overlaps-computed"
     "is set when a contig's overlap information is computed.)")
  (let ((c (#^contiguous-sequence gene)))
    (if (#^overlaps-computed? c) 
        (#^overlaps gene)
      (progn
        (overlaps-and-embeds-in-contig c t nil)
        (#^overlaps gene)
        ))))

(defun embedding-genes-of (gene)
  #.(one-string-nl
     "Returns a list of all the genes that embed GENE on GENE's contig."
     "If embed information about GENE's contig has not yet been computed,"
     "it is computed for all the contig's genes.  (The slot #$embedders-computed"
     "is set when a contig's embed information is computed.)")
  (let ((c (#^contiguous-sequence gene)))
    (if (#^embedders-computed? c)
        (#^embedders gene)
      (progn
        (overlaps-and-embeds-in-contig c nil t)
        (#^embedders gene)
        ))))


(defvar *overlapping-genes-computed* nil)
(defvar *embedded-genes-computed* nil)

(defun all-overlapping-and-embedded-genes
       (&key (organisms (loaded-organisms)) (overlaps? t) (embeds? t))
  (let ((do-overlaps? (and overlaps? (null *overlapping-genes-computed*)))
        (do-embeds? (and embeds? (null *embedded-genes-computed*))))
    (when (or do-overlaps? do-embeds?)
      (compute-overlaps-and-embeds organisms do-overlaps? do-embeds?)
      (when do-overlaps? (setq *overlapping-genes-computed* t))
      (when do-embeds? (setq *embedded-genes-computed* t))
      )))

(defun all-overlapping-genes-of (obj)
  (purge-duplicates
   (cond 
    ;; only organisms have contiguous sequences
    ((#^contiguous-sequences obj)
     (loop for c in (#^contiguous-sequences obj) do 
           (unless (#^overlaps-computed? c)
             (overlaps-and-embeds-in-contig c t nil)))
     (mapcan (lambda (x) (copy-list (#^overlaps x))) (#^genes obj)))
    ;; only contiguous sequences have this slot
    ((#^genes-sorted-by-position obj)
     (unless (#^overlaps-computed? obj) 
       (overlaps-and-embeds-in-contig obj t nil))
     (loop for g across (#^genes-sorted-by-position obj) nconc 
           (copy-list (#^overlaps g))))
    (t (#^overlaps obj))
    )))

(defun clear-overlaps-and-embeds ()
  (loop for org in (loaded-organisms) do
        (loop for c in (#^contiguous-sequences org) do
              (setf (#^overlaps-computed? c) nil)
              (setf (#^embedders-computed? c) nil))
        (loop for g in (#^genes org) do
              (when (slotv g #$overlaps) (setf (slotv g #$overlaps) nil))
              (when (slotv g #$embedders) (setf (slotv g #$embedders) nil))
              )))
              
(defun compute-overlaps-and-embeds (organisms overlaps? embeds?)
  (loop for org in organisms 
        do
        (cformatt "Overlaps and embeds for ~A" org)
        (loop for c in (#^contiguous-sequences org) do
              (overlaps-and-embeds-in-contig c overlaps? embeds?)
              )))
                
(defun overlaps-and-embeds-in-contig (contig do-overlaps? do-embeds?)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((lc (#^sequence-length contig))
         (genemap (make-array (list (+ 2 lc)) :initial-element nil))
         (embeds nil)
         )
    (declare (type (simple-array t) genemap))
    ;; Fill in the gene map
    (loop for g across (#^genes-sorted-by-position contig)
          as from-index fixnum = (#^from g)
          as to-index fixnum = (#^to g)
          do 
          (flet ((mark (j)
                   (declare (fixnum j))
                   (push g (aref genemap j))))
            (if (> from-index to-index) 
                ;; A 'wrapped' gene on a circular contig
                (progn
                  (loop for j fixnum from from-index to lc do (mark j))
                  (loop for j fixnum from 1 to to-index do (mark j)))
              ;; A non-wrapped (normal) gene
              (loop for j fixnum from from-index to to-index do (mark j))
              )))
    ;; Figure out which genes overlap.  Any map location with more
    ;; than one gene in it indicates that all the genes at that location
    ;; overlap.
    (when do-overlaps?
      (loop for g across (#^genes-sorted-by-position contig) 
            as from-index fixnum = (#^from g)
            as to-index fixnum = (#^to g)
            as overlaps = nil
            do
            (flet ((add-overlaps (j) 
                     (declare (fixnum j))
                     (let ((overlapping-genes (aref genemap j)))
                       (loop for og in overlapping-genes do
                             (when (not (eq og g)) (pushnew og overlaps))
                             ))))
              (if (> from-index to-index) 
                  (progn
                    (loop for j fixnum from from-index to lc do (add-overlaps j))
                    (loop for j fixnum from 1 to to-index do (add-overlaps j)))
                (loop for j fixnum from from-index to to-index 
                      do (add-overlaps j)
                      )))
            (when overlaps (setf (#^overlaps g) overlaps)))
      (setf (#^overlaps-computed? contig) t))
    ;; Figure out which genes are embedded in other genes.  A gene
    ;; whose every location is occupied by one or more other genes
    ;; must be embedded in those other genes.
    (when do-embeds?
      (loop for g across (#^genes-sorted-by-position contig)
            as from-index fixnum = (#^from g)
            as to-index fixnum = (#^to g)
            with overlap-list = nil 
            do 
            (setq 
             overlap-list 
             (flet ((get-overlapping-genes (i) 
                      (delete g (copy-list (aref genemap i)))))
               (if (> from-index to-index)
                   (nconc 
                    (loop for i fixnum from from-index to lc collect
                          (get-overlapping-genes i))
                    (loop for i fixnum from 1 to to-index collect
                          (get-overlapping-genes i)))
                 (loop for i fixnum from from-index to to-index collect 
                       (get-overlapping-genes i)
                       ))))
            (block exit
              (unless (some 'null overlap-list) 
                (let ((embedding-genes (first overlap-list)))
                  (loop for gl in (cdr overlap-list) do
                        (setq embedding-genes (intersection embedding-genes gl))
                        (when (null embedding-genes) (return-from exit nil))
                        )
                  (setq 
                   embeds 
                   (nconc 
                    (loop for embedding-gene in embedding-genes collect 
                          (list g embedding-gene))
                    embeds
                    ))))))
      (loop for (embedee embedder) in embeds do 
            (push embedder (#^embedders embedee)))
      (setf (#^embedders-computed? contig) t)
      )))
                      
                     
(defun verify-overlaps ()
  (loop 
   with count = 0
   for org in (loaded-organisms) do
   (loop for gene in (#^genes org) do 
         (vwhen (ogs (#^overlaps gene))
           (incf count)
           (loop for og in ogs do
                 (unless (overlap-between og gene)
                   (cformatt 
                    (one-string-nl
                     "Gene ~A recorded as overlapping gene ~A"
                     "but OVERLAP-BETWEEN returns NIL!!")
                    gene og
                    )))))
   finally (cformatt "~D genes checked for overlap" count)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



#+test
(progn

(defun xoverlap-between (c1 c2 circular?)
  (overlap-between2 c1 c2 :lists-can-be-circular? circular?))

(deftest overlap-1
         (let ((c1 '(5 10)) (c2 '(12 15))) (xoverlap-between c1 c2 nil))
         nil :chapter :upstream :comparison 'equal)
(deftest overlap-2
         (let ((c1 '(5 10)) (c2 '(12 15))) (xoverlap-between c1 c2 t))
         nil :chapter :upstream :comparison 'equal)
(deftest overlap-3
         (let ((c1 '(12 15)) (c2 '(5 10))) (xoverlap-between c1 c2 nil))
         nil :chapter :upstream :comparison 'equal)
(deftest overlap-4
         (let ((c1 '(5 10)) (c2 '(8 15))) (xoverlap-between c1 c2 nil))
         '(8 10) :chapter :upstream :comparison 'equal)
(deftest overlap-5
         (let ((c1 '(5 10)) (c2 '(3 7))) (xoverlap-between c1 c2 nil))
         '(5 7) :chapter :upstream :comparison 'equal)
(deftest overlap-6
         (let ((c1 '(5 10)) (c2 '(6 8))) (xoverlap-between c1 c2 nil))
         '(6 8) :chapter :upstream :comparison 'equal)
(deftest overlap-7
         (let ((c1 '(5 10)) (c2 '(3 12))) (xoverlap-between c1 c2 nil))
         '(5 10) :chapter :upstream :comparison 'equal)
(deftest overlap-8
         (let ((c1 '(5 10)) (c2 '(10 12))) (xoverlap-between c1 c2 nil))
         '(10 10) :chapter :upstream :comparison 'equal)
(deftest overlap-9
         (let ((c1 '(5 10)) (c2 '(11 12))) (xoverlap-between c1 c2 nil))
         nil :chapter :upstream :comparison 'equal)
(deftest overlap-10
         (let ((c1 '(5 10)) (c2 '(5 5))) (xoverlap-between c1 c2 nil))
         '(5 5) :chapter :upstream :comparison 'equal)
(deftest overlap-11
         (let ((c1 '(5 10)) (c2 '(4 5))) (xoverlap-between c1 c2 nil))
         '(5 5) :chapter :upstream :comparison 'equal)
(deftest overlap-12
         (let ((c1 '(5 10)) (c2 '(1 4))) (xoverlap-between c1 c2 nil))
         nil :chapter :upstream :comparison 'equal)
(deftest overlap-13
         (let ((c1 '(20 5)) (c2 '(8 10))) (xoverlap-between c1 c2 t))
         nil :chapter :upstream :comparison 'equal)
(deftest overlap-14
         (let ((c1 '(20 5)) (c2 '(3 10))) (xoverlap-between c1 c2 t))
         '(3 5) :chapter :upstream :comparison 'equal)
(deftest overlap-15
         (let ((c1 '(20 5)) (c2 '(18 25))) (xoverlap-between c1 c2 t))
         '(20 25) :chapter :upstream :comparison 'equal)
(deftest overlap-16
         (let ((c1 '(20 5)) (c2 '(22 25))) (xoverlap-between c1 c2 t))
         '(22 25) :chapter :upstream :comparison 'equal)
(deftest overlap-17
         (let ((c1 '(20 5)) (c2 '(2 4))) (xoverlap-between c1 c2 t))
         '(2 4) :chapter :upstream :comparison 'equal)
(deftest overlap-18
         (let ((c1 '(6 10)) (c2 '(20 5))) (xoverlap-between c1 c2 t))
         nil :chapter :upstream :comparison 'equal)
(deftest overlap-20
         (let ((c1 '(20 5)) (c2 '(22 3))) (xoverlap-between c1 c2 t))
         '(22 3) :chapter :upstream :comparison 'equal)
(deftest overlap-21
         (let ((c1 '(20 5)) (c2 '(18 3))) (xoverlap-between c1 c2 t))
         '(20 3) :chapter :upstream :comparison 'equal)
(deftest overlap-22
         (let ((c1 '(20 5)) (c2 '(22 7))) (xoverlap-between c1 c2 t))
         '(22 5) :chapter :upstream :comparison 'equal)
(deftest overlap-23
         (let ((c1 '(20 5)) (c2 '(18 7))) (xoverlap-between c1 c2 t))
         '(20 5) :chapter :upstream :comparison 'equal)
(deftest overlap-24
         (let ((c1 '(8 17)) (c2 '(8 17))) (xoverlap-between c1 c2 t))
         '(8 17) :chapter :upstream :comparison 'equal)
(deftest overlap-25
         (let ((c1 '(17 8)) (c2 '(17 8))) (xoverlap-between c1 c2 t))
         '(17 8) :chapter :upstream :comparison 'equal)
(deftest overlap-26
         (let ((c1 '(20 5)) (c2 '(3 22))) 
           (multiple-value-list (xoverlap-between c1 c2 t)))
         (list :double-overlap '(3 5) '(20 22))
         :chapter :upstream :comparison 'equal)

)



  


#+timing
(defun test-scan (orgf n)
  #.(optimization-declaration)
  (declare (fixnum n))
  (let* ((contig (first (#^Contiguous-Sequences orgf)))
         (genes (#^Genes-Sorted-by-position contig))
         (ngenes (length genes))
         (seqlen (#^Sequence-Length contig)))
    (declare (fixnum ngenes))
    (cformatt "Scanning for positions over ~D genes" ngenes)
    (time
     (loop for j fixnum from 0 below n do
           (let ((pos (random seqlen)))
             (declare (fixnum pos))
             (loop for index fixnum from 0 below (the fixnum (1- ngenes))
                   as gene1 = (aref genes index)
                   as gene2 = (aref genes (the fixnum (1+ index)))
                   do
                   (let ((from1 (#^From gene1)))
                     (declare (fixnum from1))
                     (when (< pos from1) (return (list nil gene1)))
                     (let ((from2 (#^From gene2)))
                       (declare (fixnum from2))
                       (when (< pos from2) (return (list gene1 gene2)))
                       ))
                   finally (return (list (aref genes (1- ngenes)) nil))
                   ))))))

#+test
(defun test-sequence-upstream-from (&key (verbose? t))
  (let ((overlap-results nil)
        (adjacent-results nil)
        (erroneous-results nil)
        (count 0))
    (terpri) (terpri)
    (loop for organism in *loaded-organisms* do
          (cformatt "Testing genes from organism ~A" organism)
          (loop for gene in (#^Genes organism) do
                (incf count)
                (handler-case
                    (let ((result (sequence-upstream-from gene)))
                      (cond
                       ((null result)
                        (push (list gene result) overlap-results))
                       ((not (stringp result))
                        (error "Internal error! ~A" gene))
                       ((equal "" result)
                        (push (list gene result) adjacent-results))
                       (t nil)
                       ))
                  (error
                   (c)
                   (push (list gene c) erroneous-results)
                   ))))
    (terpri) 
    (cformatt 
     "Results of TEST-SEQUENCE-UPSTREAM-FROM on ~D genes in ~D organisms:" 
     count (length *loaded-organisms*))
    (cformatt "  ~D genes caused SEQUENCE-UPSTREAM-FROM to error out"
              (length erroneous-results))
    (cformatt "  ~D genes returned an empty string (indicating adjacency)"
              (length adjacent-results))
    (cformatt 
     "  ~D genes returned a NULL result (indicating overlap or embeddedness)"
     (length overlap-results))
    (terpri)
    (when erroneous-results
      (cformatt "The following genes caused errors:")
      (loop for e in erroneous-results do (cformatt "~A" (car e))))
    (when verbose?
      (terpri)
      (cformatt "The following genes are adjacent to their upstream partner:")
      (loop for a in adjacent-results do (cformatt "~A" (car a)))
      (terpri)
      (cformatt 
       (one-string
        "The following genes overlap with/ are embedded in "
        "their upstream partner:")
       (loop for o in overlap-results do (cformatt "~A" (car o))))
      (terpri)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Post-upload code to fabricate GO bindings if there aren't any already.
;;; This has to be manually-run by the user.  It either reloads precomputed
;;; bindings or heuristicates new ones.

(defvar *go-guesses* nil)

(defun guess-additional-go-fields-for-organism 
       (organism &key (create-if-does-not-exist? t) (force-recreate? nil))
  (let* ((seqfile (or (#^genome-sequence-file organism) 
                      (#^sequence-file organism)))
         (file (merge-pathnames "goguesses.lisp" seqfile))
         (faslfile (merge-pathnames "goguesses.fasl" seqfile)))
    (cond 
     ((and (not force-recreate?) (probe-file file))
      ;; Although we only require the .lisp to be there, we'll load either.
      ;; Compilation has to be done manually.
      (cformatt "In GUESS-ADDITIONAL... ~a exists and is being loaded..." file)
      (cond 
       ((probe-file faslfile) 
        (load faslfile))
       (t (load file)))
      (loop for (gene assignments) in *go-guesses*
            do 
	    (setf (#^go-guesses gene) assignments)
	    ;; Update the reverse pointer from the related-genes slot 
            ;; of the go frame to this gene.
	    (loop for (nil nil matching-frame) in assignments
		  do (pushnew gene
			      (#^go.related-genes matching-frame)
			      :test #'equal))
	    )
      (setq *go-guesses* nil))
     ((or create-if-does-not-exist? force-recreate?)
      (cformatt 
       "In GUESS-ADDITIONAL... ~a does not exist but is being recreated." file)
      ;; Kill the fasl if it's there!
      (when (probe-file faslfile) 
	(cformatt 
         (one-string
          "The previously existing FASL file is being deleted; "
          "You may wish to recompile the new lisp file.")
         file)
	(delete-file faslfile))
      (with-open-file 
          (o file 
             :direction :output 
             :if-exists (if force-recreate? :supersede :error))
        (format o "(in-package :bio)~%")
        (format o "(setq *go-guesses* '(~%")
        (loop for gene in (#^genes organism) for count fixnum from 0 do
              ;; Don't print it to the file if it's nil.
              (when (zerop (mod count 100)) (format t "."))
              (let ((assignments (guess-additional-go-fields gene)))
                (when assignments
                  (print (list gene assignments) o))))
        (format o "~%))~%")
        )
      (guess-additional-go-fields-for-organism organism))
     (t (cformatt 
         (one-string
          "In GUESS-ADDITIONAL... ~a does not exist, and is not being "
          "recreated because neither CREATE-IF-DOES-NOT-EXIST? nor "
          "FORCE-RECREATE? were T.")
         file))
     )))

;;; This will look at these fields, in order: 
;;; ANNOTATION, EC-DESCRIPTION, COG-DESCRIPTION, BEST-HIT-DESCR
;;; Some genes won't have some of these fields.  
;;; We get the top n matches overall.

(defvar *go-guessing-slots* 
  '(#$ANNOTATION #$EC-DESCRIPTION #$COG-DESCRIPTION #$BEST-HIT-DESCR))

;;; Result format if we're really lucky will be: 
;;; (...(slot-name score matching-frame)...)
;;; This tries to remove the source organism, which is often included 
;;; in the string descriptor of a hit in [square brackets].  
;;; This is grossly heuristic!

(defun guess-additional-go-fields (gene &key (n-to-keep 3))
  (first-n 
   n-to-keep
   (sort 
    (mapcan 
     (lambda (slot-name)
       (let ((string (slotv gene slot-name)))
         (when string 
           ;; Remove [...] to the end of the string (usually an organism name)
           ;; Need to protect against it being at the head, which will break
           ;; (Sometimes there a note at the start of the annotation entry; UUU)
           (let ((bpos (position #\[ string)))
             (when (and bpos (> bpos 5))
               (setq string (subseq string 0 (1- bpos)))))
           (mapcar #'(lambda (result)
                       (cons slot-name result))
                   (frame-fnamed-by-word-homology string)))))
     *go-guessing-slots*)
    #'>
    :key #'second)))

;;; Like frame-fnamed but uses word homology and caches the compiled
;;; fname 

(defun frame-fnamed-by-word-homology 
       (string &key (targets *go-frames*) (limit 0.6) &aux result)
  (let ((cstring (compile-word string)))
    (mapcar #'(lambda (frame)
                 (let ((score (utils::score-homology-fast cstring 
                                              (get-or-set-wh-fname frame))))
                   (when (> score limit) (push (list score frame) result))))
            targets
             )
    result))

(defun get-or-set-wh-fname (frame)
  (or (slotv frame #$compiled-fname)
      (setf (slotv frame #$compiled-fname)
            (compile-word (slotv frame #$fname)))))

;;; Postload code that connects BioCyc frames to the appropriate gene 
;;; for the organism.  This might
;;; have to change when the BioCyc model changes. 
;;;  It is very particular to the BioCyc and Organism
;;; gene naming schemes.


(defun thread-biocyc-to-genes (organism &aux (count 0))
  (loop with prefix = (#^Organism-Prefix organism)
	for polypeptide in (#^subclasses #$oc.polypeptides)
	as gname = (#^fname polypeptide)
	as cutpos = (search "-Monomer" gname)
	when cutpos
	do (let* ((gene-string-name (subseq gname 3 cutpos))
		  (target-fname (one-string prefix gene-string-name))
		  (target-gene (frame-fnamed target-fname)))
	     (when (and target-gene (eq organism (#^organism target-gene)))
	       (setf (#^oc.gene polypeptide) target-gene)
	       (setf (#^oc.polypeptide target-gene) polypeptide)
	       (incf count)
	       ))
	finally 
        (cformatt 
         "~a BioCyc polypeptide frames were threaded with ~a genes." 
         count prefix)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-close-matches (query organism max-mismatches)
  #.(one-string-nl
     "(FIND-CLOSE-MATCHES query-sequence organism max-mismatches)"
     "- Returns instances of the query-sequence found in the genome of the"
     "organism with no more than max-mismatches."
     "- Returns list in following format:"
     "(n MISMATCHES contig direction AT coordinate)"
     "sorted by number of mismatches")
  #.(optimization-declaration)
  (let ((match-set nil) (query-len (length query)))
    (declare (fixnum query-len))
    (flet ((mismatches-between (target target-from target-to)
             (declare (simple-string query target))
             (let ((count 0))
               (declare (fixnum count))
               (loop for i fixnum from 0 below query-len
                     for j fixnum from target-from to target-to do
                     (when (not (eql (schar query i) (schar target j))) 
                       (incf count))
                     (when (> count max-mismatches) (return nil))
                     finally (return count)
                     )))
         (proper-coordinate (coordinate direction target-len)
           (declare (fixnum coordinate target-len))
           (declare (simple-string direction))
           (if (string-equal direction "F")
               (the fixnum (1+ coordinate))
             (the fixnum (- target-len coordinate query-len -1))
             )))
      (loop for contig in (#^Contiguous-sequences organism) do
            (loop for dir in '("F" "B")
                  as target = (extract-sequence contig :direction dir)
                  as target-len fixnum = (length target) 
                  as extent fixnum = (the fixnum (- target-len query-len)) do
                  (loop for pos fixnum from 0 to extent
                        as target-to fixnum = (the fixnum (+ pos query-len))
                        as mismatches = (mismatches-between 
                                         target pos target-to)
                        do
                        (when mismatches
                          (push (list mismatches contig dir
                                      (proper-coordinate pos dir target)
                                      (subseq target pos target-to))
                                match-set
                                )))))
      (stable-sort 
       match-set 
       (lambda (x y)
         (cond
          ((< (first x) (first y)) t)
          ((> (first x) (first y)) nil)
          (t 
           (cond
            ((string< (#^fname (second x)) (#^fname (second y))) t)
            ((string> (#^fname (second x)) (#^fname (second y))) nil)
            (t 
             (cond
              ((< (fourth x) (fourth y)) t)
              (t nil)
              )))))))

      )))
