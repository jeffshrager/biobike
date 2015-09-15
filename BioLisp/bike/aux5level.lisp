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

;;; Author: JP Massar, Jeff Elhai, Arnaud Taton. 

(defparameter *use-new-blast-aux* t)
(defparameter *debug-blast-aux* nil)

(DEFUN Inferred-program (q-type t-type translate)
  "Returns blast flavor based on types of input"
  (COND
   ((AND translate (SYMBOL= q-type 'DNA) (SYMBOL= t-type 'DNA))
    "tblastx")
   ((AND translate (SYMBOL= q-type 'DNA) (SYMBOL= t-type 'AA))
    "blastx")
   ((AND translate (SYMBOL= q-type 'AA) (SYMBOL= t-type 'DNA))
    "tblastn")
   ((AND translate (SYMBOL= q-type 'AA) (SYMBOL= t-type 'AA))
    (ERROR "TRANSLATE flag inappropriate for comparing amino acid sequences"))
   ((AND (SYMBOL= q-type 'DNA) (SYMBOL= t-type 'DNA))
    "blastn")
   ((AND (SYMBOL= q-type 'AA) (SYMBOL= t-type 'AA))
    "blastp")
   ((AND (SYMBOL= q-type 'DNA) (SYMBOL= t-type 'AA))
    (WARN "Translating DNA sequences to allow comparison with amino acid sequences")
    "blastx")
   ((AND (SYMBOL= q-type 'AA) (SYMBOL= t-type 'DNA))
    (WARN "Translating DNA sequences to allow comparison with amino acid sequences")
    "tblastn")
   (T (ERROR "INTERNAL ERROR in Inferred-program!"))))

(DEFUN List-type-for-blast (program list query-or-target)
  "Returns type (DNA or AA) depending on input"
  (COND
   ((AND (EQUAL query-or-target "Q") 
         (MEMBER program '("blastn" "blastx" "tblastx") :TEST 'EQUAL))
    'DNA)
   ((AND (EQUAL query-or-target "Q") 
         (MEMBER program '("blastp" "tblastn") :TEST 'EQUAL))
    'AA)
   ((AND (EQUAL query-or-target "T") 
         (MEMBER program '("blastn" "tblastn" "tblastx") :TEST 'EQUAL))
    'DNA)
   ((AND (EQUAL query-or-target "T") 
         (MEMBER program '("blastp" "blastx") :TEST 'EQUAL))
    'AA)
   (T (SEQUENCE-TYPE-OF-LIST list))))


(DEFUN Make-blast-database (path targets-are-proteins?)
  (LET ((formatdb-command
         (FORMAT NIL "~Aformatdb -p ~A -i ~A >& /dev/null" 
                 cl-user::*blast-executable-toplevel-dir* 
                 targets-are-proteins? path)))
    (when *debug-blast-aux*
      (formatt "formatdb-command = ~S~%" formatdb-command))
    (CASE (PROTECTED-SHELL-COMMAND formatdb-command
                                   :ACTION-ON-INVALID-RETURN :error)
      (:Timeout (ERROR "Database generation failed due to timeout!"))
      (OTHERWISE NIL))))

; ---------------------------------------

; --------------- PLOT-aux --------------

(DEFUN Gnuplot (data &KEY (type 'lines) bin-interval min max ymin ymax xlabel ylabel
                    counts show-command)
  "General plotting"
  ; Data must be in one of the following two formats:
  ;   ((n1a n1b) (n2a n2b) ...)
  ;   (n1 n2 n3 ...)
  ; Legal types: '(lines points boxes impulses)
  ; Bin-interval: When > 0, causes binning to take place
  ;       Legal only when data is a simple list
  ; Min and max, when provided, supercedes min and max of points
 
  (UNLESS (TYPEP data 'List)
    (ERROR (S+ "Data to Gnuplot must be in the form of a "
               "list, not ~A")
        (TYPE-OF data)))
  (LET* ((types '(lines points boxes impulses))
         (sub-command NIL)
         (imin) (imax) (jmin) (jmax) (xtitle) (ytitle) (x1)(x2)(y1)(y2)
         (logx) (logy) ; eventually make these arguments
         (xrange) (xmargin) (yrange) (ymargin)
         (margin% 0.1)
         (sub-list data) 
         (command) )

    (UNLESS (MEMBER type types :TEST 'SYMBOL=)
      (ERROR (S+ "Type of graph specified to Gnuplot, '~A', is "
                 "not one of the legal types: ~A")
         type types))

    (COND
       (counts
         (SETF sub-list
            (FOR-EACH n IN sub-list
                 INITIALIZE table = (NEW-TABLE '($))
                 AS value = (REF table n)
                 DO (IF value 
                        (INCF (REF table n))
                        (SETF (REF table n) 1))
                 FINALLY 
                   (RETURN 
                      (LOOP FOR label IN (LABELS-OF table DIMENSION 1)
                            COLLECT (LIST label (REF table label)))))))
       (bin-interval
         (SETF sub-list
            (BIN-DATA-OF sub-list 
               (OR min (MIN-OF data))
               (OR max (MAX-OF data))
               bin-interval)))
       ((TYPEP sub-list 'Simple-list)
          (SETF x1 (OR min 0))
          (SETF sub-list
             (LOOP FOR item IN sub-list
                   FOR i FROM 1
                   COLLECT (LIST i item)))))

    (setf x1 (MIN-OF (BBL::FIRST IN-EACH sub-list)))
    (setf x2 (MAX-OF (BBL::FIRST IN-EACH sub-list)))
    (WHEN (AND bin-interval (SYMBOL= type 'boxes))
          (SETF x1 (- x1 (/ bin-interval 2)))
          (SETF x2 (+ x2 (/ bin-interval 2))))
    (SETF xrange (- x2 x1))
    (IF (= xrange 0)
        (SETF xrange
             (IF (= x1 0) (/ margin%) (/ x1 margin%))))
    (SETF xmargin (* margin% xrange))
    (SETF imin (OR min (- x1 xmargin)))
    (SETF imax (OR max (+ x2 xmargin)))
    (SETF xtitle (OR xlabel "x-axes"))
    (SETF ytitle (OR ylabel "y-axes"))
    (setf y1 (MIN-OF (BBL::SECOND IN-EACH sub-list)))
    (setf y2 (MAX-OF (BBL::SECOND IN-EACH sub-list)))
    (SETF yrange (- y2 y1))
    (IF (= yrange 0)
        (SETF yrange
             (IF (= y1 0) (/ margin%) (/ y1 margin%))))
    (SETF ymargin (* margin% yrange))
    (SETF jmin (OR ymin (- y1 ymargin)))
    (SETF jmax (OR ymax (+ y2 ymargin)))   
    
   (bio::WITH-TEMP-FILE-IN (filename wb::*tmp-directory* :DELETE? NIL)
     (bio::WITH-OPEN-FILE
         (data filename :DIRECTION :OUTPUT :IF-EXISTS :SUPERSEDE)
           (BBL::WRITE FROM sub-list TO filename TAB-DELIMITED)
           (PUSH (FORMAT NIL "plot '~A' with ~A notitle" 
                    filename (BB-STRING-OF type :LOWER-CASE T))
                 sub-command)
           (IF (SYMBOL= type 'boxes)
               (PUSH (FORMAT NIL "set boxwidth 0.7 relative" filename)
                     sub-command))
           (IF logx (PUSH "set logscale x" sub-command))
           (IF logy (PUSH "set logscale y" sub-command))
           (PUSH (FORMAT NIL "set xlabel '~A'" xtitle) sub-command)
           (PUSH (FORMAT NIL "set ylabel '~A'" ytitle) sub-command)
           (PUSH (FORMAT NIL "set xrange [~A:~A]" imin imax) sub-command)
           (PUSH (FORMAT NIL "set yrange [~A:~A]" jmin jmax) sub-command)
           (PUSH "set size square" sub-command)	
           (PUSH "reset" sub-command)
           (SETF command (join sub-command by *newline*))
           (IF show-command (PRINT command))
           (bio::gnuplot-commands-to-jpeg (LIST command) filename)))))

; ---------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
(DEFUN GENERATE-FILE (data)
  "Generates a file for Gnuplot use."    
  (lisp::check-type data string)
  (LET* ((filename
            (bio::NAMESTRING
                (bio::MAKE-PATHNAME :name data :type "dat"
                   :defaults (cl-user:translate-simple-lp "tmp:foo.bar"))))) 
    filename))
)

(DEFUN Multiple-plot (data &KEY xmin xmax ymin ymax xlabel ylabel)
  "Generates multiple plots on the same graph."
  (LET* ((sub-command (LIST *newline*))
         (filename "abc")
         )
    (FOR-EACH dat IN data
     FOR-EACH i FROM 1
     FOR-EACH line-type IN '(1 6 7 3 8)
       (IF-TRUE (LISTP dat)
           THEN (SETF filename (generate-file (format nil "xy~A" i)))
                (bio::WITH-TEMP-FILE-IN (data wb::*tmp-directory* :DELETE? NIL)
                 (bio::WITH-OPEN-FILE
                  (temp-file filename :DIRECTION :OUTPUT :IF-EXISTS :SUPERSEDE)
                  (BBL::WRITE FROM dat TO filename TAB-DELIMITED)
                  (PUSH (FORMAT NIL "plot '~A' with lines notitle lt ~A"
                        filename line-type) 
                        sub-command)))
           ELSE (PUSH dat sub-command))
       (PUSH *newline* sub-command))

    (IF-TRUE (AND xmin xmax)
        THEN (PUSH (FORMAT NIL "set xrange [~A:~A]" xmin xmax) sub-command)
             (PUSH *newline* sub-command))
    (IF-TRUE (AND ymin ymax)
        THEN (PUSH (FORMAT NIL "set yrange [~A:~A]" ymin ymax) sub-command)
             (PUSH *newline* sub-command))
    (IF-TRUE xlabel
        THEN (PUSH (FORMAT NIL "set xlabel '~A'" xlabel) sub-command)
             (PUSH *newline* sub-command))
    (IF-TRUE ylabel
        THEN (PUSH (FORMAT NIL "set ylabel '~A'" ylabel) sub-command)
             (PUSH *newline* sub-command))
    (PUSH "set multiplot" sub-command)
    (PUSH *newline* sub-command)
    (PUSH "reset" sub-command)   
    (bio::gnuplot-commands-to-jpeg (LIST sub-command)  "whatever")))

; ---------------------------------------


(DEFUN Sequences-similar-to-with-lookup
       (query target threshold return)
  (LET* ((best-hit) (best-eval) (temp-hits)
         (protein (IF (IS-GENE? query) 
		              (FIRST (REF query #$proteins))
					  query))
         (singlet?)
         (target-organism
          (TYPECASE target
            ((OR Contiguous-sequence Protein)
             (SLOTV target #$ORGANISM))
            (Organism target)
            (T (ERROR "Internal error!"))))
         (hits-in-organism
          (PROGN
            (MULTIPLE-VALUE-SETQ (best-hit best-eval temp-hits)
                (BEST-BLAST-ORTHOLOG-OF protein target-organism 
                                        threshold))
            temp-hits))
         (hits
          (TYPECASE target
            (Organism hits-in-organism)
            (Contiguous-sequence
             (LOOP FOR hit IN (ENSURE-LIST hits-in-organism)
                   AS protein = (FIRST hit)
                   AS gene = (SLOTV protein #$GENE)
                   AS contig = (SLOTV gene #$CONTIGUOUS-SEQUENCE)
                   WHEN (EQUAL contig target)
                   COLLECT hit))
            (Protein
             (SETF singlet? T)
             (LOOP FOR hit IN (ENSURE-LIST hits-in-organism)
                   AS protein = (FIRST hit)
                   WHEN (EQUAL protein target)
                   COLLECT hit)))))
    (DECLARE (IGNORABLE best-hit best-eval temp-hits))
      
    (COND
     (singlet? (FIRST hits))
     (return (SUBSEQ hits 0 (MIN return (LENGTH hits))))
     (T hits))))

(DEFUN Sequences-similar-to-with-mismatches
       (query target mismatches-allowed return seq-type &KEY one-strand no-label)
  (LABELS 
      ((Mismatches-between (query target target-from target-to max-mismatches-allowed)
         (DECLARE (Simple-string query target))
         (DECLARE (Fixnum max-mismatches-allowed))
         (DECLARE (Optimize (speed 3) (safety 0) (debug 0)))
         (LET ((count 0))
           (DECLARE (Fixnum count))
           (LOOP FOR i Fixnum FROM 0 BELOW (LENGTH query)
                 FOR j Fixnum FROM target-from TO target-to 
                 DO (WHEN (NOT (EQL (SCHAR query i) (SCHAR target j)))
                      (INCF count))
                 (WHEN (> count max-mismatches-allowed)
                   (RETURN))
                 FINALLY (RETURN count))))

       (Proper-coordinate (coordinate direction target query)
         (DECLARE (FIXNUM coordinate))
         (DECLARE (SIMPLE-STRING direction target query))
         (IF-TRUE (STRING-EQUAL direction "F")
                  THEN (1+ coordinate)
                  ELSE (- (LENGTH target) coordinate (LENGTH query) -1)))

       (Matches-in-target (query target)
        (DECLARE (SIMPLE-STRING query target))
        (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0) (DEBUG 0)))
         (LET* ((directions 
                   (COND
                      (one-strand '("F"))
                      ((TYPEP target 'Protein) '("F"))
                      (T '("F" "B"))))
               (matches
                (LOOP 
                 FOR direction IN directions
                 AS current-target = 
                 (IF-TRUE (EQUAL direction "F")
                          THEN target
                          ELSE (INVERSION-OF target))
                 APPEND
                 (LOOP
                  FOR pos FROM 0 TO (- (LENGTH current-target) (LENGTH query))
                  AS target-to = (+ pos (LENGTH query))
                  AS mismatches = 
                  (Mismatches-between query current-target pos 
                                      target-to mismatches-allowed)
                  WHEN mismatches
                  COLLECT (LIST mismatches direction 
                                (PROPER-COORDINATE pos direction 
                                                   current-target query)
                                (SUBSEQ current-target pos target-to)))))) 
           matches))
     
       (Sort-matches (match-set return)
         (LET ((list-length (LENGTH match-set)))
           (SUBSEQ 
            (STABLE-SORT
             match-set
             #'(LAMBDA (x y)
                 (COND 
                  ((< (FIRST x) (FIRST y)) T)
                  ((> (FIRST X) (FIRST y)) NIL)
                  (T (COND
                      ((< (THIRD x) (THIRD y)) T)
                      (T NIL))))))
            0 (MIN (OR return *big-number*) list-length))))

       (Matches-in-target-set (query-seq target set-type return)
         (LET ((set
                (CASE set-type
				  (List target)
                  (Gene (GENES-OF target))
                  (Protein (PROTEINS-OF target))
                  (Contigs (CONTIGS-OF target)))))
           (IF set
               (LOOP FOR item IN set
                     AS target-seq = (SEQUENCE-OF item DISPLAY-OFF) ; (EXTRACT-SEQUENCE item)
                     AS matches = (MATCHES-IN-TARGET query-seq target-seq)
                     AS item-name = (IF (IS-FRAME? item) item (NAME-OF item))
                     WHEN matches
                     COLLECT (LIST item-name (SORT-MATCHES matches return)))
             (SORT-MATCHES 
              (MATCHES-IN-TARGET query-seq (EXTRACT-SEQUENCE target))
              return))))
       ) ; *** END LABELS
       
    (LET* ((query-seq
            (TYPECASE query
              (String query)
              (Labeled-sequence (LABELED-SEQUENCE-SEQUENCE query))
              ((OR Gene Protein) (EXTRACT-SEQUENCE query))
              (OTHERWISE (ERROR "Query must be a sequence, not '~S'"
                                query))))
           (target-seq
            (TYPECASE target
              (String target)
              (Labeled-sequence (LABELED-SEQUENCE-SEQUENCE target))
              ((OR Gene Protein Contiguous-sequence) (EXTRACT-SEQUENCE target))
              (Organism NIL)
              (List target)
              (OTHERWISE (ERROR "Target must be a sequence, not '~S'"
                                target))))
           (match-set
            (TYPECASE target
              ((OR Gene Protein String Labeled-sequence Contiguous-sequence)
               (LET* ((target-name
                       (COND
                          ((TYPEP target 'String)
                             (IF-TRUE (> (LENGTH target) 15)
                                 THEN (CONCATENATE 'STRING 
                                         (SUBSEQ target 0 15) "...")
                                 ELSE target))
                          ((TYPEP target 'Labeled-sequence)
                             (LABELED-SEQUENCE-LABEL target))
                          (T target)))
                      (hits
                       (MATCHES-IN-TARGET query-seq target-seq)))
                 (IF hits (SETF hits (SORT-MATCHES hits return)))
                 (IF no-label
                     hits
                     (LIST target-name hits))))
              (Organism (MATCHES-IN-TARGET-SET query-seq target (OR seq-type 'Contigs) return))
              (List (MATCHES-IN-TARGET-SET query-seq target (OR seq-type 'List) return))
              (T (ERROR "Target must be a sequence, not '~S'"
                        target)))))
      match-set)))

; =============== DINUCLEOTIDE-COMPARISON-aux ==========
(DEFUN dinucleotide-bias-aux (entity &KEY labeled label-biases one-strand)
  (LET* ((sequence "")
         (length NIL)
         (name NIL)
         (counts NIL)
         (freqs NIL)
         (result NIL)
         (dinucleotides 
           (LIST "AA" "AC" "AG" "CA" "CC" "GA" 
                 "TT" "GT" "CT" "TG" "GG" "TC" 
                 "AT" "CG" "GC" "TA"))
         (alpha-order '(1 2 3 13 4 5 14 9 6 15 11 8 16 12 10 7))
        )

  (without-code-walker
    (WHEN (TYPEP entity 'Organism)
          (SETF name (NAME-OF entity SHORT))
          (SETF entity (CONTIGS-OF entity)))
    (COND
       ((AND (LISTP entity) (= (LENGTH entity) 16) 
             (ALL-TRUE (MAPCAR 'NUMBERP entity)))
          (SETF name "unknown")
          (SETF result entity))
       ((AND (LISTP entity) (= (LENGTH entity) 2)
             (LISTP (SECOND entity))
             (ALL-TRUE (MAPCAR 'NUMBERP (SECOND entity))))
          (SETF name (FIRST entity))
          (SETF result (SECOND entity)))
       ((ALL-TRUE 
          (MAPCAR (LAMBDA (ent)
                     (ANY-TRUE (MAPCAR (LAMBDA (type) (TYPEP ent type)) 
                                  '(organism protein gene contiguous-sequence 
                                    labeled-sequence string))))
                  (ENSURE-LIST entity)))
          (ASSIGN sequence = (SEQUENCE-OF entity))
          (SETF name 
              (COND 
                 (name name)
                 ((LISTP entity) "aggregate")
                 (T (NAME-OF entity SHORT))))
          (SETF counts
            (COND
               ((AND (LISTP sequence) (= (LENGTH sequence) 1))
                  (FIRST (COUNTS-OF dinucleotides IN-EACH sequence)))
               ((LISTP sequence)
                  (SUM-OF (COUNTS-OF dinucleotides IN-EACH (FLATTEN sequence))))
               (T (COUNTS-OF dinucleotides IN sequence))))
          (SETF length (SUM-OF counts))
          (WHEN (NOT one-strand)
                (SETF (SUBSEQ counts 0 6)
                      (LOOP FOR dinuc1 IN (REF counts 1 -> 6)
                          FOR dinuc2 IN (REF counts 7 -> 12)
                          COLLECT (/ (+ dinuc1 dinuc2) 2)))
                (SETF (SUBSEQ counts 6 12) (SUBSEQ counts 0 6)))
          (SETF freqs (DIVIDE counts BY length))
          (SETF result
            (LOOP FOR freq IN freqs
                  FOR dinuc IN dinucleotides
                  WITH bg-freqs 
                     = (INTERLEAVE '("A" "C" "G" "T") 
                            (IF one-strand
                               (BACKGROUND-FREQUENCIES-OF sequence)
                               (BACKGROUND-FREQUENCIES-OF sequence BOTH-STRANDS)))
                  AS nuc1 = (REF dinuc 1)
                  AS nuc2 = (REF dinuc 2)
                  AS nuc1-freq = (SECOND (ASSOC nuc1 bg-freqs :TEST 'EQUAL))
                  AS nuc2-freq = (SECOND (ASSOC nuc2 bg-freqs :TEST 'EQUAL))
                  AS bias = (IF (OR (= nuc1-freq 0) (= nuc2-freq 0))
                                1
                                (/ freq nuc1-freq nuc2-freq))
                  COLLECT bias))
          (IF label-biases
              (SETF result (INTERLEAVE dinucleotides result)))
          (SETF result (REF result alpha-order)))
       (T
          (ERROR "Bad entity type given to DINUCLEOTIDE-BIAS-aux")))
        
     (IF labeled
         (LIST (OR name entity) result)
         result))))

(DEFUN Dinucleotide-comparison-aux (query-biases target-biases)
  (without-code-walker
    (LET ((sum-of-diffs
             (LOOP FOR qbias IN query-biases
                   FOR tbias IN target-biases
                   SUM (ABS (- tbias qbias)))))
      (/ sum-of-diffs 16))))

; =============== CODON-FREQUENCIES-OF-aux ==========

(defparameter *use-new-codon-frequencies-of* t)

(DEFUN Codon-frequencies-of-aux 
       (entity absolute labeled label-frequencies sort-by-codon)
  (if *use-new-codon-frequencies-of*
      (codon-frequencies-of-aux1 
       entity absolute labeled label-frequencies sort-by-codon)
    (without-code-walker
      (LET* ((label entity)
         ; ****** Crude hack follows
             (entity 
              (COND
               ((EQUAL entity #$NC_008211.gene0042) NIL)              
               ((EQUAL entity #$ranid_herpesvirus_1)
                (REMOVE
                 #$NC_008211.gene0042 (CODING-GENES-OF entity) :TEST 'EQUAL))
               (T entity)))
             (sequence-list 
              (BBL::SECOND 
                IN-EACH (SEQUENCE-LIST-OF (ENSURE-LIST entity) 
                                          :SEQ-TYPE 'CODING-GENE)))
             (codon-list (SIMPLIFY-LIST (SPLIT sequence-list EVERY 3)))
             (total-codons (LENGTH codon-list))
             (results
              (progn
                (FOR-EACH 
                 aa IN *amino-acids*
                 AS codons = (aa-to-codons aa)
                 AS codon-counts = (COUNTS-OF codons IN codon-list)
                 AS pre-codon-counts-sum = (SUM-OF codon-counts)
                 AS codon-counts-sum
                 = (COND
                    (absolute total-codons)
                    ((= pre-codon-counts-sum 0) 1)
                    (T pre-codon-counts-sum))
                 AS codon-freqs 
                 = (DIVIDE codon-counts 
                     BY codon-counts-sum)
                 (IF (OR label-frequencies sort-by-codon)
                     (SETF codon-freqs
                           (INTERLEAVE codons codon-freqs)))
                 COLLECT codon-freqs)))
             )
        (IF sort-by-codon
            (LET ((sorted-results
                   (SETF results (BBL::SORT (APPLY 'APPEND results)))))
              (SETF results
                    (IF label-frequencies
                        sorted-results
                      (MAPCAR 'SECOND sorted-results)))))
        (IF labeled
            (LIST label results)
          results) 
        ))))

(defun codon-frequencies-of-aux1
       (entity absolute labeled label-frequencies sort-by-codon)
  (without-code-walker
    (let* ((label entity)
         ; ****** crude hack follows
           (entity 
            (cond
             ((equal entity #$nc_008211.gene0042) nil)              
             ((equal entity #$ranid_herpesvirus_1)
              (remove
               #$nc_008211.gene0042 (coding-genes-of entity) :test 'equal))
             (t entity)))
           (sequence-list 
            (bbl::second 
              in-each (sequence-list-of (ensure-list entity) 
                                        :seq-type 'coding-gene)))
           (codon-list (simplify-list (split sequence-list every 3)))
           (results
            (multiple-value-bind (frequency-table total-count)
                (codon-frequency-table codon-list)
              (create-codon-frequencies 
               frequency-table total-count absolute
               label-frequencies sort-by-codon
               ))))
      (if labeled (list label results) results)
      )))

(defun codon-to-index (codon)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (block exit
    (loop for j fixnum from 0 below 3
          with multiplier fixnum = 1
          with code fixnum = 0 
          as ch = (schar codon j)
          do
          (incf 
           code 
           (the fixnum
                (* multiplier 
                   (the fixnum
                        (case ch
                          ((#\A #\a) 0)
                          ((#\C #\c) 1)
                          ((#\G #\g) 2)
                          ((#\T #\t) 3)
                          (otherwise (return-from exit -1))
                          )))))
          (setq multiplier (the fixnum (* 4 multiplier)))
          finally (return code)
          )))
        
(defun index-to-codon (index)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((code1 (logand index #b11))
        (code2 (ash (logand index #b1100) -2))
        (code3 (ash (logand index #b110000) -4)))
    (declare (fixnum code1 code2 code3))
    (flet ((convert (x) (ecase x (0 "A") (1 "C") (2 "G") (3 "T"))))
      (s+ (convert code1) (convert code2) (convert code3))
      )))
       

(defun codon-frequency-table (list-of-triples)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((table (make-array '(64) :element-type '(unsigned-byte 28)
                           :initial-element 0)))
    ;; 28 inline codes for allegro for windows and linux, 
    ;; 32 does not inline for windows 
    (declare (type (simple-array (unsigned-byte 28) (*)) table))
    (loop for triple in list-of-triples 
          for count fixnum from 1
          as index fixnum = (codon-to-index triple) 
          do
          (unless (minusp index)
            (incf (the fixnum (aref table (the fixnum index)))))
          finally (return (values table count))
          )))

(defun create-codon-frequencies
       (table table-sum absolute? associate-codon? sort?)
  (let ((data 
         (loop
          for aa in *amino-acids*
          as codons = (aa-to-codons aa)
          as denominator = 
          (if absolute? 
              table-sum 
            (loop for codon in codons 
                  as index = (codon-to-index codon)
                  sum (aref table index)
                  ))
          collect
          (loop for codon in codons 
                as index = (codon-to-index codon)
                as codon-count = (aref table index)
                as frequency = 
                (if (zerop denominator) 0.0 (float (/ codon-count denominator)))
                collect
                (if (or associate-codon? sort?)
                    (list codon frequency) 
                  frequency
                  )))))
    ;; (print data)
    (when sort? (setq data (loop for datum in data nconc datum)))
    (cond
     ((and associate-codon? sort?) (sort data 'string-lessp :key 'first))
     (associate-codon? data)
     (sort? (mapcar 'second (sort data 'string-lessp :key 'first)))
     (t data)
     )))

; =============== CODON-FREQUENCY-COMPARISON-aux ==========

(DEFUN Codon-frequency-comparison-aux (query-freqs target-freqs aa-freqs)
   (FOR-EACH aa-freq IN aa-freqs
    FOR-EACH q-aa-set IN query-freqs
    FOR-EACH t-aa-set IN target-freqs
        AS sum-diffs 
          = (FOR-EACH q-freq IN q-aa-set
             FOR-EACH t-freq IN t-aa-set
                 SUM (ABS (- q-freq t-freq)))
        SUM (* aa-freq sum-diffs)))

; ================= SAM functions =====================

(DEFUN EXPRESSION-VALUES-OF (group-indices gene-data)
 "takes two sets of indices and a list of expression"
  (LET* ((group1-indices (FIRST group-indices))
         (group2-indices (SECOND group-indices))
         (expression-values (SUBLIST-OF gene-data FROM 2))
         (group1 (SUBLIST-OF expression-values ITEM group1-indices))
         (group2 (SUBLIST-OF expression-values ITEM group2-indices))
        )
    (LIST group1 group2)))
  
  
  
(DEFUN gene-specific-scatter (set1 set2)
 "Calculates standard deviation as defined in Tusher et al 2001"
 (LET* ((mean1 (MEAN set1))
        (mean2 (MEAN set2))
        (m (LENGTH set1))
        (n (LENGTH set2))
        (a (/ (+ (/ m) (/ n)) (+ m n -2)))
        (factor1 
          (FOR-EACH xm IN set1
               AS diff = (- xm mean1)
               SUM (* diff diff)))
        (factor2
          (FOR-EACH xn IN set2
               AS diff = (- xn mean2)
               SUM (* diff diff)))
       )
    (SQRT (* a (+ factor1 factor2)) )))
  


(DEFUN SigGenes (NoSigList th)
 "Finds the significant genes and returns a list along with names and scores sorts it descending"
  (LET* ((SigLIST 
           (FOR-EACH GeneSet IN NoSigList                     
                INITIALIZE threshold = th
                AS ExpectedD = (SECOND GeneSet)
                AS ObservedD = (FIRST (FIRST GeneSet))
                AS GeneName = (SECOND (FIRST GeneSet)) 
                AS sig = (ABS (- ObservedD ExpectedD))
                WHEN (> sig threshold)   
                  COLLECT (LIST sig GeneName ExpectedD ObservedD)))
        )
    (bbl::SORT SigList DESCENDING)
  ))

(DEFUN Return-indices (gene-data)
 "Takes the first gene of a data set and creates 2 sets of indices to randomize"
  (LET* ((+length+ (- (length (rest gene-data)) 2))
         (list1 (loop for i upto (/ +length+ 2) collect (+ i 1)))
         (list2 (loop for i upto (/ +length+ 2) collect (+ (+ i (/ +length+ 2)) 2)))
        )
    (LIST list1 list2)
  ))


(DEFUN LISTd (group-indices user-data)
 "Combines data into lists then sorts"
    (FOR-EACH gene-data IN user-data
         AS d-value = (*(D-OF (EXPRESSION-VALUES-OF group-indices gene-data)) -1)
         COLLECT (LIST d-value(REF gene-data 1))))


(DEFUN D-OF (groups &KEY (s0 3.3))
 "Calculates d as a function of two groups of"
 (LET* ((group1 (FIRST groups))
        (group2 (SECOND groups))
        (mean1 (MEAN group1))
        (mean2 (MEAN group2))
       )
    (/ (- MEAN1 MEAN2)
       (+ (GENE-SPECIFIC-SCATTER GROUP1 GROUP2)
            S0))))


(DEFUN Dei (user-groups USER-data)
 "Sorts expected scores"
  (LET* ((list
           (TRANSPOSE-LIST
              (FOR-EACH group-indices IN user-groups
                   AS rel-diffs 
                     = (APPLY-FUNCTION 
                          (D-OF (EXPRESSION-VALUES-OF GROUP-INDICES Udata))
                          REPLACING Udata
                          WITH USER-data)
                   COLLECT rel-diffs)))
          )
    (FOR-EACH sublist IN List
         COLLECT (MEAN sublist))
  ))


;; =============== write microarray description file ===================

(defun make-mouseover-box (file-path file-name)
(LET* ((file-content (bio::FILE-TO-STRING-LIST file-path))
(content-for-box 

(FOR-EACH s IN file-content 
  FOR-EACH n FROM 1 TO 10
  INITIALIZE in-box = NIL
  AS 100first = (BBL::FIRST 50 s)
  AS 100first+ = (JOIN 100first "&lt;br&gt;")
  DO (PUSH 100first+ in-box)
  FINALLY (RETURN (JOIN (REVERSE in-box)))))
(mouseover-link (format nil "~a~a~a~a~a~a~a"
                    "<a onmouseover=\"popup('" file-name "&lt;br&gt;" content-for-box "','lightgreen')\" ;=\"\" onmouseout=\"kill()\">" file-name "</a>"))) mouseover-link))


(defun read-user-marray-directory (upload-directory)
(LET* ((file-paths (directory (format nil "~a~a" upload-directory "/*")))
         (list-of-links-and-nb
          (FOR-EACH file-path IN  (BBL::SORT file-paths)
            INITIALIZE divided-l = (ROUND (/ (LENGTH-OF file-paths) 3))
            INITIALIZE n = 0
            INITIALIZE file-nb = 1

;           INITIALIZE path-p1 = (FORMAT NIL "~a~a~a~a~a" "javascript:poptastic('http://" cl-user::*WEBLISTENER-MACHINE-NAME* ":" wb::*current-weblistener-port* "/Topdir1/")
            AS file-name = (BBL::LAST (SPLIT (STRING-OF file-path) EVERY "/"))
            AS formouseover = (make-mouseover-box file-path file-name)

;           AS file-path-p2 = (BBL::LAST (SPLIT (STRING-OF file-path) AT "biobikewww"))
;           AS web-file-path = (FORMAT NIL "~a~a" path-p1 file-path-p2)
;           AS link-file = (FORMAT NIL "~a~a~a~a~%~a~%"   "<a href=\"" web-file-path "')\"> " file-name "</a>&nbsp;&nbsp; => &nbsp;&nbsp; ")
            
            AS link-file = formouseover
            AS new-col = (IF (= n (- divided-l 1)) "</td><td>" "")
            AS link-file-nb = (FORMAT NIL "~a~a~a~a~a" link-file "&nbsp;&nbsp; => &nbsp;&nbsp; " file-nb "&nbsp;&nbsp;<br>" new-col)
            DO 
            (INCF n) (WHEN (= n divided-l) (ASSIGN n = 0))
            (INCF file-nb)
            COLLECT link-file-nb))
        (list-in-a-table (JOIN "<table cellpadding=\"5\"> <thead valign=\"top\"> <tr><td>"  list-of-links-and-nb "</tr></table>"))
        (file-listing (JOIN (FLATTEN (LIST " <table cellpadding=\"5\"> <tr><td width='700' BGCOLOR='#EFEFEF' style='font-size:14.0pt; color:#006699'> DIRECTORY CONTENT (files vs numbers)</td></tr></table>" list-in-a-table))))) 
 file-listing))


(defun marray-form-header (upload-directory)
  
  (format nil "~a~%~a~%~a~%~a~%~a~%"
"<?php
function stripFormSlashes($arr) {
if (!is_array($arr)) {
return stripslashes($arr);
} else {
return array_map('stripFormSlashes', $arr);
}
}
    
if (get_magic_quotes_gpc()) {
$_GET  = stripFormSlashes($_GET);
$_POST = stripFormSlashes($_POST);
}
?>
"    
"
<head>
<title>Microarray Description Form</title>

<style type=\"text/css\">
<!--
#dek {POSITION:absolute;VISIBILITY:hidden;Z-INDEX:200;}
//-->
</style>

<div style=\"visibility: visible; left: 200px; top: 200px; display: none;\" id=\"dek\"><table bgcolor=\"lightgreen\" border=\"1\" bordercolor=\"black\" cellpadding=\"2\" cellspacing=\"0\" width=\"350\"><tbody><tr><td align=\"center\"><font color=\"black\" size=\"2\">After *EXECUTION-TIMELIMIT* (seconds) are reached for a given expression, a break occurs. (valid range???)</font></td></tr></tbody></table></div>

<script type=\"text/javascript\"> 
<!--
function poptastic(url)
{
newwindow=window.open(url,'name','height=300,width=600');
if (window.focus) {newwindow.focus()}
}
//-->
</script>

<script type=\"text/javascript\">
<!--
Xoffset=-60;    // modify these values to ...
Yoffset= 20;    // change the popup position.

var old,skn,iex=(document.all),yyy=-1000;
var ns4=document.layers
var ns6=document.getElementById&&!document.all
var ie4=document.all

if (ns4)
skn=document.dek
else if (ns6)
skn=document.getElementById(\"dek\").style
else if (ie4)
skn=document.all.dek.style
if(ns4)document.captureEvents(Event.MOUSEMOVE);
else{
skn.visibility=\"visible\"
skn.display=\"none\"
}
document.onmousemove=get_mouse;

function popup(msg,bak){
var content=\"<TABLE  WIDTH=350 BORDER=1 BORDERCOLOR=lightgreen CELLPADDING=2 CELLSPACING=0 BGCOLOR=lightgreen><TD ALIGN=left><FONT COLOR=black SIZE=2>\"+msg+\"</FONT></TD></TABLE>\";
yyy=Yoffset;
 if(ns4){skn.document.write(content);skn.document.close();skn.visibility=\"visible\"}
 if(ns6){document.getElementById(\"dek\").innerHTML=content;skn.display=''}
 if(ie4){document.all(\"dek\").innerHTML=content;skn.display=''}
}

function get_mouse(e){
var x=(ns4||ns6)?e.pageX:event.x+document.body.scrollLeft;
skn.left=x+Xoffset;
var y=(ns4||ns6)?e.pageY:event.y+document.body.scrollTop;
skn.top=y+yyy;
}

function kill(){
yyy=-1000;
if(ns4){skn.visibility=\"hidden\";}
else if (ns6||ie4)
skn.display=\"none\"
}

//-->
</script>
</head>

<body>"

    (read-user-marray-directory upload-directory)

    "<br> <table cellpadding=\"5\"> <tr><td width='700' BGCOLOR='#EFEFEF' style='font-size:14.0pt; color:#006699'> CONDITION DESCRIPTIONS vs FILES (given numbers)</td></tr></table>"

    "<?php
    if (isset($_POST['Submit']) &&")
    )


(defun Marray-Form-Body-1 (nb-conditions)   ; nb-bioreplicates
  (bio::string-join (flatten
                      (FOR-EACH condition FROM 1 TO nb-conditions
                       FOR-EACH condcount IN "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
;                       INITIALIZE condcount = 1
                        AS condition_field = (format nil "~a~a" "condition_field" condcount)
                        AS condition_string = (format nil "~a~a~a~%~a~a~a~%"
                                                "isset($_POST['" condition_field "']) &&"
                                                "trim($_POST['" condition_field "']) != '' &&")
                        AS GvsR_field = (format nil "~a~a~a" "GvsR_field" condcount 1)
                        AS RvsG_field = (format nil "~a~a~a" "RvsG_field" condcount 1)
                        AS condition-letter-vs-nb-conditions = (bio::NTH (- nb-conditions 1) (SPLIT "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
                        AS nb-cond-and-repl = (FORMAT NIL "~a~a" condition-letter-vs-nb-conditions 1)
                        AS countcount = (FORMAT NIL "~a~a" condcount 1)
                        AS andand = (IF (SAME countcount nb-cond-and-repl) "" "&&")


                        AS replicate_string = (format nil "~a~a~a~%~a~a~a~%~a~a~a~%~a~a~a~a~%"
                                           "(isset($_POST['" GvsR_field "']) &&"
                                           "trim($_POST['" GvsR_field "']) != '' ||"
                                           "isset($_POST['" RvsG_field "']) &&"
                                           "trim($_POST['" RvsG_field "']) != '') " andand)
                        

#|
                        AS replicate_string = (FOR-EACH replicate FROM 1 TO nb-bioreplicates
                                                INITIALIZE replcount = 1
                                                INITIALIZE nb-cond-and-repl = (FORMAT NIL "~a~a" nb-conditions nb-bioreplicates)
                                                AS countcount = (FORMAT NIL "~a~a" condcount replcount)
                                                AS andand = (IF (SAME countcount nb-cond-and-repl) "" "||")
                                                AS GvsR_field = (format nil "~a~a~a" "GvsR_field" condcount replcount)
                                                AS RvsG_field = (format nil "~a~a~a" "RvsG_field" condcount replcount)
                                                AS repl_string = (format nil "~a~a~a~%~a~a~a~%~a~a~a~%~a~a~a~a~%"
                                                                   "isset($_POST['" GvsR_field "']) ||"
                                                                   "trim($_POST['" GvsR_field "']) != '' ||"
                                                                   "isset($_POST['" RvsG_field "']) ||"
                                                                   "trim($_POST['" RvsG_field "']) != '' " andand)
                                                DO
                                                (INCF replcount)
                                                COLLECT repl_string)
|#
;                        DO
;                       (INCF condcount)
                        COLLECT (JOIN condition_string replicate_string)))))


(defun user-microarray-directory (upload-directory)
  (bio::pathname-in-directory-form
    (format nil "~a~a"
      (wb::visitor-directory *username*) upload-directory)))


(defun Marray-Form-Body-2 (upload-directory-path)
  (format nil "~a~a~a~a~%~a~%~a"
    "     ) {
  echo '<h1>Thank you for filling out this form!</h1>';
  $data = '';
  if ($data != '') {
  $data = unserialize($data);
  }
  $data[] = $_POST;
  file_put_contents('" upload-directory-path "php-description-file.txt" "', serialize($data));
  } else {
  ?>"
"<table cellpadding=\"5\"> <tr><td width=\"700\"> Please give a short description for each condition and fill in the replicate table using the
      numbers given above. You may enter several numbers within the same field. For an example please click here.</td><tr></table>"
  "<table cellpadding=\"5\">"))



(defun Marray-Form-Body-3 (nb-conditions nb-bioreplicates)
  (bio::string-join
   (flatten
    (FOR-EACH condition FROM 1 TO nb-conditions
              FOR-EACH condcount IN "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              ;; INITIALIZE condcount = 1
              AS condition_field = (format nil "~a~a" "condition_field" condcount)
              AS condition_string = (format nil "~a~%~a~a~%~a~%~a~%~a~a~a~%~a~%~a~a~a~a~a~%~a~%~a~%~a~%"
                                            "<tr><td colspan=\"3\"> </td></tr><tr><td width=\"200\" BGCOLOR='#EFEFEF' style='color:#006699'>"
                                            "CONDITION " condcount
                                            "</td><td colspan=\"2\" BGCOLOR='#EFEFEF'>"
                                            "  <form method=\"post\" action=\"<?php echo htmlspecialchars($_SERVER['PHP_SELF']);  ?>\">"
                                            "    <input type=\"text\" name=\"" condition_field "\""
                                            "      value=\"<?php"
                                            "  echo (isset($_POST['" condition_field "'])) ? htmlspecialchars($_POST['" condition_field "']) : '';"
                                            "      ?>\" size=\"54\"/>"
                                            "</td></tr>"
                                            "<tr><td > </td> <td > Green control vs Red experiment </td><td >Red control vs Green experiment</td></tr>")
              AS replicate_string = (FOR-EACH replicate FROM 1 TO nb-bioreplicates
                                              INITIALIZE replcount = 1
                                              AS GvsR_field = (format nil "~a~a~a" "GvsR_field" condcount replcount)
                                              AS RvsG_field = (format nil "~a~a~a" "RvsG_field" condcount replcount)
                                              AS repl_string = (format nil "~a~%~a~a~%~a~%~a~%~a~a~a~%~a~%~a~a~a~a~a~%~a~%~a~%~%~a~%~a~%~a~a~a~%~a~%~a~a~a~a~a~%~a~%~a~%"
                                                                       "<tr><td>"
                                                                       "Biological replicate " replcount
                                                                       "</td><td>"
                                                                       "  <form method=\"post\" action=\"<?php echo htmlspecialchars($_SERVER['PHP_SELF']);  ?>\">"
                                                                       "    <input type=\"text\" name=\"" GvsR_field "\""
                                                                       "      value=\"<?php"
                                                                       "  echo (isset($_POST['" GvsR_field "'])) ? htmlspecialchars($_POST['" GvsR_field "']) : '';"
                                                                       "      ?>\" size=\"25\"/>"
                                                                       "</td>"
                                                                   
                                                                       "<td>"
                                                                       "  <form method=\"post\" action=\"<?php echo htmlspecialchars($_SERVER['PHP_SELF']);  ?>\">"
                                                                       "    <input type=\"text\" name=\"" RvsG_field "\""
                                                                       "      value=\"<?php"
                                                                       "  echo (isset($_POST['" RvsG_field "'])) ? htmlspecialchars($_POST['" RvsG_field "']) : '';"
                                                                       "      ?>\" size=\"25\"/><br/>"
                                                                       "</td></tr>")
                                              DO
                                              (INCF replcount)
                                              COLLECT repl_string)
              DO
              (progn nil)
              ;;(INCF condcount)
              COLLECT (JOIN condition_string replicate_string))
    )))

(Defun Marray-Form-Body-4 ()
  (format nil "~a"
    "</table>
    <br />
    <input type=\"submit\" name=\"Submit\" />
    </form>
    <?php
    }
    ?>
    </body>
    "))

(defun write-and-show-php-form-etc (file-content-string)
(bio::WITH-TEMP-FILE-IN 
   (php-form-file cl-user::*WEBTMP-DIRECTORY* :prefix "microarray-desc-form" :type "php" :delete? NIL)
      (WITH-OPEN-FILE (file-content-stream php-form-file :direction :output :if-exists :supersede)
              (FORMAT file-content-stream "~a" file-content-string))
   (let ((php-form-file-name (BBL::LAST (BBL::SPLIT (STRING-OF php-form-file) EVERY "/")))) 
(WB::MAKE-URL :PATH  (wb::webtmp-url php-form-file-name) :DISPLAY-STRING "Fill in microarray description form" :TARGET "_blank")
)))


(defun display-blast-aux (blast-table labels sort-by full)
  (let* ((right-justifieds?
          '(NIL T T T NIL T T T T NIL NIL))
         (head-format "")
         (lines (LABELS-OF blast-table DIMENSION 1))
         (line-col-width (+ 1 (LENGTH (STRING-OF (LENGTH lines)))))
         (new-labels nil)
         (formats nil)
         (max-lengths nil)
         (extracted-lines NIL) 
         (sorted-lines)
         (key-position)
         (sequences '("Q-SEQ" "T-SEQ")))
    
    (IF full
        (LOOP FOR label IN (LABELS-OF blast-table DIMENSION 2)
          DO (IF-TRUE (NOT (MEMBER label labels :TEST 'EQUAL))
                      THEN (ASSIGN labels = (JOIN labels label AS-LIST))
                      (ASSIGN right-justifieds? 
                              = (JOIN right-justifieds? T AS-LIST)))))
    (WHEN sort-by
        (SETF key-position (POSITION-OF (STRING-OF sort-by) IN labels))
        (IF (NOT key-position)
            (ERR+ PROBLEM "SORT-BY '~A' specification does not match "
                  INDENT  "a legal label:"
                  INDENT  "~A"
                  FORMAT-ARGS sort-by labels))) 
    
    (ASSIGN max-lengths =
            (LOOP FOR label IN labels
              COLLECT
              (LOOP FOR line IN lines
                AS value = (utils::ref blast-table line label)
                AS string = (STRING-OF value)
                AS len = (LENGTH string)
                DO (IF (AND (NOT (CALLED-FROM-VPL)) (IS-FRAME? value))
                       (INCF len 2))
                WHEN string 
                MAXIMIZE len)))
            
    (ASSIGN formats = (LOOP FOR label IN labels
                        FOR max-length IN max-lengths
                        FOR right-justified? IN right-justifieds?
                        AS col-width 
                        = (IF (> max-length 0)
                              (+ (MAX max-length (LENGTH label)) 2))
                        AS directive 
                        = (IF right-justified? "< ~A ~>" "< ~A ~;~>")
                        DO (IF col-width (PUSH label new-labels))
                        (IF (AND col-width (MEMBER label sequences :TEST 'EQUAL))
                            (ASSIGN col-width 12))
                        WHEN col-width
                        COLLECT (LIST "~" col-width directive)))
  
    (ASSIGN new-labels = (REVERSE new-labels))
    (ASSIGN head-format = (S+ "~&" (REPEAT " " TIMES (1+ line-col-width)) 
                              "~{" (JOIN (JOIN formats)) "~}"))
    (FORMAT T head-format new-labels)
        
    (SETF extracted-lines
      (LOOP FOR line IN lines  
         AS hit-info = NIL
         COLLECT
               (LOOP FOR label IN (REVERSE new-labels)
                     AS value = (utils::ref blast-table line label)
                     DO (IF (AND (MEMBER label sequences :TEST 'EQUAL)
                                 (> (LENGTH value) 7))
                            (ASSIGN value = (JOIN (BBL::FIRST 7 value) "...")))
                        (PUSH value hit-info)
                     FINALLY (RETURN (PUSH line hit-info)))))
    
    (IF key-position
        (ASSIGN sorted-lines
           (BBL::SORT extracted-lines BY-POSITION (1+ key-position))))
              ; Add 1 to key-position to account for line-number

    (LOOP FOR line-values IN (OR sorted-lines extracted-lines)
          AS line-format = NIL
          AS new-formats = NIL
          AS line = (POP line-values)
          DO (LOOP FOR label IN (REVERSE new-labels)
                   FOR (tilde col-width directive) IN (REVERSE formats)
                   FOR value = (REVERSE line-values)
                   DO (IF (AND (CALLED-FROM-VPL) (IS-FRAME? value))
                          (PUSH (LIST tilde (+ 2 col-width) directive) new-formats)
                          (PUSH (LIST tilde col-width directive) new-formats)))
             (ASSIGN line-format = (S+ "~&~" (STRING-OF line-col-width) "D." 
                                  "~{" (JOIN (JOIN new-formats)) "~}"))
             (FORMAT T line-format line line-values))
   (OR sorted-lines extracted-lines)
))


(DEFUN Data-list-type-of (data-list)
  ; Used by PLOT
  (LET ((sublist-found NIL)
        (long-sublist-found NIL)
       )

  (FOR-EACH item IN data-list
       (IF-TRUE (LISTP item)
           THEN (SETF sublist-found T)
                (IF (> (LENGTH item) 2)
                    (SETF long-sublist-found T))
           ELSE (IF sublist-found
                    (ERROR "ERROR IN PLOT: List of values must consist of a simple list, a list of duplets, or a list of lists. A singlet cannot appear in a list of values containing a list."))))
  (IF long-sublist-found
      'multiple
      (IF sublist-found
          'list-of-duplets
          'simple-list))))

(DEFUN List-to-counts (data-list)
  ; Used by PLOT
   (FOR-EACH n IN data-list
        INITIALIZE table = (NEW-TABLE '($))
        AS value = (REF table n)
        DO (IF value 
               (INCF (REF table n))
               (SETF (REF table n) 1))
        FINALLY 
          (RETURN 
             (LOOP FOR label IN (LABELS-OF table DIMENSION 1)
                   COLLECT (LIST label (REF table label))))))
       
	   
(DEFUN genes-as-target-check (target confine-to)
  (TYPECASE target
    (Gene (LIST target))
    (Protein (LIST (REF target #$Gene)))
    ((OR Contiguous-sequence Organism) (GENES-OF target))
    (T 
     (IF (OR (AND (SYMBOL= confine-to 'Gene) (IS-DNA-SEQUENCE? target))
             (AND (SYMBOL= confine-to 'Protein) (IS-PROTEIN-SEQUENCE? target)))
         target
       (ERROR 
        "When GENE search is specified, TARGET sequence, '~A~A', must be DNA"
        (TRIM (FIT (STRING-OF target) INTO 10))
        (IF (> (LENGTH (STRING-OF target)) 10) "..." "")
        )))))

(DEFUN Blast-local 
       (query target threshold word-size return program confine-to
              translate no-display remake-database 
              &KEY use-database return-targets)
  (LET* ((queries (ENSURE-LIST query))
         (targets
          (IF (PROVIDED confine-to)
              (FLATTEN 
               (FOR-EACH item IN (FLATTEN (ENSURE-LIST target))
                         COLLECT (GENES-AS-TARGET-CHECK item confine-to)))
            (ENSURE-LIST target)))
         (program (OR program
                      (AND (ELEMENTS-ARE-OF-TYPE queries '(Protein Coding-gene))
                           (ELEMENTS-ARE-OF-TYPE targets '(Protein Coding-gene Organism))
                           "blastp")))
         (blast-list 
          (BLAST-AUX (ENSURE-LIST query) targets program 
                     threshold word-size translate remake-database
                     :USE-DATABASE use-database)))

    (IF (AND blast-list (PROVIDED return))
        (SETF blast-list 
              (FOR-EACH
               n IN (LABELS-OF blast-list DIMENSION 1)
               FOR-EACH i FROM 1 TO return
               INITIALIZE new-list = (NEW-TABLE (list return '$))
               DO
               (FOR-EACH label IN (LABELS-OF blast-list DIMENSION 2)
                         DO (ASSIGN (ref new-list n label)
                                    = (ref blast-list n label)))
               FINALLY (RETURN new-list))))

    (IF (IS-TABLE? blast-list)
        (MAYBE-DISPLAY-BLAST-TABLE blast-list no-display return-targets)
        blast-list)
    ))

(DEFUN Make-circular-map-aux (chromosome-or-plasmid &KEY superimpose as-point)
  (LET* ((chromo-tab-file-content 
          (create-circular-map-tab chromosome-or-plasmid))
         (probe-tab-file-content 
          (COND ((AND superimpose as-point) 
                 (surimpose-on-map superimpose NIL))
                (superimpose (surimpose-on-map superimpose T))
                (T "")))
         (tab-file-content
          (JOIN chromo-tab-file-content *newline* probe-tab-file-content))
         (return-map-tab (write-map-tab tab-file-content))
         (circular-map-htm-content (create-circular-map-htm return-map-tab)))
    (write-and-show-map-htm circular-map-htm-content)))
	
	
; =============== SMART Domain analysis functions =========
(DEFUN numerify (atom-or-list)
  (IF (LISTP atom-or-list)
      (MAPCAR 'CONVERT-TO-NUMBER-MAYBE atom-or-list)
      (CONVERT-TO-NUMBER-MAYBE atom-or-list)))

(DEFUN Parse-SMART (file &KEY as-file debug)
   (LET* ((file-string 
             (IF as-file 
                 (JOIN (bbl::READ file TEXT))
                 file))
          (gene-link 
              (MATCH-OF-PATTERN-aux T "<h2.*?>(.*?)</h2>" file-string '(NIL NIL NIL T)
			      :CROSS-LINES T))
          (gene-descr
              (MATCH-OF-PATTERN-aux T "<h3.*?>(.*?)</h3>" file-string '(NIL NIL NIL T)
			      :CROSS-LINES T))
          (interaction-map
              (MATCH-OF-PATTERN-aux T "Interaction network.{0,10}Pathway information.*?src=.(.*?)[\"']" 
                   file-string '(NIL NIL NIL T) :CROSS-LINES T))
          (maps-region)(groups-region)
          (maps)(groups)
          (feature-regions)(features)(parsed-features)
         )

     (ASSIGN (maps-region groups-region) =
        (MATCH-OF-PATTERN-aux NIL (S+ "is possibly involved in the following metabolic pathways:"
		                               "(.*?)<p .{0,50}These assignments.*?</p>(.*?)</td>")
                   file-string '(NIL NIL NIL T) :CROSS-LINES T))

    (ASSIGN maps = 
        (MATCH-OF-PATTERN-aux T "<A.*?</A>[^<]*" maps-region '(NIL NIL T NIL) :CROSS-LINES T))
    (ASSIGN groups =
        (MATCH-OF-PATTERN-aux T "<A.*?</A>[^<]*" groups-region '(NIL NIL T NIL) :CROSS-LINES T))
    (ASSIGN feature-regions =
       (MATCHES-OF-PATTERN "kendo.data.DataSource.*?\"data\":.(.*?).,\"total\"" 
              IN file-string CROSS-LINES AS-REGEX +Sub-MATCHES -DISPLAY))
    (ASSIGN features =
       (FOR-EACH feature-region IN feature-regions
            AS domains = (MATCHES-OF-PATTERN "\{.*?\}" IN feature-region
                            CROSS-LINES AS-REGEX +MATCHES)
            APPEND domains))
    (ASSIGN parsed-features
       (FOR-EACH feature IN features
            AS (end e-value descr start) =
               (MATCHES-OF-PATTERN
                  (JOIN "'en':'(*..)'*..'ev':'?(*..)'?,*.."
                     "'>(*..)<*..'st':'(*..)'") IN feature
                  +SUB-MATCHES CROSS-LINES +1ST-MATCH-ONLY)
            COLLECT (JOIN "" descr (APPLY-FUNCTION (CONVERT-TO-NUMBER-MAYBE item :ELSE-NIL T) 
                              REPLACING item WITH (LIST start end e-value)))))
    (IF features
      (SETQ parsed-features (BBL::SORT parsed-features POSITION 3)))
  (IF debug (DISPLAY-DATA gene-link gene-descr interaction-map maps groups parsed-features))
    (LIST gene-link gene-descr interaction-map maps groups parsed-features)
))

(defun print-html-lines (lines)
  (loop for line in lines do (net.aserve::HTML (:princ line) :br))) 

(defun webpage-with-html-lines (lines)
  (net.aserve::HTML (:body :br (print-html-lines lines))))

(DEFUN Popup-with-HTML (lines)
  (CREATE-AND-USE-UNIQUE-FILE
    (FORWARD-PACKAGE-FUNCALL :vpl :USER-TEMP-VPL-DIR)
    (LAMBDA (FILE P)
       (DECLARE (IGNORE FILE))
	   (net.html.generator::HTML-STREAM P
         (PRINT-HTML-LINES lines)
       ))
    (LAMBDA (FILE)
       (FORWARD-PACKAGE-FUNCALL :vpl :SHOW-VPL-POPUP-URL-WINDOW
         (WEBLISTENER::PUBLISH-PATH-FOR-FILE-IN-VIEWABLE-SUBDIR
               FILE)
         :RELATIVE-P 0
         :WIDTH "800px"
         :HEIGHT "800px"
         :MENUBAR "yes"))
    :NAME (S+ "execution-log-" (STRING *SESSIONID*)) 
    :TYPE "html")
)

(DEFUN Display-SMART-results (results submit-url)
   (POPUP-WITH-HTML
      (JOIN
         "<BIG>SMART domain analysis</BIG>"
         ""
         (FOR-EACH (protein domain descr interaction metabolic groups features)
             		 IN results
              INITIALIZE list = NIL
              INITIALIZE spaces = "&nbsp; &nbsp; &nbsp; "
			  AS url = (JOIN submit-url (SEQUENCE-OF protein))
              AS group = (FIRST groups)
			  (LIST domain)   ; squelch warning
              (PUSH (JOIN 
			          (IF (IS-FRAME? protein)
                          (JOIN "<A href=/" 
                            (wb::forward-package-funcall :de
   							    :simple-frame-editor-url protein)
                            " target=_blank>" (GENE-OF protein) "</A>")
					      (NAME-OF protein))
					  spaces "(<A href='" url "'>Link to SMART page</A>)")
					list)
              (PUSH (JOIN spaces "<B>Description:</B> " descr AS-STRING) list)
              (IF interaction 
                  (PUSH (JOIN spaces "<B>Interaction map:</B> " 
                              "<A href='" interaction "'>Click here</A> "
                              "(per the <A href='http://string-db.org'>STRING database</A>)"
                              AS-STRING) 
                        list))
			  (IF metabolic
                  (PUSH (JOIN spaces "<B>Metabolic map:</B> " 
                              metabolic
                              " (per <A href='http://string-db.org'>KEGG</A>)"
                              AS-STRING) 
                        list))
			  (IF group
			      (PUSH (JOIN spaces "<B>KEGG group:</B> "
				              (BBL::REPLACE INTO group REPLACING-FIRST "path:ko" WITH "ko:"))
					    list))
			  (IF features
			      (FOR-EACH (feature abbrev from to pre-eval) IN features
				       AS eval = (IF (EQUAL pre-eval "-") 
                          ""
                          (JOIN ", E-VALUE: " pre-eval))
					   AS (label name) = 
					       (IF (SEARCH ":" abbrev)
						       (SPLIT abbrev AT ":")
							   '(NIL NIL))
					   AS abbrev-w-link = 
					      (IF (SAME label "Pfam")
						      (JOIN "<A href='"
							        "http://pfam.janelia.org/family?acc="
									name "'>" abbrev "</A>")
							  abbrev)
					   AS parenthetical-feature =
					      (IF (EQUAL feature "") 
						      "" 
							  (JOIN " (" feature ")"))
				       AS line = (JOIN spaces "<B>Feature: </B> "
					                   "FROM " from " TO " to 
                                       eval ", FAMILY: " abbrev-w-link
									   parenthetical-feature)
					   (PUSH line list)))
; http://pfam.sanger.ac.uk/family?acc=PsaA_PsaB
; http://pfam.janelia.org/family?acc=PsaA_PsaB

	  	      (PUSH "" list)
              FINALLY (RETURN (REVERSE list)))))
)

(DEFUN Get-SMART-result (url)
  (LET* ((SMART-page (bio::WEB-PAGE-CONTENTS url))
         (jobid (MATCH-OF-PATTERN-aux NIL "job_status.pl.(jobid=.*?)[\"']"
		             smart-page '(NIL NIL NIL T) :CROSS-LINES T))
		 (job_status_url-base "http://smart.embl.de/smart/job_status.pl")
		 (job_status_url (JOIN job_status_url-base "?" jobid))
		)
	(LOOP FOR i FROM 1 TO 6
	      WHILE jobid
	      DO (SLEEP 5)
             (SETQ smart-page
                (bio::WEB-PAGE-CONTENTS job_status_url))
	         (SETQ jobid (MATCH-OF-PATTERN-aux NIL "job_status.pl.(jobid=.*?)[\"']"
		                      smart-page '(NIL NIL NIL T) :CROSS-LINES T))
		     (SETQ job_status_url (JOIN job_status_url-base "?" jobid))
		)
	SMART-page
	))
	
(DEFUN Process-domains-of-results (results threshold 
         return Coordinates? Pfam-only? domain-description?
          domain-name? e-value? protein-name? gene-description?)
  (LET* ((fields (LIST coordinates? domain-description?
                       domain-name? e-value? protein-name? gene-description?))
         (everything (NONE-TRUE fields))
		 (processed-results NIL)
		 (filtered-results NIL)
		 )
    (SETF processed-results
      (FOR-EACH (protein gene-link gene-descr interaction-map maps groups features)
                 IN results
			AS filtered-features = NIL
	        AS new-result =
		       (FOR-EACH (fam-descr fam from-coord to-coord e-val) IN features
			    FOR-EACH result-count FROM 1 TO return
			        AS result-list = NIL
					(UNLESS (OR (AND pfam-only? 
					               (NOT (SAME (bbl::FIRST 4 IN fam) "Pfam")))
				                (AND (NUMBERP e-val) (> e-val threshold)))
					   (PUSH (LIST fam-descr fam from-coord to-coord e-val)
					         filtered-features)
					   (IF (OR everything protein-name?)
					       (PUSH protein result-list))
					   (IF (OR everything coordinates?)
					       (PROGRAM 
						      (PUSH from-coord result-list)
						      (PUSH to-coord result-list)))
					   (IF (OR everything e-value?)
					       (PUSH e-val result-list))
					   (IF (OR everything domain-name?)
					       (PUSH fam result-list))
					   (IF (OR everything domain-description?)
					       (PUSH fam-descr result-list))
					   (IF (OR everything gene-description?)
					       (PUSH (FIRST gene-descr) result-list)))
					WHEN result-list COLLECT (REVERSE result-list))
			(IF (= return 1) (SETF new-result (FIRST new-result)))
			(SETF filtered-features
			     (IF (= return 1)
			         (FIRST (REVERSE filtered-features))
				     (REVERSE filtered-features)))
			(IF filtered-features
			   (PUSH (LIST protein gene-link gene-descr interaction-map maps 
			          groups filtered-features) filtered-results))
			COLLECT new-result))
	(IF (= (LENGTH processed-results) 1)
	    (LIST (FIRST processed-results) (REVERSE filtered-results))
		(LIST processed-results (REVERSE filtered-results)))
))
; ===============

(DEFUN Process-documentation-html (given-html)
  (LET* ((html (JOIN given-html BY *newline*))
         (aka (MATCHES-OF-PATTERN
                "<head><title>*..\\(a.k.a.*..\\)</title>"
                IN html CROSS-LINES +SUB-MATCHES +1st-MATCH-ONLY))
         (parm-section (MATCHES-OF-PATTERN "<h2>Parameters*?</h2>(*..)<h"
                       IN html CROSS-LINES +SUB-MATCHES +1st-MATCH-ONLY))
         (returns (MATCHES-OF-PATTERN "<h2>Returns*?</h2><p>(*..)<h"
                    IN html CROSS-LINES +SUB-MATCHES +1st-MATCH-ONLY))
         (examples (MATCHES-OF-PATTERN "<h2>Examples*?</h2>(*..)<h"
                    IN html CROSS-LINES +SUB-MATCHES +1st-MATCH-ONLY))
         (description (MATCHES-OF-PATTERN "<h2>*{0,3}Description*{0,5}</h2>(*..)<h"
                        IN html CROSS-LINES +SUB-MATCHES +1st-MATCH-ONLY))
         (keywords (MATCHES-OF-PATTERN "<h2>*{0,3}Keywords*{0,4}</h2>(*..)<h"
                    IN html CROSS-LINES +SUB-MATCHES +1st-MATCH-ONLY))
         (raw-parameters 
             (MATCHES-OF-PATTERN "<font COLOR=*..>([~<]..)<*..&nbsp;(*..)</P>" 
                   IN parm-section +SUB-MATCHES))
         (parameters
            (FOR-EACH (parameter1 parameter2) in raw-parameters
                 AS revised-parameter2 = 
                   (BBL::REPLACE INTO parameter2 REPLACING-EVERY "<BR>" WITH "")
                 COLLECT (JOIN parameter1 ": " revised-parameter2 *newline*)))
         )
    (JOIN aka parameters returns examples description keywords BY *newline*)
))

(DEFUN compress-newlines (text)
 (LET* ((*bbl-current-case-mode* :case-sensitive)
        (tandem-NL (JOIN *newline* *newline*))
        (tandem-count (COUNT-OF tandem-NL IN text)))
    (IF (> tandem-count 0)
        (FORWARD-FUNCALL 'COMPRESS-NEWLINES
            (HARD-STRING-REPLACEX tandem-NL (STRING *newline*) text))
        text)
))

(DEFUN REMOVE-HTML-INTELLIGENTLY (text &KEY documentation)
(LET* ((LF (STRING (CODE-CHAR 10)))
         (fake-LF (STRING (CODE-CHAR 255)))
         (LF-strings '("<BR>" "<P>" "</P>" "<BLOCKQUOTE>"
                "</BLOCKQUOTE>" "<OL>" "<UL>" "</OL>" "</UL>"))
         (new-text
            (JOIN (IF documentation
                      (PROCESS-DOCUMENTATION-HTML text)
                      text)
                  BY " "))
         (parts-with-title 
              (MATCH-OF-PATTERN-aux T "(.*?)<TITLE>.*?</TITLE>(.*)"
                    new-text '(NIL NIL NIL T) :CROSS-LINES T)) 
         (replacements
            '(("&nbsp;" " ") ("&gt;" ">") ("&lt;" "<") ("&mdash;" "-")
              ("&quot;" "\"") ("&amp;" "&") ))
      )
  (IF parts-with-title
      (SETF new-text (JOIN parts-with-title AS-STRING)))  
  (FOR-EACH LF-string IN LF-strings
       (SETF new-text 
         (bbl::REPLACE INTO new-text REPLACING-EVERY LF-string WITH fake-LF)))
  (SETF new-text (REMOVE-HTML new-text))
  (SETF new-text 
     (bbl::REPLACE INTO new-text REPLACING-EVERY fake-LF WITH LF))
  (SETF new-text 
     (bbl::REPLACE INTO new-text 
         REPLACING-EACH (bbl::FIRST IN-EACH replacements)
         WITH-EACH (bbl::SECOND IN-EACH replacements)))
  (COMPRESS-NEWLINES new-text)
))

; ================= RNAfold function ==================
(DEFUN Fold-RNA-aux (given-seq given-label save graphic?)
  ; ***********************
  ; Accesses RNAfold program in ViennaRNA package
  ; RNAfold requires that the ps file it creates go to user/..., where
  ; user is not the actual user but the instance, e.g. biodemovcu
  ; *tmp-directory* is in the the user directory
  ; RNAfold gets the directory from the FastA header of the sequence (!)
  ; ***********************
  (FLET ((pathless-name (path)
           ; a/b/c/d.g --> d.g; a/b/c/ -> c/
           (LET* ((path-string (STRING-OF path))
                  (last-word (BBL::LAST (SPLIT path-string EVERY "/"))))
             (IF (EQUAL (bbl::LAST path-string) "/")
                 (JOIN last-word "/")
                 last-word)
          )))
  (LET* ((seq (SEQUENCE-OF given-seq))
         (name (OR given-label 
                   (IF (LABELED-SEQUENCE-P given-seq)
                       (NAME-OF given-seq))
                   "seq"))
         (path "/usr/local/ViennaRNA-2.1.7/bin/")
         (user-directory (wb::USER-DIRECTORY wb::*username*))
         (full-name (JOIN name "-folded"))
         (pdf-full-name (JOIN full-name ".pdf"))
         (outfile-full-name (JOIN full-name ".txt"))
         (pspath 
            (JOIN (PATHLESS-NAME wb::*tmp-directory*) full-name))
         (pspath-full 
            (JOIN (STRING-OF wb::*tmp-directory*) full-name "_ss.ps"))
         (command)(result)(pdf-url)(outfile-url)(delta-g)
         )
   (bio::WITH-TEMP-FILE-IN (seqfile wb::*tmp-directory* :DELETE? NIL :TYPE "txt")
   (bio::WITH-TEMP-FILE-IN (outfile wb::*tmp-directory* :DELETE? NIL :TYPE "txt" 
            :NAME full-name)
   (bio::WITH-TEMP-FILE-IN (pdffile wb::*tmp-directory* :DELETE? NIL :TYPE "pdf" 
            :NAME full-name)
   (bio::WITH-TEMP-FILE-IN (web-outfile cl-user::*webtmp-directory* 
                             :DELETE? NIL :TYPE "txt" :NAME full-name)
   (bio::WITH-TEMP-FILE-IN (web-pdffile cl-user::*webtmp-directory* 
                             :DELETE? NIL :TYPE "pdf" :NAME full-name)
       (bio::WITH-OPEN-FILE (data seqfile :DIRECTION :OUTPUT :IF-EXISTS :SUPERSEDE)
           (bbl::WRITE seq TO seqfile FASTA HEADER pspath))
       (bio::WITH-OPEN-FILE (data seqfile :DIRECTION :INPUT)
       (bio::WITH-OPEN-FILE (data outfile :DIRECTION :OUTPUT :IF-EXISTS :SUPERSEDE)
           (SETF command 
              (JOIN path "RNAfold < " seqfile " > " outfile
                    " " (IF (NOT graphic?) "--noPS")))
           (SETF result (bbi::PROTECTED-SHELL-COMMAND command))
           (WHEN (= result 0) ; RNAfold exited normally
             ; *** extract delta-g, convert ps to pdf
             ; *** optionally copy output files to user's directory
             ; *** move output files to web-accessible directory
		     (SETF delta-g (MATCHES-OF-PATTERN "`(([-#][.#]...)`)" 
			                 IN (FILE-TO-STRING outfile)
							 +1st-MATCH-ONLY +SUB-MATCHES CROSS-LINES -DISPLAY))
             (IF delta-g (SETF delta-g (CONVERT delta-g TO Number)))
		     (WHEN graphic?
               (bbi::PROTECTED-SHELL-COMMAND
			      (JOIN "ps2pdf " pspath-full " " pdffile))
               (SETF pdf-url (wb::webtmp-url (PATHLESS-NAME web-pdffile))))
             (SETF outfile-url (wb::webtmp-url (PATHLESS-NAME web-outfile)))
             (WHEN save
                (WHEN graphic?
                  (bbi::PROTECTED-SHELL-COMMAND 
                     (JOIN "cp " pdffile " " 
                         (JOIN user-directory pdf-full-name))))
                (bbi::PROTECTED-SHELL-COMMAND 
                   (JOIN "cp " outfile " " 
                       (JOIN user-directory outfile-full-name))))
             (WHEN graphic? 
                 (bbi::PROTECTED-SHELL-COMMAND 
                    (JOIN "mv " pdffile " " web-pdffile)))
             (bbi::PROTECTED-SHELL-COMMAND 
                 (JOIN "mv " outfile " " web-outfile)))
       ))
      (VALUES
	    delta-g
        (IF graphic? (wb::make-url :path pdf-url :display-string "Folded RNA (graphic)"))
        (wb::make-url :path outfile-url :display-string "Folded RNA (text)")) 
    )))))
)))