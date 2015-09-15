;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE bbi)

(DEFUN contig-sequence-of (frame given-start given-end invert
              &KEY from-end? to-end? truncate wrap)
  (LET* ((full-length (SLOTV frame #$Sequence-length ))
         (start (1+ (HUMAN-BASE-TO-0-BASE given-start full-length frame
                    :WRAP wrap :TRUNCATE truncate :FROM-END from-end?)))
         (end (IF-TRUE (UNPROVIDED? given-end)
                  THEN full-length
                  ELSE (1+ (HUMAN-BASE-TO-0-BASE given-end full-length frame
                         :WRAP wrap :TRUNCATE truncate :FROM-END to-end?)))))

    (IF-TRUE invert
        THEN (EXTRACT-SEQUENCE frame :FROM start :TO end
                     :DIRECTION :B)
        ELSE (EXTRACT-SEQUENCE frame :FROM start :TO end))))


(DEFUN gene-sequence-of (gene given-start given-end invert 
              &KEY from-end? to-end? truncate zero-based)
  (DECLARE (IGNORE truncate))
  (LET* ((full-length
          ;; LENGTH-OF is not defined yet and it is a macro (define-function)
          ;; so you cannot use it before it is defined
          (forward-funcall 'length-of-gene gene))
         (start-offset 
              (IF-TRUE zero-based
                  THEN given-start
                  ELSE (HUMAN-BASE-TO-0-BASE given-start full-length 
                              gene :FROM-END from-end? :NO-CHECK T)))
         (end (COND
                 (zero-based (1+ given-end))
                 ((UNPROVIDED? given-end) full-length)
                 (T (1+ (HUMAN-BASE-TO-0-BASE given-end full-length 
                              gene :FROM-END to-end? :NO-CHECK T)))))
         (end-offset (- end full-length))
         (architecture (SLOTV gene #$ARCHITECTURE))
        ; (contig (SLOTV gene #$CONTIGUOUS-SEQUENCE))
         ;(direction (SLOTV gene #$DIRECTION))
         (seq-without-inserts))

    (IF (OR (< end start-offset) (AND (= end 0) (= start-offset 0)))
        (ERROR "Given start ~A lies after given end ~A in gene ~A"
           start-offset
           (if (= given-end *unprovided+*) full-length given-end)
           gene))
     
    (SETF seq-without-inserts
     (IF-TRUE (NOT architecture)
         THEN (EXTRACT-SEQUENCE gene
                     :START-OFFSET start-offset :END-OFFSET end-offset)
         ELSE (LET* ((pregene-length
                        (COND
                           ((>= start-offset 0) 0)
                           ((>= end 0) (- start-offset))
                           (T (- end start-offset -1))))
                     (pregene-part
                        (IF-TRUE (> pregene-length 0)
                            THEN (SUBSEQ
                                   (EXTRACT-SEQUENCE gene
                                      :START-OFFSET start-offset)
                                   0 pregene-length)
                            ELSE ""))
                     (gene-seq (EXTRACT-GENE-SEQUENCE gene)) ; :TRUNCATE truncate))
                     (gene-part 
                       (LET ((s (MIN (MAX start-offset 0) (1+ full-length)))
                             (e (MAX (MIN end full-length) -1)))
                         (IF-TRUE (> s e) 
                             THEN ""
                             ELSE (SUBSEQ gene-seq s e))))
                     (postgene-part 
                      (IF-TRUE (> end-offset full-length)
                          THEN (EXTRACT-SEQUENCE gene
                                  :START-OFFSET full-length
                                  :END-OFFSET end-offset)
                          ELSE "")))
                (S+ pregene-part gene-part postgene-part))))
     (maybe-complement seq-without-inserts invert)
    ))

(DEFUN Protein-sequence-of (protein given-start given-end
           &KEY from-end? to-end? truncate zero-based no-stops)
   (LET* ((full-length (SLOTV protein #$Sequence-length ))
          (start (IF zero-based
                     given-start
                     (HUMAN-BASE-TO-0-BASE given-start full-length protein
                        :FROM-END from-end? :NO-CHECK T)))
          (end (COND
                  (zero-based given-end)
                  ((UNPROVIDED? given-end) full-length)
                  (T (1+ (HUMAN-BASE-TO-0-BASE given-end full-length protein
                               :FROM-END to-end? :NO-CHECK T))))))

    (IF (OR (< end start) (AND (= end 0) (= start 0)))
        (ERROR "Given start ~A lies after given end ~A in gene ~A"
               given-start 
               (if (= given-end *unprovided+*) full-length given-end)
               protein))
       
     (IF-TRUE (OR (< start 0) (> end full-length))
         THEN (LET* ((gene (SLOTV protein #$gene ))
                     (gene-start (* start 3))
                     (gene-end (* end 3))
                     (gene-seq (GENE-SEQUENCE-OF gene gene-start gene-end NIL 
                            :TRUNCATE truncate :ZERO-BASED T))
                     (result))
                (SETF result 
                   (TRANSLATE-TO-AA gene-seq :if-partial-codon :ignore 
                       :IF-UNKNOWN-CODON "X"))
                (IF no-stops
                    (ONLY-AA result)
                    result))
         ELSE (LET* ((protein-sequence (EXTRACT-SEQUENCE protein))
                     (returnable 
                        (IF (>= end (LENGTH protein-sequence))
                            (SUBSEQ protein-sequence start)
                            (SUBSEQ protein-sequence start end)))
                     (length (LENGTH returnable)))
                (IF (EQUAL (SUBSEQ returnable (1- length) length) "*")
                    (SUBSEQ returnable 1 (1- length))
                    returnable)))))
        
(defparameter *sequence-of-bad-chars* 
  (S+ "1234567890/ " (STRING *newline*) (STRING *CR*) (STRING *tab*)))

(defun any-characters-of? (characters string)
  (declare (simple-string characters string))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop for sch across string 
    do
    (loop for ch across characters 
      do
      (when (eql sch ch) (return-from any-characters-of? t)))
    finally (return nil)
    ))

(defun process-string-for-sequence-of (s)
  (let ((bad-chars *sequence-of-bad-chars*))
    (when (any-characters-of? bad-chars s)
      (setq s (remove-if (lambda (x) (find x bad-chars)) s)))
    (when (lisp:some #'lower-case-p s) (setq s (string-upcase s)))
    s
    ))

;; Presumably you would never give this function both 
;; :protein and :invert, because then it would try to complement 
;; amino acids, which doesn't make sense.  -- JP and Mark

;; Make this function only remove bad characters and uppercase the string 
;; if necessary.  This is because otherwise perfectly but very long strings
;; get copied multiple times needlessly and cause GC havoc.  

(DEFUN string-sequence-of (string given-start given-end invert 
           &KEY dna protein from-end? to-end? truncate wrap sequence-ok
           nonstrict)
  (LET* ((processed-string
          (IF sequence-ok
              string
            (process-string-for-sequence-of string)
            ))
         (full-length (length processed-string))
         (start (HUMAN-BASE-TO-0-BASE given-start full-length "(given string)"
                    :WRAP wrap :TRUNCATE truncate :FROM-END from-end?))
         (end 
          (IF-TRUE 
           (UNPROVIDED? given-end)
           THEN (- full-length 1)
           ELSE (HUMAN-BASE-TO-0-BASE 
                 ;; CRUDE HACK: MUST FIX!
                 ;; MAY FAIL IF USER SPECIFIES END 
                 (MIN given-end (LENGTH processed-string))
                 full-length "(given string)"
                 ;; given-end full-length "(given string)"
                 :WRAP wrap :TRUNCATE truncate :FROM-END to-end?)))
         (null-string (= full-length 0))
         (final-seq))
     
      (IF (AND (NOT nonstrict)
               (OR (< end start) 
                   (AND null-string (NOT (= 0 given-start given-end)))))
          (ERROR 
           (formatn
            (one-string-nl
             "*** Trying to access characters beyond end of the string!"
             "The given start ~A lies after the end coordinate.  "
             "The end of the string (or the end you provided) is ~D.  "
             "~A")
            given-start 
            (if (= given-end *unprovided+*) 
                (length processed-string)
              given-end)
            (if (/= (length string) (length processed-string))
                (formatn 
                 (one-string-nl
                  "The input string is ~D long, but the processed string "
                  "(removing digits and whitespace) is ~D long."
                  )
                 (length string) (length processed-string)
                 )
              ""
              )))
        (SETF final-seq 
              (IF null-string "" (SUBSEQ processed-string start (1+ end)))))
 
      (IF-TRUE (OR dna protein) 
          THEN (LET* ((type (IF dna "DNA" "protein"))
                      (scan-string 
                         (IF dna
                             (S+ "[" *legal-dna-chars* "]")
                             (S+ "[" *legal-protein-chars* "]")))
                      (illegals (CL-PPCRE:REGEX-REPLACE-ALL 
                                     scan-string final-seq "")))
                  (IF (NOT (EQUAL illegals ""))
                     (ERROR
                      (S+ "Illegal characters found in supposed ~A sequence:"
                          "~&    ~A")
                      type 
                      (REMOVE-DUPLICATES (BB-SPLIT illegals) :TEST 'EQUAL)
                      ))))

      (maybe-complement final-seq invert)
  ))

(DEFUN Maybe-complement (string complement?)
  (IF complement? (bioutils::ncomplement-base-pairs 
                   string :extended-alphabet? t)
    string))




