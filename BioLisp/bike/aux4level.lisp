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

(DEFUN Sequence-type-of-list (spec-list &KEY (verbose T))
  "Determines compatible sequence type from a list of specifications"
  ;; Returns seq-type as 'TEXT, 'AA, or 'DNA
  (LOOP
   FOR item IN (ENSURE-LIST spec-list)
   WITH dna-found = NIL
   WITH aa-found = NIL
   WITH gene-found = NIL
   WITH protein-found = NIL
   WITH text-found = NIL
   DO (TYPECASE item
        ((OR String Labeled-sequence)
         (LET* ((label)
                (seq 
                 (IF-TRUE (STRINGP item)
                          THEN (SETF label (JOIN (BBL::FIRST 10 IN item) "..."))
                          item
                          ELSE (SETF label (LABELED-SEQUENCE-LABEL item))
                          (LABELED-SEQUENCE-SEQUENCE item)))
                (item-type (BB-SEQUENCE-TYPE-OF seq T)))                        
           (CASE item-type
             ((DNA RNA)
              (IF aa-found 
                  (ERROR "DNA or RNA sequence '~A' found in list including amino acid sequence" 
                         label))
              (SETF dna-found T))
             (PROTEIN 
              (IF dna-found 
                  (ERROR "Amino acid sequence '~A' found in list including DNA or RNA sequence" 
                         label))
              (SETF aa-found T))
             (OTHERWISE 
              (IF verbose 
                  (ERROR "Only nucleotide or protein sequences may be compared, not '~A'"
                         label))
              (SETF text-found T)))))
        (Gene (SETF gene-found T))
        (Protein (SETF protein-found T))
        (Organism)
        (Contiguous-sequence)
        (OTHERWISE
         (ERROR "Only nucleotide or protein sequences may be compared, not '~A'"
                item)))
   FINALLY
   (RETURN
    (COND
     (text-found 'TEXT)
     (aa-found 'AA)
     (dna-found 'DNA)
     (protein-found 'AA)
     (gene-found 'DNA)
     (T 'DNA)))))


(DEFUN Count-of-aux
       (query map-query not target case-sensitive case-insensitive 
              &KEY freq-count labeled both-strands)

  (FLET ((Count-of-string (query string)
           (LET* ((pos 0)
                  (count
                   (IF-TRUE 
                    (OR freq-count (= (LENGTH query) 1))
                    THEN
                    (LET ((test (IF case-sensitive 'EQL 'CHAR-EQUAL)))
                      (reduce '+ (FREQUENCY-COUNT
                                 query string 
                                 :TEST test :RETURN-AS :element-position-list))
                      #+oops
                      (APPLY '+ (FREQUENCY-COUNT
                                 query string 
                                 :TEST test :RETURN-AS :element-position-list))
                      )
                    ELSE 
                    (LET ((test (IF case-sensitive 'EQUAL 'EQUALP)))
                      (LOOP WHILE (AND pos (< pos (LENGTH string)))
                            AS new-pos 
                            = (SEARCH query string :TEST test :START2 pos)
                            DO (SETF pos (AND new-pos (1+ new-pos)))
                            WHEN pos 
                            COUNT pos)))))
             (IF-FALSE not
                       THEN count
                       ELSE (- (LENGTH string) count))))

         (No-string-error (target-type)
           (Err+ problem  "The query ~A is not a valid type of argument"
                 indent  "when you pass ~A as target such as ~A."
                 advice  "Use a string as query." 
                 help~A
                 format-args
                 query target-type target *df-name*)
           )
         (Result-from (count)
           (IF labeled
               (LIST (STRING-LABEL query) count)
             count))) 

    (ERROR-IF-MORE-THAN-ONE case-sensitive case-insensitive)
    (COND
     ((TYPEP target 'Organism)
      (IF-TRUE (OR not map-query (TYPEP query 'String))
               THEN (LET ((result
			                (LOOP FOR replicon IN (SLOTV target #$Contiguous-sequences )
                                  COLLECT (FORWARD-FUNCALL 'COUNT-OF-AUX 
								                query map-query not 
                                                replicon case-sensitive NIL :LABELED labeled
												:BOTH-STRANDS both-strands))))
						(IF (< (LENGTH result) 2)
                            (FIRST result)
							(SUM-OF result))) 
               ELSE (NO-STRING-ERROR "organisms")))
     (not
      (COND
       ((UNPROVIDED? target)
        (Err+ problem "The NOT option requires a target for the count."
              advice "Provide a target using either the IN or IN-EACH options."
              help~A
              format-args
              *df-name*)
        ) 
       ((LISTP target)
        (IF case-sensitive
            (LENGTH (SET-DIFFERENCE target (ENSURE-LIST query) :TEST 'EQUAL))
          (LENGTH (SET-DIFFERENCE target (ENSURE-LIST query) :TEST 'BB-SAME))))
       ((AND (LISTP query) (NOT (EVERY 'STRINGP query)))
        (ERR+ problem 
              "~A in the list following NOT is not a valid type of argument."
              advice  "When COUNT-OF acts on a string (or sequence),"
              indent  "every element in the list following NOT must be a string."
              help~A
              format-args
              (LOOP FOR item IN query
                WHEN (NOT (STRINGP item))
                RETURN item) *df-name*
              )
        )
       ((OR (STRINGP query) (CHARACTERP query))
        (LET* ((query-string 
                (ALPHABET-OF 
                 (IF (STRINGP query) query (STRING-JOIN+ query "")))
                #+oops
                (ALPHABET-OF (IF (STRINGP query) query (APPLY 'S+ query)))
                )
               (count
                (- (LENGTH-OF target) 
                   (FORWARD-FUNCALL 'COUNT-OF-AUX query-string NIL NIL target
                                    case-sensitive case-insensitive :freq-count T 
                                    :LABELED labeled :BOTH-STRANDS both-strands))))
          (RESULT-FROM count)))
       (T 
        (ERR+ problem "~A following NOT is not valid when counting a string."
              advice  "When COUNT-OF acts on a string (or sequence),"
              indent  "every element in the list following NOT must be a string."
              help~A
              format-args
              query *df-name*)
        )))
     ((AND map-query (LISTP query))
      (LOOP FOR item IN query
            COLLECT (FORWARD-FUNCALL 'COUNT-OF-AUX item NIL not target 
                                     case-sensitive case-insensitive :LABELED labeled
                                     :BOTH-STRANDS both-strands)))
     (both-strands   ; LABELED must be NIL or addition fails
      (+ (FORWARD-FUNCALL 'COUNT-OF-AUX query NIL not target
                          case-sensitive case-insensitive)
         (FORWARD-FUNCALL 'COUNT-OF-AUX (INVERSION-OF query) NIL not target
                          case-sensitive case-insensitive)))
     (T (TYPECASE target
          (Unprovided
           (IF-TRUE (TYPEP query 'List)
                    THEN (LENGTH query)
                    ELSE 
                    (ERR+ problem "The target was not provided!" 
                          indent "If you provide a string as the first argument"
                          indent "you must provide a target (using IN) in which"
                          indent "to locate your string."
                          advice 
                          "Specify the query AND the target:" 
                          indent "(COUNT-OF \"AB\" in MY-STRING)"
                          help~A
                          format-args *df-name*)
                    )) 
          (List
           (LET ((count
                  (IF-TRUE (AND (STRINGP query) (NOT case-sensitive)) 
                           THEN (COUNT query target :TEST 'EQUALP)
                           ELSE (COUNT query target :TEST 'EQUAL))))
             (IF-FALSE not 
                       THEN (RESULT-FROM count)
                       ELSE (RESULT-FROM (- (LENGTH target) count)))))
          (Table
           (LET* ((test (IF (AND (STRINGP query) (NOT case-sensitive))
                            'EQUALP 'EQUAL))
                  (length (GARRAY-CURRENT-TOTAL-SIZE target))
                  (count (COUNT-IF 
                          (LAMBDA (x) (FUNCALL test x 47)) 
                          (FLATTEN (GMAP 'IDENTITY target)))))
             (IF-FALSE not 
                       THEN (RESULT-FROM count)
                       ELSE (RESULT-FROM (- length count)))))
          ((OR Gene Protein Contiguous-sequence)
           (IF-TRUE (TYPEP query 'String)
                    THEN (LET ((sequence (EXTRACT-SEQUENCE target)))
                           (RESULT-FROM (COUNT-OF-STRING query sequence)))
                    ELSE (NO-STRING-ERROR "genes proteins, and replicons")))
          #|     (Gene-part
            (LET* ((contig (SLOTV (SLOTV target #$GENE) #$CONTIGUOUS-SEQUENCE))
                   (left-coord  (SLOTV target #$FROM))
                   (right-coord (SLOTV target   #$TO))
                   (direction (SLOTV target #$DIRECTION))
                   (sequence (EXTRACT-SEQUENCE contig :FROM left-coord
                                :TO right-coord :DIRECTION direction)))
                     (COUNT-OF-STRING query sequence)))      |#
          #|     (Labeled-sequence)     |#
          (String
           (IF-TRUE (TYPEP query 'String)
                    THEN (RESULT-FROM (COUNT-OF-STRING query target))
                    ELSE (NO-STRING-ERROR "strings")))
          (OTHERWISE
           (ERROR "The target (~A) is of a type ('~A') not supported by BBL."
                  target (TYPE-OF target))))))))


(DEFUN Orfs-in-aux (sequence pattern return-sequences &KEY strand)
   (LOOP 
      WITH start = 0 
      WITH still-hope = T
      WITH result = NIL
      WITH hit-start = NIL
      WITH hit-end = NIL
      WHILE still-hope
      DO (ASSIGN (hit-start hit-end) 
            = (CL-PPCRE:SCAN pattern sequence :START start))
         (IF-TRUE hit-start
             THEN (COND
			         ((EQUAL strand "F")
					    (ASSIGN result = (LIST "F" (1+ hit-start) hit-end)))
					 ((EQUAL strand "B")
					    (ASSIGN result 
						   = (LIST "B" 
						       (- (LENGTH sequence) hit-end -1)
							   (- (LENGTH sequence) hit-start))))
					 (T (ASSIGN result (LIST (1+ hit-start) hit-end))))
			      (ASSIGN start = (+ hit-start 2))
             ELSE (ASSIGN still-hope = NIL))
		 (IF (AND hit-start return-sequences)
             (ASSIGN result 
               = (APPEND result (LIST (SUBSEQ sequence hit-start hit-end)))))
      WHEN hit-start
        COLLECT result))
		
(defun to-pssm-sequence (source dna?)
  (typecase source
    (string (list source))
    (organism 
     (mapcar
      'extract-sequence
      (if dna? (#^contiguous-sequences source) (#^proteins source)))
     )
    (gene 
     (if dna? 
         (list (extract-sequence source))
       (mapcar 'extract-sequence (#^proteins source))
       ))
    (protein
     (list
      (if dna? (extract-sequence (#^gene source)) (extract-sequence source))
      ))
    (labeled-sequence (list (labeled-sequence-sequence source)))
    (otherwise (error "Don't know how to get a sequence from ~S" source))
    ))


(DEFUN Context-of-gene-aux (gene gene-width width no-display)
  ;; made the code test for no-display -- JP
  (DECLARE (IGNORE width))
  (LET* ((horizen (UNLESS-PROVIDED gene-width 2))
         (left-side
          (LOOP
           FOR i FROM 1 TO horizen
           WITH current-gene = (GENE-LEFT-OF gene)
           AS IG = (LENGTH (SEQUENCE-RIGHT-OF current-gene))
           AS description = (DESCRIPTION-OF current-gene LENGTH 30)
           AS dir-symbol 
           = (AND current-gene
                  (IF-TRUE (SYMBOL= (SLOTV current-gene #$Direction) :F)
                           THEN '->
                           ELSE '<-))
           APPEND 
           (IF-TRUE current-gene
                    THEN (LIST IG (LIST dir-symbol current-gene description))
                    ELSE (LIST " " (LIST 'BEGINNING-OF-CONTIG)))
           DO (IF-TRUE current-gene
                       THEN (SETF current-gene (GENE-LEFT-OF current-gene))
                       ELSE (SETF i *big-number*))))
         (right-side
          (LOOP 
           FOR i FROM 1 TO horizen
           WITH current-gene = (GENE-RIGHT-OF gene)
           AS IG = (LENGTH (SEQUENCE-LEFT-OF current-gene))
           AS description = (DESCRIPTION-OF current-gene LENGTH 30)
           AS dir-symbol 
           = (AND current-gene
                  (IF-TRUE (SYMBOL= (SLOTV current-gene #$Direction) :F)
                           THEN '->
                           ELSE '<-))
           APPEND
           (IF-TRUE current-gene
                    THEN (LIST IG (LIST dir-symbol current-gene description))
                    ELSE (LIST " " (LIST 'END-OF-CONTIG)))
           DO (IF-TRUE current-gene
                       THEN (SETF current-gene (GENE-RIGHT-OF current-gene))
                       ELSE (SETF i *big-number*))))
         (middle
          (LIST (LIST 
                 (IF-TRUE (SYMBOL= (SLOTV gene #$Direction) :F)
                          THEN '->
                          ELSE '<-)
                 gene (DESCRIPTION-OF gene LENGTH 30))))
         (context
          (CONCATENATE 'LIST
                       (REVERSE left-side)
                       middle
                       right-side))
         (gene-list
          (LOOP FOR item IN context
                WHEN (AND (Is-LIST? item) (SECOND item))
                COLLECT (SECOND item))))
     (unless no-display
       (FORMAT T "~&~A~{ ~A~%~A~}~%" (FIRST context) (REST context)))
    gene-list))


(DEFUN Context-of-symbol (directiona directionb precise)
   (COND
      ((AND (STRING-EQUAL directiona :F)
            (STRING-EQUAL directionb :F))
        (IF precise "PR" "P"))
      ((AND (STRING-EQUAL directiona :B)
            (STRING-EQUAL directionb :B))
        (IF precise "PL" "P"))
      ((AND (STRING-EQUAL directiona :F)
            (STRING-EQUAL directionb :B)) "C")
      ((AND (STRING-EQUAL directiona :B)
            (STRING-EQUAL directionb :F)) "D")
      (T (ERROR "CONTEXT-OF-symbol got bad input: ~A ~A"
              directiona directionb))))

(DEFUN Context-of-coord-calculation (coord contig precise &KEY (verbose T))
  (LET* ((sorted-genes (SLOTV contig #$Genes-sorted-by-position))
         (last-gene (1- (LENGTH sorted-genes)))
         (contig-length (SLOTV contig #$Sequence-length))
         (index1)(indexa)(indexb)
         (genea)(geneb)
         (froma)(toa)(directiona)(left-distance)
         (fromb)(directionb)(right-distance)
         (at-end)(linear)
         )
    (IF (OR (NOT sorted-genes) (= (LENGTH sorted-genes) 0))
	    (IF verbose (WARN (S+ "Contig " contig " has no genes")))
		(PROGN
    (MULTIPLE-VALUE-SETQ (index1 indexa indexb) 
        (binsearch coord sorted-genes 
                   :key '#^from :if-not-found :range))
    (SETF at-end (AND (NOT index1) (OR (NOT indexa) (NOT indexb))))
    (SETF linear (AND at-end (NOT (SLOTV contig #$CIRCULAR))))
    (SETF indexa (OR index1 indexa))

    (SETF genea (OR (AND indexa (AREF sorted-genes indexa))
                    (AND (NOT linear) (AREF sorted-genes last-gene))))
    (SETF geneb (OR (AND indexb (AREF sorted-genes indexb))
                    (AND (NOT linear) (NOT index1) 
                         (AREF sorted-genes 0))))

    (IF genea
      (MULTIPLE-VALUE-SETQ (froma toa directiona) 
         (VALUES-LIST (LIST (SLOTV genea #$FROM)
                            (SLOTV genea #$TO)
                            (SLOTV genea #$DIRECTION))))
      (IF linear (SETF toa 0)))

    (IF geneb
      (MULTIPLE-VALUE-SETQ (fromb directionb) 
         (VALUES-LIST (LIST (SLOTV geneb #$FROM)
                            (SLOTV geneb #$DIRECTION))))
      (IF linear (SETF fromb contig-length)))

    (SETF left-distance (MOD (- coord toa) contig-length))
    (IF fromb (SETF right-distance (MOD (- fromb coord) contig-length)))

    (COND
       ((AND (<= coord toa) (OR (NOT at-end) (NOT indexb) (< toa froma)))  
          ; coord in gene, maybe last gene spanning origin
          (SETF left-distance (MOD (- coord froma) contig-length))
          (SETF right-distance (- toa coord))
          (LIST "I" (OR genea geneb) NIL left-distance 
                right-distance (OR directiona directionb)))  
       (linear
          (LIST (COND
                   ((AND genea (EQUAL directiona :F)) "N")
                   (genea "U")
                   ((EQUAL directionb :F) "U")
                   (T "N"))
                genea geneb
                left-distance right-distance (OR directiona directionb)))
       (T ; coord after last gene spanning origin
          (LIST (CONTEXT-OF-SYMBOL directiona directionb precise)
                genea geneb left-distance 
                right-distance directiona)))))))



(DEFUN Context-of-coord-aux (coord given-contig gene-width width 
             no-display &KEY precise (verbose T))
  (DECLARE (OPTIMIZE (Speed 3) (Safety 0) (Debug 0)))
  (DECLARE (IGNORE gene-width width))
  (LET* ((contig
          (IF-TRUE (TYPEP given-contig 'Organism)
                   THEN 
                   (OR (CHROMOSOME-OF given-contig NOWARNINGS)
                       (ERROR 
                        (ONE-STRING 
                         "CONTEXT-OF requires a specific contiguous sequence "
                         "amongst the many in ~A")
                        given-contig))
                   ELSE given-contig))
         (contig-length (SLOTV contig #$Sequence-length))
         (context))

    (declare (ignore temp))

    (IF (> coord contig-length)
        (ERROR (ONE-STRING 
                "~&Coordinate '~A' bigger than length "
                "(~A) of ~A")
               coord contig-length contig))

    (SETF context 
         (CONTEXT-OF-COORD-CALCULATION coord contig precise :VERBOSE verbose))

    (COND
       ((AND (EQUAL (FIRST context) "I")(NOT no-display))
          (LET* ((gene (NAME-OF (SECOND context) SHORT))
                 (gene-length (LENGTH gene))
                 (dir (SIXTH context))
                 (dir-char (IF (EQUAL dir :F) ">" "<")))
            (FORMAT T "~A~&"
                    (JOIN "           "
                       (FIT coord INTO gene-length CENTER)))      
            (FORMAT T "~A~&"
                    (JOIN "    |" 
                       (FIT (FOURTH context) INTO 6 CENTER WITH "-")
                       (FIT "|" INTO gene-length CENTER WITH "-")
                       (FIT (FIFTH context) INTO 6 CENTER WITH "-")
                       "|"))
            (FORMAT T "~A~&"
                    (JOIN "-----" 
                          (FIT gene INTO (+ gene-length 12) 
                               CENTER WITH dir-char)
                          "-----"))
            (FORMAT T "~%~A" "")))
       ((NOT no-display)
          (LET* ((gene1 (NAME-OF (SECOND context) SHORT))
                 (gene2 (NAME-OF (THIRD context) SHORT))
                 (gene-length1 (LENGTH gene1))
                 (gene-length2 (LENGTH gene2))
                 (symbol (FIRST context))
                 (dir (SIXTH context))
                 (dir1)
                 (dir2
                    (COND 
                     ((EQUAL symbol "D")
                        (SETF dir1 "<") ">")
                     ((EQUAL symbol "C")
                        (SETF dir1 ">") "<")
                     ((EQUAL dir :F)
                        (SETF dir1 ">") ">")
                     (T  (SETF dir1 "<") "<"))))

            (FORMAT T "~A~&"
                    (JOIN (REPEAT " " TIMES (+ gene-length1 4))                 
                          (FIT coord INTO 15 CENTER)))
            (FORMAT T "~A~&"
                    (JOIN (REPEAT " " TIMES (+ gene-length1 4))
                          "|"
                          (FIT (FOURTH context) INTO 6 CENTER WITH "-")
                          "|"
                          (FIT (FIFTH context) INTO 6 CENTER WITH "-")
                          "|"
                          ))

            (FORMAT T "~A~&"
                    (JOIN (FIT gene1 INTO (+ gene-length1 4) CENTER WITH dir1)
                          (REPEAT "-" TIMES 7)
                          "*"
                          (REPEAT "-" TIMES 7)
                          (FIT gene2 INTO (+ gene-length2 4) CENTER WITH dir2)
                          ))
            (FORMAT T "~%~%~A" ""))
     ))
                        
    context))

; ---------------------------------------

(DEFUN BB-POSITION-OF (query target in in-each multiple given-test partial
                          &KEY both-strands)
  (LET* ((*bbl-level* (MAX 2 (INCREMENT-BBL-LEVEL)))
         (search-function 'SEARCH)
         (start :start2)
         (test (IF (AND partial (LISTP target))
                   (LAMBDA (qy tg) (INNER-SEARCH qy tg :TEST given-test))
                 given-test))
		 (query-type)
         (query-sequence
          (TYPECASE query
;            ((OR gene protein contiguous-sequence organism)
;			 (SETF query-type 'entity)
 ;            (SEQUENCE-OF query))
            (labeled-sequence
             (SETF query-type 'string)
             (LABELED-SEQUENCE-Sequence query))
            (String 
			  (SETF query-type 'string)
              query)	
            (OTHERWISE 
			 (SETF query-type 'unknown)
             (SETF search-function 'POSITION)
             (SETF start :start)
             query)))        
         (target-type)			 
         (target-sequence
          (TYPECASE target
            ((OR gene protein contiguous-sequence organism)
             (SETF target-type 'entity)
             (SEQUENCE-OF target))
            (labeled-sequence
			 (SETF target-type 'string)
             (LABELED-SEQUENCE-Sequence target))
            (String 
			  (SETF target-type 'string)
              target)
            (OTHERWISE 
             (SETF target-type 'unknown)
             (SETF search-function 'POSITION)
             (SETF start :start)
             target)))
         (unpack-list
          (COND
           ((AND in in-each)
            (ERROR "Can't specify both IN and IN-EACH."))
           ((OR (AND in-each (LISTP target)) 
                (AND (TYPEP target 'organism) (LISTP target-sequence)))
            T)
           (in-each 
            (ERR+ problem  "~A is not a valid type of argument."
                  indent   "The KEYWORD IN-EACH must be followed by:"
                  indent   "a list of items or an organism."
                  advice "Use the appropriate type of argument for IN-EACH"
                  indent   "or use another valid keyword."
                  help~A
                  format-args
                  target *df-name*)
            ) 
           (T NIL)))
         )
  (FLET ((SEARCH-ONE-STRAND (q-seq t-seq strand &KEY (t-start 0))
             (LET* ((q-seq-strand
                       (IF (OR (NOT strand) (= strand 1))
                           q-seq
                           (INVERSION-OF q-seq)))
                    (hit (FUNCALL search-function
                            q-seq-strand t-seq START t-start :TEST test)))
               (IF hit (SETF hit (1+ hit)))
               (COND
                  ((NOT strand) hit)
                  ((NOT hit) NIL)
                  ((= strand 1) (LIST hit :F))
                  (T (LIST hit :B)))
             )))

    (WHEN (AND (NOT (LISTP target))
	           (EQUAL target-type 'String)
               (EQUAL query-type 'Unknown))
        (SETF search-function 'SEARCH)
        (SETF start :start2)
        (SETF query-sequence (STRING-OF query)))
 			 
    (COND
     (unpack-list
      (LOOP
       FOR sequence IN target-sequence
       COLLECT (FORWARD-FUNCALL 'BB-POSITION-OF 
                       query-sequence sequence T NIL multiple test 
                       partial :BOTH-STRANDS both-strands)))
	 ((AND (NOT (LISTP target))
           (NOT (STRINGP query-sequence))
           (NOT (CHARACTERP query)))
      (ERR+ problem (S+ "You asked for a match of '~A' within ~A. "
                        "Matching a protein/gene with a protein/gene is not now permitted.")
            advice (S+ "You might take the SEQUENCE-OF the query or the target")
            help~A
            format-args query 
			   (IF (EQUAL target-type 'Entity)
			       (S+ "the sequence of " (NAME-OF target))
                   (S+ "'" target "'"))
               *df-name*))
     ((NOT multiple)
        (IF both-strands
            (OR (SEARCH-ONE-STRAND query-sequence target-sequence 1)
                (SEARCH-ONE-STRAND query-sequence target-sequence 2))
            (SEARCH-ONE-STRAND query-sequence target-sequence NIL)))
     (T (FOR-EACH strand IN (IF both-strands '(1 2) '(NIL))
             AS matches =
                (XLOOP 
                  INITIALIZE i = 0
                  INITIALIZE hit = T
                  UNTIL (NOT hit)
                  DO (SETF hit (SEARCH-ONE-STRAND query-sequence target-sequence strand :T-START i))
                     (IF hit (SETF i (IF strand (FIRST hit) hit)))
                  WHEN hit
                    COLLECT hit)
             WHEN matches APPEND matches))))))

;; ================= MATCHES-OF-PATTERN functions ==================
(DEFUN Mask-quoted-characters (pattern back-quote-code)
   (LOOP WITH offset = 128
         WITH back-quote-char = (CODE-CHAR back-quote-code)
         FOR i FROM 0 TO (- (LENGTH pattern) 2)
         DO (WHEN (EQUAL "`" (SUBSEQ pattern i (1+ i)))
               (LET* ((quoted-char-string (SUBSEQ pattern (+ i 1)(+ i 2)))
                      (quoted-char (CHAR quoted-char-string 0))
                      (quoted-char-code (CHAR-CODE quoted-char))
                      (quoted-char-new (CODE-CHAR (+ quoted-char-code offset)))
                     )
                 (SETF (CHAR pattern i) back-quote-char)
                 (SETF (CHAR pattern (1+ i)) quoted-char-new)
               )))
   pattern
)

(DEFUN Unmask-quoted-characters (pattern back-quote-code)
   (LOOP WITH offset = 128
         WITH back-quote = (STRING (CODE-CHAR back-quote-code))
         WITH i = 0
         UNTIL (>= i (- (LENGTH pattern) 1))
         AS code = (CHAR-CODE (CHAR pattern i))
         DO (WHEN (= code back-quote-code)
               (LET* ((next-char (CHAR pattern (1+ i)))
                      (next-char-code (CHAR-CODE next-char))
                      (quoted-char (CODE-CHAR (- next-char-code offset)))
                     )
                 (SETF (CHAR pattern (1+ i)) quoted-char)
                 (INCF i)
               ))
            (INCF i)
         FINALLY (RETURN (HARD-STRING-REPLACEX back-quote "\\" pattern)))
)
	 
(DEFUN pattern-to-regex (pattern replacement-list)
  (IF (LISTP pattern)
     (FOR-EACH p IN pattern
          COLLECT (FORWARD-FUNCALL 'PATTERN-TO-REGEX p replacement-list))
     ; ELSE
  (LET* ((char-offset 128)
         (back-quote-code char-offset)
         (result)
         (*bbl-current-case-mode* :case-insensitive)
         (masked-pattern (MASK-QUOTED-CHARACTERS pattern back-quote-code))
        )
     (FOR-EACH (old new) IN replacement-list
          (ASSIGN masked-pattern
                (HARD-STRING-REPLACEX old new masked-pattern)))
     (SETF result (UNMASK-QUOTED-CHARACTERS masked-pattern back-quote-code))
     (LET* ((len (LENGTH result)))  ; Implement "|" = begin or end
       (IF (AND (> len 0) (EQUAL (SUBSEQ result 0 1) "|"))
           (SETF (SUBSEQ result 0 1) "^"))
       (IF (AND (> len 0) (EQUAL (SUBSEQ result (1- len) len) "|")
                (OR (= len 1) (NOT (EQUAL (SUBSEQ result (- len 2)(- len 1)) "\\"))))
           (SETF (SUBSEQ result (1- len) len) "$")))
     result
)))

(DEFUN BioBIKE-pattern-to-regex (pattern)
  (LET* ((char-offset 128)
         (dot (STRING (CODE-CHAR (+ char-offset 1))))
		 (carat (STRING (CODE-CHAR (+ char-offset 3))))
         (replacement-list
            (LIST '("'" "['\"]")
                  '("~#" "\\D")
                  '("~$" "\\W")
                  '("~^" "\\S")
                  '("~@" "\\s")
			      '("#" "\\d")
                   (LIST "$" "\\w")
                   (LIST "^" "\\s")
			      '("@" "\\S")
 				   (LIST "*" dot)
				   (LIST "[~" (S+ "[" carat))
				  '("?..." "*")
			      '("?.." "*?")
				  '("..." "+")
			      '(".." "+?")
			      '("." "\\.")
			       (LIST dot ".")
				   (LIST carat "^")
			       )))
    (PATTERN-TO-REGEX pattern replacement-list)
))

(DEFUN DNA-pattern-to-regex (pattern)
   (LET* ((replacement-list
            '(("M" "[AC]")
              ("R" "[AG]")
              ("W" "[AT]")
              ("S" "[CG]")
              ("Y" "[CT]")
              ("K" "[GT]")
              ("V" "[ACG]")
              ("H" "[ACT]")
              ("D" "[AGT]")
              ("B" "[CGT]")
              ("N" "[ACGT]"))))
    (PATTERN-TO-REGEX pattern replacement-list)
))
		 
(DEFUN Match-of-item-aux (plural? query target in in-each partial test
    &KEY q-label t-label-info compression strand-info coord-info)
  (without-code-walker
   (LET* ((length-threshold 20)
          (compress?)
          (long-list?)
		  (t-label?)
          (q-type 
		     (TYPECASE query
                ((OR labeled-sequence string) 
				  (BB-SEQUENCE-TYPE-OF 
				      (STRING-SEQUENCE-OF query 1 (LENGTH query) 
					     NIL :NONSTRICT T) NIL))
				(OTHERWISE NIL)))
          (both-strands?
             (COND 
                ((LISTP query) NIL)
                ((FIRST strand-info) T)
                ((SECOND strand-info) NIL)
                ((EQUAL q-type 'DNA) T)))
		  (coordinates? (NOT (SECOND coord-info)))
          (result))
   (FLET ((output-unit (value targ t-index)
             (LET ((unit (JOIN  	
                           (IF q-label query)
                           (IF t-label?
                               (COND
                                  ((IS-FRAME? targ) targ)
                                  ((TYPEP targ 'Labeled-sequence) 
                                       (NAME-OF targ))
                                  (T t-index)))
                           (IF coordinates? 
						       (IF value value '(NIL)))
                           AS-LIST)))
               (IF (= (LENGTH unit) 1)
                   (FIRST unit)
                   unit))))				   
   (COND
      ((AND (IS-TABLE? target) (> (GARRAY-RANK target) 1))
       (ERR+ problem 
             "At present ~A works on tables only of 1-dimension"
             format-args
             *df-name*)
       )
      ((IS-TABLE? target)
           (LET ((answers 
                   (IF-TRUE partial
                       THEN (GMAP (LAMBDA (x) (INNER-SEARCH query x :TEST test)) target)
                       ELSE (GMAP (LAMBDA (x) (FUNCALL test query x)) target))))
             (LOOP FOR answer IN answers
                   FOR label IN (GARRAY-COMPONENT-INDICES target)
                   WHEN (AND answer T) ; STRINGP label
                     COLLECT label)))
      (T (SETF result
            (BB-POSITION-OF query target in in-each plural? 
                  test partial :BOTH-STRANDS both-strands?))
	     (SETF long-list? (AND (LISTP result) 
	                          (> (LENGTH result) length-threshold)))
         (SETF compress?
            (COND
               ((FIRST compression) T)     ; +compression
               ((SECOND compression) NIL)  ; -compression
               (long-list? T)
			   ((NOT coordinates?))))
	     (SETF t-label?
	        (COND
		       ((FIRST t-label-info) T)    ; +target-label
			   ((SECOND t-label-info) NIL) ; -target-label
			   (in-each T)
			   ; (compress? T)
               ))
         (COND
            ((NOT result) NIL)
            ((AND (LISTP result) (ALL-FALSE result)) NIL)
			(in-each
			   (FOR-EACH r-item IN (ENSURE-LIST result)
			    FOR-EACH t-item 
				    IN (IF (TYPEP target 'Organism)
                           (REPLICONS-OF target)
                           (ENSURE-LIST target))
                FOR-EACH i FROM 1
				AS matches-for-targets 
				  = (FOR-EACH match 
                        IN (IF (AND plural? r-item) r-item (LIST r-item))
				WHEN (OR (NOT compress?) match)
						   COLLECT (OUTPUT-UNIT match t-item i))
                APPEND matches-for-targets))
		    (T (OUTPUT-UNIT result target 1)))))
))))



(DEFUN Match-of-enzyme-aux (plural? query target case-sensitive)
  (LET* ((*suppress-warnings* T)
         (enzyme-info 
            (LET ((temp-info (OR (GETHASH (STRING query) *digesters*) query)))
               (TYPECASE temp-info
                  (List temp-info)
                  (Symbol (ERROR "Enzyme ~A not found" query))
                  (String 
                      (IF (EQUAL (BB-SEQUENCE-TYPE-OF query T) 'DNA)
                            (LIST query ''D (DNA-PATTERN-TO-REGEX query) 1)
                            (LIST query NIL query 1))))
            ))
         (enzyme-seq (THIRD enzyme-info))
         (enzyme-type (SECOND enzyme-info))
         (cut-site (FOURTH enzyme-info))
         (DNA? (EQUAL enzyme-type ''D))
         (symmetrical? 
           (AND DNA?
                (EQUAL enzyme-seq (INVERSION-OF enzyme-seq))))
         (both-strands? (AND DNA? (NOT symmetrical?)))
		 (strand-info (LIST both-strands? (NOT both-strands?) DNA? NIL))
         (result
            (IF enzyme-seq
               (FORWARD-FUNCALL 'MATCH-OF-PATTERN-AUX plural? enzyme-seq 
                  target '(T NIL NIL NIL) :STRAND-INFO strand-info 
                  :CASE-SENSITIVE case-sensitive)))
         )
    (BBL::SORT   
      (FOR-EACH hit IN result
           AS coord = 
             (IF cut-site
                 (+ (FIRST hit) cut-site -1)
                 (BBL::ROUND (+ (FIRST hit) (SECOND hit))))
           COLLECT coord))
 ))


(DEFUN Match-of-aux-aux (plural? type query in in-each target  
                                 return case-sensitive &KEY partial cross-lines)
  (1+ return)  ; stop warning. May be used someday
  (LET ((test (IF case-sensitive 'EQUAL 'BB-SAME)))
    (COND
     ((OR (NULL type) (SYMBOL= type 'ITEM))
      (MATCH-OF-ITEM-aux plural? query target in in-each partial test))

     ((SYMBOL= type 'ITEMS)
      (MATCH-OF-ITEM-aux T query target in in-each partial test))

     ((OR (SYMBOL= type 'PATTERN) (SYMBOL= type 'PATTERNS))
      (MATCH-OF-PATTERN-aux plural? query target '(T T T T)
             :CASE-SENSITIVE case-sensitive :CROSS-LINES cross-lines))
           
     ((OR (SYMBOL= type 'ENZYME) (SYMBOL= type 'ENZYMES))
      (MATCH-OF-ENZYME-aux plural? query target test))

     ((OR (AND (LISTP target)(ANY-FALSE (MAPCAR 'IS-FRAME? target)))
          (AND (NOT (LISTP target)) (NOT (IS-FRAME? target))))
      (ERR+ problem "'~A' interpreted as a slot, so '~A' must be a frame or"
            indent  "a list of frames, but it isn't"
            format-args type target)
      )

     (T (MATCH-OF-SLOT-aux plural? type query target partial test)))))


; ------------------ TAB-DELIMITED ------------

(DEFUN Tab-delimited (list)
"
(TAB-DELIMITED list)
 - Takes a list of the form ((a b c d ...) (e f g h ...) (...))
 - Returns a string with tabs between each item and newlines between
     each line
"
  (STRING-JOIN 
     (LOOP FOR line IN LIST
           COLLECT 
             (STRING-JOIN 
                 (IF-TRUE (LISTP line)
                     THEN (LOOP FOR item IN line
                                COLLECT (BB-STRING-OF item))
                     ELSE (LIST (BB-STRING-OF line)))
                 #\tab))
     #\newline))

(DEFUN JALVIEW-ALIGNMENT (alignment)
  (WITH-TEMP-FILE-IN (ali_ cl-user::*webtmp-directory* :name "ali" 
                           :delete? NIL :type "fa")
    (WITH-OPEN-FILE (ali ali_ :direction :output)
      (FOR-EACH lseq IN alignment
                AS l = (REF lseq 1)
                AS s = (REF lseq 2)
                DO (FORMAT ali "~a~a~%~a~%" ">" l s)))
    (LET ((aliname (LAST (SPLIT (STRING-OF ali_) EVERY "/"))))
      (WITH-TEMP-FILE-IN (jalview_ cl-user::*webtmp-directory* :name "jalviewhtml" 
                                   :delete? NIL :type "html")
        (WITH-OPEN-FILE (jalviewhtml jalview_ :direction :output)
          (FORMAT jalviewhtml 
                  "~a~%~a~%~a~%~a~a~a~a~%~a~%~a~%~a~%~a~%~a~%"
                  "<html><head><TITLE>JalView - Applets</TITLE></head><body>"
                  " <applet code=\"jalview.bin.JalviewLite\""
                  "    width=\"140\" height=\"35\""
                  "    archive=\"jalviewApplet.jar\">"
                  "    <param name=\"file\" value=\""
                  (FIRST aliname)
                  "\">"
                  "    <param name=\"defaultColour\" value=\"Clustal\">"
                  "                        <param name=\"showFullId\" value=\"false\">"
                  "    <param name=\"APPLICATION_URL\" value=\"launchApp.jnlp\">"
                  " </applet>"
                  "</body></html>"))
        (LET* ((jalview-u (LAST (SPLIT (STRING-OF jalview_) EVERY "/")))
               (URL-to-jalview 
                (FORMAT NIL "~a~a~a~a" 
                        "http://" cl-user::*WEBLISTENER-MACHINE-NAME* "/biobikewww/" 
                        (FIRST jalview-u))))
          (WB::MAKE-URL :PATH URL-to-jalview  :DISPLAY-STRING 
                        "View alignment" :TARGET "_blank")
          )))))


(defun alignment-of-aux 
       (sequence-list line-length group-length remove-duplicates no-consensus 
                      no-return no-display colored label-by-organism
                      label-by-nickname gap-open-penalty gap-extension-penalty
                      &KEY no-gapped-columns)
  (let* ((processed-sequence-list
          (IF remove-duplicates
              (REMOVE-DUPLICATES sequence-list :TEST #'EQUAL)
            sequence-list))
         (sequences (SEQUENCES-OF processed-sequence-list))
         (alignment nil)
         (sequences-to-display nil)
         (names nil)
         (*suppress-warnings* T))

    (IF (< (LENGTH sequences) 2)
        (ERROR "Need at least two sequences to align, not: '~A'"
               sequences))
    (SETF names
          (LOOP FOR item IN processed-sequence-list
            FOR item-number FROM 1
            AS org = (UNLESS (OR (TYPEP item 'String) 
                                 (TYPEP item 'LABELED-SEQUENCE))
                        (ORGANISM-OF item))
            AS org-name  
              = (COND
                   ((NULL org) NIL)
                   (label-by-organism (NAME-OF org))
                   (label-by-nickname (FIRST (REF org #$nicknames))))
            AS name  
              = (COND
                   (org-name org-name)
                   ((STRINGP item)
                       (S+ "Seq " item-number ":" (bbl::FIRST 5 item)))
		   (T (NAME-OF item SHORT)))
            DO (IF (TYPEP item 'Organism)
               (ERR+ problem  "The query ~A is an organism."
                     indent  "At present, ALIGNMENT-OF is not capable of "
                     indent  "aligning whole genomes."
                 advice  "Use a portion of the sequence, obtained with SEQUENCE-OF." 
                 help~A
                 format-args
                 item "ALIGNMENT-OF"))
            COLLECT name))
    (SETF alignment (ALIGN (INTERLEAVE names sequences)
                       :GAPOPEN gap-open-penalty :GAPEXT gap-extension-penalty))
    (SETF sequences-to-display (SLOTV alignment #$alignments))
  
    (UNLESS (or no-consensus colored)
      (SETF sequences-to-display
            (APPEND sequences-to-display
                    (LIST (LIST "consensus" (SLOTV alignment #$Consensus))))))
    (WHEN no-gapped-columns
       (LET* ((raw-gap-columns
                 (FOR-EACH seq IN sequences-to-display
                      APPEND (BB-POSITION-OF "-" (SECOND seq) T NIL T 'EQUAL NIL)))
              (gap-columns (BBL::SORT (REMOVE-DUPLICATES raw-gap-columns)))
              (ungapped-columns 
                  (SUBTRACT-SET gap-columns 
                     FROM (FROM 1 TO (LENGTH (SECOND (FIRST sequences-to-display))))))
             )
          (SETQ sequences-to-display
             (FOR-EACH (label seq) in sequences-to-display
                  WHEN ungapped-columns
                     COLLECT (LIST label (REF seq ungapped-columns))))))
      
    (COND
     (COLORED (JALVIEW-ALIGNMENT sequences-to-display))
     (no-display sequences-to-display)
     (no-return (DISPLAY-SEQUENCE sequences-to-display
                                  :LINE-LENGTH line-length :SEGMENT-LENGTH group-length))
     (T (DISPLAY-SEQUENCE sequences-to-display
                          :LINE-LENGTH line-length :SEGMENT-LENGTH group-length) sequences-to-display)
     )))

(defun counts-of-aux
       (each not query in in-each case-sensitive 
             case-insensitive both-strands labeled)
  (IF each (WARN (S+ "The EACH option is not necessary in COUNTS-OF."
                     "~&(It IS necessary in COUNT-OF, if you wish to consider each "
                     "element separately in a list of queries)")))
  (ERROR-IF-MORE-THAN-ONE in in-each)
  (COND
   (in-each
    (IF (NOT (LISTP in-each))
        (ERR+ problem  "~A is not a valid type of argument."
              advice   "The IN-EACH keyword requires a list of items."
              help~A
              format-args in-each *df-name*)
      ) 
    (LOOP FOR given-target IN in-each
      AS target = (IF (TYPEP given-target 'Labeled-sequence)
                      (LABELED-SEQUENCE-SEQUENCE given-target)
                    given-target)
      COLLECT (COUNT-OF-AUX query T not target
                            case-sensitive case-insensitive :LABELED labeled
                            :BOTH-STRANDS both-strands)))
   ((AND each (NOT in-each) (UNPROVIDED in))
    (LOOP FOR item IN query
      COLLECT (COUNT-OF-AUX item NIL not in
                            case-sensitive case-insensitive :LABELED labeled
                            :BOTH-STRANDS both-strands)))
   (T (COUNT-OF-AUX query T not (OR in in-each)  
                    case-sensitive case-insensitive :LABELED labeled
                    :BOTH-STRANDS both-strands))))

(defun display-list-aux
       (each-value list columns-of-length padding alignment
        Centered Flush-left Flush-right &KEY labels)
  (let ((number-of-columns nil) (column-numeric?))
    (IF (AND (SYMBOL= each-value 'EACH) (NULL list))
        (SETF each-value NIL))
    (COND
      ((SYMBOL= each-value 'EACH)
       (IF (NOT (LISTP list))
           (ERR+ PROBLEM "The EACH option must be followed by a list."))
       (SETF number-of-columns 
             (APPLY 'MAX (MAPCAR (LAMBDA (x) (LENGTH (ENSURE-LIST x))) list))))
      ((LISTP list)
       (SETF number-of-columns (LENGTH list))
       (SETF list (LIST list)))
      (T (SETF list (LIST list))
         (SETF number-of-columns 1)))

    (IF labels (SETF list (CONS labels list)))       
    (FOR-EACH sublist IN list
	     INITIALIZE col-numeric? 
	       = (MAKE-GARRAY (LIST number-of-columns) :INITIAL-ELEMENT T)
         INITIALIZE length-of-col
           = (MAKE-GARRAY (LIST number-of-columns) :INITIAL-ELEMENT -1)
         AS label? = labels THEN NIL
         (FOR-EACH item IN (ENSURE-LIST sublist)
          FOR-EACH column FROM 0
              AS max-length 
                 = (IF (>= column (GARRAY-CURRENT-TOTAL-SIZE length-of-col))
                       -1
                      (REF length-of-col column))
              AS item-string 
			     = (IF (LISTP item)
				       (FORMAT NIL "~A" item)
				       (BB-STRING-OF item))
              AS item-length = (LENGTH item-string)
              (IF (AND (NOT label?) (NOT (NUMBERP item)))
				  (SETF (REF col-numeric? column) NIL))
              (IF (> item-length max-length)
                  (setf (REF length-of-col column) item-length)))
          FINALLY
	        (PROGRAM
              (SETF column-numeric? (GMAP 'IDENTITY col-numeric?))
              (SETF columns-of-length 
                 (COND
                    ((NULL columns-of-length)
                       (GMAP 'IDENTITY length-of-col))
                    ((LISTP columns-of-length) 
                       (FOR-EACH calc-column IN (GMAP 'IDENTITY length-of-col)
                            AS given-column = (POP columns-of-length)
                            COLLECT (OR given-column calc-column)))
                    (T (REPEAT-aux columns-of-length T number-of-columns T NIL T))))
		      ))
    
    (FOR-EACH sublist IN list
         AS temp-alignment = (COPY-LIST alignment)
     (APPLY 'DISPLAY-LINE 
       (FOR-EACH item IN (ENSURE-LIST sublist)
        FOR-EACH column IN columns-of-length
		FOR-EACH numeric? IN column-numeric?
            AS specific-alignment = (POP temp-alignment)
            AS item-string 
               = (IF (LISTP item)
			         (FORMAT NIL "~A" item)
				     (BB-STRING-OF item))
			AS flush-right? = (AND (NOT (MEMBER specific-alignment '("C" "L") 
                                               :TEST 'BB-SAME))
                                   (OR (BB-SAME specific-alignment "R")
                                       flush-right
			                           (AND (NOT flush-left) (NOT centered) numeric?)))
			AS flush-left? = (AND (NOT (MEMBER specific-alignment '("C" "R") 
                                               :TEST 'BB-SAME))
                                  (OR (BB-SAME specific-alignment "L")
                                      flush-left
			                          (AND (NOT flush-right?) (NOT centered))))
			AS centered? = (AND (NOT (MEMBER specific-alignment '("L" "R") 
                                               :TEST 'BB-SAME))
                                (OR (BB-SAME specific-alignment "C")
                                    centered))
            AS pre-fit-string 
			  = (BB-FIT item-string column
                    :CENTER? centered? :FLUSH-RIGHT flush-right?
                    :FLUSH-LEFT flush-left?)
			AS fit-string
			  = (IF (IS-FRAME? item)
			        (REPLACE-STRING-WITH-FRAME pre-fit-string item)
					(LIST pre-fit-string))
            APPEND 
              (BB-JOIN (LIST fit-string 
                  (MAKE-STRING padding :INITIAL-ELEMENT #\space))
				  :AS-LIST T))))
))
	
(DEFUN Parse-meme-results (meme-results)
  (LET* ((result-string (STRING-OF meme-results))
         (path (FORWARD-FUNCALL 'MATCH-OF-PATTERN-ONE-ITEM-aux
          		 "http[^ ]*" result-string 1 '(NIL NIL T T) NIL NIL NIL T))
         (web-page (bio::WEB-PAGE-CONTENTS path))
         (hits-info (FORWARD-FUNCALL 'MATCH-OF-PATTERN-ONE-ITEM-aux 
                      (JOIN "summary_doc.*?width = *?(\\d+)"
                            ".*?sites = *?(\\d+)"
                            ".*?llr = *?(\\d+)"
                            ".*?E-value = ([\\d.e-]+)"
                            ".*?sites_doc.>SITES</A>(.*?)<HR>"
                            ".*?regular expression.*?<HR>.?([A-Z[\\]]+)")
					   web-page *unprovided+* '(NIL NIL NIL T) NIL NIL NIL T))
         )
   (FOR-EACH hit-info IN hits-info
        AS width = (FIRST hit-info)
        AS sites = (SECOND hit-info)
        AS llr = (THIRD hit-info)
        AS e-value = (FOURTH hit-info)
        AS line-glob = (FIFTH hit-info)
        AS regex = (SIXTH hit-info)
        AS lines = (FORWARD-FUNCALL 'MATCH-OF-PATTERN-ONE-ITEM-aux 
		              "<TR><TD>(.*?)<.*?RIGHT>([0-9]*).*?RIGHT>([0-9.e-]*).*?RIGHT><TT>.*?</TT>(.*?)<TD ALIGN=LEFT>" 
                 line-glob 
				*unprovided+* '(NIL NIL NIL T) NIL NIL NIL T)
        AS seqs = NIL
        AS matches =
           (FOR-EACH info IN lines
		        AS motif-match = (NEW-TABLE '($))
				(ASSIGN (REF motif-match 'name) = (FIRST info))
                (ASSIGN (REF motif-match 'start) = (SECOND info))
                (ASSIGN (REF motif-match 'e-value) = (THIRD info))
                (ASSIGN (REF motif-match 'seq) = (REMOVE-HTML (FOURTH info)))
                (PUSH (REF motif-match 'seq) seqs)
                COLLECT motif-match)
       AS alignment = (REVERSE seqs)
	   AS motif = (NEW-TABLE '($))
	   (ASSIGN (REF motif 'WIDTH) =  width)
	   (ASSIGN (REF motif 'SITES) = sites)
       (ASSIGN (REF motif 'LOG-LIKELIHOOD-RATIO) = llr)
	   (ASSIGN (REF motif 'E-VALUE) = e-value)
	   (ASSIGN (REF motif 'MATCHES) = matches)
       (ASSIGN (REF motif 'ALIGNMENT) = alignment)
       (ASSIGN (REF motif 'REGEX) = regex)
       COLLECT motif)
  ))

(DEFUN GC-FRACTION-OF-aux (entity)
  (LET* 
    ((sequence 
      (TYPECASE entity
         (Gene (GENE-SEQUENCE-OF entity 1 *unprovided+* NIL))
         (Contiguous-sequence (CONTIG-SEQUENCE-OF entity 1 *unprovided+* NIL))
         (String entity)
         (Organism (SEQUENCE-OF entity))
         (Labeled-sequence (LABELED-SEQUENCE-SEQUENCE entity))
         (List (SEQUENCE-OF entity))
         (OTHERWISE
            (ERROR "INTERNAL ERROR in GC-FRACTION-OF (type): Please report"))))
    (length (SUM-OF (LENGTHS-OF (ENSURE-LIST sequence))))
    (freq-counts (FREQUENCY-COUNT T (ENSURE-LIST sequence)))
    (GC-count
       (LOOP FOR char IN '(#\G #\C #\g #\c)
             AS p = (POSITION char freq-counts :TEST 'CHAR= :KEY 'FIRST)
             AS n = (IF-TRUE p 
                        THEN (SECOND (Nth p freq-counts)) 
                        ELSE 0)
             SUM n))
    )
  (FLOAT (/ GC-count length))
))

