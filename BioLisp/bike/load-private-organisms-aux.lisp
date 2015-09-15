;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE bbi)

; ===================================================
;            LOAD PRIVATE ORGANISM FUNCTIONS 
; ===================================================

; =============== UTILITIES =================

(DEFUN Number-of-letters (entity)
   "Counts the number of letters in a string or symbol"
   (LET* ((alpha-list (SPLIT "abcdefghijklmnopqrstuvwxyz"))
          )
     (SUM-OF (COUNT-OF EACH alpha-list IN entity))
))

(DEFUN Join-lines (string &KEY trim)
  "Replaces line-breaks with spaces"
  (IF (LISTP string)
      (FOR-EACH item IN string
           COLLECT (FORWARD-FUNCALL 'JOIN-LINES item))
      (LET* ((lines (SPLIT string EVERY (STRING *newline*))))
        (IF trim (SETF lines (TRIM lines)))
        (JOIN lines BY " "))
))

(DEFUN Remove-from-list (elements list)
   "Removes elements form a list without changing its order"
   (FOR-EACH item IN list
        WHEN (NOT (MEMBER item elements :TEST 'SAME))
           COLLECT item))

(DEFUN Remove-excess-blanks (string)
  "Trims spaces left and right and removes internal consecutive spaces"
  (IF (LISTP string)
      (FOR-EACH item IN string
           COLLECT (FORWARD-FUNCALL 'REMOVE-EXCESS-BLANKS item))
      (JOIN (MATCH-OF-PATTERN-aux T "([^ ]+)" string '(NIL NIL NIL T))
          BY " "))
)

; =============== PROCESS-MULTIPLE-FILES =========

(DEFUN expand-file-list (file1 file2)
 (LET* ((contig-max 10000)
        (file1-parts
           (MATCHES-OF-PATTERN "(*..)(#...)(`.*)?" IN file1 
              +1st-MATCH-ONLY +SUB-MATCHES -DISPLAY))
        (file2-parts
           (MATCHES-OF-PATTERN "(*..)(#...)(`.*)?" IN file2 
              +1st-MATCH-ONLY +SUB-MATCHES -DISPLAY))
        (constant-part1 (FIRST file1-parts))
		(constant-part2 (FIRST file2-parts))
		(numeric-part1 (SECOND file1-parts))
		(numeric-part2 (SECOND file2-parts))
		(constant-part
            (COND 
               ((NOT (EQUAL constant-part1 constant-part2))
                   (ERR+ PROBLEM "File names '~A' and '~A' must"
                         INDENT "be the same preceding the numeric part."
                         FORMAT-ARGS file1 file2))
               ((NOT (= (LENGTH numeric-part1)(LENGTH numeric-part2)))
                   (ERR+ PROBLEM "Numeric parts of '~A' and '~A' must"
                         INDENT "have the same lengths."
                         FORMAT-ARGS file1 file2))
               (T (FIRST file1-parts))))
         (numeric1 (CONVERT numeric-part1 TO Number))
         (numeric2 (CONVERT numeric-part2 TO Number))
		 (number-length (LENGTH numeric-part1))
         (number-of-files (- numeric2 numeric1 -1))
        )
   (COND
      ((< number-of-files 1)
          (ERR+ PROBLEM "Numeric part of the first file, '~A', can't"
                INDENT  "be greater than the numeric part of the"
                INDENT  "second file, '~A'."
                FORMAT-ARGS file1 file2))
      ((> number-of-files contig-max)
          (ERR+ PROBLEM "Can't handle so many contigs (~A) between"
                INDENT "first file, '~A', and second file, '~A'."
                FORMAT-ARGS (- numeric2 numeric1 -1) file1 file2))
      )
   (FOR-EACH numeric FROM numeric1 TO numeric2
        AS numeric-part = (FIT numeric INTO number-length WITH "0" FLUSH-RIGHT)
        AS file-name = (JOIN constant-part numeric-part)
        COLLECT file-name)
))

(DEFUN list-of-files (file-specification)
  (LET ((arrow-positions (POSITIONS-OF '-> IN file-specification))
     	(spec-len (LENGTH file-specification))
		(final-list NIL)
       )
    (FOR-EACH arrow-pos IN (REVERSE arrow-positions)
         AS pos2 = (- arrow-pos 2)
         AS pos3 = (+ arrow-pos 2)
         AS left-list = 
            (COND 
			   ((>= pos2 1)
                   (SUBLIST file-specification FROM 1 TO pos2))
               ((< pos2 0)
                   (ERR+ PROBLEM "No filename to left of '-> ~A'"
				         FORMAT-ARGS (SECOND file-specification))))
         AS right-list =
            (COND 
			   ((<= pos3 spec-len)
                   (SUBLIST file-specification FROM pos3))
               ((> pos3 (1+ spec-len))
                   (ERR+ PROBLEM "No filename to right of '~A ->'"
				         FORMAT-ARGS (SECOND (REVERSE file-specification)))))
	     AS central-list 
		   = (EXPAND-FILE-LIST (REF file-specification (1- arrow-pos))
		                       (REF file-specification (1+ arrow-pos)))
		 (SETF final-list 
		    (JOIN central-list right-list final-list AS-LIST))					   
         (SETF file-specification left-list)
		 (SETF spec-len (LENGTH file-specification)))
     (JOIN file-specification final-list AS-LIST)
))
         		 
(DEFUN append-files 
   (files 
     &KEY (base-url
            "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nuccore&id=")
          (retrieval-type "&rettype=gbwithparts"))
  (JOIN
     (FOR-EACH file-name IN files
          AS url = (JOIN base-url file-name retrieval-type)
          COLLECT (bio::web-page-contents url))
     BY *newline* AS-STRING)
)


; =============== PROCESS-HEADER-INFORMATION=========

(DEFUN Make-nickname-for (full-name)
   "Construct nickname from fullname"
   (LET* ((forbidden (LIST "RNA" "DNA" "virus" "sequence" "sequences"  
                             "associated"))
		  (discouraged (LIST "alpha" "beta" "gamma" "mosaic" "disease"))
		  (pre-name-words (SPLIT full-name EVERY "_"))
		  (name-words (REMOVE-FROM-LIST forbidden pre-name-words))
		  (number-of-words (LENGTH name-words))
		  (last-word (BBL::LAST name-words))
		  (last-word-numbers? (MATCH-OF-PATTERN-aux T "^\\d+$" last-word '(T T T T)))
		  (short-names NIL) ; **** initialize to prefixes of all organisms
		  (candidates
             (LIST (IF (AND (NOT last-word-numbers?) 
                       (NOT (MEMBER last-word discouraged :TEST 'EQUAL)))
                       last-word)
                   (IF (> number-of-words 1)
                       (JOIN (BBL::FIRST 4 IN (FIRST name-words)) "-" last-word))
                   (IF (> number-of-words 2)
                       (JOIN (BBL::FIRST 4 IN (FIRST name-words)) "-"
                             (BBL::FIRST 3 IN (SECOND name-words)) "-"
                             last-word))
                   (JOIN name-words)))
		  (short-name 
            (FOR-EACH candidate IN candidates
                 (IF (AND (> (LENGTH candidate) 2)
                          (NOT (MEMBER candidate short-names :TEST 'EQUAL)))
                     (RETURN candidate))
                 FINALLY (DISPLAY-LINE "No nickname for: " full-name *tab* candidates)))
		 )
     short-name
))

(DEFUN Process-header-information (headers)
   (LET ((locus)(length)(molec-type)(topology)(date)(definition)(version)
         (organism)(phylogeny)(rest)(plasmid)
         (references) (prime-reference))

     (ASSIGN (locus length molec-type topology date definition version
              organism phylogeny rest)
       = (MATCH-OF-PATTERN-aux NIL
            (JOIN "LOCUS *(\\S*) * (\\d*) bp *(\\S*)  *"
                  "(\\S*).*?(\\d*-...-....)\\n.* *DEFINITION *(.*?)\\n"
                  ".* *VERSION *(.*?)\\n.*\\n  *ORGANISM *(.*?)\\n"
                  " *(.*?\\.)\\n(.*)")
            headers '(NIL NIL NIL T) :CROSS-LINES T))
     (ASSIGN plasmid (MATCH-OF-PATTERN-aux NIL "plasmid ([^ ,]*)"
	                    definition '(NIL NIL NIL T)))
     (IF (NOT locus)
         (ASSIGN (locus length molec-type topology)
           = (MATCH-OF-PATTERN-aux NIL
                (JOIN "LOCUS *(\\S*) * (\\d*) bp *(\\S*)  *"
                      "(\\S*)")
                headers '(NIL NIL NIL T))))
     (ASSIGN references = (SPLIT rest EVERY "REFERENCE"))
     (ASSIGN prime-reference
       = (BBL::LOOP FOR reference IN references
           AS (ignore-auth authors title journal ignore)
              = (BBL::FIRST 5 IN
			    (JOIN 
                (MATCH-OF-PATTERN-aux NIL
                   (JOIN "(AUTHORS|CONSRTM) *(.*?)\\n.*?"
                         "TITLE *(.*?)\\n.*?"
                         "JOURNAL *(.*?)\\n.*?"
                         "(COMMENT|$)")
                   reference '(NIL NIL NIL T) :CROSS-LINES T)
                  '(NIL NIL NIL NIL NIL)))
           WHEN authors
            (PROGN 
              (IDENTITY ignore-auth) (IDENTITY ignore) ; to squelch warnings
              (RETURN
                (APPLY-FUNCTION-OF (field)
                    = (JOIN (SPLIT field EVERY (STRING *newline*)) BY " ")
                    TO (LIST authors title journal))))))

     (LIST locus length molec-type topology date definition version 
      organism plasmid phylogeny prime-reference)))

; =============== PROCESS-GENES-INFORMATION=========
  
(DEFUN Parse-extent (type line length)
  ; RETURNS join complement from inc-left inc-right to
   (LET* ((pattern
             "(\\<|&lt;)?(\\d*)\\..*\\.(\\>|&gt;)?(\\d*)")
          (complement (OR (EQUAL type "COMPLEMENT")
                          (EQUAL type "JOIN-COMP")))
          (join (OR (EQUAL type "JOIN")
                    (EQUAL type "JOIN-COMP")))
          (direction (IF complement "B" "F"))
          (architecture 
             (IF-TRUE join 
                 THEN (LET ((n2 (MATCH-OF-PATTERN-aux NIL
                                   "^\\d+..(\\d+),\\d+..\\d+\\)?$"
                                   line '(NIL NIL NIL T))))
                        (IF-FALSE (EQUAL n2 length) THEN line))))
          (from)(inc-left)(inc-right)(to)(inc-start)(inc-end))
     (ASSIGN (inc-left from inc-right to)
         = (MATCH-OF-PATTERN-aux NIL pattern line '(NIL NIL NIL T)))
     (IF-TRUE (EQUAL direction "F")
         THEN (ASSIGN inc-start (EXISTS inc-left))
              (ASSIGN inc-end (EXISTS inc-right))
         ELSE (ASSIGN inc-start (EXISTS inc-right))
              (ASSIGN inc-end (EXISTS inc-left)))
     (LIST direction from inc-start inc-end to architecture)))

(DEFUN Parse-gene-features (lines)
   (FOR-EACH line IN (JOIN lines "END" AS-LIST)
    FOR-EACH line-number FROM 1
        INITIALIZE old-tag = ""
        INITIALIZE value-buffer = ""
        INITIALIZE exit = NIL
        UNTIL exit
        AS (tag value continuation type type-def
            comp-join complement join 1st-line)
         = (LIST NIL NIL NIL NIL NIL NIL NIL NIL NIL)
        AS (tag-to-go value-to-go) = (LIST NIL NIL)
        (LENGTH type-def)  ;; to squelch warning
        (IF (NOT
              (SETF comp-join (MATCH-OF-PATTERN-aux NIL "^ *complement\\(join(.*)\\)"
                                  line '(NIL NIL NIL T))))
         (IF (NOT
               (MULTIPLE-VALUE-SETQ (complement join)
                 (VALUES-LIST (MATCH-OF-PATTERN-aux NIL "^ *join\\((complement\\))(.*)"
                                 line '(NIL NIL NIL T)))))
         (IF (NOT
               (SETF join (MATCH-OF-PATTERN-aux NIL 
                     "^ *join(.*)"
                     line '(NIL NIL NIL T))))
          (IF (NOT
               (MULTIPLE-VALUE-SETQ (complement 1st-line)
            (VALUES-LIST (MATCH-OF-PATTERN-aux NIL
                             "^ {1,19}(complement\\()?((\\<|&lt;)?(\\d.*\\d))"
                             line '(NIL NIL NIL T)))))
           (IF (NOT
                 (MULTIPLE-VALUE-SETQ (tag value) 
                    (VALUES-LIST (MATCH-OF-PATTERN-aux NIL "^ */([^=]*)=\\\"?([^\\\"]*)" 
                                     line '(NIL NIL NIL T)))))
            (IF (NOT
                 (SETF continuation 
                     (MATCH-OF-PATTERN-aux NIL "^ {21}([^/\\\"][^ \\\"]*)" line 
                                 '(NIL NIL NIL T))))
                (MULTIPLE-VALUE-SETQ (type type-def) 
                     (VALUES-LIST (MATCH-OF-PATTERN-aux NIL "^ {1,19}([^ ]+) *(.*)" 
                                     line '(NIL NIL NIL T))))))))))
        (COND
          ((OR comp-join (AND join complement))
              (MULTIPLE-VALUE-SETQ (old-tag value-buffer)
                 (VALUES-LIST (LIST "JOIN-COMP" (OR comp-join join)))))
           (complement
              (MULTIPLE-VALUE-SETQ (old-tag value-buffer) (VALUES-LIST (LIST "COMPLEMENT" 1st-line))))
           (join
              (MULTIPLE-VALUE-SETQ (old-tag value-buffer) (VALUES-LIST (LIST "JOIN" join))))
           (1st-line
              (MULTIPLE-VALUE-SETQ (old-tag value-buffer) (VALUES-LIST (LIST "EXTENT" 1st-line))))
           (tag 
              (MULTIPLE-VALUE-SETQ (tag-to-go value-to-go) (VALUES-LIST (LIST old-tag value-buffer)))
              (MULTIPLE-VALUE-SETQ (old-tag value-buffer) (VALUES-LIST (LIST tag value))))
           ((OR type (EQUAL line "END"))
              (MULTIPLE-VALUE-SETQ (tag-to-go value-to-go) (VALUES-LIST (LIST old-tag value-buffer)))
              (SETF exit T))
           (continuation
              (SETF value-buffer 
                     (JOIN value-buffer " " continuation))))
         WHEN tag-to-go
           COLLECT (LIST tag-to-go value-to-go)))
       
(DEFUN Parse-architecture (architecture)
  (LET ((arch-list (JOIN "(" architecture ")")))
    (SETF arch-list 
       (bbl::REPLACE INTO arch-list REPLACING-EVERY "," WITH ")("))
    (SETF arch-list 
       (bbl::REPLACE INTO arch-list REPLACING-EVERY ".." WITH " "))
  (STRING-TO-LIST arch-list)
))


(DEFUN Process-tags (tag-info length)
  (LET ((locus-tag)(gene)(note)(protein-id)(product)(translation)
        (direction)(from)(inc-left)(inc-right)(to)(architecture))
  (FOR-EACH (tag-string tag-value) IN tag-info
       (COND
          ((MEMBER tag-string (LIST "JOIN" "COMPLEMENT" "EXTENT" "JOIN-COMP")
               :TEST 'EQUAL)
             (ASSIGN (direction from inc-left inc-right to architecture)
                = (PARSE-EXTENT tag-string tag-value length))
             (IF architecture 
                 (SETF architecture (PARSE-ARCHITECTURE architecture))))
          ((EQUAL tag-string "locus_tag")
             (COND
                ((AND locus-tag (EQUAL locus-tag gene)))
                (locus-tag      
                   (ASSIGN note 
                       = (IF note (JOIN note "; " locus-tag) locus-tag))))
             (ASSIGN locus-tag = tag-value))
          ((EQUAL tag-string "note") 
             (COND
                ((OR locus-tag (> (LENGTH tag-value) 12))
                   (ASSIGN note 
                       = (IF note (JOIN note "; " tag-value) tag-value)))
                (T (ASSIGN locus-tag tag-value))))
          ((EQUAL tag-string "gene")
             (IF (NOT locus-tag)
                 (ASSIGN locus-tag tag-value))
             (ASSIGN gene tag-value))
          ((EQUAL tag-string "protein_id")
             (ASSIGN protein-id tag-value))
          ((EQUAL tag-string "product")
             (ASSIGN product tag-value))
          ((EQUAL tag-string "translation")
             (ASSIGN translation (SEQUENCE-OF tag-value DISPLAY-OFF))))
      FINALLY (RETURN (LIST locus-tag direction from inc-left inc-right to
                       architecture gene note product protein-id
                       translation)))))
        

(DEFUN Make-gene-prefix (locus-tag org-prefix)
  (LET* ((keep-prefix NIL)
         (left-part)(num-part)
        )
    (ASSIGN (left-part num-part)
       (MATCH-OF-PATTERN-aux NIL "^(.*_)\\D*(\\d+)$" locus-tag
                '(NIL NIL NIL T)))
    (COND
       ((OR (NOT left-part) (NOT num-part)))
       ((< (LENGTH left-part) 3))
       ((EQUALP (REF left-part 2) "gp"))
       ((AND (> (LENGTH left-part) 3)
             (EQUALP (REF left-part 4) "gene")))
       (T (SETF keep-prefix T)))
    (TRANSLITERATE 
      (IF keep-prefix 
          left-part
          (LET ((result (JOIN org-prefix "-" AS-STRING)))
             (WARN (S+ "Can't parse gene name ('~A') from genbank file. "
                       "Consider specifying the constant part yourself, "
                       "using the GENE-PREFIX option. Using '~A' instead.")
                   locus-tag result)
             result))
      "_" "-")
))

	 
(DEFUN Set-locus-tag (locus-tag0 gene-prefix from to number-string)
  (LET* ((locus-part1)(locus-part2)(extra-part)(num-part)(note)
         (locus-tag)
         (num-len (LENGTH number-string))
         (needs-hyphen (NOT (OR (EQUAL (bbl::LAST gene-prefix) "-")
                                (EQUAL (bbl::LAST gene-prefix) "_"))))
         )
    (IF locus-tag0
        (ASSIGN (locus-part1 locus-part2)
           (MATCH-OF-PATTERN-aux NIL "([^;,]*)[ ;,]*(.*)" locus-tag0
                '(NIL NIL NIL T))))
    (IF-TRUE (> (LENGTH locus-part2) 0)
        THEN (SETF locus-tag0 locus-part1)
             (IF (> (LENGTH (TRIM locus-part2)) 0)
                 (SETF note locus-part2)))
    (COND
       ((NOT locus-tag0))
       ((NOT (AND from to)))
       ((OR (SAME gene-prefix "*")(SAME gene-prefix "**"))
          (SETF locus-tag locus-tag0))
       (gene-prefix
          (ASSIGN (extra-part num-part)
             (OR (MATCH-OF-PATTERN-aux NIL "_(\\D*)(\\d+)" locus-tag0
                      '(NIL NIL NIL T))
                 (MATCH-OF-PATTERN-aux NIL "(\\D*)(\\d+)" locus-tag0
                      '(NIL NIL NIL T))
                 (LIST NIL NIL)))
          (IF (AND locus-tag (NOT num-part))
              (DISPLAY (bio::FORMATN "!!! Gene ~A doesn't have '_' and doesn't end in a number!!!" locus-tag)))
          (IF (OR (EQUALP extra-part "gene")(EQUALP extra-part "gp"))
              (SETF extra-part NIL))
          (SETF locus-tag
             (IF num-part
                 (JOIN gene-prefix 
                       (IF needs-hyphen "_" "") extra-part
                       (FIT num-part INTO num-len WITH "0" FLUSH-RIGHT))
                 (JOIN gene-prefix extra-part number-string AS-STRING))))
      ((OR (NOT locus-tag) 
           (< (NUMBER-OF-LETTERS locus-tag) 3)) 
         (SETF locus-tag (JOIN "gene" number-string)))
     )
    (UNLESS (SAME gene-prefix "**")
      (SETF locus-tag (TRANSLITERATE locus-tag FROM "_" TO "-"))
      (SETF locus-tag (TRANSLITERATE locus-tag FROM " " TO "-")))
    (LIST locus-tag note)
))

 
(DEFUN Process-genes-information (features length organism segment gene-prefix
        org-prefix gene-number num-len)
   (LET* ((genes-source 
             (SIMPLIFY-LIST (SPLIT (SPLIT features EVERY "CDS      ") 
                                   EVERY "RNA      ")))
          (source-info (POP genes-source))
          (host (JOIN-LINES
                  (MATCH-OF-PATTERN-aux NIL "\\/specific_host=.(.*?)\".*?"
                      source-info '(NIL NIL NIL T) :CROSS-LINES T)))
          (host-note (JOIN-LINES
                  (MATCH-OF-PATTERN-aux NIL "\\/note=.(.*?)\""
                     source-info '(NIL NIL NIL T) :CROSS-LINES T)))
          (protein-sequences) (protein-names)
          (genes-info NIL)
          (first-gene? T)
          (protein-gene-equiv NIL))
 
     (ASSIGN genes-info
        =   (FOR-EACH gene-source IN genes-source
                 INITIALIZE all-genes = NIL
                 INITIALIZE all-proteins = NIL
               ; AS ignore0 = (DEFINE current-gene AS gene-source)
                 AS lines 
                   = (SPLIT gene-source EVERY (STRING *newline*))
                 AS tag-info = (PARSE-GENE-FEATURES lines)
                 AS (locus-tag0 direction from inc-left inc-right to
                     architecture gene-name note product protein-id translation)
                   = (PROCESS-TAGS tag-info length)
                 AS encodes-protein? = (IF translation T NIL)
                 AS number-string = 
                    (FIT (INCF gene-number) INTO num-len WITH "0" FLUSH-RIGHT)	
                 AS (locus-tag note2)  
                   = (PROGRAM
                       (WHEN first-gene?
                         (IF (NOT gene-prefix)
                             (SETF gene-prefix (MAKE-GENE-PREFIX locus-tag0 org-prefix)))
                         (SETF first-gene? NIL))
                       (SET-LOCUS-TAG locus-tag0 gene-prefix from to number-string))
                 AS protein-name = (JOIN "p-" locus-tag)

                 (IF note2 (SETF note (JOIN note note2 BY ", ")))
                 (IF-TRUE (MEMBER locus-tag all-genes :TEST 'EQUALP)
                     THEN (DISPLAY-LINE "Duplicate gene " locus-tag " in " 
                             organism "(" segment ")")
                          (ASSIGN locus-tag = NIL)
                     ELSE (PUSH locus-tag all-genes))
                 (IF encodes-protein?
                   (IF-TRUE (MEMBER protein-name all-proteins :TEST 'EQUALP)
                       THEN (DISPLAY-LINE "Duplicate protein " protein-name 
                                 " in " organism "(" segment ")")
                            (ASSIGN locus-tag = NIL)
                       ELSE (PUSH protein-name all-proteins)))
                 (IF-TRUE (AND locus-tag encodes-protein?)
                     THEN (PUSH (SEQUENCE-OF translation DISPLAY-OFF) protein-sequences)
                          (PUSH protein-name protein-names)
                          (PUSH (LIST protein-name locus-tag) protein-gene-equiv))
                 (IF architecture 
                     (DISPLAY-LINE "INTRON in " locus-tag 
                          " in organism " organism "(" segment ")" ": " architecture))
                 WHEN locus-tag
                   COLLECT  
                     (NIL-TO-hyphen
                         (LIST locus-tag segment encodes-protein? direction
                          inc-left inc-right from to architecture 
                          gene-name protein-ID product note))))

    (LIST host (REMOVE-EXCESS-BLANKS host-note) genes-info 
        protein-sequences protein-names 
        (REVERSE protein-gene-equiv) gene-number)
))

; ============== CREATE/MAINTAIN FILES ===============
(DEFUN Create-files-in (path proteins protein-names protein-gene-equiv
                        genes-info sequence2 segment circular?)
  (LET ((genes-info-header
          '((NAME GENOME-COMPONENT ENCODES-PROTEIN DIRECTION
             START-UNKNOWN END-UNKNOWN FROM TO ARCHITECTURE
             GENE-NAME PROTEIN-ID ANNOTATION NOTE)))
        (protein-gene-equiv-header '((NAME GENE)) ))

     (ENSURE-DIRECTORIES-EXIST (JOIN path "proteins/"))
       (BBL::WRITE FROM proteins TO (JOIN path "proteins/proteins.fasta")
              HEADER protein-names FASTA)
       (BBL::WRITE FROM (JOIN protein-gene-equiv-header protein-gene-equiv)
              TO (JOIN path "proteins/proteins.tbl") TABBED)
     (ENSURE-DIRECTORIES-EXIST (JOIN path "genes/"))
       (BBL::WRITE FROM (JOIN genes-info-header genes-info)
              TO (JOIN path "genes/genes.tbl") TABBED)
     (ENSURE-DIRECTORIES-EXIST (JOIN path "documentation/"))
       (BBL::WRITE FROM "" TO (JOIN path "documentation/gene.test"))
     (ENSURE-DIRECTORIES-EXIST (JOIN path "genome/"))
       (BBL::WRITE FROM sequence2 TO (JOIN path "genome/genome.fasta") 
              FASTA HEADER (STRING-OF segment))
       (BBL::WRITE FROM (LIST '(NAME CIRCULAR) (LIST segment circular?) ) 
              TO (JOIN path "genome/genome.tbl") TABBED)))

(DEFUN Append-to (path proteins protein-names protein-gene-equiv genes-info
                    sequence2 segment circular?)

   (BBL::WRITE FROM proteins
          TO (JOIN path "proteins/proteins.fasta")
          HEADER protein-names FASTA APPEND)
   (BBL::WRITE FROM (JOIN '(()) protein-gene-equiv AS-LIST)
          TO (JOIN path "proteins/proteins.tbl") TABBED APPEND)
   (BBL::WRITE FROM (JOIN '(()) genes-info)
          TO (JOIN path "genes/genes.tbl") TABBED APPEND)
   (BBL::WRITE FROM sequence2 TO (JOIN path "genome/genome.fasta") 
          FASTA HEADER (STRING-OF segment) APPEND)
   (BBL::WRITE FROM (LIST '() (LIST segment circular?)) 
          TO (JOIN path "genome/genome.tbl") TABBED APPEND))


(DEFUN Make-new-organism-documentation 
   (full-name &KEY version date completed sequencer reference links
       alternative-names organism-prefix phylogeny nicknames habitat
       no-genes-or-proteins? no-proteins? description locus molec-type
       topology host class note no-proteins-or-genes genome-char-predicate
       protein-char-predicate base-directory no-genes relaxed oblivious)
   "Creates organism plist file"
  (LET* ((organism-short "")
         (path (JOIN base-directory full-name "/"))
		 (filename (JOIN path "plist.lisp"))
		 (data NIL)
		 )
    (SETF organism-prefix
      (COND
         ((NOT (OR organism-prefix locus))
             (ERROR "Must specify either organism-prefix or locus"))
         ((AND organism-prefix (EQUAL (BBL::LAST organism-prefix) "."))
             organism-prefix)
         (organism-prefix (JOIN organism-prefix "."))
         (locus (JOIN locus "."))
         (T (ERROR "INTERNAL ERROR: Impossible condition in COND"))))
    (SETF organism-short (BBL::FIRST (1- (LENGTH organism-prefix)) IN organism-prefix))
    (IF (NOT (MEMBER organism-short nicknames :TEST 'EQUALP))
        (SETF nicknames
           (JOIN organism-short nicknames AS-LIST)))
    (IF (OR no-genes no-proteins-or-genes)
        (SETF no-genes-or-proteins? T))
    (IF-TRUE relaxed 
        THEN (SETF genome-char-predicate 'bio::Allowable-genome-char-unfinished?)
             (SETF protein-char-predicate 'bio::Allowable-protein-char-unfinished?))
    (IF-TRUE oblivious 
        THEN (SETF genome-char-predicate 'bio::allowable-protein-char-all-letters?)
             (SETF protein-char-predicate 'bio::allowable-protein-char-all-letters?))
    (ASSIGN data =
      (FOR-EACH label 
           IN '(BIO::VERSION BIO::LOCUS BIO::DATE BIO::COMPLETED 
                BIO::CLASS
                BIO::SEQUENCER BIO::PHYLOGENY BIO::MOLEC-TYPE 
                BIO::TOPOLOGY BIO::HOST BIO::NOTE BIO::REFERENCE 
                BIO::LINKS BIO::ALTERNATIVE-NAMES 
                BIO::ORGANISM-PREFIX BIO::NICKNAMES BIO::HABITAT 
                BIO::NO-GENES-OR-PROTEINS? BIO::NO-PROTEINS? 
                BIO::GENOME-CHAR-PREDICATE BIO::PROTEIN-CHAR-PREDICATE
                BIO::DESCRIPTION)
       FOR-EACH value 
           IN (LIST version locus date completed class sequencer phylogeny
               molec-type topology host note reference links 
               alternative-names organism-prefix nicknames habitat
               no-genes-or-proteins? no-proteins? genome-char-predicate
               protein-char-predicate description)
           WHEN value
             COLLECT (LIST label value)))

    (WITH-OPEN-FILE (plist-file filename 
                      :IF-DOES-NOT-EXIST :create 
                      :IF-EXISTS :supersede :DIRECTION :output)
      (LET ((*package* (FIND-PACKAGE :bio)))
        (FORMAT plist-file "~S" data)))
)) 
		
; ===================
(DEFUN Upload-private-organism-aux
      (file-name nicknames gene-prefix organism-name from-directory to-directory 
       circular completed num-len oblivious)
	  "Writes files necessary for BioBIKE to upload organism"
  (LET* 
    ((from-path (JOIN from-directory file-name))
     (file-contents
        (LET 
         ((prelim-contents
           (COND
              ((AND (LISTP file-name) (POSITIONS-OF '-> file-name))
                (APPEND-FILES (LIST-OF-FILES file-name)))
              ((LISTP file-name)
                (APPEND-FILES file-name))
              ((PROBE-FILE from-path)
	               (FILE-TO-STRING from-path :MAX 50000000))
              (T (APPEND-FILES (ENSURE-LIST file-name)))))
              )
            (IF (SEARCH "<ERROR>" prelim-contents)
                (ERR+ PROBLEM "Argument \"~A\" is neither a file in the"
                      INDENT  "specified directory nor a Genbank accession ID."
                      ADVICE  "Perhaps the file is in the shared directory"
                      INDENT  "and you forgot to specify the SHARED option?"
                      INDENT  "Or the filename is mispelled? (upper/lower case matters!)"
                      INDENT  "Or the Genbank accession ID isn't right?"
                      FORMAT-ARGS file-name))
            prelim-contents))
     (*bbl-current-case-mode* :case-insensitive)
     (masked-file-contents (HARD-STRING-REPLACEX "http://" "!@#$%^&" file-contents))
	 (pre-raw-contigs (SPLIT masked-file-contents EVERY "//"))
	 (pre-raw-contigs (IF (< (LENGTH (BBL::LAST pre-raw-contigs)) 50)
	                  (SUBSEQ pre-raw-contigs 0 (1- (LENGTH pre-raw-contigs)))
					  pre-raw-contigs))
	 (raw-contigs 
	    (BBL::SECOND IN-EACH
		   (BBL::SORT
	         (FOR-EACH contig IN pre-raw-contigs
		          COLLECT (LIST (LENGTH contig) contig))
			 DESCENDING)))
	 (total-length 0)
	 (gene-number 0)
	 (full-name)
	 (prefix (FIRST (ENSURE-LIST nicknames)))
     (segment-count
       (FOR-EACH raw-contig IN raw-contigs
	    FOR-EACH segment-number FROM 1
	        AS (headers features raw-sequence)
                = (FIRST
			        (MATCH-OF-PATTERN-aux T "(LOCUS.*)(^ *FEATURES.*) *ORIGIN.(.*)"
                        raw-contig '(NIL NIL NIL T) :CROSS-LINES T))
			AS stop-to-assess-results 
			    = (IF (OR (NOT raw-sequence) (<= (LENGTH raw-sequence) 2))
				      (ERR+ problem "The file '~A'"
					        indent "is not in proper GenBank format."
                            advice  "The most likely problems are that it is not a GenBank"
                            indent  "file or it is a GenBank file without a sequence."
							indent  "Examine the file, especially the end, after the word"
                            indent	"'ORIGIN', where the sequence should reside."
                          ; help~A
                            format-args file-name *df-name*))
            AS (locus length molec-type topology date definition version
                prepreorganism preplasmid phylogeny prime-reference)
              = (PROCESS-HEADER-INFORMATION headers)
            AS preorganism = (OR organism-name prepreorganism)
		    AS organism =
			   (LET ((temp-name))
		         (WHEN (NOT preorganism) 
		             (ERROR "Organism not named in file! Use ORGANISM-NAME option"))
				 (SETF temp-name 
				     (TRIM (TRANSLITERATE preorganism FROM " ./()[]'" TO "_-------")
                           CUTTING "-"))
	             (SETF temp-name (JOIN (SPLIT temp-name EVERY "_-") BY "-"))
                 (SETF temp-name (JOIN (SPLIT temp-name EVERY "-_") BY "-"))
                 (SETF temp-name (JOIN (SPLIT temp-name EVERY "-") BY "-"))
				 (SETF temp-name (STRING-DOWNCASE temp-name))
				 (COND
				    ((AND full-name (EQUAL full-name temp-name)) full-name)
					(full-name
					   (ERROR (JOIN "Organism names within segments don't match! "
					                "'~A' vs '~A'")
						    full-name temp-name))
					(T (SETF full-name temp-name)))
				 (UNLESS prefix
		             (SETF prefix (MAKE-NICKNAME-FOR full-name)))
				 full-name)
		    AS plasmid = 
			   (IF preplasmid 
			       (LET ((temp-name
			               (TRANSLITERATE preplasmid FROM " ./()[]'" TO "--------")))
				     (SETF temp-name (JOIN (SPLIT temp-name EVERY "-") BY "-"))
				     (STRING-DOWNCASE temp-name)))
            AS segment-name =
               (COND 
                  (plasmid plasmid)
                  ((OR (> segment-number 1)
                       (NOT (EQUAL topology "circular"))
                       (AND length (< (CONVERT length TO Number) 800000)))					   
				    (JOIN "SEG-" segment-number))
				  (T "chromosome"))
	        AS path = (JOIN to-directory full-name "/")
            AS (host note genes-info proteins protein-names protein-gene-equiv
			      new-gene-number)
              = (PROCESS-GENES-INFORMATION features length organism segment-name 
			         gene-prefix prefix gene-number num-len)
            AS sequence2 = (IF raw-sequence (SEQUENCE-OF raw-sequence DISPLAY-OFF))
			AS real-topology = (OR circular (EQUAL topology "circular"))
           (EQ stop-to-assess-results NIL)  ; To quell warning
           (SETF gene-number new-gene-number)	
	       (IDENTITY definition) ; to squelch warning
           (IF-FALSE sequence2
               THEN (DISPLAY-DATA genes-info)
                    (ERROR (JOIN "NO SEQUENCE IN " organism "!!!!")))
		   (IF (> segment-number 1)
               (APPEND-TO path proteins protein-names protein-gene-equiv 
                      genes-info sequence2 segment-name 
					  real-topology)
  	           (PROGRAM
		          (CREATE-FILES-IN path proteins protein-names protein-gene-equiv 
                    genes-info sequence2 segment-name (EQUAL topology "circular"))
                  (MAKE-NEW-ORGANISM-DOCUMENTATION full-name
                     :BASE-DIRECTORY to-directory
                     :LOCUS locus
                     :MOLEC-TYPE molec-type
                     :PHYLOGENY phylogeny
                     :TOPOLOGY real-topology
                     :DATE date
                     :VERSION version
                     :HOST host
                     :NOTE note
                     :COMPLETED completed
                     :REFERENCE prime-reference
                     :ORGANISM-PREFIX prefix
                     :NICKNAMES (ENSURE-LIST nicknames)
                     :RELAXED T
                     :OBLIVIOUS oblivious
                     :NO-PROTEINS-OR-GENES (= (LENGTH protein-names) 0))))
	       (INCF total-length (CONVERT length TO Number))
	       COUNT segment-name))
	 )
    (LIST full-name segment-count total-length)
))