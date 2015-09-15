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


(defparameter *kegg-organisms-translation-table*
  #+:allegro
  (bio::FLET ((kegg-org-hash (x)
                (utils::CREATE-HASH-TABLE
                 (LOOP FOR sublist IN x
                     ; AS entry_id = (bio::ASSOCDR :|entry_id| sublist)
                     ; AS definition = (bio::ASSOCDR :|definition| sublist)
     				   AS entry_id = 
					     (bio::ASSOCDR "entry_id" sublist :TEST 'EQUAL)
                       AS definition = 
					     (bio::ASSOCDR "definition" sublist :TEST 'EQUAL)
                       COLLECT (FLATTEN (LIST entry_id definition)))
                 :test 'equalp)))
    (HANDLER-CASE
        (LET* ((kegg-org
                (bbi::ARRAY-TO-LIST 
                 (SECOND
                  (SECOND
                   ;; soap call
                   (kegg::LIST-ORGANISMS) 
                   ))))
			   (kegg-org-stringified
                  (LOOP FOR org-info IN kegg-org
                        COLLECT
                          (LOOP FOR (label value) IN org-info
                                COLLECT (LIST (BB-STRING-OF label)
                                              (BB-STRING-OF value)))))
               )
          (WITH-OPEN-FILE
              (stream
               (cl-user::TRANSLATE-SIMPLE-LP "biol:KEGG;kegg_organisms.lisp")
               :direction :output :if-exists :supersede)
            (FORMAT stream "~S" kegg-org-stringified))
          (kegg-org-hash kegg-org-stringified))
      (ERROR 
       ()  
       (WITH-OPEN-FILE
           (stream
            (cl-user::TRANSLATE-SIMPLE-LP "biol:KEGG;kegg_organisms.lisp"))
         (LET ((kegg-org (bio::read stream)))
           (kegg-org-hash kegg-org)
           )))))
  #+:lispworks
  nil
  )

; ------------- SEQUENCE-LIST-OF -----------

(DEFUN Sequence-list-of
       (spec-list &KEY seq-type numeric keep-frames retain-list translate
	     min-length)
  "Returns ((label1 sequence1)(label2 sequence2)...)"
  ;; seq-type is 'DNA, 'Gene, 'Coding-gene, 'AA, 'Protein, or NIL
  ;;    allows translation to specified type
  ;; numeric forces labels of nameless sequences to be numbers
  ;;    otherwise "seq n"
  ;; keep-frames forces labels to be frame-names instead of strings
  ;;    when possible
  ;; retain-list forces even singlets to be returned as a list
  (LET* 
      ((singlet (NOT (OR retain-list (LISTP spec-list))))
       (processed-spec-list
        (FLATTEN
         (LOOP
          FOR item IN (ENSURE-LIST spec-list)
          COLLECT
          (TYPECASE item
            (Organism
             (SETF singlet NIL)
             (CASE seq-type
               ((OR AA PROTEIN) (PROTEINS-OF item))
               ('CODING-GENE (CODING-GENES-OF item))
               ('GENE (GENES-OF item))
               (T (REPLICONS-OF item))))
            (CONTIGUOUS-SEQUENCE
             (CASE seq-type
               ((OR AA PROTEIN) (PROTEINS-OF item))
               ('GENE (GENES-OF item))
               (T item)))
            (PROTEIN
             (CASE seq-type
               ((OR DNA GENE) (GENE-OF item))
               (T item)))
            (GENE
             (CASE seq-type
               ((OR AA PROTEIN) (PROTEIN-OF item))
               (T item)))
            ((OR STRING LABELED-SEQUENCE)
             (LET* ((seq-of-item
                     (IF (STRINGP item)
                         item
                       (LABELED-SEQUENCE-SEQUENCE item)))
                    (item-type (BB-SEQUENCE-TYPE-OF seq-of-item T)))
               (COND
                ((NULL seq-type) item)
				((= (LENGTH seq-of-item) 0) item)
                ((AND (SYMBOL= item-type 'DNA)
                      (OR (SYMBOL= seq-type 'DNA)
                          (SYMBOL= seq-type 'GENE)
                          (SYMBOL= seq-type 'CODING-GENE)))
                 item)
                ((AND (SYMBOL= item-type 'PROTEIN)
                      (OR (SYMBOL= seq-type 'AA)
                          (SYMBOL= seq-type 'PROTEIN)))
                 item)
                (translate
                   (TRANSLATE-D/RNA-TO-AA item :if-partial-codon 
                            :ignore :if-unknown-codon :warn))
                (T
                 (ERROR "Sequence '~A' must be of type ~A but isn't"
                        (NAME-OF-aux seq-of-item NIL)
                        (case seq-type
                          (AA :amino-acid)
                          (otherwise seq-type)
                          ))))))))))
       (seq-list
        (LOOP FOR item IN processed-spec-list
              FOR item-number FROM 1
              AS seq = NIL
              AS name 
              = (TYPECASE item
                  (String 
                   (SETF seq item)
                   (IF numeric item-number (S+ "Seq" item-number)))
                  (Labeled-sequence
                   (SETF seq (LABELED-SEQUENCE-SEQUENCE item))
                   (LABELED-SEQUENCE-LABEL item))
                  ((OR Contiguous-sequence Protein Gene)
                   (SETF seq (EXTRACT-SEQUENCE item))
                   (IF keep-frames item (NAME-OF item))) ; SHORT)))
                  (OTHERWISE (ERROR "INTERNAL ERROR!")))
			  WHEN (NOT (AND min-length (< (LENGTH seq) min-length)))
                COLLECT (LIST name seq))))
    (IF singlet (FIRST seq-list) seq-list)))

(DEFUN Group-permutations-aux (preceding remaining len)
   (IF-TRUE (AND len (> len 0))
       THEN (ASSIGN len = (- len 1))
            (FOR-EACH item IN remaining
                 AS new-preceding = (JOIN preceding item AS-LIST)
                 (ASSIGN remaining = (REST remaining))
                 APPEND (FORWARD-FUNCALL 'GROUP-PERMUTATIONS-aux new-preceding remaining len))
       ELSE (LIST preceding)))

(DEFUN Inside-aux 
       (target from to by length item randomize reverse command)
  (IF (EVERY 'IDENTITY (list (PROVIDED length) (PROVIDED to) (PROVIDED from)))
      (ERR+ problem 
            "You may not simultaneously specify the LENGTH, FROM, and TO keywords."
            advice  "Specify at most 2 of these KEYWORDS."
            help~A
            format-args command)
    )
  (IF (AND (PROVIDED item)
           (OR (PROVIDED length) (PROVIDED to) (PROVIDED from) ))
      (ERR+ problem
               "The ITEM keyword is not compatible while using the "
            Indent "LENGTH, FROM, or TO keywords."
            advice  "You can specify ranges within an ITEM clause, or "
            indent  "omit the ITEM clause."
            help~A
           format-args command)
              )
  (ASSIGN (from to length by) 
          = (RESOLVE-*END* target (list from to length by)))
  (SETF by (AND (NOT (= by 1)) (PROVIDED by)))

  (LET* ((result)
         (indices
           (IF (PROVIDED item)
               (IF (LISTP item) (FLATTEN item) item)))
         )
    (COND
       ((AND (PROVIDED from) (PROVIDED to)))
       ((AND (PROVIDED from) (PROVIDED length))
           (SETF to (+ from length -1)))
       ((PROVIDED from) 
           (SETF to (LENGTH target)))
       ((AND (PROVIDED to) (PROVIDED length))
           (SETF from (- to length -1)))
       ((PROVIDED to)
           (SETF from 1))
       ((PROVIDED length)
           (ERR+ problem 
                 "LENGTH requires the use of either the FROM or TO keyword."
                 advice "Specify either FROM or TO or remove LENGTH"
                 help~A
                 format-args command))
       (T (SETF from 1) (SETF to (LENGTH target)))
       )
    (IF by (SETF indices (FROM from TO to BY by)))
    (SETF result
       (IF indices
          (REF target indices)
          (PROGN
             (WHEN (< to from) 
                 (SWAP from to)
                 (SETF reverse (NOT reverse)))
             (IF (> to (LENGTH target))
                 (SETF to (LENGTH target)))
             (SUBSEQ target (1- from) to))))

    (IF reverse (SETF result (REVERSE result)))
    (IF randomize (SETF result (SHUFFLE result)))
    result
))


(DEFUN Element-of-frame-aux (target item display labeled)
  (LET* ((no-op NIL)
         (item/s
            (COND 
               ((NULL item) (SETF no-op T))
               ((UNPROVIDED item) (SLOTS-OF target))
               ((SYMBOLP item) (FRAME-FNAMED (SYMBOL-NAME item)))
               ((STRINGP item) (FRAME-FNAMED item))
               ((IS-FRAME? item) item)
               (T (ERROR (S+ "INTERNAL ERROR in ELEMENT-OF-FRAME! " 
                             "~A is not a legal type!" *newline*
                             "Please report circumstances to authorities")
                      item)))))
 
   (IF (AND display (NOT no-op))
       (LOOP FOR i IN (ENSURE-LIST item/s)
             DO (FORMAT T "~%~30A ~A" (SLOTV i #$FNAME) (SLOTV target i))))

   (COND
      (no-op NIL)
      ((LISTP item/s)
         (LOOP FOR i IN item/s
               COLLECT
               (IF labeled 
                   (LIST i (SLOTV target i))
                   (SLOTV target i))))
       (labeled (LIST item (SLOTV target item/s)))
       (T (SLOTV target item/s)))))


(DEFUN Only-if-evidence (gene-or-gene-list &KEY any-evidence any-field)
  (LET* ((result
          (LOOP FOR gene IN (ENSURE-LIST gene-or-gene-list)
                AS info-lists =
				   (IF any-field
				       (FORWARD-FUNCALL 
                          'bio::RETRIEVE-ANNOTATION-INFORMATION 
                          (REF gene #$seed-id) :MODE :CURRENT)
					   (FORWARD-FUNCALL 
                          'bio::RETRIEVE-ANNOTATION-INFORMATION 
                          (REF gene #$seed-id) :MODE :CURRENT :TYPES :ANN))
				AS evidence? =
  				   (LOOP FOR info IN info-lists
				         AS type = (TENTH info)
				         AS evidence? =
				            (IF any-evidence 
				                (AND type (NOT (EQUAL type "SSB")))
					            (EQUAL type "Lab"))
						 DO (IF evidence? (RETURN T)))
                WHEN evidence?
                   COLLECT gene))
         )
    (IF (LISTP gene-or-gene-list)
        result
      (FIRST result))
    ))	    
	   
(defun gene-described-by-aux (query entity &key in-each)
  (let ((descriptor (bb-string-of query)))
    (flet ((do-list (entity) 
             (let ((result
                    (loop for e in entity
                          as iquery =
                          (default-gene-described-by-aux e descriptor)
                          when (not (equal iquery nil))
                          collect iquery
                          )))
               (if in-each result (flatten result))
               )))
      (case user::*application-instance*
        (:bioseed 
         (if bio::*enable-descriptions-map* 
             (bioseed-gene-described-by-aux entity descriptor in-each)
           (if (listp entity)
               (do-list entity)
             (default-gene-described-by-aux entity descriptor)
             )))
        (otherwise
         (if (listp entity)
             (do-list entity)
           (default-gene-described-by-aux entity descriptor)
           ))))))

(defun default-gene-described-by-aux (entity descriptor)
  (flet ((findit (x)
           (and x (search descriptor x :test 'string-equal))))
    (LOOP FOR gene IN (ENSURE-LIST (GENE-OF entity))
          AS description = (SLOTV gene #$Description)
          AS best-hit-descr = (SLOTV gene #$Best-hit-descr)
          AS annotation = (SLOTV gene #$Annotation)
          AS sub-function = (SLOTV gene #$Sub-function)
          AS COG-description = (SLOTV gene #$COG-description)
          AS EC-description = (SLOTV gene #$EC-description)
          AS genetic-name = (SLOTV gene #$Genetic-name)
          when (or (findit description) 
                   (findit best-hit-descr)
                   (findit annotation)
                   (findit sub-function)
                   (findit cog-description)
                   (findit ec-description)
                   (findit genetic-name))
          COLLECT gene
          )))

(defun bioseed-gene-described-by-aux (entity search-string in-each)
  (cond
   ((and (eq entity bio::*loaded-organisms*) (not in-each))
    (forward-funcall 
     'bio::every-gene-described-by-in-descriptions-map search-string))
   ((and (listp entity) 
         (every (lambda (x) (typep x 'bio::seed-organism)) entity))
    (let ((results 
           (loop for e in entity 
                 collect
                 (forward-funcall 
                  'bio::every-gene-described-by-in-organism search-string e)
                 )))
      (if in-each results (flatten results))
      ))
   ((not (listp entity))
    (let ((genes (genes-of entity)))
      (forward-funcall 
       'bio::every-gene-described-by-in-genes search-string genes)
      ))
   (t 
    (let ((results 
           (loop 
            for e in entity 
            collect 
            (forward-funcall 
             'bio::every-gene-described-by-in-genes 
             search-string (ensure-list (genes-of e)))
            )))
      (if in-each results (flatten results))
      ))))

(DEFUN Random-integer-number-aux (integer? from to in)
  "Serves RANDOME-INTEGER and RANDOM-NUMBER"
  (ERROR-IF-MORE-THAN-ONE in from)
  (ERROR-IF-MORE-THAN-ONE in to)
  (COND
   (in (BB-CHOOSE-FROM in))
   (integer?
      (IF (UNPROVIDED from) 
          (SETF from 1)
        (SETF from (CEILING (- from SINGLE-FLOAT-EPSILON))))
      (IF (UNPROVIDED to) 
          (SETF to 100)
        (SETF to (FLOOR (+ to SINGLE-FLOAT-EPSILON))))
      (UNLESS (<= from to)
          (ERROR "FROM value cannot exceed TO value"))
      (LET ((range (- to from -1)))
        (+ from (RANDOM range))))
   (T (IF (UNPROVIDED from) (SETF from 0))
      (IF (UNPROVIDED to) (SETF to 1))
      (UNLESS (<= from to)
        (ERROR "FROM value cannot exceed TO value"))
      (LET ((range (FLOAT (- to from))))
        (+ from (RANDOM range))))))

(DEFUN NCBI-URL-TO-STRING (link)
  "Transforms sequence file in NCBI format to string, without html"
  (LET* ((raw-html (bio::web-page-contents link))
         (scanner1 (CL-PPCRE:CREATE-SCANNER "<html>.*<pre class=\"genbank\">(.*)" 
                      :MULTI-LINE-MODE T
                      :SINGLE-LINE-MODE T))
         (processed-html (CL-PPCRE:SCAN-TO-STRINGS scanner1 raw-html))
         )
     
     (IF processed-html
         (JOIN (FORWARD-FUNCALL 'MATCH-OF-PATTERN-ONE-ITEM-aux
                 "([^<>]*)<.*?>" processed-html *unprovided+*
                 '(NIL NIL NIL T) NIL NIL NIL T))
		 raw-html)))

(DEFUN GenBank-sequence (accession)
  (LET* ((base-url 
;          "http://www.ncbi.nlm.nih.gov/entrez/viewer.fcgi?db=nucleotide&list_uids=")
;          "http://www.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nucleotide&list_uids=") 
		   "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nuccore&id=")
         (retrieval-type "&rettype=fasta")
         (url (JOIN base-url accession retrieval-type))
         (raw-sequence (SPLIT (bio::web-page-contents url) EVERY *newline*))
         (label (POP raw-sequence))
         (sequence (JOIN raw-sequence))
        )
   ; Currently ignoring label
    (SETF label label)  ; to avoid a warning
    sequence))

(defun kegg-organisms-of-aux (item)
  (LET* ((kegg-nickname (FIRST (BB-SPLIT item :AT ":"))))
    (GETHASH kegg-nickname *kegg-organisms-translation-table*)
    ))


(DEFUN Kegg-sequence (kegg-id dna aa short-label org-label long-label )
  (LET* ((dna-seq (FORMAT NIL "-f -n n ~a" KEGG-ID))
         (aa-seq (FORMAT NIL "-f -n a ~a" KEGG-ID))
         (kegg-seq 
          (COND 
           (DNA 
            ;; soap call
            (kegg::BGET :string dna-seq)
            )
           (AA 
            ;; soap call
            (kegg::bget :string aa-seq)
            )
           (T (FORMAT NIL "~a" "Please indicate DNA or AA" ))))
         (kegg-seq1 (SECOND (SECOND kegg-seq)))
         (splitted-kegg-seq (BB-SPLIT kegg-seq1 :AT *newline*))
         (label (FIRST (BB-SPLIT (FIRST (BB-SPLIT (FIRST splitted-kegg-seq) :AT " " )) :AT ">")))
         (llabel (FIRST (BB-SPLIT (FIRST (BB-SPLIT (FIRST splitted-kegg-seq) :AT " (A)" )) :AT ">")))
         
         (seq0 (BB-JOIN (BB-SPLIT (SECOND splitted-kegg-seq) :EVERY *newline*)))
         (seq (STRING-UPCASE seq0))
         (org (BB-JOIN (BB-SPLIT (KEGG-ORGANISMS-OF-aux label) :EVERY " ") :BY "_"))
         (org-g (FORMAT NIL "~a~a~a" org ".." kegg-id))
         
         (labeled-seq (COND (org-label (LABELED-SEQUENCE-FROM seq org-g))
                            (long-label (LABELED-SEQUENCE-FROM seq llabel))
                            (short-label (LABELED-SEQUENCE-FROM seq label))
                            (T  seq )))
         )
    labeled-seq))

   
(DEFUN Find-start-end (entity from from-end to to-end length)
  (LET* ((full-length (FORWARD-FUNCALL 'DF-FUNCTION-FOR-LENGTH-OF entity NIL))
         (start 
            (COND
               ((PROVIDED from) from)
               ((PROVIDED from-end) 
                   (IF (< from-end 0)
                       (+ full-length from-end 1)
                       (+ full-length from-end)))
               (T (MIN full-length 1))))
         (end 
            (COND
               ((PROVIDED to) to)
               ((PROVIDED to-end) 
                   (IF (< to-end 0)
                       (+ full-length to-end 1)
                       (+ full-length to-end)))
               (T full-length)))
         )
    (COND
       ((AND (PROVIDED length) (OR (PROVIDED to) (PROVIDED to-end)))
            (SETF start (- end length -1)))
       ((PROVIDED length) 
            (SETF end (+ start length -1))))
    (LIST start end full-length)))

(DEFUN Sequence-of-display-aux (entity sequence from from-end to to-end length line-length segment-length
         no-coordinates unsegmented fasta to-file &KEY double-stranded first-time)
   "Formats a nucleotide or amino acid sequence (or any text)"

   (LET* ((name (NAME-OF entity SHORT))
          (seq (IF (TYPEP sequence 'Labeled-sequence)
                   (LABELED-SEQUENCE-Sequence sequence)
                   sequence))
    ;     (start-end (FIND-START-END entity from from-end to to-end length))
          (start-end (FIND-START-END sequence from from-end to to-end length))
          (start (FIRST start-end))
          (end (SECOND start-end))
          (length-of-entity (THIRD start-end))
          (from-to-label 
             (IF (AND (= start 1) (= end length-of-entity)) 
                 (BB-JOIN (LIST " (size: " length-of-entity ")"))
                 (BB-JOIN (LIST " from " start " to " end))))
          (label (BB-JOIN (LIST name from-to-label)))
          (segment-length 
             (IF-TRUE unsegmented
                 THEN line-length
                 ELSE segment-length))
          (sequence-packet (LIST (LIST name seq) ))
          )
      (IF (PROVIDED to-file)
          (PROGN
             (IF first-time
                (FORWARD-FUNCALL 'bbl::DF-FUNCTION-FOR-WRITE "" to-file 
                   NIL NIL NIL NIL NIL T NIL))
             (FORWARD-FUNCALL 'bbl::DF-FUNCTION-FOR-WRITE seq to-file NIL NIL fasta NIL NIL 
                   (NOT fasta) T :HEADER label))
          (PROGN
            (IF (NOT fasta)
                (FORWARD-FUNCALL 'DISPLAY-LINE "Sequence-of " label *newline*))
            (FORWARD-FUNCALL 'DISPLAY-SEQUENCE-OF-aux sequence-packet
                 line-length segment-length no-coordinates start end fasta :LABEL label
                 :DOUBLE-STRANDED double-stranded)
            (DISPLAY-LINE *newline* *newline*)))
))

(DEFUN Sequence-of-viewer-aux (entity from from-end to to-end length)
  (LET* ((length-of-entity (FORWARD-FUNCALL 'DF-FUNCTION-FOR-LENGTH-OF entity NIL))
         (from 
            (COND
              ((PROVIDED from) from)
              ((PROVIDED from-end) (+ length-of-entity from-end))
              (T NIL)))
         (to
            (COND
              ((PROVIDED to) to)
              ((PROVIDED to-end) (+ length-of-entity to-end))
              ((AND from length) (+ from length -1))
              (T NIL)))
         )
    (IF (AND to length (NOT from))
        (SETF from (- to length -1)))

  (TYPECASE entity
     (Organism (FORWARD-FUNCALL 'SEQUENCE-VIEWER-aux :ORGANISM entity :FROM from :TO to))
     (Contiguous-sequence (FORWARD-FUNCALL 'SEQUENCE-VIEWER-aux :CONTIG entity :FROM from :TO to))
     (Gene 
        (LET* ((replicon (CONTIG-OF entity))
               (len (REF replicon #$sequence-length))
               (offset (REF entity #$from))
               (from (IF from (MOD (+ offset from -1) len)
                              (MOD (- offset 120) len)))   
               (to (IF to (MOD (+ to from -1) len)
                          (MOD (+ offset length-of-entity 120 -1) len)))
               )   
          (FORWARD-FUNCALL 'SEQUENCE-VIEWER-aux :CONTIG replicon :FROM from :TO to))))
     )
)
      

(DEFUN Sequence-of-map-aux (entity)
   (FORWARD-FUNCALL 'MAKE-CIRCULAR-MAP-aux entity))


(DEFUN Sequence-of-aux
       (entity dna protein from from-end to to-end length invert 
               truncate wrap no-stops &KEY labeled sequence-ok
               with-label from-genbank from-kegg)

  (IF (= from 0) (SETF from 1))
  (IF (= from-end 0) (SETF from-end 1))
  (IF (= to 0) (SETF to -1))
  (IF (= to-end 0) (SETF to-end -1))

  (LET* ((entity (IF (AND from-genbank (SYMBOLP entity))
                     (BB-STRING-OF entity)
                     entity))
         (string (IF (STRINGP entity) entity))
         (start-end (IF (NOT string) 
                    (FIND-START-END entity from from-end to to-end length)))
         (start (FIRST start-end))
         (end (SECOND start-end))
         (from-end? (PROVIDED from-end))
         (to-end? (OR (PROVIDED to-end)
                      (AND (PROVIDED from-end) (PROVIDED length))))
         (label 
            (COND
               ((PROVIDED with-label) with-label)
               ((AND labeled invert)
                  (JOIN (NAME-OF entity SHORT) "-R"))
               ((AND labeled from-kegg) entity)
               ((TYPEP entity 'DOMAIN) (FORMAT NIL "~a~a~a" (DOMAIN-GENE entity) ".." (DOMAIN-START entity)))
               (labeled (NAME-OF entity SHORT))
               (T NIL)))
         (result NIL))

    (SETF result
          (TYPECASE entity
            (String
             (COND (from-genbank
                    (OR (SETF string (GENBANK-SEQUENCE string))
                        (ERR+ PROBLEM "~A is not a GenBank accession ID."
                              ADVICE  "Check the spelling."
                              FORMAT-ARGS entity)))
                   (from-kegg
                    (IF dna
                        (SETF string (KEGG-SEQUENCE string  T NIL NIL NIL NIL))
                      (SETF string (KEGG-SEQUENCE string  NIL T NIL NIL NIL)))
                    ))
             (SETF 
              start-end (FIND-START-END string from from-end to to-end length))
             (SETF start (FIRST start-end))
             (SETF end (SECOND start-end))
             (STRING-SEQUENCE-OF 
              string start end invert :DNA dna :PROTEIN protein
              :TRUNCATE truncate :WRAP wrap :SEQUENCE-OK sequence-ok))
            (metagenome-read 
             (let ((read (#^sequence entity)))
               (SETF 
                start-end 
                (find-start-end read from from-end to to-end length))
               (setq start (first start-end))
               (setq end (second start-end))
               (string-sequence-of 
                read start end invert :dna nil :protein nil 
                :truncate nil :wrap nil :sequence-ok t
                )))
            (Domain
             (SETF
              string (KEGG-SEQUENCE (domain-gene entity)  NIL T NIL NIL NIL))
             (STRING-SEQUENCE-OF 
              string (domain-start entity) (domain-stop entity) invert
              :DNA dna :PROTEIN protein
              :FROM-END? from-end? :TO-END? to-end? :TRUNCATE truncate
              :WRAP wrap :SEQUENCE-OK sequence-ok))

            (Gene 
             (IF wrap
                 (ERR+ problem "The WRAP flag argument is not applicable to a gene,"
                       indent "such as ~A!"
                       advice 
                       "You may not use this flag or use it on arguments of type:"
                       indent "Chromosome or Plasmids" 
                       help~A
                       format-args
                       entity "SEQUENCE-OF")
               ) 
             (GENE-SEQUENCE-OF entity start end invert :TRUNCATE truncate))
            (Protein
             (IF wrap
                 (ERR+ problem "The WRAP flag is not applicable to a protein,"
                       indent "such as ~A!"
                       advice
                       "You may not use this FLAG or use it on arguments of type:"
                       indent "Chromosome or Plasmid." 
                       help~A
                       format-args
                       entity "SEQUENCE-OF")
               ) 
             (PROTEIN-SEQUENCE-OF 
              entity start end :TRUNCATE truncate :NO-STOPS no-stops))
            (Contiguous-sequence 
             (CONTIG-SEQUENCE-OF
              entity start end invert 
              :TRUNCATE truncate :WRAP wrap))
            (Labeled-sequence
             (STRING-SEQUENCE-OF 
              (LABELED-SEQUENCE-SEQUENCE entity) 
              start end invert :DNA dna :PROTEIN protein
              :TRUNCATE truncate :WRAP wrap))))
    (IF-TRUE label 
        THEN (MAKE-LABELED-SEQUENCE :SEQUENCE result :LABEL label)
        ELSE result)))

(DEFUN Arithmetic (function entity1 entity2)
  (FLET ((bad-list (label value)
           (IF (NOT (IS-SIMPLE-LIST? value))
                (ERR+ problem "Arithmetic operations can act only on simple lists."
                      indent "This is not the case for the ~A entity '~A'.~&"
                      format-args
                      label value)
             (ERR+ problem "Arithmetic operations can act only on lists that"
                   indent "consist only of numbers."
                   indent "This is not the case for the ~A entity '~A'.~&"
                   format-args
                   label value)
             )))
    (COND
     ((TYPEP entity1 'Number)
      (COND
       ((TYPEP entity2 'Number)
        (FUNCALL function entity1 entity2))
       ((AND (LISTP entity2) (IS-SIMPLE-LIST? entity2) 
             (ALL-TRUE (MAPCAR 'NUMBERP entity2)))
        (MAPCAR (LAMBDA (x) (FUNCALL function entity1 x)) entity2))
       ((LISTP entity2) (BAD-LIST "second" entity2))
       ((Is-TABLE? entity2)
        (LET ((result (GARRAY-COPY entity2)))
          (GMAPSET (LAMBDA (x) (FUNCALL function entity1 x)) result))))) 

     ((AND (TYPEP entity1 'List)(IS-SIMPLE-LIST? entity1) 
           (ALL-TRUE (MAPCAR 'NUMBERP entity1)))
      (COND
       ((TYPEP entity2 'Number)
        (MAPCAR (LAMBDA (x) (FUNCALL function x entity2)) entity1))
       ((AND (LISTP entity2) (IS-SIMPLE-LIST? entity2) 
             (ALL-TRUE (MAPCAR 'NUMBERP entity2)))
        (IF (NOT (= (LENGTH entity1) (LENGTH entity2)))
            (ERR+ problem "Lists ~A (~A elements) and ~A (~A elements) "
                  indent  "must be the same length.~&"
                  format-args
                  entity1 (LENGTH entity1) entity2 (LENGTH entity2))
          )
        (LOOP FOR number1 IN entity1
              FOR number2 IN entity2
              COLLECT (FUNCALL function number1 number2)))
       ((LISTP entity2) (BAD-LIST "second" entity2))
       ((Is-TABLE? entity2)
        (ERR+ problem "Don't know how to perform this operation on a list '~A' and a table '~A'.~&"
              format-args
         entity1 entity2)
        )))
     ((LISTP entity1) (BAD-LIST "first" entity1))
     ((TYPEP entity1 'Table)
      (COND
       ((TYPEP entity2 'Number)
        (LET ((result (GARRAY-COPY entity1)))
          (GMAPSET (LAMBDA (x) (FUNCALL function x entity2)) result))) 
       ((OR (LISTP entity2) (IS-TABLE entity2))
        (ERR+ problem "Don't know how to perform this operation on a table '~A' and a list or another table '~A'.~&"
              format-args
              entity1 entity2)
        ))))))



;; If return-position is T, and the list is even, 
;; the position of the lower of the two bounding values is 
(declaim (ftype function median-aux))

(defun median-aux (list)
  (if (every 'numberp list)
      (let ((len (length list)))
        (cond 
         ((zerop len) (error "Cannot take median of null list!"))
         ((= len 1) (first list))
         (t 
          (let ((sorted-list (sort (copy-list list) '<)))
            (if (oddp len) 
                (let ((pos (ceiling len 2)))
                  (nth (1- pos) sorted-list))
              (let ((pos (/ len 2)))
                (let* ((p (nthcdr (1- pos) sorted-list))
                       (v1 (first p))
                       (v2 (second p))
                       (sum (+ v1 v2)))
                  (if (AND (INTEGERP sum) (evenp sum)) (/ sum 2) (/ sum 2.0))
                  )))))))
    (loop for elem in list 
          when (not (listp elem))
          do 
          (error "Median list subcomponent, ~S, is not a list!" elem)
          collect (median-aux elem)
          )))


(DEFUN Calc-sums (list &KEY var)
  (XLOOP INITIALIZE x-sum = 0
         INITIALIZE x2-sum = 0
         INITIALIZE N = (LENGTH list)
         FOR x IN list
         AS x2 = (* x x)
         DO 
         ;; make without-code-walker work
         (setf x-sum (+ x-sum x))
         ;; (INCREMENT x-sum by x)
         (setf x2-sum (+ x2-sum x2))
         ;; (INCREMENT x2-sum by x2)
         FINALLY 
         (IF (NOT var)
             (RETURN (list N x-sum x2-sum))
           (RETURN (list N x-sum x2-sum
                         (/ (- (* N x2-sum) (* x-sum x-sum))
                            (* N (- N 1)))) ))))

(defun chromosome-of-aux (entity nowarnings)
  (typecase entity
    (null nil)
    (gene
     (let* ((contig (slotv entity #$contiguous-sequence))
            (org (#^organism entity))
            (completed? (#^completed org)))
       (if (and completed? 
                (contig-is-chromosome? cl-user::*organisms-descriptor* contig))
           contig 
         (unless nowarnings 
           (if completed? 
               (warn (s+ "~&The gene ~s, on contig ~S,"
                         "~&does not belong to the chromosome of the organism"
                         "~&~A"
                         "~&NIL is being returned."
                         "~& Try (contig-of ~a) to get the gene's contig." 
                         "~&To avoid this warning, use NOWARNINGS.")
                     entity contig org entity)
             (warn (S+ "~&The chromosome of ~S"  
                       "~&is not yet defined in BioBIKE!" 
                       "~&(~A is not a completely sequenced organism yet)"
                       "~&NIL is being returned."
                       "~&Try (contig-of ~A) to get the gene's contig."
                       "~%To avoid this warning, use NOWARNINGS.")
                   org org entity))
           nil
           )))) 
    (contiguous-sequence
     (let* ((org (#^organism entity))
            (completed? (#^completed org)))
       (if (and completed? 
                (contig-is-chromosome? cl-user::*organisms-descriptor* entity))
           entity
         (progn
           (unless nowarnings
             (warn 
              (s+ 
               "~S is a plasmid or a contig!  It is not the chromosome of ~A"
               "~%(or ~A is not a completely sequenced organism yet)."
               "~%To avoid this warning use NOWARNINGS.")
              entity org org
              ))
           nil
           ))))
    (organism
     (if (slotv entity #$completed)
         ;; return contig with the longest sequence as per JE phone conversation
         ;; 1/12/10 -- JP
      ;  (chromosome-of-organism cl-user::*organisms-descriptor* entity)
         (chromosome-of-organism T entity)
       (progn
         (unless nowarnings
           (WARN (S+ "~&The chromosome of ~S" 
                     "~&is not yet defined in BioBIKE! NIL is returned."
                     "~&You may try: (CONTIG-OF ~A)." 
                     "~&To avoid this warning, use NOWARNINGS.")
                 entity (official-prefix entity)))
         nil
         )))))

(defmethod contig-is-chromosome? ((od t) contig)
  (eq contig (chromosome-of-organism od (#^organism contig))))

(defmethod chromosome-of-organism ((od (eql :seed-organisms)) org)
  (if (TYPEP org 'biolisp::seed-organism) 
      (REF org #$chromosome)
	  (ERROR "CHROMOSOME-OF-ORGANISM given non-organism, '~A'"
	         org)))

(defmethod chromosome-of-organism ((od t) org)
  (first 
   (sort  
    (copy-list (#^contiguous-sequences org))
    '> 
    :key (lambda (x) (#^sequence-length x))
    )))


(defun hydrophobicity-of-aux (entity nowarnings sequence amino-acid)
  (IF-TRUE
   (AND (EQUAL (AMINO-ACID-DESIGNATOR? entity) T) (NOT sequence))
   THEN 
   (IF-TRUE 
    amino-acid
    THEN 
    (GETHASH (AA-TO-1-LETTER-CODE entity)  *hydrophobicity-hash*)
    ELSE  
    (WARN
     (S+
      "~&The provided string corresponded to the Amino-acid designation of ~S. " 
      "~&The hydrophobicity of that amino acid was returned. "
      "~&To obtain the hydrophobicity of each character use the "
      "flag SEQUENCE."
      "~&If you do not want this warning use the flag AMINO-ACID.") 
     (AA-TO-LONG-NAME entity))
    (GETHASH (AA-TO-1-LETTER-CODE entity) *hydrophobicity-hash*))      
   ELSE                     
   (LET* ((raw-sequence
           (TYPECASE entity            
             (Protein (EXTRACT-SEQUENCE entity))
             (Gene 
              (IF-TRUE 
               (SLOTV entity #$ENCODES-PROTEIN)
               THEN (EXTRACT-SEQUENCE (FIRST (SLOTV entity #$PROTEINS)))
               ELSE 
               (ERR+ problem "The given gene ~S does not encode a protein!"
                     indent  
                     "You cannot obtain the hydrophobicity of amino-acids"
                     indent  "in a protein that does not exist.~&"
                     format-args entity)
               ))
                                       
             (STRING entity)))

          (splitted-sequence
           (SPLIT (STRING-UPCASE raw-sequence))))
             
     (LOOP FOR aa IN splitted-sequence
       AS hydrophobicity = 
       (IF-TRUE (AMINO-ACID-DESIGNATOR? aa)
                THEN  (GETHASH aa *hydrophobicity-hash*)
                ELSE 
                (IF-TRUE 
                 nowarnings
                 THEN  NIL
                 ELSE 
                 (WARN 
                  (S+ 
                   "~&You are trying to obtain the hydrophobicity of the"
                   "~&non-amino-acid character: ~S. The function returns NIL."
                   "~&If you do not want this warning use the flag NOWARNINGS."
                   ) aa)
                 NIL))
       COLLECT hydrophobicity)
     )))


(defun insert-spaces-between (s &key (how-many? 1))
  (declare (simple-string s) (fixnum how-many?))
  (let* ((len (length s))
         (rlen (- (* len (1+ how-many?)) how-many?))
         (rstring (make-string rlen :initial-element #\Space)))
    (declare (fixnum len rlen) (simple-string rstring))
    (loop 
      for j fixnum from 0 below len 
      for k fixnum from 0 by (1+ how-many?)
      do
      (setf (aref rstring k) (aref s j))
      finally (return rstring)
      )))

(defun reading-frames-of-aux
       (entity line-length segment-length do-not-display nowarnings)
  (LET* ((sequence
          (COND
           ((STRINGP entity) entity)
           ((SYMBOLP entity) (SYMBOL-NAME entity))
           ((OR (TYPEP entity 'GENE) (TYPEP entity 'Contiguous-sequence))
            (EXTRACT-SEQUENCE entity))
           ((TYPEP entity 'PROTEIN)
            (EXTRACT-SEQUENCE (SLOTV entity #$Gene)))
           ((TYPEP entity 'Labeled-sequence)
            (LABELED-SEQUENCE-SEQUENCE entity)) 
           (T NIL)))
         (length (LENGTH sequence))
         (inverse-sequence (INVERSION-OF sequence)) 
         (complement-sequence (REVERSE inverse-sequence)) 
         (sequence-list 
          (LIST sequence 
                (SUBSEQ sequence 1 length)                  
                (SUBSEQ sequence 2 length)                 
                inverse-sequence 
                (SUBSEQ inverse-sequence 1 length)          
                (SUBSEQ inverse-sequence 2 length)))        
         (reading-frames 
          (IF-TRUE
           nowarnings
           THEN (MAPCAR 
                 (LAMBDA (x) 
                   (TRANSLATE-D/RNA-TO-AA 
                    x :if-partial-codon :ignore :if-unknown-codon :ignore))
                 sequence-list)
           ELSE (MAPCAR 
                 (LAMBDA (x) 
                   (TRANSLATE-D/RNA-TO-AA
                    x :if-partial-codon :ignore :if-unknown-codon :warn))
                 sequence-list)))
         (spaced-reading-frames 
          ;; probably a lot faster -- JP 
          (mapcar 
           (lambda (s) (insert-spaces-between s :how-many? 2))
           reading-frames)
          )
         (prespace 
          (LIST
           "" " " "  " 
           (MAKE-STRING (+ (MOD    length    3) 2) :initial-element #\space)
           (MAKE-STRING (+ (MOD (+ length 2) 3) 2) :initial-element #\space)
           (MAKE-STRING (+ (MOD (+ length 1) 3) 2) :initial-element #\space))))
    (IF-FALSE 
     do-not-display
     THEN                                                            
     (FORWARD-FUNCALL 
      'DISPLAY-SEQUENCE
      (LIST (LIST "Sequence" sequence)
            (LIST "Translation-Frame-1" (FIRST spaced-reading-frames))
            (LIST "Translation-Frame-2" 
                  (CONCATENATE 'STRING " " (SECOND spaced-reading-frames)))
            (LIST "Translation-Frame-3" 
                  (CONCATENATE 'STRING "  " (THIRD spaced-reading-frames)))
            (LIST "Complement" complement-sequence)
            (LIST "Translation-Frame-4" 
                  (CONCATENATE 'STRING (FOURTH prespace) 
                               (REVERSE (FOURTH spaced-reading-frames))))
            (LIST "Translation-Frame-5" 
                  (CONCATENATE 'STRING (FIFTH prespace)
                               (REVERSE (FIFTH spaced-reading-frames))))
            (LIST "Translation-Frame-6" 
                  (CONCATENATE 'STRING (SIXTH prespace)
                               (REVERSE (SIXTH spaced-reading-frames)))))
      :LINE-LENGTH line-length :SEGMENT-LENGTH segment-length)
     ELSE reading-frames)))

(defun bin-data-of-aux 
       (list min max interval)
  (let ((number-of-bins nil)
        (residue nil)
        (bins nil))
    (MAPCAR
       (LAMBDA (x) 
          (IF (NOT (NUMBERP x))
              (ERROR "List must consist solely of numbers")))
       list)
     (ASSIGN (number-of-bins residue)
         = (CEILING (/ (- max min) interval 1.0)))
    (IF (= residue 0)
        (INCF number-of-bins))
    (SETF bins 
      (MAKE-ARRAY number-of-bins :INITIAL-ELEMENT 0))
           
    (FLET ((BIN-OF (number)
             (FLOOR (/ (- number min) interval 1.0))))

      (LOOP FOR number in list
            AS bin = (BIN-OF number)
            DO (IF (AND (>= number min) (<= number max))
                   (INCF (AREF bins bin))))
    
      (LOOP FOR bin FROM 0 TO (- number-of-bins 1)
            WITH n = (+ min (/ interval 2.0))
            AS line = (LIST n (AREF bins bin))
            DO (INCF n interval)
            COLLECT line))))


(DEFUN Table-to-list-aux (table &KEY with-labels)
  (IF (NOT (TYPEP table 'Table))
      (ERROR "Attempt to convert to a list something that should be a table but is instead of type ~A"
             (TYPE-OF table)))
  (LET* ((labels (LABELS-OF table))
         (dimensions (LENGTH (REMOVE-NILS labels)))
         (1st-dim-labels
            (FOR-EACH label IN (FIRST labels)
                 (UNLESS
                    (OR (NUMBERP label)
                        (AND (STRINGP label) (NUMBERP (CONVERT-TO-NUMBER-MAYBE label))))
                    (RETURN (FROM 1 TO (LENGTH (FIRST labels)) LIMIT *big-number*)))
                 COLLECT (CONVERT-TO-NUMBER-MAYBE label)))
        )
    (COND
       ((= dimensions 0) NIL)
       ((= dimensions 1)
           (FOR-EACH label IN (FIRST labels)
            FOR-EACH n1 IN 1st-dim-labels
                AS n2 = (REF table label)
                AS item-list = (IF with-labels (LIST label n2) (LIST n1 n2))
                COLLECT item-list))
       ((AND (= dimensions 2) with-labels)
           (FOR-EACH label1 IN (FIRST labels)
                APPEND
                   (FOR-EACH label2 IN (GARRAY-COMPONENT-INDICES table label1)
                        AS n2 = (REF table label1 label2)
                        COLLECT (LIST label1 label2 n2))))
       ((= dimensions 2)
           (FOR-EACH label2 IN (SECOND labels)
                COLLECT
                   (FOR-EACH label1 IN (FIRST labels)
                    FOR-EACH n1 IN 1st-dim-labels
                        AS n2 = (REF table label1 label2)
                        COLLECT (LIST n1 n2))))
       (T (ERROR "Can't at present convert tables with dimensions greater than 2")))))

(DEFUN Organism-in-group-aux 
     (name bacteria-only phage-only per-phage-host +taxonomy 
            sort-by-group display-off)
  (DECLARE (SPECIAL bio::*DEFAULT-MYSQL-DATABASE-NAME*))
  (LET* ((database bio::*DEFAULT-MYSQL-DATABASE-NAME*)
         (results NIL)
         (domain 
          (COND
           ;; *all-phage* is only defined in seed instances and would
           ;; cause a compiler warning if used directly.  
           ;; furthermore, it is a symbol-macro, so we can't just use 
           ;; (symbol-value '*all-phage*)
           ((OR phage-only per-phage-host)
            (COND 
             ((EQUAL "seed" database) 
              (eval '*all-phage*))
             ((EQUAL "viruses" database) 
              (eval '*prokaryotic-viruses*))
             ((EQUAL "cyanobacteria" database) NIL)
             (T (ERROR "SYSTEM ERROR! Unknown instance ~A"
                       database))))
           (bacteria-only
            (COND
             ((EQUAL "seed" database) 
              (eval '*all-bacteria*))
             ((EQUAL "viruses" database) NIL)
             ((EQUAL "cyanobacteria" database) 
              (eval '*all-cyanobacteria*))
             (T (ERROR "SYSTEM ERROR! Unknown instance ~A"
                       database))))
           (T *all-organisms*))))
    (IF (EQUAL database "seed")
        (LOOP FOR organism IN domain
              AS target =
              (IF per-phage-host 
                  (STRING-JOIN (#^Host-phylogeny organism) "/")
                (#^Taxonomy organism))
              DO
              (WHEN (SEARCH name target :TEST 'STRING-EQUAL)
                (IF +taxonomy
				    (PUSH (LIST organism target) results)
                    (PUSH organism results))))
      (LOOP FOR organism IN domain
            DO (IF (OR (SAME (#^Group organism) name)
			           (SEARCH name (#^FName organism) :TEST 'STRING-EQUAL))
                   (PUSH organism results))))
    (COND
     ((NULL results) NIL)
     ((NULL (CDR results)) (FIRST results))
     (T (LET ((sorted-results
               (IF (AND (EQUAL database "seed") sort-by-group +taxonomy)
                   (BBL::SORT results BY-POSITION 2 
                              THEN-SORT-ASCENDING-BY 1)
                 (SORT results 'BB-LESS-THAN))))
          (IF (AND (BBL-TOPLEVEL?) (NOT display-off))
              (LET ((name-list
                     (LOOP FOR info IN sorted-results
                           AS org = (IF +taxonomy (FIRST info) info)
                           AS prefix = (#^Organism-prefix org)
                           AS nickname = (SUBSEQ prefix 0 (1- (LENGTH prefix)))
                           COLLECT 
                           (APPEND (LIST nickname) (ENSURE-LIST info)))))
                (FORWARD-FUNCALL 'DISPLAY-LIST-aux 'EACH name-list
                                 NIL 3 NIL NIL T NIL)))
          sorted-results))) 
    ))
	
; ================ MYSQL FUNCTIONS ===============
(DEFUN Stringify-for-mysql (list &KEY with-quotes)
 ;; Converts list into a mysql list, with commas separating items
 ;; Strips quotes from items, optionally putting single quotes around strings
 ;; Example with default:
 ;;   '("A" "B" "C" 1) --> "(A,B,C,1)
 ;; Example with WITH-QUOTES = T
 ;;   '("A" "B" "C" 1) --> "('A','B','C',1)

  (LET ((new-list
          (FOR-EACH item IN list
               AS new-item =
	              (IF (AND (STRINGP item) with-quotes)
		              (JOIN "'" item "'")
			          item)
               COLLECT new-item)))
    (JOIN "(" (JOIN new-list BY ",") ")")
 ))

(DEFUN Replace-item-in-mysql-table 
   (table key-label key-value target-label value 
     &KEY replace-all verbose no-change)
 ;; REPLACE-ITEM-IN-MYSQL-TABLE syntax
 ;;   table: name of mysql table, in quotes
 ;;   key-label: label of column used to identify item(s) to change
 ;;   key-value: value that column must have to identify item(s) to change
 ;;   target-label: label of column to be changed
 ;;   value: value to replace old value in target-label
 ;; By default, function will error out if more than one match is found
 ;; Options:
 ;;   replace-all: If T, then all matches will be altered, even if multiple found
 ;;   verbose: item is printed, before and after change
 ;;   no-change: search is performed but database is not changed

  (LET* ((key-value-string
            (IF (STRINGP key-value)
			    (JOIN "'" key-value "'")
				key-value))
         (labels) (label-index) (rows))

    (ASSIGN (rows labels) = 
    ( ; BBI-SEED-QUERY 
     genericdb::xsql 
          (JOIN "select * from " table " where " 
		        key-label " = " key-value-string)))
    (SETF label-index (POSITION target-label labels :TEST 'EQUAL))
    (IF (NOT label-index)
        (ERROR "TARGET-LABEL '~A' not found in labels ~A"
              target-label labels))
    (IF (AND (> (LENGTH rows) 1) (NOT replace-all))
        (ERROR (JOIN "Multiple matches found. Correct or use REPLACE-ALL. "
                  "Matches are: ~a") rows))
    (FOR-EACH row IN rows  
         (IF verbose (DISPLAY-LINE row))
         (UNLESS no-change
            ( ; BBI-SEED-QUERY
             genericdb::xsql 
              (JOIN "delete from " table " where "
                  (FIRST labels) " = " (FIRST row))))
         (SETF (NTH label-index row) value)
         (IF verbose (DISPLAY-LINE row))
         (UNLESS no-change				  
            ( ; BBI-SEED-QUERY
             genericdb::xsql 
              (JOIN "insert into " table " " 
                  (STRINGIFY-FOR-MYSQL labels) " values "
                  (STRINGIFY-FOR-MYSQL row :WITH-QUOTES T) ";"))))
 ))
 
#|
(DEFUN Mysql-labels-of (table)
  (NTH-VALUE 1
    (BBI-SEED-QUERY
      (JOIN "select * from " table " limit 0"))))
|#
(DEFUN Mysql-labels-of (table)
  (genericdb::xsql (JOIN "select * from " table " limit 0")))
                    
                    
(DEFUN Mysql-item-from (table key-label key-value)
 (LET* ((key-value-string
            (IF (STRINGP key-value)
			    (JOIN "'" key-value "'")
				key-value))
       )
  ( ;BBI-SEED-QUERY 
   genericdb::xsql 
     (JOIN "select * from " table " where " 
        key-label " = " key-value-string))     
))