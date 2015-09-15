;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE :bbi)

(DEFCONSTANT type-code
  (LIST (LIST "delete" (CODE2CHAR 255))
        (LIST "indent" (CODE2CHAR 254))
        (LIST "section" (CODE2CHAR 253))
        (LIST "no-break" (CODE2CHAR 252))
        (LIST "place-holder" (CODE2CHAR 251))
        (LIST "literal-string" (CODE2CHAR 249))
        (LIST "literal-list" (CODE2CHAR 248))
        (LIST "function" (CODE2CHAR 247))
        (LIST "list-end" (CODE2CHAR 246))
))   

(DEFCONSTANT function-sections
  (LIST (LIST "DEFINE-FUNCTION" 
          (LIST
            (LIST "SUMMARY" "BODY")
            (LIST "REQUIRED" "FLAGS" "KEYWORDS"
                  "TYPE" "CONVERT" "MAP" "INIT")))
        (LIST "FOR-EACH" 
           (LIST
            (LIST "WHILE" "UNTIL" "INITIALIZE" "ASSIGN"
                  "BODY"
                  "WHEN" "COLLECT" "APPEND" "MIN" "MAX" 
                  "SUM" "COUNT" "FINALLY")
            (LIST "FOR")))
        (LIST "APPLY-FUNCTION"
           (LIST
            (LIST "REPLACING" "WITH") NIL))
        )
)  
         
(DEFUN Sections-of-function (function)
  (SECOND (ASSOC function function-sections :TEST 'EQUAL))
)
       
(DEFUN Code-of-type (type &KEY as-char)
  (LET ((code (SECOND (ASSOC type type-code :TEST 'EQUAL)))
       )
    (IF as-char (CHAR code 0) code)
))    

(DEFUN Type-of-code (code)
  (FIRST (FIRST (bbl::FILTER type-code SAME-AS code BY-POSITION 2)))
)

(DEFUN feature-keys (feature-table type)
  (bbl::SECOND IN-EACH
     (BBL::FILTER
        (CONVERT feature-table TO List) 
        SAME-AS type))
) 

(DEFUN Encode-key (type &OPTIONAL n)
  (LET* ((base 112)
         (max-key-value (* base base))
         (type-char (OR (CODE-OF-TYPE type)
                    (ERROR "Bad type '~A'" type)))
         )
   (COND
    ; ((OR (EQUAL type "indent")) type-char)
      ((NULL n) (ERROR "No number specified"))
      ((NOT (INTEGERP n))
       (ERROR "Attempt to encode a non-integer ~A" n))
      ((< n 0)
       (ERROR "Attempt to encode a negative integer ~A" n))
      ((> n max-key-value)
       (ERROR "Attempt to encode a integer (~A) that's too big " n))
      (T
        (LET* ((high-digit)(low-digit))
          (ASSIGN (high-digit low-digit) (FLOOR n base))
          (JOIN type-char 
              (CODE2CHAR (+ 132 high-digit))
              (CODE2CHAR (+ 132 low-digit)))
         )))
))

(DEFUN Decode-key (key)
  (LET* ((base 112)
         (type-char (bbl::FIRST key))
         (type (OR (TYPE-OF-CODE type-char)
                   (ERROR "Bad type character '~A'" 
                            (CHAR2CODE type-char))))
         )
   (COND
    ; ((OR (EQUAL type "indent")) type)
      ((NOT (= (LENGTH key) 3))
        (ERROR "Attempt to decode a key ('~A') not of length 3" key))
      (T
        (LET* ((high-digit (- (CHAR2CODE (bbl::SECOND key)) 132))
               (low-digit (- (CHAR2CODE (bbl::THIRD key)) 132))
               (n (+ (* high-digit base) low-digit))
              )
          (IF (< high-digit 0)
              (ERROR "Attempt to decode a key (~A) with an illegal digit" 
                          (CHAR2CODE (bbl::SECOND key))))
          (IF (< low-digit 0)
              (ERROR "Attempt to decode a key (~A) with an illegal digit" 
                          (CHAR2CODE (bbl::THIRD key))))
          (LIST type n)
        )))
))


(DEFUN Increment-key (key &OPTIONAL given-type)
  (LET* ((max-key-value (* 112 112))
         (key-value)
         (key-type)
        )
    (IF key (ASSIGN (key-type key-value) =  (DECODE-KEY key)))
    (COND
       ((AND (NULL key) (NULL given-type))
          (ERROR "Type must be specified"))
       ((NULL key) (ENCODE-KEY given-type 0))
   ;   ((OR (EQUAL key-type "indent"))
   ;      (ERROR "Can't increment a key-type ('~A') without a value" key-type))
       ((NOT (EQUAL key-type given-type))
          (ERROR "Key type ('~A') doesn't match specified type ('~A')"
               key-type given-type))
       ((>= key-value max-key-value)
          (ERROR "Too many instances of ~A" key-type))
       (T (ENCODE-KEY key-type (+ key-value 1) )))
 ))

(DEFUN Next-key (feature-table type)
  (LET* ((keys (BBL::SORT (FEATURE-KEYS feature-table type)))
        )
    (INCREMENT-KEY (bbl::LAST keys) type)
  ))

  
(DEFUN All-keys (feature-table type &OPTIONAL (direction 'DESCENDING))
  (IF (SAME direction 'DESCENDING)
      (BBL::SORT (FEATURE-KEYS feature-table type) DESCENDING)
      (BBL::SORT (FEATURE-KEYS feature-table type) ASCENDING))
)
  
(DEFUN Replace-into (string from to replacement)
   (CONCATENATE 'STRING
      (IF (> from 1)
          (SUBSTRING string TO (1- from)))
      replacement
      (IF (< to (LENGTH string))
          (SUBSTRING string FROM (1+ to)))
      )
 )

      
(DEFUN Delete-within-string (string from to key)
  (LET* ((delete-symbol (CODE-OF-TYPE "delete"))
         (replacement 
           (JOIN key (REPEAT delete-symbol (- to from -1 (LENGTH key)))))
      )
    (REPLACE-INTO string from to replacement)
))
  
(DEFUN Compress-literal-strings (code-string &OPTIONAL feature-table)
  (LET* ((literal-sites (MATCHES-OF-PATTERN "\"[^\"]+\"" IN code-string 
          -DISPLAY AS-REGEX))
         (to-delete (CODE-OF-TYPE "delete" :AS-CHAR T))
        )
    (IF (NOT feature-table)
        (SETQ feature-table (NEW-TABLE '($ $))))
    (FOR-EACH literal-site IN literal-sites
         AS coord1 = (FIRST literal-site)
         AS coord2 = (SECOND literal-site)
         AS literal = (THIRD literal-site)
         AS key = (NEXT-KEY feature-table "literal-string")
         (SETF (REF feature-table "literal-string" key) literal)
         (SETF code-string (DELETE-WITHIN-STRING code-string coord1 coord2 key))
        )
    (SETF code-string (REMOVE to-delete code-string))
    (LIST code-string feature-table) 
))


(DEFUN match-of-pattern-kludge (string from)
  (LET* ((matches (MATCHES-OF-PATTERN "\\' *?\\(" 
                       IN (SUBSTRING string FROM from)
                       +1st-MATCH-ONLY +COORDINATES -DISPLAY AS-REGEX))
         )
    (IF matches (SUM-OF matches from -1))
))

(DEFUN Compress-literal-lists (code-string &OPTIONAL feature-table)
    (IF (NOT feature-table)
        (SETF feature-table (NEW-TABLE '($ $))))
    (FOR-EACH i FROM 1 ; TO 1 00000
         AS literal-list-match 
              = (MATCHES-OF-PATTERN "\\' *?\\(" IN code-string 
                       +1st-MATCH-ONLY +COORDINATES -DISPLAY AS-REGEX)
         WHILE literal-list-match
         AS quote-start = (FIRST literal-list-match)
         AS literal-list-start = (SECOND literal-list-match)
         AS literal-list-end =
            (FOR-EACH pos FROM literal-list-start TO (LENGTH code-string)
           ; FOR-EACH letter IN (SUBSTRING code-string FROM literal-list-start)
                 AS letter = (SUBSTRING code-string FROM pos LENGTH 1)
                 INITIALIZE paren-level = 0
                 (WHEN-VALUE-OF letter
                      IS "(" THEN (INCF paren-level)
                      IS ")" THEN (DECF paren-level))
                 (WHEN (<= paren-level 0) (RETURN pos))
                )
         AS literal-list 
            = (JOIN "'"
                (SUBSTRING code-string FROM literal-list-start TO literal-list-end))
         (ASSIGN (literal-list feature-table)
            = (COMPRESS-NESTED-LIST literal-list feature-table "literal-list"))
         (SETF code-string (REPLACE-INTO code-string quote-start
                                 literal-list-end literal-list))
         )
    (LIST code-string feature-table) 
)

(DEFUN Decompress-lists (code-string feature-table type)
   (LET* ((all-keys (ALL-KEYS feature-table type))
         )
   (FOR-EACH key IN all-keys
        AS match = (SEARCH key code-string)
        AS coord1 = (IF match (+ match 1))
        AS coord2 = (IF match (+ coord1 2))
        AS literal = (REF feature-table type key)
     ;  AS tag = (IF (SAME type "literal-list")
     ;               (NEXT-KEY feature-table "no-break")
     ;               (NEXT-KEY feature-table "function"))
        (IF match
            (SETF code-string 
               (REPLACE-INTO code-string coord1 coord2 
                               (JOIN key literal key)))
          #| (ERROR "Code (~a) of type ~a not found in '~A' " 
                     key type code-string)  |#
            )        
        )
    code-string
))
        
  
(DEFUN Decompress-literal-strings (code-string feature-table)
   (LET* ((literal-string-key (CODE-OF-TYPE "literal-string"))
          (place-holder (CODE-OF-TYPE "place-holder"))
          )
     (FOR-EACH match IN 
             (REVERSE (MATCHES-OF-PATTERN (JOIN literal-string-key "..") 
                         IN code-string AS-REGEX))
          AS coord1 = (FIRST match)
          AS coord2 = (SECOND match)
          AS key = (THIRD match)
          AS literal = (REF feature-table "literal-string" key)
          AS dummy-literal = (JOIN key (REPEAT place-holder (LENGTH literal)) key)
          (SETF code-string (REPLACE-INTO code-string coord1 coord2 dummy-literal)))
     code-string
))

(DEFUN Replace-dummy-literal-strings (code-string feature-table)
   (LET* ((tag-code (CODE-OF-TYPE "literal-string"))
          (dummy-code (CODE-OF-TYPE "place-holder" :AS-CHAR T))
          (dummy-matches
            (MATCHES-OF-PATTERN (JOIN "(" tag-code "..)(.*?)(" tag-code "..)")
                IN code-string +SUB-MATCHES +SUB-COORDINATES -DISPLAY AS-REGEX))
         )
    (FOR-EACH (code1-info dummy code2-info) IN (REVERSE dummy-matches)
         AS coord1 = (FIRST code1-info)
         AS coord2 = (SECOND code2-info)
         AS tag1 = (THIRD code1-info)
         AS tag2 = (THIRD code2-info)
         AS dummy-string = (THIRD dummy)
         AS literal-string = (REF feature-table "literal-string" tag1)
         (IF (NOT (EQUAL tag1 tag2))
             (ERROR (S+ "Supposedly matching tags (~A and ~A) don't "
                        "match in '~A'") tag1 tag2 code-string))
         (IF (NOT (EQUAL (REMOVE dummy-code dummy-string) ""))
             (ERROR (S+ "Attempt to remove place-holder in literal string "
                        "found something else ('~A') in '~A'")
                    dummy-string code-string))
         (SETF code-string 
             (REPLACE-INTO code-string coord1 coord2 literal-string)))
     code-string
))


(DEFUN Replace-text-with-code (code-string start end key)
  (LET ((to-delete (CODE-OF-TYPE "delete" :AS-CHAR T)))
   (SETF code-string 
      (DELETE-WITHIN-STRING code-string start end key))
   (SETF code-string (REMOVE to-delete code-string))
   code-string
))

(DEFUN Restore-special-list (word text feature-table)
  (FOR-EACH tag 
          IN (MATCHES-OF-PATTERN (JOIN word " (...)") IN text 
                +SUB-MATCHES AS-REGEX)
       AS tag-code = (bbl::FIRST tag)
       (WHEN (EQUAL (TYPE-OF-CODE tag-code) "function")
          (LET ((list (REF feature-table "function" tag)))
            (SETF (REF feature-table "function" tag) NIL)
            (SETF text 
              (bbl::REPLACE INTO text REPLACING-FIRST tag WITH list))))
   )
   text
)

(DEFUN Parse-special-functions (list-text function function-tag feature-table)
  (LET* ((section-word-lists (SECTIONS-OF-FUNCTION function))
         (section-words (JOIN section-word-lists AS-LIST))
         (section-words-special (SECOND section-word-lists))
         (section-code (CODE-OF-TYPE "section"))
         (tag (JOIN section-code (SUBSTRING function-tag FROM 2)))
        )

    (FOR-EACH word IN section-words
         AS query = (JOIN " " word " ")
         (IF (MEMBER word section-words-special :TEST 'EQUAL)
             (SETF list-text 
                (RESTORE-SPECIAL-LIST word list-text feature-table)))
         (LOOP WHILE (SEARCH query list-text)
             DO (SETF list-text
                   (bbl::REPLACE INTO list-text 
                         REPLACING-FIRST query
                         WITH (JOIN " " tag word " ")))
             )
         )
    list-text
))


(DEFUN Compress-list (code-string feature-table type)
  (LET* ((list-match 
              (MATCHES-OF-PATTERN "\\'?\\([^(]+?\\)" IN code-string 
                       +1st-MATCH-ONLY -DISPLAY AS-REGEX))
         (coord1 (FIRST list-match))
         (coord2 (SECOND list-match))
         (list-text (THIRD list-match))
         (key (NEXT-KEY feature-table type))
         (function 
            (IF (EQUAL type "function")
                (MATCHES-OF-PATTERN "\\( *([^ ]+)" IN list-text 
                       +1st-MATCH-ONLY -DISPLAY +SUB-MATCHES AS-REGEX)))
        )
    (WHEN list-match
       (SETF list-text (PARSE-SPECIAL-FUNCTIONS list-text function key feature-table))
       (SETF (REF feature-table type key) list-text)
       (SETF code-string (REPLACE-TEXT-WITH-CODE code-string coord1 coord2 key))
       )
    (LIST code-string feature-table)
))

(DEFUN Compress-nested-list (code-string feature-table type)
   (FOR-EACH i FROM 1 ; TO 1 00000
        AS new-code-string = NIL
        AS new-feature-table = NIL
        (ASSIGN (new-code-string new-feature-table)
           = (COMPRESS-LIST code-string feature-table type))
        (IF-TRUE (EQUAL code-string new-code-string)
            THEN (RETURN (LIST code-string feature-table))
            ELSE (SETF code-string new-code-string)
                 (SETF feature-table new-feature-table))
        )
   (LIST code-string feature-table)
)

(DEFUN Compress-functions (code-string &OPTIONAL feature-table)
    (IF (NOT feature-table)
        (SETF feature-table (NEW-TABLE '($ $))))
   (COMPRESS-NESTED-LIST code-string feature-table "function")
   )

(DEFUN test-parsing (&OPTIONAL (code-string 
     "(LIST (LENGTH-OF all4312) \"abcd\" '(1 2 (3 4)))"
   ; "(LIST '(1 2 3)  \" ABC\" '(4 5 6))"
     ))
  (LET* ((feature-table)
        )
     (DISPLAY-DATA code-string feature-table)
     (ASSIGN (code-string feature-table)
       = (COMPRESS-LITERAL-STRINGS code-string))
     (DISPLAY-DATA code-string feature-table)
     (ASSIGN (code-string feature-table)
       = (COMPRESS-LITERAL-LISTS code-string feature-table))
     (DISPLAY-DATA code-string feature-table)
     (ASSIGN (code-string feature-table)
       = (COMPRESS-FUNCTIONS code-string feature-table))
     (DISPLAY-DATA code-string feature-table)
     (SETF code-string (DECOMPRESS-LISTS code-string feature-table "function"))
     (DISPLAY-DATA code-string feature-table)
     (SETF code-string (DECOMPRESS-LISTS code-string feature-table "literal-list"))
     (DISPLAY-DATA code-string feature-table)
     (SETF code-string (DECOMPRESS-LITERAL-STRINGS code-string feature-table))
     (DISPLAY-DATA code-string feature-table)
     (LIST code-string feature-table)
     )       
)

(DEFUN Tagless-length-of (string)
   (+ (- (LENGTH string)
         (COUNT-OF 
            (BBL::FILTER 
               (APPLY-FUNCTION (CHAR2CODE letter) 
                  REPLACING letter 
                  WITH (SPLIT string) -DISPLAY)
                > 127)))
      (COUNT-OF (CODE-OF-TYPE "place-holder") IN string))
)

(DEFUN Extract-tagged-segment (string &OPTIONAL tag)
   (IF (< (LENGTH string) 6) 
       (LIST string NIL)
       (PROGRAM
          (IF (NOT tag) (SETF tag (bbl::FIRST 3 IN string)))
          (IF (TYPE-OF-CODE (bbl::FIRST tag))
              (MATCHES-OF-PATTERN (JOIN tag "(.+?)" tag "(.*)") 
                  IN string +1st-MATCH-ONLY CROSS-LINES +SUB-MATCHES AS-REGEX)
              (LIST string NIL))))
)

(DEFUN Detag (string feature-table)
  (LET* ((new-string (REPLACE-DUMMY-LITERAL-STRINGS string feature-table))
         (code-threshold 246)
         )
    (SETF new-string
      (FOR-EACH letter IN new-string
           INITIALIZE to-delete = 0
           AS code = (CHAR2CODE (STRING letter))
           (COND
              ((> code code-threshold) (SETF to-delete 3))
              (T (DECF to-delete)))
           WHEN (< to-delete 1)
             COLLECT letter)
           )
    (JOIN new-string)
))
  
(DEFUN Compress-white-spaces (string)
  (LET* ((new-string (REMOVE (CODE-CHAR 10) string))
         )
    (JOIN (SPLIT new-string BETWEEN-WORDS) BY " ")
))

(DEFMACRO FF (function &REST rest)
  `(FORWARD-FUNCALL ,function ,@rest)
)

(DEFUN Output-fragment (lines remaining feature-table indentation 
                        &OPTIONAL (unindent 0))
 ; (DISPLAY-DATA "**********" lines remaining indentation unindent)
   (LET* ((width 65)
          (spacing 3)
          (net-indentation (- indentation unindent))
          (indent (REPEAT " " net-indentation))
          (tag (IF (> (LENGTH remaining) 5) (bbl::FIRST 3 IN remaining)))
          (tag-type (IF tag (TYPE-OF-CODE (bbl::FIRST tag))))
          (matches)
         )
   (COND
      ((EQUAL (REMOVE-ALL-WHITESPACE remaining) ""))
      ((<= (TAGLESS-LENGTH-OF remaining) (- width net-indentation))
            ; (DISPLAY-DATA net-indentation) ; ***********
           (SETF lines (JOIN lines (JOIN indent (DETAG remaining feature-table)) AS-LIST)))
      ((OR (EQUAL tag-type "function") (EQUAL tag-type "section"))
           (LET* ((section-tag (JOIN (CODE-OF-TYPE "section") (SUBSTRING tag FROM 2)))
                  (part1)(part2) (part1-sections))
              (ASSIGN (part1 part2) = (EXTRACT-TAGGED-SEGMENT remaining tag))
              (SETF part1-sections (SPLIT part1 EVERY section-tag))
              (FOR-EACH part IN part1-sections
                   AS new-indentation = (+ indentation spacing) THEN (+ indentation spacing spacing)
                   (SETF lines (FF 'OUTPUT-FRAGMENT lines part feature-table
                        new-indentation spacing)))
              (SETF lines (FF 'OUTPUT-FRAGMENT lines part2 feature-table indentation))
              ))
      ((MATCHES-OF-PATTERN (CODE-OF-TYPE "function") IN remaining -DISPLAY AS-REGEX)
           (LET* ((split (SPLIT remaining BEFORE AT (CODE-OF-TYPE "function")))
                  (part1 (FIRST split))
                  (part2 (SECOND split)))
              (SETF lines (FF 'OUTPUT-FRAGMENT lines part1 feature-table indentation unindent))
              (SETF lines (FF 'OUTPUT-FRAGMENT lines part2 feature-table indentation))))
      ((SETF matches (MATCHES-OF-PATTERN (CODE-OF-TYPE "literal-list") 
           IN (SUBSTRING remaining TO (MIN (LENGTH remaining) (- width 3))) -DISPLAY AS-REGEX))
                        ; Inaccurate, since tags aren't first removed
          (LET* ((split-point (FIRST (bbl::LAST matches)))
                 (split (SPLIT remaining BEFORE AT split-point))
                 (part1 (FIRST split))
                 (part2 (SECOND split))
                 (part2-a) (part2-b))
              (ASSIGN (part2-a part2-b) = (EXTRACT-TAGGED-SEGMENT part2))
              (SETF lines (FF 'OUTPUT-FRAGMENT lines part1 feature-table indentation unindent))
              (SETF lines (FF 'OUTPUT-FRAGMENT lines part2-a feature-table indentation))
              (SETF lines (FF 'OUTPUT-FRAGMENT lines part2-b feature-table indentation))
              ))
      ((SETF matches (MATCHES-OF-PATTERN " " 
           IN (SUBSTRING remaining TO (MIN (LENGTH remaining) (- width net-indentation))) 
                   -DISPLAY AS-REGEX))
                        ; Inaccurate, since tags aren't first removed
          (LET* ((split-point (FIRST (bbl::LAST matches)))
                 (split (SPLIT remaining BEFORE AT split-point))
                 (part1 (FIRST split))
                 (part2 (IF (> (LENGTH (SECOND split)) 1)
                                (SUBSTRING (SECOND split) FROM 2))))
              (SETF lines (FF 'OUTPUT-FRAGMENT lines part1 feature-table indentation unindent))
              (IF part2 (SETF lines (FF 'OUTPUT-FRAGMENT lines part2 feature-table indentation)))))
       (T ; line too long, no obvious split
          (LET* ((available-width (- width net-indentation spacing))
                 (detagged-remaining (DETAG remaining feature-table))
                 (parts (SPLIT detagged-remaining EVERY available-width)))
            (FOR-EACH part IN parts
                (SETF lines (FF 'OUTPUT-FRAGMENT lines part feature-table indentation unindent)))))
       )
   lines 
))

(DEFUN Display-parsed-code (code-string feature-table)
  (LET* ((indentation 0)
         (code-string-edited (COMPRESS-WHITE-SPACES code-string))
         (output-lines (OUTPUT-FRAGMENT NIL code-string-edited feature-table indentation)) 
        )
   ; (DISPLAY-LIST EACH output-lines)
   output-lines
))

(DEFUN parse-code (code-string)
  (LET* ((feature-table)
         (parsed-code-string (COMPRESS-WHITE-SPACES code-string))
        )
     ; ***** Replace all literal strings with tags, saving strings in feature table
     (ASSIGN (parsed-code-string feature-table)
       = (COMPRESS-LITERAL-STRINGS parsed-code-string))
     ; ***** Replace all literal lists with tags, saving lists in feature table
     (ASSIGN (parsed-code-string feature-table)
       = (COMPRESS-LITERAL-LISTS parsed-code-string feature-table))
     ; ***** Replace all other (...) with tags, saving in feature table
     ; ***** Mark sections of special functions
     ; ***** parsed-code-string should now be "@", where @ is one tag
     (ASSIGN (parsed-code-string feature-table)
       = (COMPRESS-FUNCTIONS parsed-code-string feature-table))
     ; ***** Restore (...), each surrounded with unique tag
     (SETF parsed-code-string (DECOMPRESS-LISTS parsed-code-string feature-table "function"))
     ; ***** Restore literal lists, each surrounded with unique tag
     (SETF parsed-code-string (DECOMPRESS-LISTS parsed-code-string feature-table "literal-list"))
     ; ***** Restore literal lists, but with content replaced by tag dummy-string tag     
     (SETF parsed-code-string (DECOMPRESS-LITERAL-STRINGS parsed-code-string feature-table))
     (LIST parsed-code-string feature-table)
     )       
)
   
(DEFUN Display-code-aux (code &OPTIONAL p)
  (LET* ((feature-table)(parsed-code)(output-lines)
         (code-string (FORMAT NIL "~S" code) )
        )
    (ASSIGN (parsed-code feature-table) = (PARSE-CODE code-string))
    (SETF output-lines (DISPLAY-PARSED-CODE parsed-code feature-table))
    (IF p
        (FOR-EACH line IN output-lines
             (FORMAT p "~A~%" line))
        (DISPLAY-LIST EACH output-lines))
))

(DEFUN Display-code (code &optional p)
  (bbi::WITH-BBL-FORM (DISPLAY-CODE-aux code p))
)

(DEFUN Collapse-code (code-string)
  (LET* ((feature-table)
         (parsed-code-string)
        )
     (ASSIGN (parsed-code-string feature-table)
       = (COMPRESS-LITERAL-STRINGS code-string))
     (ASSIGN (parsed-code-string feature-table)
       = (COMPRESS-LITERAL-LISTS parsed-code-string feature-table))
     (ASSIGN (parsed-code-string feature-table)
       = (COMPRESS-FUNCTIONS parsed-code-string feature-table))
     (LIST parsed-code-string feature-table)
))
      
;; Now to collapse nested functions and parse DEFINE-FUNCTION and LOOP and APPLY-FUNCTION
;; encode CR before keywords