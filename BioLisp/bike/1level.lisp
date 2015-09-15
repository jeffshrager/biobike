;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-       **********

(IN-PACKAGE :bbi)

; ========= LEVEL I ===========

; ******* INTERNAL SERVICE FUNCTIONS *******

; =========== Argument and error handling ==========

(DEFUN Error-if-more-than-one-aux (symbols values)
   (LET ((provided-items 
            (COUNT-IF-NOT (LAMBDA (x) (OR (NOT x) (UNPROVIDED? x)))
                             values)))
     (IF (> provided-items 1)
         (ERR+ problem "You may only choose one of these options:"
                          indent  "~{ ~A~}.~&"
                          format-args symbols)
       )))
       
(DEFMACRO Error-if-more-than-one (&REST symbols)
   `(ERROR-IF-MORE-THAN-ONE-AUX '(,@symbols) (LIST ,@symbols)))

(DEFUN Provided-value (given-value)
   (IF (UNPROVIDED? given-value) NIL given-value))

(DEFMACRO One-from (symbols &KEY (error T))
   `(PROGN
      (IF ,error (ERROR-IF-MORE-THAN-ONE-AUX '(,@symbols) (LIST ,@symbols)))
      (FIND-IF-NOT 'NULL (MAPCAR 'PROVIDED-VALUE (LIST ,@symbols)))))

(DEFUN Lone-replicon-or-Error (organism)
  (LET* ((contigs (SLOTV organism #$Contiguous-sequences)))
    (IF (= (LENGTH contigs) 1)
        (biolisp::FIRST contigs)
      (ERR+
       problem "Organism ~A has more than" 
       indent  "one contiguous-sequence: "
       indent  "~A."
       advice  "Specify which one you mean.~&"
       format-args
       organism 
       (IF (< (LENGTH contigs) 10)
           contigs
         (FORMAT NIL "~A ..." (REF contigs 1 -> 9))))
      )))

(DEFUN Provided (given-value)
   (UNLESS (OR (UNPROVIDED? given-value) 
               (NULL given-value))
       given-value))

(DEFUN Unprovided (given-value)
   (NOT (provided given-value)))

(DEFUN Unless-provided (given-value default)
   (IF (PROVIDED given-value)
       given-value
       default))

(DEFUN Internal-error (place)
   (ERROR "INTERNAL ERROR in ~A! Please report conditions leading to this error."
      place))

(DEFUN Check-syntax (args key-list flag-list)
   (LOOP FOR i FROM 1 TO (LENGTH args)
         AS arg = (REF args i)
         DO (COND 
               ((MEMBER arg key-list :TEST 'SYMBOL=) (INCF i))
               ((MEMBER arg flag-list :TEST 'SYMBOL=) NIL)
               (T (RETURN NIL)))
        FINALLY (RETURN T)))

(DEFUN Resolve-*end* (list-or-string value-or-list)
   (LET* ((end-threshold 
           (- *end* 
              #+allegro 200000000
              #+lispworks 2000000
              #-(or allegro lispworks) (error "Fix me!")
              ))
          (result
            (LOOP FOR item IN (ENSURE-LIST value-or-list)
                  COLLECT
                     (COND
                        ((< item end-threshold) item)
                        ((= item *unprovided+*) item)
                        (T (+ (- item *end*) (LENGTH list-or-string))))))) 
    (IF (LISTP value-or-list) result (REF result 1))))


(DEFMACRO Chosen-symbol (value-list &OPTIONAL symbol-list)
  (IF (NOT symbol-list) (SETF symbol-list value-list))
  (COND 
     ((AND (LISTP value-list) (LISTP symbol-list))
       `(REF ',symbol-list (1+ (POSITION T (LIST ,@value-list)))))
     ((LISTP symbol-list)
       `(REF ',symbol-list (1+ (POSITION T ,value-list))))
     ((LISTP value-list)
       `(REF ,symbol-list (1+ (POSITION T (LIST ,@value-list)))))
     (T
       `(REF ,symbol-list (1+ (POSITION T ,value-list))))))

(DEFMACRO Chosen-symbols (value-list &OPTIONAL symbol-list)
  (IF (NOT symbol-list) (SETF symbol-list value-list))
  (COND 
     ((AND (LISTP value-list) (LISTP symbol-list))
       `(LOOP FOR item in ',symbol-list
              FOR value in (LIST ,@value-list)
              WHEN value COLLECT item))
     ((LISTP value-list)
       `(LOOP FOR item in ,symbol-list
              FOR value in (LIST ,@value-list)
              WHEN value COLLECT item))
     ((LISTP symbol-list)
       `(LOOP FOR item in ',symbol-list
              FOR value in ,value-list
              WHEN value COLLECT item))
     (T
       `(LOOP FOR item in ,symbol-list
              FOR value in ,value-list
              WHEN value COLLECT item))))


;;; Must not be processed by BBLOAD !!!!!
;;; This function is called by a macro.

(DEFUN Force-in-bounds (item label low high strict)
  (COND
   ((< item low)
    (IF strict
        (ERR+ Problem "~A cannot take a value less than ~A,"
              Indent "but ~A has the value of ~A."
              Advice "Bring the number within bounds,"
              Indent "or use the NONSTRICT flag."
              format-args label low label item)
      low))
   ((> item high)
    (IF strict
        (ERR+ Problem "~A cannot take a value greater than the length of"
              Indent "the target (~A),"
              Indent "but ~A has the value of ~A"
              Advice "Bring the number within bounds,"
              Indent "or use the NONSTRICT flag."
              format-args label high label item)
      high))
   (T item)))

(DEFUN complex-nsubstitute (new old list &KEY (test 'EQUAL))
  (NSUBSTITUTE new old list :TEST test)
  ;; Must use LISP:LOOP because function is called from a BBL macro
  ;; Macros cannot call functions at expansion time which execute BBL code.
  ;; JP
  (LISP:LOOP FOR item IN list
        DO (IF (LISTP item)
               (FORWARD-FUNCALL 'COMPLEX-NSUBSTITUTE new old item :TEST test))))

; =========================================

(DEFUN Called-from-vpl ()
  wb::*VPL-EVALUATING?*)

(DEFUN Add-df-symbol-to-vpl-palette (symbol &KEY variable)
  ;; Adds symbol to function palette
  ;; Used by ENTER
  (UNLESS (SYMBOL-VALUE-IN-PACKAGE :*vpl-system-initialized?* 'NVPL)
     (ERROR "VPL system is not running...~%"))
  (UNLESS wb::*sessionid*
     (ERROR "No current user session...~%"))
  (UNLESS wb::*vpl-evaluating?*
    (ERROR
     #.(ONE-STRING-NL
        "ADD-DF-SYMBOL-TO-VPL-PALETTE can only be evaluated"
        "when the VPL is evaluating a form that calls it."
        )))
  (IF variable
      (FORWARD-PACKAGE-FUNCALL
         :nvpl :add-definition-for-user `(bbi::DEFINE ,symbol))
     (FORWARD-PACKAGE-FUNCALL
         :nvpl :add-definition-for-user `(bbi::DEFINE-FUNCTION ,symbol)))
  )

(DEFUN Add-variable-symbol-to-vpl-palette (symbol)
  (ADD-DF-SYMBOL-TO-VPL-PALETTE symbol :VARIABLE T))


(DEFUN Read-file-enter-functions (file-name &OPTIONAL package-symbol)
  ;; At present, this works only when package-symbol is provided
  ;; It should work when the functions are not in a package, but doesn't
  ;; How could that be done?
  (LET ((lisp:*load-verbose* nil) 
        (lisp:*load-print* nil)
        (lisp:*compile-verbose* nil) 
        (lisp:*compile-print* nil))
    (PROGV (list (intern :*vpl-console-log* :nvpl)) (list nil)
      (UNLESS (AND package-symbol 
                   (FIND-PACKAGE package-symbol))
         (C/L file-name))
      (IF (CALLED-FROM-VPL) 
          (LOOP FOR fn 
                IN (EXTERNAL-DEFINE-FUNCTIONS-OF-PACKAGE package-symbol)
                DO (ADD-DF-SYMBOL-TO-VPL-PALETTE fn)
                   (FORMAT T "~&Adding ~A to the FUNCTIONS menu" fn))))))

; ================= ALL|ANY-TRUE|FALSE-aux ===========                   
(DEFUN All-true-aux (arguments &KEY in-complex-list)
  (without-code-walker
    (LET ((mapping-function (IF in-complex-list 'MAPTREE 'MAPCAR)))
     (IF (NULL arguments)
        NIL
        (BLOCK Exit
          (FUNCALL mapping-function
            (LAMBDA (x) (WHEN (NULL x) (RETURN-FROM Exit NIL)))
            arguments)
          T))
 )))	

(DEFUN All-false-aux (arguments &KEY in-complex-list)
  (without-code-walker
    (LET ((mapping-function (IF in-complex-list 'MAPTREE 'MAPCAR)))
     (IF (NULL arguments)
         T
         (BLOCK Exit
           (FUNCALL mapping-function
             (LAMBDA (x) (WHEN x (RETURN-FROM Exit NIL)))
             arguments)
           T))
)))  

(DEFUN Any-true-aux (arguments &KEY in-complex-list)
  (without-code-walker
    (LET ((mapping-function (IF in-complex-list 'MAPTREE 'MAPCAR)))
     (IF (NULL arguments)
        NIL
        (BLOCK Exit
          (FUNCALL mapping-function
            (LAMBDA (x) (WHEN x (RETURN-FROM Exit T)))
            arguments)
          NIL))
)))
		
(DEFUN Any-false-aux (arguments &KEY in-complex-list)
  (without-code-walker
    (LET ((mapping-function (IF in-complex-list 'MAPTREE 'MAPCAR)))
     (IF (NULL arguments)
         T
         (BLOCK Exit
           (FUNCALL mapping-function
             (LAMBDA (x) (WHEN (NULL x) (RETURN-FROM Exit T)))
             arguments)
        NIL))
)))
   
   
(DEFUN Array-to-list (array)
 "Returns list equivalent of given array"
  (LOOP FOR item ACROSS array
    COLLECT item))

(DEFUN String-to-$ (list)
   (LOOP FOR item IN list
         AS new-item = 
             (COND
               ((NUMBERP item) item)
               ((LISTP item) (ERROR "Index can't be a list!"))
               (T "$"))
         COLLECT new-item))

(DEFMACRO Defined (symbol)
 ; SUMMARY "Determines if given symbol has already been defined"
 ; REQUIRED symbol
 ; BODY
  (IF *in-bbl-form-processor* 
      (IF (SYMBOLP symbol)
          (IF (IN-BBL-ENVIRONMENT? symbol) 
              T 
              `(BOUNDP ',symbol))
          NIL
        )
    `(LET ((error (GENSYM)))
       (IF (EQUAL (HANDLER-CASE ,symbol (ERROR () error)) error)
           NIL
         T))))

(defun define-function-p (s) 
  (and (symbolp s) (fboundp s) (get s :define-function-parse)))

(DEFUN External-define-functions-of-package (p)
  (LET ((edfs NIL))
    (DO-EXTERNAL-SYMBOLS (x p)
      (WHEN (define-function-p x)
        (PUSH x edfs)
        ))
    edfs
    ))

(DEFUN Defined-locally (symbol)
  (unless *in-bbl-form-processor*
    (error 
     "You shouldn't be calling DEFINED-LOCALLY when not in BBL form processing!"))
  (in-bbl-environment? symbol))

(DEFUN Defined-globally (symbol) (boundp symbol))

(DEFMACRO Expand-macro (macro)
"Shows expansion of macro without executing it"
  `(PROGN (PPRINT (MACROEXPAND-1 ',macro))
          (VALUES)))


(DEFMACRO Exists (variable-or-expression &OPTIONAL (return-value NIL))  
  "
 (EXISTS variable-or-expression)
   - If variable has never been defined, then returns NIL
       (or RETURN-VALUE if specified)
   - Otherwise, returns value of variable or expression
  "
  (IF (NOT (SYMBOLP variable-or-expression))
      variable-or-expression
     `(LOCALLY
         (DECLARE (Optimize (Speed 2) (Safety 1) (Space 1) (Debug 3)))
         (HANDLER-CASE ,variable-or-expression (ERROR () ,return-value))
      )))


(DEFUN Human-base-to-0-base (coord length name
                             &KEY wrap truncate from-end no-check)
 "Translates coordinate as follows:
                          start ...  entity  ...   end
      Human-base:  -3 -2 -1 | +1 +2 +3 ... -3 -2 -1 | +1 +2 +3 (0 doesn't exist)
      0-base       -3 -2 -1    0 +1 +2 ...L-3 -2 -1   +0 +1 +2
  where L = length
  If WRAP is specified, then forces coord to within confines 
     of circular contig by MOD coord length.
  If TRUNCATE is specified, then truncates start at beginning of sequence
     and end at end of sequence
  Otherwise errors out
  When NO-CHECK is specified, the boundaries of the entity is ignored.
     This allows the return of negative offsets in calls from SEQUENCE-OF
  Returns modified-coordinate
 "  
  ;; Change base
  (IF (> coord 0) 
      (IF from-end
          (SETF coord (+ length coord -1))
          (SETF coord (- coord 1)))
      (IF from-end
          (SETF coord (+ length coord))))

  ;; Possibly wrap or truncate
  (COND
     ((AND (< coord 0) (NOT no-check))
       (COND
          (wrap (1+ (Lisp:MOD (1- coord) length)))
          (truncate 0)
          (T 
           (ERR+ problem "The given coordinate, '~A', in ~A"
                 indent  "is less than 1. "
                 advice  "Specify WRAP if you want the coordinate to wrap "
                 indent  "around the circular sequence."
                 indent  "Specify TRUNCATE if you want negative coordinates "
                 indent  "to be set to 1.~&"
                 format-args coord name)
           )))
     ((AND (>= coord length) (NOT no-check))
       (COND
         ;(wrap (1+ (Lisp:MOD (1- coord) length)))
          (wrap (Lisp:MOD coord length))
          (truncate (1- length))
          ((AND (= coord 0) (= length 0)) 0)
          (T (ERROR 
              (S+ (ERR+ 
                   problem "The given coordinate, '~A', is larger than the length"
                   indent  "of sequence ~A, ~A."
                   advice  "Specify WRAP if you want the coordinate to wrap "
                   indent  "around the circular sequence."
                   indent  "Specify TRUNCATE if you want negative coordinates "
                   indent  "to be set to length.~&" 
                   format-args (1+ coord) name length
                   ))))))
     (T coord)))


(DEFUN Inner-search (query target &KEY (test 'EQUAL) (start 0))
  "Looks for match within a list or string"
   (COND
      ((LISTP target)
         (POSITION query target :TEST test :START start))
      ((AND (STRINGP query) (STRINGP target))
         (SEARCH query target :TEST test :START2 start))
      ((AND (CHARACTERP query) (STRINGP target))
         (FIND query target :TEST test :START start))
      ((AND (CHARACTERP query) (CHARACTERP target))
         (EQL query target))
      (T (FUNCALL test query target))))


(DEFUN IsLIST? (list?)
  (AND list? (LISTP list?)))

(DEFUN BB-Is-list? (x &KEY nil-ok)
   (IF NIL-OK
       (LISTP x)
       (AND x (LISTP x))))      

(DEFUN Possibly-return (return? thing-to-return)
  ; SUMMARY  "(INTERNAL FUNCTION) Optionally suppresses results"
  ; REQUIRED (return? thing-to-return)
  ; TYPE return? = Boolean
  ; BODY
    (COND
       (return? thing-to-return)
       ((LISTP thing-to-return)
           (FORMAT NIL "List of length ~A suppressed" 
               (LENGTH thing-to-return)))
       (T "Results suppressed")))

(DEFUN Parse-range (range)
  ; SUMMARY "Parses list of format {m -> n}"
  ; REQUIRED range
  ;   TYPE range = List
  ; KEYWORD command = "(PARSE-RANGE ...)"
  ;   TYPE command = String
  ; RETURNS List
  ; BODY
     (IF (OR (> (COUNT '-> range) 1)
             (OR (NOT (= (LENGTH range) 3))
                 (NOT (EQUAL (biolisp::SECOND range) '->))))
         (ERROR "Poorly formed range '~A' in PARSE-RANGE" range))
     (LIST (biolisp::FIRST range) (1+ (biolisp::THIRD range))))


(DEFUN Parse-table-specs (given-specs)  
  ; SUMMARY "Parses specification string given to TABLE-FORMAT"
  ; REQUIRED given-specs
  ; ; TYPE given-specs = List
  ; RETURNS List
  ; BODY
  (LOOP FOR spec IN given-specs
        COLLECT 
        (COND
         ((EQUAL spec $) '$)
         ((EQUAL spec "$") '$)
         ((INTEGERP spec) 
          (IF (PLUSP spec) 
              (LIST 1 (1+ spec))
            (PARSE-RANGE (LIST spec -> -1))))
         ((AND (LISTP spec) (MEMBER '-> spec)) 
          (PARSE-RANGE spec))
         ((LISTP spec)
          (CONS '(:ENUM EQUALP) 
                (MAPCAR (LAMBDA (x) 
                          (IF (INTEGERP x) x (BB-STRING-OF x)))
                        spec)))
         (T 
          (err+
           problem "You passed an unrecognized specification,~A, for creating"
           indent "a table."
           advice "Value must be either:"
           indent "> $, to indicate a hash."
           indent "> an integer to indicate a dimension size."
           indent "> (from -> to) to indicate a range."
           indent "> (:enum x1 x2 ...) to indicate a specific set of labels."
           format-args spec
           )))))

(DEFUN Remove-degenerate-sublists (list)
 "(REMOVE-DEGENERATE-SUBLISTS list)
   - Returns modified list with sublists consisting of one element 
       changed to that element
   - EXAMPLE:
       (REMOVE-DEGENERATE-SUBLISTS '(1 2 (3 4) (5)))
             ==> '(1 2 (3 4) 5)
 "
   (IF (= (LENGTH list) 1)
       (FIRST list)
       (LOOP FOR item IN list
             COLLECT (IF (LISTP item)
                         (REMOVE-DEGENERATE-SUBLISTS item)
                         item))))

; ================ STRING-LABEL ===================

(DEFUN String-label (string)
  (IF (NOT (STRINGP string))
      (ERROR "STRING-LABEL was expecting a string, not ~A" string))
  (LET ((max-length 10))
    (IF (> (LENGTH string) max-length)
        (CONCATENATE 'STRING (SUBSEQ string 0 10) "...")
        string)))

; ================ BB-STRING-OF ===================

(DEFUN BB-String-of (entity &KEY lower-case upper-case)
  (IF (TYPEP entity 'RATIO)
      (SETF entity (FLOAT entity)))
  (LET ((result
          (IF (FRAMEP entity)
              (fname entity)
              (LET ((*print-pretty* NIL)) (FORMAT NIL "~A" entity)))))
    (COND
       (upper-case (STRING-UPCASE result))
       (lower-case (STRING-DOWNCASE result))
       (T result))))

; ================ BB-CHOOSE-FROM ===================

(DEFUN BB-CHOOSE-FROM (sequence)
  "Selects at random an element from a string, vector, or list"
  (LET ((choice (elt sequence (random (length sequence)))))
    (IF (STRINGP sequence)
        (STRING choice)
        choice)))

; ================ BB-FIT ===================

(DEFUN BB-Fit (pre-string columns 
       &KEY (with " ") if-too-big (flush-left T) center? flush-right)

    (LET* ((padding 
              (IF (= (LENGTH with) 1)
                  (CHAR with 0)
                (ERR+ problem "The value '~A' is not valid for the WITH keyword."
                      advice  
                      "Replace '~A' a one-character long string"
                      help~A
                      format-args
                      with with *df-name*)
                ))
           (too-big-char
              (COND 
                 ((NULL if-too-big) NIL)
                 ((= (LENGTH if-too-big) 1) (CHAR if-too-big 0))
                 (T 
                   (ERR+
                     problem
                     "The value '~A' is not valid for the IF-TOO-BIG keyword."
                     advice  
                     "Replace '~A' with one-character long string."
                     help~A
                     FORMAT-ARGS if-too-big if-too-big *df-name*))))
           (full-string (BB-STRING-OF pre-string))
           (length (LENGTH full-string)))

     (COND
        ((> length columns)
           (COND
             (if-too-big 
                 (MAKE-STRING columns :INITIAL-ELEMENT too-big-char))
             (center? 
                 (LET ((start (FLOOR (/ (- length columns) 2))))
                    (SUBSEQ full-string start (+ start columns))))
             (flush-right (SUBSEQ full-string (- length columns)))
          ;  (T (SUBSEQ full-string 0 length))))
             (T (SUBSEQ full-string 0 columns))))
        ((= length columns) full-string)
        (flush-left
           (LET ((format-string (S+ "~" (BB-STRING-OF columns) ",,,'" padding "A")))
              (FORMAT NIL format-string full-string)))
        (flush-right
           (LET ((format-string (S+ "~" (BB-STRING-OF columns) ",,,'" padding "@A")))
              (FORMAT NIL format-string full-string)))
        (center?
           (LET ((format-string (S+ "~" (BB-STRING-OF columns) ",,,'" padding ":@<~A~>")))
              (FORMAT NIL format-string full-string))))))

(DEFUN String-or-symbol? (x) 
   (OR (STRINGP x) (SYMBOLP x)))

; ================ BB-GENE-NAMED ===================

(DEFUN BB-Gene-named (gene-name &OPTIONAL (search-space *all-organisms*))
  "Converts string name into gene frame"
  (LET ((result
     (COND
        ((TYPEP gene-name 'Gene) gene-name)
        ((TYPEP gene-name 'Protein) (SLOTV gene-name #$Gene))
        ((SYMBOLP gene-name)
            (FORWARD-FUNCALL 'BB-GENE-NAMED (SYMBOL-NAME gene-name)))
        ((FIND #\. gene-name)
          (LET ((frame (FRAME-FNAMED gene-name NIL)))
            (COND 
               ((AND frame (IS-GENE? frame)) frame)
               ((AND frame (IS-PROTEIN? frame)) (SLOTV frame #$gene)) 
               (T NIL))))
        (T (LOOP FOR organism IN (ENSURE-LIST search-space)
                 AS prefix = (SLOTV organism #$Organism-prefix )
                 AS frame = (FRAME-FNAMED (S+ prefix gene-name))
                 WHEN (AND frame (IS-GENE? frame))
                   DO (RETURN frame)
                 WHEN (AND frame (IS-PROTEIN? frame))
                   DO (RETURN (SLOTV frame #$gene )))))))
     (IF (OR (NOT search-space) (NOT result)
             (MEMBER (SLOTV result #$Organism)
                     (ENSURE-LIST search-space)))
         result)))

(DEFUN BB-Gene-named-general (gene-name organism)
  "Converts string name into gene frame"
  (OR (BB-GENE-NAMED gene-name organism)
      ; *** gene-if-short-name ***
      (LOOP FOR g IN (SLOTV organism #$Genes)
            AS gene-symbol = (SLOTV g #$Genetic-name)
            WHEN (AND gene-symbol
                      (SEARCH gene-name gene-symbol
                        :TEST 'EQUALP))
              DO (RETURN g))))

; ================ BB-IS-SUBSET? ===================

(DEFUN BB-IS-SUBSET? (set1 set2 &KEY case-sensitive)
  (LET ((test (IF case-sensitive 'EQUAL 'BB-SAME)))
  (FLET ((Letters-of (string)
           (LOOP FOR char ACROSS string
                 COLLECT (STRING char))))
    (COND
       ((LISTP set2) NIL)
       ((NOT (STRINGP set2))
          (ERROR "In BB-IS-SUBSET? set2 must be a set or a string, not '~A'"
                set2))
       ((LISTP set1) 
          (SETF set2 (LETTERS-OF set2)))
       ((STRINGP set1)
          (SETF set1 (LETTERS-OF set1))
          (SETF set2 (LETTERS-OF set2)))
       (T (ERROR "In BB-IS-SUBSET?, unless set2 is a list, set1 must be either a string or list, not '~A'" set1)))
   (SUBSETP (ENSURE-LIST set1) (ENSURE-LIST set2) :TEST test)
)))


; ================ BB-SAME ===================

(DEFUN BB-Same (x y &KEY Case-sensitive (safety T))
   ; safety = NIL removes protection from EQUALP acting on frames
  (FLET ((String-or-symbol? (x) (OR (STRINGP x) (SYMBOLP x))))
    (COND
     ((EQL x y) t)     ; Both are nonaggregates or frames
     ((AND (NUMBERP x) (NUMBERP y)) (= x y))
     ((AND (STRINGP x) (STRINGP y))
      (FUNCALL (IF case-sensitive 'STRING= 'STRING-EQUAL) x y))
     ((AND (CHARACTERP x) (CHARACTERP y))
      (FUNCALL (IF case-sensitive 'CHAR= 'CHAR-EQUAL) x y))
     ((AND (STRING-OR-SYMBOL? x) (STRING-OR-SYMBOL? y))
                          ; Both are strings or symbols
      (STRING-EQUAL x y))
     ((AND (IsFRAME? x) (IsFRAME? y)) NIL)   ; Unequal frames
     ((OR (IsFRAME? x) (IsFRAME? y))        ; One frame
             (EQUALP (ANYTHING-STRINGLIKE-TO-STRING x) (ANYTHING-STRINGLIKE-TO-STRING y)))
     ((OR (NUMBERP x) (NUMBERP y))        ; One number
         (LET ((n1 (BB-CONVERT x 'Number :IF-NO-CONVERSION? T :IF-CONVERSION-ERROR? NIL))
               (n2 (BB-CONVERT y 'Number :IF-NO-CONVERSION? T :IF-CONVERSION-ERROR? NIL)))
            (AND n1 n2 (= n1 n2))))
     ((AND (OR (AND (LISTP x) (LISTP y))
               (AND (VECTORP x) (VECTORP y)))
           (= (LENGTH x) (LENGTH y)))
                          ; Compare list/vector item by item
      (block exit
        (map 
         nil 
         (lambda (i j) 
           ;; fixed bug, bb-same was being called without keywords
           (WHEN (NOT (utils::FORWARD-FUNCALL 
                       'BB-SAME i j 
                       :case-sensitive case-sensitive 
                       :safety safety))
             (RETURN-from exit nil)))
         x y)
        t))
     (case-sensitive (EQUAL x y))
     ((NOT safety) (EQUALP x y))
     (T NIL))))



; ================ GENES-OF-CONTIG ===================

(DEFUN Genes-of-contig (contig)
  "Returns as a list the genes of a contig or replicon"
  (LET ((genes (SLOTV contig #$genes-sorted-by-position)))
    (IF genes (ARRAY-TO-LIST genes))
))

; ================ ONLY-NUMERALS ===================

(DEFUN Only-numerals (string)
  "Returns T if string consists only of numerals"
  (IF (NOT (STRINGP string))
      (ERROR (S+ "INTERNAL ERROR! (Please report)" *newline*
                 "ONLY-NUMERALS expected string, not '~A'")
             string))
  (LOOP FOR num ACROSS "0123456789"
        DO (SETF string (REMOVE num string :TEST 'EQUAL))
           (IF (EQUAL string "")
               (RETURN T))))

; ============= SEQ-TYPE-OF ==========

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant ep-flag #b000001)
  (defconstant  p-flag #b000010)
  (defconstant ed-flag #b000100)
  (defconstant  d-flag #b001000)
  (defconstant er-flag #b010000)
  (defconstant  r-flag #b100000)
  )

(defparameter *bbl-sequence-char-properties*
  #.(let ((a (make-array 
              (list 128)
              :element-type '(unsigned-byte 8)
              :initial-element 0
              )))
      (DECLARE (type (simple-array (unsigned-byte 8) (128)) a))

      (LABELS 
         ((set-bit (ch bit &aux (index (char-code ch)))
             (DECLARE (fixnum bit index))
             (SETF (AREF a index) (LOGIOR (AREF a index) bit)))

          (Set-bit-for-alphabet (alphabet bit)
            (LOOP FOR ch ACROSS alphabet 
                  DO (SET-BIT (CHAR-DOWNCASE ch) bit)
                     (SET-BIT (CHAR-UPCASE ch) bit)
                       )))
        (set-bit-for-alphabet *legal-extended-protein-chars* ep-flag) 
        (set-bit-for-alphabet *legal-protein-chars* p-flag)   
        (set-bit-for-alphabet *legal-extended-dna-chars* ed-flag) 
        (set-bit-for-alphabet *legal-dna-chars* d-flag)  
        (set-bit-for-alphabet *legal-extended-rna-chars* er-flag) 
        (set-bit-for-alphabet *legal-rna-chars* r-flag)
        a
        )))

(DEFUN BB-Sequence-type-of (seq extended &KEY (dna-threshold 0.5))
  (DECLARE (optimize (speed 3) (safety 0) (debug 0)))
  (LET ((flags #xFF) 
        (a *bbl-sequence-char-properties*))
     (DECLARE (FIXNUM flags))
     (DECLARE (type (Simple-array (unsigned-byte 8) (128)) a))
     (MAP nil
         (lambda (ch) 
            (SETQ flags (LOGAND flags (AREF a (CHAR-CODE ch)))))
         seq)
       (IF (= (LENGTH seq) 0)
           (RETURN-FROM BB-SEQUENCE-TYPE-OF NIL))
    (MACROLET ((flagon? (bit) `(PLUSP (LOGAND flags (THE FIXNUM ,bit)))))

      (IF extended
          (COND
            ((AND (FLAGON? ed-flag) (NOT (FLAGON? ep-flag))) 'DNA)
            ((AND (FLAGON? er-flag) (NOT (FLAGON? ed-flag))) 'RNA)
            ((AND (FLAGON? ep-flag) (FLAGON? ed-flag))
              (LET ((count 0))
                (DECLARE (FIXNUM count))
                (MAP 
                  NIL
                  (LAMBDA (ch) 
                    (WHEN (PLUSP (LOGAND (AREF a (CHAR-CODE ch)) d-flag))
                      (INCF count)))
                  seq)
                (IF (>= (/ count (LENGTH seq)) dna-threshold) 'DNA 'PROTEIN)
               ))
            ((FLAGON? ep-flag) 'PROTEIN)
            (T NIL))
        (COND
          ((FLAGON? d-flag) 'DNA)
          ((FLAGON? r-flag) 'RNA)
          ((FLAGON? p-flag) 'PROTEIN)
          (T NIL)
         )))))

; =========================

(DEFUN Verify-number (x description)
  (UNLESS (NUMBERP x)
    (ERR+ problem "~A is not a number. You tried to pass ~S.~&"
          format-args description x)
    )
  x)

; ******* EXPORTED BASIC LANGUAGE FUNCTIONS *******

(DEFUN Last-N (n list)
 "(LAST-N n list)
  - Extracts last n elements from a list
 "
  (NREVERSE (FIRST-N n (REVERSE list))))


; ================== EXCLUSIVE-OR ======================

(defun xor2 (x y) (or (and x (not y)) (and (not x) y)))

(DEFUN Exclusive-or (&REST args)
  "Performs exclusive-or on each argument or arguments within a list"
  (COND
   ((AND (= (LENGTH args) 1) (LISTP (biolisp::FIRST args)))
    (reduce 'xor2 (biolisp::FIRST args))
    #+oops
    (APPLY 'EXCLUSIVE-OR (biolisp::FIRST args))
    )
   ((> (LENGTH args) 0)  
    (LET ((first-T (POSITION-IF-NOT 'NULL args)))
      (AND first-T (EVERY 'NULL (NthCDR (1+ first-T) args)))))
   (T 
    (ERR+ "Badly formed argument list~&")
    )))

; ================== EXTENTS-OF-COMPLEXT-LIST ======================

(DEFUN Extents-of-complex-list (list)
   (IF (NOT (LISTP list))
       (ERROR "Input to EXTENTS-OF-COMPLEX-LIST must be a list, not '~A'"
           list))
   (LOOP WITH min-extent = *big-number*
         WITH max-extent = 0
         FOR item IN list
         DO (IF (LISTP item)
                (SETF max-extent (MAX max-extent (LENGTH item)))
                (SETF min-extent 0))
         FINALLY (RETURN (LIST min-extent max-extent)))
 )

; ================== EITHER ======================

(DEFMACRO Either (&REST args)
  "Returns T if any of the arguments (separated by OR) are true"
  (IF (= (LENGTH args) 0)
      (ERR+ problem "EITHER requires at least one argument")
    )
  (LET ((new-args (LIST (POP args))))
    (LOOP UNTIL (NOT args)
      AS arg1 = (POP args)
      AS arg2 = (POP args)
      DO (IF (NOT (AND (SYMBOLP arg1) (SYMBOL= arg1 'OR)))
             (ERR+ "All arguments to EITHER after the first"
                   "must be preceded by OR")
           )
      (SETF new-args (APPEND new-args (LIST arg2))))
    `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
	  (OR ,@new-args))))

; ================== BOTH ======================

(DEFMACRO Both (&REST args)
  "Returns T if all of the arguments (separated by AND) are true"
  (IF (= (LENGTH args) 0)
      (ERR+ problem "BOTH requires at least one argument")
    )
  (LET ((new-args (LIST (POP args))))
    (LOOP UNTIL (NOT args)
      AS arg1 = (POP args)
      AS arg2 = (POP args)
      DO (IF (NOT (AND (SYMBOLP arg1) (SYMBOL= arg1 'AND)))
             (ERR+ problem 
                   "All arguments to BOTH after the first"
                   "must be preceded by AND")
           )
      (SETF new-args (APPEND new-args (LIST arg2))))
    `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
	  (AND ,@new-args))))

; =============== CONDITION ===================

(DEFMACRO BBL::Condition (&REST args)
  "Directs the logical flow based on meeting conditions"
  `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
	  (COND ,@args)))


;;; ================ INTERLEAVE ====================
 

(DEFUN Interleave-aux (lists truncate &KEY simplify)

  ;; proposed fix for null lists 
  
  (cond
   ((every 'null lists) (return-from interleave-aux nil))
   ((some 'null lists) 
    (if truncate
        (return-from interleave-aux nil)
      (ERROR "Lists to interleave are of unequal sizes (One is empty).")))
   (t nil)
   )

   (LET ((list-length
           (LOOP
            FOR item IN lists
            WITH min-length = 0
            AS length = (IF (LISTP item)
                            (LENGTH item)
                          0)
            DO (IF (AND (> length 0)
                        (> min-length 0)
                        (NOT truncate)
                        (NOT (= length min-length)))
                   (ERROR "Lists to interleave are of unequal sizes: ~A and ~A"
                          min-length length))
            (SETF min-length
                  (COND
                   ((= length 0) min-length)
                   ((> min-length 0) 
                    (MIN min-length length))
                   (T length)))
            FINALLY (RETURN min-length))))
       (IF (= list-length 0)
           (ERROR "No lists to interleave!"))
       (LOOP FOR index FROM 0 BELOW list-length
             AS new-list 
               = (LOOP FOR list IN lists
                       AS item 
                         = (IF (LISTP list)
                               (Nth index list)
                               list)
                       COLLECT item)
             COLLECT
               (IF simplify (FLATTEN new-list) new-list))))

(DEFMACRO Interleave (&REST args)
  #.(one-string-nl
     "Takes given lists and produces single list."
     "E.g., (interleave '(1 2 3) '(4 5 6)) --> ((1 4) (2 5) (3 6))")
  (LET* ((truncate NIL)
         (simplify NIL)
         (lists 
          (REVERSE
           (LOOP FOR arg IN (REVERSE args)
                 WITH list-found = NIL
                 DO (IF (NOT list-found)
                        (COND
                         ((AND (NOT truncate) 
                               (SYMBOL= arg 'TRUNCATE))
                          (SETF truncate T))
                         ((AND (NOT simplify) 
                               (SYMBOL= arg 'SIMPLIFY))
                          (SETF simplify T))
                         (T (SETF list-found T))))
                 WHEN list-found
                 COLLECT arg))))
    `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
	   (INTERLEAVE-aux (LIST ,@lists) ,truncate :SIMPLIFY ,simplify))))

;;; ================ LIMITED-VERSION-OF ====================

(DEFUN Limited-version-of (x &KEY limit)
  (LET ((string-limit 10)
        (list-limit 3))
  (COND
     ((STRINGP x)
        (IF (> (LENGTH x) (OR limit string-limit))
            (S+ (SUBSEQ x 0 (OR limit string-limit)) "...")
            x))
     ((LISTP x)
        (IF (> (LENGTH x) (OR limit list-limit))
            (APPEND (SUBSEQ x 0 (OR limit list-limit)) (LIST (INTERN "...")))
            x))
     (T x))
 ))

;;; ================ MAX-OF MIN-OF SUM-OF PRODUCT-OF ====================

(defun xop-of (identity lisp-operator bbl-operator &rest args)
  (let ((len (length args)))
    (cond
     ((zerop len) 
      (if identity identity (error "Zero arguments not allowed!")))
     ((= len 1) 
      (let ((only-arg (lisp:first args)))
        (cond
         ;; make sure we don't use APPLY on a long list, so special
         ;; case short lists and loop over the elements of long lists
         ((listp only-arg)
          (cond
           ((null only-arg) identity)
           ((null (cdr only-arg)) 
            (xop-of identity lisp-operator bbl-operator (lisp:first only-arg)))
           ((null (cddr only-arg))
            (apply 'xop-of identity lisp-operator bbl-operator only-arg))
           (t
            (let ((result 
                   (xop-of 
                    identity lisp-operator bbl-operator
                    (lisp:first only-arg) (lisp:second only-arg)
                    )))
              (loop for arg in (cddr only-arg) do
                    (setq 
                     result 
                     (xop-of identity lisp-operator bbl-operator result arg)
                     ))
              result
              ))))
         ((numberp only-arg) (funcall lisp-operator only-arg))
         (t 
          (error "Argument to ~A is not a number: ~A !" bbl-operator only-arg))
         )))
     ((= len 2) 
      (let ((arg1 (bio::first args))
            (arg2 (bio::second args)))
        (cond
         ((and (numberp arg1) (numberp arg2)) 
          (funcall lisp-operator arg1 arg2))
         ((and (numberp arg1) (listp arg2))
          (mapcar (lambda (x) (funcall lisp-operator arg1 x)) arg2))
         ((and (listp arg1) (numberp arg2))
          (mapcar (lambda (x) (funcall lisp-operator x arg2)) arg1))
         ((and (listp arg1) (listp arg2)) 
          (unless (= (length arg1) (length arg2))
            (error 
             "Cannot use ~A on 2 lists of numbers that aren't the same length!"
             bbl-operator
             ))
          (mapcar 
           (lambda (x y) (xop-of identity lisp-operator bbl-operator x y))
           arg1 arg2
           ))
         (t 
          (cond
           ((and (not (numberp arg1)) (not (listp arg1)))
            (error 
             "First argument to ~A is not valid: ~A !" bbl-operator arg1))
           (t 
            (error 
             "Second argument to ~A is not valid: ~A !" bbl-operator arg2
             )))))))
     (t
      (cond
       ((every 'numberp args) (reduce lisp-operator args))
       (t 
        ;; operate on first two arguments
        (let ((result 
               (xop-of 
                identity lisp-operator bbl-operator
                (lisp:first args) (lisp:second args)
                )))
          ;; combine result of first two arguments with next argument,
          ;; and so on
          (loop for arg in (lisp:cddr args) do
                (setq 
                 result
                 (xop-of identity lisp-operator bbl-operator result arg)
                 ))
          result
          )
        ))))))

#|
(defun sum-of (&rest args)
  (apply 'xop-of 0 '+ 'sum-of args))

(defun product-of (&rest args)
  (apply 'xop-of 1 '* 'product-of args))

(defun difference-of (&rest args)
  "Returns the difference between two numbers"
  (apply 'xop-of 0 '- 'difference-of args))

(defun quotient-of (&rest args)
  "Finds the quotient of two numbers"
  (apply 'xop-of 1 'bbl-one-or-two-arg-quotient 'quotient-of args))
|#

(DEFMACRO xop-of-macro (function symbol identity &REST args)
  "Generalized macro for SUM-OF et al"
  (IF (= (LENGTH args) 0)
      (ERR+ problem "DIFFERENCE-OF requires at least one argument")
    )
  (LET ((new-args (LIST (POP args))))
    (LOOP UNTIL (NOT args)
      AS arg1 = (POP args)
      DO (IF (NOT (AND (SYMBOLP arg1) (SYMBOL= arg1 symbol)))
             (SETF new-args (APPEND new-args (LIST arg1)))))
    `(APPLY 'XOP-OF ,identity ',symbol ,function (LIST ,@new-args))))

(DEFMACRO SUM-OF (&REST args)
  "Adds one or more numbers to the first number (or adds a list)"
  `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
      (XOP-OF-MACRO 'SUM-OF + 0 ,@args)
))

(DEFMACRO DIFFERENCE-OF (&REST args)
  "Subtracts one or more numbers from the first number (or subtracts a list)"
  `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
     (XOP-OF-MACRO 'DIFFERENCE-OF - 0 ,@args)
))

(DEFMACRO PRODUCT-OF (&REST args)
  "Multiplies one or more numbers with the first number (or multiplies a list)"
  `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
     (XOP-OF-MACRO 'PRODUCT-OF * 1 ,@args)
))

(DEFMACRO QUOTIENT-OF (&REST args)
  "Quotient of sucessive numbers (a / b) / c) ..."
  `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
     (XOP-OF-MACRO 'QUOTIENT-OF / 1 ,@args)
))


(defun bbl-one-or-two-arg-quotient (x &optional y)
  (if (null y) 
      (bbl-one-or-two-arg-quotient 1.0 x)
    (let ((result (/ x y)))
      (typecase result
        (ratio (float result))
        (otherwise result)
        ))))

(defun max-of (&rest args)
  (apply 'xop-of nil 'max 'max-of args))

(defun min-of (&rest args)
  (apply 'xop-of nil 'min 'min-of args))

(defun bbl::abs (arg) 
  "Returns the absolute value of a number."
  (if (listp arg) (mapcar 'lisp:abs arg) (lisp:abs arg)))

(defun bbl::mod (number divisor) 
  "Returns number modulus divisor"
  (COND
     ((AND (LISTP number) (LISTP divisor))
        (MAPCAR (LAMBDA (n d) (Lisp:MOD n d)) number divisor))
     ((LISTP number)
        (MAPCAR (LAMBDA (n) (Lisp:MOD n divisor)) number))
     ((LISTP divisor)
        (MAPCAR (LAMBDA (d) (Lisp:MOD number d)) divisor))
     (T (Lisp:MOD number divisor)))
)

(defun bbl::exp (exponent) 
  "Returns e raised to the given power"
  (if (listp exponent) 
      (mapcar 'lisp:exp exponent) 
      (lisp:exp exponent)))


(defun bbl::log (n &optional b)
  "Returns the value of log N base e...if b supplied, it returns the value of log N base B"
  (if (listp n) 
      (mapcar 
       (lambda (m) (if b (lisp:log m b) (lisp:log m)))
       n)
    (if b (lisp:log n b) (lisp:log n))
    ))

(defun bbl::sqrt (number) 
  "Returns the square root of a number."
  (if (listp number) (mapcar 'lisp:sqrt number) (lisp:sqrt number)))

(defun negation-of (arg)
  "Returns the negation of a number or each of a list of numbers"
  (if (listp arg) (mapcar 'lisp:- arg) (lisp:- arg)))

;;; ========================== ORDER =====================================

(DEFMACRO Order (&REST args)
  "Returns true if arguments are in order specified by embedded operators"
  (IF (< (LENGTH args) 3)
      (ERROR "Insufficient arguments given to ORDER"))
  (LET ((operators '(< <= = >= > SAME GREATER-THAN LESS-THAN EQUAL EQUALP)))
    `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
      (AND
       ,@(LOOP WHILE (> (LENGTH args) 1)
               AS length = (LENGTH args)
               AS x = (POP args)
               AS operator = (POP args)
               AS y = (FIRST args)
               DO (IF (< length 1)
                      (ERROR "Insufficient arguments"))
               COLLECT
                  (IF (MEMBER operator operators :TEST 'SYMBOL=)
                     `(,operator ,x ,y)
                     `(FUNCALL ,operator ,x ,y)))))))

;;; ====================== ORTHOLOGS-KNOWN? =========================


(DEFUN Orthologs-known? (organism-or-contig)
  "Determines whether organism (or organism of contig) is represented in crossblast table"
  (DECLARE (SPECIAL cl-user::*blast-lookup-database*))
  (CASE (EXISTS cl-user::*blast-lookup-database*)
     (:SEED
        (TYPECASE organism-or-contig
           (Organism
              (IF (MEMBER organism-or-contig *all-organisms*) T NIL))
           (Contiguous-sequence
              (IF (MEMBER (SLOTV organism-or-contig #$organism) *all-organisms*) T NIL))
           (List
              (EVERY 'IDENTITY (MAPCAR (LAMBDA (x) (FORWARD-FUNCALL 'orthologs-known? x)) organism-or-contig)))
           ((OR Gene Protein)
              (FORWARD-FUNCALL 'ORTHOLOGS-KNOWN? (SLOTV organism-or-contig #$organism)))
           (OTHERWISE NIL)))
     (:CROSS-BLAST
        (TYPECASE organism-or-contig
           (Organism
              (IF (MEMBER organism-or-contig *organisms-with-orthologs*) T NIL))
           (Contiguous-sequence
              (IF (MEMBER (SLOTV organism-or-contig #$organism) *organisms-with-orthologs*) T NIL))
           (List
              (EVERY 'IDENTITY (MAPCAR (LAMBDA (x) (FORWARD-FUNCALL 'orthologs-known? x)) organism-or-contig)))
           ((OR Gene Protein)
              (FORWARD-FUNCALL 'ORTHOLOGS-KNOWN? (SLOTV organism-or-contig #$organism)))
           (OTHERWISE NIL)))
     (OTHERWISE NIL)
	 )
)


;;; ========================== PROGRAM =====================================

(DEFMACRO Program (&REST forms)
  "Bundles different commands into a single command"
  `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
      (PROGN ,@forms)))

;;; ========================== SHUTDOWN-STATS ==============================

(DEFUN shutdown-stats (&OPTIONAL lines-set?)
   (IF lines-set?
       (PROGN
         (excl::GC)
         (PRINT (wb::ROOM T))
         (utils::forward-funcall 'wb::UMU)
         (USERS))
       (PRINT "Set maximum lines in preferences to a large number?")))


;;; ========================== STRING-JOIN+ ==============================

(DEFUN String-join+ (string-list &optional (sep #\Space))
  "Like STRING-JOIN except that it converts items to strings"
  (string+-join string-list sep)
  )

;;; ================= INTERNAL PSSM-FUNCTIONS ============================

;; RESULTS must be a 2-element vector of single floats declared as 
;; (simple-array single-float (2)).  The two results (forwards and backwards)
;; are returned in the two elements of RESULTS.  (The backwards result
;; is only computed if both-strands? is T)

;; SEQ must be a simple string 
;; START and END are indices into SEQ which are inclusive

;; Analyzes the subsequence of SEQ from START to END inclusive, forwards
;; (and backwards if both-strands?), scoring the characters of the subsequence keeping
;; a running product of each character's score (presumably as a probability).
;; INFO-LIST is a boolean list, one item for each element of the window.
;; If the corresponding element of INFO-LIST is non-NIL, the character is 
;; scored, otherwise it is not.
;; (Note: if INFO-LIST is all NIL the score will be 1.0)
;; both-strands? should be true ifff this is a DNA sequence and the reverse
;; sequence score is to be computed.


(DEFUN Really-really-fast-score
       (results seq start end main-table info-list both-strands?)
  (DECLARE ((SIMPLE-ARRAY SINGLE-FLOAT (2)) results))
  (DECLARE (SIMPLE-STRING seq))
  (DECLARE (FIXNUM start end))
  (DECLARE ((SIMPLE-ARRAY SINGLE-FLOAT (* *)) main-table))
  (DECLARE (OPTIMIZE (speed 3) (safety 0) (debug 0)))
  (SETF (AREF results 0) 1.0)
  (SETF (AREF results 1) 1.0)
  (IF (NOT both-strands?)
      (PROGN
        (SETF (AREF results 1) -1.0)
        (LOOP 
          FOR forward FIXNUM FROM (TFIX (1- start)) TO (TFIX (1- end))
          FOR pos FIXNUM FROM 1 
          FOR info-rich IN info-list
          WHEN info-rich
            DO (SETF (AREF results 0) 
                       (* (AREF results 0) 
                          (AREF main-table pos (CHAR-CODE (SCHAR seq forward)))))))
      (LOOP 
        WITH backward-ch = nil
        FOR forward FIXNUM FROM (TFIX (1- start)) TO (TFIX (1- end))
        FOR backward FIXNUM FROM (TFIX (1- end)) DOWNTO (TFIX (1- start))
        FOR pos FIXNUM from 1 
        FOR info-rich IN info-list
        WHEN info-rich
          DO (SETF (AREF results 0) 
                     (* (AREF results 0) 
                        (AREF main-table pos (CHAR-CODE (SCHAR seq forward)))
                      ))
             (SETF backward-ch
                (CASE (SCHAR seq backward)
                   (#\a #\t) (#\A #\T)
                   (#\c #\g) (#\C #\G)
                   (#\g #\c) (#\G #\C)
                   (#\t #\a) (#\T #\A)
				   (OTHERWISE
				     (WARN (S+ "Your sequence has a nonstandard "
					            "nucleotide in it: ~A") 
							(SCHAR seq backward))
					 (SCHAR seq backward))
                 ))
             (SETF (AREF results 1) 
                     (* (AREF results 1) 
                        (AREF main-table pos (CHAR-CODE backward-ch))
              )))))

(DEFUN pssm-body 
     (sequence-set pssm optimized-pssm info-list threshold both-strands?)
  (LET ((window-size (LENGTH (GARRAY-COMPONENT-INDICES pssm)))
        (window-scores (MAKE-ARRAY 2 :ELEMENT-TYPE 'single-float))
        (fthresh (FLOAT threshold 0.0))
        (results NIL))
    (DECLARE (FIXNUM window-size))
    (DECLARE (SINGLE-FLOAT fthresh))
    (UNLESS (TYPEP window-scores '(SIMPLE-ARRAY single-float (2)))
      (ERROR "Internal error: MAKE-ARRAY not working as we want!"))

    (LOOP
       FOR item IN sequence-set
       AS label = (IF (LISTP item) 
                      (biolisp::FIRST item) 
                      item)
       AS sequence = 
          (COND
             ((LISTP item) (biolisp::SECOND item))
             ((IsFrame? item) (EXTRACT-SEQUENCE item))
             (T 
              (ERR+ problem "~A must be a frame or labeled sequence.~&"
                    format-args item)
              ))
       AS slen FIXNUM = (TFIX (LENGTH sequence))
       AS nwindows FIXNUM = (TFIX (1+ (TFIX (- slen window-size))))
       AS extent FIXNUM = (TFIX (1- window-size))
       DO (LOOP FOR start FIXNUM FROM 1 TO nwindows
                AS end FIXNUM = (TFIX (+ start extent))
                WHEN (<= window-size slen)
                  DO (REALLY-REALLY-FAST-SCORE
                        window-scores sequence start end 
                        optimized-pssm info-list both-strands?)
                     (WHEN (> (AREF window-scores 0) fthresh)
                        (PUSH (LIST label start :forward 
                                  (AREF window-scores 0)) results))
                     (WHEN (> (AREF window-scores 1) fthresh)
                        (PUSH (LIST label start :backward 
                                  (AREF window-scores 1)) results))
                ))
    (SORT results '> :KEY 'biolisp::FOURTH)
    ))

(defmacro bbl-code (code) "Execute code entered in lisp form" 
     (LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
       code))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-vpl-popup-window-and-display 
       (object &key (type nil) (width "800px") (height "800px") (relative-p 0))
  (when (called-from-vpl)
    (progn 
      (forward-package-funcall 
       :vpl :create-vpl-popup-window-and-display
       object :type type :width width :height height :relative-p relative-p
       )
      t
      )))


(DEFUN Show-popup-window (url)
  (FORWARD-PACKAGE-FUNCALL :nvpl :show-vpl-popup-url-window url
   :relative-p 1
   :location "yes"
   :directories "yes"
   :status "yes"
   :menubar "yes"
   :width "inherit"
   :height "inherit"
   ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN remove-html (text)
  ;; Simple-minded method, removes abc<...>def -> abcdef
   (LET* ((first-and-later-groups
             (FORWARD-FUNCALL 'MATCH-OF-PATTERN-ONE-ITEM-aux
			    "^([^<>]*)(.*)$" text 1 '(NIL NIL NIL T) NIL NIL NIL T))
		  (first-group (FIRST first-and-later-groups))
          (later-groups (SECOND first-and-later-groups))
          (parsed-later-groups
             (MAPCAR 'SECOND 
                (FORWARD-FUNCALL 'MATCH-OF-PATTERN-ONE-ITEM-aux
                     "(<[^<>]*>)+([^<>]+)" later-groups *unprovided+*
					 '(NIL NIL NIL T) NIL NIL NIL T)))
         )
      (FORWARD-FUNCALL 'BB-JOIN 
	      (LIST first-group parsed-later-groups) :AS-STRING T)
  ))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

(defun simple-view-sequence (contig &key (from 1) (to (+ from 200)))
  (when (eq #$organism (#^isA contig))
    (setq contig (first (#^contiguous-sequences contig))))
  (wb::make-url 
   :path (format nil "/ajax/seqview.html?PKG=~A&CONTIG=~A&FROM=~D&TO=~D" 
                 wb::*sessionid* (fname contig) from to)
   :display-string "View sequence" :target "_blank"
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter valid-when-value-of-flags '(:case-sensitive))

(defmacro when-value-of (object &body body &AUX (test 'BB-SAME))
  (multiple-value-bind (real-body flags)
      (when-value-of-body-and-flags body)
    (when (member :case-sensitive flags :test 'utils::symbol=)
      (setq test 'equal))
    (multiple-value-bind (is-clauses otherwise-clause)
        (when-value-of-clauses real-body)
      (let ((object-symbol (gensym "OBJECT-")))
        `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
          (let ((,object-symbol ,object))
           (cond
            ,@(mapcar 
               (lambda (clause) 
                 (multiple-value-bind (values actions)
                     (when-value-of-is-values-and-actions clause)
                   (if (= (length values) 1)
                       `((,test ,object-symbol ,(first values)) ,@actions)
                     `((or 
                        ,@(mapcar 
                           (lambda (value) `(,test ,object-symbol ,value))
                           values 
                           ))
                       ,@actions
                       ))))
               is-clauses
               )
            ,@(when otherwise-clause 
                (let ((actions 
                       (when-value-of-otherwise-actions otherwise-clause)
                       ))
                  `((t ,@actions))
                  )))))))))

(defun when-value-of-body-and-flags (body)
  (let ((rbody (reverse body))
        (flags nil))
    (loop 
     while t 
     do 
     (if (member (first rbody) valid-when-value-of-flags :test 'symbol=)
         (push (pop rbody) flags)
       (return nil)
       ))
    (values (reverse rbody) (reverse flags))
    ))

(defun when-value-of-clauses (real-body)
  (let ((opos (position :otherwise real-body :test 'symbol=)))
    (if (null opos)
        (values (collect-is-clauses real-body) nil)
      (values 
       (collect-is-clauses (subseq real-body 0 opos))
       (subseq real-body opos)
       ))))

(defun collect-is-clauses (body)
  (cond
   ((null body) nil)
   ((unless (symbol= :is (first body)) 
      (error "Invalid syntax in when-value-of form (no IS after object): ~S"
             body)))
   (t 
    (loop 
     with is-position = nil
     with is-clauses = nil
     while (setq is-position (position :is (cdr body) :test 'symbol=))
     do
     (push (subseq body 0 (1+ is-position)) is-clauses)
     (setq body (nthcdr (1+ is-position) body))
     finally 
     (progn 
       (push body is-clauses)
       (return (reverse is-clauses))
       )))))

(defun when-value-of-is-values-and-actions (clause &aux values actions)
  (let ((thenpos (position :then clause :test 'symbol=)))
    (unless (integerp thenpos)
      (error "Invalid WHEN-VALUE-OF clause, no then! ~S" clause))
    (setq actions (nthcdr (1+ thenpos) clause))
    (setq values (remove :or (subseq clause 1 thenpos) :test 'symbol=))
    (values values actions)
    ))

(defun when-value-of-otherwise-actions (otherwise-clause)
  (cdr otherwise-clause)
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro calc (token &optional (minus-action :always))
  (setq minus-action
        (cond
         ((symbol= minus-action :only-with-whitespace) 
          :only-with-whitespace)
         ((symbol= minus-action :always) :always)
         ((symbol= minus-action :unary-and-whitespace) :unary-and-whitespace)
         ((symbol= minus-action :always_to-) :always_to-)
         (t (error "Illegal flag for CALC: ~A" minus-action))
         ))
  (setq token
        (if (and (listp token) (eq 'quote (first token)))
            (second token)
          token
          ))
  (let* ((ts (formatn "~A" token))
         (tsl (length ts)))
    (labels ((edash (s) (format s "~A" "\\-"))
             (dash (s) (format s "~A" "-"))
             (escape-dash-if (condition s) 
               (if condition (edash s) (dash s))
               ))
      (when (and (not (eq minus-action :always))
                 (not (eq minus-action :always_to-))
                 (> tsl 1))
        (setq 
         ts 
         (with-output-to-string (s)
           (loop 
            for j from 0 below tsl 
            as ch = (char ts j)
            do 
            (if (char= ch #\-)
                (ecase minus-action 
                  (:only-with-whitespace 
                   (cond
                    ((zerop j)
                     (escape-dash-if (char/= (char ts (1+ j)) #\Space) s))
                    ((= j (1- tsl))
                     (escape-dash-if (char/= (char ts (1- j)) #\Space) s))
                    (t 
                     (escape-dash-if 
                      (or (char/= (char ts (1+ j)) #\Space) 
                          (char/= (char ts (1- j)) #\Space))
                      s
                      ))))
                  (:unary-and-whitespace 
                   (cond
                    ((zerop j) 
                     (escape-dash-if nil s))
                    (t
                     (escape-dash-if (alphanumericp (char ts (1- j))) s))
                    )))
              (format s "~A" (string ch))
              ))))))
    (let* ((lisp-expression (infix:string->prefix ts))
           (form
            (case minus-action
              (:always_to- 
               (if (listp lisp-expression) 
                   (maptree 
                    (lambda (x) 
                      (if (symbolp x) 
                          (let* ((p (symbol-package x))
                                 (s (symbol-name x))
                                 (new-name (substitute #\- #\_ s)))
                            (intern new-name p))
                        x
                        ))
                    lisp-expression
                    )
                 lisp-expression
                 ))
              (otherwise lisp-expression))
            ))
      `(LET ((*BBL-LEVEL* (INCREMENT-BBL-LEVEL))) ,form)
      )))

(defun bbi-seed-query (query &rest args)
  (apply 'forward-package-funcall :bio :seed-query (cons query args)))
  
(DEFUN Increment-bbl-level () 
  (IF (ZEROP *bbl-level*) 
      2 
      (THE FIXNUM (1+ *bbl-level*))))

        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro join (&rest args)
  "Joins strings or lists together"
  (LET* ((by-value nil)
         (by-position (position 'by args :test 'symbol=))
         (as-string-position (position 'as-string args :test 'symbol=))
         (as-list-position (position 'as-list args :test 'symbol=))
         (nil-ok-position (position 'nil-ok args :test 'symbol=))
         (as-list nil) 
         (as-string nil) 
         (nil-ok nil)
         (elements-to-remove nil))

    (when as-string-position
      (setq as-string t)
      (push as-string-position elements-to-remove))

    (when as-list-position
      (setq as-list t)
      (push as-list-position elements-to-remove))
    (when nil-ok-position
      (setq nil-ok t)
      (push nil-ok-position elements-to-remove))
      
    (error-if-more-than-one as-string as-list)

    (when by-position
      (setq by-value (nth (1+ by-position) args))
      (push by-position elements-to-remove)
      (push (1+ by-position) elements-to-remove))

    (setq args
          (loop for elem in args
                for j from 0
                when (not (member j elements-to-remove)) 
                collect elem
                ))
   
    `(let ((*bbl-level* (increment-bbl-level)))
       (bb-join 
        (list ,@args) 
        :by ,by-value :as-string ,as-string 
        :as-list ,as-list :nil-ok ,nil-ok
        ))))

(DEFUN macro-to-function-symbol (macro)
  (LET* ((package (SYMBOL-PACKAGE macro))
         ) 
  (IF (MACRO-FUNCTION macro)
      (bbi::INTERN (S+ "DF-FUNCTION-FOR-" macro) package)
      macro)
))


; ============ CHECK-BYTE-LIMIT =============
(DEFUN check-byte-limit (number-of-bytes i-mean-it function)
  (LET ((byte-warning-threshold 1e7)
        (byte-death-threshold 2e8)
        )
    (IF (AND (> number-of-bytes byte-warning-threshold)
             (NOT I-mean-it))
        (ERR+ 
         problem "Your request to ~A would generate an estimated ~A bytes. "
         advice  "If you really want to do this, repeat the command "
         indent  "but using the I-MEAN-IT flag."
         help~a
         format-args function number-of-bytes function))
    (IF (> number-of-bytes byte-death-threshold)
        (ERR+
         problem "Your request to ~A would generate an estimated ~A bytes. "
         advice  "We can't handle this."
         format-args function number-of-bytes))
))

; ============ BYTE-COUNT-OF =============
(DEFUN Byte-count-of (entity)
   (TYPECASE entity
      (String (LENGTH entity))
      (Labeled-sequence (LENGTH (LABELED-SEQUENCE-sequence entity)))
      (Number 8)
      (LIST (APPLY '+ (MAPCAR 'BYTE-COUNT-OF entity)))
      (OTHERWISE (LENGTH (ANYTHING-STRINGLIKE-TO-STRING entity))))
)      

; ============ REMOVE-nTH-CHARACTER =============
(DEFUN Remove-nth-character (n string)
  (COND
     ((< n 0) (ERROR "Bad n (~A) in REMOVE-nTH-CHARACTER" n))
     ((> n (1- (LENGTH string)))
         (ERROR "Position (~A) lies beyond end of string (~A) in REMOVE-nTH-CHARACTER"
                n string))
    ((= n 0) (SUBSEQ string 1))
    ((= n (1- (LENGTH string))) (SUBSEQ string 0 (1- (LENGTH string))))
    (T (CONCATENATE 'STRING (SUBSEQ string 0 n) (SUBSEQ string (1+ n) (LENGTH string)))))
)

; ====================== TRUE? =====================
(DEFUN Lies-between (x conditions &KEY (inclusive T) case-sensitive)
  "x = any entity that can be compared by LESS-THAN, SAME, and GREATER-THAN
   conditions = list of the form:
      ( [(a b) | c] [(a b) | c] ...)
      or simply (a b)
      where a is the lower boundary, b is the upper boundary, c is the target
   A condition is met if a < x < b or x = c
   If EXCLUSIVE is specified, then comparisons are done excluding the boundary values
   Function returns T if any of the conditions are met
   Examples:
     (LIES-BETWEEN 29 '(1 47)) --> T
     (LIES-BETWEEN 29 '((1 20)(30 47))) --> NIL
     (LIES-BETWEEN \"P\" '(\"A\" \"Z\")) --> T
     (LIES-BETWEEN 29 '((1 20) 29 (30 47))) --> T
     (LIES-BETWEEN 29 '(1 29) :INCLUSIVE NIL) --> NIL
     (LIES-BETWEEN 29 '((1 20) 29 (30 47)) :INCLUSIVE NIL) --> T
     (LIES-BETWEEN 29 29) --> ERROR
   "
   
   (IF (NOT (LISTP conditions))
       (ERROR "Argument to LIES-BETWEEN must be a list, not ~A" conditions))
   (LOOP FOR condition 
            IN (IF (IS-SIMPLE-LIST? conditions)
                   (LIST conditions)
                   conditions)
         DO
           (IF (LISTP condition)
               (PROGN
                  (IF (NOT (= (LENGTH condition) 2))
                      (ERROR (S+ "Boundary condition given to LIES-BETWEEN "
                                 "must have precisely two elements, not ~A") condition))
                  (LET* ((low-bound (FIRST condition))
                         (high-bound (SECOND condition))
                         )
                    (IF (FORWARD-FUNCALL 'BB-GREATER-THAN low-bound high-bound :CASE-SENSITIVE? case-sensitive)
                        (ERROR (S+ "Boundary conditions must be given low to high, "
                                   "but ~A > ~A") low-bound high-bound))
                    (IF (OR (AND (OR (FORWARD-FUNCALL 'BB-GREATER-THAN x low-bound :CASE-SENSITIVE? case-sensitive) 
                                     (AND inclusive (FORWARD-FUNCALL 'BB-SAME x low-bound :CASE-SENSITIVE case-sensitive))) 
                                 (OR (FORWARD-FUNCALL 'BB-LESS-THAN x high-bound :CASE-SENSITIVE? case-sensitive)
                                     (AND inclusive (FORWARD-FUNCALL 'BB-SAME x high-bound :CASE-SENSITIVE case-sensitive)))))
                        (RETURN T))))
                (IF (FORWARD-FUNCALL 'BB-SAME x condition :CASE-SENSITIVE case-sensitive) (RETURN T)))
        )
)

; ============================ TRUE? ============================
(DEFUN True?-aux-lists (each1 value1 operator each2 value2 )
  (LET* ((comparative-operators 
           '(GREATER-THAN LESS-THAN = < > MATCHED-BY-PATTERN))
         (set-operators '(CONTAINED-IN A-SUBSET-OF))
         (list1)(list2)
         (each-error-msg "EACH was specified for ~A, but ~A is not a list. It is '~A'")
         (between-error-msg 
            (S+ "When BETWEEN is specified as the operator of TRUE?, value2 must be a list, determining "
                 "the boundaries of the range, e.g. (0 1), not ~A"))
         )
  (FLET ((ENTITY1 (x)
            (SETF each1 'THE-ENTITY)
            x)
         (ENTITY2 (x)
            (SETF each2 'THE-ENTITY)
            x))

  (IF (AND (SYMBOL= each1 'EACH) (NOT (LISTP value1)))
      (ERROR each-error-msg 'VALUE1 'VALUE1 value1))
  (IF (AND (SYMBOL= each2 'EACH) (NOT (LISTP value2)))
      (ERROR each-error-msg 'VALUE2 'VALUE2 value2))

  (SETF list1
     (COND
        ((SYMBOL= each1 'EACH) value1)
        ((SYMBOL= each1 'THE-ENTITY) (LIST value1))))
  (SETF list2
     (COND
        ((SYMBOL= each2 'EACH) value2)
        ((SYMBOL= each2 'THE-ENTITY) (LIST value2))))
  (COND
     ((SYMBOL= operator 'SAME-AS)
        (SETF list1 (OR list1 (ENTITY1 (LIST value1))))  ; THE-ENTITY
        (SETF list2 (OR list2 (ENTITY2 (LIST value2)))))  ; THE-ENTITY
     ((MEMBER operator comparative-operators :TEST 'SYMBOL=)
        (SETF list1 (OR list1 (ENSURE-LIST value1)))   ; EACH
        (SETF list2 (OR list2 (ENSURE-LIST value2))))  ; EACH
     ((SYMBOL= operator 'BETWEEN)
        (SETF list1 (OR list1 (ENSURE-LIST value1)))  ; EACH
        (SETF list2 
           (IF (LISTP value2) 
               (ENTITY2 (LIST value2))  ; THE-ENTITY
               (ERROR between-error-msg value2))))
     ((MEMBER operator set-operators :TEST 'SYMBOL=)
        (SETF list1 (OR list1 (ENTITY1 (LIST value1))))  ; THE-ENTITY
        (SETF list2 (OR list2 (ENTITY2 (LIST value2)))))  ; THE-ENTITY
     ((SYMBOL= operator 'TRUE-PER)
        (SETF list1 (OR list1 (ENTITY1 (LIST value1))))  ; THE-ENTITY
        (SETF list2 (OR list2 (ENSURE-LIST value2))))  ; EACH
     ((OR (NOT operator) (SYMBOL= operator 'NON-NIL))
        (SETF list1 (OR list1 (LIST value1)))  ; THE-ENTITY
        (SETF list2 list1))  ; THE-ENTITY
     )

  (COND 
     ((= (LENGTH list1) (LENGTH list2)) )
     ((OR (ATOM value1) (SYMBOL= each1 'THE-ENTITY))
        (SETF list1 (LOOP FOR i FROM 1 TO (LENGTH list2) COLLECT value1)))
     ((OR (ATOM value2) (SYMBOL= each2 'THE-ENTITY))
        (SETF list2 (LOOP FOR i FROM 1 TO (LENGTH list1) COLLECT value2)))
     ((NOT (OR (SYMBOL= each1 'THE-ENTITY) (SYMBOL= each2 'THE-ENTITY)))
        (ERROR (S+ "If both values of TRUE? are lists, then the lists must be of the same length. "
                 "The first list is of length ~A and the second of length ~A.")
             (LENGTH list1) (LENGTH list2))))
   (LIST list1 list2)
)))

; ------------ TRUE?-aux -------------

(DEFUN True?-aux (each1 value1 is-isnt operator each2 value2 case-sensitive inclusive 
           T-if-all-T T-if-any-T count by-position)
  (LET* ((user-provided-operator 
             (IF (SYMBOL= operator 'TRUE-PER) value2))
         (must-be-number (MEMBER operator '(= < >)))
         (v1 (IF (AND (NOT (SYMBOL= operator 'TRUE-PER))
                      (NOT (EQ value1 T)) (NOT (EQ value1 NIL))
                      (SYMBOLP value1))
                 (STRING value1)
                 value1))
         (v2 (IF (AND (NOT (SYMBOL= operator 'TRUE-PER))
                      (NOT (EQ value2 T)) (NOT (EQ value2 NIL))
                      (SYMBOLP value2))
                 (STRING value2)
                 value2)) 
         (lists (TRUE?-aux-lists each1 v1 operator each2 v2)) 
     ;   (lists (TRUE?-aux-lists each1 value1 operator each2 value2))
         (list1 (FIRST lists))
         (list2 (SECOND lists))
         (result)
         (not-string-message "MATCHED-BY-PATTERN in TRUE? requires that the ~A be a string, not '~A'")
         )
   (IF (AND by-position (NOT (LISTP value1)) (NOT (LISTP value2))) 
       (ERROR (S+ "BY-POSITION may be specified in TRUE? only when at least one value is a list")))

   (SETF result
    (LOOP FOR a IN list1
          FOR b IN list2
          AS target1 = (COND
                          ((LISTP a) (IF by-position (REF a by-position) a)) 
                          ((LABELED-SEQUENCE-P a) (LABELED-SEQUENCE-SEQUENCE a))
                          (T a))
          AS target2 = (COND
                          ((LISTP b) (IF by-position (REF b by-position) b))
                          ((LABELED-SEQUENCE-P b) (LABELED-SEQUENCE-SEQUENCE b))
                          (T b))
          AS op = operator
          DO
           (WHEN (AND (SYMBOL= operator 'CONTAINED-IN) (STRINGP target2))
              (IF (NOT (STRINGP target1))
                  (ERR+ PROBLEM (S+ "Using the CONTAINED-IN option of TRUE? demands "
                                    "that if the target is a string, then the query "
                                    "must be as well. The query is ~A.")
                        ADVICE "Perhaps the query can be made into a string?"
                        INDENT "Or the EACH option before the query might be appropriate?"
                        FORMAT-ARGS target1))
              (SETF op 'MATCHED-BY-PATTERN)
              (LET ((z target1))
                 (SETF target1 target2)
                 (SETF target2 z)))
           (IF (AND must-be-number (OR (NOT (NUMBERP target1)) (NOT (NUMBERP target2))))
               (ERR+ PROBLEM "Non-number, '~A', found using =, <, or >"
                     ADVICE "If you must use non-numbers, try IS, GREATER-THAN, etc"
                     FORMAT-ARGS (IF (NUMBERP target2) target1 target2)))
          COLLECT
                (COND
                   ((SYMBOL= op 'SAME-AS)
                      (FORWARD-FUNCALL 'BB-SAME target1 target2 :CASE-SENSITIVE case-sensitive))
                   ((SYMBOL= op 'GREATER-THAN)
                      (OR (FORWARD-FUNCALL 'BB-GREATER-THAN target1 target2 :CASE-SENSITIVE? case-sensitive)
                          (AND inclusive (FORWARD-FUNCALL 'BB-SAME target1 target2 :CASE-SENSITIVE case-sensitive))))
                   ((SYMBOL= op 'LESS-THAN)
                      (OR (FORWARD-FUNCALL 'BB-LESS-THAN target1 target2 :CASE-SENSITIVE? case-sensitive)
                          (AND inclusive (FORWARD-FUNCALL 'BB-SAME target1 target2 :CASE-SENSITIVE case-sensitive))))
                   ((SYMBOL= op '=) (= target1 target2))
                   ((SYMBOL= op '<) 
                      (IF inclusive
                          (<= target1 target2)
                          (< target1 target2)))
                   ((SYMBOL= op '>) 
                      (IF inclusive
                          (>= target1 target2)
                          (> target1 target2)))
                   ((SYMBOL= op 'BETWEEN) 
                      (LIES-BETWEEN target1 target2 :CASE-SENSITIVE case-sensitive :INCLUSIVE inclusive))
                   ((SYMBOL= op 'CONTAINED-IN) 
                      (LET* ((test (IF case-sensitive 'EQUAL 'BB-SAME))
                            (match (MEMBER target1 target2 :TEST test)))
                        (IF match T NIL)))
                   ((SYMBOL= op 'A-SUBSET-OF)
                      (LET* ((match))
                        (COND
                           ((LISTP target2) NIL)
                           ((NOT (STRINGP target2))
                              (ERR+ PROBLEM "When A-SUBSET-OF is specified in ~A, "
                                    INDENT (S+ "value2 must be a list, "
                                               "or a string, not '~A'")
                                    FORMAT-ARGS *df-name* target2))
                           ((NOT (OR (LISTP target1)(STRINGP target1)))
                              (ERR+ PROBLEM "When A-SUBSET-OF is specified in ~A, "
                                    INDENT (S+ "value1 must bea list, "
                                               "or a string, not '~A'")
                                    FORMAT-ARGS *df-name* target1))) 
                        (SETF match (BB-IS-SUBSET? target1 target2 
                                       :CASE-SENSITIVE case-sensitive))
                        (IF match T NIL)))
                   ((SYMBOL= op 'MATCHED-BY-PATTERN) 
                  ;   (IF (NOT (STRINGP target1)) (ERROR not-string-message "target" target1))
                      (IF (NOT (STRINGP target2)) (ERROR not-string-message "pattern" target2))
                      (LET ((match (FORWARD-FUNCALL 'MATCH-OF-PATTERN-aux NIL 
	                                  (FORWARD-FUNCALL 'BIOBIKE-PATTERN-TO-REGEX target2) 
                                      target1
						          '(T NIL NIL NIL) :CASE-SENSITIVE case-sensitive)))
                        (IF (MEMBER operator '(does-not-contain does-not-match-pattern))
                            (NOT match) (IF match T NIL))))
                   ((SYMBOL= op 'TRUE-PER)
                      (LET* ((function-symbol (MACRO-TO-FUNCTION-SYMBOL user-provided-operator))
                             (function (SYMBOL-FUNCTION function-symbol))
                             (argument-description
                               (swank-backend:ARGLIST function))
                             (number-of-extra-args (1- (LENGTH argument-description)))
                             (argument-list
                                (CONCATENATE 'LIST (LIST target1)
                                    (IF (> number-of-extra-args 0)
                                        (MAKE-LIST number-of-extra-args))))
                             )
                        (APPLY function argument-list)))
                   ((SYMBOL= op 'NON-NIL) (IF target1 T NIL))
                   (T (IF target1 T NIL))
                   )
             ))
   (IF (SYMBOL= is-isnt 'IS-NOT)
       (SETF result (MAPCAR 'NOT result)))
   (COND
      (T-if-all-T (ALL-TRUE-aux result))
      (T-if-any-T (ANY-TRUE-aux result))
      (count (LENGTH (FLATTEN result)))
      ((= (LENGTH result) 1)
         (FIRST result))
      (T result))
)) 

; ------------ TRUE? -------------
(DEFMACRO true? (&REST rest)
  (LET* ((words rest)
         (operator-names 
          '(same-as greater-than less-than between
            = < > contained-in a-subset-of 
            matched-by-pattern true-per non-nil))
         (inclusive-operators '(greater-than less-than between < >))
         (operator)
         (value1)(value2)(each1)(each2)(is-isnt)
         (case-sensitive)(inclusive)(T-if-all-T)(T-if-any-T)(count)(by-position)
         (first-flag)
         (nil-value (GENSYM))
        )
    (LOOP WHILE words
      AS word = (POP words)
      DO 
       (COND
          ((AND (NOT each1) (NOT value1) 
                (OR (SYMBOL= word 'EACH) (SYMBOL= word 'THE-ENTITY)))
             (SETF each1 word))
          ((NOT value1) 
              (SETF value1 
                (COND
                   ((AND each1 (KEYWORDP word))
                       ; Guard against TRUE? each :T-OF-ALL-T and similar where each1 is a variable, not a token
                       (LET ((previous-word each1))
                         (SETF each1 NIL)
                         (PUSH word words) ; put word back into list
                         previous-word))
                   ((NULL word) nil-value)
                   (T word))))
          ((AND (NOT is-isnt) 
                (OR (SYMBOL= word 'IS) (SYMBOL= word 'IS-NOT)))
             (SETF is-isnt word))
          ((AND (NOT operator) 
                (OR (AND (NOT is-isnt) (NULL word))
                    (MEMBER word operator-names :TEST 'SYMBOL=)))
             (SETF operator (OR word nil-value)))
          ((AND operator (NOT each2) (OR (SYMBOL= word 'EACH) (SYMBOL= word 'THE-ENTITY)))
             (SETF each2 word))
          ((AND (NOT case-sensitive) (EQUAL word :CASE-SENSITIVE))
             (IF (NOT first-flag) (SETF first-flag :CASE-SENSITIVE))
             (SETF case-sensitive T))
          ((AND (NOT inclusive) (EQUAL word :INCLUSIVE))
             (IF (NOT first-flag) (SETF first-flag :INCLUSIVE))
             (SETF inclusive T))
          ((AND (NOT T-if-all-T) (EQUAL word :T-IF-ALL-T))
             (IF (NOT first-flag) (SETF first-flag :T-IF-ALL-T))
             (SETF T-if-all-T T))
          ((AND (NOT T-if-any-T) (EQUAL word :T-IF-ANY-T))
             (IF (NOT first-flag) (SETF first-flag :T-IF-ANY-T))
             (SETF T-if-any-T T))
          ((AND (NOT count) (EQUAL word :COUNT))
             (IF (NOT first-flag) (SETF first-flag :COUNT))
             (SETF count T))
          ((AND (NOT by-position) (SYMBOL= word 'BY-POSITION) (> (LENGTH words) 0))
             (IF (NOT first-flag) (SETF first-flag :CASE-SENSITIVE))
             (SETF by-position (POP words)))
          ((AND (OR operator is-isnt) (NOT value2))
             (IF first-flag (ERROR "INTERNAL ERROR in TRUE? Please report '~A'" rest))
             (SETF value2 (OR word nil-value)))
          ((AND each1 (NOT operator))
             (PUSH word words)
             (PUSH value1 words)
             (SETF value1 each1)
             (SETF each1 NIL)) 
          (T (IF (NOT operator)
                 (ERROR "No legal operator found in 'TRUE? ~A'. Did you forget to specify a test?" 
                      ;(STRING-JOIN+ (FLAT rest))
                      (FORWARD-FUNCALL 'BB-JOIN rest :BY " " :AS-STRING T :NIL-OK T)
                      )
                 (ERROR "Unknown keyword/flag '~A' in TRUE?" word))))
         )
  (WHEN (AND each2 (NOT value2))  ; (TRUE? x operator each), i.e. each is a variable
     (SETF value2 each2)
     (SETF each2 NIL))
  (IF (EQUAL operator nil-value) (SETF operator NIL))
  (IF (AND operator (NOT value2) (NOT (SYMBOL= operator 'NON-NIL)) first-flag)
      (SETF value2 first-flag))  ; Unfortunately, leaves flag set
  (IF (AND is-isnt (NOT operator) value2)
      (SETF operator 'SAME-AS))
  (UNLESS (OR value2 (NOT operator) (SYMBOL= operator 'NON-NIL))
      (ERROR "TRUE? requires a second value for every operator except NON-NIL"))
  (IF (AND (SYMBOL= operator 'NON-NIL) (OR value2 each2))
      (ERROR "The NON-NIL operator of TRUE? does not use a second value"))
  (ERROR-IF-MORE-THAN-ONE count t-if-all-t t-if-any-t)
  (IF (AND inclusive (NOT (MEMBER operator inclusive-operators :TEST 'SYMBOL=)))
      (WARN "The INCLUSIVE option is not used for the ~A operator" operator))
  (IF (EQUAL value1 nil-value)(SETF value1 NIL))
  (IF (EQUAL value2 nil-value)(SETF value2 NIL))
  (WHEN (AND each1 (NOT is-isnt)
         (MEMBER value1 '(IS IS-NOT NIL) :TEST 'SYMBOL=))
     (SETF is-isnt value1)
     (SETF value1 each1)
     (SETF each1 NIL))

 `(LET* ((*bbl-level* (INCREMENT-BBL-LEVEL)))
    (TRUE?-aux ',each1 ,value1 ',is-isnt ',operator ',each2 ,value2 
      ,case-sensitive ,inclusive ,t-if-all-t ,t-if-any-t ,count ,by-position))
))


; =============== IF-TRUE and family ===================

(DEFMACRO If-true-aux (options &REST forms)
  "Governs flow of logic"
  (IF (NOT forms)               ; Check for body of IF-TRUE
      (ERR+ problem "If-true statement missing body~&")
    ) 
     
  (LET ((test-forms NIL)
        (THEN-begin (POSITION 'THEN forms :TEST 'SYMBOL=))
        (THEN-end)
        (THEN-forms nil)
        (ELSE-begin (POSITION 'ELSE forms :TEST 'SYMBOL=))
        (ELSE-end)
        (ELSE-forms nil))

    (IF (NOT THEN-begin)         ; If no THEN, ERROR
        (ERROR "No THEN clause specified in IF-TRUE"))
    (SETF test-forms (SUBSEQ forms 0 THEN-begin))
    (INCF THEN-begin 1)
    (COND (ELSE-begin            ; ELSE determines end of THEN
           (SETF THEN-end ELSE-begin)
           (INCF ELSE-begin)
           (SETF ELSE-end (LENGTH forms)))
          (T                     ; If no ELSE then everything is THEN
           (SETF THEN-end (LENGTH forms))))
 
    (SETF THEN-forms (SUBSEQ forms THEN-begin THEN-end))
    (IF ELSE-begin
        (SETF ELSE-forms (SUBSEQ forms ELSE-begin ELSE-end)))
    
    ;; Only generate PROGN's if they are really needed.

    (flet ((one-or-progn (forms)
             (if (= (length forms) 1)
                 (biolisp::first forms)
               `(progn ,@forms))))
      (IF ELSE-begin
          `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
	         (IF (TRUE? ,@test-forms ,@options)             ; IF-THEN-ELSE
                 ,(one-or-progn then-forms)
                 ,(one-or-progn else-forms)))
          `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
	         (when (TRUE? ,@test-forms ,@options)          ; IF-THEN 
               ,(one-or-progn then-forms))))
)))

(DEFINE-MACRO IF-TRUE
  REQUIRED args
  KEYWORD by-position
  FLAG case-sensitive
  FLAG T-if-all-T
  FLAG T-if-any-T
  FLAG %%EXPANDABLE%%
  BODY
   (LET* ((options
            (CONCATENATE 'LIST
               (IF (OR t-if-all-t (NOT t-if-any-t))
                   (LIST :T-IF-ALL-T))
               (IF t-if-any-t (LIST :T-IF-ANY-T))
               (IF case-sensitive (LIST :CASE-SENSITIVE))
               (LIST :BY-POSITION by-position)))
          )
    `(IF-TRUE-aux ,options ,@args)
    ))


(DEFMACRO If-false (test &REST forms)
   `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
	  (IF-TRUE (NOT ,test) ,@forms)))

(DEFMACRO If-not-true (test &REST forms)
   `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
      (IF-TRUE (NOT ,test) ,@forms)))

(DEFUN List-to-table-aux (list &KEY with-labels embedded-labels 
                                    contains-header dimension existing-table)
  (IF (NOT (LISTP list))
      (ERROR "Argument to LIST-TO-TABLE-aux must be a list"))
  (LET* ((header (IF contains-header (POP list)))
         (labels 
           (COND
              (with-labels with-labels)
              (header header)))
         (dimension 
            (COND
               (existing-table (utils::GARRAY-RANK existing-table))            
               (dimension dimension)
               ((IS-NON-NIL-LIST? (FIRST list))
                  (COND
                     ((AND embedded-labels (NOT labels))
                        (- (LENGTH (FIRST list)) 1))
                     ((NOT labels) ; nor embedded-labels
                        (LENGTH (FIRST list)))
                     (T 2))) ; because if dimension is 1, then not a list, 
                             ;     if > 2, then embedded-labels
               (embedded-labels
                   (ERROR (S+ "When using embedded labels, each element of list "
                              "given to LIST-TO-TABLE-aux must be a list.")))
               (T 1)))
         (table 
             (OR existing-table
                 (IF (OR labels embedded-labels)
                  ;  (NEW-TABLE (REPEAT '$ TIMES dimension AS-LIST))
                     (NEW-TABLE-aux (MAKE-LIST dimension :INITIAL-ELEMENT '$) NIL NIL)
                     (IF (< dimension 2)
                     ;   (NEW-TABLE (LIST (LENGTH list)))
                         (NEW-TABLE-aux (LIST (LENGTH list)) NIL T)
                         (LET ((extent1 (LENGTH list))
                               (extent2 (LENGTH (FIRST list))))
                     ;     (NEW-TABLE (LIST extent1 extent2))
                           (NEW-TABLE-aux (LIST extent1 extent2) NIL T))))))
         )
          
   (COND
      ((AND embedded-labels (NOT labels))
         (LOOP FOR line in list
              AS value = (FIRST (LAST line))
              AS indices = (SUBSEQ line 0 (1- (LENGTH line)))
              DO (utils::APPLY-SETF-GREF table value indices)))
      ((> dimension 2)
          (ERROR (S+ "At present tables with dimensionality > 2 can be handled "
                     "only as lists with embedded labels")))
      ((AND (NOT labels) (= dimension 1))
         (LOOP FOR i FROM 1
               FOR value IN list
               DO (SETF (REF table i) value)))
      ((NOT labels) ; dimension = 2
         (LOOP FOR i FROM 1
               FOR sublist IN list
               DO (LOOP FOR j FROM 1
                        FOR value IN sublist
                        DO (SETF (REF table i j) value))))
      ((AND labels embedded-labels)
         (LOOP FOR sublist IN list
               AS label1 = (POP sublist)
               DO (COND
                     ((= (LENGTH sublist) (1- (LENGTH labels)))
                         (POP labels))
                     ((< (LENGTH labels) (LENGTH sublist))
                         (ERROR "Not enough labels in ~A in LIST-TO-TABLE"
                                labels))
                     ((> (LENGTH labels) (LENGTH sublist))
                         (ERROR "Too many labels in ~A in LIST-TO-TABLE"
                                labels)))
                  (LOOP FOR label2 IN labels
                        FOR value IN sublist
                        DO (SETF (REF table label1 label2) value))))
      ((AND labels (= dimension 1))
         (LOOP FOR label IN labels
               FOR value IN list
               DO (SETF (REF table label) value)))
      (labels ; dimension = 2
         (LOOP FOR line IN list
               FOR label1 IN (FIRST labels)
               DO (LOOP FOR value IN line
                        FOR label2 IN (SECOND labels)
                        DO (SETF (REF table label1 label2) value))))
      (T (ERROR "Internal error in LIST-TO-TABLE"))
      )
   table
))

(DEFUN all-sets-in (groups &KEY head)
  ; groups = ((a b c ...)(d e f ...)...)
  ; Returns all possible sets, choosing one element from each group
  ; Example: (ALL-SETS-IN '((a b c) (d) (e f)))
  ;  --> ((a d e) (a d f) (b d e) (b d f) (c d e) (c d f))
  (LET* ((current-group (POP groups))
         )
  (IF (NULL current-group)
      head
     (LOOP FOR item IN (ENSURE-LIST current-group)
           AS set-element = (FORWARD-FUNCALL 'ALL-SETS-IN groups 
                           ;  :HEAD (JOIN head item AS-LIST))
                              :HEAD (APPEND head (LIST item)))
           WHEN (IS-SIMPLE-LIST? set-element)
              COLLECT set-element
           WHEN (NOT (IS-SIMPLE-LIST? set-element))
              APPEND set-element))
))

(DEFMACRO symbol-to-string-maybe (item)
  "Converts a non-NIL symbol (or list) to a string (or list of strings), all lower-case"
   `(COND
       ((LISTP ,item)
          (SETF ,item 
		     (MAPCAR (LAMBDA (x) (BB-STRING-OF x :LOWER-CASE T)) ,item)))
       ((AND ,item (SYMBOLP ,item))
          (SETF ,item (BB-STRING-OF ,item :LOWER-CASE T)))
       )
)

(DEFUN NIL-to-hyphen (item-or-list)
  (IF (AND item-or-list (LISTP item-or-list))
      (LOOP FOR item IN item-or-list
            COLLECT (OR item "-"))
      (OR item-or-list "-"))
)