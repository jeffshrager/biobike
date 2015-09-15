;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cl-user; -*-

(in-package :cl-user) 

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

;;; Author: JP Massar. 


(defun cl-symbols ()
  (let ((special-ops nil)
        (macros nil)
        (functions nil)
        (vars nil)
        (constants nil)
        (types nil)
        (other nil))
    (do-external-symbols (x :common-lisp)
      (when (eq (find-package :common-lisp) (symbol-package x))
        (cond 
         ((fboundp x) 
          (cond
           ((special-operator-p x) (push x special-ops))
           ((macro-function x) (push x macros))
           (t (push x functions))
           ))
         ((boundp x)
          (cond
           ((constantp x) (push x constants))
           (t (push x vars))
           ))
         (t
          (cond 
           ((ignore-errors (subtypep x t)) (push x types))
           (t (push x other))
         )))))
    (mapcar 
     'wb::sort-symbols-smartly 
     (list special-ops macros functions vars constants types other))))


(defparameter *useful-cl-special-ops* 
  '(
    BLOCK 
    ;; CATCH
    ;; EVAL-WHEN
    FLET
    FUNCTION
    GO
    IF
    ;; LABELS
    LET
    LET*
    ;; LOAD-TIME-VALUE
    ;; LOCALLY
    ;; MACROLET
    ;; MULTIPLE-VALUE-CALL
    ;; MULTIPLE-VALUE-PROG1
    PROGN
    ;; PROGV
    QUOTE
    RETURN-FROM
    SETQ
    ;; SYMBOL-MACROLET
    ;; TAGBODY
    ;; THE
    ;; THROW
    ;; UNWIND-PROTECT
    ))

(defparameter *useful-cl-macros*
  '(
    AND 
    ;; ASSERT
    CASE
    ;; CCASE
    ;; CHECK-TYPE
    COND
    ;; CTYPECASE
    DECF
    ;; DECLAIM
    ;; DEFCLASS
    ;; DEFCONSTANT
    ;; DEFGENERIC
    ;; DEFINE-COMPILER-MACRO
    ;; DEFINE-CONDITION
    ;; DEFINE-METHOD-COMBINATION
    ;; DEFINE-MODIFY-MACRO
    ;; DEFINE-SETF-EXPANDER
    ;; DEFINE-SYMBOL-MACRO
    DEFMACRO
    ;; DEFMETHOD
    ;; DEFPACKAGE
    DEFPARAMETER
    ;; DEFSETF
    DEFSTRUCT
    ;; DEFTYPE
    ;;; *** Need to work in lambda list parsing stuff
    DEFUN
    DEFVAR
    DESTRUCTURING-BIND
    ;; used for the 'DO' keyword in LOOP 
    DO
    ;; DO*
    ;; DO-ALL-SYMBOLS
    ;; DO-EXTERNAL-SYMBOLS
    ;; DO-SYMBOLS
    ;; DOLIST
    ;; DOTIMES
    ECASE
    ;; ETYPECASE
    ;; FORMATTER
    ;; GENERIC-FUNCTION
    ;; HANDLER-BIND
    HANDLER-CASE
    ;; IGNORE-ERRORS
    IN-PACKAGE
    INCF
    LAMBDA
    ;; LOOP
    ;; LOOP-FINISH
    ;; MULTIPLE-VALUE-BIND
    ;; MULTIPLE-VALUE-LIST
    ;; MULTIPLE-VALUE-SETQ
    ;; NTH-VALUE
    OR
    POP
    ;; PPRINT-EXIT-IF-LIST-EXHAUSTED
    ;; PPRINT-LOGICAL-BLOCK
    ;; PPRINT-POP
    ;; PRINT-UNREADABLE-OBJECT
    ;; PROG
    ;; PROG*
    PROG1
    ;; PROG2
    ;; PSETF
    ;; PSETQ
    PUSH
    ;; PUSHNEW
    ;; REMF
    ;; RESTART-BIND
    ;; RESTART-CASE
    RETURN
    ;; ROTATEF
    SETF
    ;; SHIFTF
    ;; STEP
    ;; TIME
    ;; TRACE
    ;; TYPECASE
    UNLESS
    ;; UNTRACE
    WHEN
    ;; WITH-ACCESSORS
    ;; WITH-COMPILATION-UNIT
    ;; WITH-CONDITION-RESTARTS
    ;; WITH-HASH-TABLE-ITERATOR
    ;; WITH-INPUT-FROM-STRING
    ;; WITH-OPEN-FILE
    ;; WITH-OPEN-STREAM
    ;; WITH-OUTPUT-TO-STRING
    ;; WITH-PACKAGE-ITERATOR
    ;; WITH-SIMPLE-RESTART
    ;; WITH-SLOTS
    ;; WITH-STANDARD-IO-SYNTAX
    ))

(defparameter *useful-cl-functions*
  '(
    *
    +
    -
    /
    /=
    1+
    1-
    <
    <=
    =
    >
    >=
    ;; ABORT
    ;; ABS
    ;; ACONS
    ACOS
    ;; ACOSH
    ;; ADD-METHOD
    ;; ADJOIN
    ;; ADJUST-ARRAY
    ;; ADJUSTABLE-ARRAY-P
    ;; ALLOCATE-INSTANCE
    ;; ALPHA-CHAR-P    ;; needs to be shadowed to allow length 1 string arg
    ;; ALPHANUMERICP   ;; needs to be shadowed to allow length 1 string arg
    APPEND
    ;; APPLY
    ;; APROPOS
    ;; APROPOS-LIST
    ;; AREF
    ;; ARITHMETIC-ERROR-OPERANDS
    ;; ARITHMETIC-ERROR-OPERATION
    ;; ARRAY-DIMENSION
    ;; ARRAY-DIMENSIONS
    ;; ARRAY-DISPLACEMENT
    ;; ARRAY-ELEMENT-TYPE
    ;; ARRAY-HAS-FILL-POINTER-P
    ;; ARRAY-IN-BOUNDS-P
    ;; ARRAY-RANK
    ;; ARRAY-ROW-MAJOR-INDEX
    ;; ARRAY-TOTAL-SIZE
    ;; ARRAYP
    ;; ASH
    ASIN
    ;; ASINH
    ;; ASSOC
    ;; ASSOC-IF
    ;; ASSOC-IF-NOT
    ATAN
    ;; ATANH
    ;; ATOM
    ;; BIT
    ;; BIT-AND
    ;; BIT-ANDC1
    ;; BIT-ANDC2
    ;; BIT-EQV
    ;; BIT-IOR
    ;; BIT-NAND
    ;; BIT-NOR
    ;; BIT-NOT
    ;; BIT-ORC1
    ;; BIT-ORC2
    ;; BIT-VECTOR-P
    ;; BIT-XOR
    ;; BOOLE
    ;; BOTH-CASE-P
    ;; BOUNDP
    ;; BREAK
    ;; BROADCAST-STREAM-STREAMS
    ;; BUTLAST
    ;; BYTE
    ;; BYTE-POSITION
    ;; BYTE-SIZE
    ;; CAAAAR
    ;; CAAADR
    ;; CAAAR
    ;; CAADAR
    ;; CAADDR
    ;; CAADR
    ;; CAAR
    ;; CADAAR
    ;; CADADR
    ;; CADAR
    ;; CADDAR
    ;; CADDDR
    ;; CADDR
    CADR
    CAR
    ;; CDAAAR
    ;; CDAADR
    ;; CDAAR
    ;; CDADAR
    ;; CDADDR
    ;; CDADR
    ;; CDAR
    ;; CDDAAR
    ;; CDDADR
    ;; CDDAR
    ;; CDDDAR
    ;; CDDDDR
    ;; CDDDR
    ;; CDDR
    CDR
    CEILING
    ;; CELL-ERROR-NAME
    ;; CERROR
    ;; CHANGE-CLASS
    ;; CHAR
    ;; CHAR-CODE
    ;; CHAR-DOWNCASE
    ;; CHAR-EQUAL
    ;; CHAR-GREATERP
    ;; CHAR-INT
    ;; CHAR-LESSP
    ;; CHAR-NAME
    ;; CHAR-NOT-EQUAL
    ;; CHAR-NOT-GREATERP
    ;; CHAR-NOT-LESSP
    ;; CHAR-UPCASE
    ;; CHAR/=
    ;; CHAR<
    ;; CHAR<=
    ;; CHAR=
    ;; CHAR>
    ;; CHAR>=
    ;; CHARACTER
    ;; CHARACTERP
    ;; CIS
    ;; CLASS-NAME
    ;; CLASS-OF
    ;; CLEAR-INPUT
    ;; CLEAR-OUTPUT
    ;; CLOSE
    ;; CLRHASH
    ;; CODE-CHAR
    COERCE
    ;; COMPILE
    ;; COMPILE-FILE
    ;; COMPILE-FILE-PATHNAME
    ;; COMPILED-FUNCTION-P
    ;; COMPILER-MACRO-FUNCTION
    ;; COMPLEMENT
    ;; COMPLEX
    ;; COMPLEXP
    ;; COMPUTE-APPLICABLE-METHODS
    ;; COMPUTE-RESTARTS
    ;; CONCATENATE
    ;; CONCATENATED-STREAM-STREAMS
    ;; CONJUGATE
    CONS
    ;; CONSP
    ;; CONSTANTLY
    ;; CONSTANTP
    ;; CONTINUE
    ;; COPY-ALIST
    COPY-LIST
    ;; COPY-PPRINT-DISPATCH
    ;; COPY-READTABLE
    COPY-SEQ
    ;; COPY-STRUCTURE
    ;; COPY-SYMBOL
    ;; COPY-TREE
    COS
    ;; COSH
    ;; COUNT
    ;; COUNT-IF
    ;; COUNT-IF-NOT
    ;; DECLARE
    ;; DECODE-FLOAT
    ;; DECODE-UNIVERSAL-TIME
    ;; DELETE
    ;; DELETE-DUPLICATES
    ;; DELETE-FILE
    ;; DELETE-IF
    ;; DELETE-IF-NOT
    ;; DELETE-PACKAGE
    ;; DENOMINATOR
    ;; DEPOSIT-FIELD
    ;; DESCRIBE
    ;; DESCRIBE-OBJECT
    ;; DIGIT-CHAR
    ;; DIGIT-CHAR-P    ;; needs to be shadowed to allow length 1 string arg
    ;; DIRECTORY
    ;; DIRECTORY-NAMESTRING
    ;; DISASSEMBLE
    ;; DOCUMENTATION
    ;; DPB
    ;; DRIBBLE
    ;; ECHO-STREAM-INPUT-STREAM
    ;; ECHO-STREAM-OUTPUT-STREAM
    ;; ED
    ;; EIGHTH
    ;; ELT
    ;; ENCODE-UNIVERSAL-TIME
    ;; ENDP
    ;; ENOUGH-NAMESTRING
    ;; ENSURE-DIRECTORIES-EXIST
    ;; ENSURE-GENERIC-FUNCTION
    EQ
    EQL
    EQUAL
    EQUALP
    ERROR
    ;; EVAL
    EVENP
    EVERY
    ;; EXP
    ;; EXPORT
    EXPT
    ;; FBOUNDP
    ;; FCEILING
    ;; FDEFINITION
    ;; FFLOOR
    ;; FIFTH
    ;; FILE-AUTHOR
    ;; FILE-ERROR-PATHNAME
    ;; FILE-LENGTH
    ;; FILE-NAMESTRING
    ;; FILE-POSITION
    ;; FILE-STRING-LENGTH
    ;; FILE-WRITE-DATE
    ;; FILL
    ;; FILL-POINTER
    ;; FIND
    ;; FIND-ALL-SYMBOLS
    ;; FIND-CLASS
    ;; FIND-IF
    ;; FIND-IF-NOT
    ;; FIND-METHOD
    ;; FIND-PACKAGE
    ;; FIND-RESTART
    ;; FIND-SYMBOL
    ;; FINISH-OUTPUT
    ;; FIRST
    FLOAT
    ;; FLOAT-DIGITS
    ;; FLOAT-PRECISION
    ;; FLOAT-RADIX
    ;; FLOAT-SIGN
    ;; FLOATP
    FLOOR
    ;; FMAKUNBOUND
    ;; FORCE-OUTPUT
    ;; FORMAT
    ;; FOURTH
    ;; FRESH-LINE
    ;; FROUND
    ;; FTRUNCATE
    ;; FUNCALL
    ;; FUNCTION-KEYWORDS
    ;; FUNCTION-LAMBDA-EXPRESSION
    ;; FUNCTIONP
    ;; GCD
    ;; GENSYM
    ;; GENTEMP
    ;; GET
    ;; GET-DECODED-TIME
    ;; GET-DISPATCH-MACRO-CHARACTER
    ;; GET-INTERNAL-REAL-TIME
    ;; GET-INTERNAL-RUN-TIME
    ;; GET-MACRO-CHARACTER
    ;; GET-OUTPUT-STREAM-STRING
    ;; GET-PROPERTIES
    ;; GET-SETF-EXPANSION
    ;; GET-UNIVERSAL-TIME
    ;; GETF
    GETHASH
    ;; GRAPHIC-CHAR-P
    ;; HASH-TABLE-COUNT
    ;; HASH-TABLE-P
    ;; HASH-TABLE-REHASH-SIZE
    ;; HASH-TABLE-REHASH-THRESHOLD
    ;; HASH-TABLE-SIZE
    ;; HASH-TABLE-TEST
    ;; HOST-NAMESTRING
    IDENTITY
    ;; IMAGPART
    ;; IMPORT
    ;; INITIALIZE-INSTANCE
    ;; INPUT-STREAM-P
    ;; INSPECT
    ;; INTEGER-DECODE-FLOAT
    ;; INTEGER-LENGTH
    INTEGERP
    ;; INTERACTIVE-STREAM-P
    ;; INTERN
    ;; INTERSECTION
    ;; INVALID-METHOD-ERROR
    ;; INVOKE-DEBUGGER
    ;; INVOKE-RESTART
    ;; INVOKE-RESTART-INTERACTIVELY
    ;; ISQRT
    ;; KEYWORDP
    ;; LAST
    ;; LCM
    ;; LDB
    ;; LDB-TEST
    ;; LDIFF
    LENGTH
    ;; LISP-IMPLEMENTATION-TYPE
    ;; LISP-IMPLEMENTATION-VERSION
    LIST
    ;; LIST*
    ;; LIST-ALL-PACKAGES
    ;; LIST-LENGTH
    ;; LISTEN
    LISTP
    ;; LOAD
    ;; LOAD-LOGICAL-PATHNAME-TRANSLATIONS
    ;; LOG
    ;; LOGAND
    ;; LOGANDC1
    ;; LOGANDC2
    ;; LOGBITP
    ;; LOGCOUNT
    ;; LOGEQV
    ;; LOGICAL-PATHNAME
    ;; LOGICAL-PATHNAME-TRANSLATIONS
    ;; LOGIOR
    ;; LOGNAND
    ;; LOGNOR
    ;; LOGNOT
    ;; LOGORC1
    ;; LOGORC2
    ;; LOGTEST
    ;; LOGXOR
    ;; LONG-SITE-NAME
    ;; LOWER-CASE-P
    ;; MACHINE-INSTANCE
    ;; MACHINE-TYPE
    ;; MACHINE-VERSION
    ;; MACRO-FUNCTION
    ;; MACROEXPAND
    ;; MACROEXPAND-1
    ;; MAKE-ARRAY
    ;; MAKE-BROADCAST-STREAM
    ;; MAKE-CONCATENATED-STREAM
    ;; MAKE-CONDITION
    ;; MAKE-DISPATCH-MACRO-CHARACTER
    ;; MAKE-ECHO-STREAM
    MAKE-HASH-TABLE
    ;; MAKE-INSTANCE
    ;; MAKE-INSTANCES-OBSOLETE
    ;; MAKE-LIST
    ;; MAKE-LOAD-FORM
    ;; MAKE-LOAD-FORM-SAVING-SLOTS
    ;; MAKE-PACKAGE
    ;; MAKE-PATHNAME
    ;; MAKE-RANDOM-STATE
    ;; MAKE-SEQUENCE
    MAKE-STRING
    ;; MAKE-STRING-INPUT-STREAM
    ;; MAKE-STRING-OUTPUT-STREAM
    ;; MAKE-SYMBOL
    ;; MAKE-SYNONYM-STREAM
    ;; MAKE-TWO-WAY-STREAM
    ;; MAKUNBOUND
    ;; MAP
    ;; MAP-INTO
    ;; MAPC
    MAPCAN
    MAPCAR
    ;; MAPCON
    ;; MAPHASH
    ;; MAPL
    ;; MAPLIST
    ;; MASK-FIELD
    MAX
    MEMBER
    ;; MEMBER-IF
    ;; MEMBER-IF-NOT
    ;; MERGE
    ;; MERGE-PATHNAMES
    ;; METHOD-COMBINATION-ERROR
    ;; METHOD-QUALIFIERS
    MIN
    MINUSP
    ;; MISMATCH
    ;; MOD
    ;; MUFFLE-WARNING
    ;; NAME-CHAR
    ;; NAMESTRING
    ;; NBUTLAST
    ;; NCONC
    ;; NINTERSECTION
    ;; NINTH
    ;; NO-APPLICABLE-METHOD
    ;; NO-NEXT-METHOD
    NOT
    ;; NOTANY
    ;; NOTEVERY
    ;; NRECONC
    ;; NREVERSE
    ;; NSET-DIFFERENCE
    ;; NSET-EXCLUSIVE-OR
    ;; NSTRING-CAPITALIZE
    ;; NSTRING-DOWNCASE
    ;; NSTRING-UPCASE
    ;; NSUBLIS
    ;; NSUBST
    ;; NSUBST-IF
    ;; NSUBST-IF-NOT
    ;; NSUBSTITUTE
    ;; NSUBSTITUTE-IF
    ;; NSUBSTITUTE-IF-NOT
    ;; NTH
    ;; NTHCDR
    NULL
    NUMBERP
    ;; NUMERATOR
    ;; NUNION
    ODDP
    ;; OPEN
    ;; OPEN-STREAM-P
    ;; OUTPUT-STREAM-P
    ;; PACKAGE-ERROR-PACKAGE
    ;; PACKAGE-NAME
    ;; PACKAGE-NICKNAMES
    ;; PACKAGE-SHADOWING-SYMBOLS
    ;; PACKAGE-USE-LIST
    ;; PACKAGE-USED-BY-LIST
    ;; PACKAGEP
    ;; PAIRLIS
    PARSE-INTEGER
    ;; PARSE-NAMESTRING
    ;; PATHNAME
    ;; PATHNAME-DEVICE
    ;; PATHNAME-DIRECTORY
    ;; PATHNAME-HOST
    ;; PATHNAME-MATCH-P
    ;; PATHNAME-NAME
    ;; PATHNAME-TYPE
    ;; PATHNAME-VERSION
    ;; PATHNAMEP
    ;; PEEK-CHAR
    ;; PHASE
    PLUSP
    ;; POSITION
    ;; POSITION-IF
    ;; POSITION-IF-NOT
    ;; PPRINT
    ;; PPRINT-DISPATCH
    ;; PPRINT-FILL
    ;; PPRINT-INDENT
    ;; PPRINT-LINEAR
    ;; PPRINT-NEWLINE
    ;; PPRINT-TAB
    ;; PPRINT-TABULAR
    ;; PRIN1
    ;; PRIN1-TO-STRING
    ;; PRINC
    ;; PRINC-TO-STRING
    PRINT
    ;; PRINT-NOT-READABLE-OBJECT
    ;; PRINT-OBJECT
    ;; PROBE-FILE
    ;; PROCLAIM
    ;; PROVIDE
    ;; RANDOM
    ;; RANDOM-STATE-P
    ;; RASSOC
    ;; RASSOC-IF
    ;; RASSOC-IF-NOT
    ;; RATIONAL
    ;; RATIONALIZE
    ;; RATIONALP
    ;; READ
    ;; READ-BYTE
    ;; READ-CHAR
    ;; READ-CHAR-NO-HANG
    ;; READ-DELIMITED-LIST
    ;; READ-FROM-STRING
    ;; READ-LINE
    ;; READ-PRESERVING-WHITESPACE
    ;; READ-SEQUENCE
    ;; READTABLE-CASE
    ;; READTABLEP
    ;; REALP
    ;; REALPART
    ;; REDUCE
    ;; REINITIALIZE-INSTANCE
    ;; REM
    ;; REMHASH
    ;; REMOVE
    ;; REMOVE-DUPLICATES
    ;; REMOVE-IF
    ;; REMOVE-IF-NOT
    ;; REMOVE-METHOD
    ;; REMPROP
    ;; RENAME-FILE
    ;; RENAME-PACKAGE
    ;; REPLACE
    ;; REQUIRE
    REST
    ;; RESTART-NAME
    ;; REVAPPEND
    REVERSE
    ;; ROOM
    ;; ROUND     ;; Temporary until DEFINE-FUNCTION works on redefined Lisp functions
    ;; ROW-MAJOR-AREF
    ;; RPLACA
    ;; RPLACD
    ;; SBIT
    ;; SCALE-FLOAT
    ;; SCHAR
    ;; SEARCH
    ;; SECOND
    ;; SET
    ;; SET-DIFFERENCE
    ;; SET-DISPATCH-MACRO-CHARACTER
    ;; SET-EXCLUSIVE-OR
    ;; SET-MACRO-CHARACTER
    ;; SET-PPRINT-DISPATCH
    ;; SET-SYNTAX-FROM-CHAR
    ;; SEVENTH
    ;; SHADOW
    ;; SHADOWING-IMPORT
    ;; SHARED-INITIALIZE
    ;; SHORT-SITE-NAME
    ;; SIGNAL
    ;; SIGNUM
    ;; SIMPLE-BIT-VECTOR-P
    ;; SIMPLE-CONDITION-FORMAT-ARGUMENTS
    ;; SIMPLE-CONDITION-FORMAT-CONTROL
    ;; SIMPLE-STRING-P
    ;; SIMPLE-VECTOR-P
    SIN
    ;; SINH
    ;; SIXTH
    SLEEP
    ;; SLOT-BOUNDP
    ;; SLOT-EXISTS-P
    ;; SLOT-MAKUNBOUND
    ;; SLOT-MISSING
    ;; SLOT-UNBOUND
    ;; SLOT-VALUE
    ;; SOFTWARE-TYPE
    ;; SOFTWARE-VERSION
    SOME
    ;; SORT ;; obtain from SHADOWLISP package
    ;; SPECIAL-OPERATOR-P
    ;; SQRT
    ;; STABLE-SORT  ;; obtain from SHADOWLISP package
    ;; STANDARD-CHAR-P
    ;; STORE-VALUE
    ;; STREAM-ELEMENT-TYPE
    ;; STREAM-ERROR-STREAM
    ;; STREAM-EXTERNAL-FORMAT
    ;; STREAMP
    STRING
    ;; STRING-CAPITALIZE
    ;; STRING-DOWNCASE
    STRING-EQUAL
    ;; STRING-GREATERP
    ;; STRING-LEFT-TRIM
    ;; STRING-LESSP
    ;; STRING-NOT-EQUAL
    ;; STRING-NOT-GREATERP
    ;; STRING-NOT-LESSP
    ;; STRING-RIGHT-TRIM
    STRING-TRIM
    STRING-UPCASE
    ;; STRING/=
    STRING<
    STRING<=
    STRING=
    STRING>
    STRING>=
    STRINGP
    ;; SUBLIS
    SUBSEQ
    ;; SUBSETP
    ;; SUBST
    ;; SUBST-IF
    ;; SUBST-IF-NOT
    ;; SUBSTITUTE
    ;; SUBSTITUTE-IF
    ;; SUBSTITUTE-IF-NOT
    ;; SUBTYPEP
    ;; SVREF
    ;; SXHASH
    ;; SYMBOL-FUNCTION
    ;; SYMBOL-NAME
    ;; SYMBOL-PACKAGE
    ;; SYMBOL-PLIST
    ;; SYMBOL-VALUE
    ;; SYMBOLP
    ;; SYNONYM-STREAM-SYMBOL
    ;; TAILP
    TAN
    ;; TANH
    ;; TENTH
    TERPRI
    ;; THIRD
    ;; TRANSLATE-LOGICAL-PATHNAME
    ;; TRANSLATE-PATHNAME
    ;; TREE-EQUAL
    ;; TRUENAME
    ;; TRUNCATE
    ;; TWO-WAY-STREAM-INPUT-STREAM
    ;; TWO-WAY-STREAM-OUTPUT-STREAM
    ;; TYPE-ERROR-DATUM
    ;; TYPE-ERROR-EXPECTED-TYPE
    ;; TYPE-OF
    TYPEP
    ;; UNBOUND-SLOT-INSTANCE
    ;; UNEXPORT
    UNINTERN
    ;; UNION
    ;; UNREAD-CHAR
    ;; UNUSE-PACKAGE
    ;; UPDATE-INSTANCE-FOR-DIFFERENT-CLASS
    ;; UPDATE-INSTANCE-FOR-REDEFINED-CLASS
    ;; UPGRADED-ARRAY-ELEMENT-TYPE
    ;; UPGRADED-COMPLEX-PART-TYPE
    ;; UPPER-CASE-P
    ;; USE-PACKAGE
    ;; USE-VALUE
    ;; USER-HOMEDIR-PATHNAME
    VALUES
    ;; VALUES-LIST
    VECTOR
    ;; VECTOR-POP
    ;; VECTOR-PUSH
    ;; VECTOR-PUSH-EXTEND
    ;; VECTORP
    ;; WARN
    ;; WILD-PATHNAME-P
    ;; WRITE
    ;; WRITE-BYTE
    ;; WRITE-CHAR
    ;; WRITE-LINE
    ;; WRITE-SEQUENCE
    ;; WRITE-STRING
    ;; WRITE-TO-STRING
    ;; Y-OR-N-P
    ;; YES-OR-NO-P
    ZEROP
    ))

(defparameter *useful-cl-variables*
  '(**
    ***
    ++
    +++
    //
    ///
    ;; *BREAK-ON-SIGNALS*
    ;; *COMPILE-FILE-PATHNAME*
    ;; *COMPILE-FILE-TRUENAME*
    ;; *COMPILE-PRINT*
    ;; *COMPILE-VERBOSE*
    ;; *DEBUG-IO*
    ;; *DEBUGGER-HOOK*
    ;; *DEFAULT-PATHNAME-DEFAULTS*
    ;; *ERROR-OUTPUT*
    ;; *FEATURES*
    ;; *GENSYM-COUNTER*
    ;; *LOAD-PATHNAME*
    ;; *LOAD-PRINT*
    ;; *LOAD-TRUENAME*
    ;; *LOAD-VERBOSE*
    ;; *MACROEXPAND-HOOK*
    ;; *MODULES*
    ;; *PACKAGE*
    ;; *PRINT-ARRAY*
    ;; *PRINT-BASE*
    ;; *PRINT-CASE*
    ;; *PRINT-CIRCLE*
    ;; *PRINT-ESCAPE*
    ;; *PRINT-GENSYM*
    *PRINT-LENGTH*
    *PRINT-LEVEL*
    *PRINT-LINES*
    ;; *PRINT-MISER-WIDTH*
    ;; *PRINT-PPRINT-DISPATCH*
    *PRINT-PRETTY*
    ;; *PRINT-RADIX*
    ;; *PRINT-READABLY*
    ;; *PRINT-RIGHT-MARGIN*
    ;; *QUERY-IO*
    ;; *RANDOM-STATE*
    ;; *READ-BASE*
    ;; *READ-DEFAULT-FLOAT-FORMAT*
    ;; *READ-EVAL*
    ;; *READ-SUPPRESS*
    ;; *READTABLE*
    *STANDARD-INPUT*
    *STANDARD-OUTPUT*
    ;; *TERMINAL-IO*

    ))

(defparameter *useful-cl-constants* 
  '(
    ;; ARRAY-DIMENSION-LIMIT
    ;; ARRAY-RANK-LIMIT
    ;; ARRAY-TOTAL-SIZE-LIMIT
    ;; BOOLE-1
    ;; BOOLE-2
    ;; BOOLE-AND
    ;; BOOLE-ANDC1
    ;; BOOLE-ANDC2
    ;; BOOLE-C1
    ;; BOOLE-C2
    ;; BOOLE-CLR
    ;; BOOLE-EQV
    ;; BOOLE-IOR
    ;; BOOLE-NAND
    ;; BOOLE-NOR
    ;; BOOLE-ORC1
    ;; BOOLE-ORC2
    ;; BOOLE-SET
    ;; BOOLE-XOR
    ;; CALL-ARGUMENTS-LIMIT
    ;; CHAR-CODE-LIMIT
    ;; DOUBLE-FLOAT-EPSILON
    ;; DOUBLE-FLOAT-NEGATIVE-EPSILON
    ;; INTERNAL-TIME-UNITS-PER-SECOND
    ;; LAMBDA-LIST-KEYWORDS
    ;; LAMBDA-PARAMETERS-LIMIT
    ;; LEAST-NEGATIVE-DOUBLE-FLOAT
    ;; LEAST-NEGATIVE-LONG-FLOAT
    ;; LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT
    ;; LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT
    ;; LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT
    ;; LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT
    ;; LEAST-NEGATIVE-SHORT-FLOAT
    ;; LEAST-NEGATIVE-SINGLE-FLOAT
    ;; LEAST-POSITIVE-DOUBLE-FLOAT
    ;; LEAST-POSITIVE-LONG-FLOAT
    ;; LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT
    ;; LEAST-POSITIVE-NORMALIZED-LONG-FLOAT
    ;; LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT
    ;; LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT
    ;; LEAST-POSITIVE-SHORT-FLOAT
    ;; LEAST-POSITIVE-SINGLE-FLOAT
    ;; LONG-FLOAT-EPSILON
    ;; LONG-FLOAT-NEGATIVE-EPSILON
    ;; MOST-NEGATIVE-DOUBLE-FLOAT
    ;; MOST-NEGATIVE-FIXNUM
    ;; MOST-NEGATIVE-LONG-FLOAT
    ;; MOST-NEGATIVE-SHORT-FLOAT
    ;; MOST-NEGATIVE-SINGLE-FLOAT
    ;; MOST-POSITIVE-DOUBLE-FLOAT
    ;; MOST-POSITIVE-FIXNUM
    ;; MOST-POSITIVE-LONG-FLOAT
    ;; MOST-POSITIVE-SHORT-FLOAT
    ;; MOST-POSITIVE-SINGLE-FLOAT
    ;; MULTIPLE-VALUES-LIMIT
    NIL
    PI
    ;; SHORT-FLOAT-EPSILON
    ;; SHORT-FLOAT-NEGATIVE-EPSILON
    ;; SINGLE-FLOAT-EPSILON
    ;; SINGLE-FLOAT-NEGATIVE-EPSILON
    T
    ))


(defparameter *useful-cl-types*
  '(
    ;; ARITHMETIC-ERROR
    ARRAY
    ;; BASE-CHAR
    ;; BASE-STRING
    ;; BIGNUM
    ;; BIT-VECTOR
    BOOLEAN
    ;; BROADCAST-STREAM
    ;; BUILT-IN-CLASS
    ;; CELL-ERROR
    ;; CLASS
    ;; COMPILED-FUNCTION
    ;; CONCATENATED-STREAM
    ;; CONDITION
    ;; CONTROL-ERROR
    ;; DIVISION-BY-ZERO
    ;; DOUBLE-FLOAT
    ;; END-OF-FILE
    ;; EXTENDED-CHAR
    ;; FILE-ERROR
    ;; FILE-STREAM
    FIXNUM
    ;; FLOATING-POINT-INEXACT
    ;; FLOATING-POINT-INVALID-OPERATION
    ;; FLOATING-POINT-OVERFLOW
    ;; FLOATING-POINT-UNDERFLOW
    ;; HASH-TABLE
    INTEGER
    ;; KEYWORD
    ;; LONG-FLOAT
    ;; METHOD
    ;; METHOD-COMBINATION
    NUMBER
    ;; PACKAGE
    ;; PACKAGE-ERROR
    ;; PARSE-ERROR
    ;; PRINT-NOT-READABLE
    ;; PROGRAM-ERROR
    ;; RANDOM-STATE
    ;; RATIO
    ;; READER-ERROR
    ;; READTABLE
    ;; REAL
    ;; RESTART
    ;; SATISFIES
    SEQUENCE
    ;; SERIOUS-CONDITION
    ;; SHORT-FLOAT
    ;; SIGNED-BYTE
    ;; SIMPLE-ARRAY
    ;; SIMPLE-BASE-STRING
    ;; SIMPLE-BIT-VECTOR
    ;; SIMPLE-CONDITION
    ;; SIMPLE-ERROR
    ;; SIMPLE-STRING
    ;; SIMPLE-TYPE-ERROR
    ;; SIMPLE-VECTOR
    ;; SIMPLE-WARNING
    SINGLE-FLOAT
    ;; STANDARD-CHAR
    ;; STANDARD-CLASS
    ;; STANDARD-GENERIC-FUNCTION
    ;; STANDARD-METHOD
    ;; STANDARD-OBJECT
    ;; STORAGE-CONDITION
    ;; STREAM
    ;; STREAM-ERROR
    ;; STRING-STREAM
    ;; STRUCTURE
    ;; STRUCTURE-CLASS
    ;; STRUCTURE-OBJECT
    ;; STYLE-WARNING
    ;; SYMBOL
    ;; SYNONYM-STREAM
    ;; TYPE-ERROR
    ;; UNBOUND-SLOT
    ;; UNBOUND-VARIABLE
    ;; UNDEFINED-FUNCTION
    ;; UNSIGNED-BYTE
    ;; WARNING
    ))

(defparameter *useful-cl-other-symbols* 
  '(
    &ALLOW-OTHER-KEYS
    &AUX
    &BODY
    ;; CALL-METHOD
    ;; CALL-NEXT-METHOD
    ;; COMPILATION-SPEED
    ;; COMPILER-MACRO
    ;; DEBUG
    ;; DECLARATION
    ;; DYNAMIC-EXTENT
    ;; ECHO-STREAM
    &ENVIRONMENT
    ;; FTYPE
    IGNORABLE
    ;; IGNORE
    ;; INLINE
    &KEY
    ;; MAKE-METHOD
    ;; NEXT-METHOD-P
    ;; NOTINLINE
    ;; OPTIMIZE
    &OPTIONAL
    ;; OTHERWISE
    &REST
    ;; SAFETY
    ;; SPACE
    ;; SPECIAL
    ;; SPEED
    ;; STANDARD
    ;; TWO-WAY-STREAM
    ;; TYPE
    ;; VARIABLE
    &WHOLE
    ))



(defvar *common-lisp-special-operators-and-macros* 
  '(
    common-lisp:and 
    common-lisp:assert 
    common-lisp:block 
    common-lisp:case 
    common-lisp:catch 
    common-lisp:ccase 
    common-lisp:check-type 
    common-lisp:cond 
    common-lisp:ctypecase 
    common-lisp:decf 
    common-lisp:declaim 
    common-lisp:defclass 
    common-lisp:defconstant 
    common-lisp:defgeneric 
    common-lisp:define-compiler-macro 
    common-lisp:define-condition 
    common-lisp:define-method-combination 
    common-lisp:define-modify-macro 
    common-lisp:define-setf-expander 
    common-lisp:define-symbol-macro 
    common-lisp:defmacro 
    common-lisp:defmethod 
    common-lisp:defpackage 
    common-lisp:defparameter 
    common-lisp:defsetf 
    common-lisp:defstruct 
    common-lisp:deftype 
    common-lisp:defun 
    common-lisp:defvar 
    common-lisp:destructuring-bind 
    common-lisp:do 
    common-lisp:do* 
    common-lisp:do-all-symbols 
    common-lisp:do-external-symbols 
    common-lisp:do-symbols 
    common-lisp:dolist 
    common-lisp:dotimes 
    common-lisp:ecase 
    common-lisp:etypecase 
    common-lisp:eval-when 
    common-lisp:flet 
    common-lisp:formatter 
    common-lisp:function 
    common-lisp:go 
    common-lisp:handler-bind 
    common-lisp:handler-case 
    common-lisp:if 
    common-lisp:ignore-errors 
    common-lisp:in-package 
    common-lisp:incf 
    common-lisp:labels 
    common-lisp:lambda 
    common-lisp:let 
    common-lisp:let* 
    common-lisp:load-time-value 
    common-lisp:locally 
    common-lisp:loop 
    common-lisp:loop-finish 
    common-lisp:macrolet 
    common-lisp:multiple-value-bind 
    common-lisp:multiple-value-call 
    common-lisp:multiple-value-list 
    common-lisp:multiple-value-prog1 
    common-lisp:multiple-value-setq 
    common-lisp:nth-value 
    common-lisp:or 
    common-lisp:pop 
    common-lisp:pprint-logical-block 
    common-lisp:print-unreadable-object 
    common-lisp:prog 
    common-lisp:prog* 
    common-lisp:prog1 
    common-lisp:prog2 
    common-lisp:progn 
    common-lisp:progv 
    common-lisp:psetf 
    common-lisp:psetq 
    common-lisp:push 
    common-lisp:pushnew 
    common-lisp:quote 
    common-lisp:remf 
    common-lisp:restart-bind 
    common-lisp:restart-case 
    common-lisp:return 
    common-lisp:return-from 
    common-lisp:rotatef 
    common-lisp:setf 
    common-lisp:setq 
    common-lisp:shiftf 
    common-lisp:step 
    common-lisp:symbol-macrolet 
    common-lisp:tagbody 
    common-lisp:the 
    common-lisp:throw 
    common-lisp:time 
    common-lisp:trace 
    common-lisp:typecase 
    common-lisp:unless 
    common-lisp:untrace 
    common-lisp:unwind-protect 
    common-lisp:when 
    common-lisp:with-accessors 
    common-lisp:with-compilation-unit 
    common-lisp:with-condition-restarts 
    common-lisp:with-hash-table-iterator 
    common-lisp:with-input-from-string 
    common-lisp:with-open-file 
    common-lisp:with-open-stream 
    common-lisp:with-output-to-string 
    common-lisp:with-package-iterator 
    common-lisp:with-simple-restart 
    common-lisp:with-slots 
    common-lisp:with-standard-io-syntax 
    ))