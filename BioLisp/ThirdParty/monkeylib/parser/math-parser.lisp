;; math-parser.lisp -- An example of using PARSER to build up objects
;; as the parse proceeds. The parser ARITHMETIC parses simple
;; arithmetic statements in infix notation (+, -, *, /, and ()'s) and
;; returns a tree of symbols for the operators and numbers in prefix
;; form (i.e. appropriate for passing to Lisp's eval.)
;;
;; Copyright (c) 2003-2006 Peter Seibel

(in-package :com.gigamonkeys.math-parser)

(defchartype digit     '(satisfies digit-char-p))
(defchartype term-op   '(member #\+ #\-))
(defchartype factor-op '(member #\/ #\*))

(defprod number (d)
  (+ (^ (@ digit (setq d (digit-char-p digit)))
        (+ (* 10 (or number 0)) d))))

(defprod expression ()
  ((^ term)
   (* (^ (term-op term)
         (list (find-symbol (string term-op) :common-lisp) expression term)))))

(defprod term ()
  ((^ factor)
   (* (^ (factor-op factor)
         (list (find-symbol (string factor-op) :common-lisp) term factor)))))

(defprod factor ()
  (/ (^ number)
     (#\( (^ expression) #\))))
  
(defparser arithmetic (^ expression))

(defun calculator (string)
  "Parse the given string and evaluate it as an arithmetic expression."
  (multiple-value-bind (ok tree) (arithmetic string)
    (when ok (eval tree))))

(defmacro infix (string)
  (multiple-value-bind (ok tree) (arithmetic string)
    (when ok tree)))

