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

;;; Author: JP Massar, Mark Slupesky.

(defvar *saved-defconversion-hash* nil)

(defmacro defdftest (name &body body)
  `(tests:deftest ,name ,@body :chapter :df :comparison 'equal))

(defmacro define-test-function (name &rest args)
  `(progn 
     (define-function ,name ,@args)
     (help::undocument-function ,(if (listp name) (first name) name))
     ))

(defun me1 (form)
  (values (macroexpand-1 form)))

;; Warning.  You cannot run an individual test that uses any of the 
;; defconversion stuff.  The defconversion definitions only are in effect
;; when you do (run-chapter-tests :df), and this is caused by the 
;; setf forms immediately below, which stash away the current 
;; defconversion table and replace it with the tests conversion table.

(defun execute-df-tests-defconversion-forms ()
  (defconversion (float to boolean) (num) (if (> num 15) t nil))
  (defconversion (integer to boolean) (num) (if (evenp num) t nil))
  (defconversion (boolean to float) (b) (if b 1.0 0.0))
  (defconversion (boolean to integer) (b) (if b 1 0))
  (defconversion (float to integer) (f) (round f))
  (defconversion (float to integer named bb-truncate) (f) (truncate f))
  (defconversion (float to integer named bb-ceiling) (f) (ceiling f))
  (defconversion (t to string named alpha) (arg) (string arg))
  (defconversion (integer to string named num-to-string) (num) 
    (formatn "~S" num))
  (defconversion (list to vector named pointless)
      (list) 
    (declare (ignore list)) (vector 1 2 3))
  (defconversion (number to list named uhoh) (num) 
    (list (/ num 0)))
  (defconversion (integer to list) (num) (list num))
  (defconversion (integer to list named double) (num) (list (list num)))
  )
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS

(define-test-function z-1-required-arg
  required x 
  body (+ x 9))
(define-test-function z-2-required-args
  required (x y) 
  body (+ x y))
(define-test-function z-3-required-args
                 required x
                 required y
                 flag a
                 required z
                 body 
                  (* x y z (if a 9 8)))
(define-test-function z-no-required-args
                 keyword a = 3
                 body (+ a 9))

(define-test-function z-basic-tokens
                 required (r1 (in) r2 (this that) r3 (and the) (other thing))
                 body (list r1 r2 r3))
(define-test-function z-harder-tokens
                 required (r1 (in) r2 (this that) r3 (and the) (other thing))
                 body (flet ((b (x) (if x 1 0)))
                        (+ r1 r2 r3 (b in) (b this) (b that)
                           (b and) (b the) (b other) (b thing))))
(define-test-function z-token-check
                 required (r1 (in) r2 (this that) r3 (and the) (other thing))
                 body (list r1 r2 r3 in this that and the other thing))

(define-test-function z-big-token-check
                 required (a (ignore ignoring) b (pay attention) c)
                 flag stripes?
                 keyword 
                 (alpha = (if stripes? 5 8) kiy = ignoring)
                 body 
                 (mapcar (lambda (x) (if (numberp x) (* x 2) x)) 
                         (list a b c alpha kiy ignore ignoring pay attention)))

(define-test-function z-unbound-keyword
                 required ()
                 keyword a
                 body (list a))
(define-test-function z-bound-keyword
                 required ()
                 keyword num = 13
                 body (* 2 num))
(define-test-function z-keyword-aliases
                 required b
                 keyword (x y z) = 10
                 body (progn b) (expt x 3))
(define-test-function z-flag-n-key
                 flag flaggy
                 keyword ((a b) = 5 (d e) = 9)
                 body (* (if flaggy 5 a) d))

(define-test-function z-just-a-flag 
                 flag a 
                 body (if a 3 4))
(define-test-function z-just-lots-of-flags
                 flag (h i j)
                 body (if (or (and h i) (and h j)) 3 4))

(define-test-function z-1-flag-arg
                 required (x y)
                 flags (return-8)
                 body (progn (list x y) (if return-8 8 6)))
(define-test-function z-3-flag-args
                 required ()
                 flag x
                 flag y
                 flag z
                 body
                 (list (and x y z) (or x y z)))
(define-test-function z-3-flag-args-b
                 required ()
                 flag (x y z)
                 body (list (and x y z) (or x y z)))

(define-test-function (z-function-alias z-funky)
                 required ()
                 body (* 9 2))

(define-test-function z-2-required-types
                 required (x y)
                 type (x = integer y = float)
                 body (+ x y))

(define-test-function z-2-required-types-a
                 required (x y)
                 type (x y) = integer 
                 body (+ x y))

(define-test-function z-2-required-types-b
                 required (x y)
                 type x = integer y = float
                 body (+ x y))

(define-test-function z-required-syntax-1
                 required (x y z q r)
                 type (x y z) =integer q= float r=integer
                 body (+ x y z q r))

(define-test-function z-required-syntax-2
                 required (x y z q r)
                 type ((x y z) =integer q= float r=integer)
                 body (+ x y z q r))

(define-test-function z-3-required-types
                 required (a b c)
                 type (a = string b = float c = integer)
                 body (list a b c))

(define-test-function z-or-types
                 required (a b)
                 type (a = (or function integer) b = (or error integer))
                 body (progn a b) (+ 2 3))

(define-test-function z-1-key-type
                 keyword a = 3
                 type a = integer
                 body (list a))
(define-test-function z-3-key-types
                 required z
                 keyword (a = 'aaa b = "bbb" c = 33 d)
                 type (a = symbol b = string c = integer z = integer)
                 body (list a b c d z))

(define-test-function z-1-required-return
                 required a
                 return integer 
                 body (* a 3))
(define-test-function z-2-required-return
                 required (x y)
                 return vector
                 body (list x y))

(define-test-function z-nil-required-return
                 return string
                 body (string 'abc))

(define-test-function z-convert-1-required
                 required (s1 s2)
                 convert s1 from list to string
                 body (string-join (list s1 s2)))

(define-test-function z-convert-2-requireds
                 required (a b)
                 convert (a from fixnum to string b from fixnum to string)
                 body (list a b))

(define-test-function z-convert-2-requireds-a
                 required (a b)
                 convert (a b) from fixnum to string using num-to-string
                 body (list a b))

(define-test-function z-convert-2-requireds-b
                 required (a b)
                 convert a from fixnum to string using num-to-string
                         b from fixnum to string using num-to-string
                 body (list a b))

(define-test-function z-convert-2-requireds-c
                 required (a b c)
                 convert (a b) from fixnum to string using num-to-string
                         c from fixnum to string using num-to-string
                 body (list a b c))

(define-test-function z-yet-another-convert
  required (a b c)
  convert (a from integer to float b from list to error c from symbol to symbol) 
  body (list a b c))   

(define-test-function z-fib 
                 required (a b c d)
                 convert (a from float to boolean
                            b from integer to boolean
                            c from boolean to float
                            d from boolean to integer)
                 body (list a b c d))

(define-test-function z-default-vs-named-dc 
                 required (x y)
                 convert (x from integer to list 
                            y from integer to list using double)
                 body (list x y))
(define-test-function z-default-vs-named-dc2
                 keyword (a = 1.5 b = 2.9 c = 3.1)
                 convert a from float to integer
                 convert b from float to integer using bb-truncate
                 convert c from float to integer using bb-ceiling
                 body (list a b c))

(define-test-function z-multiple-convert-options
	required (x y)
	convert x from fixnum to single-float 
	convert x from integer to (complex single-float)
        convert y from cons to vector 
        convert y from list to vector 
        body (list x y)
	)
(define-test-function z-multiple-convert-options2
                 required (x y)
                 convert x from fixnum to single-float
                 convert y from cons to vector
                 convert y from list to vector
                 convert x from list to vector
                 body (list x y))

(define-test-function z-spef-dc-returns-error 
                 required x
                 convert x from number to list using uhoh
                 body x)

(define-test-function z-dconvert
  required (r1 r2 r3 r4 r5 r6)
  convert (r1 from float to integer using bb-truncate
              r2 from float to integer using bb-ceiling
              r3 from t to string using alpha
              r4 from integer to string using num-to-string
              r5 from list to vector using pointless
              r6 from number to list using uhoh)
  body (list r1 r2 r3 r4 r5 r6))


(define-test-function z-adefconversion
                 required (a b)
                 convert (a from t to string using alpha
                            b from t to string using alpha)
                 body (list a b))

(define-test-function z-another-convert
                 required (a b)
                 convert (a from string to list b from string to list)
                 body (append a b))

(define-test-function z-mapcar-2-requireds
                 required (x y)
                 map (x y)
                 body (+ x y))

(define-test-function z-maptree-1-required
                 required x
                 maptree x
                 body (1+ x))
(define-test-function z-mapcar-3-requireds 
                 required (x y z)
                 map (x y z)
                 flag a
                 body 
                 (forward-funcall (if a #'list #'+) x y z))

(define-test-function z-maptree-2-of-3-requireds
                 required (x y z)
                 maptree (x y)
                 body (+ x y z))

(define-test-function z-crossmap-2-requireds
                 required (x y)
                 cross-product (x y)
                 body (list x y))

(define-test-function z-nomap
                 required ()
                 keyword a
                 map ()
                 body (unless a (* 9 2)))

(define-test-function z-maptree-4-requireds 
                 required (a b c d)
                 map-into (a b c d)
                 body (+ a b c d))

(define-test-function z-nomaptree
                 maptree ()
                 body (* 7 7))

(define-test-function z-1-basic-init
                 required x
                 init h = 33
                 map x
                 body (+ x h))

(define-test-function z-2-basic-inits
                 required ()
                 initialize (b = 3 c = 4)
                 body (+ b c))

(define-test-function z-2-basic-inits-a
                 required ()
                 initialize b = 3 c = 4
                 body (+ b c))

(define-test-function z-2-basic-inits-b
                 required ()
                 initialize (b c d) = 9
                 body (+ b c d))

(define-test-function z-2-basic-inits-c
                 required a
                 initialize z = 8 (b c) =a
                 body (+ a z b c))

(define-test-function z-2-basic-inits-d
                 required a
                 initialize (z = 8 (b c) =a)
                 body (+ a z b c))


(define-test-function z-complex-inits
                 required (a b c)
                 flag (x y)
                 keyword (alpha = 2 beta = 3 gamma = 4)
                 init (h = (* (+ a b c) (* alpha beta gamma))
                         i = (when x (* alpha beta))
                         j = (when y (* gamma alpha)))
                 body (list h i j))

(define-test-function z-first-assault
                 required (a (ignore ignoring) b c)
                 init temp = 24
                 flag (stars? stripes?)
                 keyword 
                 (stars = (if stars? 5 8) (stripes strps) = (if stripes? 2 3))
                 body 
                 (mapcar (lambda (x) (if (numberp x) (* x 2) x)) 
                         (list a b c temp stars stripes)))

(define-test-function z-easy-defun (x y)
                 (+ x y))
(define-test-function z-defun-no-requireds nil (+ 7 6))
(define-test-function z-defun-with-key (x y &key z)
                 (list x y z))
(define-test-function z-complex-defun-1
                 (a b &optional c (d 4) &rest extras)
                 (list a b c d extras))
;; seibel 5
(define-test-function z-complex-defun-2 
                 (&rest rest &key a b c) 
                 (list rest a b c))

(define-test-function z-complex-defun-3
                 (&key (a 4) (b 6) (c a) (d c))
                 (+ a b c d))

(define-test-function (z-defun-like-alias z-dfn-lk-als)
                 (x y &optional (z 10))
                 (- x y z))

(define-test-function 
 z-defun-like-1 (x y)
 (+ x y))

(define-test-function 
 z-defun-like-2 (x y z)
 (+ x y z))
                      

(define-test-function z-test-factorial
                 required x
                 body 
                 (unless (< x 1)
                   (if (= x 1)
                       x
                     (* x (z-test-factorial (- x 1))))))

(define-test-function z-call-other-df 
                 required (a b)
                 body (* (z-test-factorial a) b))
(define-test-function z-df-call-fun
                 required (x y)
                 flag flaggy
                 keyword ((a b) = 5 (d e) = 9)
                 body (* (if flaggy 5 a) d x y))
(define-test-function z-df-call-fun-b
                 required (x y)
                 flag flaggy
                 keyword ((a b) = 5 (d e) = 9) 
                 body
                 (list (when flaggy flaggy) x y a d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun foo1 () nil)

(defun foo2 () nil)

(defun foo3 () nil)

(defun foo4 () nil)

;;  =====================================================================
;;  JP additions

(defvar *count* 0)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun weird-type-p (x) (incf *count*) (typep x 'fixnum)))

(deftype weird-type () `(satisfies weird-type-p))

(define-test-function z-df-return-test
  required (a b)
  body (if (zerop a) (return-from z-df-return-test 5) (+ a b)))


(define-test-function z-df-map-return-test
  required (a b)
  mapcar (a b)
  body (if (zerop a) (return-from z-df-map-return-test 5) (+ a b)))

(define-test-function z-map-and-type-test-1
  required a 
  mapcar a
  type a = fixnum
  body (+ 1 a))

(define-test-function z-map-and-type-test-2
  required (a b)
  mapcar (a b)
  type a = weird-type 
  type b = weird-type
  body (+ a b))

(define-test-function z-map-type-coerce-1
 required (a b)
 map (a b)
 type (a = integer b = integer)
 convert (b from float to integer)
 body (+ a b))
                
(define-test-function z-map-and-coerce-test-1
  required a 
  mapcar a
  type a = fixnum
  convert a from float to integer using bb-truncate 
  body (+ 1 a))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Multiple value tests 

(define-test-function 
 z-mv-1
 required a 
 map a 
 returns (values string number)
 body
 (values a 3))

(define-test-function 
 z-mv-2
 required (a b)
 map (a b)
 returns (values string number)
 body
 (values a b))

                      