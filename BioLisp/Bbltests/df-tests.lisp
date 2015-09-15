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

   
(tests:deftest dc1 (z-dconvert 3.0 3.0 'a 3 '(a b c) 'b) 
               '(3 3 "A" "3" #(1 2 3) B)
               :chapter :df :comparison 'equalp)

(tests:deftest e-dc1 (handler-case 
                         (z-dconvert 3.0 3.0 'a 3 '(a b c) 9)
                       (df-execute-error () 9))
               9
               :chapter :df)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; VALID CALL TESTS 

;; 1 required arg
(defdftest r1
  (z-1-required-arg 19)
  28)

;; 2 required args
(defdftest r2
           (z-2-required-args 3 4)
           7)

;; 3 required args and unused flag
(defdftest r3
           (z-3-required-args 2 3 4)
           192)

;; 3 required args and used flag
(defdftest r4
           (z-3-required-args 2 3 4 a)
           216)
;; no required args
(defdftest r5
           (z-no-required-args)
           12)
(defdftest r6
           (z-no-required-args a 9)
           18)


;; different combos of tokens
(defdftest t1
           (z-basic-tokens 1 2 3)
           (list 1 2 3))
(defdftest t2
           (z-basic-tokens 1 in 5 this 28 other)
           (list 1 5 28))
(defdftest t3
           (z-basic-tokens 1 5 this 28 the thing)
           (list 1 5 28))
(defdftest t4
           (z-basic-tokens 'muenster in 'cheddar that 'gouda)
           (list 'muenster 'cheddar 'gouda))
(defdftest t5
           (z-basic-tokens pi pi this pi)
           (list pi pi pi))



;; testing that token variables are bound to T or NIL depending on whether
;; they're mentioned in the call
(defdftest t6
           (z-harder-tokens 0 0 0)
           0)
(defdftest t7
           (z-harder-tokens 0 in 0 this 0 and)
           3)
(defdftest t8
           (z-harder-tokens 0 0 that 0 the other)
           3)
(defdftest t9
           (z-harder-tokens 0 in 0 this 0 the thing)
           4)
(defdftest t10
  (let ((in 3))
    (declare (ignore in))
    (z-token-check 'a in 'b that 'c thing))
  '(a b c t nil t nil nil nil t))

(defdftest t11
  (z-big-token-check 3 4 5)
  '(6 8 10 16 nil nil nil nil nil))
  
(defdftest t12
  (let ((ignoring 3))
    (declare (ignore ignoring))
    (z-big-token-check 3 ignoring 4 5))
  '(6 8 10 16 t nil t nil nil))
 
(defdftest t13
  (let ((ignoring '(1 2 3)) (pay 6))
    (declare (ignore ignoring pay))
    (z-big-token-check 2 ignoring 3 pay 4 kiy 9 stripes?))
  '(4 6 8 10 18 nil t t nil))

;; make sure unspecified key words are nil
(defdftest k1
           (z-unbound-keyword)
           (list nil))

;; user's keyword value
(defdftest k2
           (z-unbound-keyword a 3)
           (list 3))

;; keyword default value
(defdftest k3
           (z-bound-keyword)
           26)
;; keyword aliases
(defdftest k4
           (z-bound-keyword num 3)
           6)

;; 1 required, 1 unused keyowrd
(defdftest k5
           (z-keyword-aliases 5)
           1000)

;; 1 required, use keyword alias
(defdftest k6
           (z-keyword-aliases 8 z 3)
           27)
;; 1 required, use other alias
(defdftest k7
  (z-keyword-aliases 12 y 2)
  8)


;; testing keywords and alias variations amongst flags 
(defdftest k8
  (=
   (z-flag-n-key flaggy)
   (z-flag-n-key))
  t)
(defdftest k9
  (= (z-flag-n-key b 5 e 9)
     (z-flag-n-key))
  t)
(defdftest k10
  (z-flag-n-key a 4)
  36)
(defdftest k11
  (z-flag-n-key a 4 flaggy)
  45)
(defdftest k12
  (z-flag-n-key b 14)
  126)
(defdftest k13
  (z-flag-n-key b 14 flaggy)
  45)
(defdftest k14
  (z-flag-n-key a 12 d 19)
  228)
(defdftest k15
  (z-flag-n-key a 12 d 19 flaggy)
  95)
(defdftest k16
  (z-flag-n-key a 3 e 6)
  18)
(defdftest k17
  (z-flag-n-key a 3 e 6 flaggy)
  30)
(defdftest k18
  (z-flag-n-key b 1 d 78)
  78)
(defdftest k19
  (z-flag-n-key b 1 d 78 flaggy)
  390)
(defdftest k20
  (z-flag-n-key b 2 e 82)
  164)

;; flag arguments
(defdftest f1
           (z-1-flag-arg 5 6)
           6)
(defdftest f2
           (z-1-flag-arg 5 6 return-8)
           8)
(defdftest f3
           (z-3-flag-args)
           '(nil nil))
(defdftest f4
           (z-3-flag-args-b)
           '(nil nil))
(defdftest f5
           (z-3-flag-args x)
           '(nil t))
(defdftest f6
           (z-3-flag-args-b x)
           '(nil t))
(defdftest f7
           (z-3-flag-args x y)
           '(nil t))
(defdftest f8
           (z-3-flag-args-b x y)
           '(nil t))
(defdftest f9
           (z-3-flag-args x y z)
           '(t t))
(defdftest f10
           (z-3-flag-args-b x y z)
           '(t t))
(defdftest f11
  (z-just-a-flag)
  4)
(defdftest f12
  (z-just-a-flag a)
  3)
(defdftest f13
  (z-just-lots-of-flags h)
  4)
(defdftest f14
  (z-just-lots-of-flags h i j)
  3)

;; function name aliases
(defdftest alias
           (z-funky)
           18)

;; check required argument types
(defdftest reqtype1
  (z-2-required-types 3 (coerce 6 'float))
  9.0)
(defdftest reqtype1a
  (z-2-required-types-a 3 4)
  7)
(defdftest reqtype1b
  (z-2-required-types-b 3 4.0)
  7.0)
(defdftest reqsyn1 
  (z-required-syntax-1 1 2 3 4.0 5)
  15.0)
(defdftest reqsyn2
  (z-required-syntax-1 1 2 3 4.0 5)
  15.0)
(defdftest reqtype2
  (z-3-required-types "a16" 3.0 16)
  (list "a16" 3.0 16))

;; make sure OR syntax is OK
(defdftest reqtype3
           (z-or-types 8 8)
  5)
(defdftest reqtype4
  (z-or-types #'first 8)
  5)

;; check keyword types
(defdftest k21
  (z-flag-n-key b 2 e 82 flaggy)
  410)
(defdftest k22
  (z-1-key-type)
  '(3))
(defdftest k23
  (z-1-key-type a 19)
  '(19))
(defdftest k24
  (z-3-key-types 100 a 'cjk b "mmm" c 1000 d 13)
  '(cjk "mmm" 1000 13 100))

;; check return types
(defdftest rettype1
  (z-1-required-return 3)
  9)

(defdftest rettype2
  (z-nil-required-return)
  (string 'abc))

;; check conversion
(defdftest conv1
           (z-convert-1-required '(#\a #\b #\c) "def")
           "abc def")
(defdftest conv2
  (z-adefconversion 'kij 'mnu)
  (list "KIJ" "MNU"))
(defdftest conv3
           (z-another-convert "k" "b")
           (list #\k #\b))

(defdftest conv4 
  (z-convert-2-requireds-a 3 4)
  (list "3" "4"))

(defdftest conv5
  (z-convert-2-requireds-b 3 4)
  (list "3" "4"))

(defdftest conv6
  (z-convert-2-requireds-c 4 5 6)
  (list "4" "5" "6"))



(defdftest dc2 
  (z-default-vs-named-dc 3 6)
  '((3) ((6))))

(defdftest dc3 
  (z-default-vs-named-dc2 b 3.9)
  '(2 3 4))

(tests:deftest dc4  
               (z-multiple-convert-options 5 '(1 2 3))
               '(5.0 #(1 2 3))
               :chapter :df :comparison 'equalp)

(tests:deftest dc5 
               (z-multiple-convert-options2 2 '(5 6))
               '(2 #(5 6))
               :chapter :df :comparison 'equalp)

(defdftest dc6
           (z-fib 2.4 9 t nil)
           '(nil nil 1.0 0))
(defdftest dc7
           (z-fib 15.9 2 nil t)
           '(t t 0.0 1))
                    

;; correct mapcar calls
(defdftest mc1
           (z-mapcar-2-requireds '(1 2 3) '(4 5 6))
           '(5 7 9))
(defdftest mc2
           (z-mapcar-2-requireds '(1 2 3) 4)
           '(5 6 7))
(defdftest mc3
           (z-mapcar-2-requireds 1 '(1 2 3))
           '(2 3 4))
(defdftest mc4
           (z-mapcar-2-requireds 4 5)
           9)
(defdftest mc5
  (z-mapcar-3-requireds '(1 2 3) '(4 5 6) '(7 8 9))
  '(12 15 18))
(defdftest mc6
  (z-mapcar-3-requireds '(1 2 3) '(4 5 6) '(7 8 9) a)
  '((1 4 7) (2 5 8) (3 6 9)))

(defdftest mc7
          (every
           (lambda (x) (equal x (list 10)))
           (list
            (z-mapcar-3-requireds '(1 2 3) '(4) 5)
            (z-mapcar-3-requireds '(1 2 3) 5 '(4))
            (z-mapcar-3-requireds 5 '(1 2 3) '(4))
            (z-mapcar-3-requireds 5 '(4) '(1 2 3))
            (z-mapcar-3-requireds '(4) '(1 2 3) 5)
            (z-mapcar-3-requireds '(4) 5 '(1 2 3))))
          t)
(defdftest mc8 
           (z-mapcar-3-requireds '(1 2 3) '(4 5) 5 a)
           '((1 4 5) (2 5 5)))
(defdftest mc9
           (z-mapcar-3-requireds '(1 2 3) 5 '(4) a) 
           '((1 5 4)))
(defdftest mc10
           (z-mapcar-3-requireds 5 '(1 2 3) '(4) a)
           '((5 1 4)))
(defdftest mc11
           (z-mapcar-3-requireds 5 '(4) '(1 2 3) a)
           '((5 4 1)))
(defdftest mc12
           (z-mapcar-3-requireds '(4) '(1 2 3) 5 a) 
           '((4 1 5)))
(defdftest mc13
           (z-mapcar-3-requireds '(4) 5 '(1 2 3) a)
           '((4 5 1)))

;; no map-carred args
(defdftest mc14
  (z-nomap)
  18)
(defdftest mc15
  (z-nomap a 3)
  nil)

;; correct maptree calls
(defdftest mt1
           (z-maptree-1-required '((1 2) (3 (4 5)) 6))
           '((2 3) (4 (5 6)) 7))

(defdftest mt2 (z-maptree-2-of-3-requireds '(1 (2 (3 (4 5 (6 7)))) 9)
                        '(2 (3 (4 (5 6 (7 8)))) 10)
                        1)
  '(4 (6 (8 (10 12 (14 16)))) 20))

(defdftest mt3
  (z-maptree-4-requireds '(1 (2 (3 4)) 5)
               '(2 (3 (4 5)) 6)
               '(3 (4 (5 6)) 7)
               '(4 (5 (6 7)) 8))
  '(10 (14 (18 22)) 26))

(defdftest mt4
  (z-maptree-4-requireds 
    '(1 (2 (3 4)) 5)
    2
    '(3 (4 (5 6)) 7)
    3)
  '(9 (11 (13 15)) 17))

(defdftest mt5
  (z-maptree-4-requireds 1 2 3 4)
  10)

(defdftest mt6
  (z-maptree-4-requireds 1 2 3 '(8 (8 8 (8 8))))
  '(14 (14 14 (14 14))))

;; no map-treed args
(defdftest mt7
  (z-nomaptree)
  49)

;; correct crossmap calls
(defdftest cm1
           (z-crossmap-2-requireds 3 4)
           '(3 4))
(defdftest cm2
           (z-crossmap-2-requireds '(1 2) '(3 4))
           '((1 3) (1 4) (2 3) (2 4)))
(defdftest cm3
           (z-crossmap-2-requireds 4 '(1 2 3))
           '((4 1) (4 2) (4 3)))

;; local variables
(defdftest init1
           (z-1-basic-init '(1.1 2.2 3.3))
           '(34.1 35.2 36.3))
(defdftest init2
  (z-2-basic-inits)
  7)

(defdftest init2a 
  (z-2-basic-inits-a)
  7)
(defdftest init2b
  (z-2-basic-inits-b)
  27)
(defdftest init2c
  (z-2-basic-inits-c 5)
  23)
(defdftest init2d
  (z-2-basic-inits-c 5)
  23)

(defdftest init3
  (z-complex-inits 0 0 0 x y)
  '(0 6 8))
(defdftest init4
  (z-complex-inits 6 5 4 alpha -1 beta -2 gamma -3 y)
  '(-90 nil 3))

;; correct defun-like calls
(defdftest dl1
           (z-easy-defun 3 4)
           7)
(defdftest dl2 (z-defun-no-requireds) 13)
(defdftest dl3
  (z-complex-defun-1 6 7 8 9 1 1 1 1)
  '(6 7 8 9 (1 1 1 1)))
(defdftest dl4
  (z-complex-defun-1 9 10 11)
  '(9 10 11 4 nil))
;; seibel 5
(defdftest dl5
  (z-complex-defun-2 :a 1 :b 2 :c 3) 
  '((:A 1 :B 2 :C 3) 1 2 3))
(defdftest dl6
  (z-complex-defun-2)
  '(nil nil nil nil))
(defdftest dl7
  (z-complex-defun-3) 18)
(defdftest dl8
  (z-complex-defun-3 :c 9) 28)
(defdftest dl9
  (z-complex-defun-3 :a 19 :b 2 :c 4 :d 100) 125)
(defdftest dl10
  (and 
   (= (z-defun-like-alias 16 17) (z-dfn-lk-als 16 17 10))
   (= (z-defun-like-alias 1 2 3) (z-dfn-lk-als 1 2 3)))
  t)

(defdftest fa1
  (z-first-assault "a" #\b 23)
  (list "a" #\b 46 48 16 6))
(defdftest fa2
  (z-first-assault "b" #\c 12 stars?)
  (list "b" #\c 24 48 10 6))
(defdftest fa3
  (z-first-assault 12 ignore 2 3 stripes?)
  (list 24 4 6 48 16 4))
(defdftest fa4
  (z-first-assault 'alpha ignoring 'beta 'gamma stars? stripes?)
  (list 'alpha 'beta 'gamma 48 10 4))
(defdftest fa5
  (z-first-assault "a" ignore "b" "c" stars 12 strps 100)
  (list "a" "b" "c" 48 24 200))
(defdftest fa6
  (z-first-assault 5 6 7 stars 55 stripes 44 stars? stripes?)
  (list 10 12 14 48 110 88))
(defdftest fa7
  (z-first-assault 5 6 7 stars 12 stripes?)
  (list 10 12 14 48 24 4))

;; recursive calls
(defdftest fac1
           (z-test-factorial 7)
           5040)
(defdftest fac2
           (z-test-factorial (z-test-factorial 3))
           720)
(defdftest fac3
           (z-test-factorial 1)
           1)
(defdftest fac4
           (z-test-factorial 0)
           nil)

;; calling other define-test-functions 
(defdftest odf1
           (z-call-other-df 1 1)
           1)
(defdftest odf2
           (z-call-other-df (z-test-factorial 3) 3)
           2160)

;; safety tests 

(defdftest safe1
           (let ((*safety* nil)) (z-2-required-types 8.0 7.0d0)) 15.0d0)

(defdftest safe2
           (let ((*safety* nil)) (z-3-key-types 'a b 12))
           '(aaa 12 33 nil a))

(defdftest safe3 
           (let ((*safety* nil)) (z-2-required-return 1 1))
           '(1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ERROR INTERCEPTION
;; e stands for error
(defdftest e-r1
  (handler-case (me1 '(z-3-required-args 1 1 a 1))
    (df-call-error () 19))
  19)
(defdftest e-r2
  (handler-case (me1 '(z-2-required-args 1))
    (df-call-error () 2))
  2)
(defdftest e-r3
  (handler-case  (me1 '(z-2-required-args 1 2 3 4))
    (df-call-error () 34))
  34)
;; breaking the required type 
(defdftest e-r4 
  (handler-case (z-2-required-types 'a 'b)
    (df-execute-error () 8))
  8)
(defdftest e-r5
  (handler-case (z-3-required-types "d" 3.0 'l)
    (df-execute-error () 9))
  9)

;; breaking both types
(defdftest e-rk1
  (handler-case (z-3-key-types 'a b 12)
    (df-execute-error () 18))
  18)  

;; breaking return types
(defdftest e-rettype1
  (handler-case (z-1-required-return 3.1)
    (df-execute-error () 87))
  87)
(defdftest e-rettype2
 (handler-case (z-2-required-return 1 1)
   (df-execute-error () 2))
 2)

;; token in wrong place
(defdftest e-t1
  (handler-case (me1 '(z-basic-tokens 1 this 2 3))
    (df-call-error () 5))
  5)
;; bad tokens
(defdftest e-t2
  (handler-case (me1 '(z-basic-tokens 'a 'b this that 'c the other))
    (df-call-error () 19))
  19)
(defdftest e-t3
  (handler-case (me1 '(z-basic-tokens 1 2 this))
    (df-call-error () 22))
  ; 22    ***** Change in VERIFY-DF-CALL causes THIS to be interpretted as 3rd argument, if no other choice
  (list 'DF-FUNCTION-FOR-Z-BASIC-TOKENS 1 2 'this NIL NIL NIL NIL NIL NIL NIL))
(defdftest e-t4
  (handler-case
      (me1 '(z-basic-tokens 'a in 'b this 'c the thing extra))
    (df-call-error () 88))
  88)
(defdftest e-t5
  (handler-case
     (me1 '(z-basic-tokens))
  (df-call-error () 12))
  12)


;; using multiple keyword aliases in a call
(defdftest e-k1
           (handler-case 
               (me1 '(z-keyword-aliases 5 y 8 z 9))
             (df-call-error () 42))
           42)

;; breaking the keyword type
(defdftest e-k2
  (handler-case (z-1-key-type a 3.3)
    (df-execute-error () 8))
  8)
(defdftest e-k3
  (handler-case (z-3-key-types 100 a 19 b 12 c 'dff)
    (df-execute-error () 3))
  3)
(defdftest e-k4
  (handler-case (z-3-key-types 100 c 32.2)
    (df-execute-error () 88))
  88)

(defdftest e-f1
  (handler-case (me1 '(z-3-flag-args-b x x))
    (df-call-error () 87))
  87)

(defdftest e-f2
  (handler-case (me1 '(z-3-flag-args-b x x y y z z))
    (df-call-error () 88))
  88)

(defdftest e-f3
  (handler-case (me1 ' (z-3-flag-args-b x y z z))
    (df-call-error () 87))
  87)

;; can't convert numbers to strings using COERCE
(defdftest e-conv1
  (handler-case (z-convert-2-requireds 3 5)
    (df-execute-error () 99))
  99)

(defdftest e-conv2
  (handler-case (z-yet-another-convert 3 '(1 2 3) 'def)
    (df-execute-error () 12))
  12)

(tests:deftest
 e-dc1
 (handler-case 
     (z-dconvert 3.0 3.0 'a 3 '(a b c) 9)
   (df-execute-error () 9))
 9
 :chapter :df)

;; specified conversion errors out
(defdftest e-dc2
 (handler-case 
     (z-spef-dc-returns-error 4)
   (df-execute-error () 12))
 12)                         

;; defun-like with wrong number of arguments
(defdftest e-dl1
           (handler-case 
               (eval '(z-defun-like-1 2 9 3))
             (error () 67))
           67)
(defdftest e-dl2 
           (handler-case (eval '(z-defun-like-2 54))
             (error () 9))
           9)

(defdftest e-dl3
  (handler-case (z-complex-defun-2 5)
    (error () 9))
  9)

(defdftest e-gen1
  (handler-case (me1 '(z-df-call-fun 1 flaggy 1)) 
    (df-call-error () 3))
  3)
(defdftest e-gen2
  (handler-case (me1 '(z-df-call-fun flaggy 1 1))
    (df-call-error () 4))
  4)
(defdftest e-gen3
  (handler-case (me1 '(z-df-call-fun 1 a 3 1))
    (df-call-error () 19))
  19)
(defdftest e-gen4
  (handler-case (me1 '(z-df-call-fun 1 e 9 1))
    (df-call-error () 22))
  22)
(defdftest e-gen5
  (handler-case (me1 '(z-df-call-fun 'jk flaggy 'lm))
    (df-call-error () 20))
  20)
(defdftest e-gen6
  (handler-case (me1 '(z-df-call-fun 2 2 a))
    (df-call-error () 18))
  18)
(defdftest e-gen7
  (handler-case (me1 '(z-df-call-fun 2 2 a 3 d))
    (df-call-error () 990))
  990)
(defdftest e-gen8
  (handler-case (me1 '(z-df-call-fun 2 2 flaggy b))
    (df-call-error () 22))
  22)    
(defdftest e-gen9
  (handler-case (me1 '(z-df-call-fun-b 2 3 3 g))
    (df-call-error () 88))
  88)
(defdftest e-gen10
  (handler-case (me1 '(z-df-call-fun-b 2 3 3 h 9))
    (df-call-error () 1))
  1)
(defdftest e-gen11
  (handler-case
      (let ((i 21))
        (declare (ignore i))
        (me1 '(z-df-call-fun-b 2 3 3 i)))
    (df-call-error () 9))
  9)
(defdftest e-gen12
  (handler-case (me1 '(z-df-call-fun 2 4 5 a 3 a 5))
    (df-call-error () 1))
  1)
(defdftest e-gen13
  (handler-case (me1 '(z-df-call-fun 2 4 5 a 3 b 9))
    (df-call-error () 1))
  1)
(defdftest e-gen14
  (handler-case (me1 '(z-df-call-fun 2 a 2 b 5 d 12 e 45))
    (df-call-error () 1))
  1)
(defdftest e-mt1
  (handler-case (z-maptree-2-of-3-requireds '(1 (2 3)) '(2 (3 4 5)) 14)
    (df-execute-error () 9))
  9)



;;  =====================================================================
;;  JP additions


(defdftest df-return-test-01
  (z-df-return-test 0 23)
  5)

(defdftest df-return-test-02
  (z-df-return-test 1 1)
  2)

(defdftest df-map-return-test-01
  (z-df-map-return-test '(1 0 2) '(1 2 3))
  (list 2 5 5))

(defdftest map-and-type-1 
  (z-map-and-type-test-1 (list 1 2 3))
  '(2 3 4))


(defdftest map-and-type-2
  (progn
    (setq *count* 0)
    (z-map-and-type-test-2 3 '(3 4 5))
    *count*)
  4)

(defdftest map-and-type-3
  (handler-case 
      (z-map-and-type-test-2 3 '(3.0 4 5))
    (df-execute-error () 9))
  9)

(defdftest map-and-type-4
  (handler-case 
      (z-map-and-type-test-2 3 '(3 4 5.0))
    (df-execute-error () 9))
  9)

(defdftest map-and-coerce-1
  (z-map-and-coerce-test-1 (list 1.8 2.1 3.783))
  '(2 3 4))

(defdftest map-and-coerce-2
  (z-map-and-coerce-test-1 3.2)
  4)
  
(defdftest map-and-coerce-3
  (handler-case 
      (z-map-and-coerce-test-1 '("a" 3 4))
    (df-execute-error () 9))
  9)

(defdftest map-and-coerce-4
  (handler-case 
      (z-map-and-coerce-test-1 '(3 4 "a"))
    (df-execute-error () 9))
  9)

(defdftest mtc1 
  (z-map-type-coerce-1 3 4)
 7)

(defdftest mtc2 
  (z-map-type-coerce-1 3 4.2)
  7)

(defdftest mtc3
  (z-map-type-coerce-1 3 '(5 4.2))
  '(8 7))

(defdftest mtc4
  (z-map-type-coerce-1 '(4 5 6) '(2.1 3.8 4))
  '(6 9 10))

(defdftest mtc5
  (handler-case
      (z-map-type-coerce-1 '(4 5 6) '(2.1 3.8 #C(1.0 1.0)))
    (df-execute-error () 9))
  9)

(defdftest mtc6
  (handler-case
      (z-map-type-coerce-1 '(4 5.0 6) '(2.1 3.8 1.0))
    (df-execute-error () 9))
  9)

(defdftest mv1
  (multiple-value-list (z-mv-1 "3"))
  (list "3" 3))

(defdftest mv2
  (z-mv-1 '("a" "b"))
  (list "a" "b"))

(defdftest mv3
  (handler-case 
      (multiple-value-list (z-mv-1 3))
    (df-execute-error () 9))
  9)

(defdftest mv4
  (multiple-value-list (z-mv-2 "a" 2))
  (list "a" 2))

(defdftest mv5
  (handler-case 
      (multiple-value-list (z-mv-2 "a" "b"))
    (df-execute-error () 9))
  9)

(defdftest mv6
  (z-mv-2 '("a" "b") '(1 2))
  (list "a" "b"))