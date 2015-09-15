;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio) 

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

;;; Until
(looptest 
 loop-basic-00
 (let ((j 0)) (xloop until (> j 3) do (incf j)) j)
 4)

;;; While
(looptest 
 loop-basic-01
 (let ((j 0)) (xloop while (< j 3) do (incf j)) j)
 3)

;;; RETURN
(looptest 
 loop-basic-02
 (let ((j 0)) (xloop do (incf j) (when (> j 10) (return 100))))
 100)

(looptest 
 loop-basic-02a
 (let ((j 0)) 
   (xloop for k from 0 to (if (zerop j) (return 23) 10) 
          do (setq j k)
          finally (return j)))
 23)


;;; INIT, FINALLY

(looptest 
 loop-basic-03
 (xloop init j = 0 until (> j 3) do (incf j) finally (return 23))
 23)

(looptest 
 loop-basic-04
 (xloop init j = 0 while (> j 3) do (incf j) finally (return 23))
 23)

;;; Implied DO
(looptest 
 loop-basic-05
 (xloop init j = 0 while (> j 3) (incf j) finally (return 23))
 23)

;;; Body not executed
(looptest 
 loop-basic-06
 (let ((k 0)) (xloop while (minusp k) (incf k)) k)
 0)

;;; Finally with no body
(looptest 
 loop-basic-07
 (let ((k 0)) (xloop while (minusp k) (incf k) finally (setq k 23)) k)
 23)

(looptest
 loop-init-01
 (xloop init j = 3 init k = 4 for z in nil do (print z) finally (return (+ j k)))
 7)

(looptest
 loop-init-02
 (xloop init (j k) = (list 3 4) for z in nil do (print z) finally (return (+ j k)))
 7)

(looptest
 loop-init-03
 (xloop init (i j k) = (iota 3) init (x y) = (list i j) 
        for z in nil do (print z) finally (return (+ i j k x y)))
 4)

(looptest
 loop-init-04
 (xloop init (i j k) = (iota 2) for z in nil do (print z)
        finally (return (and (null k) (integerp i) (integerp j))))
 t)

(looptest
 loop-init-05
 (xloop init (i j k) = (iota 4) for z in nil do (print z)
        finally (return (and (integerp k) (integerp i) (integerp j))))
 t)

;;; For RANGES

(looptest 
 loop-for-range-00
 (let ((k 0)) (xloop for j from 1 to 10 do (setq k j)) k)
 10)

(looptest 
 loop-for-range-00a
 (let ((k 0)) (xloop for j fixnum from 1 to 10 do (setq k j)) k)
 10)

(looptest 
 loop-for-range-00b
 (let ((k 0)) (xloop for j fixnum from 1 upto 10 do (setq k j)) k)
 10)

(looptest 
 loop-for-range-00c
 (let ((k 0)) (xloop for j fixnum from (1+ k) to (+ k 10) do (setq k j)) k)
 10)

(looptest
 loop-for-range-00d
 (let ((k 0)) (xloop for j fixnum from 1 to pi do (setq k j)) k)
 3
 )

(looptest
 loop-for-range-00e
 (let ((k 0)) (xloop for j fixnum from pi to 5 do (setq k (floor j))) k)
 4
 )

(looptest
 loop-for-range-00f
 (let ((k 0)) (xloop for j fixnum from 1 to 10 by pi do (setq k (floor j))) k)
 7
 )

;;; Make sure 'as' works like 'for'
(looptest 
 loop-for-range-01
 (let ((k 0)) (xloop as j from 1 to 10 do (setq k j)) k)
 10)

(looptest 
 loop-for-range-02
 (let ((k 0)) (xloop for j from 10 downto 1 do (setq k j)) k)
 1)

(looptest 
 loop-for-range-03
 (let ((k 0)) (xloop for j from 1 below 10 do (setq k j)) k)
 9)

(looptest 
 loop-for-range-04
 (let ((k 0)) (xloop for j from -1 downto -10 do (setq k j)) k)
-10)

(looptest 
 loop-for-range-05
 (let ((k 0)) (xloop for j from k below 0 do (setq k j) (setq k 23)) k)
 0)

(looptest
 loop-for-range-06
 (let ((k 0) (j 7)) 
   (xloop for i from k to 20 by j do (setq k i))
   k)
 14)

(looptest
 loop-for-range-07
 (let ((k 0)) 
   (xloop for i from (1+ most-positive-fixnum) to (+ most-positive-fixnum 10)
          by 1 do (setq k i))
   k)
 (+ most-positive-fixnum 10)
 )

(looptest 
 loop-for-range-08
 (let ((k -1) (value 23)) 
   (xloop for j from 10 to 1 by k do (setq value (progn j 10))) value)
 23)

(looptest 
 loop-for-range-08a
 (let ((k 1) (value 23)) 
   (xloop for j from -10 downto -1 by k 
          do (setq value (progn j 10))) value)
 23)

(looptest 
 loop-for-range-08b
 (let ((k 1) (value 23)) 
   (xloop for j from 10 below 10 by k do (setq value (progn j 10))) value)
 23)

;;; FOR FROM [BY]

(looptest 
 loop-for-from-01
 (let ((k 0)) (xloop for j from 10 until (> j 20)  do (setq k j)) k)
 20)

(looptest 
 loop-for-from-02
 (let ((k 0)) (xloop for j from 10 by 3 until (> j 20)  do (setq k j)) k)
 19)

(looptest 
 loop-for-from-02a
 (let ((k 0) (start 10) (inc 3))
   (xloop for j fixnum from start by inc until (> j 20)  do (setq k j)) k)
 19)

(looptest 
 loop-for-from-03
 (let ((k 0) (foo 10) (bar 3))
   (xloop for j from (+ foo foo) by (+ bar bar)
          until (> j 30)  
          do (setq k j)) k)
 26)

;;; FOR =

(looptest 
 loop-for-=-05
 (let ((k 0)) (xloop for j = 1 then (1+ j) until (> j 9) do (setq k j)
                          finally (return (list k j))))
 (list 9 10))

(looptest 
 loop-for-=-05a
 (let ((k 0))
   (xloop for j fixnum = 1 then (1+ j) until (> j 9) do (setq k j)
          finally (return (list k j))))
 (list 9 10))

(Looptest 
 loop-for-=-06
 (let ((k 0)) 
   (xloop for j = 1 until (> k 9) do (incf j) (incf k) finally (return j)))
 1)


(looptest 
 loop-for-=-07
 (let ((k 0) (i 0)) 
   (xloop for (x y) = (list k i)
          until (> k 9)
          do (incf i) (incf k) finally (return (list x y))))
 (list 10 10))

(looptest 
 loop-for-=-08
 (let ((k 0)) 
   (xloop for (x y) = '(1 -1) then (list (1+ x) (1- y)) until (> x 9) do 
          (setq k (list x y))
          finally (return (list k (list x y)))))
 (list (list 9 -9) (list 10 -10)))

(looptest 
 loop-for-=-09
 (let ((k 0)) 
   (xloop for (x nil) = '(1 -1) then (list (1+ x) 0) until (> x 9) do 
          (setq k x)
          finally (return (list k x))))
 (list 9 10))

;;; FOR IN

;;; LIST in
(looptest 
 loop-for-in-01
 (let ((k 0)) (xloop for j in (iota 10) do (setq k j)) k)
 9)

;;; LIST on
(looptest 
 loop-for-in-02
 (let ((k 0)) 
   (xloop for j on (iota 10) do (unless (null j) (setq k j)))
   (first k))
 9)

(looptest 
 loop-for-in-02a
 (let ((k 0)) 
   (xloop for j on (iota 10) by cdddr do (unless (null j) (setq k j)))
   (first k))
 9)

(looptest 
 loop-for-in-02b
 (let ((k 0)) 
   (xloop for j on (iota 10) by 'cdddr do (unless (null j) (setq k j)))
   (first k))
 9)

(looptest 
 loop-for-in-02c
 (let ((k 0)) 
   (xloop for j on (iota 10) by #'cdddr do (unless (null j) (setq k j)))
   (first k))
 9)

(looptest 
 loop-for-in-02d
 (let ((k 0)) 
   (xloop for j on (iota 10) by (lambda (x) (cdddr x))
          do (unless (null j) (setq k j)))
   (first k))
 9)

(looptest 
 loop-for-in-02e
 (let ((k 0)) 
   (xloop for j on (iota 10) by #'(lambda (x) (cdddr x))
          do (unless (null j) (setq k j)))
   (first k))
 9)

;;; VECTOR
(looptest 
 loop-for-in-03
 (let ((k 0)) (xloop for j in (vector 1 2 3) do (setq k j)) k)
 3)

;;; STRING
(looptest 
 loop-for-in-04
 (let ((k 0)) (xloop for j in "abcdef" do (setq k j)) k)
 #\f)

;;; null vector
(looptest 
 loop-for-in-05
 (let ((k 0)) (xloop for j in (vector) do (setq k j)) k)
 0)

;;; array
(looptest 
 loop-for-in-06
 (let ((k 0)) 
   (xloop for j in (make-array '(2 2) :initial-contents '((1 2) (3 4)))
          (incf k j))
   k)
 10)

;;; HASH TABLE keys
(looptest 
 loop-for-in-07
 (let ((k 0)) 
   (xloop for j in (create-hash-table '((1 2) (3 4))) do (incf k (first j))) k)
 4)

;;; HASH TABLE keys and values
(looptest 
 loop-for-in-08
 (let ((k 0)) 
   (xloop 
    for (nil val) in (create-hash-table '((1 2) (3 4))) 
    do (incf k val))
   k)
 6)

;;; FRAME slots and values
(looptest 
 loop-for-in-09
 (let ((k 0)) 
   (xloop 
    for (nil value) in 
    ;; made these slot values into lists because in new frame system 
    ;; other slots have integer values (ie #$sys.timestamp)
    (def-frame #$test.testfoo #$slot1 (list 1) #$slot2 (list 2))
    do 
    (when (consp value) (incf k (first value))))
   k)
 3)

;;; LIST in
(looptest 
 loop-for-in-10
 (let ((k 0)) (xloop for j in (iota 6) by cddr do (incf k j)) k)
 6)

(looptest 
 loop-for-in-10a
 (let ((k 0)) (xloop for j in (iota 6) by 'cddr do (incf k j)) k)
 6)

(looptest 
 loop-for-in-10b
 (let ((k 0)) (xloop for j in (iota 6) by #'cddr do (incf k j)) k)
 6)

(looptest 
 loop-for-in-10c
 (let ((k 0)) 
   (xloop for j in (iota 6) by (lambda (x) (cddr x)) do (incf k j))
   k)
 6)

;;; COUNT 

(looptest 
 loop-count-1
 (xloop for j in '(1 2 3 nil 4 5 nil) count j)
 5)

(looptest 
 loop-count-2
 (xloop for j in (iota 10) when (oddp j) count j)
 5)

(looptest 
 loop-count-3
 (xloop for j in (iota 10) when (oddp j) count (evenp j))
 0)

(looptest 
 loop-count-4
 (xloop for j from 10 downto 3 
        for k = 8 then 4 
        count (and (evenp j) (= k 8)))
 1)

 

;;; SUM
(looptest 
 loop-agg-01
 (xloop for j in (iota 5) sum j)
 10)

(looptest 
 loop-agg-02
 (xloop for j in (iota 5) summing j)
 10)


;;; SUM and body form
(looptest 
 loop-agg-03
 (let ((k 0))
   (list (xloop for j in (iota 5) sum j do (incf k)) k))
 (list 10 5)
 )

;;; SUM and finally
(looptest 
 loop-agg-04
 (let ((k 0))
   (list (xloop for j in (iota 5) sum j finally (setq k 23)) k))
 (list 10 23)
 )

;;; COLLECT
(looptest 
 loop-agg-05
 (xloop for j in (iota 5) collect j)
 (iota 5))

;;; MAX
(looptest 
 loop-agg-06
 (xloop for j in (iota 5) max j)
 4)

;;; MIN
(looptest 
 loop-agg-07
 (xloop for j in (reverse (iota 5)) min j)
 0)

;;; MIN
(looptest 
 loop-agg-07a
 (xloop for j in (reverse (iota 5)) minimizing j)
 0)

;;; APPEND
(looptest 
 loop-agg-08
 (xloop for j in (mapcar 'list (iota 5)) append j)
 (iota 5))

(looptest 
 loop-agg-08a
 (xloop for j in (list (list 1) nil (list 2) nil (list 3)) append j)
 '(1 2 3))



;;; NCONC
(looptest 
 loop-agg-09
 (xloop for j in (mapcar 'list (iota 5)) nconc j)
 (iota 5))

(looptest 
 loop-agg-09a
 (xloop for j in (list (list 1) nil (list 2) nil (list 3)) nconc j)
 '(1 2 3))



;;; SIMPLE CONDITIONAL

(looptest
 loop-conditional-01
 (xloop for j in (iota 10) when (< j 5) sum j)
 10)

(looptest
 loop-conditional-01a
 (xloop for j in (iota 10) when (> j 5) collect j)
 '(6 7 8 9))

(looptest
 loop-conditional-02
 (xloop for j in (iota 10) unless (>= j 5) sum j)
 10)

(looptest
 loop-conditional-03
 (let ((k 0)) (xloop for j in (iota 10) when (< j 5) (setq k j)) k)
 4)

(looptest
 loop-conditional-04
 (let ((k 0)) (xloop for j in (iota 10) unless (>= j 5) (setq k j)) k)
 4)


;;; COMPOUND TESTS
(looptest
 loop-compound-01
 (xloop init x = 5 
        init y = 10
        for j from 1 to 10 by 2
        for k on (iota 20)
        as q = (+ j (first k))
        do 
        (setq x q)
        (incf x y)
        collect j)
 '(1 3 5 7 9))
        
(looptest
 loop-compound-02
 (let ((y (copy-list '((1 2) (3 4) (5 6)))))
   (xloop for x on y
          for ((foo bar) . baz) on y
          for xcount from 1 by 10
          do (setf (first x) (progn baz foo)) (incf bar)
          finally (return (list xcount y))
          ))
 (list 21 '(1 3 5))
 )

(looptest
 loop-compound-03
 (every
  #'identity
  (mapcar
   (lambda (x) (apply '= x))
   (xloop for j in (iota 20) by cddr
          for k from 0 below 20 by 2
          for i = 0 then (+ i 2)
          collect (list i j k)
          )))
 t
 )

(looptest
 loop-compound-04
 (let ((k 10))
   (list
    (xloop for j in (iota 20) by cddr
           when (oddp j) 
           collect j
           when (evenp j)
           (setq k j))
    k))
 (list nil 18))

(looptest
 loop-compound-05
 (xloop for j from 0 below 5
        sum
        (xloop for i from j below 5 sum (progn i 1)))
 15)


;;; Complex Destructuring 

 (looptest 
 loop-destructuring-01
 (xloop for (nil value) in (create-hash-table '((1 2) (4 3))) sum value)
 5)

(looptest 
 loop-destructuring-02
 (xloop for (a b . c) in (list '(1 2 . 3) '(3 4 . 5)) sum (+ a b c))
 18)

(looptest 
 loop-destructuring-03
 (xloop for (nil nil . c) in (list '(1 2 . 3) '(3 4 . 5)) sum c)
 8)

(looptest 
 loop-destructuring-04
 (xloop for ((a b) . c) on (list '(1 2) '(3 4) '(5 6)) sum (+ a b (length c)))
 24
 )

(looptest 
 loop-destructuring-05
 (xloop for (nil nil) on nil sum 1)
 0
 )

;;; Nested 

(looptest 
 loop-nest-01 
 (xloop for j from 1 to 10 
        sum
        (xloop for i from 1 to 10 do (+ i j) sum 1))
 100)

(looptest
 loop-nest-02
 (length
  (xloop for j from 1 to 5
         nconc
         (xloop for i from 1 to 5
                nconc
                (xloop for k from 1 to 5 collect (+ i j k)))))
 125)

;;; Garrays

(looptest
 loop-garray-01
 (let ((g (make-garray '(2 2))))
   (xloop 
    init v = 0 
    for j from 0 below 2 
    (xloop for i from 0 below 2
           (setf (gref g j i) v)
           (incf v)
           ))
   (xloop for v in g collect v))
 '(0 1 2 3))



(looptest
 loop-garray-02
 (sort 
  (let ((g (make-garray '(2 $ 2))))
    (xloop 
     init v = 0 
     for j from 0 below 2 
     (xloop for key in '(:fred :wilma)
            (xloop for i from 0 below 2
                   (setf (gref g j key i) v)
                   (incf v)
                   )))
    (xloop for v in g collect v))
  '<)
 (iota 8))

(looptest 
 loop-garray-03
 (let ((g (make-garray '(( -3 3)))))
   (xloop for j from -3 below 3 do 
          (setf (gref g j) j))
   (xloop for v in g collect v))
 '(-3 -2 -1 0 1 2))

(looptest 
 loop-garray-04
 (sort 
  (let ((g (make-garray '($))))
    (xloop for j from -3 below 3 do 
           (setf (gref g j) j))
    (xloop for v in g collect v))
  '<)
 '(-3 -2 -1 0 1 2))

