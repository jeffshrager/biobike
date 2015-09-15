(COMMENT "This file is for testing the javascript file compiler")

(COMMENT
 "This is a multiline comment."
 "Here's another line."
 "And another")

(defun lambda-test ()
  (lambda () x))

(defun lambda-test-2 ()
  (lambda () x y))

(defun lambda-test-3 ()
  (lambda () x y z))




(block foo
  x)

(block foo
  x
  (block bar
    (return-from foo 10)))

(block foo
  (return-from foo 10))

(block foo
  (if whatever (return-from foo 10))
  10)

  



(defun iftest (x y z)
  (if a b c)
  (if x y z))

(defun progntest1 (x y z)
  (progn a b c)
  (progn x))

(defun progntest2 (x y z)
  (progn a b c)
  (progn x y))

(defun progntest3 (x y z)
  (progn a b c)
  (progn x y z))

(defun fortest ()
  (for ((VAR x 0) (< x 10) (++ x)) (do-stuff))
  (for ((VAR x 0) (< x 10) (++ x)) (do-other-stuff)))

(defun do-test ()
  (do (a
       (b)
       (c 10)
       (d 0 (+ 1 d)))
      ((= d 10) (+ c d))
    (alert (+ a " " b " " c " " d))))

(defun do-test-2 ()
  (do (a
       (b)
       (c 10)
       (d 0 (+ 1 d)))
      ((= d 10))
    (alert (+ a " " b " " c " " d))))

(defun do-star-test ()
  (do* (a
	(b)
	(c 10)
	(d 0 (+ 1 d)))
       ((= d 10) (+ c d))
    (alert (+ a " " b " " c " " d))))

(defun do-star-test-2 ()
  (do* (a
	(b)
	(c 10)
	(d 0 (+ 1 d)))
       ((= d 10))
    (alert (+ a " " b " " c " " d))))

(defun dotimes-test ()
  (dotimes (i 10) (do-stuff))
  (dotimes (i 20) (do-other-stuff)))

(defun dolist-test ()
  (dolist (i array) (do-stuff i))
  (dolist (i 20) (do-other-stuff))
  false)

(defun dolist-test ()
  (+ (dolist (i array) (do-stuff i))))



(defun iftest (x y z)
  (if x y z)
  (if x y z))

(defun progntest ()
  (progn
    (alert "one")
    (alert "two"))
  (progn
    (alert "three")
    (alert "four")))

(defun let-test ()
  (let ((x 0))
    x))

(defun let-test-2 ()
  (let ((x ((lambda (y) (* 2 y)) 10)))
    x))

(defun block-test ()
  (block foo
    (let ((x (lambda () (return-from foo 10))))
      (block bar
	(let ((x (lambda () (return-from foo 20))))
	  (block-test-2 x)))
      (block-test-2 x))))

(defun block-test-2 (fn)
  (fn))


(defun bar () (+ 1 2))

(defun baz () 
  (let ((x 10)) (+ x 20)))

(defun quux ()
  (if 1 2 3))

(defun f1 ()
  (unless x (frob x) (fribitz)))

(defun bar ()
  (progn
    "one"
    "two"
    "three"))

(VAR x (let ((x 10))
	 (lambda () (+ 1 x))))

(defun bar ()
  (let ((x 10) (y 20)) (+ x y)))

(defun f2 ()
  (let ((x 10)) (frob x))
  (let ((y 20)) (frob y))
  1)

(defun f3 (x)
  (cond
    ((== (% x 2) 0) "even")
    ((== (% x 2) 1) "odd")
    (true "not a number")))

(defun f4 ()
  (if x y))

(defun f5 ()
  (or)
  (or x)
  (or x y))

(defun f6 ()
  (and)
  (and x)
  (and x y))

(defun f7 ()
  (dotimes (i 10) (alert "hello")))


(defun foo ()
  ((METHOD x) y z a b c))

(defun bar ()
  (.x y z a b c))


;; sanity check that we can compile a NIL block.
(block nil (return 10))

;; should optimize away the implicit block named block$foo
(defun foo ()
  10)

;; should keep the implicit block named block$bar
(defun bar ()
  (if whatever (return-from bar 20))
  10)
