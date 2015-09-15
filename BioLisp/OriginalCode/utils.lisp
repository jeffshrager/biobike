;;; Copyright (c) 2002-2003 by Jeff Shrager and Mike Travers; All rights reserved.

;;; This software is made avilable for EDUCATIONAL PURPOSES
;;; ONLY, and WITHOUT ANY WARRANTY, express or implied, of
;;; its merchantability or fitness for a particular purpose. 

;;; Utilities for BioLisp lessons

;;; LSETQ gives the length instead of the value, and is good for
;;; setting long strings.

;;; This is the original version in the first lesson:

; (defmacro lsetq (var value)
;  `(length (setq ,var ,value)))


;;; This is a better version, contributed by JP Massar.
;;; It gives you back both the type of the object, and the length.
;;; If it can't figure out the length, you get the type and the
;;; object itself.

(defun dwim-lsetq-func (x)
  (cond ((listp x) (list 'list (length x)))
        ((stringp x) (list 'string (length x)))
        ((vectorp x) (list 'vector (length x)))
        (t (list (type-of x) x))))

(defmacro lsetq (var value)
   `(dwim-lsetq-func (setq ,var ,value)))

;;; Mike Travers' string split doesn't copy the string, and so is pretty efficient!
;;; BE FOREWARNED ABOUT POSSIBLE SIDE-EFFECT CONFUSION!

(compile
(defun string-split (string &optional (delimiter #\space))
  (let ((substrings '())
        (length (length string))
        (last 0))
    (flet ((add-substring (i)
	     (push (make-array (- i last)
			       :element-type 'character
			       :displaced-to string
			       :displaced-index-offset last)
		   substrings)))
      (dotimes (i length)
        (when (eq (char string i) delimiter)
          (add-substring i)
          (setq last (1+ i))))
      (add-substring length)
      (nreverse substrings)))))

;;; Get the first n elements of a list into a new list.

(compile
(defun first-n (n list)
  (loop for k from 1 to n
	as item in list
	collect item)))

;;; This is just a convenience for lisp beginners; In reality,
;;; we shouldn't be using PLists, and if we are, we should be using
;;; cons'ed ones instead of list'ed ones.

(defmacro assocadr (key alist)
  `(cadr (assoc ,key ,alist)))

;;; Some statistical functions.  Although some of these are trivial, they
;;; variously protect against weird conditions, and handle lits of numbers.

(defun sum (l &aux (sum 0))
  (dolist (n l) (incf sum n)) sum)

(defun sqr (a)
  (if (numberp a) (expt a 2)
      (mapcar #'* a a)))

(defun mean (l)
  (/ (sum l) (float (length l))))

;;; --- Correlation of two sequences, as in Ferguson & Takane, 1989,
;;; p. 125.  Assumes NO MISSING VALUES!

(defun correlate (x y)
  (if (not (= (length x) (length y)))
      (break "Can only correlate equal-sized sets."))
      (let* ((mx (mean x))
             (my (mean y))
             (devx (mapcar #'(lambda (v) (- v mx)) x))
             (devy (mapcar #'(lambda (v) (- v my)) y))
             (sumdevxy (sum (mapcar #'* devx devy)))
             (sumsqdevx (sum (sqr devx)))
             (sumsqdevy (sum (sqr devy)))
             (r (/ sumdevxy (sqrt (* sumsqdevx sumsqdevy))))
             )
         (list :r r :r2 (sqr r))
       ))

