 ;;; -*- Package: data-editor; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :data-editor)

(defclass de-test ()
  ((a :initarg :a :accessor de-test-a)
   (b :initarg :b :accessor de-test-b)
   ))
          

(defvar sst nil)
(defvar hht nil)
(defvar sht nil)
(defvar hst nil)
(defvar eet nil)
(defvar est nil)
(defvar sqet nil)
(defvar eht nil)
(defvar het nil)
(defvar tl nil)
(defvar ts nil)
(defvar th nil)
(defvar t1gn nil)
(defvar t1gh nil)
(defvar det nil)
(defvar la nil)

(defun set-up-test-objects (&optional (frame #$testframe))
  (setq la (setf (#^la frame) (make-array '(50 50) :initial-element 24)))
  (setq sst (setf (#^sst frame) (sequence-sequence-table)))
  (setq hht (setf (#^hht frame) (hash-hash-table)))
  (setq sht (setf (#^sht frame) (sequence-hash-table)))
  (setq hst (setf (#^hst frame) (hash-sequence-table)))
  (setq eet (setf (#^eet frame) (enum-enum-table)))
  (setq est (setf (#^est frame) (enum-sequence-table)))
  (setq sqet (setf (#^set frame) (sequence-enum-table)))
  (setq eht (setf (#^eht frame) (enum-hash-table)))
  (setq het (setf (#^het frame) (hash-enum-table)))
  (setq tl (setf (#^tl frame) (test-list)))
  (setq ts (setf (#^ts frame) (test-sequence)))
  (setq th (setf (#^th frame) (test-hash)))
  (setq t1gn (setf (#^t1gn frame) (test-1dgarray-numeric)))
  (setq t1gh (setf (#^t1gh frame) (test-1dgarray-hash)))
  (setq det (setf (#^det frame) (make-instance 'de-test :a 5 :b 6)))
  t
  )

(defun test-list () (iota 3000))

(defun test-sequence () (coerce (test-list) 'vector))

(defun test-hash () 
  (let ((h (make-hash-table :test 'equalp)))
    (loop for j from 1000 to 3000 do
          (setf (gethash j h) (formatn "~R" j)))
    h
    ))

(defun test-1dgarray-numeric ()
  (utils::make-garray '((-500 500)) :initial-element 3))

(defun test-1dgarray-hash ()
  (let ((h (utils::make-garray '($) )))
    (loop for j from 1000 to 3000 do
          (setf (ref h j) (formatn "~R" j)))
    h
    ))

(defun sequence-sequence-table ()
  (let ((g (make-garray '((1 100) (1 100)))))
    (loop for i from 1 to 99 do
          (loop for j from 1 to 99 do
                (setf (gref g i j) (+ i j))
                ))
    g
    ))
                
(defun hash-hash-table ()
  (let ((g (make-garray '($ $))))
    (loop for gene in (#^genes bio::npun) do
          (setf (gref g gene :from) (#^from gene)
                (gref g gene :to) (#^to gene)
                (gref g gene :direction) (#^direction gene)
                ))
    g
    ))

(defun sequence-hash-table ()
  (let ((g (make-garray `((1 ,(length (#^genes bio::npun))) $))))
    (loop for gene in (#^genes bio::npun) 
          for j from 1 to (length (#^genes bio::npun))
          do
          (setf (gref g j :gene) gene
                (gref g j :from) (#^from gene)
                (gref g j :to) (#^to gene)
                (gref g j :direction) (#^direction gene)
                ))
    g
    ))

(defun hash-sequence-table ()
  (let ((g (make-garray '($ (-1 10)))))
    (loop for gene in (#^genes bio::npun) do
          (setf (gref g gene -1) :negative)
          (setf (gref g gene 0) :zero)
          (setf (gref g gene 1) (#^from gene)
                (gref g gene 2) (#^to gene)
                (gref g gene 3) (#^direction gene)
                )
          (loop for j from 4 to 10 do
                (setf (gref g gene j) (random 1000))))
    g
    ))

(defun enum-enum-table ()
  (let ((e1 '((:enum equal) "a" "b" "c"))
        (e2 '(:fred :wilma :us)))
    (let ((g (make-garray (list e1 e2))))
      (loop for x in (cdr e1)
            do
            (loop for y in e2
                  do
                  (setf (gref g x y) (random 1000))
                  ))
      g
      )))

(defun enum-sequence-table ()
  (let ((e1 '((:enum equal) "a" "b" "c")))
    (let ((g (make-garray (list e1 5))))
      (loop for x in (cdr e1) do
            (loop for y from 0 to 4
                  do
                  (setf (gref g x y) (random 1000))
                  ))
      g
      )))

(defun sequence-enum-table ()
  (let ((e2 '((:enum equal) "a" "b" "c")))
    (let ((g (make-garray (list '(2 6) e2))))
      (loop for y from 2 to 6
            do
            (loop for x in (cdr e2)
                  do
                  (setf (gref g y x) (random 1000))
                  ))
      g
      )))

(defun enum-hash-table ()
  (let ((e1 '((:enum equalp) "a" "b" "c"))
        (e2 '(:fred :wilma :us)))
    (let ((g (make-garray (list e1 '$))))
      (loop for x in (cdr e1)
            do
            (loop for y in e2
                  do
                  (setf (gref g x y) (random 1000))
                  ))
      g
      )))
 
(defun hash-enum-table ()
  (let ((e1 '((:enum equal) "a" "b" "c"))
        (e2 '(:fred :wilma :us)))
    (let ((g (make-garray (list '$ e1))))
      (loop for x in e2
            do
            (loop for y in (cdr e1)
                  do
                  (setf (gref g x y) (random 1000))
                  ))
      g
      )))


        