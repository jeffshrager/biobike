;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

;;; Author:  JP Massar

(in-package :weblistener)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stuff for frames-tests


(defun run-frames-tests ()
  (tests:run-chapter-tests :frames :verbose t))

(defmacro defframetest (name &body body)
  `(tests:deftest ,name ,@body :chapter :frames))

(def-frame #$test.magic-word)
(def-frame #$test.xyzzy)
(def-frame #$test.plugh)
(def-frame #$test.plover)

(def-computed-slot (#$test.Fname-length frame slot) 
  (length (slotv frame #$Fname)))

(defparameter *side-effect-variable* 0)

(def-computed-slot (#$test.SideEffect frame slot) (incf *side-effect-variable*))

(def-always-computed-slot (#$test.Always-compute-me)
  (incf *side-effect-variable*))

(defun sort-frames-by-fname (frames)
  (sort (copy-list frames) 'string-lessp :key (lambda (x) (slotv x #$Fname))))

(def-inverse-slot #$test.Subclasses #$test.isa)

(def-inverse-slot #$test.InstantiationsOf #$test.IsAnInstanceOf)

(def-inherited-slot #$test.Number-of-legs #$test.IsAnInstanceOf)

(def-inherited-slot #$test.part-of-speech)

(def-frame-class 
 #$test.gene ()
 ((#$test.from :allocation :instance :initform 0 :domain integer)
  (#$test.proteins :initform nil :domain :set)
  #$test.to
 (#$test.component-of :allocation :class :initform #$test.organism)
 ))

(def-frame-class
 #$test.organism ()
 ((#$test.chemical-basis :allocation :class :initform #$test.carbon)))

(def-frame-class
 #$test.mammal (#$test.organism)
 ((#$test.reproductive-method :allocation :class :initform #$test.sexual)))

(def-frame-class
 #$test.cat (#$test.mammal)
 ((#$test.legs :allocation :class :initform 4)
  (#$test.owner :allocation :instance :domain string)))

(defun unique-test-gene ()
  (let* ((name (one-string "test." (string (gensym "g"))))
         (f (frame-fnamed name)))
    (if (null f)
        (make-frame-instance 'frames::test.gene name t)
      (progn
        (unintern-frame f)
        (frames::delete-instance f)
        (unique-test-gene)
        ))))

(defmacro with-temporary-unique-gene ((g) &body body)
  `(let ((,g (unique-test-gene)))
     (prog1 (progn ,@body)
       (unintern-frame ,g)
       (frames::delete-instance ,g)
       )))



