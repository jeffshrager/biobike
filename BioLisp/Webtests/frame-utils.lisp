;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

;;; Author:  JP Massar. 

(in-package :weblistener)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stuff for frames-tests

(defmacro defframetest (name &body body)
  `(tests:deftest ,name ,@body :chapter :frames))

(def-frame #$test.magic-word)
(def-frame #$test.xyzzy)
(def-frame #$test.plugh)
(def-frame #$test.plover)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *side-effect-variable* 0))

(defun sort-frames-by-fname (frames)
  (sort (copy-list frames) #'string-lessp :key (lambda (x) (#^Fname x))))


