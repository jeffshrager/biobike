;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

;;; Author:  JP Massar. 

(in-package :weblistener)

(def-computed-slot (#$Test.Fname-Length frame slot) 
  (length (slotv frame #$Fname)))

(def-computed-slot (#$Test.Sideeffect frame slot) (incf *side-effect-variable*))

;; Make sure slots which are always computed really are.

(def-always-computed-slot (#$Test.Always-Compute-Me) 
  (incf *side-effect-variable*))

;;; Create the inverted slot pair #$IsAnInstanceOf and #$InstantiationsOf
;;; and test in the same manner

(def-inverse-slot #$Test.Instantiationsof #$Test.IsAnInstanceOf)

;; Test standard inheritance through ISA

(def-inherited-slot #$test.part-of-speech)

;; Test that non-standard inheritance (through a different slot)
;; works too, and works through multiple levels, and works with
;; more than one thing on the inherits-from list.

(def-inherited-slot #$Test.Number-of-legs #$test.IsAnInstanceOf)

(frames::def-reciprocal-inverse-slots 
 #$test.might-be-an-instance-of #$test.might-contain)

(def-transitive-slot #$test.AllClassesOf #$test.IsAnInstanceOf)

(defslot #$Test.Portfolio :set-valued? t)

(defslot #$test.siblings :set-valued? t)

(def-indexed-slot #$Test.Portfolio :test 'equal)
(def-indexed-slot #$Test.Bestpick)
