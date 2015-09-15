;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bioutils; -*-

(in-package :bioutils) 

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *xloop-api-symbols*
    '(
      xloop
      )))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (export *xloop-api-symbols* (find-package :bioutils))
    ))

(defmacro xloop (&rest args) 
  #.(one-string-nl
     "A variation on the LISP:LOOP macro.  For a detailed specification"
     "see .../doc/loop-syntax.txt."
     "Revised features include simplified syntax and semantics, the"
     "ability to loop over hash tables, vectors, and arrays using"
     "'in' syntax, and elimination of WITH in favor of INIT to"
     "avoid confusion with AS and FOR.")
  (parse-loop `(loop ,@args)))

#|

A loop statement is divided into clauses.  Each clause begins with a
loop keyword, except for an implied DO clause.  The allowable loop
keywords are defined in *LOOP-KEYWORDS*.  One or more keywords (KEYS)
are allowed to have the same meaning (such as FOR and AS, or SUM and
SUMMING).  Keywords which have the same meaning are canonicalized into
something referred to in this code as a CKEY.  To convert a KEY to a
CKEY, we use the function KEY->CKEY.  A KEY can be any
symbol whose name is identical to the symbol named in *LOOP-KEYWORDS*.
KEYS are generally made into keywords using KEYWORDIZE before they are
processed further.  To convert a KEY into a CKEY the function
KEY->CKEY is used.  

A loop clause also belongs to exactly one CLAUSE-CLASS.  These clause
classes and the mapping from KEYS to CLAUSE-CLASSES is defined by
*LOOP-CLAUSE-CLASSES* and KEY->CLASS.  KEY->CLASS takes
a KEY and returns the appropriate class.  

Once a loop clause is parsed, it is parsed into a clause type.  The
clause type is more specialized than a CKEY, because there is
potentially more than one clause type a loop clause of type CKEY can
belong to, depending not just on the loop clauses keyword, but on
further syntactic parsing of the loop clause.  Right now except for
FOR clauses, the clause type is the same as the CKEY.  For FOR
clauses, the clause type is one of the symbols defined in
*FOR-CLAUSES*.  Clause classes are actually symbols but they are NOT
keywords, they are symbols in whatever package this file is written
in.  

Each loop clause is parsed into a LOOP-CLAUSE construct, which
is defined immediately below.  

|#

;;; If CKEY = :implied-do then the key will be NIL, otherwise 
;;; the KEY is the keywordized symbol that begins the clause.  


(defstruct loop-parse
  class type ckey key form syntax-ok? semantics-ok?
  declaration original-form)

(defparameter *loop-keywords* 
  '(
    init initialize with
    for as assign
    while
    until
    when
    unless 
    do body
    collect collecting 
    append appending
    nconc nconcing
    count counting
    sum summing
    max maximizing
    min minimizing
    finally
    ))

(defparameter *loop-reserved-words*
  (append *loop-keywords* '(from to below by step then)))

(defun loop-keyword? (symbol)
  (member symbol *loop-keywords* :test 'symbol=))

(defparameter *for-clauses* 
  '(:for-symbol-in :for-symbol-in-by
    :for-symbols-in :for-symbols-in-by
    :for-symbol-on :for-symbol-on-by
    :for-symbols-on :for-symbols-on-by
    :for-symbol-= :for-symbol-=-then 
    :for-symbols-= :for-symbols-=-then 
    :for-symbol-from :for-symbol-from-by
    :for-symbol-from-to :for-symbol-from-to-by
    :for-symbol-from-downto :for-symbol-from-downto-by
    :for-symbol-from-below :for-symbol-from-below-by
    ))

(defun key->ckey (key)
  (if (null key)
      :implied-do
    (ecase (keywordize key)
      ((:init :initialize :with) :init)
      ((:for :as :assign) :for)
      (:while :while)
      (:until :until)
      (:when :when)
      (:unless :unless)
      ((:do :body) :do)
      ((:collect :collecting) :collect)
      ((:append :appending) :append)
      ((:nconc :nconcing) :nconc)
      ((:count :counting) :count)
      ((:sum :summing) :sum)
      ((:max :maximizing) :max)
      ((:min :minimizing) :min)
      (:finally :finally)
      )))

(defparameter *clause-symbols-by-type* 
  '(
    (:for-symbol-in :in)
    (:for-symbol-on :on)
    (:for-symbol-in-by :in :step :by)
    (:for-symbol-on-by :on :step :by)
    (:for-symbols-in :in)
    (:for-symbols-on :on)
    (:for-symbols-in-by :in :step :by)
    (:for-symbols-on-by :on :step :by)
    (:for-symbol-= :=)
    (:for-symbol-= :=)
    (:for-symbol-=-then := :then)
    (:for-symbols-=-then := :then)
    (:for-symbol-from :from)
    (:for-symbol-from-by :from :step :by)
    (:for-symbol-from-to :from :to :upto)
    (:for-symbol-from-to-by :from :to :upto :step :by)
    (:for-symbol-from-downto :from :downto)
    (:for-symbol-from-downto-by :from :downto :step :by)
    (:for-symbol-from-below :from :below)
    (:for-symbol-from-below-by :from :below :step :by)
    (:init :=)
    (:while)
    (:until)
    (:when)
    (:unless)
    (:collect)
    (:count)
    (:sum)
    (:max)
    (:min)
    (:append)
    (:nconc)
    (:do)
    (:implied-do)
    (:finally)
    ))

(defparameter *loop-clause-classes*
  '(loop-init loop-iterator loop-conditional
             loop-execution loop-aggregator loop-terminator))

(defparameter loop-init '(:init))
(defparameter loop-iterator '(:for :while :until))
(defparameter loop-conditional '(:when :unless))
(defparameter loop-execution '(:do :implied-do))
(defparameter loop-aggregator 
  '(:collect :append :nconc :count :sum :max :min))
(defparameter loop-terminator '(:finally))

(defun form->key (form)
  (cond
   ((null form) (error "This shouldn't happen."))
   ((and (symbolp (first form)) (loop-keyword? (first form))) (first form))
   (t nil)
   ))

(defun key->class (clause-name)
  (let ((ccn (key->ckey clause-name)))
    (loop for class in *loop-clause-classes*
          for clauses = (symbol-value class)
          when (member ccn clauses) do (return class)
          finally (return nil)
          )))

(defparameter *canonical-clause-ordering*
  ;; Only the clause classes in the second element can precede the clause class
  ;; listed in the first element.
  ;; Additional constraints:
  ;; Only one DO.
  ;; Only one AGGREGATION clause
  ;; A single WHEN or UNLESS clause can precede DO or AGGREGATION
  ;; (therefore no two WHEN/UNLESS clauses in a row)
  ;; Only one FINALLY clause
  '((loop-init (nil loop-init))
    (loop-iterator (nil loop-init loop-iterator))
    (loop-conditional (nil loop-iterator loop-execution loop-aggregator))
    (loop-aggregator (nil loop-iterator loop-conditional loop-execution))
    (loop-execution (nil loop-iterator loop-conditional loop-aggregator))
    (loop-terminator (loop-iterator loop-execution loop-aggregator))
    ))

(defun preceding-clause-classes (class)
  (second (assoc class *canonical-clause-ordering*)))
