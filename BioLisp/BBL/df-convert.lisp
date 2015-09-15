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

;;; Author: JP Massar. 


;;; Each entry of the hash table has a key which is a TO-TYPE.
;;; the values for the keys have the form:
;;;   ((FROM-TYPE CONVERSION-NAME) CONVERSION-FUNCTION)
;;; where the conversion-name is T for the default conversion, and
;;; a symbol identifying the conversion otherwise.


(defvar *defconversion-hash-table* (make-hash-table :test 'equal))
         
(defmacro defconversion 
          ((from-type to to-type 
                      &optional (named nil named?) (conversion-name t))
           (varname)
           &body body)
  #.(one-string-nl
     "Defines a function of one argument, VARNAME, consisting of BODY,"
     "which is supposed to accept an object of type FROM-TYPE and return"
     "an object of type TO-TYPE.  If CONVERSION-NAME is provided, that"
     "name is associated with the defined function (but not using DEFUN,"
     "just via a hash association table).  Functions so defined are"
     "used by DEFINE-FUNCTION when the specification has 'convert'"
     "clauses.  See the DEFINE-FUNCTION documentation and specification"
     "for further details.")
  (flet ((oops-bad-type? (type argname) 
           (handler-case 
               (subtypep from-type t)
             (error 
              (c) 
              (one-string-nl 
               "The ~S argument, ~S,"
               "does not appear to be a valid type."
               "The common lisp error generated when trying to interpret"
               "this object as a type was: ~S")
              argname type c))))
    (oops-bad-type? from-type 'from-type)
    (unless (symbol= :to to) 
      (error
       (one-string-nl
        "The token after the FROM-TYPE argument (the second argument)"
        "is ~S, but it must be the symbol 'to'.")
       to))
    (oops-bad-type? to-type 'to-type)
    (when named? 
      (unless (symbol= :named named) 
        (error
         (one-string-nl
          "The token after the TO-TYPE argument (the fourth argument, if"
          "provided) is ~S, but it must be the symbol 'named'.")
         named))
      (unless (symbolp conversion-name)
        (error 
         (one-string-nl
          "The token after the 'named' literal must be a symbol which"
          "names the particular conversion being defined."
          "But the object, ~S, is not a symbol, it is a ~S.")
         conversion-name (type-of conversion-name))))
    (unless (symbolp varname) 
      (error 
       (one-string-nl
        "The form specifying the name of the argument to"
        "the function being defined by DEFCONVERSION, ~S"
        "must be a symbol!")
       varname))
    `(add-defconversion 
      ',to-type ',from-type ',conversion-name 
      (lambda (,varname) ,@body))))

(defun add-defconversion (to-type from-type conversion-name conversion-function)
  (let* ((existing-conversions 
          (gethash to-type *defconversion-hash-table*))
         (test-key (list from-type conversion-name)) 
         (new-conversion (list test-key conversion-function)))
    (setq existing-conversions 
          (delete test-key existing-conversions :key 'first :test 'equal))
    (push new-conversion existing-conversions)
    (setf (gethash to-type *defconversion-hash-table*) existing-conversions)
    new-conversion))

(defun find-named-defconversion  (to-type conversion-name) 
  (let* ((existing-conversions (gethash to-type *defconversion-hash-table*)))
    (second (find conversion-name existing-conversions :key 'cadar :test 'eq))))

(defun find-default-defconversion (to-type from-type)
  (let* ((existing-conversions (gethash to-type *defconversion-hash-table*)))
    (vif (default-conversion 
          (find (list from-type t) existing-conversions :key 'car :test 'equal))
         (second default-conversion)
         (lambda (obj) (coerce obj to-type))
         )))
      
(defun remove-conversion (from-type to-type &optional (name t))
  #.(one-string-nl
     "Undoes the effect of the DEFCONVERSION statement specifying a conversion"
     "from FROM-TYPE to TYPE-TYPE named NAME, if any.  If NAME is :all, all"
     "conversions from FROM-TYPE to TO-TYPE are undone, including the"
     "default conversion.  If NAME is not provided it defaults to T, meaning"
     "the default conversion."
     "If a DEFINE-FUNCTION F uses a DEFCONVERSION and that DEFCONVERSION is"
     "undone using REMOVE-CONVERSION, the DEFINE-FUNCTION code for F will"
     "no longer work.  F would have to be recoded.")
  (block exit
    (let* ((h *defconversion-hash-table*)
           (existing-conversions (gethash to-type h)))
      (when (null existing-conversions) 
        (cformatt "No conversions exist going to type ~S" to-type)
        (return-from exit nil))
      (when (eq name :all)
        (let ((n (count-if 
                  (lambda (x) (eq from-type (caar x)))
                  existing-conversions
                  )))
          (when (zerop n)
            (cformatt 
             "No conversions exist from type ~S to type ~S"
             from-type to-type
             ))
          (let ((names
                 (mapcar
                  'cadar
                  (remove-if-not 
                   (lambda (x) (eq from-type (caar x)))
                   existing-conversions
                   ))))
            (loop for j from 1 to n do
                  (setq existing-conversions
                        (remove from-type existing-conversions :key 'caar)))
            (setf (gethash to-type h) existing-conversions)
            (cformatt 
             "~D conversions named ~S from type ~S to type ~S deleted."
             n names from-type to-type)
            (return-from exit t)
            )))
      (let ((id (list from-type name)))
        (when (null (find id existing-conversions :key 'car :test 'equal))
          (if (eq name t)
              (cformatt
               "No default conversion exists from type ~S to type ~S."
               from-type to-type)
            (cformatt
             "No conversion exists from type ~S to type ~S named ~S."
             from-type to-type name
             ))
          (return-from exit nil))
        (setf (gethash to-type h)
              (remove id existing-conversions :key 'car :test 'equal))
        (return-from exit t)
        ))))

(defun show-defconversions ()
  #.(one-string-nl
     "Shows all current defconversions.  The TO-TYPE is listed atop,"
     "while the FROM-TYPE(s) and NAME(s) are indented below each TO-TYPE."
     "The code for the defconversions is not shown.")
  (cformatt "")
  (cformatt "       TABLE OF BBL TYPE CONVERSIONS FOR DEFINE-FUNCTION")
  (cformatt "    (Grouped by TO-TYPE, with FROM-TYPE(s) indented beneath)")
  (cformatt "")
  (maphash 
   (lambda (to-type data) 
     (cformatt "~S" to-type) 
     (loop for ((from-type name) nil) in data
           do
           (when (eq name t) 
             (cformatt "  ~20S ~T ~S" from-type name)
             ))
     (loop for ((from-type name) nil) in data
           do
           (when (not (eq name t)) 
             (cformatt "  ~20S ~T ~S" from-type name)
             )))
   *defconversion-hash-table*))
     


;;; Each entry of the hash table has a key which is a TO-TYPE.
;;; the values for the keys have the form:
;;;   ((FROM-TYPE CONVERSION-NAME) CONVERSION-FUNCTION)
;;; where the conversion-name is T for the default conversion, and
;;; a symbol identifying the conversion otherwise.

(defun bb-convert 
       (arg 
        to-type
        &key
        (from-type nil)
        (named t)
        (if-no-conversion? :error)
        (if-multiple-conversions? :choose-one)
        (if-conversion-error? :error))
  #.(one-string-nl
     "Converts ARG to TO-TYPE if a suitable conversion method can be found"
     "from conversion methods defined by DEFCONVERSION.  If FROM-TYPE"
     "is specified, only a specific conversion from FROM-TYPE to"
     "TO-TYPE is used, assuming one exists."
     "If FROM-TYPE is not specified or NIL, then all conversions"
     "to TO-TYPE are considered, and those for which ARG is a subtype of"
     "the associated FROM-TYPE are selected as candidate conversion methods."
     ""
     "If more than one candidate conversion method is found,"
     "IF-MULTIPLE-CONVERSIONS? determines the behavior:"
     "  :CHOOSE-ONE, the default, selects the first method found,"
     "  :ERROR signals an error,"
     "  :WARN prints a warning then behaves as if :CHOOSE-ONE was specified."
     "If :CHOOSE-ONE is specified and multiple conversions are found,"
     "BB-CONVERT returns the result of the conversion as its first value"
     "and :multiple as its second."
     ""
     "If no suitable conversion exists, IF-NO-CONVERSION? determines"
     "behavior: the default, :ERROR, signals an error,"
     ":WARN prints a warning,"
     "and any other value X causes BB-CONVERT to return (values nil X)."
     ""
     "In the standard case where a single conversion method is found,"
     "BB-CONVERT returns the result of the conversion as its first value"
     "and T as its second value."
     ""
     "Once the conversion is actually attempted, IF-CONVERSION-ERROR?"
     "determines what happens if the conversion signals an error."
     "If the value is :error (the default), the error signal is not caught."
     "Any other value results in the error signal being caught and the value"
     "of IF-CONVERSION-ERROR? returns as the first value."
     )
  (block exit
    (let ((possible-conversions (gethash to-type *defconversion-hash-table*))
          (conversion nil)
          (second-return-value t)
          (error-function 
           (case if-no-conversion?
             (:error 'error)
             (:warn 'warn)
             (otherwise (lambda (&rest args) (declare (ignore args)) nil)))))
      (when (null possible-conversions)
        (return-from exit 
          (progn
            (funcall error-function "No conversions to type ~S exist!" to-type)
            (values nil if-no-conversion?)
            )))
      (setq 
       conversion
       (if from-type
           (let ((candidate
                  (find-if 
                   (lambda (x) 
                     (and (equal (caar x) from-type) (symbol= named (cadar x))))
                   possible-conversions
                   )))
             (when (null candidate)
               (if (not (eq named t)) 
                   (funcall 
                    error-function 
                    "No conversion to type ~S from type ~S named ~S exists."
                    to-type from-type named)
                 (funcall 
                  error-function 
                  "No default conversion to type ~S from type ~S exists."
                  to-type from-type
                  ))
               (return-from exit (values nil if-no-conversion?))
               )
             candidate)
         (let ((candidates 
                (loop for candidate in possible-conversions
                      as from = (caar candidate)
                      as name = (cadar candidate)
                      when (and (typep arg from) (symbol= name named)) 
                      collect candidate
                      )))
           (when (null candidates) 
             (funcall
              error-function 
              "No conversion from object, ~S, of type ~S, to type ~S, exists."
              arg (type-of arg) to-type)
             (return-from exit (values nil if-no-conversion?)))
           (when (> (length candidates) 1)
             (ecase if-multiple-conversions? 
               ((:error :warn) 
                (funcall 
                 (if (eq if-multiple-conversions? :error) 'error 'warn)
                 (one-string-nl
                  "More than 1 applicable conversion for object ~S, of type ~S,"
                  "to type ~S.")
                 arg (type-of arg) to-type))
               (:choose-one nil))
             (setq second-return-value :multiple))
           (first candidates)
           )))
      (case if-conversion-error? 
        (:error 
         (values (funcall (cadr conversion) arg) second-return-value))
        (otherwise 
         (handler-case
             (values (funcall (cadr conversion) arg) second-return-value)
           (error () (values if-conversion-error? second-return-value))
           ))))))                   
             
         
