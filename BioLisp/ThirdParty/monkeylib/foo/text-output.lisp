;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.foo.text-output)

(defvar *pretty* t
  "Controls whether output is generated with indentation and
  tracking of newlines for emitting freshlines.")

(defvar *text-output* *standard-output*
  "The stream to which output is sent. Can be bound to any character output stream.")

(defvar *text-pretty-printer* nil
  "The current instance of text-pretty-printer wrapped around
  *text-output* when *pretty* is true.")

(defmacro with-foo-output ((stream &key (pretty *pretty*)) &body body)
  `(let* ((*text-output* ,stream)
          (*pretty* ,pretty))
    ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text output interface -- this interface is implemented in two
;;; ways: the text-pretty-printer, which can be thought of as an
;;; interpreter for the text output "virtual machine", and the
;;; text-output-compiler which collects a set of instructions for the
;;; text output machine and then compiles them into either a program
;;; for the pretty printing virtual machine or calls to the raw Common
;;; Lisp output functions if pretty printing is turned off.

(defgeneric raw-string (processor string &optional check-for-newlines)
  (:documentation "Emit a string with no escaping. When
  CHECK-FOR-NEWLINES is true, string should still be scanned for
  newlines in order to deal with indentation. In general
  CHECK-FOR-NEWLINES should be true unless the string is known
  not to contain newlines."))

(defgeneric newline (processor)
  (:documentation "Unconditionally emit a newline."))
  
(defgeneric freshline (processor)
  (:documentation "Emit a newline unless the last character emitted was a newline."))

(defgeneric indent (processor)
  (:documentation "Increase the indentation level by some fixed amount."))

(defgeneric unindent (processor)
  (:documentation "Decrease the indentation level by some fixed amount."))

(defgeneric toggle-indenting (processor)
  (:documentation "Temporarily turn of indentation without changing the current indentation value."))

(defgeneric embed-value (processor value)
  (:documentation "Emit a literal value."))

(defgeneric embed-code (processor code)
  (:documentation "Run arbitrary Lisp code which will presumably emit some output."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; text-pretty-printer

(defclass text-pretty-printer ()
  ((printer   :accessor printer   :initarg :printer)
   (tab-width :accessor tab-width :initarg :tab-width :initform 2)))

(defmethod raw-string ((pp text-pretty-printer) string &optional newlines-p)
  (if newlines-p
    (emit (printer pp) string)
    (emit/no-newlines (printer pp) string)))

(defmethod newline ((pp text-pretty-printer))
  (emit-newline (printer pp)))

(defmethod freshline ((pp text-pretty-printer))
  (if *pretty*
    (emit-freshline (printer pp))
    (emit-newline (printer pp))))

(defmethod indent ((pp text-pretty-printer))
  (when *pretty* 
    (incf (indentation (printer pp)) (tab-width pp))))

(defmethod unindent ((pp text-pretty-printer))
  (when *pretty* 
    (decf (indentation (printer pp)) (tab-width pp))))

(defmethod toggle-indenting ((pp text-pretty-printer))
  (when *pretty* 
    (with-slots (indenting-p) (printer pp)
      (setf indenting-p (not indenting-p)))))

(defmethod embed-value ((pp text-pretty-printer) value)
  (error "Can't embed values when interpreting. Value: ~s" value))

(defmethod embed-code ((pp text-pretty-printer) code)
  (error "Can't embed code when interpreting. Code: ~s" code))

(defun get-pretty-printer ()
  (or *text-pretty-printer* (new-pretty-printer)))

(defun new-pretty-printer ()
  (make-instance 
    'text-pretty-printer
    :printer (make-instance 'indenting-printer :out *text-output*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiler

(defclass text-compiler ()
  ((ops :accessor ops :initform (make-op-buffer))))

(defmethod raw-string ((compiler text-compiler) string &optional newlines-p)
  (push-op `(:raw-string ,string ,newlines-p) (ops compiler)))

(defmethod newline ((compiler text-compiler))
  (push-op '(:newline) (ops compiler)))
  
(defmethod freshline ((compiler text-compiler))
  (push-op '(:freshline) (ops compiler)))

(defmethod indent ((compiler text-compiler))
  (push-op `(:indent) (ops compiler)))

(defmethod unindent ((compiler text-compiler))
  (push-op `(:unindent) (ops compiler)))

(defmethod toggle-indenting ((compiler text-compiler))
  (push-op `(:toggle-indenting) (ops compiler)))

(defmethod embed-value ((compiler text-compiler) value)
  (push-op `(:embed-value ,value) (ops compiler)))

(defmethod embed-code ((compiler text-compiler) code)
  (push-op `(:embed-code ,code) (ops compiler)))

(defun codegen-text (ops pretty)
  "Given a vector of ops, return a Common Lisp translation,
  i.e. Common Lisp code that will emit the same output as the
  text-pretty-printer would if it was given the same ops."
  (let ((*pretty* pretty))
    `(progn ,@(generate-code (optimize-static-output ops)) nil)))

(defun generate-code (ops)
  (loop for op across ops collect (apply #'op->code op)))

(defun optimize-static-output (ops)
  (let ((new-ops (make-op-buffer)))
    (with-output-to-string (buf)
      (flet ((add-op (op) 
               (compile-buffer buf new-ops)
               (push-op op new-ops)))
        (loop for op across ops do
             (ecase (first op)
               (:raw-string (write-sequence (second op) buf))
               ((:newline :embed-value :embed-code) (add-op op))
	       (:freshline (add-op (if *pretty* op '(:newline))))
               ((:indent :unindent :toggle-indenting)
                (when *pretty* (add-op op)))))
        (compile-buffer buf new-ops)))
    new-ops))

(defun compile-buffer (buf ops)
  "Compile a string possibly containing newlines into a sequence of
:raw-string and :newline ops."
  (loop with str = (get-output-stream-string buf)
     for start = 0 then (1+ pos)
     for pos = (position #\Newline str :start start)
     when (< start (length str))
     do (push-op `(:raw-string ,(subseq str start pos) nil) ops)
     when pos do (push-op '(:newline) ops)
     while pos))

(defgeneric op->code (op &rest operands))

(defmethod op->code ((op (eql :raw-string)) &rest operands)
  (destructuring-bind (string check-for-newlines) operands
    (if *pretty*
      `(raw-string *text-pretty-printer* ,string ,check-for-newlines)
      `(write-sequence ,string *text-output*))))

(defmethod op->code ((op (eql :newline)) &rest operands)
  (if *pretty*
    `(newline *text-pretty-printer*)
    `(write-char #\Newline *text-output*)))    

(defmethod op->code ((op (eql :freshline)) &rest operands)
  (if *pretty*
    `(freshline *text-pretty-printer*)
    (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :indent)) &rest operands)
  (if *pretty*
    `(indent *text-pretty-printer*)
    (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :unindent)) &rest operands)
  (if *pretty*
    `(unindent *text-pretty-printer*)
    (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :toggle-indenting)) &rest operands)
  (if *pretty*
    `(toggle-indenting *text-pretty-printer*)
    (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :embed-value)) &rest operands)
  (let ((value (car operands)))
    (if *pretty*
      `(raw-string *text-pretty-printer* (princ-to-string ,value) t)
      `(write-sequence ,value *text-output*))))

(defmethod op->code ((op (eql :embed-code)) &rest operands)
  (first operands))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; indenting-printer -- used to actually emit text to an output
;;; stream while keeping track of newlines and emitting indentation as
;;; necessary. Used by the pretty printer.

(defclass indenting-printer ()
  ((out                 :accessor out                 :initarg :out)
   (beginning-of-line-p :accessor beginning-of-line-p :initform t)
   (indentation         :accessor indentation         :initform 0)
   (indenting-p         :accessor indenting-p         :initform t)))

(defun emit (ip string)
  (loop for start = 0 then (1+ pos)
     for pos = (position #\Newline string :start start)
     do (emit/no-newlines ip string :start start :end pos)
     when pos do (emit-newline ip)
     while pos))

(defun emit/no-newlines (ip string &key (start 0) end)
  (indent-if-necessary ip)
  (write-sequence string (out ip) :start start :end end)
  (unless (zerop (- (or end (length string)) start))
    (setf (beginning-of-line-p ip) nil)))

(defun emit-newline (ip)
  (write-char #\Newline (out ip))
  (setf (beginning-of-line-p ip) t))

(defun emit-freshline (ip)
  (unless (beginning-of-line-p ip) (emit-newline ip)))

(defun indent-if-necessary (ip)
  (when (and (beginning-of-line-p ip) (indenting-p ip))
    (loop repeat (indentation ip) do (write-char #\Space (out ip)))
    (setf (beginning-of-line-p ip) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ops buffer -- slight abstraction used by text-compiler.

(defun make-op-buffer () (make-array 10 :adjustable t :fill-pointer 0))

(defun push-op (op ops-buffer) (vector-push-extend op ops-buffer))

