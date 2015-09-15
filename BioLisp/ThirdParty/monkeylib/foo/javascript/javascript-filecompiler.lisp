;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.foo.javascript)

(defmacro define-javascript-module (name &body names)
  `(setf (get ',name 'javascript-module) ',names))

(define-javascript-module com.gigamonkeys.foo.javascript.special-ops
  array object @ ref new ++ -- ? progn prog block var if
  do-while while for continue break return with switch label
  throw try function
  ;; Unary ops
  delete void typeof ~ !
  ;; Simple binary ops 
  * / % + - << >> >>> < > <= >= instanceof in
  == != === !=== & ^ \| && \|\|)

(define-javascript-module com.gigamonkeys.foo.javascript.built-in-macros
  defun defmethod defvar debug lambda let let* if when unless cond case switch
  dolist dotimes dokids return array autoref autorefset setf defcallback
  destructure html define-builder)

(defvar *mappings*)

(defun add-mappings (mappings name)
  (dolist (sym (get name 'javascript-module))
    (add-mapping sym mappings)))

(defun add-mapping (symbol mappings)
  (let ((name (intern (string-downcase symbol) :com.gigamonkeys.foo.javascript.tokens)))
    (let ((existing (gethash name mappings)))
      (if existing 
	(unless (eql existing symbol)
	  (error "Already a mapping for ~a to ~a" name existing))
	(setf (gethash name mappings) symbol)))))
  
(defparameter *default-initial-mappings* (make-hash-table))
(progn
  (add-mappings *default-initial-mappings* 'com.gigamonkeys.foo.javascript.special-ops)
  (add-mappings *default-initial-mappings* 'com.gigamonkeys.foo.javascript.built-in-macros))

(defun resolve-names (thing mappings)
  (cond
    ((null thing) nil)
    ((symbolp thing) (or (gethash thing mappings) thing))
    ((atom thing) thing)
    (t (cons (resolve-names (car thing) mappings) (resolve-names (cdr thing) mappings)))))

(defun js (string)
  (process-javascript (get-pretty-printer) (read-js-from-string string) :expression))

(defmacro with-javascript-input (&body body)
  `(let ((*readtable* (copy-readtable))
	 (*package* (find-package :com.gigamonkeys.foo.javascript.tokens))
	 (*mappings* (make-hash-table)))
     (maphash #'(lambda (k v) (setf (gethash k *mappings*) v)) *default-initial-mappings*)
     (setf (readtable-case *readtable*) :preserve)
     ,@body))

(defun read-js-from-string (string)
  (with-javascript-input
    (resolve-names (read-from-string string) *mappings*)))

(defun emit-javascript (in out)
  (with-javascript-input
    (with-html-output (out :pretty t)
      (loop with processor = (get-pretty-printer) 
	 for form = (read in nil in)
	 while (not (eql form in)) do
	   (process-javascript processor (resolve-names form *mappings*) :statement)
	   (newline processor)))))
      
(defun compile-javascript (input &key (output (make-pathname :type "js" :defaults input)))
  (assert (not (equal (pathname input) (pathname output))))
  (let ((*counter* 0))
    (with-open-file (in input)
      (with-open-file (out output :direction :output :if-exists :supersede)
	(format out "// Generated at ~a from ~a.~2%" (format-iso-8601-time (get-universal-time)) (truename in))
	(emit-javascript in out)))))