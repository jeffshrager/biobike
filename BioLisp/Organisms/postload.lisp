;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
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

;;; Author:  JP Massar

;; User-written transform code can use this to refer to the frame 
;; being transformed.

(defvar *the-current-frame*) 

;;; See template at bottom for example.

(defparameter *organism-transform-keys*
  '(:contig-transforms :gene-transforms 
    :transcript-transforms :proteins-transforms))

(defparameter *organism-transform-entities*
  '(:contiguous-sequences :genes :transcripts :proteins))

(defun organism-postload-function-name (organism-name)
  (intern 
   (one-string (string-upcase organism-name) "-POSTLOAD-FUNCTION")
   (find-package :bio)))

(defun organism-postload-file-name (organism-name)
  (declare (ignore organism-name))
   "postload.lisp")

(defmacro organism-postload-processing
          ((name orgf-symbol)
           (&rest transform-args)
           &body body
           )
  #.(one-string-nl
     "Create a function which will be executed after LOAD-ORGANISM"
     "has completed loading an organism's data.  NAME is the exact name"
     "of the organism.  TRANSFORM-ARGS specify transformations on the"
     "data read in, now found in the elements of the CONTIGUOUS-SEQUENCES,"
     "GENES, TRANSCRIPTS and PROTEINS lists of the organism.  Each"
     "transformation consists of a slot/column name, a function which"
     "does the transformation (defaulting to READ-FROM-STRING), and a function"
     "which checks to decide whether the transformation should be done"
     "(defaulting to STRINGP).  Each function takes a single argument,"
     "the object whose slot is being mutated."
     "The BODY forms are executed after all the specified transformations"
     "have been run.")
  (parse-transform-key-list transform-args)
  (let ((fname (organism-postload-function-name (string name))))
    (flet ((maybe-quote (x) (if (symbolp x) `',x x)))  
      `(defun ,fname ()
         (cformatt "Executing ~A..." ',fname)
         (let ((,orgf-symbol (frame-fnamed (string ,(maybe-quote name)))))
           (when (null ,orgf-symbol) 
             (ierror "NO ORGANISM FRAME for ~A" ,(maybe-quote name)))
           (with-standard-io-syntax
             (let ((*package* (find-package :bio))
                   (*readtable* (frames-readtable)))
               ;; For each organism entity
               (loop for transforms in 
                     ',(mapcar 
                        (lambda (tk) (get tk :transforms))
                        *organism-transform-keys*)
                     for entity in *organism-transform-entities* do
                     ;; For each transform on a slot of that entity
                     (when transforms (formatt ";; Transforms: "))
                     (loop for transform in transforms do
                           (execute-organism-postprocessing-transform
                            ,orgf-symbol (cons entity transform) t
                            ))
                     (when transforms (terpri)))                           
               ;; Done with transforms.  Execute body.
               (prog1
                   (progn ,@body)
                 (cformatt "Finished execution of ~A" ',fname))
               )))))))


(defparameter *generic-contig-transforms* 
  '((circular circular-string-to-t-or-nil)))

  
(defparameter *generic-gene-transforms* 
  '((architecture)
    (end-unknown circular-string-to-t-or-nil)
    (start-unknown circular-string-to-t-or-nil)
    (encodes-protein circular-string-to-t-or-nil)
    (direction string-to-direction)
    ))

(defparameter *generic-transcript-transforms* nil) 

(defparameter *generic-protein-transforms* nil)


(defmethod generic-postload-function 
       ((organisms-descriptor t)
        orgf
        &key
        (contig-transforms *generic-contig-transforms*)
        (gene-transforms *generic-gene-transforms*)
        (transcript-transforms *generic-transcript-transforms*)
        (protein-transforms *generic-protein-transforms*)
        (body-function nil)
        (verbose? nil)
        (postload-directory nil))
  (declare (ignore contig-transforms gene-transforms 
                   transcript-transforms protein-transforms
                   body-function verbose?))
   (cformatt " *** POSSIBLE PROBLEM! *** ")
   (cformatt " *** NO POSTLOAD FILE FOR ORGANISM ~A FOUND *** " orgf)
   (cformatt " *** LOOKING FOR FILE IN DIRECTORY ~A" postload-directory)
   )

(defmethod generic-postload-function 
       ((organisms-descriptor (eql :cyanobacteria))
        orgf
        &key
        (contig-transforms *generic-contig-transforms*)
        (gene-transforms *generic-gene-transforms*)
        (transcript-transforms *generic-transcript-transforms*)
        (protein-transforms *generic-protein-transforms*)
        (body-function nil)
        (verbose? nil)
        (postload-directory nil))
  (declare (ignore postload-directory))
  (warn "*** Should not be getting here!  No postload file?")
  (when verbose? (cformatt "Start generic postload function for ~A~%" orgf))
  (with-standard-io-syntax
    (let ((*package* (find-package :bio)) 
          (*readtable* (frames-readtable)))
      (loop for transforms in
            (list
             contig-transforms
             gene-transforms
             transcript-transforms
             protein-transforms)
            for entity in *organism-transform-entities*
            do 
            (loop for transform in transforms
                  do
                  (execute-organism-postprocessing-transform 
                   orgf (cons entity transform) verbose?
                   )))
      (when body-function (funcall body-function orgf))
      (when verbose? "End generic postload function for ~A~%" orgf)
      )))

(defmethod generic-postload-function 
       ((organisms-descriptor (eql :viruses))
        orgf
        &key
        (contig-transforms *generic-contig-transforms*)
        (gene-transforms *generic-gene-transforms*)
        (transcript-transforms *generic-transcript-transforms*)
        (protein-transforms *generic-protein-transforms*)
        (body-function nil)
        (verbose? nil)
        (postload-directory nil))
  (declare (ignore postload-directory))
  (when verbose? (cformatt "Start generic postload function for ~A~%" orgf))
  (with-standard-io-syntax
    (let ((*package* (find-package :bio)) 
          (*readtable* (frames-readtable)))
      (loop for transforms in
            (list
             contig-transforms
             gene-transforms
             transcript-transforms
             protein-transforms)
            for entity in *organism-transform-entities*
            do 
            (loop for transform in transforms
                  do
                  (execute-organism-postprocessing-transform 
                   orgf (cons entity transform) verbose?
                   )))
      (when body-function (funcall body-function orgf))
      (when verbose? "End generic postload function for ~A~%" orgf)
      )))

(defmethod generic-postload-function 
           ((organisms-descriptor (eql :seed-organisms))
            orgf
            &key
            (contig-transforms *generic-contig-transforms*)
            (gene-transforms *generic-gene-transforms*)
            (transcript-transforms *generic-transcript-transforms*)
            (protein-transforms *generic-protein-transforms*)
            (body-function nil)
            (verbose? nil)
            (postload-directory nil))
  (declare (ignore orgf))
  (declare (ignore contig-transforms gene-transforms
                   transcript-transforms protein-transforms
                   body-function verbose? postload-directory))
  nil
  )

(defun parse-transform-key-list (list)
  (loop for tk in *organism-transform-keys* do 
        (setf (get tk :transforms) nil))
  (let ((tk nil))
    (loop for elem in list do
          (if (member elem *organism-transform-keys*)
              (setq tk elem)
            (progn
              (unless tk (error "No transform key before transforms!"))
              (push elem (get tk :transforms))
              )))))

(defun execute-organism-postprocessing-transform 
       (orgf postprocess-directive verbose?)
  (destructuring-bind 
      (entity column &optional (transform 'read-from-string) (check 'stringp))
      postprocess-directive
    (when (not (symbolp transform))
      (setq transform (compile-transform-lambda transform)))
    (when (not (symbolp check))
      (setq check (compile-transform-lambda check)))
    (let ((entity-frame (frame-fnamed (string entity)))
          (column-frame (frame-fnamed (string column)))
          (error-count 0))
      (when (null entity-frame) 
        (ierror "Invalid organism entity: ~A" entity))
      (when column-frame
        (when verbose? (formatt "~A/~A, " column-frame entity-frame))
        (loop for element in (slotv orgf entity-frame) 
              ;; Can be used by callee as a special
	      as *the-current-frame* = element 
	      do
              (handler-case
                  (maybe-transform-slot element column-frame transform check)
                (error 
                 (c)
                 (incf error-count)
                 (when (< error-count 6)
                   (cformatt "***** Error doing postprocessing!")
                   (cformatt "  Organism entity: ~A, Column: ~A, Element: ~A"
                       entity-frame column-frame element)
                   (cformatt "  Actual error signalled: ~A" c)))))
        (when (>= error-count 6)
          (cformatt "  *** And ~D more errors for this column."
                    (- error-count 5)
                    ))))))

(defun compile-transform-lambda (lambda-form)
  (cond
   ((and (listp lambda-form) (eq (first lambda-form) 'function))
    (compile-transform-lambda (cadr lambda-form)))
   ((and (listp lambda-form) (eq (first lambda-form) 'lambda))
    (compile nil lambda-form))
   (t (error "Unknown function form in entity transformation: ~A" lambda-form))
   ))

(defun maybe-transform-slot (element slot transform-function check-function)
  (let ((slot-value (slotv element slot)))
    (when (funcall check-function slot-value)
      (setf (slotv element slot) (funcall transform-function slot-value))
      )))


;;; Standard transformations.

(defun default-string-means-not-present (s)
  #.(optimization-declaration)
  (declare (simple-string s))
  (let ((len (length s)))
    (declare (fixnum len))
    (or (zerop len) (and (= 1 len) (eql (schar s 0) #\-)))
    ))

(defun string-means-not-present 
       (s &optional (f 'default-string-means-not-present))
  #.(optimization-declaration)
  (declare (simple-string s))
  (funcall f s))

(defun string-to-lisp-object (s) 
  #.(one-string-nl
     "Read S and interpret the characters as a lisp object."
     "Does not necessarily read the entire string.  It stops as soon as"
     "a valid Lisp object can be formed.  (Uses READ-FROM-STRING).")
  (read-from-string s))

(defun string-to-double-float (s)
  #.(one-string-nl
     "Reads S and interprets the characters as a lisp double float"
     "or errors if it cannot")
    (let* ((*read-default-float-format* 'double-float)
           (result (string-to-lisp-object s)))
      (if (typep result 'double-float)
          result
        (error "String does not represent a valid double float: ~S" s))))

(defun string-to-single-float (s)
  #.(one-string-nl
     "Reads S and interprets the characters as a lisp single float"
     "or errors if it cannot")
    (let* ((*read-default-float-format* 'single-float)
           (result (string-to-lisp-object s)))
      (if (typep result 'single-float)
          result
        (error "String does not represent a valid single float: ~S" s))))

(defun string-to-integer (s) 
  #.(one-string-nl
     "Reads S and interprets the characters as a lisp integer"
     "or errors if it cannot")
  (parse-integer s))

(defun string-to-list (s) 
  #.(one-string-nl
     "Reads S and interprets the characters as a lisp list"
     "or errors if it cannot.  The READ is done in the context of the"
     "BIOLISP package and the BIOLISP readtable (enabling '#$' and '#^')"
     "and otherwise with standard Lisp syntax in declare.")
  (let ((result (string-to-lisp-object s)))
    (if (listp result)
        result
      (error "String when read is not a list: ~S" s))))
    
(defun string-to-double-float-or-nil (s)
  #.(one-string-nl
     "If S is the null string or the one character string '-' returns NIL."
     "Otherwise reads S and interprets the characters as a lisp double float"
     "or errors if it cannot.")
  (unless (string-means-not-present s) (string-to-double-float s)))

(defun string-to-single-float-or-nil (s)
  #.(one-string-nl
     "If S is the null string or the one character string '-' returns NIL."
     "Otherwise reads S and interprets the characters as a lisp single float"
     "or errors if it cannot.")
  (unless (string-means-not-present s) (string-to-single-float s)))

(defun string-to-integer-or-nil (s)
  #.(one-string-nl
     "If S is the null string or the one character string '-' returns NIL."
     "Otherwise reads S and interprets the characters as a lisp integer"
     "or errors if it cannot.")
  (unless (string-means-not-present s) (string-to-integer s)))

(defun string-to-list-or-nil (s) 
  #.(one-string-nl
     "If S is the null string or the one character string '-' returns NIL."
     "Otherwise reads S and interprets the characters as a lisp list"
     "or errors if it cannot.  The READ is done in the context of the"
     "BIOLISP package and the BIOLISP readtable (enabling '#$' and '#^')"
     "and otherwise with standard Lisp syntax in effect.")
  (unless (string-means-not-present s) (string-to-list s)))

(defun string-to-direction (s)
  #.(one-string-nl
     "If S is the null string or the one character string '-', or "
     "the string 'F' or 'f' returns :f.  If S is the string 'B' or 'b'"
     "returns :b.  Otherwise it errors.")
  (declare (simple-string s))
  (if (string-means-not-present s)
      :f
    (let ((len (length s)))
      (unless (= 1 len) (error "Not a valid direction string: ~S" s))
      (let ((ch (schar s 0)))
        (cond
         ((or (eql ch #\F) (eql ch #\f)) :f)
         ((or (eql ch #\B) (eql ch #\b)) :b)
         (t (error "Not a direction string: ~S" s))
         )))))

(defun boolean-string-to-boolean (s)
  #.(one-string-nl
     "If S is a one-character string containing 'F' or 'f' returns NIL."
     "If S is a one-character string containing 'T' or 't' returns T."
     "Otherwise it errors.")
  (declare (simple-string s))
  (let ((ch (schar s 0)))
    (cond
     ((or (eql ch #\T) (eql ch #\t)) t)
     ((or (eql ch #\F) (eql ch #\f)) nil)
     (t (error "Not a valid 'boolean' string: ~S" s))
     )))
  
(defun boolean-string-to-boolean-or-nil (s)
  #.(one-string-nl
     "If S is the numm string or the one character string '-', returns NIL."
     "If S is a one-character string containing 'F' or 'f' returns NIL."
     "If S is a one-character string containing 'T' or 't' returns T."
     "Otherwise it errors.")
  (declare (simple-string s))
  (unless (string-means-not-present s) (boolean-string-to-boolean s)))

(defun circular-string-to-t-or-nil (s)
  #.(one-string-nl
     "If S is the null string or the one character string '-', or "
     "the string 'F' or 'f' returns NIL.  If S is the string 'T' or 't'"
     "returns T.  Otherwise it errors.")
  (declare (simple-string s))
  ;; Not present means NIL.
  (boolean-string-to-boolean-or-nil s))


#+template
(in-package :bio)

#+template
(organism-postload-processing
    ;; Put the exact name of the organism here as the first list element,
    ;; and a symbol you want bound to the organism's frame when the body
    ;; below gets executed.
    (:nostoc_punctiforme_atcc29133 nostoc)
    (
     :contig-transforms
     ;; Transformations for #$Contiguous-Sequence slots go here.
     (circular circular-string-to-t-or-nil)
     :gene-transforms 
     ;; Transformations for #$Genes slots go here.
     (direction string-to-direction)
     :transcript-transforms
     ;; Transformations for #$Transcripts slots go here.  Currently none.
     :proteins-transforms
     ;; Transformations for #$Proteins slots go here.  Currently none.
     )
  ;; Any other code you want executed after the above transformations
  ;; have been done goes here.  The organism frame is bound to the
  ;; symbol which is the second element of the initial list above
  ;; while this code is being executed.
  "ANY OTHER CODE GOES HERE"
  )
