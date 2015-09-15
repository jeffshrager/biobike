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

(defconstant *max-user-stack-size* #.(expt 2 20))
(defconstant *initial-user-stack-size* 64)

(defvar *user-stack* (make-array 1 :initial-element nil))
(defvar *user-stack-index-limit* -1)
(defvar *user-stack-index* 0)
(declaim (type (simple-array t (*)) *user-stack*))
(declaim (fixnum *user-stack-index* *user-stack-index-limit*))

(defvar *lexical-us-var* '=user-stack=)
(defvar *lexical-usil-var* '=user-stack-il=)


(defun standard-toplevel-bbl-bindings ()
  (values
   `((*user-stack* (make-array *initial-user-stack-size* :initial-element nil))
     (*user-stack-index* 0)
     (*user-stack-index-limit* *initial-user-stack-size*)
     (,*lexical-us-var* *user-stack*)
     (,*lexical-usil-var* *initial-user-stack-size*)
     )
   `((declare (type (simple-array t (*)) ,*lexical-us-var*))
     (declare (fixnum ,*lexical-usil-var*))
     (declare (ignorable ,*lexical-us-var* ,*lexical-usil-var*))
     )))

(defmacro with-standard-toplevel-bbl-bindings (&body body)
  `(let ((*user-stack* 
          (make-array *initial-user-stack-size* :initial-element nil))
         (*user-stack-index* 0)
         (*user-stack-index-limit* *initial-user-stack-size*)
         (,*lexical-us-var* *user-stack*)
         (,*lexical-usil-var* *initial-user-stack-size*))
     (declare (type (simple-array t (*)) ,*lexical-us-var*))
     (declare (fixnum ,*lexical-usil-var*))
     (declare (ignorable ,*lexical-us-var* ,*lexical-usil-var*))
     ,@body))


(defun standard-definition-bbl-bindings ()
  (values
   `((,*lexical-us-var* *user-stack*)
     (,*lexical-usil-var* *user-stack-index-limit*)
     )
   `((declare (type (simple-array t (*)) ,*lexical-us-var*))
     (declare (fixnum ,*lexical-usil-var*))
     (declare (ignorable ,*lexical-us-var* ,*lexical-usil-var*))
     )))

(defmacro unsafely (&body body)
  `(locally
     #.(optimization-declaration)
     ;; (declare (optimize (speed 3) (safety 0) (debug 0)))     
     ,@body
     ))

(defun stack-trace-form-start-string (form &optional (length 50))
  (limited-form-string 
   form length 
   :format-mode "~S" :print-pretty t :single-line? t :strip-indentation? t
   ))

(defun increase-user-stack-or-die ()
  (let ((current-size (length *user-stack*)))
    (when (> current-size 100000)
      (error 
       (one-string-nl
        "BioBike stack overflow... You are probably in an infinite"
        "(or at least very deep) recursion -- the stack is more than"
        "100,000 deep! Please contact the system support staff for"
        "assistance; your algorithm may have to be redesigned."
        )))
    (let ((new-stack (make-array (* current-size 4))))
      (replace new-stack *user-stack*)
      (setq *user-stack* new-stack)
      (setq *user-stack-index-limit* (length new-stack))
      (values *user-stack* *user-stack-index-limit*)
      )))

(defun verify-bbl-stack-array-index (index array)
  (declare (fixnum index))
  (declare (type (simple-array t (*)) array))
  #.(optimization-declaration)
  (when (= -1 *user-stack-index-limit*) 
    (error 
     "Stack violation!  You are probably executing BBL code not in BBL context."
     ))
  (when (> index (length array))
    (error 
     (one-string-nl
      "Stack violation!  Stack-index ~D greater than stack size ~D"
      "You are probably executing BBL code not in BBL context."
      )
     index (length array))))
  

(defmacro with-bbl-stack-trace 
          (&body 
           kludge
           &aux 
           (original-form (first kludge))
           (executable-form (if (cdr kludge) (cadr kludge) original-form))
           )
  (let ((len (length kludge)))
    (unless (or (= len 1) (= len 2))
      (error "Internal error: Bad format for WITH-BBL-STACK-TRACE")))
  (if (not *safety*)
      executable-form 
    (let ((index-var (gensym "STACK-INDEX-")))
      `(let* ((,index-var (unsafely *user-stack-index*))
              (*user-stack-index* (unsafely (the fixnum (1+ ,index-var)))))
         (declare (fixnum ,index-var ,*lexical-usil-var*))
         (declare (type (simple-array t (*)) ,*lexical-us-var*))
         (unsafely
           (verify-bbl-stack-array-index ,index-var ,*lexical-us-var*)
           (when (>= ,index-var ,*lexical-usil-var*)
             (multiple-value-setq (,*lexical-us-var* ,*lexical-usil-var*)
                 (increase-user-stack-or-die)))
           (setf (aref ,*lexical-us-var* ,index-var) 
                 ,(stack-trace-form-start-string original-form)))
         ,executable-form 
         ))))

