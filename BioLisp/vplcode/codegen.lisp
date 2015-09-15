;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl) 

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

;;; create actual code that could be executed (modulo holes) from a snippet.

(defvar *allow-holes-in-code?* nil)
(defvar *splice?* nil)

(defmacro monitor (form)
  (cond
   ((symbolp form) 
    `(progn (format t "~&~a: ~a" ',form ,form) ,form))
   ((listp form)
    (let ((temp-symbol (gensym "MONITOR-"))
          (function (first form)))
      `(let ((,temp-symbol ,form))
         (format t "~&Call to ~A returns: ~A" ',function ,temp-symbol)
         ,temp-symbol
         )))))

(defun snippet-to-code 
       (snippet &optional (holes-allowed? *allow-holes-in-code?*))
  (let ((*allow-holes-in-code?* holes-allowed?)
        (*splice?* (get-snippet-property snippet :splice)))
    (vdbg "Snippet->Code: ~S, ~S~%" (type-of snippet) *splice?*)
    (snippet-to-code-method snippet)
    ))

(defmethod snippet-to-code-method ((snippet snippet))
  (vpl-internal-error 
   "Should not be calling snippet-to-code on snippet of type ~A"
   (type-of snippet)))

(defmethod snippet-to-code-method ((snippet constant-snippet))
   (values (snippet-value snippet) nil))

(defmethod snippet-to-code-method ((snippet symbol-snippet))
  (cond
   ((get-snippet-property snippet :monitoring-enabled)
    (let ((symbol (snippet-value snippet)))
      (values `(monitor ,symbol) nil)
      ))
   (t (values (snippet-value snippet) nil))
   ))

(defmethod snippet-to-code-method ((snippet literal-snippet))
  (let* ((s (string-upcase (snippet-value snippet)))
         (package 
          (or 
           (vwhen (p (get-snippet-property snippet :intern-package))
             (find-package p))
           (and (find-symbol s :bbl) (find-package :bbl))
           *package* 
           )))
    (vdbg "Literal string: ~S, package: ~A~%" s package)
    (unless package 
      (vpl-user-error "Unknown name ~S; not in any known package!" s))
    (values (intern s package) nil)
    ))

(defmethod snippet-to-code-method ((snippet form-snippet))
  (if (snippet-is-legal-hole? snippet) 
      (values +hole+ nil)
    (vpl-internal-error "Should never have FORM other than as hole?")))

(defmethod snippet-to-code-method ((snippet flag-snippet))
  (values (snippet-value snippet) nil))

(defmethod snippet-to-code-method ((snippet keyword-snippet))
  (values 
   (mapcar 'snippet-to-code (snippet-children snippet))
   t))

(defmethod snippet-to-code-method ((snippet progn-snippet))
  (values 
   (collect-child-code snippet)
   *splice?*
   ))

(defmethod snippet-to-code-method ((snippet keys-and-flags-snippet))
  (values 
   (collect-child-code snippet)
   t
   ))

(defmethod snippet-to-code-method ((snippet uniform-choice-snippet))
  (case (get-snippet-property snippet :choice-type)
    (:optional-arg 
     (let ((len (length (snippet-children snippet))))
       (case len
         (0 
          (let ((default (get-snippet-property snippet :default)))
            (case default
              (:=splice-no-value= (values nil t))
              (otherwise (values default nil))
              )))
         (1 (values (first (collect-child-code snippet)) nil))
         (otherwise 
          (vpl-internal-error 
           "Optional arg nodes should have at most one child!"))
         )))
    (:token 
     (let ((len (length (snippet-children snippet))))
       (case len
         (0 (values nil t))
         (1 (values (first (collect-child-code snippet)) nil))
         (otherwise 
          (vpl-internal-error 
           "Token choice nodes should have at most one selected token!"))
         )))
    (:simple-choice 
     (let ((len (length (snippet-children snippet))))
       (case len
         (0 
          (vpl-user-error 
           "Must choose something for ~A~%" (snippet-label snippet)))
         (1 (values (first (collect-child-code snippet)) nil))
         (otherwise 
          (vpl-internal-error 
           "Simple choice nodes should have exactly one selected token!"
           )))))
    (otherwise 
     (values 
      (collect-child-code snippet)
      *splice?*
      ))))

(defmethod snippet-to-code-method ((snippet aggregate-snippet))
  (when (and (null *allow-holes-in-code?*)
             (get-snippet-property snippet :one-form-required) 
             (or (null (snippet-children snippet))
                 (snippet-is-hole? (first (snippet-children snippet)))))
    (vpl-user-error 
     #.(one-string-nl
        "Cannot generate code because one of your clauses requires at least"
        "one value but none has been provided."
        ))) 
  (values 
   (unless (ignore-single-child? snippet)
     (collect-child-code snippet))
   *splice?*
   ))

(defmethod snippet-to-code-method ((snippet call-snippet))
  (cond 
   ((ref-snippet? snippet) (ref-snippet-to-code snippet))
   ((range-ref-snippet? snippet) (range-ref-snippet-to-code snippet))
   ((curly-snippet? snippet) (curly-snippet-to-code snippet))
   ((comment-snippet? snippet) (values nil t))
   (t 
    (cond
     ((get-snippet-property snippet :monitoring-enabled)
      (values `(monitor ,(collect-child-code snippet)) nil))
     (t (values (collect-child-code snippet) nil))
     ))))

(defun collect-child-code (snippet)
  (let ((code nil))
    (loop for child in (snippet-children snippet) do
          (multiple-value-bind (child-code splice?)
              (snippet-to-code child)
            (setq code (add-to-end child-code code splice?))
            ))
    code
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ref-snippet? (snippet)
  (let ((sval (snippet-children snippet)))
    (and (= (length sval) 5)
         (let ((third (third sval)))
           (and (typep third 'literal-snippet)
                (string= "[" (string (snippet-value third)))
                )))))

(defun range-ref-snippet? (snippet)
  (let ((sval (snippet-children snippet)))
    (and (= (length sval) 7)
         (let ((third (third sval)))
           (and (typep third 'literal-snippet)
                (string= "[" (string (snippet-value third)))
                )))))

(defun curly-snippet? (snippet)
  (let ((sval (snippet-children snippet)))
    (let ((start (first sval)))
      (and
       (typep start 'literal-snippet)
       (string= (string (snippet-value start)) "{")
       ))))

(defun comment-snippet? (snippet)
  (let ((sval (snippet-children snippet)))
    (let ((start (first sval)))
      (and
       (typep start 'literal-snippet)
       (string-equal (string (snippet-value start)) "comment")
       ))))

(defun ref-snippet-to-code (ref-snippet)
  (values
   `(ref ,(snippet-to-code (nth-snippet-child ref-snippet 2))
         ,@(snippet-to-code (nth-snippet-child ref-snippet 4)))
   nil))

(defun range-ref-snippet-to-code (ref-snippet)
  (values 
   `(ref 
     ,(snippet-to-code (nth-snippet-child ref-snippet 2))
     ,(snippet-to-code (nth-snippet-child ref-snippet 4))
     utils::->
     ,(snippet-to-code (nth-snippet-child ref-snippet 6))
     )
   nil
   ))

(defun curly-snippet-to-code (curly-snippet)
  (let* ((zero-or-more-node (second (snippet-children curly-snippet)))
         (list-items (snippet-children zero-or-more-node)))
    (values
     `(bbi::%curly-list% ,@(mapcar 'snippet-to-code list-items))
     nil
     )))

(defun add-to-end (thing list splice?)
  (ecase splice?
    ((:splice t) 
     (when (not (listp thing))
       (error "Internal error: Attempting to splice non-list: ~S" thing))
     (append list thing))
    ((:no-splice nil) 
     (append list (list thing))
     )))

(defun snippet-is-legal-hole? (snippet)
  (if (not (snippet-is-hole? snippet))
      nil
    (if *allow-holes-in-code?* 
        t
      (error 
       (make-condition 
        'hole-error
        :format-control "Hole found but not legal in code snippet ~S"
        :format-arguments (list snippet)
        )))))

;; We ignore a single child if it is a hole and the :display-one-hole
;; property is enabled.
(defun ignore-single-child? (snippet)
  (let ((children (snippet-children snippet)))
    (and (= 1 (length children))
         (null (get-snippet-property snippet :one-form-required))
         (get-snippet-property snippet :display-one-hole)
         (snippet-is-hole? (first children))
         )))

