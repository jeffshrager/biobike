;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Peter Seibel           |
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

;;; Authors:  JP Massar, Peter Seibel.

(publish
 :path *help-function-documentation-url* 
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (user-package-name (url-parameter-value :pkg input))
          (user-package-symbol (keywordize user-package-name))
          (name (url-parameter-value :name input))
          (package (url-parameter-value :package input)))
     (execute-with-standard-weblistener-environment
      req ent user-package-symbol
      (lambda ()
        (com.gigamonkeys.foo:with-html-output ((request-reply-stream req))
          (html-for-help-function (keywordize package) name))
        )))))

(defun help::html-string-for-function-documentation (symbol)
  (let ((package (keywordize (package-name (symbol-package symbol))))
        (name (symbol-name symbol)))
    (with-output-to-string (p)
      (let ((net.html.generator::*html-stream* p))
        (with-output-to-string (q)
          (let* ((com.gigamonkeys.foo.html::*text-output* q))
            (html-for-help-function package name)
            ))))))

#+spews
(defun help::html-string-for-function-documentation (symbol)
  (let ((package (keywordize (package-name (symbol-package symbol))))
        (name (symbol-name symbol)))
    (with-output-to-string (p)
      (let ((net.html.generator::*html-stream* p))
        (html-for-help-function package name)
        ))))

(defvar *documented-function-name* nil)

(defun html-for-help-function (package-symbol symbol-name)
  (let ((docs (help:find-documentation 
	       (intern symbol-name (find-package package-symbol)) 
	       'help:function-documentation)))
    (if docs 
        ;; special case when docobj is an alias for another
        (if (eq (help:explicitly-documented-p docs) :alias-of) 
            (let ((real-function (get (help:name docs) :alias-of)))
              (html-for-help-function
               (keywordize (package-name (symbol-package real-function)))
               (symbol-name real-function)
               ))
          (html-for-help-function/flavor docs (help:flavor docs)))
      (html 
       (:princ-safe 
        (formatn 
         "Internal error!  No function doc for ~A::~A"
         package-symbol symbol-name))))))

(defun html-for-help-function/flavor (docs flavor)

  (with-accessors 
      ((name name)
       (syntax syntax)
       (vpl-syntax vpl-syntax)
       (synonyms synonyms)
       (author author)
       (docstring docstring)
       (parameters parameters)
       (text text)
       (examples examples)
       (examples-package examples-package)
       (keywords keywords)
       (see-also see-also)
       (return-values return-values)
       (explicitly-documented-p explicitly-documented-p)
       (referred-to-by referred-to-by)
       (module module)) 
      docs
    
    (destructuring-bind
        (&optional 
         return-desc 
         &key ((:type return-type) 'unknown) (display-type t))
        return-values

      (let ((title 
             (formatn "Function ~a~@[ (a.k.a. ~{~a~^, ~})~]" name synonyms))
            (*examples-package* examples-package)
            (*documented-function-name* name))

	(with-standard-weblistener-page-header (title)
          
	  (html-for-module-help-links name)

	  (if explicitly-documented-p

	      (html

               (html-for-summary-information 
                docstring :size :h2 :paragraph-after-title? t)
#|
               (when syntax 
                 (html (:h2 "Syntax")
                       (loop for n in (list* name synonyms) do
                             (emit-syntax 
                              (list* n (rest syntax)) parameters flavor))))
|#
               (when (AND syntax (NOT vpl-syntax))
                 (html (:h2 "Syntax")
                       (loop for n in (list* name synonyms) do
                             (emit-syntax 
                              (list* n (rest syntax)) parameters flavor))))

               (when vpl-syntax 
                 (html (:h2 "VPL Syntax"))
                 (html-for-vpl-syntax-information vpl-syntax))

               (:h2 "Parameters")
               :p
               (if parameters
                   (emit-parameters parameters flavor)
                 (html (:i "None")))

               (:h2 "Returns")
               (:p (:princ-safe 
                    (if (or 
                         (stringp return-desc)
                         (not display-type) 
                         (member return-type '(t unknown boolean)))
                        (formatn "~a" return-desc)
                      (formatn "An object of type ~A"
                               (type-to-english-string return-type))
                      )))

               (when examples
                 (html (:h2 "Examples"))
                 (dolist (example examples)
                   (create-html-from-raw-example example)
                   ))
	    
               (html-for-text-information
                text :size :h2 :paragraph-after-title? nil)

               (when keywords (html-for-keyword-information keywords :size :h2))
	    
               (if see-also
                   (progn
                     (html (:h2 "See also"))
                     (html-for-see-also-information see-also nil))
                 (html (:h2 "See also") (:princ-safe "(No references)"))
                 )
               
               (when referred-to-by 
                 (html-for-referred-to-by-information referred-to-by))
               
               ;; need to put source link here
               ;; (emit-see-also-information see-also name)
               
               (html-for-author-information
                author :size :h2 :paragraph-after-title? t)
               
               :p
               ((:a :href 
                 (forward-package-funcall 
                  :wb :url-with-package-name-and-symbol-name 
                  'wb::make-doc-find-function-source-url name)) 
                ((:font :color "green") "Source code link"))

               )

	      (html (:h2 "Undocumented."))))))))



;; Spaces and line breaks
(defun emit-parameters (parameters flavor)
  (let ((syntactic-tokens 
         (get *documented-function-name* :syntactic-tokens)))
    (loop
      for p in parameters
      as parameter-name = (or (keyword-name p) (name p))
      do
      (unless 
          (and (symbol= :token (parameter-type p))
               (member parameter-name syntactic-tokens :test 'symbol=))
        (emit-parameters-aux p flavor)
        )
      )))

(defparameter *required-color* "red")
(defparameter *keyword-color* "green")
(defparameter *flag-color* "blue")
(defparameter *token-color* "brown")
(defparameter *optional-color* "orange")

#||
Spec

Thing A <argument-type> argument of type <types>.  
Can convert from <convert types> and mapped using <mapping style>

still working on english types, conversion types
might want to out code for different types

||#

(defun emit-parameter-description-text 
       (text &key comma? period? preceding-space?)
  (when comma? (html (:princ-safe ",") (:princ "&nbsp;")))
  (when preceding-space? (html (:princ "&nbsp;")))
  (html (:princ-safe text))
  (when period? (html (:princ-safe ".") (:princ "&nbsp;&nbsp;"))))


(defun emit-parameters-aux (p flavor)
  (declare (ignorable flavor))
  (let ((ptype (parameter-type p)))
    (html
     (:p
      (:b 
       ((:font :color 
         (cond
          ((symbol= ptype :required) *required-color*)
          ((symbol= ptype :keyword) *keyword-color*)
          ((symbol= ptype :flag) *flag-color*)
          ((symbol= ptype :token) *token-color*)
          ((symbol= ptype :optional) *optional-color*)
          ((symbol= ptype :&key) *keyword-color*)
          ((symbol= ptype :&optional) *optional-color*)
          ))
        (emit-parameter-description-text (format nil "~A" (name p)))))
      (:i 
       (emit-parameter-description-text 
        (format nil "~A ~A argument" 
                (if (symbol= :optional ptype) "An" "A")
                ptype)
        :preceding-space? t
        ))
      (:i
       (cond
        ((symbol= (value-type p) 't)
         (emit-parameter-description-text "of any type" :preceding-space? t))
        ((or (symbol= ptype :flag) (symbol= ptype :token)) nil)
        (t 
         (html
          (emit-parameter-description-text 
           (format nil "of type ~A" (type-to-english-string (value-type p)))
           :preceding-space? t
           )))))
      (:i
       (when (symbol= (parameter-type p) :keyword)
         (when (default-value p)
           (unless (eq (find-symbol "<NONE>" :bbl)
                       (fudge-default-value (default-value p)))
             (emit-parameter-description-text
              (format nil "with a default value of ~A" (default-value p))
              :preceding-space? t
              )))))
      ;; first period
      (:i (emit-parameter-description-text "" :period? t))
      (:i  
       (let ((ctypes (can-convert-from p)))
         (when ctypes
           (html :br (:princ "&nbsp;&nbsp;&mdash; ")
            (emit-parameter-description-text
             (format nil 
                 "Automatically converts from type ~A."
                 (if (= 1 (length ctypes))
                     (first ctypes)
                     (type-to-english-string (cons 'or (can-convert-from p)))
                      )))))))
      (:i 
       (IF (mapping-style p)
         (html :br (:princ "&nbsp;&nbsp;&mdash; ")
           (emit-parameter-description-text 
             (format nil 
                "The function is automatically mapped over this variable~A"
                (IF (EQUAL (mapping-style p) :MAPCAR)
                    "."
                   (S+ " using " (mapping-style p) "."))))))
        )
      (html :br 
            (:princ "&nbsp;&nbsp;&mdash; ")
            ;; Allow the parameter description to be processed by FOO to allow
            ;; documenters to link and put in arbitrary HTML
            (let ((ds (docstring p)))
              (cond 
               ((stringp ds) (html (:princ-safe ds)))
               ((consp ds) (eval `(com.gigamonkeys.foo:html ,ds)))
               (t (html (:princ-safe "No description")))
               ))
            )
           
      ))))



(defun html-for-function-parameter (p flavor)
  (html 
   (:tr 
    (:td ((:font :color "brown") 
          (:princ-safe 
           (formatn "~(~a~)" (or (keyword-name p) (name p))))))
    ((:td :align :center)
     (if (member (parameter-type p) lambda-list-keywords)
         (html (:code (:princ-safe 
                       (formatn "~(~a~)" (parameter-type p)))))
       (html (:i (:princ-safe 
		  (formatn "~(~a~)" (parameter-type p)))))))
    ((:td :align :center)
     (html (:code (:princ-safe
                   (if (eql flavor :define-function)
		       (esthetic-parameter-type (value-type p))
		       (formatn "~(~a~)" (value-type p)))))))
    (when (eql flavor :define-function)
      (html
       ((:td :align :center) 
        (:princ-safe (formatn "~(~a~)" (or (mapping-style p) :-))))
       ((:td :align :center) 
        (:princ-safe 
         (formatn "~:[-~;~:*~{~(~a~)~^, ~}~]" (can-convert-from p))))))
    ((:td :align :center)
     (if (member (parameter-type p) '(&optional &key :keyword cl:keyword)) 
         (html 
          (:code 
           (:princ-safe 
            (formatn "~(~s~)" (fudge-default-value (default-value p))))))
       (html "&mdash;")))
    (:td (if (docstring p) 
             (html (:princ-safe (docstring p)))
           (html ((:span :style "color: red;") "UNDOCUMENTED")))))))

(defun fudge-default-value (x) 
  (if (symbolp x)
      (let ((p (find-package :bbi))
            (n (symbol-name x)))
        (if (eq (symbol-package x) p)
            (cond 
             ((string-equal "*unprovided+*" n) (find-symbol "<NONE>" :bbl))
             ((string-equal "*unprovided-*" n) (find-symbol "<NONE>" :bbl))
             ((string-equal "*unprovided-string*" n) 
              (find-symbol "<NONE>" :bbl))
             (t x)
             )
          x
          ))
    x
    ))


(defgeneric emit-syntax 
    (syntax parameters flavor &key (para? t) (name? t)))

(defmethod emit-syntax
           (syntax parameters (flavor (eql :defun)) 
                   &key (para? t) (name? t))
  (declare (ignore parameters name?))
  (when para? (html :p))
  (html (:code (:princ-safe (formatn "~(~a~)" syntax)))))

(defmethod emit-syntax
           (syntax parameters (flavor (eql :define-function)) 
                   &key (para? t) (name? t))
  (flet ((type (parameter-name)
	   (esthetic-type-name
	    (value-type (find parameter-name parameters :key #'name)))))
    (when para? (html :p))
    (html
     (:code "(" (when name? (html (:princ-safe (first syntax)) " ")))
     (loop for (item . rest) on (rest syntax) do
           (cond
            ((symbolp item) 
             (html (:i (:princ-safe (string-downcase item)))))
            ((consp item)
             (case (first item)
               (:noise 
                (html "[" 
                      (loop for (x . rest) on (rest item) do
                            (html (:code (:princ-safe x)))
                            (when rest (html " | ")))
                      "]"))
               (:keyword 
                (html 
                 "[" 
                 (:code 
                  (:princ-safe 
                   (formatn "~{~#[~;~a~:;[~@{~a~^ | ~}]~]~}" (rest item))))
                 " "
                 (:i (:princ-safe (type (second item)))) "]"))
               (:flag (html "[" (:code (:princ-safe (second item))) "]")))))
           (when rest (html " ")))
     (:code ")"))))


(defun esthetic-type-name (type)
  (typecase type
    (symbol
     (if (symbol= type t)  "any" (string-downcase type)))
    (cons 
     (if (eql (car type) 'or)
         (let ((hacked-type (remove-unprovided-from-or-type type)))
           (formatn "~(~{~a~^-or-~}~)" (rest hacked-type))
           )
       (formatn "~(~a~)" type)))
    (t 
     (formatn "~(~a~)" type))))

(defun esthetic-parameter-type (type)
  (typecase type
    (symbol
     (if (symbol= type t)  "Any" (string-downcase type)))
    (cons 
     (if (eql (car type) 'or)
         (let ((hacked-type (remove-unprovided-from-or-type type)))
           (formatn
            "~(~{~#[~;~a~;~a or ~a~:;~@{~a~#[~;, or ~:;, ~]~}~]~}~)" 
            (rest hacked-type)
            ))
       (formatn "~(~a~)" type)
       ))
    (t 
     (formatn "~(~a~)" type))))

(defun remove-unprovided-from-or-type (type)
  (if (find-package :bbi)
      (remove 
       (intern "NONPROVIDED" :bbi)
       (remove (intern "UNPROVIDED" :bbi) type)
       )
    type
    ))