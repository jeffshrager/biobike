;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb)

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

;;; Authors:  JP Massar, Peter Seibel.

;;; Creates html pages for new-style documentation derived from 
;;; DOCUMENT-FUNCTION forms.  

(defparameter *examples-package* nil)

(defun html-for-help-links (&optional current-module)
  (let* ((all-modules (make-help-modules-url)))
    (html
     ((:a :href all-modules) "All Modules"))
    (when current-module
      (let* ((module-name (com.biobike.help::name current-module))
	     (current-module (help-module-url module-name))
             )
	(html " | " ((:a :href current-module) "Module: " 
                     (:princ-safe module-name)))))))

(defun help-module-url (module-symbol)
  (make-help-module-url 
   :package (package-name (symbol-package module-symbol)) 
   :symbol (symbol-name module-symbol)
   ))

(defun html-for-help-function (package-symbol symbol-name)
  (let ((docs (com.biobike.help::find-documentation 
	       (intern symbol-name (find-package package-symbol)) 
	       'com.biobike.help::function-documentation)))
    (html-for-help-function/flavor docs (com.biobike.help::flavor docs))))

(defun html-for-help-function/flavor (docs flavor)
  (with-accessors ((name name)
		   (syntax syntax)
		   (synonyms synonyms)
		   (author author)
		   (docstring docstring)
		   (parameters parameters)
		   (text text)
		   (examples examples)
		   (examples-package examples-package)
		   (see-also see-also)
		   (return-values return-values)
		   (explicitly-documented-p explicitly-documented-p)
		   (com.biobike.help::module com.biobike.help::module)) docs
    (destructuring-bind
        (&optional 
         return-desc 
         &key ((:type return-type) 'unknown) (display-type t))
        return-values
      (let ((title 
             (formatn "Function ~a~@[ (a.k.a. ~{~a~^, ~})~]" name synonyms))
            (*examples-package* examples-package))
	(with-standard-weblistener-page-header (title)
	  (html-for-help-links com.biobike.help::module)
	  (if explicitly-documented-p
	      (html
               (:h2 "Summary")
               (:p (:princ-safe docstring))
               (when syntax 
                 (html (:h2 "Syntax")
                       (loop for n in (list* name synonyms) do
                             (emit-syntax 
                              (list* n (rest syntax)) parameters flavor))))
               (:h2 "Parameters")
               :p
               (if parameters
                   (emit-parameters parameters flavor)
                 (html (:i "None")))
               (:h2 "Returns")
               (:p (:princ-safe 
                    (if (or (not display-type) 
                            (member return-type '(t unknown boolean)))
                        (formatn "~a" return-desc)
			(formatn "~:[~:(~a~)~;~:*~a as ~a~] ~(~a~)." 
                                 return-desc 
                                 (a-or-an 
                                  (esthetic-parameter-type return-type)) 
                                 return-type))))
               (when examples
                 (html (:h2 "Examples"))
                 (dolist (example examples)
                   (create-html-from-raw-example example)
                   ))
	    
               (:h2 "Description")
               (:p (map nil #'com.gigamonkeys.foo:emit-html text))
	    
               (html (:h2 "See also"))
               (emit-see-also-information see-also name)
               (when author
                 (html (:h2 "Author")
                       (:p (:princ-safe (formatn "~{~a~^, ~}" author))))))
	      (html (:h2 "Undocumented."))))))))

(defun a-or-an (string)
  (if (find (char-downcase (char (string string) 0)) "aeiou") "an" "a"))
      

(defun emit-see-also-information (see-also name)
  (multiple-value-bind (cl-functions sys-functions hyperlinks other)
      (separate-into-lists 
       (canonicalize-see-also-items see-also)
       (lambda (x) (eq (first x) :cl-function))
       (lambda (x) (eq (first x) :sys-function))
       (lambda (x) (eq (first x) :hyperlink)))
    (flet ((title (string) 
             (html "&nbsp;&nbsp; " (:princ-safe string) ": ")))
      (when cl-functions 
        (html 
         (title "Lisp functions")
         (loop for ((nil cl-function-url cl-function-name) . rest) 
               on cl-functions do
               (html ((:a :href cl-function-url)
                      (:code (:princ-safe cl-function-name))))
               (when rest (html ", ")))
         :br))
      (when sys-functions
        (html
         (title "System functions")
         (loop for ((nil sys-function-url sys-function-name) . rest) 
               on sys-functions do
               (html ((:a :href sys-function-url)
                      (:code (:princ-safe sys-function-name))))
               (when rest (html ", ")))
         :br))
      (when hyperlinks
        (html 
         (title "Other web references")
         (loop for ((nil url description) . rest) on hyperlinks do 
               (html ((:a :href url) (:princ-safe description)))
               (when rest (html ", ")))
         :br))
      (when other 
        (html
         (title "Other information")
         (loop for ((nil info) . rest) on other do
               (html (:princ-safe info))
               (when rest (html ", ")))
         :br))
      (html
       "&nbsp;&nbsp; "
       ((:a :href 
         (url-with-package-name-and-symbol-name 
          'make-doc-find-function-source-url name)) 
        ((:font :color "green") "Source code")))
      )))

(defun canonicalize-see-also-items (see-also)
  (loop for item in see-also collect
        (cond
         ((symbolp item)
          (if (eql (symbol-package item) (find-package :cl))
              (list :cl-function 
                    (common-lisp-external-symbol-url item) 
                    (string-upcase (string item)))
            (list 
             :sys-function
             (function-source-url item)
             (string-upcase (string item)))))
         ((stringp item)
          (cond 
           ((initial-subsequence-of? item "http://")
            (list :hyperlink item (subseq item 7)))
           (t 
            (let ((p&s (string-split item #\:)))
              (cond
               ((= 2 (length p&s))
                (let ((pkg (string-upcase (first p&s)))
                      (sym (string-upcase (second p&s))))
                  (if (find-package pkg)
                      (list 
                       :sys-function 
                       (function-source-url (intern sym (find-package pkg)))
                       sym)
                    (list :other item) 
                    )))
               (t (list :other item))
               )))))
         ((listp item)
          (list :hyperlink (first item) (second item)))
         (t (error "Illegal entry in see-also list: ~A" item))
         )))

(defun emit-parameters (parameters flavor)
  (html 
    ((:table :cellspacing 10)
     (:tr (:td (:b "Name"))
	  ((:td :align :center) (:b "Parameter type"))
	  ((:td :align :center) (:b "Value type"))
	  (when (eql flavor :define-function)
	    (html 
	      ((:td :align :center) (:b "Mapped?"))
	      ((:td :align :center) (:b "Can convert from"))))
	  ((:td :align :center) (:b "Default value"))
	  (:td (:b "Description")))
     (loop for p in parameters do
	  (html-for-function-parameter p flavor)))))

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
         (html (:code (:princ-safe 
                       (formatn "~(~s~)" (default-value p)))))
       (html "&mdash;")))
    (:td (if (docstring p) 
             (html (:princ-safe (docstring p)))
           (html ((:span :style "color: red;") "UNDOCUMENTED")))))))

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
     (:code "(" (when name? (html (:princ-safe (first syntax))))) " "
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
	 (formatn "~(~{~a~^-or-~}~)" (rest type))
	 (formatn "~(~a~)" type)))

    (t 
     (formatn "~(~a~)" type))))

(defun esthetic-parameter-type (type)
  (typecase type
    (symbol
     (if (symbol= type t)  "Any" (string-downcase type)))
    (cons 
     (if (eql (car type) 'or)
	 (formatn "~(~{~#[~;~a~;~a or ~a~:;~@{~a~#[~;, or ~:;, ~]~}~]~}~)" (rest type))
	 (formatn "~(~a~)" type)))
    (t 
     (formatn "~(~a~)" type))))

(defun module-is-to-be-displayed? (module)
  (let ((modes (help::display-modes module)))
    (or (member :all modes :test 'symbol=)
        (null modes)
        (case (user-mode)
          (:bbl (intersection '(:bbl :biobike) modes :test 'symbol=))
          (:biolisp (intersection '(:lisp :biolisp) modes :test 'symbol=))
          (t nil)
          ))))

(defun html-for-help-modules ()
  (html
    (with-standard-weblistener-page-header ("Modules" :listener? nil)
      (:table
       (:tr (:td (:b "Name")) (:td (:b "Description")))
       (loop for module in (sort (modules) #'string< :key #'name) 
	  for name = (name module)
	  for url = (url-with-package-name-and-symbol-name 
                     'make-help-module-url name)
          when (module-is-to-be-displayed? module)
	  do
          (html (:tr 
                 (:td ((:a :href  url) 
                       (:princ-safe (formatn "~(~a~)" (name module)))))
                 (:td (:princ-safe (docstring module))))))))))

(defun html-for-help-module (package-symbol symbol-name)
  (with-slots (name docstring functions variables macros)
      (com.biobike.help::find-documentation 
       (intern symbol-name (find-package package-symbol)) 
       'com.biobike.help::module)
    (let ((title (formatn "Module ~a" name)))
      (html
	(with-standard-weblistener-page-header (title)
	  (html-for-help-links)
	  (:p docstring)
	  (when functions
	    (html (:h2 "Functions"))
	    (thing-table functions 'function-documentation))
	  (when variables
	    (html (:h2 "Variables"))
	    (thing-table variables 'variable-documentation))
	  (when macros
	    (html (:h2 "Macros"))
	    (thing-table macros 'macro-documentation)))))))

(defun thing-table (names type)
  (if names
      (html 
       (:table
        (:tr (:td (:b "Name")) (:td (:b "Description")))
        (loop for name in (sort (copy-seq names) #'string<)
              for url = (symbol-doc-url name type)
              for docs = (find-documentation name type) 
              for explicit-docs = (explicitly-documented-p docs)
              do
              ;; shut compiler up
              (progn explicit-docs)
              (html 
               (:tr 
                (:td ((:a :href url) (:princ-safe name)))
                (:td (:princ-safe (or (docstring docs) "Undocumented"))))))))
    (html (:p (:i "None")))))



#||

	Proposal for how to treat the examples slot of DOCUMENT-FUNCTION 


Right now the examples are just strings.  This has proved inadequate,
mostly because we want to have prettily-formatted multi-line input
forms.  It would also be potentially useful to be able to mark up the
examples with arbitrary html.

Therefore we propose the following extensions to the syntax for an
example (examples of these extensions are given below):

Any example which remains a string will get output just as it is now.
If an example is a list, we will treat it specially as decribed below.

An example list will have the form 

(form &optional values comment) 


The FORM can either be:

  -- a string, in which case it is output as if it were surrounded by
the html markers <pre> </pre>.  (Use #.(one-string-nl ...) to create a
readable single string from multiple single-line strings.)

  -- a list whose first element is :foo, in which case the rest of the
list is processed just as the :text field of DOCUMENT-FUNCTION is
processed, by running it through the 'foo' html generator.

  -- any other list, in which case the pretty printer is called
on the list, and that pretty-print result is then output literally.

Note that if FORM contains BBL constructs such as [ ]'s and { }'s the
pretty-printer won't be able to print them as such because the lisp
reader will have already processed them into lisp forms.  Examples
with such constructs therefore MUST NOT use this method; they must use
one of the first two methods.  


The VALUES object can either be: 

  -- the keywords :nil or :none, in which case the example is not shown
as having a result (presumably it is executed for side effect and the
comment will explain what happens).

  -- a string, in which case it is output literally, with html-special
characters escaped. 

  -- a list whose first element is :string, in which case the list
should have one more element, a string, which will get printed out
with the proper escapes with surrounding double-quote marks.
(We turn the string into a simple string by surrounding it internally 
with double quotes)

  -- a list whose first element is :values, in which case the rest of
the list is processed recursively (except that recursive :values, :nil
and :none are not allowed).

  -- any other lisp object, which will get turned into a string using
the same pretty-printer call used for forms above.


The COMMENT object must be a string if it exists, in which case it
gets printed out with the proper escapes.  If the comment does not
begin with the character #\; then ";; " is added to the string.

The FORM, VALUES and COMMENT fields get output in a visually pleasing
way and so that they do not exceed a certain margin, (70 characters --
this is because we don't know the width of the browser window and
don't want anything extending off the right edge of a reasonable
browser window.)  (The pretty-printer output will be controlled so as
to obey this margin using *print-right-margin*.)

We need to create the actual characters that will be printed out for
the VALUES and the COMMENT, and determine whether they will be
printed on separate lines or together.  COMMENT is as above.

If VALUES is :nil or :none, indicated now by :nil, we use "", which
will flag not to print it out.

If VALUES is a string, indicated now by (:string string) we leave it
as the string with "--> " prepended.

If VALUES is a form, indicated now by (:pprint form) we pretty print
it with the standard right margin less four characters.  If there are
no newlines, we prepend it with "--> ".  If there are newlines we
prepend with (s+ "--> " #\Newline).

If VALUES is (:values ...), we recurse, turning each value into
a string, but not prepending.  If any of the value strings has a
newline we create a string with each value on a separate line, and
the first line being "--> ".  Otherwise, we create a string with
the strings concatenated together, separated by ", ", and prepended
with "--> ".  If the string is wider than the margin we revert
to individual lines, otherwise we use that.

Then we combine the VALUES and COMMENT strings.  If either has
newlines they are combined with a newline between them.  If neither
has newlines then they are concatenated together.  If the resulting
string is wider than the margin we revert to individual lines,
otherwise wwe use the combination.

Finally we combine the FORM with the VALUES/COMMENT in a similar
manner.  



If FORM is to be run through FOO processing, it will always
be on separate lines than the VALUE/COMMENT

If FORM is a string, if V/C has newlines they get printed separated
by a newline, otherwise we combine then; if the combined string exceeds
the margin we revert to separate lines, otherwise it is just one string.

If FORM is a list to be pretty-printed, we turn it into a string
in the same way as for VALUES forms.  If either the FORM string or
V/C has newlines



Between the FORM and VALUES an arrow will be output.  Between each VALUE
a comma will be placed.  The result values may appear on a separate line
and the comment may appear on a separate line, depending on the overall
width of the FORM, VALUES, and COMMENT.  If the FORM is a multiline form
(or is done using :foo-processing) a separate line will always be used 
for the VALUES.   


EXAMPLES:

  "(bar 1) --> 2 ;; adds 1"

;; This is a standard example string.  Since it's a string no special
;; parsing or handling is done.  The output is as you see it without the
;; double quotes.  (Any characters special to HTML will have been
;; escaped.)


  ((:foo "(multiple-value-bind (x y) '(3 4) " (:br) "  (+ x y 1))")
   8 "An example of MULTIPLE-VALUE-BIND")

produces: 

  (multiple-value-bind (x y)
    (+ x y 1))
  --> 8 ;; An example of MULTIPLE-VALUE-BIND

;; This is an example of simple :foo processing, using (:br) to produce
;; a newline.  


  (#.(one-string-nl 
	"(let ((x 3)"
        "      (y 4))"
        "  (values (+ x y) (- x y))))")
   (:values 7 -1)
   "The sum and difference")

produces:

  (let ((x 3)
        (y 4))
    (values (+ x y) (- x y)))
  --> 7, -1 ;; The sum and the difference

This is an example of processing the FORM as a single-string, and
multiple result values.
	
         
  ((defun a-long-function-name (argument1 argument2 argument3)
      (* argument1 argument2 argument3))
   A-LONG-FUNCTION-NAME
   "This illustrates how to define a function with three arguments.")

produces 

  (defun a-long-function-name (argument1 argument2 argument3)
    (* argument1 argument2 argument3))
  --> A-LONG-FUNCTION-NAME
  ;; This illustrates how to define a function with three arguments.

This illustrates using a lisp expression as FORM, which gets processed
by the pretty-printer.  The comment gets printed out on a separate
line because the return value and the comment won't fit on a single line.
The return value gets printed out on a separate line because the FORM
is multi-lined when it comes out of the pretty-printer.  

	      
  ((one-string "ab" "bc") (:string "abbc"))

produces 

  (one-string "ab" "bc") --> "abbc"

This illustrates using :string to insure that the result string is
shown quoted.  It also illustrates no comment field and that the
formatter will print the form and the results on a single line if they
will fit.

  
  ("(blow-up-world)" :nil "Causes world to end.")

This will output:

  (blow-up-world) ;; Causes world to end.

illustrating the use of :nil to indicate that no 
result need be shown; all that is important is the side effect, 
which should be stated in the comment.  


||#

(defparameter *standard-right-margin* 70)

(defparameter *arrow* "--> ")

(defun create-html-from-raw-example (raw-example)
  #.(one-string-nl
     "This is used to both parse an example and generate html for the example"
     "which is sent to the browser.")
  (multiple-value-bind (foo-forms string) (create-processed-example raw-example)
    (if (not foo-forms) 
        (html (:pre (:code (:princ-safe string))))
      (progn 
        (mapcar 'com.gigamonkeys.foo:emit-html foo-forms)
        (html :br (:pre (:code (:princ-safe string))))
        ))))

(defun create-processed-example (raw-example)
  #.(one-string-nl
     "This is used to parse an example and combine the parts into"
     "either a single string or a form to be foo-processed and an additional"
     "string.")     
  (multiple-value-bind (form values comment)
      (help:parse-document-function-example raw-example)
    (combine-form-and-v/c-strings 
     form 
     (combine-values-and-comment-strings 
      (turn-values-into-string values)
      comment
      ))))
    
(defun turn-values-into-string (values &optional (arrow *arrow*))
  (destructuring-bind (type data) values 
    (ecase type
      (:nil (values "" nil))
      (:string 
       (if (contains-newline? data)
           (values (s+ arrow #\newline data) t)
         (values (s+ arrow data) nil)))
      (:pretty-print 
       (let ((s (example-form-to-string data (- *standard-right-margin* 4))))
         (if (contains-newline? s) 
             (values (s+ arrow #\newline s) t)
           (values (s+ arrow s) nil)
           )))
      (:values 
       (let ((strings 
              (mapcar (lambda (x) (turn-values-into-string x "")) data)))
         (if (some 'contains-newline? strings) 
             (string-join (cons arrow strings) #\newline)
           (let ((single-string (s+ arrow (string-join strings ", "))))
             (if (> (length single-string) *standard-right-margin*)
                 (string-join (cons arrow strings) #\newline)
               single-string
               ))))))))

(defun combine-values-and-comment-strings (vs cs)
  (if (some 'contains-newline? (list vs cs))
      (s+ vs #\newline cs)
    (combine-maybe-with-newline vs cs *standard-right-margin*)))



(defun combine-form-and-v/c-strings (form vcs)
  (if (eq (first form) :foo) 
      (values (rest form) vcs)
    (destructuring-bind (type data) form
      (ecase type
        (:string 
         (values 
          nil
          (if (some 'contains-newline? (list data vcs))
              (s+ data #\newline vcs)
            (combine-maybe-with-newline data vcs *standard-right-margin*)
            )))
        (:pretty-print 
         (let ((s (example-form-to-string data *standard-right-margin*)))
           (combine-form-and-v/c-strings `(:string ,s) vcs)
           ))
        ))))


(defun contains-newline? (s) (find #\newline s))  
            
(defun combine-maybe-with-newline (s1 s2 limit)
  (if (> (+ (length s1) 1 (length s2)) limit)
      (s+ s1 #\newline s2)
    (s+ s1 #\space s2)))

(defun example-form-to-string (form limit)
  (let ((*print-pretty* t) 
        (*print-right-margin* limit)
        (*print-case* :downcase)
        (*package* *examples-package*))
    (format nil "~S" form)
    ))


 
  

