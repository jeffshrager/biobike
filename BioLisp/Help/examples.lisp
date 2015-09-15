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


;; make sure each example parses without error.  
(defun parse-document-function-examples (examples)
  (loop for example in examples 
        do (parse-document-function-example example)
        collect example
        ))

(defun parse-document-function-example (example &aux form values comment)

  (cond 

   ((stringp example)
    
    (values `(:string ,example) `(:nil :nil) ""))

   ((and (consp example) (eq (first example) :foo))
    
    example)

   (t 

    ;; parse the example into form, values, and comment 

    (progn
  
      (cond
       ((listp example) 
        (when (null example) 
          (error "An example cannot be NIL!"))
        (setq form (first example))
        (when (null (cdr example))
          (error "An example must have return values or a comment!"))
        (setq values (second example))
        (if (null (cddr example))
            (setq comment nil)
          (setq comment (third example)))
        (when (> (length example) 3) 
          (error
           #.(concatenate 
              'string
              "An example has at most 3 components, a form, return-values, and"
              "a comment.  The example ~S"
              "has ~D components.")
           example (length example)))
        )
       (t
        (error
         #.(concatenate 
            'string
            "Unknown example syntax: ~S"
            "An example must either be a string or a list.")
         example)))

      ;; check the form 

      (cond
       ((stringp form) (setq form `(:string ,form)))
       ((not (listp form))
        (error
         #.(concatenate
            'string
            "Invalid example syntax.  The form (the first component)"
            "must either be a string or a list.")))
       ((eq (first form) :foo) nil)
       ((listp form) (setq form `(:pretty-print ,form)))
       )

      ;; check the values

      (flet
          ((parse-single-value (value)
             (cond
              ((stringp value) `(:string ,value))
              ((not (listp value))
               `(:string ,(format nil "~S" value)))
              (t 
               (cond
                ((eq (first value) :string) 
                 (unless (= (length value) 2)
                   (error
                    #.(concatenate 
                       'string
                       "Invalid values syntax in example."
                       "A values form beginning with :string must have"
                       "exactly one more component, but ~S"
                       "has ~D components.")
                    value (length value)))
                 (unless (stringp (second value))
                   (error
                    #.(concatenate 
                       'string
                       "Invalid values syntax in example."
                       "A values form beginning with :string must have"
                       "exactly one more component which must be a string.")))
                 `(:string ,(format nil "~S" (second value))))
                (t
                 `(:pretty-print ,value)))))))
        (setq 
         values 
         (cond
          ((or (eq values :nil) (eq values :none)) `(:nil ,values))
          ((and (listp values) (eq (first values) :values))
           `(:values ,(mapcar #'parse-single-value (rest values))))
          (t (parse-single-value values))
          )))
      
      ;; check the comment 

      (cond
       ((null comment) (setq comment ""))
       ((not (stringp comment)) 
        (error 
         #.(concatenate 
            'string
            "Invalid comment syntax in example."
            "A comment must either be a string, NIL, or not present at all.")))
       ((and (plusp (length comment)) (eql #\; (char comment 0))) 
        nil)
       (t (setq comment (concatenate 'string ";; " comment)))
       )
      
      (values form values comment)
      
      ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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

(defparameter *examples-package* nil)

(defparameter *standard-right-margin* 70)

(defparameter *arrow* "--> ")

(defun create-html-from-raw-example (raw-example)
  #.(one-string-nl
     "This is used to both parse an example and generate html for the example"
     "which is sent to the browser.")
  (if (and (consp raw-example) (eq (first raw-example) :foo))
      (eval `(com.gigamonkeys.foo:html ,@(cdr raw-example)))
    (multiple-value-bind (foo-forms string) (create-processed-example raw-example)
      (if (not foo-forms) 
          (html (:pre "&nbsp;&nbsp;" (:b "> ") (:code (:princ-safe string))))
        (progn 
          (html (:code "&nbsp;&nbsp;" (:b "> ")))
          (mapcar 'com.gigamonkeys.foo:emit-html foo-forms)
          (html :br (:pre "&nbsp;&nbsp;" (:code (:princ-safe string))))
          )))))

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


(defun combine-maybe-with-newline (s1 s2 limit)
  (if (> (+ (length s1) 1 (length s2)) limit)
      (s+ s1 #\newline s2)
    (s+ s1 #\space s2)))

(defun example-form-to-string (form limit)
  (let ((*print-pretty* t) 
        (*print-right-margin* limit)
        (*print-case* :downcase)
        (*package* (or *examples-package* *package*)))
    (format nil "~S" form)
    ))


 
  

