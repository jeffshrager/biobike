;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi)

;;; +=========================================================================+
;;; | Copyright (c) 2007 Mark Slupesky, JP Massar                             | 
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

;; Author: Mark Slupesky, JP Massar.
;; BBL/CLPPCRE

;; more tests via mechanism

;; A dynamic variable for holding class bindings
(defparameter *class-bindings* nil)
(defparameter *pattern-language* nil)
(defparameter *special-characters*
  '(($ "\\w") (|#| "\\d")))

(defparameter *special-end-char* (code-char 1))

;; Toplevel function.  Takes a list containing the class bindings
;; and body, modifies *class-bindings* appropriately, and converts the
;; body to the correct output string
(defun mp-output-string (class-and-body-list &key (language :bbl))
  (unless (and (= (length class-and-body-list) 2)
             (listp (first class-and-body-list))
             (listp (second class-and-body-list)))
    (error 
     (one-string-nl
      "MP-OUTPUT-STRING takes a 2-element list:"
      "A list of class bindings and a pattern list"
      )))
  (let ((class-bindings 
         (loop for (var val) in (first class-and-body-list)
           collect
           (list var (string-upcase val))
           ))
        (body 
         (second class-and-body-list)))
    ;; push each binding onto the list
    (let ((*class-bindings* class-bindings)
          (*pattern-language* language))
      (special-character-replace (bigpatmatch body))
      )))

(defun special-character-replace (string)
  (let ((*class-bindings* *special-characters*))
    (let ((result (class-replace string)))
      (ecase *pattern-language* 
        (:bbl result)
        (:cl-ppcre 
         (substitute #\$ *special-end-char* result))
        ))))

(defun bigpatmatch (list-of-verb-first-lists)
  ;; holder for our final output string
  (let ((answer ""))
    ;; loop over the lists that make up the body in reverse order, 
    ;; building up ANSWER with each individual matched pattern
    (loop for vfl in (reverse list-of-verb-first-lists)
      as matched-pattern = (patmatch vfl)
      do 
      (setq answer (s+ matched-pattern answer)))
    answer
    ))

;; Takes a list whose first element is a verb, and 
;; returns the correct string.
(defun patmatch (verb-first-list) 
  (let ((verb (first verb-first-list))
        (objects (rest verb-first-list)))
    (cond
     ((symbol= verb 'pattern)
      (when (/= (length objects) 1) 
        (error "PATTERN can only take 1 argument!"))
      ;; VERB-FIRST-LIST = (pattern string-or-var)
      ;; OBJECTS contains either a literal-string or a variable
      (let ((string-or-var (first objects)))
        (etypecase string-or-var
          (string (class-replace string-or-var))
          (symbol (concatenate 'string "<" string-or-var ">"))
          )))

     ;; VERB-FIRST-LIST = (start)
     ((symbol= verb 'start)
      (when objects (error "START takes no arguments!"))
      (ecase *pattern-language* (:bbl "<START>") (:cl-ppcre "^")))

     ;; VERB-FIRST-LIST = (start)
     ((symbol= verb 'end) 
      (when objects (error "END takes no arguments!"))
      (ecase *pattern-language* 
        (:bbl"<END>") 
        (:cl-ppcre (string *special-end-char*))
        ))

     ;; VERB-FIRST-LIST = (repeat directive pattern-specification)
     ((symbol= verb 'repeat)
      (unless (or (= (length objects) 2) (= (length objects) 3))
        (error "REPEAT takes two or three arguments!"))
      (s+ 
       (cond 
        ((or (symbol= (third objects) 'minimize )
             (symbol= (third objects) 'min))
         "?")
        ((or (symbol= (third objects) 'maximize)
             (symbol= (third objects) 'max))
         "")
        ((third objects) 
         (error
          "REPEAT's third argument must be MAX, MAXIMIZE, MIN, or MINIMIZE"))
        (t "")
        )
       ;; PATTERN-SPECIFICATION
       (patmatch (lastelem verb-first-list))
       ;; DIRECTIVE
       (cond
        ;; OBJECTS = (n times)
        ((numberp (first objects))
         (concatenate 'string "{" (first objects) "}"))
        ;; OBJECTS = (indefinitely) 
        ((symbol= (first objects) 'indefinitely) "*")
        ;; OBJECTS = (from 0 to n) or (from m to n)
        ((symbol= (third objects) 'to)
         (concatenate 
          'string "{" (second objects) "," (fourth objects) "}"))
        ;; OBJECTS = (from 1 to-any)
        ((eql (third objects) 'to-any) "+"))
       ))

     ;; VERB-FIRST-LIST = (gap directive)
     ((symbol= verb 'gap)
      (when (/= (length objects) 1)
        (error "SYMBOL takes 1 argument!"))
      (cond
       ;; OBJECTS = (N times)
       ((numberp (first objects))
        (concatenate 'string ".{" (first objects) "}"))
       ;; OBJECTS = (indefinitely) 
       ((symbol= (first objects) 'indefinitely) ".*")
       ;; OBJECTS = (from 0 to n) or (from m to n)
       ((symbol= (third objects) 'to)
        (concatenate 
         'string ".{" (second objects) "," (fourth objects) "}"))
       ;; OBJECTS = from 1 to-any
       ((symbol= (third objects) 'to-any) ".+")
       ))

     ;; VERB-FIRST-LIST = (either patspec or patspec or patspec ...)
     ((symbol= verb 'either)
      ;; OBJECTS = (patspec or patspec or patspec ...)
      (let ((pattern-specifications (reverse objects))
            (final-string ""))
        (loop for patspec in (cdr pattern-specifications)
          for x from 1
          when (oddp x)
          do
          (setq final-string (s+ final-string "|" (patmatch patspec)))
          finally 
          (progn
            (setq 
             final-string 
             (s+ final-string (patmatch (car pattern-specifications)))
             )
            (return final-string))
          )))

     ;; VERB-FIRST-LIST = (capture pattern-specification)
     ((symbol= verb 'capture)
      (when (/= (length objects) 1) 
        (error "CAPTURE takes 1 argument!"))
      (let ((left "<<") (right ">>"))
        (ecase *pattern-language*
          (:bbl nil)
          (:cl-ppcre (setq left "(") (setq right ")")))
        (let ((pattern-specification (first objects)))
          (concatenate 'string left (patmatch pattern-specification) right)
          )))

     ;; VERB-FIRST-LIST = (maybe pattern-specification)
     ((symbol= verb 'maybe) 
      (when (/= (length objects) 1) 
        (error "MAYBE takes 1 argument!"))
      (s+ (patmatch (first objects)) "?"))

     ;; VERB-FIRST-LIST = (use-capture)
     ((symbol= verb 'use-capture)
      (error "Oops, USE-CAPTURE isn't implemented yet!"))
      
     ;; VERB-FIRST-LIST = (act-on-capture)
     ((symbol= verb 'act-on-capture)
      (error "Oops, ACT-ON-CAPTURE isn't implemented yet!"))

     ;; VERB-FIRST-LIST = (not pattern-specification)
     ((symbol= verb 'not)
      (when (/= (length objects) 1) 
        (error "NOT takes 1 argument!"))
      (let ((pattern-specification (first objects)))        
        (concatenate 
         'string 
         "["
         (ecase *pattern-language* (:bbl "~") (:cl-ppcre "^"))
         (patmatch pattern-specification)
         "]")
        ))

     ;; VERB-FIRST-LIST = (class class-specification)
     ((symbol= verb 'class)
      (error "Oops, CLASS isn't implemented yet!"))
     )))

(defun class-replace (string)
  (let ((varlist nil))
    (loop for (x y) in *class-bindings*
          do 
          (if (member x varlist)
              (error 
               "Each class name must be unique, ~S has duplicate definitions.")
            (push x varlist))
          (setq string (class-replace-aux x y string))
          )
    string))

(defun class-replace-aux (var val target-string)
  ;; check that var is really 1 character long
  (etypecase var
    (symbol (setq var (string-upcase var)))
    (string var)
    (character (string var)))
  (let* ((char (coerce var 'character))
         (split-string (reverse (string-split target-string char)))
         (answer ""))
    (loop for string in (cdr split-string)
          do 
          (setq answer (s+ string val answer))
          finally (setq answer (s+ answer (first split-string)))
          )
    answer
    ))

(defun simple-pattern-match (classes elhai-pattern string)
  (let ((string-pattern 
         (mp-output-string (list classes elhai-pattern) :language :cl-ppcre)))
    (let ((string-positions (cl-ppcre:all-matches string-pattern string)))
      (when string-positions 
        (setq string-positions (subseq string-positions 0 2))
        (values 
         (subseq string (first string-positions) (second string-positions))
         string-positions
         )))))
    
