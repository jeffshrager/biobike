;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

;;; Authors:  Jeff Elhai, JP Massar.

(in-package :bio)

;;; +============================================================================+
;;; | Copyright (c) 2001, 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers |
;;; |                                                                            |
;;; | Permission is hereby granted, free of charge, to any person obtaining      |
;;; | a copy of this software and associated documentation files (the            |
;;; | "Software"), to deal in the Software without restriction, including        |
;;; | without limitation the rights to use, copy, modify, merge, publish,        |
;;; | distribute, sublicense, and/or sell copies of the Software, and to         |
;;; | permit persons to whom the Software is furnished to do so, subject to      |
;;; | the following conditions:                                                  |
;;; |                                                                            |
;;; | The above copyright notice and this permission notice shall be included    |
;;; | in all copies or substantial portions of the Software.                     |
;;; |                                                                            |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,            |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF         |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.     |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY       |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,       |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE          |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                     |
;;; +============================================================================+


#|

8. Analyze a moving window
through a sequence The physical properties of DNA are determined by
its nucleotide composition. For example, the greater the content of
A's and T's, the lower the local melting temperature. Such regions are
often associated with regulatory function. You want to go through the
genome or smaller segments of DNA and determine where AT-rich regions
lie. The following function moves a window over a sequence
(continuously or by jumps, according to the value of window-increment)
and determines the AT-content and outputs it as a 

(DEFUN AT-content
(sequence window-size &OPTIONAL (window-increment NIL))
   (IF-DOES-NOT-EXIST window-increment
                 THEN (ASSIGN window-increment window-size))
   (ASSIGN window-number 0)
   (ASSIGN AT-fraction NIL)
   (LOOP FOR pos FROM 0 BELOW (LENGTH sequence) BY window-increment
         AS window-number FROM 1
         DO (ASSIGN window 
                 (EXTRACT-STRING sequence FROM pos LENGTH window-size))
            (ASSIGN AT-content (+ (COUNT #\A window) (COUNT #\T window)))
            (ASSIGN AT-fraction (/ AT-content window-size))
            (ASSIGN-ARRAY AT-fraction (window-number) 
                 (LIST pos AT-fraction)))
   (ARRAY-TO-STRING AT-fraction (*return* *tab*))) 

array-to-string (array &OPTIONAL separators): Converts values in an
array to a single string, which can be used for input to another
program (e.g. Excel).  Given separators are inserted between elements
the first separating elements of the first dimension, the second of
the second dimension, and so forth. Good default values for
separators: *return* *tab* 

|#

(defun at-content 
       (sequence window-size &optional (window-increment window-size))
  (let ((len (length sequence)))
    (2d-object-to-tab-newline-delimited-string
     (loop for pos from 0 below len by window-increment
           as window = (subseq sequence pos (min (+ pos window-size) len))
           as at-content = (+ (charcount-case-insensitive #\A window)
                              (charcount-case-insensitive #\T window))
           as at-fraction = (float (/ at-content (length window)))
           collect (list pos at-fraction)
           ))))

(defun charcount-case-insensitive (char string)
  (count char string :test 'char-equal))          


(defun 2d-object-to-tab-newline-delimited-string 
       (obj &key (column-sep #\Tab) (row-sep #\Newline) (element-format "~S"))
  #.(one-string-nl
     "Creates a string from a 2-dimensional object OBJ.  OBJ can be a "
     "two-dimensional array, or a sequence of sequences (i.e., a vector of "
     "lists, a list of vectors, a list of lists, or a vector of vectors)."
     "Each element within the secondary sequence is turned into its string"
     "representation and separated by COLUMN-SEP (default TAB) from the other"
     "elements in the same secondary sequence.  All of these COLUMN-SEP "
     "separated strings are joined together into a single string, separated"
     "by ROW-SEP.  The effect is, by default, to produce a string which could"
     "be written out to a file in a standard tab-delimited format that EXCEL"
     "might be able to read. (i.e., columns separated by TAB, rows separated"
     "by Newlines")
  (flet ((format-element (e) (format nil element-format e))
         (sequencep (x) (typep x 'sequence)))
    (cond
     ((and (arrayp obj) (= 2 (array-rank obj)))
      (string-join
       (loop for j fixnum below (array-dimension obj 0) collect
             (string-join
              (loop for i fixnum below (array-dimension obj 1) collect
                    (format-element (aref obj j i)))
              column-sep))
       row-sep))
     ((and (arrayp obj) (> (array-rank obj) 2))
      (error "Array has more than 2 dimensions!"))
     ((not (sequencep obj))
      (error "Invalid 2d object argument: ~A" obj))
     (t
      (unless (every #'sequencep obj)
        (error "Every element of ~A is not a sequence!" obj))
      (string-join
       (map 
        'list
        (lambda (sub-object)
          (string-join
           (map 'list (lambda (elem) (format-element elem)) sub-object)
           column-sep))
        obj)
       row-sep
       )))))




(defun array-to-string-aux (array dims rank fixed-indices axis separators)
  (let* ((extended-indices (nreverse (cons 0 (reverse fixed-indices))))
         (last-cons-cell (last extended-indices))
        (last-axis? (= axis (1- rank))))
    (string-join
     (loop for j from 0 below (nth axis dims) collect
           (progn
             (setf (first last-cons-cell) j)
             (if last-axis?
                 (formatn "~S" (apply 'aref array extended-indices))
               (array-to-string-aux 
                array dims rank extended-indices (1+ axis) separators
                ))))
     (nth axis separators)
     )))

(defun array-to-string (array separators &aux (seps separators))

  ;; Canonicalize the SEPARATORS argument into a list of
  ;; separators, one for each array axis.

  (when (stringp seps) (setq seps (list seps)))
  (cond
   ((not (listp seps)) (error "Invalid separators argument: ~A" separators))
   ((= 1 (length seps))
    (setq seps (make-list (array-rank array) :initial-element (first seps))))
   ((/= (length seps) (array-rank array))
    (error "Must specify a separator for each axis of the array")))
  
  (array-to-string-aux 
   array
   (array-dimensions array)
   (array-rank array)
   nil 0 seps
   ))
