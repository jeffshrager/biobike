;;; -*- Package: bioutils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bioutils)

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

;;; Warning:  this code assumes that FILE-POSITION increments by 1
;;; except possibly on a #\Newline, everywhere in a FASTA file.
;;; If this doesn't hold these algorithms won't work!

(defconstant newline-file-position-increment 
  (block exit
    #+(AND :LISPWORKS :WIN32) 
    (return-from exit 2)
    #+(AND :ALLEGRO :MSWINDOWS)
    (return-from exit 2)
    #+(AND :ALLEGRO :UNIX)
    (return-from exit 1)
    #+(AND :SBCL :UNIX)
    (return-from exit 1)
    (error "Fell through Lisp vendor / OS case for newline increment")))

(defvar *fasta-file* "No file specified")
(defvar *fasta-files* nil)
(defvar *fasta-index* nil)
(defvar *fasta-data* nil)
(defvar *fasta-file-linenum* 0)
(defvar *fdbg* nil)
(defvar *fasta-verbose* nil "Detail execution of fasta code")

;;; The FASTA INDEX data structure and the structure of its components.

;;; A fasta index is comprised of a source file and multiple
;;; fasta index datum records, one for each record in the fasta file.
(defmacro make-fasta-index (filename data) `(cons ,filename ,data))
(defmacro fasta-index-source-file (index) `(first ,index))
(defmacro fasta-index-data (index) `(rest ,index))

;;; A fasta index datum is comprised of the KEY for a given record
;;; and the information extracted for that record.

(defmacro make-fasta-index-datum (key recordinfo) `(cons ,key ,recordinfo))
;; Make them functions because we use them as function objects later.
(defun fasta-index-datum-key (x) (car x))
(defun fasta-index-datum-recordinfo (x) (cdr x))

;;; For each record in a fasta file the following information is
;;; extracted and stored:

;;; The start file position of the record (the position of the '>')
;;; The end file position of the record (the position of the next '>' 
;;;   or the EOF position (which is an integer)
;;; The file position of the start of the data for this record
;;;   (i.e., the file position of the start of the next line after a '>')
;;; The line length of each data line in the record assuming all the
;;;   lines have equal length, or NIL if they do not.
;;; The number of data lines in the record.
;;; The length of the data -- the sum of the lengths of each data line
;;;   in the record, not including the #\Newline at the end of each line.
;;;   This does not include the header.
;;; The line length of each data line in the record as a vector, one
;;;   entry per line, assuming the data lines are all not equal in
;;;   length.  If they are equal, this value is NIL.

;;; Another field, FILE-ID, is part of the data record but is given a
;;; value elsewhere.  It is used when multiple files comprise a fasta
;;; database.

(defmacro fri-startfp (x) `(aref ,x 0))
(defmacro fri-endfp (x) `(aref ,x 1))
(defmacro fri-data-startfp (x) `(aref,x 2))
(defmacro fri-data-linelen (x) `(aref ,x 3))
(defmacro fri-data-nlines (x) `(aref ,x 4))
(defmacro fri-sequence-length (x) `(aref ,x 5))
(defmacro fri-file-id (x) `(aref ,x 6))
(defmacro fri-data-line-sizes (x) `(aref ,x 7))


(defmacro fasta-index-datum-startpos (datum) 
  `(fri-startfp (fasta-index-datum-recordinfo ,datum)))
(defmacro fasta-index-datum-endpos (datum) 
  `(fri-endfp (fasta-index-datum-recordinfo ,datum)))

;;; Create a Fasta-Record-Information data structure.

(defun make-fri 
       (&key
        startfp endfp data-startfp 
        data-linelen nlines seqlen 
        file-id line-sizes
        )
  (vector 
   startfp endfp data-startfp data-linelen 
   nlines seqlen file-id line-sizes
   ))

