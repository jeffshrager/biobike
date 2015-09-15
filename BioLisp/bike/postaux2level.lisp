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

;;; Author: JP Massar, Jeff Elhai, Arnaud Taton. 

(defun read-fasta-file-aux (path shared)
  (let ((processed-path
         (IF shared
             (utils::S+ "../shared-files/" path)
           path)))
    (WITH-OPEN-FILE (stream processed-path :DIRECTION :input)
      (LET ((labeled-sequences
             (LOOP FOR line = (READ-LINE stream NIL NIL) 
               UNTIL (NULL line)
               WITH list = NIL
               WITH first-time = T
               WITH seq-parts = NIL
               WITH header = NIL
               AS sequence = NIL
               AS labeled-sequence = NIL
               AS 1st-char = (AND (> (LENGTH line) 0)
                                  (SUBSEQ line 0 1))
               DO (COND
                   ((EQUAL 1st-char ">")
                    (IF-TRUE (NOT first-time)
                             THEN (SETF sequence 
                                        (APPLY 'S+ (REVERSE seq-parts)))
                             (SETF seq-parts NIL)
                             ELSE (SETF first-time NIL)
                             (SETF header (TRIM (SUBSEQ line 1)))))
                   (1st-char (PUSH (STRING-UPCASE line) seq-parts)))
               WHEN sequence
               DO (SETF labeled-sequence
                        (MAKE-LABELED-SEQUENCE 
                         :LABEL header :SEQUENCE sequence))
               (PUSH labeled-sequence list)
               (SETF header (TRIM (SUBSEQ line 1)))
               FINALLY 
               (PROGN
                 (SETF sequence (APPLY 'S+ (REVERSE seq-parts)))
                 (SETF labeled-sequence 
                       (MAKE-LABELED-SEQUENCE 
                        :LABEL header :SEQUENCE sequence))
                 (RETURN (PUSH labeled-sequence list))))))

        (IF-TRUE (> (LENGTH labeled-sequences) 1)
                 THEN (REVERSE labeled-sequences)
                 ELSE (biolisp::FIRST labeled-sequences))))))

