;;; -*- Package: cl-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :cl-user)

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

;; Create the name of a log file given its extension
;; and the universal time the name should represent.

;; The log file name is in the form:

;; <completion-tag>-YYYYMMDD-HHMMSS.<extension>

;; where <completion-tag> is the result of calling 
;; ENCODE-TIME-TO-FOUR-CHARS (see below).

;; The point of this is to create a logfile name that
;; -- is easily readable as to the date it represents
;; -- is naturally sortable with respect to all other such logfiles
;;    with respect to time.
;; -- is easy to edit via Emacs, because file completion will get
;;    you the file you want after typing four characters in most cases.

(defun weblistener-logfile-name 
       (suffix extension &optional (time (get-universal-time)))
  "Creates a filename of the form cccc-yyyymmdd-hhmmss.xxx where xxx is
   EXTENSION and cccc is a special encoding designed to make the filename
   almost unique and easily 'completable'."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time time)
    (format 
     nil
     "~A-~4D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D~A.~A"
     (encode-time-to-four-chars time)
     year month day hour minute second 
     (if suffix (concatenate 'string "-" suffix) "")
     extension
     )))

;; Turn a universal time into a string of four lowercase alphabetic
;; characters, such that if TIME1 occurs before TIME2, and TIME1 and
;; TIME2 both occur in the same year, or in years that differ by at
;; most 1, and E1 and E2 are the respective encodings, then
;; (string<= E1 E2)
;; is always true.

;; We achieve this data compression by 
;; -- reducing the year to a single bit
;; -- reducing minutes from 0-59 down to the range 0-11

;; This way there are 2 x 12 x 31 x 24 x 12 = 214272
;; possible reduced universal times, which is less than
;; 26 x 26 x 26 x 26 = 456976, the number of possible four-character
;; lowercase strings.


(defun encode-time-to-four-chars (&optional (time (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time time)
    (declare (ignore second))
    (flet ((to-a-through-z (i) (code-char (+ i (char-code #\a)))))
      (let* ((y (mod year 2))
             (min (floor minute 5))
             (encoding 
              (+ min
                 (* hour 60)
                 (* (1- day) (* 24 60))
                 (* (1- month) (* 31 24 60))
                 (* y (* 12 31 24 60))
                 ))
             (char1 (to-a-through-z (mod encoding 26)))
             (char2 (to-a-through-z (mod (floor encoding 26) 26)))
             (char3 (to-a-through-z (mod (floor encoding (* 26 26)) 26))) 
             (char4 (to-a-through-z (mod (floor encoding (* 26 26 26)) 26)))
             )
        (format nil "~A~A~A~A" char4 char3 char2 char1)
        ))))

;; This allows this file to operate as an independent program on Unix.
;; It prints the name of the newly minted log file to standard output.

#+:ALLEGRO
(unless (find-package :weblistener)
  (sys:with-command-line-arguments 
      (("e" :short extension :required-companion))
    (restvar)
    (declare (ignore restvar))
    (format t "~%~A~%" (weblistener-logfile-name "" extension))
    (force-output t)
    ))

