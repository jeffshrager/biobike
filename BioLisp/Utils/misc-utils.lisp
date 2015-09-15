;;; -*- Package: utils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

 (in-package :utils)

;;; +=========================================================================+
;;; | Copyright (c) 2002-3005 JP Massar, Jeff Shrager, Mike Travers           |
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

;;; Authors:  JP Massar, Jeff Shrager

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defparameter *utility-misc-user-symbols* 
    '(
      make-timestamp-string 
      randomize-vector
      ))

  (defparameter *utility-misc-api-symbols*
    (append
     *utility-misc-user-symbols*
    '(
      read-until
      new-string
      weekday-index-to-weekday-name
      month-index-to-month-name
      past-time-description
      create-defpackage-form
      printed-type-of
      type-to-english-string
      write-table-rows
      create-index-and-data-file
      with-room-total-bytes-displayed
      )))

  (export *utility-misc-api-symbols* (find-package :utils)))

;;; ================================================================

(defun read-until 
    (stream end-char-or-pred &optional (string (new-string)) untyi-end?)
  #.(one-string-nl
     "Read characters from STREAM and return a string consisting of those"
     "characters.  If END-CHAR-OR-PRED is a character, reading stops when"
     "that character is found.  If END-CHAR-OR-PRED is a function, reading"
     "stops if the character read satisfies the function.  The character"
     "that causes reading to stop is 'unread' if UNTYI-END? is TRUE."
     "Reading terminates without error at an END OF FILE.")
  (do ((char (read-char stream nil :eof) (read-char stream nil :eof)))
      ((or (eq char :eof)
         (if (characterp end-char-or-pred)
             (char= char end-char-or-pred)
           (funcall end-char-or-pred char)))
       (when (and untyi-end? (not (eq char :eof)))
         (unread-char char stream))
       (values (coerce string 'simple-string) char))
    (when string 
      (vector-push-extend char string))))

(defun new-string (&optional (initial-length 10))
  "Create an adjustable string INITIAL-LENGTH long with a FILL-POINTER=0"
  (make-array initial-length :element-type 'character 
              :adjustable t :fill-pointer 0)) 

(defun make-timestamp-string 
       (&key (universal-time (get-universal-time)) (mode :mmddyyhhmmss))
  #.(one-string-nl
     "Return a timestamp based on MODE which looks like: "
     ":MMDDYYHHMMSS \"mm/dd/yy hh:mm:ss\""
     ":MMDDYY \"mm/dd/yy\""
     ":YYMMDDHHMMSS \"yy/mm/dd hh:mm:ss\""
     ":DDMMYYHHMMSS \"dd/mm/yy hh:mm:ss\""
     ":MMDDYYHHMM \"mm/dd/yy hh:mm\""
     ":HHMMSS \"hh:mm:ss\""
     ":STDFULL \"yyyymmddhhmmss\""
     ":STDYMD \"yyyymmdd\""
     ":UNIX-TIMESTAMP \"yyyy-mm-dd hh:mm:ss\""
     ":SQL-DATESTAMP \"yyyy-mm-dd\""
     )
  (multiple-value-bind (s min h d mth y)
      (decode-universal-time universal-time)
    (ecase mode
      (:mmddyyhhmmss
       (format nil "~2,'0D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D"
               mth d (mod y 100) h min s
               ))
      (:mmddyy 
       (format nil "~2,'0D/~2,'0D/~2,'0D"
               mth d (mod y 100)
               ))
      (:yymmddhhmmss
       (format nil "~2,'0D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D"
               (mod y 100) mth d h min s
               ))
      (:ddmmyyhhmmss
       (format nil "~2,'0D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D"
               d mth (mod y 100) h min s
               ))
      (:mmddyyhhmm
       (format nil "~2,'0D/~2,'0D/~2,'0D ~2,'0D:~2,'0D"
               mth d (mod y 100) h min
               ))
      (:hhmmss 
       (format nil "~2,'0D:~2,'0D:~2,'0D" h min s))
      (:stdfull
       (format nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
               y mth d h min s))
      (:stdymd 
       (format nil "~4,'0D~2,'0D~2,'0D" y mth d))
      (:unix-timestamp 
       (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
               y mth d h min s))
      (:sql-datestamp
       (format nil "~4,'0D-~2,'0D-~2,'0D" y mth d))
      )))
      

(defun weekday-index-to-weekday-name (index &key (mode :full))
  #.(one-string-nl
     "Convert a weekday index such as returned by (GET-UNIVERSAL-TIME ...)"
     "into the name of the day of the week it corresponds to.  If mode is "
     ":abbrev or :abbreviated then the 3-letter standard prefix for that day"
     "is returned instead of the full name.")
  (let ((data
         (assoc index
                '((0 "Monday") (1 "Tuesday") (2 "Wednesday")
                  (3 "Thursday") (4 "Friday") (5 "Saturday") (6 "Sunday")))))
    (ecase mode
      (:full (cadr data))
      ((:abbrev :abbreviated) (subseq (cadr data) 0 3))
      )))

(defun month-index-to-month-name (index &key (mode :full))
  #.(one-string-nl
     "Convert a month index such as returned by (GET-UNIVERSAL-TIME ...)"
     "into the name of the month it corresponds to.  If mode is "
     ":abbrev or :abbreviated then the 3-letter standard prefix for that month"
     "is returned instead of the full name.")
  (let ((data
         (assoc (1- index)
                '((0 "January") (1 "February") (2 "March") (3 "April")
                  (4 "May") (5 "June") (6 "July") (7 "August")
                  (8 "September") (9 "October") (10 "November") (11 "December")
                  ))))
    (ecase mode
      (:full (cadr data))
      ((:abbrev :abbreviated) (subseq (cadr data) 0 3))
      )))

(defun past-time-description (past-universal-time &key (mode :full))
  #.(one-string-nl
     "An English description of a time/date in the past."
     "If the time is actually in the future, no description is given.")
  (multiple-value-bind (s min h d mth y)
      (get-decoded-time)
    (declare (ignore s min h))
    (multiple-value-bind (ps pmin ph pd pmth py pwd)
        (decode-universal-time past-universal-time)
      (declare (ignore ps))
      (let* ((seconds-in-a-day (* 60 60 24))
             (seconds-in-a-week (* seconds-in-a-day 7))
             (midnight-today (encode-universal-time 0 0 0 d mth y))
             (midnight-yesterday (- midnight-today seconds-in-a-day))
             (midnight-one-week-ago (- midnight-today seconds-in-a-week))
             (phour (let ((h (mod ph 12))) (if (zerop h) 12 h)))
             (am-or-pm (if (zerop (floor ph 12)) "AM" "PM")))
        (cond
         ((> past-universal-time (get-universal-time))
          (formatn "A time/date in the future..."))
         ((>= past-universal-time midnight-today)
          (formatn "Today, at ~2,'0D:~2,'0D ~A" phour pmin am-or-pm))
         ((>= past-universal-time midnight-yesterday)
          (formatn "Yesterday, at ~2,'0D:~2,'0D ~A" phour pmin am-or-pm))
         ((>= past-universal-time midnight-one-week-ago)
          (formatn "~A (~D days ago), at ~2,'0D:~2,'0D ~A" 
                   (weekday-index-to-weekday-name pwd)
                   (ceiling (- midnight-today past-universal-time) 
                            seconds-in-a-day)
                   phour pmin am-or-pm
                   ))
         (t 
          (formatn "~A, ~A ~2,'0D, ~D at ~2,'0D:~2,'0D ~A"
                   (weekday-index-to-weekday-name pwd :mode mode)
                   (month-index-to-month-name pmth :mode mode)
                   pd py
                   phour pmin am-or-pm
                   )))))))


(defun canonicalize-package-to-keyword (package)
  (cond 
   ((packagep package) (keywordize (package-name package)))
   ((stringp package) (keywordize package))
   ((symbolp package) (keywordize (string package)))
   (t 
    (error 
     "canonicalize-package-to-keyword cannot convert ~a to a package name"
     package))))

(defun create-defpackage-form 
       (package 
        &key 
        (target-package package)
        (nicknames (package-nicknames package)))
  #.(one-string-nl
     "Create a defpackage form for an existing package or :TARGET-PACKAGE,"
     "if provided. If target package is provided the created package will"
     "be similar to PACKAGE in the sense that all its imports, exports,"
     "shadows, and shadowing imports will have the same names."
     "However, internal symbols of PACKAGE will not exist in the"
     "TARGET-PACKAGE if the defpackage form is evaluated. "
     "This can used to copy the interface of a package in order to create"
     "new code implementing the same interface."
     )
  (let ((use (mapcar #'canonicalize-package-to-keyword 
                     (package-use-list package)))
	(nicknames nicknames)
	(shadow
	 (loop for s in (package-shadowing-symbols package)
	       when (eq (symbol-package s) package)
	       collect (keywordize s)))
	(shadowing-import-from-forms
	 (let ((si-packages nil))
	   (loop for s in (package-shadowing-symbols package)
		 unless (eq (symbol-package s) package)
		 do (pushnew (symbol-package s) si-packages))
	   (mapcarnn
	    (lambda (p) 
	      (let ((symbols
		     (loop for s in (package-shadowing-symbols package)
			   when (eq (symbol-package s) p)
			   collect (keywordize s))))
		(and symbols `(:shadowing-import-from
			       ,(canonicalize-package-to-keyword p)
			       ,@(mapcar #'keywordize symbols)))))
	    si-packages)))
	(import-from-forms
	 (let ((i-packages nil))
	   (do-symbols
	    (s package)
	    (if (and (not (eq (symbol-package s) package))
		     (multiple-value-bind
		      (symbol type) (find-symbol (string s) package)
		      (declare (ignore symbol))
		      (eq :internal type)))
		(pushnew (symbol-package s) i-packages)))
	   (mapcarnn 
	    (lambda (p)
	      (let ((symbols nil))
		(do-symbols
		 (s package)
		 (when 
                     (and (eq (symbol-package s) p)
                          (not (member s (package-shadowing-symbols package))))
		   (pushnew s symbols)))
		(and symbols `(:import-from 
			       ,(canonicalize-package-to-keyword p)
			       ,@(mapcar #'keywordize symbols)))))
	    i-packages)))
	(export
	 (let ((symbols nil))
	   (do-external-symbols
	    (s package)
	    (pushnew s symbols))
	   (mapcar #'keywordize symbols)))
	)
    `(defpackage ,(canonicalize-package-to-keyword target-package)
       ,@(when use `((:use ,@use)))
       ,@(when nicknames `((:nicknames ,@nicknames)))
       ,@(when shadow `((:shadow ,@shadow)))
       ,@shadowing-import-from-forms
       ,@import-from-forms
       ,@(when export `((:export ,@export))))
    ))

(defun printed-type-of (obj)
  "Prettied up type output"
  (if (find-package :wb) 
      (case (forward-package-funcall :wb :user-mode)
        (:bbl (forward-package-funcall :bbi :bbl-printed-type-of obj))
        (otherwise (forward-package-funcall :bbi :bbl-printed-type-of obj))
        )
    (type-of obj))) 

(defun type-to-english-string (type)
  "Changes OR type to comma-separated list of types as strings."
  (labels 
      ((flatten-ors (x) 
         (cond
          ((symbolp x) (list x))
          ((not (listp x)) (error "Invalid type specifier: ~A" x))
          ((not (eq 'or (first x))) (list x))
          (t (mapcan #'flatten-ors (cdr x)))
          )))
    ;; get rid of things like (or list list) with the remove-duplicates 
    (let ((flattened-type (remove-duplicates (flatten-ors type))))
      (loop 
       with s = ""
       with first? = t
       with last? = nil
       for f-types on flattened-type
       as or-type = (first f-types)
       as st = 
       (cond
        ((symbolp or-type) (string or-type))
        (t (format nil "~S" or-type))
        )
       do
       (setq last? (null (cdr f-types)))
       (if first?
           (progn (setq s st) (setq first? nil))
         (if last? 
             (setq s (s+ s " or " st))
           (setq s (s+ s ", " st))
           ))
       finally (return s)
       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-table-rows 
       (data 
        &key 
        (maximum-width nil)
        (column-widths nil)
        (lines-between-rows 0)
        (alignment :left)
        (upper-left-alignment alignment)
        (first-row-alignment alignment)
        (first-column-alignment alignment)
        (space-between-columns 1)
        (stream *standard-output*)
        &allow-other-keys 
        &aux
        (first-row (first data))
        (number-of-rows (length data))
        (number-of-columns (length first-row))
        )
  (when (numberp column-widths)
    (setq column-widths 
          (make-list number-of-columns :initial-element column-widths)))
  (unless column-widths 
    (setq column-widths
          (loop for j from 0 below number-of-columns collect
                (reduce 'max (mapcar (lambda (x) (length (elt x j))) data))
                )))
  (loop for row in data 
        for row-count from 0 below number-of-rows 
        do
        (loop for item in row 
              for cw in column-widths 
              for ccw = cw
              then (+ ccw cw space-between-columns) 
              for column-count from 0 below number-of-columns
              as actual-alignment = 
              (cond
               ((and (zerop row-count) (zerop column-count))
                upper-left-alignment)
               ((zerop row-count) first-row-alignment)
               ((zerop column-count) first-column-alignment)
               (t alignment))
              as format = 
              (case actual-alignment 
                (:left (format nil "~~~DA" cw))
                (:right (format nil "~~~D@A" cw))
                (:center 
                 (setq item (center-in item cw))
                 (format nil "~~~DA" cw)
                 ))
              do
              (when (or (null maximum-width) (< ccw maximum-width))
                (format stream format item)
                (loop for j from 0 below space-between-columns
                      do (format stream " "))
                ))
        (terpri stream)
        (loop for j from 0 below lines-between-rows do (terpri stream))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-index-and-data-file 
       (keys-and-data 
        keys-file
        data-file
        &key
        (key-extractors '(first))
        (data-extractor 'lastelem)
        (key-writer-function (lambda (key port) (print key port)))
        (data-writer-function (lambda (datum port) (print datum port)))
        )
  #.(one-string-nl
     "Takes a set of keys and data, one list per keys and data combo"
     "and creates 2 files, a keys file and a data file.  The keys file"
     "contains each key set and a file position pointing to the location in the"
     "data file where the data for that key set is to be found."
     "The index file is in the form of separate lists, each list of the form"
     "(key1 key2 ... file-position)"
     "In the input data, KEYS-AND-DATA, by default the data is the last"
     "element of each list, and the keys are the previous elements."
     "KEY-EXTRACTORS and DATA-EXTRACTOR may be used to specify other"
     "arrangements."
     "The index file records are written out using KEY-WRITER-FUNCTION"
     "which defaults to PRINT."
     "The data file records are written out using DATA-WRITER-FUNCTION"
     "which default to PRINT.")
  (when (numberp key-extractors)
    (setq key-extractors
          (loop for j from 1 to key-extractors collect
                `(lambda (x) (nth ,(1- j) x))
                )))
  (with-open-file 
      (p data-file :direction :output :if-exists :supersede)
    (with-open-file 
        (q keys-file :direction :output :if-exists :supersede)
      (loop for item in keys-and-data do
            (let* ((keys
                    (loop for f in key-extractors collect 
                          (funcall f item)
                          ))
                   (data (funcall data-extractor item))
                   (current-file-position (file-position p))
                   (index-entry (append keys (list current-file-position)))
                   )
              (funcall key-writer-function index-entry q)
              (funcall data-writer-function data p)
              ))
      (terpri p)
      (terpri q)
      )))

(defun randomize-vector (v)
  "Modifies in place vector V, such that the elements are randomized."
  (let ((len (length v)))
    (cond
     ((< len 2) v)
     (t 
      (loop for j from len downto 1
            do
            (let* ((r (random j))
                   (temp (aref v (1- j))))
              (setf (aref v (1- j)) (aref v r))
              (setf (aref v r) temp)
              ))
      v
      ))))
            
(defun room-total-bytes (room-output-string)
  (declare (ignorable room-output-string))
  #+:allegro
  (let ((pos (search "total bytes =" room-output-string)))
    (when pos 
      (let ((s (subseq room-output-string pos)))
        (let ((npos (position #\Newline s)))
          (if npos (subseq s 0 npos) s)
          ))))
  #-:allegro
  (error "Need to implement this!")
  )

(defmacro with-room-total-bytes-displayed (&body body)
  `(prog1
       (progn ,@body)
     (format t 
      "~A~%" 
      (room-total-bytes 
       (with-output-to-string (s) 
         (let ((*standard-output* s) (*error-output* s))
           (room t)
           ))))))
       
         
   