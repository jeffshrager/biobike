;;; Jeff Shrager's General Lisp Utilities
;;; Copyright (c) Jeff Shrager 1999 2000 2001 
;;; Contact: jshrager@andrew2.stanford.edu; 650.325.1521.287

;;; There are various levels of reporting.  Change this to LOUD LOW or SILENT
;;; SCREAM reports get through LOW, but WHISPER reports don't.  SILENT cuts everything off.

(defvar *volume* 'loud)
(defvar *silence-counter* 0)

(defmacro scream (format &rest args)
  `(cond ((eq *volume* 'silent) (check-silence))
	 (t (format t ,format ,@args))))
(defmacro whisper (format &rest args)
  `(progn (when (not (member *volume* '(low silent)))
		(format t ,format ,@args))
	  (when (eq *volume* 'silent) (check-silence))))
  (defmacro volume (?)
  `(case ',?
	((loud high) (setq *volume* 'loud))
	((low whisper) (setq *volume* 'low))
	((off down silent) (setq *volume* 'silent))
	(t "Error: Volume must be one of LOUD/HIGH, LOW/WHISPER, or OFF/DOWN/SILENT")))
(defun check-silence ()
  (when (zerop (mod (incf *silence-counter*) 1000))
	(format t "[Warning, you have the cone of silence down!  Use: (volume low) or (volume loud) to turn it up.]~%")))

;;; Used to display the values of things for debugging

(defmacro display (&rest names)
  `(progn 
     ,@(loop for name in names
	     collect `(format t "~a = ~a~%" ',name ,name))
     ,(car names)))

;;; Displays the first n entries in a hash table:

(defun dht (table n)
  (maphash #'(lambda (key value)
	       (when (zerop (decf n)) (return-from dht))
	       (format t "~s: ~s~%" key value)	       
	       )
	   table))

;;; --- Some input utilities.

(defun pause ()
  (if (not (y-or-n-p "Continue?"))
      (break)))

(defun random-one-of (l)
  (nth (random (length l)) l))

#| prob. not needed unless not in commonlisp -- remove well after 961001
;;; This is used by any function that is going to do read-line's and
;;; needs to be called both interactively and sometimes from another
;;; function.  The problem is that when the user type the call at the
;;; lisp reader, there's a newline in the input buffer, but when it's
;;; called from some other function that's already doing reads, there
;;; isn't.  This missynchs input.  This function eats a single newline
;;; that's left in the input buffer, otherwise does nothing.

(defun clear-input ()
  (if (equal (peek-char) #\newline)
      (read-line)))|#

;;; --- Parse string breaks the string up at spaces into little
;;; strings.  These must be (read-from-string'ed...) outside if you
;;; want numbers or atoms.  We replace pounds and colons with * and -
;;; respectively so that if you do want to read-from-string the
;;; result, you aren't screwed.  In you'd rather not break things up
;;; on space, and know that it's all lisp-readable, you might rather
;;; use multi-rfs (multi-read-from-string).  Someday I ought to do
;;; this with readtables....

(defun parse-string (string &key (make-substitutions t) (break-char #\space))
  (fast-substitute string make-substitutions)
  (prog (pos item results break-string)
        (setq break-string (format nil "~a" break-char))
    loop
        (setq string (string-left-trim break-string string))
	(setq item (parse-string-from-string string break-char))
	(if (null item) (return (reverse results)))
	(push item results)
	(setq pos (position break-char string))
	(if (null pos) (return (reverse results)))
	(setq string (subseq string pos))
	(go loop)))

;;; This version takes a string of break chars.

(defun parse-string2 (string &key (make-substitutions t) (break-string " "))
  (fast-substitute string make-substitutions)
  (prog (item results)
    loop
        (setq string (string-left-trim break-string string))
	(setq item (parse-string-from-string2 string break-string))
	(if (null item) (return (reverse results)))
	(push item results)
	(setq string (subseq string (length item)))
	(go loop)))

(defun parse-string-from-string2 (instring break-string)
  (if (zerop (length instring))
      ()
    (prog (outstring)
	  (setq outstring "")
      loop
          (if (or (zerop (length instring))
		  (position (aref instring 0) break-string))
	      (return outstring))
	  (setq outstring (format nil "~a~a" outstring (aref instring 0)))
	  (setq instring (subseq instring 1))
	  (go loop)
	  )
    ))

;;; Fast substitute logically does:
;;; (if make-substitutions
;;;     (setq string (substitute #\- #\: (substitute #\* #\# string))))
;;; (setq string (substitute #\space #\tab string))
;;; But scanning the string that many times is too slow, so we do it inline.
;;; WARNING: THIS DESTROYS THE STRING (which is usually an okay thing to do)

(defun fast-substitute (string make-substitutions)
  (dotimes (p (length string))
    (let ((c (aref string p)))
      (if (char-equal c #\tab)
	  (setf (aref string p) #\space)
	(if make-substitutions 
	    (if (char-equal c #\:)
		(setf (aref string p) #\space)
	        (if (char-equal c #\#)
		    (setf (aref string p) #\space)
		    (if (char-equal c #\()
			(setf (aref string p) #\space)
			(if (char-equal c #\))
			    (setf (aref string p) #\space)
			    (if (char-equal c #\")
				(setf (aref string p) #\space)
				(if (char-equal c #\')
				    (setf (aref string p) #\space)
				    (if (char-equal c #\!)
					(setf (aref string p) #\space)
		  )))))))))))
  string)

;;; Stops on space, and returns nil when there's nothing there to begin with.

(defun parse-string-from-string (instring break-char)
  (if (zerop (length instring))
      ()
    (prog (outstring)
	  (setq outstring "")
      loop
          (if (or (zerop (length instring))
		  (eq break-char (aref instring 0)))
	      (return outstring))
	  (setq outstring (format nil "~a~a" outstring (aref instring 0)))
	  (setq instring (subseq instring 1))
	  (go loop)
	  )
    ))

;;; Do read-from-strings until the string is empty.  This is often used 
;;; in place of:
;;;  (mapcar #'read-from-string (parse-string ...))
;;; when you know that there's no crap in the entry.

(defun multi-read-from-string (string &aux result (start 0))
  (loop 
    (multiple-value-bind (item nextloc) 
              (read-from-string string nil '*eof* :start start)
      (if (eq '*eof* item) 
	  (return (reverse result))
	  (progn (push item result)
		 (setq start nextloc))
	  ))))

;;; Note that these prompting utils REQUIRE the default argument to be
;;; be given, which is a tad weird but it's that way so that I can use
;;; the rest for the format arg list because you can't do both &key
;;; and &rest.  I guess that I ought to use a macro here.

(defun prompt-for-number (form default &rest args &aux n)
  (setq form (format nil "~%~a [~a]: " form (if default default "no default")))
  (loop 
    (apply #'format (cons t (cons form args)))
    (let ((ans (read-line t nil nil)))
      (setq n (if (zerop (length ans))
		  () 
		  (read-from-string ans)))
	    )
    (if (null n)
        (if default
  	    (return default)
	    (format t "~%Sorry.  There's no default.~%"))
        (if (numberp n) 
	    (return n)
	    (format t "~%This has to be a number. Please try again.~%")
	    ))
    )
  )

;;; Note that prompt-for-string doesn't have any error checking, and
;;; will return nil if a null string is entered.  This might be a
;;; problem for callers.

(defun prompt-for-string (form &rest args)
  (apply #'format (append (list t) (list form) args))
  (let ((ans (read-line)))
    (if (zerop (length ans)) () ans)))

;;; This version is a little weird; it is just a short cut to use the
;;; default value, which just saves the caller from having to test and
;;; skip the prompt.

(defun get-string-w-default (prompt default &optional use-default)
  (let ((ans (if use-default 
		 (progn (format t "Using ~a~%" default) default)
	         (prompt-for-string
		   (format nil "~a (~a): " prompt default)))))
    (if ans ans default)))

;;;

(defun file-exists? (name)
  (let ((handle (open name :direction :input :if-does-not-exist nil)))
    (if handle
	(progn (close handle) t)
      ())))

;;; --- Some extensions.

;;; Same as system but does error checking.
#|
(defun system! (cmdform &rest args)
  (let*( (cmd (apply #'format (append (list nil cmdform ) args)))
	 (r (system cmd)))
    (if (not (zerop r))
	(progn (format t "System call:[~a] failed, error ~a!~%" cmd r)
	       (break "Stopping for assessment!"))
      )))
|#
;;; Order-irrelevant at the top level.

(defun set-equal (a b)
  (cond ((null a) (null b))
        ((member (car a) b :test #'equal)
	 (set-equal (cdr a) (remove (car a) b :test #'equal)))
	(t ())))

;;; Read a file into a list of lists of its lines, using read-from-string
;;; on each line.  The protected? arg only does the processing if the 
;;; first char of the line is a number.

(defun read-table-file (fn &optional protected? &aux l r)
  (with-open-file (f fn :direction :input)
     (loop (setq l (read-line f nil nil))
	   (if (null l) (return nil))
	   (if (or (not protected?)
		   (and (> (length l) 0)
			(let ((tc (char-code (aref l 0))))
			  (and (>= tc 48) (<= tc 57)))))
	       (push (multi-read-from-string l) r))
	       )
     (reverse r)))

;;; We can write either a column or matrix. 

(defun write-table-file (fn table &optional rounding-fn)
  (with-open-file (f fn :direction :output :if-exists :supersede)
    (dolist (line table)
      (if (numberp line) (setq line (list line)))
      (dolist (v line)
        (format f "~a " (if rounding-fn (funcall rounding-fn v) v)))
      (format f "~%")
      )
    ))

;;; Get the first n items of a list, somewhat efficiently.

(defun first-n (n l &aux r)
  (setq r (list '!))
  (do ((s l (cdr s))
       (v r (cdr v))
       (k n (1- k)))
      ((zerop k) (cdr r))
      (rplacd v (list (car s)))
      ))

;;; This return the LAST n items is a list; Note that it does NOT create
;;; a new list!

(defun last-n (n l)
  (nthcdr (- (length l) n) l))

;;; Form all combinations of items in a list, including the list itself.

(defun all-sublists (l)
  (cond ((null l) ())
	((null (cdr l)) (list l nil))
        (t (append (all-sublists (cdr l))
		   (mapcar #'(lambda (i) (cons (car l) i))
			   (all-sublists (cdr l)))))
	))

;;; --- Time/date functions.

(defun get-time2 ()
  (multiple-value-bind
    (second minute hour date month year dow dstp tz)
    (get-decoded-time)
    (list :second second
	  :minute minute
	  :hour (+ 2 hour) ; at the moment this seems to be two off; might it be 
	                   ; because of daylight savings time or something??
	  :date date
	  :month month
	  :year year
	  :dow dow
	  :dstp dstp
	  :tz tz)
    ))

(defun time-as-int ()
  (let ((time (get-time2)))
  (+ (* (getf time :hour) 3600)
     (getf time :second)
     (* 60 (getf time :minute)))))

(defun pretty-time (&optional time-as-int)
  (or time-as-int (setq time-as-int (time-as-int)))
  (let* ((r (mod time-as-int 3600))
	 (h (truncate (/ time-as-int 3600)))
	 (s (mod r 60))
	 (m (truncate (/ r 60))))
    (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))
    
(defun jeff-date () (date-as-int))

(defun date-as-int ()
  (let ((time (get-time2)))
    (read-from-string
     (format nil "~4d~2,'0d~2,'0d" 
	         (getf time :year)
		 (getf time :month)
		 (getf time :date))
     )))

;;; A Jeff date can be 980731 or 19980731 (although the latter is more standard)
;;; This standardizes them.

(defun std-jeff-date (date)
  (let* ((date (format nil "~a" date)) ; ensure that it's a string
	 (len (length date)))
    (cond ((= len 6)
	   (read-from-string (format nil "19~a" date)))
	  ((= len 8)
	   (read-from-string date))
	  (t (break "Tried to standardize this jeff-date: ~a" date))
	  )))

;;; To compare Jeff Dates you need to use this function to standardize
;;; and julianize them, then you can use them as real valued days from
;;; 1/1/1900 This is only approximate since it assumes 30 days/month
;;; and 365 days/yr.

(defun norm-jeff-date (jd)
  (let* ((sjd (std-jeff-date jd))
	 (jdyear (truncate (/ sjd 10000)))
	 (jdmo (truncate (/ (- sjd (* jdyear 10000)) 100)))
	 (jdday (- sjd (+ (* jdyear 10000) (* jdmo 100))))
	 )
    (+ (* 365 (- jdyear 1900))
       (* 30 jdmo)
       jdday)))

(defvar *jdmodays* '(31 27 31 30 31 30 31 31 30 31 30 31))

(defun explode-jeff-date (jd)
  (let* ((sjd (std-jeff-date jd))
	 (jdyear (truncate (/ sjd 10000)))
	 (jdmo (truncate (/ (- sjd (* jdyear 10000)) 100)))
	 (jdday (- sjd (+ (* jdyear 10000) (* jdmo 100))))
	 )
    (list jdyear jdmo jdday)))

(defun jeff-date+1 (jd)
  (let* ((sjd (std-jeff-date jd))
	 (jdyear (truncate (/ sjd 10000)))
	 (jdmo (truncate (/ (- sjd (* jdyear 10000)) 100)))
	 (jdday (- sjd (+ (* jdyear 10000) (* jdmo 100))))
	 (modays (nth (1- jdmo) *jdmodays*))
	 )
    (incf jdday)
    (when (> jdday modays)
      (setq jdday 1)
      (incf jdmo))
    (when (> jdmo 12)
      (setq jdmo 1)
      (incf jdyear)
      )
    (read-from-string
     (format nil "~4d~2,'0d~2,'0d" jdyear jdmo jdday))
    ))

(defun difference-jeff-dates (jd1 jd2)
  (- (norm-jeff-date jd1) (norm-jeff-date jd2)))

;;; Like APL compress, uses a list of t/nil to select from a target of
;;; the same length.  This is a really inefficient implementation of
;;; this.  Ought to be destructive some day.

(defun compress (selector target &aux r)
  (mapcar #'(lambda (a b) (if a (push b r))) selector target)
  (reverse r))

;;; Why isn't this standard in lisp???

(defun flatten (l)
  (cond ((null l) ())
	((atom l) (list l))
	(t (append (flatten (car l))
		   (flatten (cdr l))))))


;;; Copy the first c lines of a file (skipping s lines first).  Used to break up huge files.

(defun fscopy (infile outfile c &optional (s 0))
  (with-open-file (instream infile :direction :input)
    (with-open-file (outstream outfile :direction :output :if-exists :supersede)
      (when (not (zerop s))
	(loop for i from 1 to (* s 2) ; count the line-turn
	      do (read-line instream))
	)
      (loop for i from 1 to c
	    do 
	    (princ (read-line instream) outstream)
	    (read-line instream)
	    (terpri outstream)
	    )
      )))

#| Some versions of lisp require ^M processing, so I've dyked this, 
   and I can read it into individual programs and modified as needed.  

(defun scan-stream-for-line-beginning (stream string &key (skip 0))
  (loop with string-length = (length string)
        with sl+ = (+ skip string-length)
	for line = (read-line! stream)
	until (or (null line)
		  (and (>= (length line) sl+)
		       (string-equal (subseq line skip sl+) string)
		       ))
	finally (return line)))

(defun scan-stream-for-line-beginning (stream string &key (skip 1))
  (loop with string-length = (length string)
        with sl+ = (+ skip string-length)
	for line = (read-line stream nil nil)
	until (or (null line)
		  (and (>= (length line) sl+)
		       (string-equal (subseq line skip sl+) string)
		       ))
	finally (return line)))
|#

(defun read-line! (s)
  (loop with c* 
	for c = (read-char s nil nil)
	do
	(cond ((null c) (return nil))
	      ((member c '(#\linefeed #\newline #\return) :test #'char-equal)
	       (return (coerce (reverse c*) 'string)))
	      (t (push c c*)))))



(defun number?? (term)
  (and (not (zerop (length term))) (position (aref term 0) "0123456789")))


;;; Dan's utilities:

;;; This is a helper function to parse a XML page for the content of the first occurance of a given tag. It's input
;;; is a cons of the XML page and the tag (without any <>'s). For example, if the XML page contains 
;;; <Query><Hit>...</Hit></Query>, and the tag was "Hit", this function would return the first
;;; occurence of <Hit>...</Hit>

(defun get-inbetween-tag (page tag &key (start2 0) (from-end 'NIL))
  (let* ((start-tag (concatenate 'string "<" tag))
         (end-tag (concatenate 'string "</" tag ">"))
         (start-pos (search start-tag page :start2 start2 :from-end from-end))
         (end-pos (if (not start-pos) 0 (+ (search end-tag page :start2 start2 :from-end from-end) (length end-tag)))))
    (and start-pos 
	 (> end-pos start-pos)
	 (subseq page start-pos end-pos))
    ))

;;; This function returns the first occurance of what ever is inbetween start-token and 
;;; end-token (both strings) from the input string

(defun subseq-token (string start-token end-token)
  (if (search start-token string)
      (let* ((start-pos (+ (search start-token string) (length start-token)))
	     (end-pos (search end-token string :start2 start-pos)))
	(subseq string start-pos end-pos))))

;;; This recursively removes all occurances of what-to-remove from string

(defun remove-string (what-to-remove string)
  (if (search what-to-remove string)
      (let* ((wtr-start (search what-to-remove string))
	     (wtr-end (+ wtr-start (length what-to-remove))))
	(remove-string what-to-remove (concatenate 'string (subseq string 0 wtr-start) (subseq string wtr-end))))
    string))

;;; This removes all of the characters in characters from string

(defun remove-characters (string &rest characters)
  (loop for char in characters
	do (setq string (remove char string))
	finally (return string)))

;;; This creates a list where each item in the list is just a part of the input string seperated by token

(defun make-list-from-inbetween-token (string token &key start-with-length)
  (loop with token = (if (stringp token) token (string token))
	with rt-pos = 0
	with string = (concatenate 'string token string token)
	until (not rt-pos)
	as lf-pos = (search token string :start2 rt-pos)
	do 
	(setq rt-pos (search token string :start2 (+ lf-pos 1)))
	when (and rt-pos lf-pos)
	collect (string-trim " " (subseq string (+ lf-pos (if start-with-length (length token) 1)) rt-pos))))

;;; This removes all HTML tags from an HTML page and leaves whatever is not within and tag unchanged

(defun remove-tags (html &aux results)
  (loop for i from 0 to (- (length html) 1)
	as char = (char html i)
	as in-tag? = (or in-tag? (char= char #\<))
	do
	(if (not in-tag?) 
	    (setq results (concatenate 'string results (string char)))
	  (if (char= char #\>) (setq in-tag? 'NIL)))
	finally (return results)))

;;; This recursively removes whatever is inbetween token1 and token2 from string

(defun remove-inbetween-tokens (token1 token2 string)
  (if (position token1 string)
      (let* ((wtr-start (position token1 string))
	     (wtr-end (+ (position token2 string :start (+ wtr-start 1)) 1)))
	(remove-inbetween-tokens token1 token2 (concatenate 'string (subseq string 0 wtr-start) (subseq string wtr-end))))
    string))

;;; This takes a certain number of seconds and converts it to it's corresponding time in minutes and seconds

(defun format-time (seconds)
  (let ((min (round (truncate (/ seconds 60))))
	(sec (round (mod seconds 60))))
    (cond ((zerop sec) (format nil "~A min"  min))
	  ((zerop min) (format nil "~A sec" sec))
      ((format nil "~A min ~A sec" min sec)))))

(defun list-remove-duplicates-equal (list)
  (let (new-list)
    (loop for item in list
	  when (not (member item new-list :test 'equal))
	  do
	  (setf new-list (adjoin item new-list)))
    (reverse new-list)))

;;; This returns the intersection of strings in two lists

(defun intersection-equal (list1 list2 &aux results)
  (loop for item1 in list1
	do (loop for item2 in list2
		 do (if (equal item1 item2) (push item1 results)))
	finally (return (reverse (list-remove-duplicates-equal results)))))

;;; This takes the arithmetic mean of two numbers

(defun average (num1 num2)
  (/ (+ num1 num2) 2))

;;; This writes a hashtable to a file

(defun write-hashtable (file ht)
  (with-open-file (stream file :direction :output :if-exists :supersede)
		  (maphash #'(lambda (key value)
			       (print (cons key value) stream)) 
			   ht)))

;;; This reads a hashtable from a file

(defun read-hashtable (file)
  (let* ((ht (make-hash-table :test #'equal)))
    (with-open-file (stream file :direction :input)
		    (loop for line = (read stream nil nil)
			  until (null line)
			  do
			  (push (cdr line) (gethash (car line) ht))))
    ht))

;;; This function prepends each item in a list with pre

(defun prepend-list (pre list)
  (loop for item in list
	collect (cons pre item)))


(defun prepend-with-zeros (string length)
  (loop until (>= (length string) length)
	do (setf string (concatenate 'string "0" string)))
  string)


(defun remove-stars (long-list)
  (loop for list in long-list
	collect	(loop for item in list
		      collect (string-trim "*" item))))


(defun word-wrap (string line-length &key (newline-string "<BR>"))
  (let ((new-string ""))
    (loop for i = 0 then (+ i line-length)
	  until (> i (length string))
	  as sub = (subseq string i (min (+ i line-length) (length string)))
	  as new-string = (concatenate 'string new-string newline-string sub)
	  finally (return (string-trim (string #\null) new-string)))))

(defun concatenate-string-list (string-list)
  (let ((concatenated-string ""))
    (loop for string in string-list
	  as concatenated-string = (concatenate 'string concatenated-string string)
	  finally (return concatenated-string))))
	

;;; This removes the # characters from structures with a large depth

(defmacro pp (a) `(null (let ((*print-level* nil)) (print ,a))))

(export '(*volume* *silence-counter* scream whisper volume check-silence display
		   pp dht pause random-one-of clear-input parse-string parse-string2
		   parse-string-from-string2 fast-substitute parse-string-from-string
		   multi-read-from-string prompt-for-number prompt-for-string
		   get-string-w-default file-exists?  system!  set-equal read-table-file
		   write-table-file first-n last-n all-sublists get-time2 time-as-int
		   pretty-time jeff-date date-as-int std-jeff-date norm-jeff-date
		   *jdmodays* explode-jeff-date jeff-date+1 difference-jeff-dates
		   compress flatten fscopy scan-stream-for-line-beginning
		   scan-stream-for-line-beginning read-line!  get-inbetween-tag
		   subseq-token remove-string number??  make-list-from-inbetween-token
		   remove-tags remove-inbetween-tokens format-time intersection-equal
		   average write-hashtable read-hashtable prepend-list pp prepend-with-zeros
		   remove-stars word-wrap concatenate-string-list))

