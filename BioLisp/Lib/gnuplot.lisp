;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2012 JP Massar, Jeff Shrager, Mike Travers           |
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

;;; Authors: Jeff Shrager, JP Massar

;;; The variable *gnuplot-path* is a configuration variable 
;;; and it is defined with its default value in .../biodefs/configuration.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 
   '(
     plot-array
     line-plot
     )))

;;; This takes a series of string gnuplot commands and returns 
;;; a wb::jpg defstruct structure which, when output to the weblistener
;;; causes html to be generated which renders the plot. 

(defun gnuplot-commands-to-jpeg (commands datafile)
  (with-temp-file-in 
      (jpg-file wb::*webtmp-directory* 
                :type "jpg" :prefix "gnuplot" :if-exists :again :delete? nil)
   (with-temp-file-in 
       (web-data-file 
        wb::*webtmp-directory* 
        :type "txt" :prefix "gnuplot" :if-exists :again :delete? nil)
    (with-temp-file-in 
        (gnuplot-file wb::*tmp-directory* 
                      :type "gp"  :prefix "gnuplot" :if-exists :again)
      (with-temp-file-in 
          (postscript-file wb::*tmp-directory* 
                           :type "ps" :prefix "gnuplot" :if-exists :again)
        ;; I don't know why this would fail, but just in case....
        (ignore-errors (sys::copy-file datafile web-data-file)) 
        (with-open-file 
            (o gnuplot-file :direction :output :if-exists :supersede)
        ; (format t "Command file is ~a~%" gnuplot-file)
	  (format o "set output \"~a\"~%" postscript-file)
	  (format o "set terminal postscript portrait~%")
	  (loop for command in commands do (format o "~a~%" command)))
	(cl-user::run-shell-command 
	 (formatn "cat ~a | ~a" gnuplot-file cl-user::*gnuplot-path*))
	(cl-user::run-shell-command 
	 (formatn "gs -dNOPAUSE -sDEVICE=jpeg -sOutputFile=~a -q -dBATCH ~a"
		 jpg-file postscript-file))
	(wb::make-jpg
	 :path 
         (formatn "~a~a" cl-user::*webtmp-url* (file-namestring jpg-file))
	 :click-path 
         (formatn "~a~a" cl-user::*webtmp-url* (file-namestring web-data-file)))
	)))))

;;; Plotting data:

(defun plot-array (array labels)
  #.(one-string-nl
     "Given an array of R rows and C columns, and a list of exactly C labels "
     "(one per column of data) this function displays a line graph of the "
     "data in the columns.  There will be exactly R points along the X axis, "
     "and C separate lines in the plot.")
  (setq labels (ensure-list labels))
  (let* ((nrows (array-dimension array 0))
         (ncols (array-dimension array 1))
         (datafilename (gentemp (string (wb::user-session-id))))
	 (filename 
          (namestring
           (make-pathname 
            :name datafilename 
            :type "dat"
            :defaults (cl-user:translate-simple-lp "tmp:foo.bar")
            ))))
    (unless (= ncols (length labels))
      (error 
       #.(one-string-nl
          "The number of columns in the data array (~a) is not the same as"
          "the number of labels (~a)!")
       ncols (length labels)))
     (with-open-file (o filename :direction :output :if-exists :supersede)
       (loop for j from 0 below ncols
	     as label in labels
	     do
	     (format o "#~a~%" label)
	     (loop for i from 0 below nrows do
		   (format o "~D~c" (1+ i) #\tab) 
		   (format o "~d~%" (aref array i j)))
	     (terpri o) (terpri o)
	     ))
    (gnuplot-commands-to-jpeg
     (list 
      (with-output-to-string (o) 
        (format o "plot '~a' " filename)
	(loop for n from 0 by 1
	      with limit = (1- ncols)
	      as label in labels
	      do 
	      (unless (zerop n) (format o "'' "))
	      (format o "index ~a ti '~a' with lines" n label)
	      	      (if (= n limit) 
		  nil
		  (format o ", ")))))
     filename)
    ))

(defun abbrev-list (list lenlimit)
  (let ((len (length list)))
    (if (<= len lenlimit)
	list
      (append 
       (loop for i below lenlimit
	     as item in list
	     collect item)
       (list (format nil "<<~a more>>" (- len lenlimit)))))))
	
;;; Various sorts of input are translated by this into the canonical form used
;;; by plot-array.  We try to guess what the user has in mind.

(defun line-plot (data &key labels)
  #.(one-string-nl
     "LINE-PLOT uses GnuPlot to create simple line graphs."
     "The simplest way to use it is, for example: "
     "(line-plot '(3.1 4.1 5.9 2.5))"
     "which will plot these as Y values along the X axis from 1 to 4 "
     "(the number of values given), and will assign the line the name "
     "\"unlabeled-data\"."
     ""
     "You can label your data by simply giving the labels as the first entry:"
     "(line-plot '(\"my data\" 3.1 4.1 5.9 2.5))"
     "Although these examples are simple, they are also technically incorrect!"
     "You are supposed to give line-plot a LIST of LISTS, not just a single"
     "list, as above. For example: "
     "(line-plot '((list-a 3 2 4 1 5) (list-b 12 34 32 21 24)))"
     "which will plot BOTH lines (with appropriate labels) on the same graph."
     ""
     "Instead of putting the labels inside the lists (as the first elements,"
     "as immediately above), you can provide the optional keyword argument"
     ":labels, for example:"
     "(line-plot '((3 2 4 1 5) (12 34 32 21 24)) :labels '(list-a list-b))"
     "which turns out to be slightly more convenient for using line-plot"
     "in conjunction with other functions."
     ""
     "LINE-PLOT accepts either lists or vectors."
     "(This is often convenient for plotting TABLE-DATA objects as the"
     "function TABLE-DATA-DATA-ROWS gives you a list of one-d arrays."
     ""
     "When a list of lists or list of vectors is provided, the first element"
     "of each row will be taken to be the line label if you don't provide"
     "a value for the :labels keyword."
     ""
     "Incidentally, if you click on the graph, you'll get the raw data in a"
     "vaguely useful format that you might be able to cut-and-paste into"
     "some other program."
     )
  (cond
   ;; Handle stupid mistakes:
   ((and (listp data) (numberp (car data)))
    ;; User prob. intended a list of lists WITHOUT label.
    (let ((new (list (cons "unlabeled-data" data))))
      (cformatt 
       (one-string-nl
        "Warning: LINE-PLOT was given this: ~s"
        "but assumed that you meant this: (~s)" 
        "See (help line-plot) for more information on how to provide data.") 
       (abbrev-list data 5)
       (abbrev-list data 5))
      (setq data new)))
   ((and (listp data) (or (symbolp (car data)) (stringp (car data))))
    ;; User prob. intended a list of lists WITH label.
    (let ((new (list data)))
      (cformatt 
       (one-string-nl
        "Warning: LINE-PLOT was given this: ~s"
        "but assumed that you meant this: (~s)"
        "See (help line-plot) for more information on how to provide data.")
       (abbrev-list data 5)
       (abbrev-list data 5))
      (setq data new)))
   )
  (cond 
   ;; Simples case, a bunch of data lists in lists -- with or without labels.
   ((and (listp data) (listp (car data)))
    (let* ((nrows 
            (if labels
                (length (car data)) 
              (progn 
                (cformatt 
                 (one-string-nl
                  "Warning: LINE-PLOT wasn't given any labels for these lines"
                  "so it's assuming that the first element of each entry is"
                  "the entry's label. See (help line-plot) for more"
                  "information on how to provide data."))
                (1- (length (car data))))))
	   (ncols (length data))
	   (target-array (make-array (list nrows ncols))))
      ;; All rows better have the same number of entries!
      (loop for row in data
	    with firstlen = (length (car data))
	    unless (= (length row) firstlen)
	    do
            (error 
             (one-string-nl
              "In LINE-PLOT some of the data were not of the same length!"
              "(Lengths of given data are: ~a)" 
              (mapcar 'length data))))
      ;; If labels are given, we use them and plot all the data,
      ;; otherwise, we assume that the 0th array entry is a label.
      (loop for coldata in data
	    as col from 0 by 1
	    do (loop for rowpos from (if labels 0 1) below (length coldata)
		     as row from 0 by 1
		     as rowdata = (nth rowpos coldata)
		     do (setf (aref target-array row col) rowdata)))
      (plot-array 
       target-array 
       (or labels (loop for row in data collect (car row))))
      ))
   ;; From TABLE-DATA-DATA-ROWS you get a list of one-d array, 
   ;; and have to provide labels.
   ;; If there are given labels, we use those, otherwise, 
   ;; assume that the first entry in each row is the label.
   ((and (listp data) (arrayp (car data)))
    (cformatt "Line-plot assumes that this is a list of 1d vectors of data.")
    ;; If labels are given, we use them and plot all the data, otherwise,
    ;; we assume that the 0th array entry is a label!
    (let* ((nrows (if labels (length (car data)) (1- (length (car data)))))
	   (ncols (length data))
	   (target-array (make-array (list nrows ncols))))
      (loop for coldata in data
	    as col from 0 by 1
	    do (loop for rowpos from (if labels 0 1) below (length coldata)
		     as row from 0 by 1
		     as rowdata = (aref coldata rowpos)
		     do (setf (aref target-array row col) rowdata)))
      (plot-array target-array (or labels
				   (loop for row in data
					 collect (aref row 0))))
      ))
   (t 
    (error 
     (one-string-nl
      "LINE-PLOT does not recognize the type of data given."
      "See (help line-plot) for more information on how to provide data.")))
   ))

#|

(plot :lines '(("Line 1 label" 0 2 4 3 5) ("The next line" ....))
      :xlabel "the x axis"
      :ylabel "the y axix"
      :xrange '("a" "b" "c" "d" "e")  
      :yrange '(0 50)
      )

(plot :xy '((3 4) (5 6) ...)
      ... 
      :xrange '(0 50)
      ...)

Example:

(gnuplot-command-strings-to-png 
  "#
# $Id: gnuplot.lisp,v 1.34 2011/12/28 16:27:30 jshrager Exp $
#
# warning:  this demo is SLOW on PCs without math coprocessors!
#
# From _Automatic_Control_Systems_, fourth ed., figure 6-14
# transient response of a second-order system to a unit step input function
#
damp(t) = exp(-s*wn*t)/sqrt(1.0-s*s)
per(t) = sin(wn*sqrt(1.0-s**2)*t - atan(-sqrt(1.0-s**2)/s))
c(t) = 1-damp(t)*per(t)
#
#	wn is natural undamped frequency
#	s is damping factor
#
wn = 1.0
set xrange [0:13]
set samples 50
set dummy t
set key box
#
# plot c(t) for several different damping factors s
#
plot s=.1,c(t),s=.3,c(t),s=.5,c(t),s=.7,c(t),s=.9,c(t),s=1.0,c(t),s=1.5,c(t),s=2.0,c(t)
")

URL to all those demos: http://gnuplot.sourceforge.net/demo_4.1/


|#

(defgeneric plot (type &rest args))

(defmacro with-user-results-file ((symbol filename) &body body)
  #.(one-string-nl
     "Bind SYMBOL to the full pathname of a file, FILENAME, in the 'results'"
     "subdirectory of the user's instance home directory."
     "E.g., if FILENAME = \"foo.txt\" SYMBOL will be bound to the pathname"
     "whose namestring is:"
     "\"/home/biobike/massar/results/foo.txt\""
     "BODY is executed in the context of this binding.")
  (let ((rdsym (gensym "resultsdir")))
  `(let* ((,rdsym (append-subdir
                   (wb::visitor-directory wb::*username*) "results")))
     (ensure-directories-exist ,rdsym)
     (let ((,symbol (merge-pathnames ,filename ,rdsym)))
       ,@body))))

(defun gnuplot-command-strings-to-png 
       (strings
        &key 
        (height 500)
        (width 500)
        (results-filename 
         (formatn "~a.png" (pathname-name (make-new-temp-file-path "")))))
  (block exit
    (with-user-results-file 
     (results-path results-filename)
     (with-temp-file-in 
         (gnuplot-file wb::*tmp-directory* 
                       :type "gp"  :prefix "gnuplot" :if-exists :again)
       (with-open-file (gnuplot-stream gnuplot-file
                                       :direction :output :if-exists :supersede)
         (format gnuplot-stream "set output \"~a\"~%" (namestring results-path))
         (format gnuplot-stream 
                 "set terminal png transparent size ~d,~d~%" 
                 height width)
         (format gnuplot-stream "~a"
                 (if (stringp strings) strings
                   (string-join strings #\newline)))
         )
       (case (protected-shell-command 
              (formatn "cat ~a | ~a" gnuplot-file cl-user::*gnuplot-path*)
              :exec? nil)
         (:timeout (return-from exit nil))
         (otherwise nil)))
     (let ((link-path (make-new-temp-file-path wb::*webtmp-directory*)))
       ;; this is not likely to time out... 
       (protected-shell-command 
        (formatn "ln -s ~a ~a"
                 (namestring results-path)
                 (namestring link-path)))
       (wb::make-jpg 
        :path (one-string user::*webtmp-url* (file-namestring link-path)))
       ))))


