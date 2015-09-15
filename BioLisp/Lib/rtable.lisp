;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bio; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 by The BioBike Team                             |
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

;;; This was totally rewritten to use simple tables by JS on 20060303
;;; because the complex tables are just too hard to manage, and the
;;; complexity was too much for the simple tutorial that this is
;;; supposed to be a part of!

;;; Writes out the data array of a table to a file
;;; in a simple R format:
;;;   -- rows are numbered from 1
;;;   -- columns are named as C1, C2, ...
;;;   -- row values are delimited by #\Tab
;;;   -- no comments.  single header line followed by data
;;; This can accept either a simple table (list of lists)
;;; or a BioBike table-data object. (Could be methodized if it we're 3am!)

(defun table-data-to-simple-r-format (table file &key (format "~5,2F"))
  ;; Assumes that: (a) the length of the first row is how long every row will be...
  ;; (b) the first row is a header (so it's dropped!)
  ;; and (c) the CAR of each row is a key (also dropped!)
  (with-open-file (p file :direction :output :if-exists :supersede)
    (flet ((tab () (write-char #\Tab p)) 
	   (nl () (terpri p)))
      (let ((ncols (1- (length (pop table))))) ; Removes the header too!
	(loop for j from 1 to ncols do (format p "C~D" j) (tab))
        (nl)
        (loop for i from 0 by 1
	      as row in table
	      do 
              (format p "~D" (1+ i)) 
              (tab)
              (loop for j from 0 below ncols 
		    as elt in (cdr row) ; Skip the key.
		    do
                    (format p format elt)
                    (tab))
              (nl)
              )))))

(defun simple-r-format-to-data-array 
  (file &key (element-type 'single-float) &aux lines ndatacols array)
  (with-open-file (p file :direction :input :if-does-not-exist :error)
    (do ((line (read-line p nil nil) (read-line p nil nil))) ((null line))
      (push line lines))
    (setq lines (cdr (nreverse lines)))
    (with-input-from-string (p (first lines))
      (do ((j 0 (1+ j)) (item (read p nil nil) (read p nil nil)))
	  ((null item))
	(setq ndatacols j)))
    (setq array (make-array (list (length lines) ndatacols)
                            :element-type element-type))
    (loop for line in lines for i from 0 do
          (with-input-from-string (p line)
	    (do ((j -1 (1+ j)) (item (read p nil nil) (read p nil nil)))
                ((null item))
	      (setq item (coerce item element-type))
              (unless (minusp j) (setf (aref array i j) item)))))
    array
    ))
          
(defun r-do-d2 (commands)
  #.(one-string-nl
     "Pass in a single string representing the R code to execute; get back"
     "a single string representing the result (extracted by FILE-TO-STRING)"
     "Example: (r-do-d2 (one-string-nl \"x <- c(0:10,50)\" \"mean(x)\"))")
  (with-temp-directory 
   (dir "tmp:" :delete? *DELETE-TEMP-FILES*)
   (with-temp-file-in 
    (rout dir :name "rout" :delete? *DELETE-TEMP-FILES*)
    (with-temp-file-in 
     (rin dir :name "rin" :delete? *DELETE-TEMP-FILES*)
     (with-open-file 
      (rin rin :direction :output :if-exists :supersede)
      (format rin "~a" commands))
     (protected-shell-command 
      (format nil "~a/R --no-save < ~a > ~a" 
	      user::*R-executable-dir* rin rout))
     (file-to-string rout)))))

;;; Writes the data array of TABLE-DATA out to a file in
;;; a format that R can read.

;;; Creates an R script that reads in the data and calls the
;;; R kmeans clustering module, then writes the results out to 
;;; two files.

;;; Calls this R script.

;;; If all goes well we then read the output files created
;;; by R back into Lisp and put the data into *CENTERS* and
;;; *MEMBERSHIP*.

(defun r-cluster-kmeans 
  (table-data nclusters maxiters 
	      &key 
	      (table-data-format "~5,2F") 
	      (show-data-file? nil)
	      (show-rscript-file? t)
	      (verbose? nil)
	      &aux
	      (nl (string #\Newline))
	      (exit-status nil)
	      (centers-array nil)
	      (membership-array nil)
	      (ndatacols (1- (length (car table-data))))
	      )
  #.(one-string-nl
     "PLEASE DOCUMENT ME!"
     )
  ;; A bunch of temp files:
   (with-temp-file-in (in-rtable-file *tmp-directory* :type "rtable")
    (with-temp-file-in (in-rscript-file *tmp-directory* :type "rscript")
     (with-temp-file-in (out-centers-file *tmp-directory* :type "rcenters") 
      (with-temp-file-in (out-members-file *tmp-directory* :type "rmembers")
       ;; The functions that do the work (called later):
       (labels 
	   ((in-quotes (s) (s+ "\"" s "\""))
	    (create-rscript 
	     ()
	     (one-string
	      "library(cclust)" nl
	      "data<-read.table(" (in-quotes in-rtable-file) ")" nl
	      "mdata<-matrix(data.matrix(data),ncol=" 
	      (format nil "~D)" ndatacols) nl
	      "clusterinfo<-cclust(mdata," 
	      (format nil "~D,~D," nclusters maxiters) 
	      "verbose=FALSE,method=" (in-quotes "kmeans") ")" nl
	      "write.table(clusterinfo$cluster,file=" 
	      (in-quotes out-members-file) ",quote=FALSE)" nl
	      "write.table(clusterinfo$centers,file=" 
	      (in-quotes out-centers-file) ",quote=FALSE)" nl
	      ))
	    (write-r-table-data 
	     ()
	     (when verbose? 
	       (format t "~&;; Writing table data to ~A~%" in-rtable-file))
	     (table-data-to-simple-r-format 
	      table-data in-rtable-file :format table-data-format)
	     (when show-data-file? (list-file-contents in-rtable-file)))
	    (write-rscript 
	     (rscript)
	     (when verbose? 
	       (format t "~&;; Writing rscript to ~A~%" in-rscript-file))
	     (with-open-file (p in-rscript-file :direction :output :if-exists :supersede)
			     (format p "~A" rscript))
	     (when show-rscript-file? (list-file-contents in-rscript-file)))
	    (run-r-command 
	     ()
	     (when verbose? (format t "~&;; Running R clustering~%"))
	     (prog1
		 (cl-user::run-shell-command 
		  (format nil "~a/R --no-save < ~a" user::*R-executable-dir* in-rscript-file))
	       (when verbose? (format t "~&;; R clustering finished~%"))))
	    )
         ;; Actual execution:
	 (unwind-protect
	   (write-r-table-data)
	   (write-rscript (create-rscript))
	   (setq exit-status (run-r-command))
	   (when verbose? (format t "~&;; R exit status: ~D~%" exit-status))
	   (when (zerop exit-status)
	     (when verbose? 
	       (format t "~&;; Reading center result table from ~A~%" 
		       out-centers-file))
	     (setq centers-array
		   (simple-r-format-to-data-array out-centers-file))
	     (when verbose? 
	       (format t "~&;; Reading members result table from ~A~%" 
		       out-members-file))
	     (setq membership-array
		   (simple-r-format-to-data-array 
		    out-members-file :element-type t))))
	 ;; Pulling results together:
	 (let ((frame (make-temp-frame #$RTable)))
	   (setf (slotv frame #$Centers) centers-array)
	   (setf (slotv frame #$Membership) 
		 (2d-one-column-array-to-vector membership-array))
	   (setf (slotv frame #$Exit-Status) exit-status)
	   (setf (slotv frame #$Inputs) 
		 (list 'r-cluster-kmeans table-data nclusters maxiters))
	   (setf (slotv frame #$Rdatatype) #$Cclust)
	   (setf (slotv frame #$NClusters) nclusters)
	   (setf (slotv frame #$Data) table-data)
	   frame
	   )))))))

;; The MEMBERSHIP data is read in as a 2d array, but it is really
;; just of vector of cluster ids.  So turn it into a vector.
        
(defun 2d-one-column-array-to-vector (m)
  (let ((nrows (array-dimension m 0))
        (etype (array-element-type m)))
    (let ((vector (make-array nrows :element-type etype)))
      (loop for j from 0 below nrows do
	    (setf (aref vector j) (aref m j 0)))
      vector
      )))

;; MEMBERSHIP is a vector.  The value of MEMBERSHIP[j] is a cluster id
;; indicating to which cluster the Jth row of the TABLE was assigned.
;; The TABLE's first entry is the GENEID, as a string.  So this
;; returns a list of GENEID's belonging to the cluster.

(defun genes-in-cluster (table membership cluster-number)
  (loop for table-row in (rest table)
	for member across membership
	when (= cluster-number member)
	collect (car table-row)))

(defun genes-per-cluster (membership nclusters)
  (loop for j from 1 to nclusters collect (count j membership)))

;;; For each cluster, get the GENEID's that belong to it.  Then do
;;; some complicated analysis to see which of those GENEID's is mapped
;;; to various GO concepts, and keep track of how many GENEID's are
;;; mapped to the same concept.  Print out the concepts that are
;;; common to the most GENEID's, at increasing levels of abstraction.

(defun display-clusters (clustering-result)
  (format t "~%;; Displaying cluster means: ~%")
  (let* ((data (slotv clustering-result #$Data))
	 (membership (slotv clustering-result #$Membership))
	 (centers (slotv clustering-result #$Centers))
	 (nclusters (slotv clustering-result #$Nclusters))
	 (center (make-array (1- (length (car data))))))
    (loop for n in (genes-per-cluster membership nclusters) for j from 1 do
	  (matrix-row-slice center centers (1- j))
	  (format t "~&;; Cluster ~D, Center: ~A, Members: ~D~%"
		  j center n))
    (terpri)
    ))

;;; =========================================================================================

(defun r-anova-test ()
  (r-anova '(("mx" 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
	     ("tr" 2 2 2 2 0 0 0 0 1 1 1 1 2 2 2 2 0 0 0 0 1 1 1 1)
	     ("lr" 9.6 1.3 5.1 1.5 2.1 8.8 7.1 5.4 6.0 5.5 8.3 2.9 7.2 5.3 4.0 6.9 2.2 2.6 6.4 9.1 3.0 3.3 4.3 4.3))
	   "lr ~ mx + tr + mx*tr"))

(defun r-anova (data model)
  #.(one-string-nl
     "Runs an R-based ANOVA. The data must be in this form:"
     "    '((\"factor1name\" f11 f12 ...)" 
     "      (\"factor2name\" f21 f22 ...)"
     "      (\"observationname\" o1 o2 ...)"
     "      )"
     "The last factor is considered to be the observations."
     "The model must, for the nonce, be in R aov format, and name the"
     "same factors and data as you have given."
     "Here's a complete example:"
     "(r-anova '((\"mx\" 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)"
     "           (\"tr\" 2 2 2 2 0 0 0 0 1 1 1 1 2 2 2 2 0 0 0 0 1 1 1 1)"
     "           (\"lr\" 9.6 1.3 5.1 1.5 2.1 8.8 7.1 5.4 6.0 5.5 8.3 2.9 7.2 5.3 4.0 6.9 2.2 2.6 6.4 9.1 3.0 3.3 4.3 4.3))"
     "         \"lr ~ mx + tr + mx*tr\")"
     )
  (let* ((nl (string #\Newline)))
    (with-temp-directory (dir "tmp:" :delete? *DELETE-TEMP-FILES*)
     (with-temp-file-in (rout dir :name "rout" :delete? *DELETE-TEMP-FILES*)
      (with-temp-file-in (rin dir :name "rin" :delete? *DELETE-TEMP-FILES*)
	(with-open-file (rin rin :direction :output :if-exists :supersede)
	  (flet ((rvarout (var data)
		   (format rin "~a <- c(~a" var (car data))
		   (loop for d in (cdr data) do (format rin ",~a" d))
		   (format rin ")~%"))
		 )
	    (loop for (name . data) in data
		  do (rvarout name data)))
	  (format rin "mt <- data.frame(~a)~%"
		  (with-output-to-string (o)
		    (loop for (name . nil) in (cdr (reverse (copy-list data)))
			  do (format o "~a=factor(~a)," name name))
		    (let ((name (caar (last data))))
		      (format o "~a=~a" name name))))
	  (format rin "~a" 
		  (one-string
		   "(mt.aov <- aov(" model ", mt))" nl
		   "summary(mt.aov)" nl
		   "coefficients(mt.aov)" nl
		   "TukeyHSD(mt.aov)" nl))
	  )
	(cl-user::run-shell-command 
	 (format nil "~a/R --no-save < ~a > ~a" user::*R-executable-dir* rin rout))
	;; The result looks like:
	;; Note that the last col is not a number and can't be "read-from-string"!
        #|
							     > summary(mt.aov)
							     Df  Sum Sq Mean Sq F value Pr(>F)
							     mx           1   1.042   1.042  0.1498 0.7033 **
							     tr           2   2.331   1.165  0.1676 0.8470 ***
							     mx:tr        2  12.116   6.058  0.8712 0.4354 .
							     Residuals   18 125.170   6.954  
							     |#
	(with-open-file (rout rout)
	  ;; Skip to the summary
          (loop for line = (read-line rout nil nil)
                until (or (null line)
			  (and (> (length line) 9) (string-equal "> summary" (subseq line 0 9)))))
	  (read-line rout) ; skip header
	  (loop for line = (read-line rout nil nil)
		until (or (null line) 
			  (and (> (length line) 9) (string-equal "Residuals" (subseq line 0 9))))
		collect (let ((l (remove "" (string-split line) :test #'string-equal)))
			  (list :effect (pop l)
				(mapcan #'(lambda (label value)
					    (list label (ignore-errors (read-from-string value))))
					'(:df :ssq :msq :fval :pr :sig?) l))))
	  )
	)))))

