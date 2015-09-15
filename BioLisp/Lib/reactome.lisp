;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 The BioBike Team                                |
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

;;; Authors: Mark Slupesky, JP Massar, Jeff Shrager. 

(in-package :bio)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'create-reactome-frames :bio))

(defvar *pwys* nil)

(defvar *disallowed-frame-types* 
  '(instanceedit databaseidentifier))

(defvar *reactome-instance-frames* nil)
(defvar *reactome-defclass-frames* nil)
(defvar *reactome-slot-frames* nil)

(defun rtc (thing)
  (one-string "Rtc." (string thing)))

(defun remove-3a (symbol)
  (let ((string (string symbol)))
    (if (initial-subsequence-of? string "%3a" :element-test 'char-equal)
        (intern (subseq string 3))
      symbol)))

(defun create-reactome-frames 
       (&optional (dir (translate-simple-lp "bioetc:data;reactome;")))
  (ensure-directories-exist (merge-pathnames dir "foo.txt"))
  (clear-all-reactome-data-structures)
  (create-reactome-package)
  (process-pont-file dir)
  (process-pins-file dir)
  (destroy-reactome-package)
  (cformatt "All reactome data loaded successfully.")
  (cformatt "~D slot frames" (length *reactome-slot-frames*))
  (cformatt "~D defclass frames" (length *reactome-defclass-frames*))
  (cformatt "~D instance frames" (length *reactome-instance-frames*))
  (add-reactome-to-kdbs)
  (thread-reactome)
  )

(defun clear-all-reactome-data-structures ()
  (cformatt "Clearing Reactome data structures...")
  (setq *reactome-instance-frames* nil)
  (setq *reactome-defclass-frames* nil)
  (setq *reactome-slot-frames* nil)
  (cformatt "Reactome data structures cleared.")
  )

(defun create-reactome-package ()
  (when (not (find-package :reactome))
    (make-package :reactome)
    ))

(defun destroy-reactome-package ()
  (let ((p (find-package :reactome)))
    (do-symbols (s p) 
      (when (eq (symbol-package s) p)
        (unintern s)))
    (delete-package p)))


(defun process-pont-file (dir)
  (cformatt "Processing .pont file for deflcass slots...")
  (let ((*package* (find-package :reactome))
        (pont-file (merge-pathnames dir "reactome_15.pont")))
    (with-open-file (p pont-file)
      (loop for entry = (read p nil nil)
            until (null entry)
            as class = (when (and (listp entry) (eq 'defclass (first entry))) 
                         (second entry))
            when class 
            do
            (let ((frame (frame-fnamed 
                          (rtc (remove-3a (string-capitalize class))) t)))
              (when (member frame *reactome-defclass-frames*)
                (error "Multiple definitions for ~A" frame))
              (push frame *reactome-defclass-frames*)
              (setf (slotv frame #$rtc.defclass-form) entry)
              (keywordize-frame-data frame)
              ))))
  (cformatt ".pont file processed."))
          
(defvar *drop-reactome-frame-types* 
  '("InstanceEdit" "Person" "ReactionCoordinates" "Modified" "Figure" "affiliation"))

(defun process-pins-file (dir)
  (cformatt "Processing .pins file for instance slots...")
  (let ((*package* (find-package :reactome))
        (pins-file (merge-pathnames dir "reactome_15.pins"))
	(dropped 0) (kept 0))
    (with-open-file 
     (p pins-file)
     (loop for entry = (read p nil nil)
	   for j from 0 
	   until (null entry)
	   as label = (when (listp entry) (first entry))
	   as of = (when (listp entry) (third entry))
	   as of-frame = nil
	   do
	   (when (not label)
	     (error "Entry ~S in .pins file not a list." entry))
	   (unless (bracketsp label) 
	     (error "Entry name ~S is not a bracketed symbol." label))
	   (setq label (strip-brackets label))
	   (when (not (symbolp of))
	     (error "Entry OF specification, ~S, not a symbol." of))
	   (setq of (remove-3a of))
	   (when (zerop (mod j 1000)) (formatt "."))
	   (if (find of *drop-reactome-frame-types*
		     :test #'string-equal)
	       (incf dropped)
	     (let ((frame (frame-fnamed (rtc label) t)))
	       (incf kept)
	       (setf (slotv frame #$rtc.of) of)
	       (if (setq of-frame (frame-fnamed (rtc of)))
		   (progn
		     (push frame (slotv of-frame #$rtc.instances-of))
		     (setf (slotv frame #$rtc.of) of-frame))
		 (progn 
		   (warn "Of-slot ~A for frame ~A is not a defclass frame!" of frame)
		   (setq of-frame (frame-fnamed (rtc of) t))
		   (setf (slotv frame #$rtc.of) of-frame)
		   ))
	       (push frame *reactome-instance-frames*) 
	       (loop for (slot-name . slot-values) in (subseq entry 3) 
		     as slot-frame = (frame-fnamed (rtc slot-name) t) 
		     do 
		     (pushnew slot-frame *reactome-slot-frames*)
		     (if (null (cdr slot-values))
			 (setf (slotv frame slot-frame) (car slot-values))
		       (setf (slotv frame slot-frame) slot-values)))))
	   ))
    (cformatt "Processing slots of instances frames...")
    (loop for frame in *reactome-instance-frames* do
	  (for-each-frame-slot
	   (slot-frame slot-value) frame
	   (setf (slotv frame slot-frame) (process-slot-data slot-value frame))
	   ))
    (format t "Kept: ~a, Dropped ~a reactome frames.~%" kept dropped)
    )
  (cformatt ".pins file processed."))
  
;;; If the frame doesn't exist, do NOT automatically create it, but
;;; leave a :missing note. Many frames were not created per
;;; *drop-reactome-frame-types*

(defun process-slot-data (slot-value frame)
  (if (listp slot-value) 
      (mapcar (lambda (x) (process-slot-data x frame)) slot-value)
    (if (symbolp slot-value) 
        (if (bracketsp slot-value)
	    (or 
	     (frame-fnamed (rtc (strip-brackets slot-value)))
	     `(:missing ,slot-value))
	  (keywordize slot-value))
      slot-value)))

(defun strip-brackets (symbol)
  (let ((s (string symbol)))
    (read-from-string 
     (subseq s 1 (- (length s) 1)))))

(defun keywordize-frame-data (frame) 
  (labels ((keywordize-non-frame-data (slot-value)
             (cond
              ((symbolp slot-value) (keywordize slot-value))
              ((listp slot-value)
               (mapcar #'keywordize-non-frame-data slot-value))
              (t slot-value))))
    (for-each-frame-slot (slot-frame slot-value) frame
      (setf (slotv frame slot-frame) (keywordize-non-frame-data slot-value))
      )))

(defun bracketsp (symbol)
  (let ((s (string symbol)))
    (if (char= (aref s 0) #\[)
        (if (char= (lastelem s) #\])
            t
          (error "Symbol ~S begins with '[' but doesn't end with ']'"
                 symbol))
      (if (char= (lastelem s) #\])
          (error "Symbol ~S ends with ']' but doesn't begin with  '['"
                 symbol)
        nil))
    ))

(defun add-reactome-to-kdbs ()
  (pushnew :reactome *existing-kdb-names*))

(defmethod kdb-toplevel-frames ((kdb (eql :reactome)))
  (copy-list *reactome-defclass-frames*))

(defvar *goid->goframe* (simple-hash-table))

(defun thread-reactome ()
  (unless *pwys* (initpwys))
  (loop for rtcframe in *reactome-instance-frames*
	do 
	;; Threading to EC
	(let* ((ec (#^rtc.ecnumber rtcframe))
	       (ecframe (when ec (frame-fnamed (format nil "EC.~a" ec)))))
	  (when (and ec ecframe)
	    (setf (#^rtc.ecnumber rtcframe) ecframe)
	    (setf (#^rtc.object ecframe) rtcframe)))
	;; Threading to GO -- need to make a goid->goframes table first.
	(let* ((rdb (#^rtc.referencedatabase rtcframe)))
	  (when (and rdb (equal "GO" (#^rtc._displayname rdb)))
	    (let* ((goac (#^Rtc.ACCESSION rtcframe))
		   (goframe (when goac (goid->frame (parse-integer goac)))))
	      (when goframe
		(setf (#^rtc.goobj rtcframe) goframe)
		(setf (#^rtc.object goframe) rtcframe)))))
	))
