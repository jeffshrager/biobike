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

;;; Author: JP Massar.

;;;; RETRIEVE A RECORD OR A SUBSEQUENCE FROM AN OPEN FASTA FILE.
;;;; TOP-LEVEL ROUTINES:

;;; FIND-FASTA-RECORD
;;; FIND-FASTA-SUBSEQUENCE

;;; FIND-FASTA-RECORD returns a list of two things, which by default
;;;  are the header string and the sequence string.

(defun find-fasta-record  
       (db-handle 
        key 
        &key 
        (header-function 'default-fasta-header-func)
        (line-function 'default-fasta-line-func)
        (join-function 'default-fasta-join-func)
        )
  #.(one-string-nl
     "Find the record whose key is KEY in the FASTA database associated "
     "with DB-HANDLE. Returns a list of the header string and the data string."
     "See the FASTA API document for more details on all FASTA functions.")
  (let* ((database-ht (fastadb-index db-handle))
         (database-files (fastadb-files db-handle))
         (fri (gethash key database-ht)))
    ;; If the key was not found, return NIL.
    (when fri
      ;; Here is where we could play with stream caching.
      ;; (No need to get file and open it if corresponding stream existed)
      (let* ((fileindex (fri-file-id fri))
             (filename (aref database-files fileindex))
             (startfp (fri-startfp fri))
             (endfp (fri-endfp fri)))
        (with-open-file 
            (f filename :direction :input :if-does-not-exist :error)
          (find-fasta-record-internal 
           f startfp endfp header-function line-function join-function
           ))))))
       

(defun find-fasta-header (db-handle key)
  (let* ((database-ht (fastadb-index db-handle))
         (database-files (fastadb-files db-handle))
         (fri (gethash key database-ht)))
    ;; If the key was not found, return NIL.
    (when fri
      (let* ((fileindex (fri-file-id fri))
             (filename (aref database-files fileindex))
             (startfp (fri-startfp fri)))
        (with-open-file 
            (f filename :direction :input :if-does-not-exist :error)
          (unless (file-position f startfp)
            (error "Could not position stream ~S at position ~D" f startfp))
          (let ((line (read-line f nil nil)))
            (when (null line)
              (error "Hit EOF reading header of fasta file!"))
            (subseq line 1)
            ))))))

;;; FIND-FASTA-SUBSEQUENCE returns three values:
;;;   The string representing the subsequence
;;;   The direction the subsequence was read
;;;   Whether the subsequence is wrapped.

(defun find-fasta-subsequence (db-handle key from to direction)
  #.(one-string-nl
     "Retrieves a substring of the data string whose key is KEY in "
     "the FASTA database associated with DB-HANDLE. "
     "See the FASTA API document for more details on all FASTA functions.")
  (let* ((database-ht (fastadb-index db-handle))
         (database-files (fastadb-files db-handle))
         (fri (gethash key database-ht)))
    ;; If the key was not found, return NIL.
    (when fri
      (let* ((fileindex (fri-file-id fri))
             (filename (aref database-files fileindex)))
        (with-open-file 
            (f filename :direction :input :if-does-not-exist :error)
          (find-fasta-subsequence-internal f fri from to direction)
          )))))
       

(defun find-fasta-record-internal
       (stream startfp endfp header-function line-function join-function)
  (let ((data-lines nil))
    (unless (file-position stream startfp)
      (error "Could not position stream ~S at position ~D" stream startfp))
    (do ((pos (file-position stream) (file-position stream))
         (line (read-line stream nil nil) (read-line stream nil nil)))
        ((or (null line) (= pos endfp)))
      (when (> pos endfp)
        (error "Internal error.  ENDFP ~D not at start of line!!" endfp))
      (push (funcall line-function line) data-lines))
    (setq data-lines (nreverse data-lines))
    (list (funcall header-function (first data-lines))
          (funcall join-function (rest data-lines))
          )))

(defun find-fasta-subsequence-internal (stream fri from to direction)
  ;; FROM and TO are 1-based, not 0-based.
  ;; Convert to sequence indices, which are 0-based.
  (let ((seqlen (fri-sequence-length fri))
        (from-si (1- from))
        (to-si (1- to)))
    (cond
     ;; No wrapping
     ((<= from to)
      (let* ((chars-to-read (1+ (- to-si from-si)))
             (sequence-string (make-string chars-to-read)))
        (set-fasta-fp-from-sequence-index stream fri from-si)
        (read-fasta-subsequence-into-buffer 
         stream sequence-string 0 chars-to-read)
        (ecase direction
          ((:forward :forwards) 
           (values sequence-string :forward nil))
          ((:backward :backwards) 
           (values (nreverse sequence-string) :backward nil))
          )))
     ;; Wrapping
     (t
      (let* ((end-subseq-length (- seqlen from-si))
             (start-subseq-length (1+ to-si))
             (total-length (+ end-subseq-length start-subseq-length))
             (sequence-string (make-string total-length))
             )
        ;; Read until end of sequence
        (set-fasta-fp-from-sequence-index stream fri from-si)
        (read-fasta-subsequence-into-buffer
         stream sequence-string 0 end-subseq-length)
        ;; Read from start of sequence
        (set-fasta-fp-from-sequence-index stream fri 0)
        (read-fasta-subsequence-into-buffer
         stream sequence-string end-subseq-length start-subseq-length)
        (ecase direction
          ((:forward :forwards)
           (values sequence-string :forward t))
          ((:backward :backwards)
           (values (nreverse sequence-string) :backward t))
          ))))))


(defun set-fasta-fp-from-sequence-index (stream fri si)
  (let ((pos (file-position stream (fasta-fp-from-sequence-index fri si))))
    (or pos
        (error "Setting FILE-POSITION on fasta file stream ~A failed!" stream)
        )))

;;; A sequence index starts at 0.

;;; Neither file positions nor sequence indices are guarenteed to
;;; be fixnums!

;;; If the DATA-LINELEN field is NIL, it means each line is of different
;;; length.

(defun fasta-fp-from-sequence-index (fri si)
  (let ((seqlen (fri-sequence-length fri)))
    (unless (and (integerp si) (not (minusp si)))
      (error "Invalid sequence index: ~A.  Must be non-negative integer." si))
    (unless (< si seqlen)
      (error "Out of range index: ~A.  Sequence is only ~D long." si seqlen))
    (cond
     ((fri-data-linelen fri) 
      (fasta-fp-from-constant-linelen-record fri si))
     (t 
      (fasta-fp-from-varying-linelen-record fri si))
     )))

(defun fasta-fp-from-constant-linelen-record (fri si)
  (let* ((startfp (fri-data-startfp fri))
         (linelen (fri-data-linelen fri))
         (lineskip (+ linelen newline-file-position-increment)))
    (multiple-value-bind (lines-to-skip offset) 
        (floor si linelen)
      (+ startfp (* lineskip lines-to-skip) offset)
      )))

(defun fasta-fp-from-varying-linelen-record (fri si)
  (block exit
    (let* ((startfp (fri-data-startfp fri))
           (line-sizes (fri-data-line-sizes fri))
           (current-offset startfp))
      (unless (vectorp line-sizes)
        (error "Internal error.  Line-sizes is supposed to be a vector."))
      (loop for line-size across line-sizes do
            (cond
             ((< si line-size) 
              (return-from exit (+ current-offset si)))
             (t
              (decf si line-size)
              (incf current-offset line-size)
              (incf current-offset newline-file-position-increment)
              )))
      (error "Internal error.  SI exceeds sum of line-sizes.")
      )))
    

(defun read-fasta-subsequence-into-buffer 
       (stream buffer start-index chars-to-read)
  (declare (simple-string buffer))
  (declare (fixnum start-index chars-to-read))
  (let ((chars-read 0) (index start-index))
    (declare (fixnum chars-read index))
    (do ((ch (read-char stream nil nil) (read-char stream nil nil)))
        ((= chars-read chars-to-read))
      (cond
       ((null ch)
        (error "Internal error.  Hit EOF reading subsequence of fasta file!"))
       ((not (eql ch #\Newline))
        (setf (schar buffer index) ch)
        (setq chars-read (the fixnum (1+ chars-read)))
        (setq index (the fixnum (1+ index)))
        )))))


;;; Functions which define how the data in a FASTA record
;;; is put together and processed.  By passing different functions
;;; in to FIND-FASTA-RECORD you can control this processing.

;;; The default processing is to return the header line as a single
;;; string without the leading '>', to not do any processing on 
;;; individual data lines, and to join the data lines together
;;; into a single string.
                 
(defun default-fasta-header-func (header-line) (subseq header-line 1))
(defun default-fasta-line-func (line) line)
(defun default-fasta-join-func (lines &aux (pos 0))
  (let* ((result (make-string (loop for line in lines sum (length line)))))
    (loop for line in lines do
          (replace result line :start1 pos) 
          (incf pos (length line))
          finally (return result))))

           
(defun fastadb-record-info (db-handle key)
  (gethash key (fastadb-index db-handle)))
(defun fasta-record-data-length (db-handle key)
  (fri-sequence-length (fastadb-record-info db-handle key)))
(defun fasta-record-data-line-size (db-handle key)
  (fri-data-linelen (fastadb-record-info db-handle key)))
(defun fasta-record-data-format (db-handle key) 
  (if (fasta-record-data-line-size db-handle key)
      :fixed-width :variable-width))
(defun fasta-record-data-line-sizes (db-handle key)
  (fri-data-line-sizes (fastadb-record-info db-handle key)))
(defun fasta-record-data-nlines (db-handle key)
  (fri-data-nlines (fastadb-record-info db-handle key)))
(defun fasta-record-data-last-line-size (db-handle key)
  (let ((line-size (fasta-record-data-line-size db-handle key)))
    (if (null line-size)
        (let ((sizes (fasta-record-data-line-sizes db-handle key)))
          (aref sizes (1- (length sizes))))
      (let ((chars-in-all-but-last-line
             (* (1- (fasta-record-data-nlines db-handle key)) line-size)))
        (- (fasta-record-data-length db-handle key)
           chars-in-all-but-last-line
           )))))

