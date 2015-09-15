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

;;;; FASTA text files have the following form:

;;;; Each record (entry) starts with a line beginning with '>'
;;;; and the entry's DATA is composed of all following lines
;;;; up until the next line beginning with '>', or end of file.
;;;; Blank lines at the start of the FASTA text file are ignored.

;;;; Any text following on a line beginning with '>' constitutes
;;;; the NAME of the record and is used to compute the KEY.  By default
;;;; the key is taken to be a string composed of all characters up
;;;; to the first whitespace character, or the end of the line.
;;;; (It is possible to provide a key function which computes an
;;;; alternative KEY based on name).  It is an error if more than one
;;;; entry has the same KEY (and this applies across multiple FASTA files,
;;;; if they compose a single database with one overall index).

;;;; When a FASTA file is parsed, it produces a FASTA-INDEX.
;;;; See the data-structures.lisp file for documentation as to
;;;; the structure of a FASTA-INDEX.

(defparameter *fasta-whitespace-chars* '(#\Space #\Tab #\,))

(defun default-fasta-whitespace-function (ch) 
  (member ch *fasta-whitespace-chars* :test #'eql))

(defun default-fasta-key-function (line)
  (when (or (not (stringp line)) (zerop (length line)))
    (error "Internal error: FASTA index function given invalid line: ~S" line))
  (unless (eql (char line 0) #\>)
    (error "Internal error:  FASTA index function give invalid line: ~S" line))
  (unless (> (length line) 1)
    (fasta-file-format-error "No text after '>' to use as key"))
  (let ((pos (position-if #'default-fasta-whitespace-function line)))
    (if (null pos) 
        (subseq line 1)
      (progn
        (unless (> pos 1)
          (fasta-file-format-error "Whitespace immediately follows '>'"))
        (subseq line 1 pos)
        ))))

(defun parse-fasta-file 

       (file &key 
	     (key-function #'default-fasta-key-function)
	     (whitespace-function #'default-fasta-whitespace-function)
	     (warn-about-all-empty-lines nil)
	     (warn-about-empty-lines-within-entries t)
             (warn-about-no-data t)
             (verbose *fasta-verbose*)
	     )

  (strip-file-of-returns-preceding-newlines file)

  (with-open-file (f file :direction :input :if-does-not-exist :error)

    (let ((*fasta-verbose* verbose)
          (*fasta-file* (true-namestring file))
          (*fasta-file-linenum* 0)
          (found-first-entry nil)
          (datum-list nil)
          (record-count 0)
          (last-pos nil)
          ;; The data we accumulate for each record.
          startfp endfp data-startfp record-linecount
          sequence-length data-linelen record-line-sizes record-key
          )
      (when *fasta-verbose*
        (format t "~&;; Parsing fasta file ~A~%" *fasta-file*))

      (flet ((start-new-record ()
               (setq startfp nil endfp nil data-startfp nil)
               (setq record-linecount 0 sequence-length 0)
               (setq record-line-sizes nil record-key nil)
               )
             (add-line-info (line)
               (incf record-linecount)
               (incf sequence-length (length line))
               (push (length line) record-line-sizes)
               )
             (create-new-datum ()
               (incf record-count)
               (setq record-line-sizes (nreverse record-line-sizes))
               (multiple-value-setq (data-linelen record-line-sizes)
                   (determine-data-linelen-values record-line-sizes))
               (push
                (make-fasta-index-datum
                 record-key
                 (make-fri 
                  :startfp startfp 
                  :endfp endfp 
                  :data-startfp data-startfp
                  :data-linelen data-linelen 
                  :nlines record-linecount
                  :seqlen sequence-length 
                  :file-id nil
                  :line-sizes
                  (and record-line-sizes 
                       (coerce record-line-sizes 'vector))))
                datum-list
                )))

        ;; Read lines until EOF
        (start-new-record)
        (do ((line-start-filepos (file-position f) (file-position f))
             (line (read-line f nil nil) (read-line f nil nil)))
            ((null line) (setq last-pos line-start-filepos))
          (incf *fasta-file-linenum*)
          (cond
           ;; A blank line.  See what we should do about it.
           ((every whitespace-function line)
            (cond
             (warn-about-all-empty-lines
              (fasta-file-format-warning "Empty line encountered in file."))
             ((and warn-about-empty-lines-within-entries found-first-entry) 
              (fasta-file-format-warning "Empty line within entry found.")
              (add-line-info line))
             (t 
              (add-line-info line))
             ))
           ;; A line that doesn't begin an entry.
           ((not (eql (char line 0) #\>))
            (when (null found-first-entry)
              (fasta-file-format-error
               "The line of text ~S ~A" line
               "appears before the start of the first entry."))
            (add-line-info line)
            ;; If this is the first line of data for this record, 
            ;; record the file position so we know where the data
            ;; (along with the header) begins.
            (when (= record-linecount 1)
              (setq data-startfp line-start-filepos)))
           ;; A line that does begin an entry. 
           (t
            ;; Unless there is no previous entry, store the info
            ;; for the previous entry we've just completed reading.
            (unless (null found-first-entry) 
              (setq endfp line-start-filepos)
              (create-new-datum))
            (when (and found-first-entry 
                       (zerop record-linecount) 
                       warn-about-no-data)
              (fasta-file-format-warning "No data lines found for record."))
            (setq found-first-entry t)
            ;; Reset all the per-record information and get the key
            ;; for this new entry.
            (start-new-record)
            (setq startfp line-start-filepos)
            (setq record-key (funcall key-function line)))
           ))

        ;; End of file
        (when (null found-first-entry)
          (fasta-file-format-error 
	   "No entry (no line beginning with '>' found)."))
        (setq endfp last-pos)
        (create-new-datum)
        (when *fasta-verbose*
          (format t ";;   File parsed.  ~D records found.~%" record-count))

        ;; Check for duplicate keys.

        (when *fasta-verbose* (format t ";;   Checking for duplicate keys.~%"))
        (let ((duplicates nil) (count 0))
          (declare (ignore count))
          (setq duplicates 
                (check-for-duplicates 
                 datum-list :key 'fasta-index-datum-key :test 'string-equal
                 ))
          (when duplicates
            (format t "~&;; Duplicate keys found in fasta file!!~%")
            (loop for j fixnum from 1 for dup in duplicates do
                  (format t ";;    Duplicate key ~D: ~S~%" j dup))
            (error "Duplicate keys found.")
            ))
        (when *fasta-verbose* 
          (format t "~&;;   Checked for duplicate keys.~%"))

        (make-fasta-index *fasta-file* (reverse datum-list))

        ))))

(defun determine-data-linelen-values (linelen-list)
  (let ((len (length linelen-list)))
    (cond 
     ((= len 0) (values 0 nil))
     ((= len 1) (values (first linelen-list) nil))
     (t
      (let ((first-len (first linelen-list))
            (other-linelens-save-last (butlast (rest linelen-list) 1)))
        (if (every #'(lambda (x) (= x first-len)) other-linelens-save-last)
            (values first-len nil)
          (values nil linelen-list)
          ))))))
          



(defun fasta-file-format-error (text &rest args)
  (error "FASTA file format error.~%~A~%;; ~A~%"
	 (format nil ";; Line number: ~D, File name: ~A"
		 *fasta-file-linenum* *fasta-file*)
	 (apply #'format nil text args)))

(defun fasta-file-format-warning (text &rest args)
  (warn "FASTA file format note.~%~A~%;; ~A~%"
	(format nil ";; Line number: ~D, File name: ~A"
		*fasta-file-linenum* *fasta-file*)
	(apply #'format nil text args)))
      


