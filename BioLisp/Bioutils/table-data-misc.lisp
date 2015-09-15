;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bioutils; -*-

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

(defun describe-table-data (td &key (nrows 0))
  "Pretty print information about table TD, and NROWS (default 0) of its data"
  (format t "~&~%~%")
  (flet ((out (level format-string &rest format-args)
           (format t ";; ")
           (dotimes (j (* 2 level)) (format t " "))
           (apply #'format t format-string format-args)
           (terpri)
           ))
    (if (td-source-file td)
        (out 0 "Description of table object from file ~A:" (td-source-file td))
      (out 0 "Description of anonymous table object:"))
    (terpri)
    (when (td-name td) (out 1 "Table name: ~A" (td-name td)))
    (let ((headers (td-headers td)))
      (out 1 "Table has ~D fields, with names:" (td-ncols td))
      (loop for header in headers for column-number from 1 do
            (out 2 "Column ~2D: ~A" column-number header)))
    (terpri)
    (out 1 "There are ~D data columns and ~D other columns."
         (td-ndatacols td) (td-nothercols td))
    (out 2 "The data columns are: ~S." (td-datacols td))
    (out 2 "The other columns are: ~S." 
         (append (td-precols td) (td-postcols td)))
    (let* ((keys (td-keycols td))
           (nkeys (td-nkeycols td))
           (plural (if (> nkeys 1) "s" "")))
      (if (zerop nkeys)
          (out 1 "There are no key fields in this table.")
        (progn
          (out 1 "Table has ~D key field~A, in column~A ~A:"
               nkeys plural plural keys)
          (loop for key in keys for key-number from 1 do
                (out 2 "Key ~2D: ~A (in column ~D)" 
                     key-number (elt (td-headers td) key) key)))))
    (terpri)
    (out 1 "There are ~D rows of data in the table." (td-nrows td))
    (terpri)
    (if (and (td-data-type td) (not (eq t (td-data-type td))))
        (out 1 "Table data is of uniform type ~A." (td-data-type td))
      (out 1 "Table data is of varying type."))
    (if (plusp (td-missing-count td))
        (progn
          (out 1 "When table data was read in, ~D values were missing,"
               (td-missing-count td))
          (out 2 "The value ~S was used instead." 
               (td-missing-value td)))
      (out 1 "There are no missing data values in this table."))
    (terpri)
    (when (td-docs td)
      (out 0 "Table data documentation (~D lines):" (length (td-docs td)))
      (dolist (docline (td-docs td)) (out 1 "~A" docline))
      (terpri))
    (when (td-posthdrs td)
      (out 0 "Table data post header text (~D lines):" 
           (length (td-posthdrs td)))
      (dolist (phline (td-posthdrs td)) (out 1 " ~A" phline))
      (terpri))
    (when (plusp nrows)
      (let ((limit (min nrows (td-nrows td))))
        (out 0 "The first ~D rows of the table:" limit)
        (dotimes (j limit) (print (table-data-row td j))))
      (terpri)
      )))


  
(defun table-data-select 
       (td predicate 
           &key 
           (from :data)
           (return :row-indices)
           (tag nil)
           (index-predicate nil)
           &aux test-col return-col tag-col
           )

  #.(one-string-nl
     "Applies PREDICATE to part or all of each row of the TD table data object"
     "and returns part or all of each row (or the row index) that satisfies"
     "the test.  The results are gathered together in a list."
     "See the TABLE DATA documentation file for further details.")

  (let ((f 'table-data-select))

    (verify-td td f)
  
    ;; Determine what subset of each record that predicate
    ;; will be called with.

    (cond
     ((eq from :data) nil)
     ((eq from :other) nil)
     ((member from '(t :all :entire-record)) (setq from :all))
     ((or (stringp from) (symbolp from))
      (setq test-col (verify-string-names-column td (string from) f))
      (setq from :col))
     (t (error "~A:  Unknown value for FROM: ~S" f from))
     )

    ;; Determine what subset of each record that matches predicate
    ;; will be returned.

    (cond
     ((eq return :data) nil)
     ((eq return :other) nil)
     ((member return 
              '(:row-indices :row-indexes :row-index :index :row-number))
      (setq return :row-indices))
     ((member return '(t :all :entire-record :row :rows))
      (setq return :row))
     ((or (stringp return) (symbolp return))
      (setq return-col (verify-string-names-column td (string return) f))
      (setq return :col))
     (t (error "~A:  Unknown value for RETURN parameter: ~S" f return)))

    ;; Determine how to identify the values returned.

    (cond
     ((eq tag nil) (setq tag nil))
     ((member tag '(:row-indices :row-indexes :row-index :index :row-number
                    :row :rows :indices))
      (setq tag :row))
     ((or (stringp from) (symbolp from))
      (setq tag :col)
      (setq tag-col (verify-string-names-column td (string tag) f)))
     (t (error "~A:  Unknown value for TAG parameter: ~S" f return))
     )

    (let ((results nil) 
          (data-array (td-data-array td))
          (other-array (td-other-array td)))

      (flet ((tag-result (r j)
               (ecase tag
                 ((nil) (push r results))
                 (:row (push (list j r) results))
                 (:col 
                  (push (list (table-data-element-internal td j tag-col) r)
                        results
                        ))))
             (row-passes? (data j)
               (and (or (null index-predicate) (funcall index-predicate j))
                    (funcall predicate data)
                    )))

        ;; Doubly nested CASE on the FROM and RETURN possibilities.

        (ecase from
          (:data
           (loop for j fixnum from 0
                 for v across data-array do
                 (when (row-passes? v j)
                   (tag-result
                    (ecase return
                      (:data v)
                      (:other (aref other-array j))
                      (:row-indices j)
                      (:row (table-data-row td j :vector))
                      (:col (table-data-element-internal td j return-col)))
                    j))))
          (:other
           (loop for j fixnum from 0
                 for v across other-array do
                 (when (row-passes? v j)
                   (tag-result
                    (ecase return
                      (:data (aref data-array j))
                      (:other v)
                      (:row-indices j)
                      (:row (table-data-row td j :vector))
                      (:col (table-data-element-internal td j return-col)))
                    j))))
          (:all
           (loop for j fixnum from 0 below (td-nrows td) do
                 (let ((v (table-data-row td j :vector)))
                   (when (row-passes? v j)
                     (tag-result
                      (ecase return
                        (:data (aref data-array j))
                        (:other (aref other-array j))
                        (:row-indices j)
                        (:row v)
                        (:col (table-data-element-internal td j return-col)))
                      j)))))

          (:col
           (loop for j fixnum from 0 below (td-nrows td) do
                 (let ((datum (table-data-element-internal td j test-col)))
                   (when (row-passes? datum j)
                     (tag-result
                      (ecase return
                        (:data (aref data-array j))
                        (:other (aref other-array j))
                        (:row-indices j)
                        (:row (table-data-row td j :vector))
                        (:col (table-data-element-internal td j return-col)))
                      j)))))

          ))
      
    (nreverse results)

    )))
             

(defparameter *write-table-data-frames-as-fnames* t)
(defparameter *table-data-output-float-format* "~A")

(defun write-table-data 
       (td file &key 
           (write-docs? nil) 
           (write-posthdrs? nil) 
           (deframe? *write-table-data-frames-as-fnames*)
           (float-format *table-data-output-float-format*)
           (package (find-package :bio)))
  #.(one-string-nl
     "This writes the table data object to a file in such a way that"
     "READ-TABLE-DATA can read it back in.  "
     "See the TABLE DATA documentation file for further details.")
  (let ((f 'write-table-data)
        (*package* package)
        (*write-table-data-frames-as-fnames* deframe?)
        (*table-data-output-float-format* float-format)
        )
    (verify-td td f)
    (with-open-file (p file :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (when write-docs?
          (loop for docline in (td-docs td) do (format p "~A~%" docline)))
        (terpri p)
        ;; Write the tab-delimited headers.
        (do ((hdrlist (td-headers td) (cdr hdrlist)))
            ((null hdrlist))
          (format p "~A" (first hdrlist))
          (format p "~C" (if (cdr hdrlist) #\Tab #\Newline)))
        ;; No blank line after headers and before posthdr comments.
        (when write-posthdrs?
          (loop for posthdr in (td-posthdrs td) do (format t "~A~%" posthdr)))
        (terpri p)
        ;; Write out each row
        (loop for j fixnum from 0 below (td-nrows td) do
              (let ((row-data (table-data-row td j :list)))
                (do ((data row-data (cdr data)))
                    ((null data))
                  (write-table-data-element p (car data))
                  (format p "~C" (if (cdr data) #\Tab #\Newline))
                  )))
        ))))

(defmethod write-table-data-element ((p t) (datum t))
  (cond 
   ((isframe? datum) 
    (format p "~A" 
            (if *write-table-data-frames-as-fnames* 
                (fname datum) 
              datum)))
   (t
    (format p "~A" datum))))

(defmethod write-table-data-element ((p t) (datum float))
  (format p *table-data-output-float-format* datum))


      
