;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bioutls; -*-

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

;;; Author: JP Massar

;;; Auxiliary functions to do conversions and error checking.

(defun verify-td (td f)
  (unless (typep td 'table-data) 
    (error "~A: Not a table data object: ~A" f td))
  td)

(defun verify-key (key f)
  (unless (typep key 'table-key) 
    (error "~A: Not a valid table-key: ~A" f key))
  key)

(defun verify-index-within-limit (index limit mode)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (integerp index)
    (let ((val index))
      (declare (fixnum val limit))
      (and (not (minusp val))
           (ecase mode
             (:inclusive (<= val limit))
             (:exclusive (< val limit)))))))

(defun verify-row (td row mode f)
  (let ((nrows (td-nrows td)))
    (unless (verify-index-within-limit row nrows mode)
      (error "~A: Invalid row index: ~D.  Must be in ~A range (0,~D)"
             f row mode nrows)))
  row)
                    
(defun complete-key-to-row 
       (td key f &optional (multiple-ok? nil))
  (let ((table (table-key-table key))
        (col (table-key-column-index key))
        (rows (table-key-rows key)))
    (unless (eq td table)
      (error "~A: Row designator is a key for table ~A, ~A, ~A" f
             table "but not a key for the table being operated on, " td))
    (unless (verify-index-within-limit col (td-ncols td) :exclusive)
      (error "~A: *> Internal error <*.  Key column-index invalid: ~A" f col))
    (dolist (row rows)
      (unless (verify-index-within-limit row (td-nrows td) :exclusive)
        (error "~A: *> Internal error <*.  Key row invalid: ~A" f row)))
    (cond
     (multiple-ok? (copy-list rows))
     ((and (not multiple-ok?) (= 1 (length rows))) 
      (first rows))
     (t
      (error "~A: More than one row, (rows ~A), satisfy the key." f rows))
     )))

(defun incomplete-key-to-complete-key (td key f)
  (if (table-key-complete? key) 
      key
    (progn
      (cond
       ((table-key-column-designator key)
        (setf (table-key-column-index key)
              (verify-string-names-key-column 
               td (table-key-column-designator key) f))
        (setf (table-key-rows key)
              (table-data-key-row-indices-internal 
               td (table-key-value key) (table-key-column-index key)))
        (when (null (table-key-rows key))
          (error "~A: Key value ~A is not present in column ~A of table"
                 f (table-key-value key) (table-key-column-designator key))))
       (t
        (multiple-value-bind (rows col)
            (find-key-rows-without-col td (table-key-value key))
          (when (null rows)
            (error "~A: Key value ~S cannot be located ~A"
                   f (table-key-value key) 
                   "in any of the key columns of the table"))
          (setf (table-key-column-designator key) (nth col (td-headers td)))
          (setf (table-key-column-index key) col)
          (setf (table-key-rows key) rows)
          )))
      (setf (table-key-table key) td)
      (setf (table-key-complete? key) t)
      key
      )))

(defun row-designator-to-row (td rd f)
  (cond
   ((integerp rd) (verify-row td rd :exclusive f))
   ((table-key-p rd)
    (complete-key-to-row
     td (incomplete-key-to-complete-key td rd f) f nil))
   (t (row-designator-to-row td (table-key rd) f))))

(defun row-designator-to-rows (td rd f)
  (cond
   ((integerp rd) (list (verify-row td rd :exclusive f)))
   ((table-key-p rd)
    (complete-key-to-row
     td (incomplete-key-to-complete-key td rd f) f t))
   (t (row-designator-to-rows td (table-key rd) f))))

(defun row-designators-to-rows (td rds f)
  (if (not (typep rds 'sequence))
      (row-designator-to-rows td rds f)
    (mapcan #'(lambda (rd) (row-designator-to-rows td rd f))
            (if (listp rds) rds (coerce rds 'list))
            )))
  
(defun verify-col (td col mode f)
  (let ((ncols (td-ncols td)))
    (unless (verify-index-within-limit col ncols mode)
      (error "~A: Invalid column index: ~D.  Must be in ~A range (0,~D)"
             f col mode ncols)))
  col)

(defun verify-data-col (td col f mode)
  (let ((ncols (td-ndatacols td)))
    (unless (verify-index-within-limit col ncols mode)
      (error "~A: Invalid data column index: ~D.  Must be in ~A range (0,~D)"
             f col mode ncols)))
  col)

(defun verify-row-range (rs re f)
  (declare (fixnum rs re))
  (unless (not (minusp (the fixnum (- re rs))))
    (error "~A: Invalid row range (~D,~D).  End must not be < start."
           f rs re)))

(defun verify-col-range (rs re f)
  (declare (fixnum rs re))
  (unless (not (minusp (the fixnum (- re rs))))
    (error "~A: Invalid column range (~D,~D).  End must not be < start."
           f rs re)))

(defun header-to-column-index (td header-string)
  (position header-string (td-headers td) :test #'string-equal))
  
(defun verify-string-names-column (td string f)
  (let ((column-index (header-to-column-index td string)))
    (or column-index
      (error "~A: No column named ~S exists in table ~A" f string td))))

(defun verify-string-names-data-column (td string f)
  (let ((column-index (header-to-column-index td string)))
    (unless (and column-index (member column-index (td-datacols td)))
      (error "~A: No data column named ~S exists in table ~A" f string td))
    (- column-index (td-nprecols td))))

(defun column-designator-to-column (td cd f)
  (cond
   ((integerp cd) (verify-col td cd :exclusive f))
   ((stringp cd) (verify-string-names-column td cd f))
   (t (error "~A: Invalid column designator: ~A" f cd))
   ))

(defun column-designator-to-data-column (td cd f)
  (cond
   ((integerp cd) (verify-data-col td cd f :exclusive))
   ((stringp cd) (verify-string-names-data-column td cd f))
   (t (error "~A: Invalid data column designator: ~A" f cd))
   ))

(defun column-designators-to-columns (td seq f)
  (map 'list #'(lambda (cd) (column-designator-to-column td cd f)) seq))

(defun column-designators-to-data-columns (td seq f)
  (map 'list #'(lambda (cd) (column-designator-to-data-column td cd f)) seq))


(defun table-key (value &optional (column-designator nil) (table nil))
  #.(one-string-nl
     "Creates a table key.  See the TABLE DATA documentation file for "
     "detailed information. ")
  (let ((f 'table-key))
    (when table (verify-td table f))
    (cond
     ((and column-designator table)
      (let* ((column-index 
              (column-designator-to-column table column-designator f))
             (rows 
              (table-data-key-row-indices-internal table value column-index)))
        (when (null rows)
          (error "~A: Key value ~A is not present in column ~A of table"
                 f value column-designator))
        (make-table-key 
         :value value
         :rows rows
         :column-index column-index
         :column-designator column-designator
         :table table
         :complete? t
         )))
     (column-designator
      (make-table-key
       :value value :column-index nil 
       :column-designator column-designator
       :table nil :complete? nil
       ))
     (t
      (make-table-key
       :value value :column-index nil :column-designator nil :table nil
       :complete? nil
       )))))
    
(defun table-keys (value &rest values) 
  #.(one-string-nl
  "Returns a list of table keys, created from VALUE and any VALUES."
  "See the TABLE DATA documention file for detailed information.")
  (mapcar #'table-key (cons value values)))


;;; Functions to access table meta-information.


(defun table-data-documentation (td) 
  "Returns documentation, if any, read from the original table file"
  (td-docs (verify-td td 'table-data-documentation)))

(defun table-data-headers (td) 
  "Returns the names of all the columns of the table"
  (td-headers (verify-td td 'table-data-headers)))

(defun table-data-header (td col)
  "Returns the name of the column indexed by COL"
  (let ((f 'table-data-header))
    (verify-col (verify-td td f) col :exclusive f)
    (elt (td-headers td) col)))

(defun table-data-post-header-info (td)
  #.(one-string
     "Returns the contents of the original table file found between the "
     "column header row and the first data row, if any.  See the TABLE DATA "
     "documentation for more information.")
  (td-posthdrs (verify-td td 'table-data-post-header-info)))

(defun table-data-keycols (td)
  "The names of all the columns which serve as keys"
  (verify-td td 'table-data-keycols)
  (mapcar #'(lambda (col) (table-data-header td col)) (td-keycols td)))

(defun table-data-nrows (td) 
  "Number of rows in table."
  (td-nrows (verify-td td 'table-data-nrows)))
(defun table-data-ncols (td) 
  "Number of columns in table."
  (td-ncols (verify-td td 'table-data-ncols)))
(defun table-data-ndatacols (td) 
  "Number of data columns (as distinct from other columns) in table."
  (td-ndatacols (verify-td td 'table-data-ndatacols)))
(defun table-data-nothercols (td)
  "Number of non-data columns in table."
  (td-nothercols (verify-td td 'table-data-nothercols)))


;;; Functions to access individual cells of the table


(defun table-data-element-internal (td row col)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type table-data td))
  (declare (fixnum row col))
  (let* ((nprecols (td-nprecols td)) 
         (ndatacols (td-ndatacols td))
         (post-start (the fixnum (+ nprecols ndatacols))))
    (declare (fixnum nprecols ndatacols post-start))
    (cond
     ((< col nprecols)
      (aref (aref (td-other-array td) row) col))
     ((< col post-start)
      (aref (aref (td-data-array td) row) (the fixnum (- col nprecols))))
     (t
      (aref (aref (td-other-array td) row) (the fixnum (- col ndatacols)))
      ))))

(defun table-data-element (td row col)
  "The datum in the table indexed by ROW and COL"
  (let ((f 'table-data-element))
    (table-data-element-internal
     (verify-td td f) 
     (row-designator-to-row td row f)
     (column-designator-to-column td col f))))

;; Either multiple rows, or multiple columns, but not both.
(defun table-data-elements-internal (td rows cols)
  (cond
   ((null (cdr rows))
    (mapcar #'(lambda (c) (table-data-element-internal td (car rows) c)) cols))
   ((null (cdr cols))
    (mapcar #'(lambda (r) (table-data-element-internal td r (car cols))) rows))
   (t (error "Internal error.  Rows and cols cannot both be lists."))
   ))
      
(defun table-data-elements (td rows cols)
  #.(one-string-nl
     "One of ROWS or COLUMNS must be singular.  If ROWS is singular the data "
     "in each of COLS for that row is returned as a list.  If COLS is "
     "singular data for that COL in each of the ROWS is returned as a list.")
  (flet ((seq? (x) (typep x 'sequence)))
    (let ((f 'table-data-elements)
          (many-cols? (and (seq? cols) (not (stringp cols)))))
      (verify-td td f)
      (when (and (seq? rows) many-cols?)
        (error "~A: Cannot specify both multiple rows and multiple cols." f))
      (when (not (seq? rows)) (setq rows (list rows)))
      (let ((ris (mapcan #'(lambda (r) (row-designator-to-rows td r f)) rows)))
        (when (and (cdr ris) many-cols?)
          (error 
           "~A: Cannot specify both multiple rows and multiple cols. ~A" f
           (format nil "(The key, ~A, mapped to multiple rows)" (first rows))))
        (when (not many-cols?) (setq cols (list cols)))
        (table-data-elements-internal 
         td ris 
         (mapcar #'(lambda (c) (column-designator-to-column td c f)) cols)
         )))))


;;; Functions that retrieve particular rows or columns from the data array.

(defun table-data-data-row (td row)
  "Retrieves a row from the data array portion of the table as a vector"
  (let ((f 'table-data-data-row))
    (verify-td td f)
    (setq row (row-designator-to-row td row f))
    (aref (td-data-array td) row)
    ))

(defun table-data-data-rows (td rows)
  "Returns rows from the data array portion of the table as a list of vectors"
  (let ((f 'table-data-data-rows))
    (verify-td td f)
    (mapcar #'(lambda (row) (table-data-data-row td row)) 
         (row-designators-to-rows td rows f))))

(defun table-data-data-col (td column-designator)
  "Returns a column from the data array portion of the table as a vector"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((f 'table-data-data-col) (col 0))
    (declare (fixnum col))
    (verify-td td f)
    (setq col (column-designator-to-data-column td column-designator f))
    (let* ((size (td-nrows td))
           (array (td-data-array td))
           (col-vector (make-array size :element-type (td-data-type td))))
      (declare (fixnum col size))
      (loop for i fixnum below size do
            (setf (aref col-vector i) (aref (aref array i) col)))
      col-vector
      )))

(defun table-data-data-cols (td column-designators)
  "Returns columns from the data array portion of table as a list of vectors"
  (map 'list #'(lambda (c) (table-data-data-col td c)) column-designators))


;;; Routines to handle KEYS, KEY COLUMNS and their HASH TABLES


(defun maybe-create-hash-for-key-column (td col)
  (when (null (td-hashes td))
    (setf (td-hashes td) (make-array (td-ncols td) :initial-element nil)))
  (let ((hash-array (td-hashes td)))
    (when (null (aref hash-array col))
      (setf (aref hash-array col) 
            (make-hash-table :test #'equal :size (td-nrows td)))
      (let ((ht (aref hash-array col)))
        ;; Loop over the rows backards so PUSH will put smallest
        ;; row indices at the beginning of the list.
        (loop for row fixnum from (1- (td-nrows td)) downto 0 do
              (push row (gethash (table-data-element-internal td row col) ht))
              )))))

(defun hash-for-key-column (td col)
  (maybe-create-hash-for-key-column td col)
  (aref (td-hashes td) col))

(defun table-data-key-row-indices-internal (td key col)
  (copy-list (gethash key (hash-for-key-column td col))))

(defun find-key-rows-without-col (td key)
  (loop for col in (td-keycols td) 
        as rows = (table-data-key-row-indices-internal td key col) do
        (when rows (return (values rows col)))
        finally (return nil)
        ))

(defun verify-string-names-key-column (td string f)
  (let ((column-index (verify-string-names-column td string f)))
    (unless (member column-index (td-keycols td) :test #'=)
      (error "~A: The column named ~A was not designated a KEY." f string))
    column-index
    ))

(defun table-data-key-present? (td key)
  "Returns true if KEY is a valid key for the table TD"
  (if (typep key 'table-key)
      (let ((f 'table-data-key-present?))
        (verify-td td f)
        (verify-key key f)
        (multiple-value-bind (result error?)
            (ignore-errors (row-designator-to-rows td key f))
          (if error? nil result)
          ))
    (table-data-key-present? td (table-key key))))
      
(defun table-data-key-row-indices (td key)
  "Returns the list of rows that KEY indexes to in the table"
  (if (typep key 'table-key)
      (let ((f 'table-data-key-rows))
        (verify-td td f)
        (verify-key key f)
        (row-designator-to-rows td key f))
    (table-data-key-row-indices td (table-key key))))
      
(defun vector-of-vectors-to-2d-array (vv)
  (let ((nrows (length vv)))
    (if (zerop nrows)
        (make-array '(0 0))
      (let* ((first-vector (aref vv 0))
             (ncols (length first-vector))
             (array (make-array (list nrows ncols) 
                                :element-type
                                (array-element-type first-vector))))
        (loop for v across vv for row fixnum from 0 do
              (loop for col fixnum from 0 below ncols do
                    (setf (aref array row col) (aref v col))
                    ))
        array
        ))))

(defun table-data-data-array (td &key (as :vv))
  #.(one-string-nl
     "Returns the data array portion of the table.  If AS is :MATRIX, the "
     "data is returned as a two-dimensional-array, otherwise it is returned "
     "as a vector of vectors.")
  (verify-td td 'table-data-data-array)
  (ecase as
    ((:vv nil :default :vector-of-vectors) (td-data-array td))
    ((:matrix :2d-array) 
     (vector-of-vectors-to-2d-array (td-data-array td)))))
      
(defgeneric table-data-data-subarray (td rows cols &key as &allow-other-keys)
  (:documentation
   #.(one-string-nl
      "Returns a subarray of the data array portion of the table. The subarray"
      "is bounded by START-ROW, START-COL, END-ROW and END-COL.  The data "
      "can be returned as a 2d array or a vector of vectors.")))

(defmethod table-data-data-subarray 
           ((td table-data) (start-row integer) (start-col integer)
            &key (end-row nil) (end-col nil) (as :vv))
  (unless end-row (setq end-row (td-nrows td)))
  (unless end-col (setq end-col (td-ndatacols td)))
  (let ((f 'table-data-data-subarray))
    (verify-row td start-row :exclusive f)
    (verify-col td start-col :exclusive f)
    (verify-row td end-row :inclusive f)
    (verify-col td end-col :inclusive f)
    (verify-row-range start-row end-row f)
    (verify-col-range start-col end-col f)
    (let* ((nrows (- end-row start-row))
           (ncols (- end-col start-col))
           (array (table-data-data-array td))
           (type (td-data-type td))
           )
      (ecase as
        ((:matrix :2d-array)
         (let ((subarray (make-array (list nrows ncols) :element-type type)))
           (loop for i fixnum from 0 
                 for row fixnum from start-row below end-row do
                 (let ((row-vector (aref array row)))
                   (loop for j fixnum from 0
                         for col fixnum from start-col below end-col do
                         (setf (aref subarray i j) (aref row-vector col))
                         )))
           subarray))
        ((:vv nil :default :vector-of-vectors)
         (let ((vv (make-array nrows)))
           (loop for j fixnum below nrows do
             (setf (aref vv j) (make-array ncols :element-type type)))
           (loop for new-row-vector across vv
                 for row fixnum from start-row below end-row do
                 (let ((old-row-vector (aref array row)))
                   (loop for j fixnum from 0
                         for col fixnum from start-col below end-col do
                         (setf (aref new-row-vector j)
                               (aref old-row-vector col)
                               ))))
           vv))))))

(defmethod table-data-data-subarray
           ((td table-data) (rows sequence) (cols sequence) &key (as :vv))
  (let* ((f 'table-data-data-subarray)
         (row-indices (row-designators-to-rows td rows f))
         (nrows (length row-indices))
         (col-indices (column-designators-to-data-columns td cols f))
         (ncols (length col-indices))
         (type (td-data-type td))
         (array (table-data-data-array td)))
    (ecase as
      ((:matrix :2d-array)
       (let ((subarray (make-array (list nrows ncols) :element-type type)))
         (loop for i fixnum from 0
                   for row fixnum in row-indices do
                   (let ((row-vector (aref array row)))
                     (loop for j fixnum from 0 
                           for col fixnum in col-indices do
                           (setf (aref subarray i j) (aref row-vector col))
                           )))
         subarray))
      ((:vv nil :default :vector-of-vectors)
       (let ((vv (make-array nrows)))
         (loop for row fixnum below nrows do
             (setf (aref vv row) (make-array ncols :element-type type)))
         (loop for new-row-vector across vv
               for row fixnum in row-indices do
               (let ((old-row-vector (aref array row)))
                 (loop for j fixnum from 0
                       for col fixnum in col-indices do
                       (setf (aref new-row-vector j) (aref old-row-vector col))
                       )))
         vv)))))
      
(defun table-data-row (td row &optional (as :vector))
  "Returns a row of the table as either a vector or a list"
  (let ((f 'table-data-row))
    (verify-td td f)
    (setq row (row-designator-to-row td row f))
    (let ((size (td-ncols td)))
      (ecase as
        (:list
         (loop for col fixnum below size collect 
               (table-data-element-internal td row col)))
        (:vector
         (let ((v (make-array size)))
           (loop for col fixnum below size do
                 (setf (aref v col) (table-data-element-internal td row col))
                 finally (return v)
                 )))))))

(defun table-data-rows (td rows &optional (as :list))
  #.(one-string-nl
     "Returns a list of rows of the table. Each row is returned either as a "
     "list or a vector.")
  (let ((f 'table-data-rows))
    (verify-td td f)
    (setq rows (row-designators-to-rows td rows f))
    (mapcar #'(lambda (row) (table-data-row td row as)) rows)))

(defun table-data-col (td column-designator &optional (as :list))
  "Returns a column of the table as either a list or a vector"
  (let ((f 'table-data-col) (col nil))
    (verify-td td f)
    (setq col (column-designator-to-column td column-designator f))
    (let ((nrows (td-nrows td)))
      (ecase as
        (:vector
         (let ((col-vector (make-array nrows)))
           (declare (fixnum col nrows))
           (loop for row fixnum below nrows do
            (setf (aref col-vector row) 
                  (table-data-element-internal td row col)))
           col-vector
           ))
        (:list
         (loop for row fixnum below nrows collect
               (table-data-element-internal td row col)))))))

(defun table-data-cols (td cols &optional (as :vector))
  #.(one-string-nl
     "Returns a list of columns of the table.  Each column is returned "
     "either as a list or a vector.")
  (let ((f 'table-data-cols))
    (verify-td td f)
    (setq cols (column-designators-to-columns td cols f))
    (mapcar #'(lambda (col) (table-data-col td col as)) cols)
    ))


;;; Functions which create new table data objects from an existing object.


(defun copy-of-row-other (td row) (copy-seq (aref (td-other-array td) row)))

(defun copy-of-row-data (td row) (copy-seq (aref (td-data-array td) row)))

(defun td-other-subtable (td rows)
  (map 'vector #'(lambda (row) (copy-of-row-other td row)) rows))
(defun td-data-subtable (td rows)
  (map 'vector #'(lambda (row) (copy-of-row-data td row)) rows))


(defun table-data-row-table (td row-designator)
  "Returns an entire table object, which only has the rows specified"
  (let ((f 'table-data-row))
    (verify-td td f)
    (table-data-rows-table
     td (list (row-designator-to-row td row-designator f)))))
  
(defun table-data-rows-table (td row-designators)
  "Returns an entire table object, which only has the rows specified"  
  (let ((f 'table-data-rows))
    (verify-td td f)
    (let ((row-indices (row-designators-to-rows td row-designators f)))
      (make-instance
       'table-data
       :td-name nil
       :td-source-file nil
       :td-docs nil
       :td-headers (td-headers td)
       :td-posthdrs nil
       :td-precols (td-precols td)
       :td-datacols (td-datacols td)
       :td-postcols (td-postcols td)
       :td-othercols (td-othercols td)
       :td-keycols (td-keycols td)
       :td-rdrfuncs nil
       :td-data-rdrfunc nil
       :td-descs (td-descs td)
       :td-nrows (length row-indices)
       :td-ncols (td-ncols td)
       :td-nprecols (td-nprecols td)
       :td-ndatacols (td-ndatacols td)
       :td-npostcols (td-npostcols td)
       :td-nothercols (td-nothercols td)
       :td-nkeycols (td-nkeycols td)
       :td-other-array (td-other-subtable td row-indices)
       :td-data-array  (td-data-subtable td row-indices)
       :td-data-type (td-data-type td)
       :td-hashes nil
       :td-missing-count 0
       :td-missing-value (td-missing-value td)
       ))))


(defun elements-of (seq indices) 
  (map 'list #'(lambda (i) (nth i seq)) indices))

(defun elements-of-in-range (seq indices)
  (let ((len (length seq)))
    (loop for i in indices if (< i len) collect (nth i seq))))

(defun map-to-new-cols (old-cols selected-columns)
  (mapcar
   #'(lambda (old-col)
       (position old-col selected-columns :test #'=))
   old-cols
   ))

(defun table-data-cols-table (td column-designators &aux (f 'table-data-cols))
  "Returns an entire new table object, with only the columns specified"
  (verify-td td f)
  (let ((selected-columns
         (column-designators-to-columns td column-designators f)))
    (unless (equal (remove-duplicates selected-columns) selected-columns)
      (error "~A: Duplicated columns selected. ~A: ~A" 
             "List of selected column indices: " selected-columns))
    (setq selected-columns (sort selected-columns #'<))

    (let* ((new-headers (elements-of (td-headers td) selected-columns))
           (old-precols-selected 
            (intersection (td-precols td) selected-columns)) 
           (new-precols 
            (map-to-new-cols old-precols-selected selected-columns))
           (old-datacols-selected 
            (intersection (td-datacols td) selected-columns))
           (new-datacols 
            (map-to-new-cols old-datacols-selected selected-columns))
           (old-postcols-selected
            (intersection (td-postcols td) selected-columns))
           (new-postcols
            (map-to-new-cols old-postcols-selected selected-columns))
           (old-keycols-selected
            (intersection (td-keycols td) selected-columns))
           (new-keycols 
            (map-to-new-cols old-keycols-selected selected-columns))
           (new-othercols (append new-precols new-postcols))
           (new-descs (elements-of (td-descs td) selected-columns))
           (new-ncols (length selected-columns))
           (new-nprecols (length new-precols))
           (new-ndatacols (length new-datacols))
           (new-npostcols (length new-postcols))
           (new-nothercols (+ new-nprecols new-npostcols))
           (new-nkeycols (length new-keycols))
           (nrows (td-nrows td))
           (new-other-array (make-array nrows))
           (new-data-array (make-array nrows))
           )

      (dotimes (row nrows)
        (declare (fixnum row))
        (let ((other-vector (make-array new-nothercols))
              (data-vector
               (make-array new-ndatacols :element-type (td-data-type td))))
          (setf (aref new-other-array row) other-vector)
          (setf (aref new-data-array row) data-vector)
          (loop for col in old-precols-selected for j fixnum from 0 do
                (setf (aref other-vector j)
                      (table-data-element-internal td row col)))
          (loop for col in old-postcols-selected 
                for j fixnum from new-nprecols do
                (setf (aref other-vector j)
                      (table-data-element-internal td row col)))
          (loop for col in old-datacols-selected for j fixnum from 0 do
                (setf (aref data-vector j)
                      (table-data-element-internal td row col)))))

      (make-instance
       'table-data
       :td-name nil
       :td-source-file nil
       :td-docs nil
       :td-headers new-headers
       :td-posthdrs nil
       :td-precols new-precols
       :td-datacols new-datacols
       :td-postcols new-postcols
       :td-othercols new-othercols
       :td-keycols new-keycols
       :td-rdrfuncs nil
       :td-data-rdrfunc nil
       :td-descs new-descs
       :td-nrows (td-nrows td)
       :td-ncols new-ncols
       :td-nprecols new-nprecols
       :td-ndatacols new-ndatacols
       :td-npostcols new-npostcols
       :td-nothercols new-nothercols
       :td-nkeycols new-nkeycols
       :td-other-array new-other-array
       :td-data-array new-data-array
       :td-data-type (td-data-type td)
       :td-hashes nil
       :td-missing-count 0
       :td-missing-value (td-missing-value td)                    
       )

      )))

(defun table-data-col-table (td column-designator)
  "Returns an entire new table object, with only the columns specified"
  (table-data-cols td (list column-designator)))


(defun table-data-subtable (td rows cols)
  #.(one-string-nl
     "Returns an entire new table object, with the data elements at "
     "the crossproduct of the rows and columns designated in the new table.")
  (table-data-rows-table (table-data-cols-table td cols) rows))


