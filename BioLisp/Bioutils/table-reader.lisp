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

;;;;  See specification in DOC/TABLE-DATA.TXT.

;;; Error message localized to a line of the table we are reading.

(defun rtse (error-message &rest args)
  (format t "~&~%;; Syntax error processing table file ~A~%" *table-filename*)
  (format t ";; Error occurred on line ~D of file.~%" *table-file-line-count*)
  (apply #'error error-message args)
  )

;;; Helper functions for reading and parsing a table data file.

(defun notminusp (x) (and (integerp x) (not (minusp x))))

(defun trim (s) (string-trim " " s))

(defun verify-table-data-line (line nfields)
  (= nfields (1+ (count #\Tab line))))

(defun empty-table-data-line? (line)
  (every #'(lambda (ch) (or (eql ch #\Tab) (eql ch #\Space))) line))

(defun comment-table-data-line? (line comment-char)
  (and comment-char
       (loop for ch across line do
             (cond
              ((or (eql ch #\Space) (eql ch #\Tab)) nil)
              ((eql ch comment-char) (return t))
              (t (return nil))
              ))))

(defun string-split-on-char (string char)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (simple-string string))
  (let ((lastpos (the fixnum (1- (length string)))))
    (declare (fixnum lastpos))
    (if (< lastpos 0)
        (list "")
      (labels ((string-split (spos)
                 (let ((pos (position char string :start spos)))
                   (cond
                    ((null pos) (list (subseq string spos)))
                    ((= pos lastpos) (list (subseq string spos pos) ""))
                    ((= pos spos) (cons "" (string-split (1+ pos))))
                    (t (cons (subseq string spos pos) (string-split (1+ pos))))
                    ))))
        (string-split 0)
        ))))

(defmethod line-split ((line string) (format (eql :standard)))
  (string-split-on-char line #\Tab))

(defun next-table-file-line (s) 
  (let ((line (read-line s nil nil)))
    ;;(print (list *table-file-line-count* line))
    (incf *table-file-line-count*)
    line))

(defun parse-key-columns (key-columns n-total-fields headers)
  (let ((key-indices nil))
    (loop for key in key-columns do
          (cond
           ((integerp key)
            (unless (and (not (minusp key)) (< key n-total-fields))
              (rtse "Invalid key column: ~D.  (Total columns = ~D)"
                    key n-total-fields))
            (pushnew key key-indices))
           ((stringp key)
            (let ((column-number (position key headers :test #'string-equal)))
              (unless column-number
                (rtse "Invalid key: ~A.  Not a known name of a field." key))
              (pushnew column-number key-indices)))
           (t (rtse "Invalid key column specifier: ~A" key))
           ))
    (nreverse key-indices)))


(defun is-other-colindex (x n-predata-fields n-data-fields n-postdata-fields)
  (or (and (not (minusp x)) (< x n-predata-fields))
      (and (>= x (+ n-predata-fields n-data-fields))
           (< x (+ n-predata-fields n-data-fields n-postdata-fields))
           )))

(defun parse-other-rdrfuncs 

       (
        other-rdrfuncs data-rdrfunc
        n-predata-fields n-data-fields n-postdata-fields 
        headers
        &aux
        (n-total-fields (+ n-predata-fields n-data-fields n-postdata-fields))
        (start-pre 0)
        (end-pre n-predata-fields)
        (start-data end-pre)
        (end-data (+ n-predata-fields n-data-fields))
        (start-post end-data)
        (end-post n-total-fields)
        (results-array (make-array n-total-fields :initial-element #'identity))
        )

  ;; The OTHER-RDRFUNCS parameter must be given as either
  ;; -- a symbol or function
  ;; -- a list of symbols/functions of length N-OTHER-FIELDS
  ;; -- a assoc mapping of non-data columns to symbols/functions
  ;; -- where the column indicator is either the name of a column
  ;; -- or a column index.

  ;; Returns a vector of functions of length N-OTHER-FIELDS, one
  ;; function for each other field.

  (loop for j from start-data below end-data do 
        (setf (aref results-array j) data-rdrfunc))

  (flet ((fobj? (x) (or (symbolp x) (functionp x)))
         (other-colindex? (j)
           (is-other-colindex 
            j n-predata-fields n-data-fields n-postdata-fields))
         (assoc? (x) (and (listp x) (listp (cdr x)) (= (length x) 2)))
         (badcolspec (x)
           (lerror 
            "Invalid column specifier for OTHER-RDRFUNCS: ~A. Each column"
            "specifier must be an integer denoting a non-data column"
            "or the name of a non-data column.  The names of the non-data"
            "columns and their column indices are: "
            "~S"
            x
            (let ((cols nil) (count 0))
              (loop for j from start-pre below end-pre do
                    (push (list (nth count headers) count) cols) (incf count))
              (loop for j from start-data below end-data do (incf count))
              (loop for j from start-post below end-post do
                    (push (list (nth count headers) count) cols) (incf count))
              ))))
            

    (let ((n-other-fields (+ n-predata-fields n-postdata-fields)))
      (cond
       ;; Not provided
       ((null other-rdrfuncs) nil)
       ;; a symbol or function
       ((fobj? other-rdrfuncs)
        (loop for j from start-pre below end-pre do
              (setf (aref results-array j) other-rdrfuncs))
        (loop for j from start-post below end-post do
              (setf (aref results-array j) other-rdrfuncs)))
       ;; a simple list
       ((every #'fobj? other-rdrfuncs)
        (unless (= n-other-fields (length other-rdrfuncs))
          (lerror
           "When specifying a list of OTHER-RDRFUNCS values, there must be"
           "one function in the list for each non-data field in the table."
           "There are ~D non-data fields but there are ~D elements specified"
           "in OTHER-RDRFUNCS."
           n-other-fields (length other-rdrfuncs)))
        (let ((funcs other-rdrfuncs))
          (loop for j from start-pre below end-pre do
                (setf (aref results-array j) (pop funcs)))
          (loop for j from start-post below end-post do
                (setf (aref results-array j) (pop funcs)))))
       ;; a map from column indices or names to reader functions
       ((and (listp other-rdrfuncs) (assoc? (first other-rdrfuncs)))
        (unless (every #'assoc? other-rdrfuncs)
          (lerror
           "Invalid OTHER-RDRFUNCS column->function map value: ~A." 
           "Each element should be of the form (<column> <function>)."
           other-rdrfuncs))
        (loop for (cd func) in other-rdrfuncs do
              (cond
               ((integerp cd)
                (unless (other-colindex? cd) (badcolspec cd))
                (setq cd (nth cd headers)))
               ((or (stringp cd) (symbolp cd)) (setq cd (string cd)))
               (t (badcolspec cd)))
              (let ((pos (position cd headers :test #'string-equal)))
                (unless (and pos (other-colindex? pos)) (badcolspec cd))
                (unless (fobj? func)
                  (lerror "Invalid reader function in OTHER-RDRFUNCS"
                          "column->function map alue: ~A" func))
                (setf (aref results-array pos) func)
                )))
       (t (error "Unknown OTHER-RDRFUNCS argument value: ~A" other-rdrfuncs))
       )))

  results-array)

  
(defmethod retrieve-nonempty-line 
           ((p stream) (format (eql :standard)) 
            &key (on-eof :error) (comment-char nil))
  (do ((line (next-table-file-line p) (next-table-file-line p))) 
      (())
    (cond
     ((null line)
      (ecase on-eof
        ((nil) (return nil))
        ((:error) (rtse "End of file reached before non-blank line found"))
        ))
     ((and (not (empty-table-data-line? line))
           (not (comment-table-data-line? line comment-char)))
      (return line))
     (t nil)
     )))

(defmethod retrieve-comment-lines 
           ((n integer) (p stream) (format (eql :standard)) 
            &key (on-eof :error))
  (let ((lines nil) (line nil))
    (do ((count 0 (1+ count))) ((>= count n))
      (setq line (next-table-file-line p))
      (cond
       ((null line)
        (ecase on-eof
          ((nil) (return nil))
          ((:error) (rtse "End of file reached before ~D lines read" n))
          ))
       (t (push (trim (substitute #\Space #\Tab line)) lines))
       ))
    (nreverse lines)
    ))
     
(defmethod retrieve-posthdr-lines 
           ((n integer) (p stream) (format (eql :standard)) 
            &key (on-eof :error))
  (retrieve-comment-lines n p format :on-eof on-eof))

;; Returns a list of header strings (column names).

(defmethod parse-header-line ((line string) (format (eql :standard)))
  (let ((headers (mapcar #'trim (line-split line format))))
    (loop for count from 1 for header in headers do
          (when (string= header "")
            (rtse "In the header line: ~A~%field ~D is empty, ~A"
                  line count "but must contain the name of the field.")))
    headers
    ))
          
      
;; Returns three values
;; -- A vector of values which are the other fields contents
;; -- A vector of data values which are the data fields contents
;; -- How many values were missing

(defun parse-data-line 
       (line n-total-fields n-other-fields
             n-predata-fields n-data-fields 
             rdrfuncs missing-value data-array-element-type format
             warn-on-coercion use-missing-value-if-not-enough-fields
             &aux
             field-strings other data
             (start-pre 0) (end-pre n-predata-fields)
             (start-data n-predata-fields) 
             (end-data (the fixnum (+ n-predata-fields n-data-fields)))
             (start-post end-data) (end-post n-total-fields)
             (nfields 0) (missing-count 0)
            )
  (declare (fixnum n-total-fields n-other-fields))
  (declare (fixnum n-predata-fields n-data-fields))
  (declare (fixnum start-pre end-pre start-data end-data start-post end-post))
  (declare (fixnum nfields missing-count))
  (declare (type (simple-array t 1) rdrfuncs))

  ;; Split the line up into tab separated fields.

  (setq field-strings (line-split line format))
  (setq nfields (length field-strings))
  (unless 
      (or use-missing-value-if-not-enough-fields (= nfields n-total-fields))
    (format t ";; Data line: ~A~%" line)
    (format t ";; Field strings: ~S~%" field-strings)
    (rtse "Invalid data line. ~A"
          (format nil "Line has ~D tab-delimited fields but should have ~D."
                  nfields n-total-fields)))

  ;; Create the vectors that will hold the data.

  (setq other (make-array n-other-fields :element-type t))
  (setq data (make-array n-data-fields :element-type data-array-element-type))

  (labels
      ((parse-field (field j data-field?)
         (if (null field)
             (progn (incf missing-count) missing-value)
           (let ((s (trim field)))
             (if (string= s "") 
                 (progn (incf missing-count) missing-value)
               (let* ((datum (funcall (aref rdrfuncs j) s)))
                 (if (or (not data-field?) 
                         (typep datum data-array-element-type))
                     datum
                   (safe-coerce datum s data-array-element-type)
                   ))))))
       (safe-coerce (datum s data-type)
         (multiple-value-bind (value error?)
             (coerce datum data-type)
           (when error?
             (rtse "Data item '~A' is supposed to be of type ~A. ~A"
                   s data-type "Attempted coercion to that type failed."))
           (when warn-on-coercion
             (format t ";; WARNING:  Coerced '~A' to type ~A~%." 
                     datum data-type)
             (format t ";;   Datum occurred on line ~D.~%" 
                     *table-file-line-count*))
           value
           )))
    (loop for j fixnum from start-pre below end-pre do
          (setf (aref other j) (parse-field (pop field-strings) j nil)))
    (loop for j fixnum from start-data below end-data do
          (setf (aref data (the fixnum (- j start-data)))
                (parse-field (pop field-strings) j t)))
    (loop for j fixnum from start-post below end-post do
          (setf (aref other (the fixnum (+ (the fixnum (- j start-post))
                                           n-predata-fields)))
                (parse-field (pop field-strings) j nil)))

    (values other data missing-count)
    
    ))



(defun read-table-data 

       (file &key 
             (format :standard)
             (comment-char #\;)
             (n-doc-lines 0)
             (n-posthdr-lines 0)
             (n-predata-fields 1)
             (n-postdata-fields 0)
             (data-type 'single-float)
             (none-missing? nil)
             (missing-value nil)
             (key-columns nil)
             (field-descriptors nil)
             (name nil)
             (data-rdrfunc #'read-from-string)
             (other-rdrfuncs nil)
             (warn-on-coercion *warn-on-data-coercion*)
             (warn-on-missing-value-mismatch t)
             (use-missing-value-if-not-enough-fields nil)
             (strip-returns t)
             &allow-other-keys
             &aux 
             (docs nil)
             (missing-count 0)
             headers posthdrs line n-data-fields n-total-fields n-other-fields
             other-fields-vector
             data-fields-vector
             (key-indices nil)
             data-array-element-type
             (*table-filename* file)
             (*table-file-line-count* 0)
             )
  (declare (special *table-filename* *table-file-line-count*))

  #.(one-string
     "Read a tab-delimited FILE in a standard format into an internal "
     "representation.  See the TABLE DATA documentation file for more "
     "detailed information.")

  ;; Verify arguments that can be verified without knowing number
  ;; of columns and their names.

  (cond
   ((and (stringp comment-char) (= 1 (length comment-char)))
    (setq comment-char (char comment-char 0)))
   ((or (null comment-char) (characterp comment-char)) nil)
   (t (error "Invalid COMMENT-CHAR value: ~A" comment-char)))

  (labels
      ((notminusp (n) (and (integerp n) (not (minusp n))))
       (oops (sym val)
         (unless (notminusp val)
           (lerror "Argument ~A value is invalid: ~A.  Value must be"
                   "a non-negative integer." sym val))))
    (oops 'n-doc-lines n-doc-lines)
    (oops 'n-posthdr-lines n-posthdr-lines)
    (oops 'n-predata-fields n-predata-fields)
    (oops 'n-postdata-fields n-postdata-fields))

  (multiple-value-bind (ignore error?)
      (typep 0 data-type)
    (declare (ignore ignore))
    (when error?
      (error "Argument DATA-TYPE invalid: ~A.  Not a known type." data-type)))

  ;; The DATA-RDRFUNC parameter must be a function.  It should be
  ;; a function of a single argument and gets called on the string
  ;; value of each data field that is read.

  (unless (or (symbolp data-rdrfunc) (functionp data-rdrfunc))
    (error "Invalid DATA-RDRFUNC value: ~A" data-rdrfunc))

  ;; Can we specialize the type of the vectors holding the data elements?

  (cond
   ((or none-missing? (typep missing-value data-type))
    (setq data-array-element-type data-type))
   (t 
    (when warn-on-missing-value-mismatch
      (format t "~&;; Warning: Type of MISSING-VALUE argument, ~A~%"
              (type-of missing-value))
      (format t ";;   is not the same as the type given for the data array.~%")
      (format t ";;   This may be slower than is optimal.  Specify~%")
      (format t ";;   (:none-missing? t) if possible or provide a different~%")
      (format t ";;   value for MISSING-VALUE to stop this warning.~%"))
    (setq data-array-element-type t)))
   
  ;; Make sure file is in a standard format

  (when strip-returns (bio::strip-file-of-returns-preceding-newlines file))

  (with-open-file (s file :direction :input :if-does-not-exist :error)

    (setq line (retrieve-nonempty-line s format :comment-char comment-char))
      
    (unless (zerop n-doc-lines)
      (setq docs 
            (cons line (retrieve-comment-lines (1- n-doc-lines) s format)))
      (setq line (retrieve-nonempty-line s format :comment-char comment-char)))

    ;; Find out how many columns there are and the name of each column.

    (setq headers (parse-header-line line format))
    (setq n-total-fields (length headers))

    (setq n-other-fields (+ n-predata-fields n-postdata-fields))
    (setq n-data-fields (- n-total-fields n-other-fields))
    (when (minusp n-data-fields)
      (lerror "You specified  ~D predata fields and ~D postdata fields,"
              "but there are only ~D fields actually in the table."
              n-predata-fields n-postdata-fields n-data-fields))

    ;; Figure out the columns that are to be KEYS.

    (setq key-indices (parse-key-columns key-columns n-total-fields headers))

    ;; Parse the OTHER-RDRFUNCS argument.

    (setq other-rdrfuncs 
          (parse-other-rdrfuncs 
           other-rdrfuncs data-rdrfunc
           n-predata-fields n-data-fields n-postdata-fields
           headers))

    ;; Get any lines between the column headers and the data.

    (setq posthdrs (retrieve-posthdr-lines n-posthdr-lines s format))
      
    (setq line (retrieve-nonempty-line s format :comment-char comment-char))

    ;; Okay, we're set to read the data.  We're going to read it in to
    ;; two vectors of vectors, one for the other fields and one for the
    ;; data fields.

    (setq other-fields-vector (make-array 0 :adjustable t :fill-pointer t))
    (setq data-fields-vector (make-array 0 :adjustable t :fill-pointer t))

    (do ((data-line line (next-table-file-line s)))
        ((null data-line))
      (multiple-value-bind (other-fields data-fields missing?)
          (parse-data-line 
           data-line n-total-fields n-other-fields
           n-predata-fields n-data-fields
           other-rdrfuncs missing-value data-array-element-type format
           warn-on-coercion use-missing-value-if-not-enough-fields
           )
        (vector-push-extend other-fields other-fields-vector)
        (vector-push-extend data-fields data-fields-vector)
        (incf missing-count missing?)
        ))
      
    (when (and none-missing? (plusp missing-count))
      (format t "~&;; Warning:  You specified (:none-missing t), but~%")
      (format t ";;  ~D data fields in the table were in fact not present.~%"
              missing-count))

    (when (zerop (length data-fields-vector))
      (rtse "EOF encountered before finding any data!"))
    
    (let ((precols (loop for j from 0 below n-predata-fields collect j))
          (datacols (loop for j from n-predata-fields 
                          below (+ n-predata-fields n-data-fields) collect j))
          (postcols (loop for j from (+ n-predata-fields n-data-fields)
                          below n-total-fields collect j))
          (data-type 
           (if (typep missing-value data-type) data-type t)))
      (make-instance 
       'table-data
       :td-name name
       :td-source-file file
       :td-docs docs
       :td-headers headers
       :td-posthdrs posthdrs
       :td-precols precols
       :td-datacols datacols
       :td-postcols postcols
       :td-othercols (append precols postcols)
       :td-keycols key-indices
       :td-rdrfuncs other-rdrfuncs
       :td-data-rdrfunc data-rdrfunc
       :td-descs field-descriptors
       :td-nrows (length data-fields-vector)
       :td-ncols n-total-fields
       :td-nprecols (length precols)
       :td-ndatacols (length datacols)
       :td-npostcols (length postcols)
       :td-nothercols (+ (length precols) (length postcols))
       :td-nkeycols (length key-indices)
       :td-data-type data-type
       :td-other-array other-fields-vector
       :td-data-array data-fields-vector
       :td-hashes nil
       :td-missing-count missing-count
       :td-missing-value missing-value
       ))

    ))




#+test
(progn
(defvar *tt*)
(defun read-test-table ()
  (setq *tt*
        (read-table-data
         "biol:Analysis;test-table3.txt"
         :data-type 'single-float
         :n-doc-lines 2
         :n-posthdr-lines 4
         :n-predata-fields 3
         :n-postdata-fields 2
         :missing-value -100.0
         :key-columns '("name" "ssn" 7)
         :name "Flintstone table"
         :other-rdrfuncs `((:name ,#'(lambda (x) (bio::frame-fnamed x t)))
                           (:r1 ,#'(lambda (x) (1+ (parse-integer x)))))
         :warn-on-coercion t
         :warn-on-missing-value-mismatch t
         ))
  (describe-table-data *tt* :nrows 2))


(defvar *h*)
(defun load-hihara-test ()
  (setq *h*
	(read-table-data "biol:data;hiharadat.tbl"
			 :key-columns '(0)
                         :other-rdrfuncs #'(lambda (x) (bio::frame-fnamed x t))
			 :missing-value -100.0)))
)
