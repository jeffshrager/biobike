; -*- mode: Lisp; Syntax: Common-Lisp; Package: bio; -*-

(in-package :bio)

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

;;; FFF Make the seq file format conform to minimal fasta standard.  
;;; At the moment the whole file has to be one long string.  
;;; Indeed, the SEQUENCE has to be one long string for indexing purposes, 
;;; but there should be able to be a line break before and after each defline.

;;; FFF permit stray blank lines in various places, esp. at the end of
;;; tables, which is quite common.  (Esp. Step 3, which gives an
;;; obscure error if there's a blank line.)



(defun read-upload-table (file)
  ;; Read the entire table with all the columns as strings
  ;; and all the columns as 'data'.  That way the columns
  ;; can appear in any order.
  (read-table-data 
   file
   :n-predata-fields 0
   :n-postdata-fields 0
   :data-type 'string
   :missing-value ""
   :data-rdrfunc 
   #'(lambda (x) (if (and (= (length x) 1) (string= x "-")) "" x))
   :use-missing-value-if-not-enough-fields t
   ))

;;; If a TABLE DATA object has a PROPERTIES column, use it, returning
;;; a list of all the property-lists (either in PLIST or STRING form).
;;; If there is no PROPERTIES column, then for each row, gather all the 
;;; data from all the columns that are not on the EXCLUDED-HEADERS list
;;; and make a property list for each row from these column names and 
;;; the data in the row for the columns.  If any datum is "" then don't
;;; create a key-value pair for that datum.

;;; If there is a PROPERTIES column, then its contents is parsed to insure
;;; valid property lists.  The 2nd value returned is the number of 
;;; invalid-syntax property lists found.

(defun column-should-not-be-property (column-name)
  (declare (ignore column-name))
  nil)

(defun non-unique-elements (list test-function)
  (check-for-duplicates list :test test-function))

                    
(defun property-lists-from-tbl-data 
       (tbl excluded-headers to-what 
            &aux property-lists (error-count 0) other-headers other-properties)
  (block exit
    (pushnew *generic-properties-field* excluded-headers :test #'string-equal)
    (let ((headers (table-data-headers tbl)))
      ;; Read the PROPERTIES column.
      (cond
       ((member *generic-properties-field* headers :test #'string-equal)
        (multiple-value-setq (property-lists error-count)
            (tbl-plist-strings-to 
             (table-data-col tbl *generic-properties-field*) :lists))
        (when (plusp error-count) 
          (return-from exit (values property-lists error-count))))
       (t
        (setq property-lists 
              (make-list (table-data-nrows tbl) :initial-element nil))))
      ;; Read data from other random columns and make KEY-VALUE
      ;; property pairs out of the column name and the data.
      (setq other-headers 
            (remove-if
             'column-should-not-be-property
             (set-difference headers excluded-headers :test #'string-equal)))
      (when other-headers
        (cformatt "Columns to be treated as properties: ~A" other-headers)
        (setq other-properties
              (loop for row from 0 below (table-data-nrows tbl) collect
                    (mapcan
                     #'(lambda (key)
                         (let ((data (table-data-element tbl row key)))
                           (unless (string= data "")
                             (list (list (keywordize key) data))
                             )))
                     other-headers
                     )))
        ;; Merge the property list with the other properties.
        (setq property-lists (mapcar 'nconc property-lists other-properties)))
      ;; Return the property lists as either lists or strings
      (values
       (ecase to-what
         ((:list :lists) property-lists)
         ((:string :strings) 
          (mapcar #'plist-to-plist-string property-lists)))
       error-count
       ))))

(defun is-header-column? (tbl column-name)
  (member column-name (table-data-headers tbl) :test #'string-equal))


;;;; PROCESSING THE GENOME INFORMATION TABLE.

;;; Step 1:  Read the genome .tbl file into a table data object.
;;; Step 2:  Compare the names (keys) in the table to the names (keys)
;;; in the genome fasta data.  There should be a one-to-one and onto
;;; correspondence.
;;; Step 3:  Verify that all the property lists in the table are 
;;; syntactically valid, converting them all into canonical form.
;;; Step 4: Create the database table.
;;; Step 5: Insert each record from the table into the database

(defun genome-tbl-in-subdir-to-db 

       (tbl-file info-db-table-name 
                 &aux
                 tbl tbl-keys tbl-properties tbl-descriptions n-tbl-keys
                 fasta-keys n-fasta-keys
                 missing-fasta-keys missing-tbl-keys
                 (tbl-key-field *generic-information-table-key*)
                 (desc-field *generic-description-field*)
                 (contig-prefix (organism-element-prefix :contig))
                 )

  (block exit

    ;; Step 1

    (cformatt "Reading genome table file: ~A" (namestring tbl-file))
    (setq tbl (read-upload-table tbl-file))
    (unless (is-header-column? tbl tbl-key-field)
      (error "Genome table file has no ~A column!" tbl-key-field))
    (cformatt "Genome table file read.")

    ;; Step 2

    (cformatt "Verifying that each genome table key is unique.")
    (setq tbl-keys (table-data-col tbl tbl-key-field))
    (unless (verify-upload-key-uniqueness tbl-keys "Genome table")
      (return-from exit nil))
    (setq tbl-keys 
          (add-organism-element-prefix-to-keys tbl-keys contig-prefix))

    (cformatt "Verifying that each table key has a fasta key and vice versa")
    (setq fasta-keys *genome-keys*)
    (setq n-tbl-keys (length tbl-keys))
    (setq n-fasta-keys (length fasta-keys))
    (cond
     ((< n-fasta-keys n-tbl-keys)
      (cformatt 
       "*Ruh roh. The genome spec contains more table keys than fasta keys!"))
     ((> n-fasta-keys n-tbl-keys)
      (cformatt
       "*Ruh roh. The genome spec contains more fasta keys than table keys!")))
    (setq missing-tbl-keys 
          (check-if-any-a-is-not-in-b fasta-keys tbl-keys :test 'string-equal))
    (setq missing-fasta-keys
          (check-if-any-a-is-not-in-b tbl-keys fasta-keys :test 'string-equal))
    (when missing-fasta-keys
      (cformatt "Some genome table keys have no matching fasta key: ")
      (dolist (key missing-fasta-keys) 
        (cformatt "  Existing table key but missing fasta key: ~A" key)))
    (when missing-tbl-keys
      (cformatt "Some genome fasta keys have no matching tbl key: ")
      (dolist (key missing-tbl-keys) 
        (cformatt "  Existing fasta key but missing tbl key: ~A" key)))
    (when (or missing-fasta-keys missing-tbl-keys)
      (cformatt "Not creating genome information database table")
      (return-from exit nil))
    (cformatt "All table keys match fasta keys and vice versa.")

    ;; Step 3

    (cformatt "Verifying properties read from genome table file")
    (multiple-value-bind (plist-strings error-count)
        (property-lists-from-tbl-data 
         tbl 
         (list *generic-information-table-key* *generic-description-field*) 
         :strings)
      (when (plusp error-count) (return-from exit nil))
      (setq tbl-properties plist-strings)
      (cformatt "Genome table file properties verified."))

    ;; Step 4

    (cformatt "Creating database table ~A" info-db-table-name)
    (create-genome-information-table info-db-table-name)
    (push info-db-table-name *new-organism-tables*)
    (cformatt "Created database table ~A" info-db-table-name)

    ;; Step 5

    (cformatt "Inserting ~D records into genome table" (length tbl-keys))
    (setq tbl-descriptions (get-table-descriptions tbl desc-field))
    (loop for key in tbl-keys
          for property in tbl-properties
          for desc in tbl-descriptions
          do
          (insert-record-into-genome-information-table
           info-db-table-name key property desc
           ))
    (cformatt 
     "All data records inserted. Created genome information table.")

    t

    ))


;;;; PROCESSING THE GENES INFORMATION TABLE.

;;; Step 1:  Reads the genes.tbl file into a table data object.
;;; Step 2:  Verify that each gene name is unique.
;;; Step 3:  Verify that the genome component pointed at by each gene exists.
;;; Step 4:  Verify the properties field entries and canonicalize them.
;;; Step 5:  Verify the ARCHITECTURE field entries, and canonicalize them.
;;; Step 6:  Augment the property lists with ARCHITECTURE information.
;;; Step 7:  Create the database table to hold the gene information.
;;; Step 8:  Insert each record, suitably modified, into the database.

(defun genes-tbl-in-subdir-to-db 

       (tbl-file info-db-table-name
                 &aux
                 tbl tbl-keys tbl-properties tbl-descriptions
                 genome-pointers genome-fasta-keys
                 (tbl-key-field *generic-information-table-key*)
                 (desc-field *generic-description-field*)
                 (gc-field "GENOME-COMPONENT")
                 (direction-field "DIRECTION")
                 (from-field "FROM")
                 (to-field "TO")
                 (arch-field "ARCHITECTURE")
                 (directions nil) (from-values nil) (to-values nil)
                 (error? nil)
                 (genes-prefix (organism-element-prefix :genes))
                 (contig-prefix (organism-element-prefix :contig))
                 )

  (block exit

    ;; Step 1

    (cformatt "Reading genes table file: ~A" (namestring tbl-file))
    (setq tbl (read-upload-table tbl-file))
    (flet ((oops (column-name)
             (unless (is-header-column? tbl column-name)
               (error "Genes table file has no ~A column!" column-name))))
      (oops tbl-key-field) (oops gc-field)
      (oops direction-field) (oops from-field) (oops to-field)
      )
    (cformatt "Genes table file read.")

    ;; Step 2

    (cformatt "Verifying that each gene table key is unique.")
    (setq tbl-keys  (table-data-col tbl tbl-key-field))
    (unless (verify-upload-key-uniqueness tbl-keys "Genes table")
      (return-from exit nil))
    (setq tbl-keys (add-organism-element-prefix-to-keys tbl-keys genes-prefix))
    (setq *genes-keys* tbl-keys)

    ;; Step 3

    (cformatt "Verifying that each pointer to a genome component is valid")
    (setq genome-pointers (table-data-col tbl gc-field))
    (setq genome-pointers
          (add-organism-element-prefix-to-keys genome-pointers contig-prefix))
    (setq genome-fasta-keys *genome-keys*)
    (cformatt "Genome keys: ")
    (dolist (key genome-fasta-keys) (cformatt "  ~A" key))
    (let ((bad-pointers 
           (check-if-any-a-is-not-in-b 
            genome-pointers genome-fasta-keys :test 'string-equal)))
      (when bad-pointers
        (cformatt 
         "*Ruh roh. Genes table contains ~D invalid genome references."
         (length bad-pointers))
        (utils::limited-error-message-loop
         10 "... and more invalid references."
        for pointer in bad-pointers do
        (cformatt "  Invalid genome segment reference: ~A" pointer))
        (return-from exit nil)
        ))

    ;; Step 4

    (multiple-value-setq (tbl-properties error?)
        (property-lists-from-upload-table-data
         tbl
         (list 
          *generic-information-table-key*   
          "GENOME-COMPONENT" "FROM" "TO" "DIRECTION"
          *generic-description-field*)
         "genes table file"))
    (when error? (return-from exit nil))

    ;; Step 5

    (cformatt "Verifying DIRECTION, FROM and TO from genes table file")
    (setq directions (table-data-col tbl direction-field))
    (let ((froms (table-data-col tbl from-field))
          (tos (table-data-col tbl to-field))
          (errors nil))
      (loop for key in tbl-keys 
            for d in directions
            for f in froms 
            for to in tos do
            (multiple-value-bind (from-value to-value error-reason)
                (verify-direction-from-and-to-strings d f to)
                (if error-reason 
                    (push (list key d f to error-reason) errors)
                  (progn 
                    (push from-value from-values)
                    (push to-value to-values)
                    ))))
      (when errors
        (cformatt "*Ruh roh. Bad DIRECTION, FROM, TO values detected:")
        (utils::limited-error-message-loop
         10 "... and more bad DIRECTION, FROM, TO values."
        for (key d f to reason) in errors do
        (cformatt "  Key:  ~A, ~A, Reason: ~S" key (list d f to) reason))
        (return-from exit nil))
      (setq from-values (nreverse from-values))
      (setq to-values (nreverse to-values))
      )
    (cformatt "Verified DIRECTION, FROM and TO from genes table file")
      
    ;; Step 6

    (cformatt "Verifying architecture column from genes table file")
    (when (is-header-column? tbl arch-field)
      (let ((arch-lists nil) (bad-arch-strings nil))
        (loop for arch-string in (table-data-col tbl arch-field) 
              for key in tbl-keys do
              (handler-case
                  (if (string= arch-string "")
                      (push nil arch-lists)
                    (let ((arch-list (read-from-string arch-string)))
                      (if (valid-architecture-list arch-list)
                          (push arch-list arch-lists)
                        (push (list key arch-string) bad-arch-strings)
                        )))
                (error () (push (list key arch-string) bad-arch-strings))
                ))
        (setq arch-lists (nreverse arch-lists))
        (setq bad-arch-strings (nreverse bad-arch-strings))
        (when bad-arch-strings
          (cformatt "*Ruh roh. Bad Architecture field values detected:")
          (utils::limited-error-message-loop
           10 "... and more invalid architecture fields."
           for (key arch-string) in bad-arch-strings do
           (cformatt "Invalid architecture for key ~A: ~S" key arch-string))
          (return-from exit nil))
        (setq tbl-properties
              (mapcar
               #'(lambda (x y) (if x (cons (list :architecture x) y) y))
               arch-lists
               tbl-properties
               ))))
    (cformatt "Genome table file architecture column verified.")
    
    ;; Step 7
    
    (cformatt "Creating database table ~A" info-db-table-name)
    (create-genes-information-table info-db-table-name)
    (push info-db-table-name *new-organism-tables*)
    (cformatt "Created database table ~A" info-db-table-name)

    ;; Step 8

    (cformatt "Inserting ~D records into genes table" (length tbl-keys))
    (setq tbl-descriptions (get-table-descriptions tbl desc-field))
    (loop for key in tbl-keys
          for pointer in genome-pointers
          for direction in directions
          for from in from-values
          for to in to-values
          for property in tbl-properties
          for desc in tbl-descriptions
          do
          (insert-record-into-genes-information-table
           info-db-table-name key pointer 
           direction from to 
           (plist-to-plist-string property) desc
           ))
    (cformatt 
     "All data records inserted. Created genome information table.")

    t

    ))




;;;; PROCESSING THE PROTEINS INFORMATION TABLE.

;;; Step 1:  Reads the proteins.tbl file into a table data object.
;;; Step 2:  Verify that each protein name is unique.
;;; Step 3:  Verify that the genome component pointed at by each gene exists.
;;; Step 4:  Verify the properties field entries and canonicalize them.
;;; Step 5:  Verify the ARCHITECTURE field entries, and canonicalize them.
;;; Step 6:  Augment the property lists with ARCHITECTURE information.
;;; Step 7:  Create the database table to hold the gene information.
;;; Step 8:  Insert each record, suitably modified, into the database.

(defun proteins-tbl-in-subdir-to-db 

       (tbl-file info-db-table-name genes-db-table-name
                 &aux
                 tbl tbl-keys tbl-properties tbl-descriptions
                 (tbl-key-field *generic-information-table-key*)
                 gene-pointers genes-info-keys
                 (genes-field "GENE")
                 (desc-field *generic-description-field*)
                 (error? nil)
                 (proteins-prefix (organism-element-prefix :proteins))
                 (genes-prefix (organism-element-prefix :genes))
                 )

  (block exit

    ;; Step 1

    (cformatt "Reading proteins table file: ~A" (namestring tbl-file))
    (setq tbl (read-upload-table tbl-file))
    (flet ((oops (column-name)
             (unless (is-header-column? tbl column-name)
               (error "Proteins table file has no ~A column!" column-name))))
      (oops tbl-key-field)
      (oops genes-field)
      )
    (cformatt "Proteins table file read.")

    ;; Step 2

    (cformatt "Verifying that each protein table key is unique.")
    (setq tbl-keys  (table-data-col tbl tbl-key-field))
    (unless (verify-upload-key-uniqueness tbl-keys "Proteins table")
      (return-from exit nil))
    (setq tbl-keys 
          (add-organism-element-prefix-to-keys tbl-keys proteins-prefix))
    (setq *proteins-keys* tbl-keys)

    ;; Step 3

    (cformatt "Verifying that each pointer to a gene component is valid")
    (setq gene-pointers (table-data-col tbl genes-field))
    (setq gene-pointers 
          (add-organism-element-prefix-to-keys gene-pointers genes-prefix))
    (setq genes-info-keys 
          (all-column-values genes-db-table-name tbl-key-field))
    ;; Hack for debugging w/o database.
    (when (null genes-info-keys) (setq genes-info-keys *genes-keys*))
    (let ((bad-pointers 
           (check-if-any-a-is-not-in-b 
            gene-pointers genes-info-keys :test 'string-equal
            )))
      (when bad-pointers
        (cformatt 
         "*Ruh roh. Proteins table contains ~D invalid gene references."
         (length bad-pointers))
        (utils::limited-error-message-loop
         10 "... and more invalid gene references."
         for pointer in bad-pointers do
         (cformatt "  Invalid gene reference: ~A" pointer))
        (return-from exit nil)
        ))

    ;; Step 4

    (multiple-value-setq (tbl-properties error?)
        (property-lists-from-upload-table-data
         tbl
         (list 
          *generic-information-table-key* 
          genes-field *generic-description-field*)
         "proteins table file"))
    (when error? (return-from exit nil))

    ;; Step 5

    (cformatt "Creating database table ~A" info-db-table-name)
    (create-proteins-information-table info-db-table-name)
    (push info-db-table-name *new-organism-tables*)
    (cformatt "Created database table ~A" info-db-table-name)

    ;; Step 6

    (cformatt "Inserting ~D records into proteins table" (length tbl-keys))
    (setq tbl-descriptions (get-table-descriptions tbl desc-field))
    (loop for key in tbl-keys
          for pointer in gene-pointers
          for property in tbl-properties
          for desc in tbl-descriptions
          do
          (insert-record-into-proteins-information-table
           info-db-table-name key pointer 
           (plist-to-plist-string property) desc
           ))
    (cformatt 
     "All data records inserted. Created proteins information table.")

    t

    ))


(defun property-lists-from-upload-table-data 
       (tbl non-property-column-names table-name-string)
  (cformatt "Verifying properties read from ~A" table-name-string)
  (multiple-value-bind (plists error-count)
      (property-lists-from-tbl-data tbl non-property-column-names :list)
    (if (plusp error-count)
        (values plists t)
      (progn
        (cformatt "~A properties verified." table-name-string)
        (values plists nil)
        ))))
        
  
(defun get-table-descriptions (tbl &optional (desc-field "DESCRIPTION"))
  (if (is-header-column? tbl desc-field)
      (table-data-col tbl desc-field)
    (make-list (table-data-nrows tbl) :initial-element "")))


(defun verify-upload-key-uniqueness (keys table-name)
  (let ((non-unique-keys 
         (check-for-duplicates keys :test 'string-equal)))
      (when non-unique-keys
        (cformatt "*Ruh roh.  ~A contains duplicate keys!" table-name)
        (utils::limited-error-message-loop 
         10 "... and more duplicates."
         for key in non-unique-keys do
         (cformatt "  Duplicate key: ~A" key)))
      (null non-unique-keys)
      ))


(defun verify-direction-from-and-to-strings (d from to)
  (let ((from-value nil) (to-value nil))
    (block exit
      (flet ((oops (reason) 
               (return-from exit (values from-value to-value reason))))
        (handler-case
            (setq from-value (parse-integer from))
          (error () (oops "FROM is not an integer")))
        (cond
         ;; If FROM is -1, return TO as -1 also.
         ((= -1 from-value) (values from-value -1 nil))
         ((zerop from-value) (oops "Neither FROM nor TO can be zero!"))
         ((not (plusp from-value))
          (oops "Negative values for FROM or TO are not allowed!"))
         (t
          (unless (or (string-equal d "F") (string-equal d "B"))
            (oops "DIRECTION must be 'F' or 'B'"))
          (handler-case
              (setq to-value (parse-integer to))
            (error () (oops "TO is not an integer")))
          (cond
           ((= -1 to-value) (oops "TO cannot be -1 unless FROM is -1 as well"))
           ((zerop to-value) (oops "Neither TO nor FROM can be zero!"))
           ((not (plusp to-value))
            (oops "Negative values for TO or FROM are not allowed!"))
           (t (values from-value to-value nil))
           )))))))



