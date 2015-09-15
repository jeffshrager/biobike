;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

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

(defparameter *sequence-locations-table-name* "sequence_locations")
(defparameter *genes-table-name* "genes")


;;; The columns of the SEQUENCE LOCATION table we are creating are
;;;   
;;;  organism
;;;  id
;;;  start
;;;  end
;;;  length
;;;  direction

(defun create-sequence-location-table (&key (if-exists nil))
  (block exit
    (let ((existing-tables (mapcar #'car (esql "show tables"))))
      (when (member *sequence-locations-table-name* existing-tables 
                    :test #'equal)
        (case if-exists
          (:error 
           (error "Table ~S already exists.  Use DROP table first."
                  *sequence-locations-table-name*))
          ((:delete :supersede :supercede)
           (esql (format nil "drop table ~A" 
                         *sequence-locations-table-name*)))
          ((nil t) (return-from exit nil))
          )))
    (esql 
     (format 
      nil 
      (one-string
       "create table ~A (" 
       "organism varchar(50),"
       "id varchar(30), "
       "start integer, "
       "end integer, "
       "length integer, "
       "direction char(1), "
       "unique(id)"
       ")"
       )
      *sequence-locations-table-name*
      ))))


;;; Add location data to the table for a set of genes belonging
;;; to a given organism.

(defun populate-sequence-location-table (organism-name data)
  (create-sequence-location-table :if-exists nil)
  (let ((*db-verbose* nil))
    (loop for record in data do
          (esql 
           (format 
            nil
            "insert into ~A values (~S,~S,~D,~D,~D,~S)"
            *sequence-locations-table-name*
            organism-name
            (first record) (second record) (third record) 
            (fourth record) (fifth record)
            )))))
  
(defun depopulate-sequence-location-table (organism-name)
  (esql
   (format
    nil
    "delete from ~A where organism=~S"
    *sequence-locations-table-name*
    organism-name
    )))


(defun populate-gene-table (organism-name data)
  (declare (ignore organism-name))
  (let ((*db-verbose* nil))
    (loop for record in data do
          (esql
           (format 
            nil
            "insert into ~A values (~S,~S,~S,~S,~S)"
            *genes-table-name*
            (first record) (second record) (third record)
            (fourth record) (fifth record)
            )))))

(defun depopulate-genes-table (organism-name)
  (esql 
   (format nil "delete from ~A where organism=~S"
           *genes-table-name* organism-name
           )))

;;; Retrieve all the location data for all the genes belonging
;;; to an organism.  This is used by USE-ORGANISM to cache this
;;; information in gene frames.

(defun select-organism-sequence-location-info (organism-name)
  (esql (format 
         nil
         (one-string
          "select id,start,end,length,direction from ~A "
          "where organism=~S")
         *sequence-locations-table-name*
         organism-name
         )))

