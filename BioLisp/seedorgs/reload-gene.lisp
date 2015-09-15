;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2011 JP Massar                                            |
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

(defun sync-seed-gene (seed-id &key (commit? t))
  (declare (ignorable commit?))
  (block exit
    ;; We've been asked to reload information for a seed gene with id = seed-id
    ;; First determine whether that gene exists in biobike.  
    (multiple-value-bind (gene-frame orgf error)
        (seed-id->frame seed-id)
      (declare (ignore error))
      ;; It's possible that while the gene doesn't exist, the organism
      ;; it belongs to exists but is not currently loaded.  If so,
      ;; load the organism, and that should load the gene with the
      ;; new information.  
      (when orgf 
        (unless (#^organism-loaded? orgf)
          (load-organism orgf)
          (return-from exit (seed-id->frame seed-id))))
      ;; This seed-id is not known at all to Biobike.  It might
      ;; be a valid gene in the seed but if the organism it belongs to
      ;; is not in the master list Biobike won't know about it.  
      (unless gene-frame 
        (return-from exit (values nil "No frame to update!")))
      ;; any change in the subsystem role will be reflected 
      ;; in the standard seed database in the assigned_functions table.
      (let ((new-subsystem-role (mysql-get-description-for-peg seed-id)))
        (setf (#^subsystem-role gene-frame) new-subsystem-role))
      (let* ((in-data (fids-to-mysql-in-format (list seed-id))))
        (flet ((sync-category (category-type slot)
                 (let* ((records 
                         (mysql-get-categories-info-by-type
                          in-data category-type
                          ))
                        (last-record 
                         (lastelem 
                          (sort 
                           records 'string-lessp :key 'categories-timestamp
                           ))))
                   (when last-record 
                     (let ((new-annotation (categories-annotation last-record)))
                       (setf (slotv gene-frame slot) new-annotation)
                       )))))
          (sync-category "ANN" #$description)
          (sync-category "GEN" #$genetic-name)
          ))
      ;; We're done.  Commit the change permanently to acache.
      #-:lispworks
      (when commit? (db.ac::commit))
      (values gene-frame nil)
      )))

(defun retrieve-gene-info-from-seed-database (seed-id orgf)
  ;; All the information we retrieve gets stored as a property list
  ;; in the *features-info-hash* hash table using the seed-id as sole key.  
  (multiple-value-bind (features-data column-names)
      (seed-gene-info-for-seed-id seed-id)
    ;; Get all the information from the features table
    (process-features-info-msf features-data column-names)
    ;; Get the description data for this gene from the assigned_functions table
    (set-feature-item 
     seed-id :description (mysql-get-description-for-peg seed-id))
    ;; Get the annotation history for this gene.  
    (let* ((annotations-hash (get-seed-info-for-annotations (#^seed-id orgf)))
           (annotation-history (gethash seed-id annotations-hash)))
      (set-feature-item seed-id :annotation annotation-history)
      )))
