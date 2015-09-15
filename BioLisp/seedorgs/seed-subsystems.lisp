;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2009 JP Massar                                            |
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

;; Full list of subsystems:
;; http://edwards.sdsu.edu/FIG/subsys.cgi

(defvar *enable-subsystem-menus* t)

#-:lispworks
(def-aframe-type subsystem (aframe))

(defun subsystem-name->frame-name (sname)
  (substitute 
   #\- #\Space 
   (substitute 
    #\[ #\( 
    (substitute #\] #\) sname)
    )))

(defun existing-subsystem-frames ()
  (when *enable-subsystem-menus*
    (existing-subsystem-frames-sf)
    ))

(defun canonicalize-subsystem-classification-name (cname)
  (if (or (null cname) (eq :null cname) (every 'whitespacep cname))
      "Unclassified"
    cname
    ))

(defun assign-new-subsystem-id (sframe)
  (when (find-package :vpl)
    (unless (#^subsystem-id sframe)
      (setf (#^subsystem-id sframe) 
            (forward-package-funcall 
             :vpl :new-unique-id :subsystem-id
             )))))

(defun download-subsystem-info-and-create-frames ()
  (when (and *enable-subsystem-menus* user::*real-seed-available?*)
    (multiple-value-bind (data columns)
        (seed-query "select * from subsystem_metadata")
      (let* ((sname "subsystem")
             (subsystem-col (position sname columns :test 'string-equal)))
        (unless subsystem-col
          (error "Subsystem metadata table has changed!"))
        (let* ((col-frames
                (loop for col in columns 
                      if (string-equal col sname)
                      collect nil
                      else collect (frame-fnamed col t)
                      ))
               (existing-subsystem-frames (existing-subsystem-frames))
               (shash 
                (let ((hash (make-hash-table :test 'eq)))
                  (loop for sf in existing-subsystem-frames 
                        do (setf (gethash sf hash) nil)
                        finally (return hash)
                        ))))
          (loop 
           for record in data 
           as sname = (nth subsystem-col record)
           as fname = (subsystem-name->frame-name sname)
           as existing-frame = (frame-fnamed fname nil 'subsystem)
           do
           (cond 
            ;; there's already a subsystem frame; no need to 
            ;; recreate it, but it needs a new unique id
            (existing-frame  
             (assign-new-subsystem-id existing-frame)
             (setf (gethash existing-frame shash) t))
            (t 
             (let ((new-frame (frame-fnamed fname t 'subsystem)))
               ;; subsystem that has no frame yet 
               (loop 
                for cframe in col-frames
                for datum in record
                do
                (unless (null cframe) (setf (slotv new-frame cframe) datum))
                )
               (setf (#^seed-name new-frame) sname)
               (setf 
                (#^class_1 new-frame) 
                (canonicalize-subsystem-classification-name 
                 (#^class_1 new-frame)))
               (setf 
                (#^class_2 new-frame) 
                (canonicalize-subsystem-classification-name
                 (#^class_2 new-frame)))
               (assign-new-subsystem-id new-frame)
               (setf (gethash new-frame shash) t)
               new-frame
               ))))
          ;; remove any subsystem frames that exist in the acache database
          ;; but are no longer in the seed
          (let ((count 0))
            (maphash 
             (lambda (sframe in-seed?)
               (if (not in-seed?) 
                   (progn
                     (cformatt 
                      "Subsystem ~A no longer in seed.  Removing..." 
                      (fname sframe))
                     (delete-frame sframe))
                 (incf count)
                 ))
             shash
             )
            (cformatt "~D subsystems now in Biobike database" count)
            ))))))
                         
             

;;; Returns a list of lists of lists of the form 
;;; ((class1-name1 ((class2-name1 (s1 s2 ...)) (class2-name2 (s1 s2 ...)) ...))
;;;  (class1-name2 ...)
;;;  ...)

(defun classify-subsystems (slist)
  (let ((h1 (make-hash-table :test 'equalp)))
    (loop for subsystem-frame in slist 
          as class1 = (#^class_1 subsystem-frame)
          do
          (setf (gethash class1 h1) (make-hash-table :test 'equalp))
          )
    (loop for subsystem-frame in slist
          as class1 = (#^class_1 subsystem-frame)
          as class2 = (#^class_2 subsystem-frame)
          as subhash = (gethash class1 h1)
          do
          (push subsystem-frame (gethash class2 subhash))
          )
    (sort 
     (lmaphash 
      (lambda (class1 subhash)
        (list 
         class1
         (sort 
          (lmaphash 
           (lambda (class2 subsystem-list)
             (list 
              class2
              (sort 
               (copy-list subsystem-list) 
               'string-lessp
               :key (lambda (s) (#^fname s))
               )))
           subhash
           )
          'string-lessp
          :key 'first
          )))
      h1
      )
     'string-lessp
     :key 'first
     )))

(defmethod slotv ((frame subsystem) (slot (eql #$features)))
  (if (frames::frame-has-slot? frame slot)
      (frames::%slotv frame slot)
    (progn
      (setf (#^features frame) (features-of-subsystem frame))
      (determine-subsystem-feature-frames frame)
      )))

(defun features-of-subsystem (frame)
  (let* ((seed-subsystem-name (#^seed-name frame))
         (feature-ids 
          (seed-query 
           "select protein from subsystem_index where subsystem = ~S" 
           seed-subsystem-name
           )))
    (mapcar 'first feature-ids)
    ))

(defun determine-subsystem-feature-frames (frame)
  (let* ((features (#^features frame))
         (fids-for-mysql (fids-to-mysql-in-format features))
         (query-result
          (seed-query 
           "select genome,id from features where id in ~A" fids-for-mysql
           )))
    (setf 
     (#^feature-frames frame)
     (loop for (gid peg) in query-result
           collect
           (let ((orgf (gid->orgf gid)))
             (cond
              ((null orgf) :seed-organism)
              ((#^organism-loaded? orgf) 
               (find peg (#^genes orgf) :key #^seed-id :test 'string-equal))
              (orgf :master-list)
              ))))))
              
                

    