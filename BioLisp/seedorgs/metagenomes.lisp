;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2011 JP Massar
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

(defun loaded-metagenomes () (copy-list *loaded-metagenomes*))
(defun available-metagenomes () (copy-list *available-metagenomes*))

(defun toplevel-metagenome-directory ()
  (append-subdir 
   (append-subdir user::*bioetc-directory* "data")
   "metagenomes"
   ))

(defun metagenome-directory (metagenome-name)
  (append-subdir (toplevel-metagenome-directory) metagenome-name))

(defun available-metagenome-datasets () 
  (let* ((toplevel-metagenome-dir (toplevel-metagenome-directory))
         (mgfile (merge-pathnames "foo.txt" toplevel-metagenome-dir)))
    (handler-case 
        (ensure-directories-exist mgfile)
      (error 
       (c)
       (error "Oops! ~A does not exist and cannot create it! Actual error: ~A"
              toplevel-metagenome-dir c
              )))
    (let ((directory-listing 
           (directory-with-subdirs-in-directory-form toplevel-metagenome-dir)))
      (loop for entry in directory-listing 
            when 
            (pathname-names-directory? entry)
            collect (lastelem (pathname-directory entry))
            ))))

(defun metagenome-plist-file (mgn)
  (make-pathname
   :name "plist"
   :type "lisp"
   :defaults (metagenome-directory mgn)
   ))

(defun preload-metagenomes ()
  (setq *available-metagenomes* nil)
  (let ((metagenomes 
         (loop for mg in (available-metagenome-datasets)
               as mframe = (frame-fnamed mg t 'seed-metagenome)
               as plist-file = (metagenome-plist-file mg)
               collect
               (progn
                 (populate-frame-slots-from-plist-file mframe mg plist-file)
                 (pushnew mframe *available-metagenomes*)
                 (when (#^metagenome-loaded? mframe) 
                   (pushnew mframe *loaded-metagenomes*))
                 mframe
                 ))))
    (db.ac::commit)
    metagenomes
    ))

(defun load-metagenome (mf &key (force? nil))
  (when (and (#^metagenome-loaded? mf) (not force?))
    (cformatt "Metagenome ~A already loaded!" mf)
    (return-from load-metagenome mf)
    )
  ;; set up the read subtype and a general macro to iterate over the subtype
  (cformatt "Loading read frames for metagenome ~A" mf)
  (let* ((mfdir (metagenome-directory (#^fname mf)))
         (reads-dir (append-subdir mfdir "reads"))
         (reads-fasta-file (merge-pathnames "reads.fasta" reads-dir))
         (prefix (#^organism-prefix mf))
         (read-subtype 
          (intern 
           (string-upcase 
            (s+ (subseq prefix 0 (1- (length prefix))) "-READ"))
           :bio
           )))
    (cformatt "Reads fasta file: ~A" reads-fasta-file)
    (eval `(def-aframe-type ,read-subtype (seed-metagenome-read)))
    (setf (#^read-subtype mf) read-subtype)
    (let ((count 0))
      (with-fasta-db (dbh reads-fasta-file)
        (with-fasta-db-keys (key dbh)
          (destructuring-bind (descriptor sequence)
              (find-fasta-record dbh key)
            (let ((read-frame 
                   (frame-fnamed (s+ prefix descriptor) t read-subtype)
                   ))
              (setf (#^metagenome read-frame) mf)
              (setf (#^id read-frame) descriptor)
              (setf (#^sequence read-frame) sequence)
              (when (zerop (mod count 10000))
                (cformatt "Read frame ~D: ~A" count read-frame))
              ))
          (incf count)
          ))
      (cformatt "Created ~D read frames for ~A" count mf)
      (setf (#^reads-file mf) (namestring reads-fasta-file))
      (setf (#^nreads mf) count)
      (pushnew mf *loaded-metagenomes*)
      (setf (#^metagenome-loaded? mf) t)
      (db.ac::commit)
      mf
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Iteration macros and setup for bio::xloop (bbl::loop)

(defmacro for-each-metagenome-read ((read-symbol mf) &body body)
  `(db.ac::doclass 
    (,read-symbol (#^read-subtype ,mf))
    ,@body
    ))

(defun map-metagenome-reads (f mf)
  (for-each-metagenome-read 
   (read mf)
   (funcall f read)
   ))

(defstruct (metagenome-read-iter (:include iterator)))

(defmethod iter-init ((obj seed-metagenome))
  (make-metagenome-read-iter 
   :obj (db.ac::create-class-cursor (#^read-subtype obj))
   :state nil
   ))

(defmethod iter-next? ((iter metagenome-read-iter))
  (let* ((cursor (metagenome-read-iter-obj iter))
         (next (db.ac::next-class-cursor cursor)))
    (setf (metagenome-read-iter-state iter) next)
    (when (null next) (db.ac::free-class-cursor cursor))
    (not (null next))
    ))

(defmethod iter-next ((iter metagenome-read-iter))
  (metagenome-read-iter-state iter)
  )

#+test
(xloop init c = 0
       for r in mf
       do (incf c (length (#^sequence r))) 
       finally (return c)
       )

(defun unload-metagenome (mg &key (delete-metagenome-frame? nil))
  (when (#^metagenome-loaded? mg) 
    (for-each-metagenome-read (r mg) (db.ac::delete-instance r))
    (setf (#^metagenome-loaded? mg) nil))
  (setq *loaded-metagenomes* (delete mg *loaded-metagenomes*))
  (setf (#^nreads mg) nil)
  (let ((mgname (#^fname mg)))
    (when delete-metagenome-frame? 
      (setq *available-metagenomes* (delete mg *available-metagenomes*))
      (db.ac::delete-instance mg))
    (db.ac::commit)
    (if delete-metagenome-frame? mgname mg)
    ))
  
                            
   
  
                            
                   
        
      


            
            
    
        

    

