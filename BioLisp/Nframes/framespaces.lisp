; -*- Package: aframes; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :aframes)

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

#+allegro
;; FIXME 2010-05-28 <peter@gigamonkeys.com> -- there were only a
;; couple places that used without-interrupts. I'm pretty sure that's
;; not right. So if this code is still used, it may need a theading audit.
(defvar *frame-system-lock* (mp:make-process-lock :name "Sframes lock.")
  "The lock to protect a couple sections of code that used to be protected by without-interrupts.")


(defvar *framespace-worlds* nil "The set of all possible framespace tables.")
(defvar *framespace-table* nil "The current set of framespaces")

(defconstant *default-framespace-name* "")

(defconstant *framespace-name-key* :framespace-name
  #.(one-string-nl
     "How we find out the name of a framespace or a framespace table"
     "from the framespace object itself"))
(defconstant *framespace-pretty-name-key* :framespace-pretty-name
  #.(one-string-nl
     "How we find out the pretty name of a framespace or a framespace table"
     "from the framespace object itself"))

(defparameter *standard-framespace-table-initial-size* 32)
(defparameter *standard-framespace-initial-size* 800)

(defparameter *standard-framespace-table-name* "standard-framespace-table"
  #.(one-string-nl
     "Used as the name of the standard framespace table and as a prefix"
     "for the names of framespaces within the standard framespace world."))

(defconstant *max-framespace-name-length* 8 
  #.(one-string-nl
     "The number of characters allowed before the framespace prefix separator"
     "such that beyond that the prefix is no longer considered to be"
     "a framespace name, but just the beginning of the name component."))

(defconstant *framespace-separator* #\.)

(defvar *frames-system-frames* nil)

;;; STANDARD FRAME SYSTEM BOOTSTRAP


(defun initialize-standard-frame-system ()
  "Create the frame system if it isn't already in existence."
  (unless (framespace-table-loaded? *standard-framespace-table-name*)
    (setq *framespace-table*
          (create-framespace-table-named 
           *standard-framespace-table-name*
           :if-exists? (if user::*acache-running* :ok :error))
           )))
     
(defun create-framespace-table-named
       (name 
        &key
        (size *standard-framespace-table-initial-size*)
        (default-framespace-size *standard-framespace-initial-size*)
        (if-exists? :error))
  "Create a framespace table and a default framespace"
  (setq name (string-downcase name))
  (block exit
    (let ((table (framespace-table-exists? name)))
      (when table
        (case if-exists?
          (:error (error "Framespace table named ~A already exists!" name))
          (otherwise (return-from exit table))
          )))
    ;; Create the framespace table
    (let ((new-framespace-table (create-a-frame-table-object name :size size)))
      (set-frame-table-name new-framespace-table name)
      ;; Create the default framespace within the framespace table
      (create-framespace-named
       *default-framespace-name* 
       :ft new-framespace-table
       :table-name (one-string name "." "DEFAULT")
       :size default-framespace-size
       )
      new-framespace-table
      )))
    

;;; FRAMESPACE CREATION

;;; A framespace always has at least three key/value pairs:
;;;  -- :frame-table, which points back at the containing frame table
;;;  -- :framespace-name, which is the canonical name
;;;  -- :framespace-pretty-name, which is the name the user supplied
;;;  -- :map-name, which is the name of the map for this framespace
;;;     within allegrocache (irrelevant for pseudo-allegrocache)

(defun create-framespace-named 
       (name 
        &key
        (ft *framespace-table*)
        (table-name 
         (one-string (frame-table-name ft) "." (string-downcase name)))
        (size *standard-framespace-initial-size*)
        &aux cname
        )
  #.(one-string-nl
     "Create a framespace in framespace table FT which can be accessed"
     "via that framespace table using the key (string-downcase NAME)")
  (setq cname (string-downcase name))
  (setq table-name (string-downcase table-name))
  (let ((new-framespace (create-a-framespace-object table-name :size size)))
    (set-framespace-name new-framespace cname)
    (set-framespace-pretty-name new-framespace name)
    (set-fmap-value new-framespace :map-name table-name)
    (set-fmap-value new-framespace :frame-table ft)
    ;; Make the new framespace be contained by the frame table.
    (set-fmap-value ft cname new-framespace)
    new-framespace
    ))
         
(defun frame-table-name (ft) 
  (fmap-value ft *framespace-name-key*))
(defun set-frame-table-name (ft name)
  (set-fmap-value ft *framespace-name-key* name))

(defun framespace-name (fs)
  (fmap-value fs *framespace-name-key*))
(defun set-framespace-name (fs name)
  (set-fmap-value fs *framespace-name-key* name))
(defun framespace-pretty-name (fs)
  (fmap-value fs *framespace-pretty-name-key*))
(defun set-framespace-pretty-name (fs name)
  (set-fmap-value fs *framespace-pretty-name-key* name))  

(defun framespace-table-exists? (name)
  #.(one-string-nl
     "Returns the framespace table if it exists, or NIL"
     "If it exists, it is added to the list of framespace tables if"
     "it is not already there (which can only be true when running ACACHE")
  (or (framespace-table-loaded? name)
      (and cl-user::*acache-running*
           (let ((ft (frame-table-exists-in-acache-db? name)))
             (when ft (push ft *framespace-worlds*))
             ft
             ))))

(defun framespace-table-loaded? (name)
  (loop for ft in *framespace-worlds* do
        (when (string-equal (frame-table-name ft) name) (return ft))))

(defun frame-table-exists-in-acache-db? (name)
  #.(one-string-nl
     "Returns an open MAP if that map exists, or NIL"
     "The map must satisfy a consistency check to insure that it is"
     "in fact a frame table, not just some random map named NAME.")
  (let ((named-map
         (handler-case
             (forward-funcall 
              'open-map name :if-does-not-exist :error :if-exists :open)
           (error () nil)
           )))
    (when named-map
      (let ((map-name (frame-table-name named-map)))
        (unless map-name
          (error 
           "A MAP called ~A exists but it does not appear to be a frame table!"
           name
           ))
        (unless (string-equal map-name name)
          (error
           "A Frame table named ~A exists, its ~S value is not its name!"
           name *framespace-name-key*
           ))))
    named-map
    ))

(defun framespace-name->framespace 
       (name &optional (frame-table *framespace-table*))
  "Framespace associated with NAME in FRAME-TABLE (default current frame world)"
  (let ((cname nil))
    (
     #+allegro mp:with-process-lock #+allegro (*frame-system-lock*)
     #-:allegro progn
     (setq cname (copy-seq (%substring->fname name 0 (length name)))))
    (let ((framespace (fmap-value frame-table cname)))
      (when (null framespace)
        (error "Framespace name ~A does not exist." name))
      framespace
      )))

(defun framespace->framespace-name (framespace)
  "The name of FRAMESPACE (the canonicalized string used as a prefix)"
  (fmap-value framespace *framespace-name-key*))

(defun default-framespace (&optional (framespace-table *framespace-table*))
  (fmap-value framespace-table *default-framespace-name*))

(defun to-framespace (x allow-sequences?)
  (cond
   ((stringp x) (framespace-name->framespace x))
   ((eq x :default) (framespace-name->framespace *default-framespace-name*))
   ((symbolp x) (framespace-name->framespace (string x)))
   ((or (typep x 'ac-map) 
        (hash-table-p x)
        (and allow-sequences? (typep x 'sequence)))
    x)
   (t (error "Cannot convert ~A to a framespace object" x))
   ))
    
    

(defun create-a-frame-table-object 
       (name &key (size *standard-framespace-initial-size*))
  (create-a-map name size :frame-table))

(defun create-a-framespace-object 
       (name &key (size *standard-framespace-initial-size*))
  (create-a-map name size :framespace))

(defun create-a-map (name size type)
  (unless name (error "Internal error, need name for CREATE-A-MAP"))
  (unless (integerp size) (error "Internal error, need size for CREATE-A-MAP"))
  (if user::*acache-running* 
      (forward-funcall 
       'open-map name :if-does-not-exist :create :if-exists :open)
    (ecase type
      (:frame-table
       (vif (existing-table (framespace-table-exists? name))
            (error 
             "Ierror: CREATE-A-MAP called to create existing FRAME TABLE ~A" 
             name)
            (make-string-equal-hash-table :size size)
            ))
      (:framespace
       (vif (existing-framespace 
             ;; *framespace-table* is NIL when system first boots up.
             (and *framespace-table* 
                  (framespace-exists? name *framespace-table*)))
            (error 
             "Ierror: CREATE-A-MAP called to create existing FRAMESPACE ~A" 
             name)
            (make-string-equal-hash-table :size size)            
            )))))

(defun framespace-exists? (name &optional (framespace-table *framespace-table*))
  (fmap-value framespace-table name))



(defmethod fmap-value ((fmap ac-map) name-key) 
  (map-value fmap name-key))

(defmethod fmap-value ((fmap hash-table) name-key)
  (gethash name-key fmap))


(defmethod set-fmap-value ((fmap ac-map) name-key frame-value)
  (setf (map-value fmap name-key) frame-value))

(defmethod set-fmap-value ((fmap hash-table) name-key frame-value)
  (setf (gethash name-key fmap) frame-value))


(defmethod add-fmap-value ((fmap ac-map) name-key value &key (test 'equal))
  (pushnew value (map-value fmap name-key) :test test))

(defmethod add-fmap-value ((fmap hash-table) name-key value &key (test 'equal))
  (pushnew value (gethash name-key fmap) :test  test))


(defmethod remhash-fmap-value ((fmap ac-map) name-key)
  (remove-from-map fmap name-key))

(defmethod remhash-fmap-value ((fmap hash-table) name-key) 
  (remhash name-key fmap))


(defmethod expunge-fmap-entry ((fmap ac-map) name-key)
  (remhash-fmap-value fmap name-key))

(defmethod expunge-fmap-entry ((fmap hash-table) name-key)
  (remhash-fmap-value fmap name-key))


(defun %intern-frame (fmap frame name)
  (set-fmap-value fmap name frame))

(defun %unintern-frame (fmap fname)
  (expunge-fmap-entry fmap fname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mapft (function ft)
  (cond 
   ((typep ft 'ac-map) (map-map function ft))
   ((typep ft 'hash-table) (maphash function ft))
   (t (error "Object ~A is not a frametable or framespace." ft))
   ))

(defun fmap-count (fmap)
  (cond 
   ((typep fmap 'ac-map)
    (let ((count 0))
      (map-map (lambda (k v) (declare (ignore k v)) (incf count)) fmap)
      count))
   ((typep fmap 'hash-table)
    (hash-table-count fmap))
   (t (error "Object ~A is not an fmap" fmap))
   ))
          


(defun describe-framespace-table (&key (fst *framespace-table*))
  (let ((fst-name (frame-table-name fst)))
    (formatt "~%~%FRAMESPACE TABLE '~A' (~D Framespaces): ~%~%" 
             fst-name (1- (fmap-count fst)))
    (mapft 
     (lambda (fskey fs) 
       (unless (keywordp fskey)
         (formatt "~10@a : < Fmap of ~5D entries > ~%" fskey (fmap-count fs))))
     fst
     )))

(defun describe-framespace
       (fs &key (frame-limit 5) (frame-table *framespace-table*) &aux size)
  (when (stringp fs)
    (setq fs (framespace-name->framespace fs frame-table)))
  (setq size (fmap-count fs))
  (let ((keys nil))
    (mapft 
     (lambda (fskey fname) 
       (when (keywordp fskey) (push (list fskey fname) keys)))
     fs)
    (formatt "~%~%FRAMESPACE '~A' (~D Frames): ~%~%"
             (framespace-pretty-name fs) size)
    (loop for (key value) in keys do
          (formatt "  ~A : ~A~%" key value))
    (terpri)
    (let ((count 0))
      (block exit
        (mapft 
         (lambda (fname frame)
           (unless (keywordp fname)
             (if (> (incf count) frame-limit)
                 (return-from exit nil)
               (formatt "  ~A" frame)
               )))
         fs
         ))
      (when (< count size) (formatt " ...")))
    (terpri)
    (terpri)
    ))
      
       
          

         
  
