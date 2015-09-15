;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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

#|

This file needs to export three pieces of functionality:

  -- Build.
  This creates the various knowledge base frames from the raw data
  files.  It then instantiates the knowledge base by creating additional 
  data structures, etc, as necessary.  Finally, it threads the knowledge bases
  together as appropriate.  This KDB functionality is associated 
  with the MAKE-<kdb>-FRAMES function in each kdb.

  -- Build/Save.
  This creates the knowledge base frames as above and then stores them
  in a persistent store being either the acache database (via a simple commit)
  or into frame files via the frame dumper.

  -- Load.
  This loads the previously stored knowledge base frames created above,
  and does any other work necessary to set up the particular knowledge base
  such as creating lists or hash tables of frames or indices, that could not
  be stored to the persistent store.  (It should be the case that the
  threading done by the build procedure is storable to the persistent store, 
  but that data structure is not associated with frames such as independent hash
  tables are not stored persistently and must be recreated.  
  The loading itself is a noop when acache is running because loading
  the acache database has already effectively loaded all the frames.  
  For pseudo-acache this would use the (currently not working) frame loader.
  This KDB functionality is associated with the INSTANTIATE-<kdb>-FRAMES
  function in each kdb.    

We are removing all notions of having a complicated 'make' procedure
which would magically do all these steps as necessary as was done in
the old system.  It simply assumed that the installer or administrator
of the system will build and save the knowledge frames as is
necessary, and that developers will know how to use the build
capability appropriately to load new data as necessary.

Which knowledge bases are dealt with by the above commands is controlled by
the variable 

*existing-kdb-names*

Simply change the value of this variable to build, save, or load
subsets of the knowledge bases or individual ones.  

|#


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *kdb-api-symbols*
    '(
      build-kdb-frames
      build-and-save-kdb-frames
      load-kdb-frames
      *go-frames*
      goid->frame
      *kegg-frames*
      find-kegg-frames
      *ocelot-frames*
      ))

  (export *kdb-api-symbols* (find-package :biolisp)))


(defvar *existing-kdb-names* '(:go :ocelot :kegg))

(defvar *kdb-verbose* t)

;;; Do not compute this at load time because if we dump an image
;;; it would not be correct if the image were run elsewhere.
(defun kdb-frames-directory () (cl-user:translate-simple-lp "bioetc:data;"))

(defvar *kdbs-loaded?* nil)

(defmethod kdb-directory ((kdb t))
  (append-subdir (kdb-frames-directory) (string-downcase kdb)
   ))

(defgeneric kdb-source-files (kdb))

(defgeneric kdb-toplevel-frames (kdb)
  (:documentation
   #.(one-string-nl
      "Returns a list of frames that the knowledge base considers its"
      "top level frames (generally the things at or near the top of an"
      "ISA or PART-OF hierarchy.")))

(defmethod kdb-toplevel-frames ((kdb t)) nil)


(defmethod kdb-make-function ((kdb t))
  ;; find a symbol like MAKE-GO-FRAMES
  (find-symbol 
   (one-string "MAKE-" (string-upcase (string kdb)) "-FRAMES")
   :bio))

(defmethod kdb-instantiate-function ((kdb t))
  ;; find a symbol like INSTANTIATE-GO-FROM-LOADED-FRAMES
  (find-symbol
   (one-string 
    "INSTANTIATE-" (string-upcase (string kdb)) "-FROM-LOADED-FRAMES")
   :bio))
    
(defmethod kdb-list-variable ((kdb t))
  ;; find a symbol like *GO-FRAMES*
  (find-symbol
   (one-string "*" (string-upcase (string kdb)) "-FRAMES*")
   :bio))

;; This is only relevant to pseudo-acache
(defun kdb-frames-storage-directory ()
  (append-subdir (kdb-frames-directory) "pkdbframes"))
 

(defun build-kdb-frames (&key (verbose? *kdb-verbose*))
  (loop for kdb in *existing-kdb-names* do
        (funcall (kdb-make-function kdb) :verbose? verbose?))
  (thread-kdbs *existing-kdb-names*)
  (setq *kdbs-loaded?* t)
  )

(defun save-kdb-frames (&key (verbose? *kdb-verbose*))
  (cond
   (user::*acache-running* 
    (when verbose? (cformatt "Commiting frames to acache database."))
    (forward-package-funcall :db.ac :commit))
   (t
    (dump-user-frames (kdb-frames-storage-directory) :verbose? verbose?))
   ))

(defun build-and-save-kdb-frames (&key (verbose? *kdb-verbose*))
  (build-kdb-frames :verbose? verbose?)
  (save-kdb-frames :verbose? verbose?))

(defun load-kdb-frames (&key (verbose? *kdb-verbose*))
  (cond
   (user::*acache-running*
    (cformatt "KDB frames already loaded."))
   (t
    (retrieve-user-frames 
     (kdb-frames-storage-directory) :return-list? nil :verbose? verbose?)
    ))
  (loop for kdb in *existing-kdb-names* do
        (when verbose? (cformatt "Instantiating KDB ~A" kdb))
        (funcall (kdb-instantiate-function kdb)))
  (setq *kdbs-loaded?* t))

(defun create-kdb-toplevel-frames ()
  (loop for kdb in *existing-kdb-names*
        collect
        (list kdb (kdb-toplevel-frames kdb)) 
        ))

(defun make-kdb-fname (string &optional prefix)
  (create-valid-frame-name string :prefix prefix :case-action :capitalize))

(defun thread-kdbs (loaded-kdbs)
  (declare (ignore loaded-kdbs))
  (cformatt "No threading being done at the moment."))

#| 
(defun make-kdb-frames 

       (&key
        (dump-all-user-frames? t)
        (kdb-ids *existing-kdb-names*)
        (redo? nil)
        (verbose? t)
        )

  #.(one-string-nl
     "After this has executed successfully:"
     "  -- All the consitutuent KDB's will have been 'made'."
     "  -- A directory with files containing all the frames of all the"
     "knowledge bases will exist in the \"biol:data;\" directory."
     "  -- All the kdb frames will exist in the standard frames universe."
     "  -- The various *<DB>-FRAMES* lists will be populated."
     "  If REDO? is non-nil the frames file will be always be (re)created from"
     "the kdb files regardless of whether any frames file exist already,"
     "and all the data structures will be repopulated."
     "If REDO? is NIL:"
     "  -- If *kdb-loaded?* is non-nil nothing will be done.")

  (let* ((*kdb-verbose* verbose?)
         ;; The KDB frames file is one directory up from the subordinate
         ;; frames files.  
         (kdb-frames-dir (kdb-frames-storage-directory)))

    (labels ((recreate-kdb-from-scratch ()
               ;; Load in all the KDB's
               (loop for kdb in kdb-ids do
                     (funcall 
                      (kdb-make-function kdb) 
                      :redo? t 
                      :verbose? *kdb-verbose*))
               (thread-kdbs)
               (setq *kdb-loaded?* t)
               ;; Write all the frames out
               (save-kdb-frames-to-disk kdb-frames-dir dump-all-user-frames?))
             (recreate-kdb-from-kdb-frames ()
               (if (frame-dump-exists? kdb-frames-dir)
                   (create-kdb-frames-from-disk-data kdb-frames-dir)
                 (recreate-kdb-from-scratch)
                 )))
      (cond
       (redo? (recreate-kdb-from-scratch) :made)
       ((null *kdb-loaded?*) (recreate-kdb-from-kdb-frames) :read)
       (t 
        (when *kdb-verbose*
          (cformatt "KDB frames already loaded. Not reloading..."))
        nil
        )))))

(defun create-kdb-frames-from-disk-data (directory)
  (when *kdb-verbose* (cformatt "Reading KDB frames from ~A" directory))
  (let ((frames-read (read-kdb-frames-from-disk directory)))
    (when *kdb-verbose*
      (cformatt "~D frames read from ~A" (length frames-read) directory))
    (loop for kdb in *existing-kdb-names* do
          (let ((instantiation-function (kdb-instantiate-function kdb)))
            (when (fboundp instantiation-function)
              (cformatt "Calling ~A instantiation function" kdb)
              (funcall instantiation-function)
              )))
    (setq *kdb-loaded?* t)
    ))

(defun read-kdb-frames-from-disk (directory)
  (retrieve-user-frames directory :return-list? t :verbose? *kdb-verbose*))

(defun save-kdb-frames-to-disk (dir dump-all-user-frames?)
  (if dump-all-user-frames?
      ;; Dump all non-system frames currently in existence.
      (progn
        (when *kdb-verbose* (cformatt "Dumping all user frames to ~A" dir))
        (dump-user-frames dir :verbose? *kdb-verbose*))
    ;; Just dump frames associated with the knowledge bases.
    (progn
      (when *kdb-verbose* (cformatt "Dumping kdb-derived frames to ~A" dir))
      (dump-user-frames
       dir
       :frames 
       (apply 
        'append 
        (mapcar 
         (lambda (x) (symbol-value (kdb-list-variable x)))
         *existing-kdb-names*))
       :verbose? *kdb-verbose*
       ))))

|#




