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

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *kdb-api-symbols*
    '(
      *go-frames*
      goid->frame
      *kegg-frames*
      find-kegg-frames
      *ocelot-frames*
      ))

  (export *kdb-api-symbols* (find-package :biolisp)))


(defvar *existing-kdb-names* '(:go :kegg :ocelot))

(defvar *kdb-verbose* t)

;;; Do not compute this at load time because if we dump an image
;;; it would not be correct if the image were run elsewhere.
(defun kdb-frames-directory () (cl-user:translate-simple-lp "bioetc:data;"))

(defvar *kdb-loaded?* nil)

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


(defun create-kdb-toplevel-frames ()
  (loop for kdb in *existing-kdb-names*
        collect
        (list kdb (kdb-toplevel-frames kdb)) 
        ))

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


(defun kdb-frames-storage-directory ()
  (append-subdir (kdb-frames-directory) "kdbframes"))
 

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





