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

;;; 'Exported' functionality:
;;;
;;; MAKE-OCELOT-FRAMES
;;; INSTANTIATE-OCELOT-FROM-LOADED-FRAMES
;;; *OCELOT-FRAMES*

;; We have to create the  ocelot package
;; so we can read the file's forms in using READ, as it contains
;; symbols of the form ocelot::<symbol>.  So we'll read the whole
;; ocelot database in the context of the :OCELOT package.

(defpackage :ocelot)


(defparameter *ocelot-file* "cyano6803base.ocelot")

(defvar *ocelot-frames* nil "A list of all frames made from our Ocelot files.")

(defvar *ocelot-symbols->frames* nil
  "Temporary dynamic hash table used while ocelot frames are being created.")

(defvar *ocelot-verbose* nil)

(defvar *ocelot-loaded?* nil)

;;; Creation-Date and Creator are common to every ocelot record, so
;;; pretty much usless.  Display-Coords-2d doesn't seem worth storing
;;; in memory at the moment.

(defvar *ocelot-slot-frames-to-ignore* 
  '(#$OC.Creation-Date #$OC.Creator #$OC.Display-Coords-2d))

(defvar *ocelot-slot-frames* nil)

;;; This follows the same template as that MAKE-GO-FRAMES command.



(defmethod kdb-source-files ((kdb (eql :ocelot)))
  (list (merge-pathnames 
         (make-pathname :name *ocelot-file*)
         (kdb-directory kdb)
         )))

(defmethod kdb-toplevel-frames ((kdb (eql :ocelot)))
  (copy-list (cons #$OC.Frames (#^Subclasses #$OC.Frames))))

(defun make-ocelot-frames (&key (redo? nil) (verbose? t))
  (let* ((*ocelot-verbose* verbose?)
         (ocelot-db-file (first (kdb-source-files :ocelot))))
      (when (or redo? (null *ocelot-loaded?*))
        (forward-funcall 
         'create-ocelot-frames-from-ocelotdb-file ocelot-db-file)
        )))


;;; Read the ocelot db into a list of records, then transform
;;; the records into frames.

(defun create-ocelot-frames-from-ocelotdb-file (file)
  (let ((vb? *ocelot-verbose*))
    (when vb? (cformatt "Reading ocelot records from ~A" file))
    (let ((records (read-ocelot-records file)))
      (when vb? 
        (cformatt "~D records read" (length records))
        (cformatt "Creating OCELOT frames"))
      ;; This creates the frames
      (ocelot-records-to-frames records)
      (setq *ocelot-frames* (all-ocelot-frames))
      (setq *ocelot-loaded?* t)
      (when vb? 
        (cformatt "~D OCELOT frames created." (length *ocelot-frames*)))
      )))

;;; Read the saved set of ocelot frames back into the frames universe.

(defun create-ocelot-frames-from-frames-file (frames-file)
  (let ((vb? *ocelot-verbose*))
    (when vb? (cformatt "Reading OCELOT frames from ~A" frames-file))
    (let ((frames-read (read-ocelot-frames-file frames-file)))
      (when vb? 
        (cformatt 
         "~D OCELOT frames read from ~A" (length frames-read) frames-file)
        ))))

(defun instantiate-ocelot-from-loaded-frames ()
  (setq *ocelot-frames* (all-ocelot-frames))
  ;; We don't build a hash table for OCELOT like we do for GO, since
  ;; there is no ID for each OCELOT frame independent of the name of
  ;; the frame.  In GO, there is a numeric ID and a name, but with OCELOT
  ;; there is only a name.
  (setq *ocelot-loaded?* t)
  t)

(defun all-ocelot-frames ()
  (nmapframes (lambda (f) (and (eq (slotv f #$Source) :ocelot) f))))

;;; The file is a set of forms.  The first form is a header, which
;;; we ignore, and the rest are 'records'. 

(defun read-ocelot-records (file)
  (with-standard-io-syntax
    (let ((*package* (find-package :keyword)))
      (with-open-file (p file :direction :input)
        (read p nil nil)
        (loop for count fixnum from 0
              as record = (read p nil nil)
              until (null record)
              collect (keywordize-symbols-in-ocelot record)
              )))))

(defun keywordize-symbols-in-ocelot (x)
  (cond
   ((or (null x) (eq x :nil)) nil)
   ((or (eq x t) (eq x :t)) t)
   ((symbolp x) (keywordize (string-upcase x)))
   ;; ((symbolp x) (if (keywordp x) x (keywordize x)))
   ((listp x) 
    (cons 
     (keywordize-symbols-in-ocelot (car x))
     (keywordize-symbols-in-ocelot (cdr x))))
   (t x)
   ))


(defun ocelot-records-to-frames (records &aux (verbose? *ocelot-verbose*))
  (setq *ocelot-slot-frames* nil)
  (let ((*ocelot-symbols->frames* (make-hash-table :test 'eq)))
    (when verbose? 
      (cformatt "First pass: Converting ocelot records to frames."))
    (loop for ocelot-record in records for count fixnum from 0 do
          (when (and verbose? (zerop (mod count 1000))) (format t "."))
          ;; Make a frame 
          (let* ((name (first ocelot-record))
                 (ocelot-form-data (third ocelot-record))
                 (frame (new-ocelot-frame name)))
            ;; set up the slots of the frame
            (loop for (slot . value) in ocelot-form-data do
                  (let* ((slot-frame (new-ocelot-frame slot))
                         (slot-value (ocelot-value-for-slot slot value)))
                    (unless (member slot-frame *ocelot-slot-frames-to-ignore*)
                      (setf (slotv frame slot-frame) slot-value)
                      (pushnew slot-frame *ocelot-slot-frames*)
                      ;; all elements of these lists must become frames
                      ;; even if they are not defined in the OC database.
                      (when (or (eq slot-frame #$OC.Parents)
                                (eq slot-frame #$OC.Component-of))
                        (dolist (x value) (new-ocelot-frame x))
                        ))))))
    ;; Finish up.  Turn symbols in the ocelot data that name
    ;; ocelot frames into frames.
    (when verbose? (cformatt "Second pass: Coverting slot data to frames."))
    (maphash 
     (lambda (name-key value-frame)
       (declare (ignore name-key))
       (ocelot-frames-fixup value-frame))
     *ocelot-symbols->frames*)
    (when verbose? 
      (terpri)
      (cformatt 
       "~D ocelot frames created." (hash-table-count *ocelot-symbols->frames*)
       ))))

(defun read-ocelot-frames-file (file)
  (retrieve-user-frames file :return-list? t :verbose? *ocelot-verbose*))

(defun write-ocelot-frames-file (file)
  (dump-user-frames file :frames *ocelot-frames* :verbose? *ocelot-verbose*))

(defun new-ocelot-frame (name-symbol) 
  (vif (f (gethash name-symbol *ocelot-symbols->frames*))
       (values f nil)
       (let ((new-frame (ocelot-frame-from-ocelot-symbol name-symbol t)))
         (setf (gethash name-symbol *ocelot-symbols->frames*) new-frame)
         (setf (slotv new-frame #$Source) :ocelot)
         (values new-frame t)
         )))

(defun ocelot-frame-from-ocelot-symbol (symbol &optional (force? nil))
  (frame-fnamed (make-kdb-fname (string symbol) "OC.") force?))


;;; The idea here is that 
;;;  -- if we have (<slot> <value>)
;;; we want SLOT to have value VALUE unless it is a slot which needs
;;; a list as its value, in which case we want (<value>).
;;; --  if we have (<slot> <v1> <v2> ...) 
;;; we want SLOT have value (<v1> <v2> ...)

(defun ocelot-value-for-slot (slot value-list)
  (cond
   ;; More than one value provided
   ((cdr value-list) value-list)
   ;; One value provided but slot needs a list
   ((ocelot-slot-is-set-valued? slot) value-list)
   ;; One value provided, slot is singleton
   (t (first value-list))
   ))
  
(defun ocelot-slot-is-set-valued? (slot) 
  (case slot
    ((:parents :component-of) t)
    (otherwise nil)
    ))

;;; Convert symbols that are constituents of the value of each ocelot
;;; frame slot to ocelot frames if they name such a frame.
;;; Also tag each ocelot frame as being an ocelot frame.

(defun ocelot-frames-fixup (ocelot-frame)
  (unless (eq (slotv ocelot-frame #$source) :ocelot)
    (error "Internal error.  Ocelot frame has non :ocelot #$Source value!"))
  (for-each-frame-slot (slot value) ocelot-frame
    (let ((fixed-up-value (translate-ocelot-db-value value)))
      (setf (slotv ocelot-frame slot) fixed-up-value)
      (case slot
        (#$OC.parents (setf (slotv ocelot-frame #$isa) fixed-up-value))
        (#$OC.component-of (setf (slotv ocelot-frame #$partOf) fixed-up-value))
        (otherwise nil)
        ))))



;; If VALUE is a list, recurse over its elements.
;; If VALUE is a symbol, if it is the name of an ocelot frame or slot
;; substitute the frame or slot itself for the symbol.

(defun translate-ocelot-db-value (value)
  (cond 
   ((consp value) 
    (if (null (last value 0))
        (mapcar #'translate-ocelot-db-value value)
      ;; The value ends in a dotted pair
      (cons (translate-ocelot-db-value (car value))
            (translate-ocelot-db-value (cdr value)))))
   ((symbolp value) 
    (or (gethash value *ocelot-symbols->frames*) value))
   (t value)))
	 


