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

;;; Frames mentioned textually at some point in the system load that
;;; would otherwise be creates as generic frames instead of OcelotNode frames.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (make-frame-instance 'aframes::OC.OcelotNode "OC.Frames" t)
  (make-frame-instance 'aframes::OC.OcelotNode "OC.Component-Of" t)
  (make-frame-instance 'aframes::OC.OcelotNode "OC.Components" t)
  (make-frame-instance 'aframes::OC.OcelotNode "OC.Polypeptides" t)
  (make-frame-instance 'aframes::OC.OcelotNode "OC.Gene" t)
  )

(in-package :bio)

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

;;; Creation-Date and Creator are common to every ocelot record, so
;;; pretty much usless.  Display-Coords-2d doesn't seem worth storing
;;; in memory at the moment.

(defvar *ocelot-slot-frames-to-ignore* 
  '(:Creation-Date :Creator :Display-Coords-2d))

(defparameter *initial-ocelot-slot-frames-containing-lists* 
  '(:parents :subclasses :component-of :components))

(defparameter *ocelot-slots-that-must-contain-lists-of-frames* 
  '(:parents :subclasses :component-of :components))

(defvar *ocelot-slots* nil)
(defvar *ocelot-slot-frames* nil)
(defvar *ocelot-slot-frames-hash* nil)
(defvar *ocelot-undefined-nodes* nil)
(defvar *ocelot-unparseable-nodes* nil)
(defvar *ocelot-slots-containing-lists* nil)

(frames::def-reciprocal-inverse-slots #$OC.parents #$OC.subclasses)
(frames::def-reciprocal-inverse-slots #$OC.component-of #$OC.components)

;;; This follows the same template as that MAKE-GO-FRAMES command.

(defmethod kdb-source-files ((kdb (eql :ocelot)))
  (list (merge-pathnames 
         (make-pathname :name *ocelot-file*)
         (kdb-directory kdb)
         )))

(defmethod kdb-toplevel-frames ((kdb (eql :ocelot)))
  (copy-list (cons #$OC.Frames (#^OC.Subclasses #$OC.Frames))))

(defun make-ocelot-frames (&key (verbose? t))
  (let* ((*ocelot-verbose* verbose?)
         (ocelot-db-file (first (kdb-source-files :ocelot))))
    (create-ocelot-frames-from-ocelotdb-file ocelot-db-file)
    ))


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
      (when vb? 
        (cformatt "~D OCELOT frames created." (length *ocelot-frames*)))
      )))

(defun instantiate-ocelot-from-loaded-frames ()
  (setq *ocelot-frames* (all-ocelot-frames))
  ;; We don't build a hash table for OCELOT like we do for GO, since
  ;; there is no ID for each OCELOT frame independent of the name of
  ;; the frame.  In GO, there is a numeric ID and a name, but with OCELOT
  ;; there is only a name.
  t)

(defun all-ocelot-frames ()
  (nmapframes 
   (lambda (f) (and (typep f 'frames::OC.OcelotNode) f))
   :framespace :oc
   ))

;;; The file is a set of forms.  The first form is a header, which
;;; we ignore, and the rest are 'records'. 

;;; It turns out that there are some duplicate node names in the database...

(defun read-ocelot-records (file)
  (let* ((records 
         (with-standard-io-syntax
           (let ((*package* (find-package :keyword)))
             (with-open-file (p file :direction :input)
               (read p nil nil)
               (loop for count fixnum from 0
                     as record = (read p nil nil)
                     until (null record)
                     collect (keywordize-symbols-in-ocelot record)
                     )))))
         (duplicates 
          (find-duplicates
           records :key 'first :return-exactly-one-duplicate? t)))
    (loop for duplicate in duplicates do
          (cformatt "Warning: Duplicate node ~A (~D copies)"
                    (first duplicate) 
                    (count (first duplicate) records :key 'first)))
    (if duplicates (purge-duplicates records :key 'first) records)
    ))

(defun keywordize-symbols-in-ocelot (x)
  (cond
   ((or (null x) (eq x :nil)) nil)
   ((or (eq x t) (eq x :t)) t)
   ((symbolp x) (keywordize (string-upcase x)))
   ((listp x) 
    (cons 
     (keywordize-symbols-in-ocelot (car x))
     (keywordize-symbols-in-ocelot (cdr x))))
   (t x)
   ))

#|

To do this correctly we need to:

  -- in the first pass, we need to create frames for all the ocelot records
as is done now, and we need to create a list of all the slot names and
whether or not for each slot name it is holding a singleton or sometimes a 
list.  We also need to verify or ignore ocelot record slots whose first
element is not a symbol (there seem to be some which are lists).

  -- then we need to create ocelot frames for all the symbols in the
parents and compenent-of slots.  This is because the symbols sometimes 
reference ocelot frames that don't exist in our database; so we create
dummy frames.  

  -- finally we need to fill in the slots of all the frames with appropriate
conversions as is done now, but taking into account whether we know a slot
holds a list or a singleton.  

We want to keep track of a) the ocelot frames we create from records
in the database, b) ocelot frames we create which are dummies, 
c) slot frames, and d) ocelot frames we can't parse correctly.  

|#


(defun ocelot-analysis (records)
  (setq *ocelot-slots* nil)
  (setq *ocelot-slot-frames-hash* (make-hash-table :test 'eq))
  (setq *ocelot-slots-containing-lists* 
        (copy-list *initial-ocelot-slot-frames-containing-lists*))
  (let ((slot-hash *ocelot-slot-frames-hash*)
        (weird-frames nil))
    (loop for (name nil slots-and-values) in records do
          (loop for (slot . values) in slots-and-values do
                (if (not (symbolp slot))
                    (push (list name slot) weird-frames)
                  (let ((info (gethash slot slot-hash)))
                    (if (null info)
                        (setf (gethash slot slot-hash) (list 1 (length values)))
                      (progn
                        (incf (first info))
                        (setf (second info) (max (length values) (second info)))
                        (setf (gethash slot slot-hash) info)
                        ))))))
    (setq *ocelot-unparseable-nodes* weird-frames)
    (setq *ocelot-slots* (hash-table-keys slot-hash))
    (maphash 
     (lambda (key value) 
       (when (> (second value) 1)
         (pushnew key *ocelot-slots-containing-lists*)))
     slot-hash)
    ))

;;; purge frame of any slots from a previous attempt at 
;;; creating the ocelot frames.  Note that this doesn't affect
;;; system slots such as the timestamp or the pretty name.  

(defun clear-ocelot-frame (frame)
  (for-each-frame-slot (s value) frame
    value
    (let ((slot-name (fname s)))
      (when (initial-subsequence-of?
             slot-name "OC." :element-test 'char-equal)
        (delete-slot frame s)
        ))))
            


(defun ocelot-records-to-frames (records &aux (verbose? *ocelot-verbose*))
  (setq *ocelot-undefined-nodes* nil)
  (ocelot-analysis records)
  (let ((*ocelot-symbols->frames* (make-hash-table :test 'eq))
        (ocelot-nodes nil))
    (when verbose? 
      (cformatt "First pass: Converting ocelot records to frames."))
    ;; Must be cleared out because it is not in the set of frames in the 
    ;; database file and it is used to determine the list of toplevel frames
    ;; which gets duplicated if its slots are not purged.  
    (clear-ocelot-frame #$oc.frames)
    (loop for (name nil) in records 
          for count fixnum from 0 do
          (when (and verbose? (zerop (mod count 1000))) (format t "."))
          ;; Make a frame 
          (let* ((frame (new-ocelot-frame name t)))
            (push frame ocelot-nodes)
            (clear-ocelot-frame frame)
            ))
    (cformatt "hash table size ~D" (hash-table-count *ocelot-symbols->frames*))
    (setq ocelot-nodes (nreverse ocelot-nodes))
    (when verbose? 
      (cformatt "Second pass: Creating slot frames and frames for referenced but undefined nodes..."))
    ;; Make frames out of all the ocelot slot names and put the mapping
    ;; from the name to the frame into *ocelot-symbols->frames*
    (loop for slot-name in *ocelot-slots* do 
          (new-ocelot-frame slot-name nil))
    (loop for (name nil node-data) in records 
          for count fixnum from 0 
          unless (member name *ocelot-unparseable-nodes*)
          do 
          (when (and verbose? (zerop (mod count 1000))) (format t "."))
          ;; set up the slots of the frame
          (loop for (slot . value) in node-data 
                do
                ;; all elements of these lists must become frames
                ;; even if they are not defined in the OC database.
                (when (member 
                       slot
                       *ocelot-slots-that-must-contain-lists-of-frames*)
                  (dolist (x value) 
                    (when (null (gethash x *ocelot-symbols->frames*))
                      (let ((undefined-frame (new-ocelot-frame x t)))
                        (push undefined-frame *ocelot-undefined-nodes*)
                        ))))))
    (cformatt "hash table size ~D" (hash-table-count *ocelot-symbols->frames*))
    (when verbose?
      (cformatt "Third pass: Assigning slot values."))
    (loop for (name nil node-data) in records 
          for frame in ocelot-nodes 
          for count fixnum from 0 
          unless (member name *ocelot-unparseable-nodes*)
          do 
          (when (and verbose? (zerop (mod count 1000))) (format t "."))
          ;; set up the slots of the frame
          (loop for (slot . value) in node-data 
                as slot-value = (ocelot-value-for-slot slot value)
                as slot-frame = (gethash slot *ocelot-symbols->frames*)
                as real-value = (translate-ocelot-db-value slot-value)
                do 
                (setf (slotv frame slot-frame) real-value)
                ))
    (cformatt "hash table size ~D" (hash-table-count *ocelot-symbols->frames*))
    (setq *ocelot-slot-frames* 
          (mapcar (lambda (x) (new-ocelot-frame x nil)) *ocelot-slots*))
    (cformatt "hash table size ~D" (hash-table-count *ocelot-symbols->frames*))
    (when verbose? 
      (terpri)
      (cformatt 
       "~D ocelot frames created consisting of: " 
       (hash-table-count *ocelot-symbols->frames*))
      (cformatt "  ~D ocelot nodes" (length ocelot-nodes))
      (cformatt "  ~D slot frames" (length *ocelot-slots*))
      (cformatt "  ~D referenced but undefined frames" 
                (length *ocelot-undefined-nodes*))
      (cformatt "  ~D slot frames containing lists" 
                (length *ocelot-slots-containing-lists*))
      )))
    
    

(defun new-ocelot-frame (name-symbol node?) 
  (vif (f (gethash name-symbol *ocelot-symbols->frames*))
       (values f nil)
       (let ((new-frame (ocelot-frame-from-ocelot-symbol name-symbol node?)))
         (setf (gethash name-symbol *ocelot-symbols->frames*) new-frame)
         (values new-frame t)
         )))

(defun ocelot-frame-from-ocelot-symbol (symbol node?)
  (let ((name (make-kdb-fname (string symbol) "OC.")))
    (if node?
        (make-frame-instance 'frames::OC.OcelotNode name t)
      (frame-fnamed name t))))


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
  
;;; An ocelot slot is set-valued if it was specified to be so a priori
;;; or we determine via the ocelot-analysis routine that at least one 
;;; of the frames that contain this slot has a slot value for this slot
;;; that has more than one element.  

(defun ocelot-slot-is-set-valued? (slot)
  (member slot *ocelot-slots-containing-lists*))

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
	 


