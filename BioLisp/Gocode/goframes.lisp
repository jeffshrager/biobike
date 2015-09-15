;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2005 JP Massar, Jeff Shrager, Mike Travers           |
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

;;; Authors:  JP Massar, Jeff Shrager. 

;;; This replaces go-toplevel.lisp and go-to-frames.lisp.
;;; Call MAKE-GO-FRAMES to set up GO for BioLingua.
;;; See the documentation for MAKE-GO-FRAMES below.

;;; The 'exported' functionality from this file is:
;;;
;;;   MAKE-GO-FRAMES
;;;   INSTANTIATE-GO-FROM-LOADED-FRAMES
;;;   *GO-FRAMES*
;;;   GOID->FRAME
;;;   TRANSFORM-GO-IDS-TO-FRAMES


(defparameter *godb-file* "godb.lisp")

(defvar *go-frames* nil "A list of all frames made from the GO Ontology")

(defvar *go-loaded?* nil)

(defvar *goid->go-frame* (make-hash-table :test #'eql)
  "A mapping from GO Ids to the frames we create for them.")

(defparameter *go-toplevel-component-frames*
  '(
    ;; These are the three roots of the Go hierarchy
    #$Go.Cellular_Component
    #$Go.Biological_Process
    #$Go.Molecular_Function
    ;; This will contain in its #$subclasses slot all the MOL. molecule
    ;; frames the reaction code creates.
    #$Go.Molecule 
    #$Go.Reaction
    #$Go.Related-Genes
    #$Go.reactantIn 
    #$Go.reactants
    #$Go.productIn 
    #$Go.products
    #$Go.directPrecursors
    #$EC.Enzyme
    #$EC.Nicezyme-link
    ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun initialize-go-hierarchy ()
      (loop for f in *go-toplevel-component-frames* do
            (setf (slotv f #$Source) :go)
            )))

(initialize-go-hierarchy)

(defun goid->frame (goid) 
  #.(one-string-nl
     "Given a numerical Gene Ontology ID, as, for example: 0001234,"
     "this finds the frame that represents that ID."
     "(You can include the GO: part of the ID.)" 
     "For example (goid->frame 1234)")
  (cond
   ((integerp goid) (gethash goid *goid->go-frame*))
   ((stringp goid) 
    (goid->frame 
     (read-from-string
      (let ((colonpos (position #\: goid)))
	(if colonpos (subseq goid (1+ colonpos)) goid)))))
   (t (error "Invalid GOID: ~A" goid))
   ))

(defvar *go-verbose* nil)

(defmacro def-go-slot (go-name slot)
  `(progn
     (defslot ,slot)
     (setf (get ',go-name :slot) ,slot)))

;;; The names of all the possible data slots in a GO record.

(def-go-slot :id #$GO.goid)
(def-go-slot :part_of #$partof)
(def-go-slot :is_a #$isa)
(def-go-slot :definition #$GO.definition)
(def-go-slot :synonym #$GO.synonym)
(def-go-slot :ec-ref #$GO.ECRef)
(def-go-slot :name #$GO.prettyname)
(def-go-slot :dbxrefs #$GO.DBXRefs)
(def-go-slot :namespace #$GO.Namespace)

;;; Old names
(def-go-slot :partof #$notused) ; not used
(def-go-slot :isa #$notused) ; not used


(defmethod kdb-source-files ((kdb (eql :go)))
  (list (merge-pathnames 
         (make-pathname :name *godb-file*)
         (kdb-directory kdb)
         )))

(defmethod kdb-toplevel-frames ((kdb (eql :go)))
  (remove-duplicates
   (copy-list
    (cons #$Go.Gene_Ontology 
          (append (#^Parts #$Go.Gene_Ontology) *go-toplevel-component-frames*)
          ))))

(defun make-go-frames (&key (redo? nil) (verbose? t))
  (let* ((*go-verbose* verbose?)
         (godb-file (first (kdb-source-files :go))))
    (when (or redo? (null *go-loaded?*))
      (forward-funcall 'initialize-go-hierarchy)
      (forward-funcall 'create-go-frames-from-godb-file godb-file)
      )))


(defun create-go-frames-from-godb-file (godb-file)
  (let ((vb? *go-verbose*))
    (when vb? (cformatt "Reading go records from ~A" godb-file))
    (let ((go-records (read-go-records godb-file)))
      (when vb? 
        (cformatt "~D records read" (length go-records))
        (cformatt "Creating GO frames"))
      ;; This creates the frames, and populates *GOID->GO-FRAME* hash.
      (go-records-to-frames go-records)
      (setq *go-frames* (all-go-frames))
      (when vb? (cformatt "Adding GO reaction info"))
      (add-go-reaction-info)
      (setq *go-loaded?* t)
      (when vb? (cformatt "~D GO frames created." (length *go-frames*)))
      )))

(defun create-go-frames-from-frames-file (goframes-file)
  (when *go-verbose* (cformatt "Reading GO frames from ~A" goframes-file))
  (let ((frames-read (read-go-frames-file goframes-file)))
    (when *go-verbose*
      (cformatt 
       "~D GO frames read from ~A" 
       (length frames-read) goframes-file
       ))))

(defun instantiate-go-from-loaded-frames ()
  (setq *go-frames* (all-go-frames))
  (create-goid-hash-table)
  (setq *go-loaded?* t)
  t)

(defun all-go-frames ()
  (nmapframes 
   (lambda (f) (and (eq (slotv f #$Source) :go) f))
   ))

(defun create-goid-hash-table (&key (frames nil))
  (flet ((add-frame-if-goid-slot (f)
           (vwhen (goid (slotv f #$GO.Goid))
             (setf (gethash goid *goid->go-frame*) f))))
    (cond
     ((and frames (typep frames 'sequence))
      (map nil #'add-frame-if-goid-slot frames))
     ((null frames) (for-all-frames (f) (add-frame-if-goid-slot f)))
     (t (error "Invalid FRAMES argument to CREATE-GOID-HASH-TABLE"))
     )))

(defun read-go-records (file)
  (with-open-file (p file :direction :input)
    (with-standard-io-syntax
      (let ((*package* (find-package :keyword)))
        (loop for count fixnum from 0
              as record = (read p nil nil)
              until (null record)
              collect record
              )))))
          

(defun read-go-frames-file (file)
  (retrieve-user-frames file :return-list? t :verbose? *go-verbose*))

(defun write-go-frames-file (file)
  (dump-user-frames file :frames *go-frames* :verbose? *go-verbose*))

(defun make-kdb-fname (string &optional prefix)
  (create-valid-frame-name string :prefix prefix :case-action :capitalize))


;;; Turn the GO records into GO frames.


(defun go-records-to-frames (go-records &aux (verbose *go-verbose*))

  (clrhash *goid->go-frame*)

  ;; Create a frame for each go entry, deleting any previously existing frame.
  ;; Populate *go-frames* and *goid->go-frames*

  (when verbose (cformatt "First GO pass"))
  (loop for go-entry in go-records for c fixnum from 0 do
    (let* ((id (first go-entry))
           (name (assocadr :name (rest go-entry)))
           (go-name (make-kdb-fname name "GO.")))
      (unless name (error "Ruh roh.  No name field for ~A" go-entry))
      ;; Create a 'blank' frame for this name and store that frame
      ;; on the list of all GO Frames.  Store the mapping from the GOID 
      ;; of the record to the frame.
      (let ((frame (frame-fnamed go-name t)))
        (purge-frame frame)
        (setf (gethash id *goid->go-frame*) frame)
        (setf (slotv frame #$GO.Goid) id)
        (setf (slotv frame #$Source) :go)
        (when (and verbose (zerop (mod c 1000))) (format t ".") (force-output))
        )))
  (when verbose (terpri))

  ;; Process the data from each go record, putting said data
  ;; into the corresponding frame's slots.

  (when verbose (cformatt "Second GO pass"))
  (loop for go-entry in go-records for c fixnum from 0 do
    (let* ((id (first go-entry)) 
           (entry-slots (cdr go-entry))
           (frame (gethash id *goid->go-frame*)))
      (dolist (slot entry-slots)
        (let* ((slot-name (keywordize (car slot)))
	       (slot-value (cadr slot))
               ;; The frame for this slot defined above with DEF-GO-SLOT
	       (slot-frame (get slot-name :slot)))
	  (unless slot-frame (error "Unrecognized go slot: ~A" slot-name))
	  (case slot-name
	    ;; Value is multiple frames
	    ((:part_of :is_a) 
	     (pushnew (goid->frame slot-value) (slotv frame slot-frame)))
            ;; ignore these
	    ((:partof :isa) nil)
	    ;; Value is multiple non-frames
	    ((:synonym)
	     (pushnew slot-value (slotv frame slot-frame) :test 'string-equal))
            ((:ec-ref)
             (unless (and (consp slot-value)
                          (eq :reference (keywordize (first slot-value)))
                          (string (second slot-value)))
               (error "Unknown EC-REF slot value: ~A" slot-value))
             (let* ((ec-frame-name (make-kdb-fname (second slot-value) "EC."))
                    (ec-frame (frame-fnamed ec-frame-name t)))
               (setf (slotv ec-frame #$Source) :go)
               (pushnew #$EC.Enzyme (slotv ec-frame #$isA))
	       (pushnew ec-frame (slotv frame slot-frame))
	       (setf (slotv ec-frame #$ec.nicezyme-link) (second slot-value))
	       ))
	    ;; Shrager goes his own way
	    ((:dbxrefs) (setf (slotv frame slot-frame) (cdr slot)))
	    ;; Namespace comes in as a string; Coerce to the same fnamed frame
	    ((:namespace)
	     (setf (slotv frame slot-frame) 
	       (let ((frame (frame-fnamed (one-string "GO." slot-value) nil)))
		 (or frame (error "Missing namespace frame: ~a" slot-value)))))
	    ;; Value is a singular non-frame
	    (t (setf (slotv frame slot-frame) slot-value))
            )))
      (when (and verbose (zerop (mod c 1000))) (format t ".") (force-output))
      ))
  (when verbose (terpri))
  )


;;; This link doesn't work for "abstract" EC numbers (ie, 1.5.3.-) 
;;; Could use http://www.chem.qmul.ac.uk/iubmb/enzyme/EC1/5/3/

(defun html-for-ec-nicezyme-slot (val)
  (html 
   ((:a :href (format nil bio::*ec-ref-url-template* val))
    (:princ-safe val)
    )))

(setf (slotv #$ec.nicezyme-link #$HTMLGenerator) 'html-for-ec-nicezyme-slot)

(defun html-for-ecref-slot (val)
  (html ((:a :href (format nil bio::*ec-ref-url-template* (cadr val)))
         (:princ-safe (cadr val))
         )))

(setf (slotv #$ECRef #$HTMLGenerator) 'html-for-ecref-slot)

(defun html-for-dbxrefs-slot (val)
  (html
   "("
   (loop for (key value) in val do
         (html
          "(" (:princ key) " "
          ;; The key is currently stored as a string, but just in case...
          (case (keywordize (fstring key))
            (:EC 
             (html ((:a :href (formatn bio::*ec-ref-url-template* value))
                    (:princ-safe value))))
            (:|MetaCyc| 
             (html ((:a :href 
                     (formatn bio::*metacyc-ref-url-template* value))
                    (:princ-safe value))))
            (otherwise (html (:princ-safe value))))
          ") "))
   ")"))

(setf (slotv #$DBXRefs #$HTMLGenerator) 'html-for-dbxrefs-slot)



;;; This conversion is used during POSTLOAD for organisms which
;;; have genes with associated go bindings in the form of GOIDS.
;;; The GOID's for each single gene are converted to GO frames and a list of
;;; all such GO frames are stored in each genes #$GO.RELATED-GENES slot.

;;; *CURRENT-FRAME* will be a gene frame of the organism whose slots are
;;; currently being transformed.  See the postload.lisp file in the
;;; Organisms directory and the organism postload files.

(defun transform-go-ids-to-frames (ids)
  (declare (special *the-current-frame*))
  (when (stringp ids) (setq ids (forward-funcall 'string-to-list ids)))
  (loop for id in ids
	as goframe = (goid->frame id)
	when (framep goframe)
	do (pushnew *the-current-frame* (slotv goframe #$go.related-genes))
	collect goframe))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

