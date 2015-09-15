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

;;; Authors:  JP Massar, Jeff Shrager. 

#|

This describes how the GO will be fit into the new frames system.

There will be a DEF-FRAME-CLASS class called GO.GoNode whose instantiated
elements will be the nodes of the GO hierarchy as read in from the godb.lisp
file.  The variable *go-frames* will be a list of all these node frames.

The Go nodes will first be discriminated as to whether they are of 
certain types or not.  Currently there is only one type that we distinguish,
and that is a reaction node (or a node that represents the catalysis of a 
reaction, ie, represents an enzyme).  A subclass of Go.GoNode called
Go.ReactionNode will be created to handle these nodes.  So as the Go is read in,
and it is figured out what kind of go node something is, a frame of the proper
CLOS type will be instantiated.  This requires some hackery because we don't
have CHANGE-CLASS so we have to figure out the type of frame a GO node is
before we create the frame for the go node.  

Therefore all Go hierarchy nodes will belong to the Go.GoNode class and
could be iterated over using the Allegrocache DOCLASS macro, but this doesn't
work for pseudo-acache because that macro does not exist and would be 
essentially impossible to implement.  So we will continue to define the
variable *go-frames* as a list of all go nodes but we will be careful
to not put on this list any frame which is not an explicit Go Node read in
from the database file.  Since every frame created by the Go mechanism
will be prefixed by "go." and therefore in the "go" framespace it will
be possible to iterate over all these nodes as well.  

Note: We may be forced to implement the DOCLASS macro for pseudo-acache
because otherwise there will be no way to iterate over elements of a 
class in pseudo-acache mode except possibly by iterating over all frames.
One way to do this would be to extend the DOCLASS macro to allow it to
accept framespace arguments restricting the search.  So perhaps write a
DO-FRAME-CLASS macro that is portable between pseudo-acache and acache 
and which restricts itself to a named set of framespaces.   

All the molecule nodes created by processing the GO (done in the
reactions.lisp file) should be prefixed by Go.Mol. and therefore in
the Go namespace.  There should be a DEF-FRAME-CLASS for Go.Molecule.

Instead of using #$sys.isa and #$sys.partof we will use #$go.isa and #$go.partof
and their inverses #$go.subclasses and #$go.parts.  These will be reciprocal
inverse slots as defined in the frame system.  

We will need a method that prints out the parent/child tree that now 
gets printed out automatically by the frame browser, specialized on go nodes.
(The idea is that this method gets called after the standard code that 
prints out the frame.)  The generic function and a null method need to be
defined in the frame browser display code.  

There needs to be a method for each KDB which determines whether KDB
has been loaded already or not.  Previously, we just used a flag,
but that doesn't work because of Allegrocache.  What we need to do is
to define a canonical frame for each KDB (actually, for each subframesystem, 
as it were) and a test that determines whether the frame is filled in 
or empty. If it is filled in, then the KDB is loaded by definition, otherwise not.

|#

;;; Frames mentioned textually at some point in the system load that
;;; would otherwise be creates as generic frames instead of GoNode frames.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (make-frame-instance 'frames::Go.GoNode "Go.Molecular_Function" t)
  (make-frame-instance 'frames::Go.GoNode "Go.Biological_Process" t)
  (make-frame-instance 'frames::Go.GoNode "Go.Cellular_Component" t)
  )

;;; The 'exported' functionality from this file is:
;;;
;;;   MAKE-GO-FRAMES
;;;   INSTANTIATE-GO-FROM-LOADED-FRAMES
;;;   *GO-FRAMES*
;;;   GOID->FRAME
;;;   TRANSFORM-GO-IDS-TO-FRAMES

(defvar *go-verbose* nil)

(defparameter *godb-file* "godb.lisp")

(defvar *go-frames* nil "A list of all frames made from the GO Ontology")

(defvar *goid->go-frame* (make-hash-table :test #'eql)
  "A mapping from GO Ids to the frames we create for them.")

(frames::def-reciprocal-inverse-slots #$go.isa #$go.subclasses)
(frames::def-reciprocal-inverse-slots #$go.parts #$go.partof)


(defparameter *go-toplevel-component-frames*
  '(
    ;; These are the three roots of the Go hierarchy
    #$Go.Cellular_Component
    #$Go.Biological_Process
    #$Go.Molecular_Function
    ;; This is a def-frame-class thing.  Its instances are all the 
    ;; molecule frames created by the reaction code.
    #$Go.Molecule 
    ;; This is a def-frame-class which is a subclass of #$Go.GoNode 
    ;; Its instances are those go nodes which are associated with reactions.
    #$Go.ReactionNode
    ;; This is a def-frame-class thing
    #$Go.EC.Enzyme
    ))

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


(defmacro def-go-slot (go-name slot)
  `(progn
     (defslot ,slot)
     (setf (get ',go-name :slot) ,slot)))

;;; The names of all the possible data slots in a GO record.

(def-go-slot :id #$GO.goid)
(def-go-slot :part_of #$go.partof)
(def-go-slot :is_a #$go.isa)
(def-go-slot :definition #$GO.definition)
(def-go-slot :synonym #$GO.synonym)
(def-go-slot :ec-ref #$GO.ECRef)
(def-go-slot :name #$GO.prettyname)
(def-go-slot :dbxrefs #$GO.DBXRefs)
(def-go-slot :namespace #$GO.Namespace)

(def-go-slot :partof #$notused) ; not used
(def-go-slot :isa #$notused) ; not used

(defmethod kdb-source-files ((kdb (eql :go)))
  (list (merge-pathnames 
         (make-pathname :name *godb-file*)
         (kdb-directory kdb)
         )))

(defmethod kdb-toplevel-frames ((kdb (eql :go)))
   (copy-list *go-toplevel-component-frames*))

(defun make-go-frames (&key (verbose? t))
  (let* ((*go-verbose* verbose?)
         (godb-file (first (kdb-source-files :go))))
    (create-go-frames-from-godb-file godb-file)
    ;; (setq *go-frames* (all-go-frames))
    ))


(defun create-go-frames-from-godb-file (godb-file)
  (let ((vb? *go-verbose*))
    (when vb? (cformatt "Reading go records from ~A" godb-file))
    (let ((go-records (read-go-records godb-file)))
      (when vb? 
        (cformatt "~D records read" (length go-records))
        (cformatt "Creating GO frames"))
      ;; This creates the frames, and populates *GOID->GO-FRAME* hash.
      (go-records-to-frames go-records)
      (cformatt "*Go-frames* is no longer every frame in the go framespace!!")
      (cformatt "Skipping GO reaction processing for now!!")
      (setq *go-frames* (all-go-frames))
#|
      (when vb? (cformatt "Adding GO reaction info"))
      (add-go-reaction-info)
|#
      (when vb? (cformatt "~D GO frames created." (length *go-frames*)))
      )))

(defun instantiate-go-from-loaded-frames ()
  (setq *go-frames* (all-go-frames))
  (create-goid-hash-table)
  t)

;; Cannot use db.ac:do-class because pseudo-acache does not have that 
;; functionality
(defun all-go-frames ()
  (nmapframes 
   (lambda (f) (and (typep f 'frames::go.gonode) f))
   :framespace :go
   ))

(defun create-goid-hash-table ()
  (mapframes 
   (lambda (f) 
     (when (typep f 'frames::go.gonode)
       (vif (goid (slotv f #$GO.Goid))
            (setf (gethash goid *goid->go-frame*) f)
            (error "~A is a go node but has no go id slot!" f)
            )))
   :framespace :go
   ))

(defun read-go-records (file)
  (with-open-file (p file :direction :input)
    (with-standard-io-syntax
      (let ((*package* (find-package :keyword)))
        (loop for count fixnum from 0
              as record = (read p nil nil)
              until (null record)
              collect record
              )))))
          

;;; Turn the GO records into GO frames.

(defun go-reaction-node? (x)
  (declare (ignore x))
  nil)


(defun go-records-to-frames (go-records &aux (verbose *go-verbose*))

  (clrhash *goid->go-frame*)

  ;; Create a frame for each go entry, deleting any previously existing frame.
  ;; Populate *go-frames* and *goid->go-frames*

  (when verbose (cformatt "First GO pass"))
  (loop for go-entry in go-records for c fixnum from 0 do
    (let* ((id (first go-entry))
           (name (assocadr :name (rest go-entry)))
           (go-name (make-kdb-fname name "GO."))
           (frame nil))
      (unless name (error "Ruh roh.  No name field for ~A" go-entry))
      (setq frame 
            (cond 
             ((go-reaction-node? go-entry)
              (make-frame-instance 'frames::Go.GoReaction go-name t))
            (t 
             (make-frame-instance 'frames::Go.GoNode go-name t))))
      ;; Create a 'blank' frame for this name and store that frame
      ;; on the list of all GO Frames.  Store the mapping from the GOID 
      ;; of the record to the frame.
      (purge-frame frame)
      (setf (gethash id *goid->go-frame*) frame)
      (setf (slotv frame #$GO.Goid) id)
      (when (and verbose (zerop (mod c 1000))) (format t ".") (force-output))
      ))
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
             (let* ((ec-frame-name 
                     (make-kdb-fname (second slot-value) "GO.EC."))
                    (ec-frame
                     (make-frame-instance 
                      'frames::Go.EC.Enzyme ec-frame-name t)))
	       (pushnew ec-frame (slotv frame slot-frame))
	       (setf (slotv ec-frame #$go.ec.nicezyme-link) (second slot-value))
	       ))
	    ;; Shrager goes his own way
	    ((:dbxrefs) (setf (slotv frame slot-frame) (cdr slot)))
	    ;; Namespace comes in as a string; Coerce to the same fnamed frame
            ((:namespace)
	     (setf (slotv frame slot-frame) 
	       (let ((frame 
                      (frame-fnamed (make-kdb-fname slot-value "GO.") nil)))
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

(setf (slotv #$go.ec.nicezyme-link #$HTMLGenerator) 'html-for-ec-nicezyme-slot)

(defun html-for-ecref-slot (val)
  (html ((:a :href (format nil bio::*ec-ref-url-template* (cadr val)))
         (:princ-safe (cadr val))
         )))

(setf (slotv #$Go.ECRef #$HTMLGenerator) 'html-for-ecref-slot)

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

(setf (slotv #$Go.DBXRefs #$HTMLGenerator) 'html-for-dbxrefs-slot)



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

