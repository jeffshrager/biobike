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

#|
Load KEGG data into frame system.
Mike Travers March 2004
Hacked by JP Massar September 2004.

To use:

1) Download the data from:
ftp://ftp.genome.ad.jp/pub/kegg/ligand/ligand.weekly.last.tar.Z

2) Change (kegg-directory) as appropriate to reflect where the files are

3) Run (make-kegg-frames)

|#


;;; The 'exported' functionality from this file is:
;;;
;;;   MAKE-KEGG-FRAMES
;;;   INSTANTIATE-KEGG-FROM-LOADED-FRAMES
;;;   *KEGG-FRAMES*


;;; If this gets changed to someplace other than bioetc you also need to edit
;;;  maybe-add-image-link below. Argh.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun kegg-directory ()
    (cl-user:translate-simple-lp "bioetc:data;kegg;")))

(defparameter *kegg-db-files* '("compound" "reaction" "glycan" "enzyme"))

(defvar *kegg-frames* nil "A list of all KEGG frames")

(defvar *kegg-slots-hash-table* (make-hash-table :test 'eq))
(defvar *kegg-frames-hash-table* (make-string-equal-hash-table))
(defvar *kegg-names-hash-table* (make-string-equal-hash-table))

(defvar *kegg-loaded?* nil)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *kegg-prefix* "KEGG."))

(defparameter *kegg-toplevel-component-frames*
  '(#$Kegg.KeggNode 
    ;; The toplevel frames.  All the frames read from the KEGG db
    ;; are components of these frames.
    #$Kegg.Compound #$Kegg.Reaction #$Kegg.Glycan #$Kegg.Enzyme
    ))


(def-inverse-slot #$KEGG.compound.s #$KEGG.reaction.s)
(def-inverse-slot #$KEGG.enzymeReactions.s #$KEGG.enzyme.s)
(def-inverse-slot #$KEGG.GOframe.s #$GO.KEGGframe.s)

(defvar *kegg-verbose* t)

(defmacro wkvb (&rest cformatt-args)
  `(when *kegg-verbose* (cformatt ,@cformatt-args)))


(defmethod kdb-source-files ((kdb (eql :kegg)))
  (mapcar 
   (lambda (x) (merge-pathnames x (kdb-directory kdb)))
   *kegg-db-files*
   ))

(defmethod kdb-toplevel-frames ((kdb (eql :kegg)))
  (copy-list *kegg-toplevel-component-frames*))

        
(defun make-kegg-frames (&key (verbose? t))
  (let* ((*kegg-verbose* verbose?)
         (kegg-files (kdb-source-files :kegg)))
    (create-kegg-frames-from-kegg-files kegg-files)
    ))

(defun instantiate-kegg-from-loaded-frames ()
  (setq *kegg-frames* (all-kegg-frames))
  (clrhash *kegg-names-hash-table*)
  (loop for kf in *kegg-frames* 
        as names = (#^Kegg.names.s kf) do
        (loop for name in names do
              (setf (gethash name *kegg-names-hash-table*) kf)
              ))
  (setq *kegg-loaded?* t)
  )


;; All the frames defined in the kegg files, not all the frames possibly created
;; by processing these files.  
(defun all-kegg-frames ()
  (nmapframes 
   (lambda (f) 
     (and (typep f 'frames::KEGG.KeggNode) (#^kegg.source.s f) f))
   :framespace :kegg
   ))

(defun create-kegg-frames-from-kegg-files (kegg-files)
  (clrhash *kegg-slots-hash-table*)
  (clrhash *kegg-frames-hash-table*)
  (clrhash *kegg-names-hash-table*)
  (wkvb "Parsing Kegg files into records and frames...")
  (dolist (kf kegg-files) (parse-kegg-file kf))
  (wkvb "Fixing Enzyme references...")
  (fixup-enzyme-refs)
  ;; Can't rename because names are not unique...
  ;; (wkvb "Renaming KEGG frames...")
  ;; (rename-kegg-frames)
  (setq *kegg-frames* (all-kegg-frames))
  (setq *kegg-loaded?* t)
  (clrhash *kegg-slots-hash-table*)
  (clrhash *kegg-frames-hash-table*)
  (wkvb "Kegg processing complete.")
  )

(defun parse-kegg-file 

       (db-file
        &aux 
        (db-name (pathname-name (pathname db-file)))
        (dbkey (keywordize db-name))
        (kegg-records nil)
        )

  (wkvb "Parsing KEGG db file ~A..." db-file)

  (setq kegg-records (collect-kegg-records db-file))

  ;; Now process each slot of each record based on its :slot-name

  (wkvb "Parsing record slots into frames...")
  (loop 
   for record in kegg-records
   for j fixnum from 0
   do
   (when (and *kegg-verbose* (zerop (mod j 1000))) (format t "."))
   (let ((frame nil) (real-name-list nil))
     (loop 
      for (slot-name . data) in record 
      as real-slot-name = (kegg-real-slot-name slot-name dbkey)
      as slot = (kegg-sym->slot real-slot-name) 
      do
      (unless (or (eq real-slot-name :entry) frame)
        (error "Kegg error: ENTRY is not first line of record ~A" record))
      (let ((rnl (pushnew real-slot-name real-name-list)))
        (unless (eq real-slot-name (first rnl))
          (warn "Kegg: ~A: ~A: ~A slot listed more than once."
                dbkey frame slot-name)))
      (case real-slot-name
        (:entry
         (setq frame (parse-kegg-entry-line record data dbkey)))
        ;; Each item on each line is a separate datum;
        ;; Enzyme ID's need to have "EC" prefixed to them.
        ((:reaction :glycan :compound :enzyme)
         (dolist (line data)
           (let ((tokens (kegg-line-to-tokens line)))
             (when (eq slot-name :enzyme)
               (setq tokens (mapcar (lambda (x) (one-string "EC" x)) tokens)))
             (setf (slotv frame slot) 
                   (nconc (slotv frame slot)
                          (mapcar 
                           (lambda (x) (kegg-ref->frame x real-slot-name))
                           tokens
                           ))))))
        ;; Combine all lines into a single string
        ((:comment :reference :reactstring)
         (setf (slotv frame slot) (string-join data #\Space)))
        ;; Some names are continued across lines using '$' as the continuation
        ;; character.  We have to strip out this character to get the 
        ;; actual name.  Presumably there are otherwise no names which
        ;; contain '$'.  
        ((:name) 
         (setf (slotv frame slot) 
               (append 
                (mapcar (lambda (x) (remove #\$ x)) data) 
                (slotv frame slot)
                )))
        ;; Each line is a separate piece of data
        ;; Basically. we don't parse this stuff any further.
        ((:name :formula :dblinks :equation
          :definition :product :substrate                
          :pathway :composition :mass :class :binding :ortholog
          :genes :motif :disease :sysname :structures)
         (setf (slotv frame slot) (append data (slotv frame slot))))
        (otherwise
         (warn "Kegg: ~A: ~A: Unknown slot ~A: ~A" 
               dbkey frame slot record)
         (setf (slotv frame slot) (append data (slotv frame slot)))
         )))
     ;; Create mapping for all the names associated with this frame back to
     ;; this frame.  
     (loop for name in (#^kegg.name.s frame) do
           (push frame (gethash name *kegg-names-hash-table*))) 
     (push frame *kegg-frames*)
     (ecase dbkey
       (:enzyme
        ;; Create always-computed slot. 
        (setf (slotv frame #$KEGG.link.s) nil))
       ((:compound :glycan)
        ;; Create always-computed slot for image link.
        (setf (slotv frame #$Kegg.image.s) nil))
       (:reaction
        (setf (slotv frame #$Kegg.image.s) nil)
        (make-reaction-links frame)
        ))
     ))

   (when *kegg-verbose*
     (terpri)
     (cformatt "Record slots parsed into frames.")
     (cformatt "Unique kegg slots so far: ~D"
               (hash-table-count *kegg-slots-hash-table*))
     (cformatt "Total Kegg frames resulting from ENTRY records: ~D" 
               (length *kegg-frames*))
     (cformatt 
      "Total KEGG frames created so far: ~D"
      (hash-table-count *kegg-frames-hash-table*))
     (cformatt "Total KEGG names mapped to frames: ~D" 
               (hash-table-count *kegg-names-hash-table*))
     (terpri)
     ))


(defun collect-kegg-records (kegg-file &aux lines kegg-records)

  ;; Collect all the lines of the kegg db file into a list
  (setq lines
        (with-open-file (stream kegg-file :direction :input)
          (loop as line = (read-line stream nil :eof) until (eq line :eof)
                collect line
                )))
  (wkvb "~D lines read from ~A" (length lines) kegg-file)

  ;; Divide up the list of lines into records
  (setq kegg-records
        (collect-and-process-subsequences
         lines
         'kegg-db-file-start-of-record-line?
         'kegg-db-file-end-of-record-line?
         nil
         ))
  (wkvb "Data parsed into ~D records" (length kegg-records))

  ;; Separate each record into slots.  Each slot being of the form
  ;; (<:slot-name> &rest string-data).
  ;; Then take the STRING-DATA and merge together all the continuation lines.
  (prog1
      (loop for record in kegg-records collect
            (let ((slots-list
                   (collect-and-process-subsequences
                    record
                    'kegg-db-file-slot-start-line?
                    nil
                    'kegg-db-process-record-lines)))
              (loop for slot in slots-list collect
                    (cons
                     (first slot)
                     (collect-and-process-subsequences
                      (rest slot)
                      (lambda (x) (not (kegg-db-data-continuation? x)))
                      nil
                      (lambda (strings) 
                        (reduce
                         (lambda (x y) 
                           (unless (eql (aref y 0) #\$)
                             (error "Continuation line ~A has no $" y))
                           (one-string x (subseq y 1)))
                         strings)
                        ))))))
    (wkvb "All records parsed into slots.")
    ))

(defun parse-kegg-entry-line (record data dbkey)
  (flet ((oops (f msg)
           (funcall f (one-string "Kegg: ~A: Entry line: ~S, " msg)
                  dbkey (first data))))
    (unless (eql 1 (length data))
      (error "Kegg: ~A: ENTRY spans multiple lines: ~A" dbkey record))
    (let* ((line-tokens (kegg-line-to-tokens (first data)))
           (ntokens (length line-tokens))
           (accession
            (ecase dbkey
              ((:compound :reaction) 
               (unless (= 1 ntokens) (oops 'warn "Unknown data"))
               (first line-tokens))
              (:enzyme
               (unless (and (> ntokens 1) (string= (first line-tokens) "EC"))
                 (oops 'error "Not of form 'EC <num>'"))
               (when (> ntokens 2) 
                 (unless (and (= ntokens 3) 
                              (string-equal "Obsolete" (third line-tokens)))
                   (oops 'warn "Extra goo after 'EC <num>'")))
               (one-string (first line-tokens) (second line-tokens)))
              (:glycan
               (unless (= ntokens 2) (oops 'warn "Unexpected format"))
               (first line-tokens)
               ))))
      (unless (eql (char accession 0)
                   (ecase dbkey 
                     (:compound #\C) (:reaction #\R)
                     (:glycan #\G) (:enzyme #\E)
                     ))
        (error "Kegg: ~A: Accession ~S not consistent with DB. ~A" 
               dbkey accession record
               ))
      ;; Make the new frame, named based on its accession.
      (let ((frame (kegg-ref->frame accession dbkey)))
        (setf (slotv frame #$Kegg.source.s) dbkey)
        (setf (slotv frame #$KEGG.accession.s) accession)
        frame
        ))))

(defun kegg-sym->slot (sym)
  (or (gethash sym *kegg-slots-hash-table*)
      (let ((frame
             (frame-fnamed 
              (one-string 
               *kegg-prefix* (string-downcase (symbol-name sym)) ".s")
              t)))
        (setf (gethash sym *kegg-slots-hash-table*) frame)
        frame
        )))

(defun kegg-slot-name-to-frame-slot (name) (kegg-sym->slot (keywordize name)))

(defun kegg-ref->frame (string dbkey)
  (or (gethash string *kegg-frames-hash-table*)
      (let ((frame-name (make-kdb-fname string *kegg-prefix*)))
        (setf (gethash string *kegg-frames-hash-table*) 
              (make-frame-instance 
               (ecase dbkey
                 (:Compound 'frames::KEGG.Compound)
                 (:Reaction 'frames::KEGG.Reaction)
                 (:Enzyme 'frames::KEGG.Enzyme)
                 (:Glycan 'frames::KEGG.Glycan))
               frame-name
               t)))))


(defun kegg-db-file-start-of-record-line? (line)
  #.(optimization-declaration)
  (declare (simple-string line))
  (and (>= (length line) 5)
       (eql (schar line 0) #\E)
       (eql (schar line 1) #\N)
       (eql (schar line 2) #\T)
       (eql (schar line 3) #\R)
       (eql (schar line 4) #\Y)
       ))

(defun kegg-db-file-end-of-record-line? (line)
  #.(optimization-declaration)
  (declare (simple-string line))
  (and (>= (length line) 3) 
       (eql (schar line 0) #\/) 
       (eql (schar line 1) #\/) 
       (eql (schar line 2) #\/)
       ))

(defun kegg-db-file-slot-start-line? (line) 
  #.(optimization-declaration)
  (declare (simple-string line))
  (not (eql (schar line 0) #\Space)))

(defun kegg-db-data-continuation? (data)
  #.(optimization-declaration)
  (declare (simple-string data))
  (and  (> (length data) 0) (eql (schar data 0) #\$)))


(defun kegg-db-file-line-data (line) (subseq line 12))


(defun kegg-db-process-record-lines (record-lines)
  (let ((*package* (find-package :keyword)))
    (let ((slot-name-line (first record-lines)))
      (cons
       (read-from-string slot-name-line)
       (mapcar 'kegg-db-file-line-data record-lines)
       ))))

    
(defun char-starts-kegg-token? (seq i)
  #.(optimization-declaration)
  (declare (simple-string seq) (fixnum i))
  (and (not (eql (schar seq i) #\Space))
       (or (zerop i) (eql (aref seq (the fixnum (1- i))) #\Space))))

(defun char-ends-kegg-token? (seq i)
  #.(optimization-declaration)
  (declare (simple-string seq) (fixnum i))
  (and (eql (schar seq i) #\Space)
       (not (zerop i))
       (not (eql (schar seq (the fixnum (1- i))) #\Space))
       ))

(defun kegg-line-to-tokens (seq)
  (collect-and-process-subsequences
   seq 'char-starts-kegg-token? 'char-ends-kegg-token? nil 
   :predicate-mode :binary))

(defun kegg-real-slot-name (slot-name dbkey)
  ;; For REACTION  slot name, different meanings in different files.
  ;; So change to to REACTSTRING in ENZYME records
  (if (and (eq slot-name :reaction) (eq dbkey :enzyme))
      :reactstring
    slot-name))



(defun make-reaction-links (rframe)
  (let ((eq (slotv rframe #$KEGG.equation.s))
	rdef substrings split lhs rhs)
    ;; fixup stupid equation format 
    ;; (+++ should be done in original import I suppose)
    (if (> (length eq) 1) 
	(setf (slotv rframe #$KEGG.equation.s)
              (list (one-string (cadr eq) " " (car eq)))))
    (setf rdef (car (slotv rframe #$KEGG.equation.s))) 
    (flet ((linkify (side)
             ;; find the compound frames for all the compounds mentioned
             ;; on a particular side of the equation, and return the 
             ;; list of these frames.
	     (let ((result nil))
	       (dolist (string side)
		 (when (char= #\C (char string 0))
                   (let* ((fname (make-kdb-fname string *kegg-prefix*))
                          (frame (frame-fnamed fname)))
                     (if frame 
                         (push frame result)
                       (progn 
                         (warn "No compound ~S, but reaction ~A uses it."
                             fname rframe)
                         (push (make-kdb-fname string *kegg-prefix*) result)
                         )))))
	       (nreverse result))))
      (when rdef
	(setf substrings (kegg-line-to-tokens rdef))
	(setf split (position "<=>" substrings :test #'equal))
	(setf lhs (subseq substrings 0 split))
	(setf rhs (subseq substrings (1+ split)))
	(setf (slotv rframe #$KEGG.lhs.s) (linkify lhs))
	(setf (slotv rframe #$KEGG.rhs.s) (linkify rhs))
	))))
      

;;; NOTE: this works only because #$KEGG.substrate.s and #$KEGG.product.s
;;;  don't have inverses.
;;; They probably should, then this will stop working and another technique 
;;; will need to be used.

(defvar *kegg-unresolved-enzyme-refs* nil)

(defun fixup-enzyme-refs ()
  (let ((unresolved-list nil))
    (labels 
        ((compound-named (name)
           (or (find-if (lambda (x) (eq (#^kegg.source.s x) :compound))
                        (gethash name *kegg-names-hash-table*))
               (progn (push name unresolved-list) nil)))
         (fixup-slot (frame slot)
           (let ((strings (slotv frame slot)))
             (setf (slotv frame slot) nil)
             (dolist (string strings)
               (vif (x (compound-named string))
                    (add-element frame slot x)
                    (add-element frame slot string))))))
      (loop for eframe in *kegg-frames*
            for j fixnum from 0 
            when (typep eframe 'frames::kegg.Enzyme) do
            (when (and *kegg-verbose* (zerop (mod j 1000))) (formatt "."))
            (fixup-slot eframe #$KEGG.product.s)
            (fixup-slot eframe #$KEGG.substrate.s)
            ))
    (wkvb (terpri))
    (when unresolved-list
      (cformatt "Could not resolve ~D KEGG descriptors to compounds"
                (length unresolved-list))
      (cformatt "Unresolved references stored in *KEGG-UNRESOLVED-ENZYME-REFS*")
      (setq *kegg-unresolved-enzyme-refs* unresolved-list)
      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        

(def-always-computed-slot (#$Kegg.link.s frame)
  (let ((accession (slotv frame #$KEGG.accession.s)))
    (when accession 
      (let ((ecref (subseq accession 2))
            (db-name (string-downcase (string (slotv frame #$Kegg.Source.s)))))
        (format nil bio::*kegg-ref-url-template* db-name ecref)
        ))))

(def-always-computed-slot (#$Kegg.image.s frame)
  (let ((accession (slotv frame #$KEGG.accession.s))
        (subdir
         (case (slotv frame #$Kegg.Source.s)
           (:compound "gif")
           (:glycan "glycan_gif")
           (:reaction "reaction_gif")
           (t nil)
           )))
    (when (and accession subdir)
      (let ((image-file
             (merge-pathnames 
              (make-pathname
               :directory `(:relative ,subdir)
               :name accession
               :type "gif")
              (kegg-directory))))
        (when (probe-file image-file)
          (formatn "/kegg/~A/~A.gif" subdir accession)
          )))))

(defslot #$KEGG.link.s :base #$fbrowser.URLvalue)
(defslot #$KEGG.image.s :base #$fbrowser.imageValue)

;;; Enable Allegroserve to access various KEGG directories containing
;;; image data.

(defun publish-kegg-webdirs ()
  (let ((keggdir (kegg-directory)))
    (publish-directory 
     :destination (namestring (merge-pathnames "gif/" keggdir))
     :prefix "/kegg/gif")
    (publish-directory 
     :destination (namestring (merge-pathnames "reaction_gif/" keggdir))
     :prefix "/kegg/reaction_gif")
    (publish-directory 
     :destination (namestring (merge-pathnames "glycan_gif/" keggdir))
     :prefix "/kegg/glycan_gif")
    ))

(cformatt "*** Must call PUBLISH-KEGG-DIRS at startup time from dumped exe")
(publish-kegg-webdirs)



