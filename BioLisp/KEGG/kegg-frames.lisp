;;; -*- Package: biolisp; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 by the BioBike team                        s    |
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
Many later hackings ... see CVS for history!

To use:

1) Download the data from:
      ftp://ftp.genome.jp/pub/kegg/release/current/ligand.tar.gz

2) Change (kegg-directory) as appropriate to reflect where the files are

3) Run (make-kegg-frames)

|#

;;; The 'exported' functionality from this file is:
;;;
;;;   MAKE-KEGG-FRAMES
;;;   *KEGG-FRAMES*
;;;   INSTANTIATE-KEGG-FROM-LOADED-FRAMES
;;;   FIND-KEGG-FRAMES

;;; If this gets changed to someplace other than bioetc you also need
;;; to edit maybe-add-image-link below. Argh.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun kegg-directory ()
    (cl-user:translate-simple-lp "bioetc:data;kegg;")))

(defparameter *kegg-db-files* '("compound" "reaction" "glycan" "enzyme"))

(defvar *kegg-frames* nil "A list of all KEGG frames")

(defvar *kegg-slots-hash-table* (make-hash-table :test 'eq))

(defvar *kegg-frames-hash-table* (make-string-equal-hash-table))

(defvar *kegg-loaded?* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *kegg-prefix* "KEGG."))

;; Make sure #$KEGG.Thing is first element of this list,
;; as initialization code depends on that hypothesis.
(defparameter *kegg-toplevel-component-frames*
  '(#$Kegg.Thing 
    ;; The toplevel frames.  All the frames read from the KEGG db
    ;; are components of these frames.
    #$Kegg.Compound #$Kegg.Reaction #$Kegg.Glycan #$Kegg.Enzyme
    ;; An indexed frame which records the hash of all the values of the 
    ;; Kegg NAME slot for all KEGG frames.
    #$Kegg.Name.s
    ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun initialize-kegg-hierarchy ()
    (let ((tops *kegg-toplevel-component-frames*))
      (setf (slotv (first tops) #$Source) :kegg)
      (setf (slotv (first tops) #$Kegg.Source.s) nil)
      (loop for f in (cdr tops) do
            (pushnew (first tops) (slotv f #$isA))
            (setf (slotv f #$Source) :kegg)
            (setf (slotv f #$Kegg.Source.s) nil)
            ))))

(initialize-kegg-hierarchy)

(defvar *kegg-verbose* t)

(defmacro wkvb (&rest cformatt-args)
  `(when *kegg-verbose* (cformatt ,@cformatt-args)))

#-:jpmtf
(def-always-computed-slot (#$Kegg.link.s frame)
  (when (eq (slotv frame #$Source) :kegg)
    (let* ((accession (slotv frame #$KEGG.accession.s))
           (ecref (subseq accession 2))
           (db-name (string-downcase (string (slotv frame #$Kegg.Source.s)))))
      (when accession
        (format nil bio::*kegg-ref-url-template* db-name ecref)
        ))))

#-:jpmtf
(def-always-computed-slot (#$Kegg.image.s frame)
  (when (eq (slotv frame #$Source) :kegg)
    (let ((accession (slotv frame #$KEGG.accession.s))
          (subdir
           (case (slotv frame #$Kegg.Source.s)
             (:compound "gif")
             (:glycan "glycan_gif")
             (:reaction "reaction_gif")
             (t nil)
             )))
      (when subdir
        (let ((image-file
               (merge-pathnames 
                (make-pathname
                 :directory `(:relative ,subdir)
                 :name accession
                 :type "gif")
                 (kegg-directory))))
          (when (probe-file image-file)
            (formatn "/kegg/~A/~A.gif" subdir accession)
            ))))))


#-:jpmtf
(progn 
  (defslot #$KEGG.link.s :base #$URLvalue)
  (defslot #$KEGG.image.s :base #$imageValue)
  (defslot #$Kegg.name.s :set-valued? t)
  (def-indexed-slot #$KEGG.name.s)
  (def-inverse-slot #$KEGG.compound.s #$KEGG.reaction.s)
  (def-inverse-slot #$KEGG.enzymeReactions.s #$KEGG.enzyme.s)
  (def-inverse-slot #$KEGG.GOframe.s #$GO.KEGGframe.s)
  )


;;; Enable Allegroserve to access various KEGG directories containing
;;; image data.

(eval-when (:compile-toplevel :load-toplevel :execute)
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
      )))

#+do-not-worry-about-this-now
(cformatt "*** Must call PUBLISH-KEGG-DIRS at startup time from dumped exe")
(publish-kegg-webdirs)

(defmethod kdb-source-files ((kdb (eql :kegg)))
  (mapcar 
   (lambda (x) (merge-pathnames x (kdb-directory kdb)))
   *kegg-db-files*
   ))

(defmethod kdb-toplevel-frames ((kdb (eql :kegg)))
  (copy-list *kegg-toplevel-component-frames*))

(defun make-kegg-frames (&key (redo? nil) (verbose? t))
  (let* ((*kegg-verbose* verbose?)
         (kegg-files (kdb-source-files :kegg)))
      (when (or redo? (null *kegg-loaded?*))
        (forward-funcall 'initialize-kegg-hierarchy)
        (forward-funcall 'create-kegg-frames-from-kegg-files kegg-files)
        )))

(defun create-kegg-frames-from-kegg-files (kegg-files)
  (clrhash *kegg-slots-hash-table*)
  (clrhash *kegg-frames-hash-table*)
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

(defun create-kegg-frames-from-frames-file (file)
  (wkvb "Reading KEGG frames file ~A" file)
  (let ((frames-read (read-kegg-frames-file file)))
    (wkvb "~D frames read from KEGG frames file  ~A" (length frames-read) file)
    ))

(defun instantiate-kegg-from-loaded-frames ()
  (setq *kegg-frames* (all-kegg-frames))
  ;; Build some kind of hash table?
  (setq *kegg-loaded?* t)
  )

(defun all-kegg-frames ()
  (nmapframes 
   (lambda (f) (and (eq (slotv f #$Source) :kegg) f))
   ))

(defun read-kegg-frames-file (file)
  (retrieve-user-frames file :return-list? t :verbose? *kegg-verbose*))

(defun write-kegg-frames-file (file)
  (dump-user-frames file :frames *kegg-frames* :verbose? *kegg-verbose*))

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
                          (mapcar 'kegg-ref->frame tokens))))))
        ;; Combine all lines into a single string
        ((:comment :reference :reactstring)
         (setf (slotv frame slot) (string-join data #\Space)))
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
     ;; Give the frame its real name.
     ;; (rename-kegg-frame frame)
     ))
   (when *kegg-verbose*
     (terpri)
     (cformatt "Record slots parsed into frames.")
     (cformatt 
      "Total KEGG frames created so far: ~D"
      (hash-table-count *kegg-frames-hash-table*))
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
                        (reduce (lambda (x y) (one-string x y)) strings)
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
      ;; Make the new frame, named by its accession,
      ;; and store a mapping from its name to the frame itself in
      ;; *kegg-frames-hash-table*. 
      (let ((frame (kegg-ref->frame accession)))
        (purge-frame frame)
        (setf (slotv frame #$source) :kegg)
        (setf (slotv frame #$Kegg.source.s) dbkey)
        (setf (slotv frame #$KEGG.accession.s) accession)
        (pushnew 
         (ecase dbkey
           (:compound #$KEGG.Compound) (:reaction #$KEGG.Reaction)
           (:glycan #$KEGG.Glycan) (:enzyme #$KEGG.Enzyme))
         (slotv frame #$isA))
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
        (pushnew #$slot (slotv frame #$isA))
        frame
        )))

(defun kegg-slot-name-to-frame-slot (name) (kegg-sym->slot (keywordize name)))

(defun kegg-ref->frame (string)
  (or (gethash string *kegg-frames-hash-table*)
      (let ((frame (frame-fnamed (make-kdb-fname string *kegg-prefix*) t)))
        (setf (gethash string *kegg-frames-hash-table*) frame)
        frame
        )))

(defun kegg-reference-to-frame (x) (kegg-ref->frame (string x)))

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

(defun rename-kegg-frames ()
  (let ((hash (make-string-equal-hash-table))
        (duplicate-count 0)
        (frames-with-only-duplicates nil)
        (frames-with-names-count 0)
        (total-names-count 0))
    (maphash
     (lambda (key frame)
       (declare (ignore key))
       (let ((names (slotv frame #$Kegg.name.s)))
         (when names
           (incf frames-with-names-count)
           (dolist (name (slotv frame #$Kegg.name.s))
             (incf total-names-count)
             (pushnew frame (gethash name hash))
             (when (/= 1 (length (gethash name hash)))
               (incf duplicate-count)
               )))))
     *kegg-frames-hash-table*)
    (cformatt "Number of frames with names: ~D" frames-with-names-count)
    (cformatt "Total number of names: ~D" total-names-count)
    (cformatt "Number of duplicate names: ~D" duplicate-count)
    (cformatt "Number of frames with only duplicate names: ~D"
              (length frames-with-only-duplicates))
    (cformatt "Duplicate names and co-occuring frames:")
    (break)
    (maphash
     (lambda (name frame-list)
       (when (> (length frame-list) 1)
         (cformatt "  ~A" name)
         (formatt "  ")
         (dolist (f frame-list) (formatt "  ~A" f))
         (terpri)
         ))
     hash
     )))

(defun rename-kegg-frame (f new-base-name)
  (let* ((first-char (char (slotv f #$KEGG.accession.s) 0))
         ;; Don't cons up a prefix string, use constant
         (prefix
          (case first-char
            ((#\C #\c) #.(one-string *kegg-prefix* "C."))
            ((#\R #\r) #.(one-string *kegg-prefix* "R."))
            ((#\G #\g) #.(one-string *kegg-prefix* "G."))
            ((#\E #\e) #.(one-string *kegg-prefix* "E."))
            (t 
             (warn "Unrecognized Kegg name prefix: '~A'" first-char)
             (one-string *kegg-prefix* (string first-char) "."))
            ))
         (new-name (make-kdb-fname new-base-name prefix)))
    (rename-frame f new-name :if-new-frame-exists :replace)
    ))


;; (def-indexed-slot #$KEGG.accession.s :test 'equal)

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
	     (let ((result nil))
	       (dolist (string side)
		 (when (char= #\C (char string 0))
		   (push (kegg-ref->frame string) result)))
	       (nreverse result))))
      (when rdef
	(setf substrings (kegg-line-to-tokens rdef))
	(setf split (position "<=>" substrings :test #'equal))
	(setf lhs (subseq substrings 0 split))
	(setf rhs (subseq substrings (1+ split)))
	(setf (slotv rframe #$KEGG.lhs.s) (linkify lhs))
	(setf (slotv rframe #$KEGG.rhs.s) (linkify rhs))
	;; maybe delete equation slot?
	))))
      

;;; NOTE: this works only because #$KEGG.substrate.s and
;;; #$KEGG.product.s don't have inverses.  They probably should, then
;;; this will stop working and another technique will need to be used.

(defvar *kegg-unresolved-enzyme-refs* nil)

(defun fixup-enzyme-refs ()
  (let ((unresolved-list nil))
    (labels 
        ((compound-named (name)
           (or (car (member #$KEGG.Compound (slot-lookup name #$KEGG.name.s) 
                            :key (lambda (f) (car (slotv f #$isa)))))
               (progn (push name unresolved-list) nil)))
         (fixup-slot (frame slot)
           (let ((strings (slotv frame slot)))
             (setf (slotv frame slot) nil)
             (dolist (string strings)
               (vif (x (compound-named string))
                    (add-element frame slot x)
                    (add-element frame slot string))))))
      (loop for eframe in (slotv #$KEGG.Enzyme #$allsubclasses)
            for j fixnum from 0 do
            (when (and *kegg-verbose* (zerop (mod j 1000))) (formatt "."))
            (fixup-slot eframe #$KEGG.product.s)
            (fixup-slot eframe #$KEGG.substrate.s)
            ))
    (wkvb (terpri))
    (when unresolved-list
      (cformatt "Could not resolve ~D KEGG descriptors to compounds"
                (length unresolved-list))
      (cformatt "Unresolved references stored in *KEGG-UNRESOLVED-ENZYME-REFS*")
      )))


(defun find-kegg-frames
       (string 
        &key
        (search-mode :full-match)
        (other-slots nil)
        (test 'string-equal)
        (threshold 0.8)
        (limit nil))
  #.(one-string-nl
     "If SEARCH-MODE is :full-match (the default),"
     "returns a list of Kegg frames which have as one of the"
     "elements of their Kegg.name.s slot STRING."
     "If SEARCH-MODE is :PARTIAL-MATCH, the function looks for matches of"
     "STRING in any substring of the elements of the Kegg.name.s slots."
     "If SEARCH-MODE is :word-homology, a scoring algorithm is used to compare"
     "STRING to each name element.  Those frames which have a name which"
     "exceeds THRESHOLD when compared with STRING are returned."
     "If TEST is STRING-EQUAL (the default) matching is done case-insensitively"
     "while if TEST is STRING= matching is case-sensitive."     
     "If LIMIT is non-nil it should be a positive integer. In this case the"
     "LIMIT frames found will be returned.")
  (block exit
    ;; Default search of the NAME slot of the KEGG frames, which has
    ;; been indexed.
    (when (and (eq search-mode :full-match) 
               (null other-slots)
               (or (eq test 'string-equal) (eq test #'string-equal)))
      (return-from exit (slot-lookup (fstring string) #$Kegg.name.s)))
    ;; Otherwise do involved search over all the KEGG frames
    (let* ((found-frames nil) 
           (search-string (fstring string))
           (slots-to-search (cons #$Kegg.name.s other-slots))
           (search-test
            (cond
             ((or (eq test 'string=) (eq test #'string=)
                  (eq test 'equal) (eq test #'equal))
              'char=)
             (t 'char-equal)))
           (found-count 0)
           (max-count (if (null limit) most-positive-fixnum limit))
           (css (compile-word search-string)))
      (block frame-search-exit
        ;; For each Kegg frame
        (dolist (kf *kegg-frames*)
          ;; For each slot we are searching
          (block slot-search-exit
            (dolist (slot slots-to-search)
              (flet ((add-frame ()
                       (pushnew kf found-frames)
                       ;; If we've found as many as we want to find, exit
                       ;; the entire search, otherwise just exit the search
                       ;; for this particular frame.
                       (if (>= (incf found-count) max-count)
                           (return-from frame-search-exit)
                         (return-from slot-search-exit)
                         )))
                ;; When the slot has contents
                (vwhen (slot-value (slotv kf slot))
                  ;; Make contents a list so we don't have to special case
                  ;; singletons vs lists
                  (setq slot-value (ensure-list slot-value))
                  ;; For each element of those contents
                  (dolist (value slot-value)
                    ;; Coerce the element into a string if possible, else ignore
                    (let ((searched-string
                           (cond 
                            ((or (stringp value) (symbolp value) (framep value))
                             (fstring value))
                            (t nil)
                            )))
                      ;; Okay, we have a string to be searched. Phew!
                      (when searched-string
                        (ecase search-mode
                          (:full-match
                           (when (funcall test search-string searched-string) 
                             (add-frame)))
                          (:partial-match
                           (when (search search-string searched-string 
                                         :test search-test)
                             (add-frame)))
                          (:word-homology
                           (when (> (score-homology css searched-string)
                                    threshold)
                             (add-frame)))
                          ))))))))))
      found-frames
      )))
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| 

Post-upload code to fabricate organisms-specific Kegg bindings. There
must be a file called organism-specific-pathway-maps.lisp in the kdb
directory, and it must have this format:

(("syn" "Synechocystis PCC 6803" "S6803" "00020"
                                 ^^^^^^ this must match the organism prefix (sans suffixing period)
  "Citrate cycle (TCA cycle)")
 ("sll0401" "sll0823" "sll0891" "sll1023" "sll1557" "sll1625" "slr0018"
  "slr0665" "slr1096" "slr1233" "slr1289")) 
...etc...

|#

;;; %%% The setup of the kegg maps frames etc should get moved to kegg
;;; general setup code above -- this is the WRONG place for it, and it
;;; takes extra time to do here bcs it's done over and over, but until
;;; such time as these are created at kegg load time, this will
;;; do. (When that code is created, the redundant code here will do
;;; nearly no work, and can eventually be removed.)

(defvar *kegg-maps* nil)

(defun thread-genes-to-kegg-pathways (organism)
  (ignore-errors		 ; Silently fail on any sort of snags!
    (let* ((prefix. (#^Organism-Prefix organism))
	   (target-orgid (when prefix. (subseq prefix. 0 (1- (length prefix.)))))
	   (k 0)
	   )
      (with-open-file 
       (i (merge-pathnames (bio::kdb-directory :kegg) "organism-specific-pathway-maps.lisp"))
       (loop for entry = (read i nil nil)
	     as ((nil nil entry-orgid map-number map-description) genenames) = entry
	     until (null entry)
	     when (string-equal target-orgid entry-orgid)
	     do 
	     (loop for genename in genenames
		   as gene-frame = (frame-fnamed (format nil "~a.~a" target-orgid genename))
		   ;; This shouldn't really be here, but way above someplace; It creates the map frames if it can't find them!
		   as map-frame = (frame-fnamed (format nil "KEGG.MAP~a" map-number) t)
		   when gene-frame
		   do 
		   (pushnew map-frame (#^kegg-maps gene-frame))
		   (push gene-frame (#^organism.genes map-frame))
		   (incf k)
		   ;; These are highly redundant and should be done at kegg loadup someplace way above!
		   (setf (#^description map-frame) map-description)
		   (setf (#^number map-frame) (parse-integer map-number))
		   (pushnew map-frame *kegg-maps*)
		   )))
      (format t "Mapped ~a genes to kegg pathways for ~a~%" k organism))))
