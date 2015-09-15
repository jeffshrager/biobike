;; -*- package: frames; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :frames)

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

;;; Author:  JP Massar.

;;;; FRAME DUMPER / SERIALIZER

;;; Limitations:

;;;   -- Only dumps components of frames that are specified; other
;;;      frames are written out using #$ notation, so there is no
;;;      descent through frame structure, only through other objects.
;;;      If some other object has a frame as a component the descent
;;;      stops right there.
;;;   -- Doesn't handle generic defstruct objects -- maybe it can?
;;;      Need way to burrow into each slot and value like CLOS objects.
;;;      Right now can write methods to handle each kind of defstruct
;;;      you want to store.
;;;   -- EQ strings and EQ pathnames are duplicated when read back in.
;;;   -- Indirect recursive list structure will cause an infinite loop
;;;      (e.g., A list X has an element which is a list which has an element
;;;      Y which is (EQ X Y).  (A single list with circular structure
;;;      is detected and flagged as undumpable)
;;;   -- Won't dump/serialize function objects, readtables, random states,
;;;      streams or package objects.
;;;   -- Doesn't preserve displaced array-ness or the fact that a vector
;;;      might have a fill pointer.
;;;   -- You're fucked if you use initialize-instances in reloaded objects
;;;      bcs the args could be a (FDID ...) object instead of what you're
;;;      expecting.

;;; INTERFACE

;;;  -- DUMP-USER-FRAMES
;;;  -- FRAME-DUMP-EXISTS?
;;;  -- RETRIEVE-USER-FRAMES

;;;  -- METHODS TO EXTEND FUNCTIONALITY FOR PARTICULAR CLOS OR DEFSTRUCT OBJS
;;;     -- TEST-OBJECT-FOR-DUMPABILITY
;;;     -- FRAME-DUMP-VALUE
;;;     -- FRAME-DUMP-BPHUNTER

;;;  -- MISCELLANEOUS
;;;     -- WITH-SYSTEM-FRAMES-IN-HASH-TABLE

(defvar *frame-dump-stream*)

(defvar *frame-dump-table*)
(defvar *dump-table-counter*)

;;; Variables to deal with with non-dumpable objects

(defvar *dump-subframe-predicate* 'identity)
(defvar *non-dumpable-traces*)
(defvar *non-dumpable-count*)
(defvar *dump-abort-threshold*)
(defparameter *dump-stack* nil)

(defvar *frame-dump-load-table*)

;;; Variables dealing with I/O

(defparameter *frame-dump-init-file* "frames-init.lisp")
(defparameter *frame-dump-final-file* "frames-final.lisp")
(defparameter *frame-dump-count-file* "frames-count")
(defun nth-frame-dump-file-name (n) (formatn "frames~D.lisp" n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CODE DEALING WITH DUMP TABLE

;;; Keep track of objects with structure so that we don't dump them
;;; more them once and so we can dump recursive structures (but not
;;; recursive list structure or recursive structure caused by list
;;; structure, just things like a CLOS object X pointing at a CLOS
;;; object Y which points back at X).

(defstruct fdi id refcount written?)
(defstruct fdid id)

(defun backpointer (id)
  (make-fdid :id id))

(defun backpointer? (obj) (typep obj 'fdid))

(defun make-dump-table (size) (make-hash-table :test 'eq :size size))
(defun object-in-dump-table? (obj) (gethash obj *frame-dump-table*))
(defmacro make-initial-frame-dump-info (unique-id) 
  `(make-fdi :id ,unique-id :refcount 1 :written? nil))
(defmacro frame-dump-info-unique-id (fdi) `(fdi-id ,fdi))
(defmacro frame-dump-info-refcount (fdi) `(fdi-refcount ,fdi))
(defmacro frame-dump-info-written? (fdi) `(fdi-written? ,fdi))

(defmacro with-dump-table ((size) &body body)
  `(let ((*frame-dump-table* (make-dump-table ,size))
         (*dump-table-counter* 0))
     ,@body))

(defmacro with-object-on-dump-stack ((object-symbol) &body body)
  (unless (symbolp object-symbol) (ierror "Must be a symbol"))
  `(let ((*dump-stack* (cons ,object-symbol *dump-stack*)))
     ,@body
     ))
       
(defmacro with-object-in-dump ((object) &body body)
  #.(one-string-nl
     "Executes BODY iff OBJECT is not already recorded in dump hash table."
     "BODY is executed in context of *DUMP-STACK* augmented with OBJECT.")
  (let ((obj-symbol (gensym "OBJ-"))
        (new-symbol (gensym "NEW-")))
    `(let* ((,obj-symbol ,object)
            (,new-symbol (maybe-add-object-to-dump ,obj-symbol))
            )
       (when ,new-symbol 
         (with-object-on-dump-stack (,obj-symbol) ,@body)
         ))))
     
(defun maybe-add-object-to-dump (object)
  #.(one-string-nl
     "Put object in hash table if not there."
     "Return nil-NIL if new object, NIL is already present.")
  (let ((dump-info (gethash object *frame-dump-table*)))
    (if (null dump-info)
        (setf (gethash object *frame-dump-table*)
              (make-initial-frame-dump-info (incf *dump-table-counter*)))
      (progn
        (incf (frame-dump-info-refcount dump-info))
        nil
        ))))

(defun record-non-dumpable-object (obj)
  (push (cons obj *dump-stack*) *non-dumpable-traces*)
  (when (>= (incf *non-dumpable-count*) *dump-abort-threshold*)
    (throw :abort-threshold :abort)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; METHODS THAT TEST WHETHER AN OBJECT IS DUMPABLE


(defgeneric test-object-for-dumpability (object)
  (:documentation 
   #.(one-string-nl
      "Whether OBJECT can be written out using the frame dumper."
      "If the object is not dumpable the method must call the function" 
      "RECORD-NON-DUMPABLE-OBJECT on OBJECT.")))
    
(defmethod test-object-for-dumpability ((obj %aframe)) t)

(defmethod test-object-for-dumpability ((obj hash-table))
  (with-object-in-dump (obj)
    (maphash 
     (lambda (key value)
       (test-object-for-dumpability key)
       (test-object-for-dumpability value))
     obj
     )))

;;; Note that we do NOT put all cons cells into the dump table!!
;;; So indirect circularity will destroy this algorithm and if there
;;; are multiple pointers to the same CONS cell the CONS will be
;;; descended multiple times.

(defmethod test-object-for-dumpability ((obj cons))
  (multiple-value-bind (length type)
      (length-circular-or-dotted? obj)
    length
    (ecase type
      (:proper (map nil 'test-object-for-dumpability obj)) 
      (:dotted
       (loop for x on obj
             until (not (consp (cdr x))) do
             (test-object-for-dumpability (car x))
             finally 
             (progn
               (test-object-for-dumpability (car x))
               (test-object-for-dumpability (cdr x))
               )))
      (:circular (record-non-dumpable-object obj))
      )))

(defmethod test-object-for-dumpability ((obj string)) t)

(defmethod test-object-for-dumpability ((obj vector))
  (with-object-in-dump (obj) (map nil 'test-object-for-dumpability obj)))

(defmethod test-object-for-dumpability ((obj array))
  (with-object-in-dump (obj)
    (let ((size (array-total-size obj)))
      (loop for j fixnum from 0 below size do
            (test-object-for-dumpability (row-major-aref obj j))
            ))))

(defmethod test-object-for-dumpability ((obj t))
  (cond
   ((or (symbolp obj) 
        (numberp obj) 
        (characterp obj) 
        (pathnamep obj)
        )
    ;; These objects should always be printable readably, but just in case...
    (unless (printable-readably? obj) (record-non-dumpable-object obj)))
   ((or (readtablep obj) 
        (streamp obj) 
        (packagep obj) 
        (random-state-p obj)
        (functionp obj)
        )
    (record-non-dumpable-object obj))
   (t
    (let ((class-type (type-of (class-of obj))))
      (case class-type
        (standard-class (test-clos-object-for-dumpability obj))
        (structure-class (test-defstruct-object-for-dumpability obj))
        (otherwise (record-non-dumpable-object obj))
        )))))

(defun test-defstruct-object-for-dumpability (obj)
  (record-non-dumpable-object obj))

(defun test-clos-object-for-dumpability (obj)
  (with-object-in-dump (obj)
    ;; Make sure we don't try to work with an unbound slot.
    (let* ((unique (cons nil nil))
           (slots (utils:slot-names-and-slot-values obj unique)))
      (loop for (slot-name slot-value) in slots do
            (progn 
              slot-name
              (when (not (eq unique slot-value))
                (test-object-for-dumpability slot-value)
                ))))))

(defun printable-readably? (value)
  (let ((*print-readably* t))
    (handler-case
        (progn (formatn "~S" value) t)
      (print-not-readable () nil)
      (error 
       (c) 
       (let ((*print-readably* nil))
         (cformatt "Ruh roh!! Some other error try to print readably!")
         (cformatt "Actual value: ~S" value)
         (cformatt "Actual condition: ~A" c)
         )))))

(defun test-frames-for-dumpability (frames abort-threshold)
  #.(one-string-nl
     "Loop over the frames provided, descending into their slot values"
     "Abort the process if more than ABORT-THRESHOLD items are found not"
     "to be dumpable.")
  (let ((*dump-abort-threshold* abort-threshold)
        (*non-dumpable-traces* nil)
        (*non-dumpable-count* 0))
    (if (eq :abort 
            (catch 
                :abort-threshold
              (dolist (f frames)
                (when (funcall *dump-subframe-predicate* f)
                  (with-object-on-dump-stack (f)
                    (for-each-frame-slot (slot-frame slot-value) f
                      (test-object-for-dumpability slot-frame)
                      (test-object-for-dumpability slot-value)
                      ))))))
        (progn (display-non-dumpable-items) nil)
      (or (zerop *non-dumpable-count*)
          (progn (display-non-dumpable-items) nil)          
          ))))
                       
(defun display-non-dumpable-items ()
  (terpri)
  (cformatt 
   "Number of undumpable objects: ~D~%" (length *non-dumpable-traces*))
  (loop for trace in *non-dumpable-traces*
        as print-trace = (reverse trace) do
        (terpri)
        (loop for next-trace on print-trace
              as item = (first next-trace)
              as indent = 0 then (+ indent 2)
              until (null (cdr next-trace)) do
              (dotimes (j indent) (formatt " "))
              (formatt "Within ~A ...~%" item)
              finally
              (formatt "*** Object ~A is undumpable!~%" item)
              )))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; THE FRAME DUMPER TOPLEVEL

(defmacro with-system-frames-in-hash-table ((hash-table-symbol) &body body)
  `(let ((,hash-table-symbol 
          (make-hash-table :test 'eq :size (length *frames-system-frames*))))
     (dolist (f *frames-system-frames*) (setf (gethash f system-frames-ht) f))
     ,@body
     ))


(defun frame-dump-exists? (directory)
  "Are there a minimally complete set of frame dump files in DIRECTORY?"
  (and (ensure-directories-exist 
        (merge-pathnames *frame-dump-init-file* directory))
       (probe-file (merge-pathnames *frame-dump-init-file* directory))
       (probe-file (merge-pathnames *frame-dump-count-file* directory))
       (probe-file (merge-pathnames *frame-dump-final-file* directory))
       (probe-file (merge-pathnames (nth-frame-dump-file-name 0) directory))
       ))


(defun dump-user-frames 
       (directory
        &key
        (frames nil)
        (test 'identity)
        (store-variable-name '*stored-frames*)
        (delete-source-files? nil)
        (abort-threshold 10)
        (file-size-limit 1000000)
	(dump-subframe-predicate *dump-subframe-predicate*)
        (verbose? t)
        )
  #.(one-string-nl
     "Writes the frames in FRAMES to DIRECTORY, in a form suitable to be read"
     "back in using (RETRIEVE-USER-FRAMES ...)."
     "If FRAMES is NIL"
     "all the non-system frames in it are considered.  If FRAMES is a sequence"
     "all the frames in the sequence are considered, and if any frame is in"
     "fact a system frame an error is signalled."
     "Only frames which pass TEST are actually written."
     "The files that are written are .lisp files which are then compiled into"
     ".fasl files.  If DELETE-SOURCE-FILES? is non-nil, then after"
     "the compilation, the .lisp files are deleted."
     "ABORT-THRESHOLD determines the number of frames which cannot"
     "be dumped (because they contain some object that is un-dumpable)"
     "before the entire dump process is terminated.  Any frame"
     "which is determined to be un-dumpable is simply not dumped."
     "FILE-SIZE-LIMIT determines the approximate maximum file size"
     "of each .lisp file; once this maximum size is reached, another file"
     "is opened and written to.  The allegro compiler seems to have an"
     "n-squared dependency on file size so keeping this number relatively"
     "small is important."
     )
  (let ((user-frames nil)
	(*dump-subframe-predicate* dump-subframe-predicate)
        (frames-in-sequence? (and frames (typep frames 'sequence))))

    ;; Collect all the frames that are to be dumped into a list, USER-FRAMES.

    (with-system-frames-in-hash-table (system-frames-ht)
      (if frames-in-sequence?
          (loop for f in frames do
                (when (gethash f system-frames-ht)
                  (error "Attempting to write system frame: ~A" f))
                finally 
                (progn
                  (setq user-frames frames)
                  (cformatt "Dumping provided sequence of frames...")
                  ))
        (progn
          (unless (null frames) 
            (error "Invalid frames argument value: ~A" frames))
          (with-frames-iterated (f)
            (let ((is-system-frame? (gethash f system-frames-ht))
                  (to-be-written? (funcall test f)))
              (unless (or (null to-be-written?) is-system-frame?)
                (push f user-frames)
                )))
          (cformatt "Dumping non-system frames from standard frames table...")
          )))

    (when verbose? 
      (cformatt "There are ~D frames to be dumped." (length user-frames)))

    ;; Test them all for dumpability

    (when verbose? (cformatt "Testing frames to be dumped for writeability."))

    (with-dump-table ((length user-frames))

      ;; This will return NIL when ABORT-THRESHOLD is reached, or if all
      ;; frames are scanned and any are not dumpable.  Only if all are
      ;; dumpable will it return T.

      (when (not (test-frames-for-dumpability user-frames abort-threshold))
        (Return-from dump-user-frames nil))
      
      (when verbose? (cformatt "All frames dumpable. Beginning dump..."))

      ;; Dump them

      (ensure-directories-exist directory)

      (with-standard-io-syntax
        (let ((*package* (find-package :frames))
              (*readtable* (frames-readtable))
              (*print-readably* t)
              (*frame-dump-stream* nil)
              (initial-file 
               (merge-pathnames *frame-dump-init-file* directory))
              (final-file 
               (merge-pathnames *frame-dump-final-file* directory))
              (count-file 
               (merge-pathnames *frame-dump-count-file* directory))
              (file-list nil)
              (filecount 0)
              (sv store-variable-name)
              )
          (labels ((open-next-file ()
                     (let ((filename
                            (merge-pathnames 
                             (nth-frame-dump-file-name filecount) directory)
                            ))
                       (push filename file-list)
                       (setq *frame-dump-stream* 
                             (open filename 
                                   :direction :output 
                                   :if-exists :supersede
                                   ))
                       (write-frames-file-header sv)
                       (when verbose?
                         (terpri)
                         (cformatt "Writing frames to file ~A~%" filename)
                         )))
                   (close-current-file ()
                     (write-frames-file-trailer sv)
                     (close *frame-dump-stream*)
                     (setq *frame-dump-stream* nil)
                     (incf filecount)
                     ))
            (write-frames-initial-file initial-file sv)
            (open-next-file)
            (loop for f in user-frames
                  for j fixnum from 0 do
                  (write-individual-frame f)
                  (when verbose?
                    (when (zerop (mod j 500)) (formatt "."))
                    (when (zerop (mod j 25000)) (terpri)))
                  (when (> (file-length *frame-dump-stream*) file-size-limit)
                    (close-current-file) (open-next-file)
                    ))
            (close-current-file)
            (write-frames-final-file final-file sv)
            (write-frames-count-file count-file filecount)
            )
          (unless (= filecount (length file-list))
            (ierror "Ruh roh!! Filecount not same as length of files!"))
          (when verbose? 
            (cformatt "~D frames data files created." (length file-list)))
          ;; Compile them, loading the initial file which defines
          ;; variables used in the other files.
          (let ((source-files
                 (append 
                  (list initial-file) (reverse file-list) (list final-file)
                  )))
            (when verbose? 
              (cformatt "Compiling all frames data files."))
            (load (compile-file (first source-files)))
            (map nil 'compile-file (rest source-files))
            (when delete-source-files? (map nil 'delete-file source-files))
            ))

        ))))

(defun write-frames-initial-file (file store-variable)
  (let ((*package* (find-package :frames)))
    (with-open-file (p file :direction :output :if-exists :supersede)
      (format p "(in-package :frames)~%~%")
      (format p "(defparameter ~A nil)~%~%" store-variable)
      (format 
       p 
       "(defparameter *frame-dump-load-table* (make-hash-table :test 'eq))~%")
      )))

(defun write-frames-final-file (file store-variable)
  (let ((*package* (find-package :frames)))
    (with-open-file (p file :direction :output :if-exists :supersede)
      (format p "(in-package :frames)~%~%")
      (format p "(restore-user-frames ~A)~%~%" store-variable)
      (format p "(setq ~A nil)~%~%" store-variable)
      (format p "(setq *frame-dump-load-table* nil)~%")
      )))

(defun write-frames-count-file (file count)
  (with-open-file (p file :direction :output :if-exists :supersede)
    (format p "~D" count)
    ))

(defun write-frames-file-header (store-variable)
  (format *frame-dump-stream* "(in-package :frames)~%~%")
  (format *frame-dump-stream* "(setq ~S (nconc '(~%" store-variable))

(defun write-frames-file-trailer (store-variable) 
  (format *frame-dump-stream*  " )~% ~S))~%" store-variable))

(defun write-individual-frame (frame)
  (format *frame-dump-stream* " (")
  (cond 
   ((eq (type-of frame) '%aframe) nil)
   (t 
    (format *frame-dump-stream* "~S ~S " :typed (string (type-of frame)))))
  (format *frame-dump-stream* "~S" (slotv frame #$Fname))
  (for-each-frame-slot (slot-frame slot-value) frame
    (unless (eq slot-frame #$Fname) 
      (format *frame-dump-stream* "~%  ~S ~S"
              (slotv slot-frame #$Fname)
              (frame-dump-value slot-value)
              )))
  (write-string ")" *frame-dump-stream*)
  (terpri *frame-dump-stream*)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; METHODS THAT CREATE FORMS FOR INDIVIDUAL DUMPABLE OBJECTS


(defgeneric frame-dump-value (obj)
  (:documentation
   #.(one-string-nl
      "The form the write out that represents OBJ. This may OBJ itself, or a"
      "form which the frame dumper knows how to interpret to reconstruct OBJ."
      )))

(defmacro with-frame-dump-object-created 
          ((obj &key (obj-itself-ok? nil)) &body creation-code)
  (let ((obj-symbol (gensym "OBJ-"))
        (ok?-symbol (gensym "OK-"))
        (create-symbol (gensym "CREATE-"))
        )
    `(flet ((,create-symbol () ,@creation-code))
       ;; Retrieve the information about this object
       (let* ((,obj-symbol ,obj)
              (,ok?-symbol ,obj-itself-ok?)
              (obj-info (object-in-dump-table? ,obj-symbol)))
         (unless obj-info
           (ierror "Object ~A is <<NOT>> in frame dump table!" ,obj-symbol))
         ;; Decompose the information about this object.
         (let ((unique (frame-dump-info-unique-id obj-info))
               (refcount (frame-dump-info-refcount obj-info))
               (written? (frame-dump-info-written? obj-info)))
           (cond
            ;; The object is only referenced once, and this is it. Just return
            ;; the code that creates it, or the object itself, if it is OK
            ;; to do so.
            ((= refcount 1)
             (when written?
               (ierror 
                "Object ~A has only one reference but is already written!"
                ,obj-symbol))
             (if ,ok?-symbol obj `(:frame-dump-eval ,(,create-symbol))))
            ;; The object was already written once, or is currently being
            ;; written and this is a recursive reference.
            ;; Return code that
            ;; causes the loader to retrieve the object from a hash table
            ;; using its unique id as the key.
            (written? `(:frame-dump-eval (backpointer ,unique)))
            ;; The object has not yet been written, but is referenced
            ;; more than once.  Note that we have now written it, and
            ;; generate code that will create the object and store the
            ;; object in our load-time hash table using its unique id as key.
            (t
             (setf (frame-dump-info-written? obj-info) t)
             `(:frame-dump-eval
               (frame-dump-load-hash ,unique ,(,create-symbol))
               ))))))))


(defun fdeval? (x) (and (listp x) (eq (first x) :frame-dump-eval)))
(defun strip-fde (x) (if (fdeval? x) (cadr x) x))
(defun strip-fde-or-quote (x) (if (fdeval? x) (cadr x) (maybe-quote x)))
(defun maybe-quote (x) 
  (cond
   ((or (eq x t) (eq x nil) (keywordp x)) x)
   ((or (symbolp x) (consp x)) (list 'quote x))
   (t x)
   ))

;;; Hash tables.

(defmethod frame-dump-value ((obj hash-table))
  (with-frame-dump-object-created (obj)
    (flet ((cons-fde? (cons) (or (fdeval? (car cons)) (fdeval? (cdr cons)))))
      (let* ((hash-conses
              (lmaphash 
               (lambda (key value) 
                 (cons (frame-dump-value key) (frame-dump-value value)))
               obj))
             (anything-needs-to-be-evaluated? (some #'cons-fde? hash-conses))
             (ht-initial-contents-form
              (if (not anything-needs-to-be-evaluated?)
                  (list 'quote hash-conses)
                `(list ,@(mapcar
                          (lambda (hash-cons)
                            (if (cons-fde? hash-cons)
                                `(cons ,(strip-fde-or-quote (car hash-cons))
                                       ,(strip-fde-or-quote (cdr hash-cons)))
                              (list 'quote hash-cons)
                              ))
                          hash-conses
                          )))))
        `(create-hash-table
          ,ht-initial-contents-form
          :test ',(hash-table-test obj)
          :mode :dotted-pair
          )))))


;;; Strings

(defmethod frame-dump-value ((obj string)) obj)

;;; Vectors.

(defmethod frame-dump-value ((obj vector))
  (with-frame-dump-object-created (obj :obj-itself-ok? t)
    (let* ((vforms (map 'list (lambda (elem) (frame-dump-value elem)) obj))
           (anything-needs-to-be-evaluated? (some 'fdeval? vforms))
           )
      (if (not anything-needs-to-be-evaluated?)
          obj
        `(vector ,@(mapcar 'strip-fde-or-quote vforms))
        ))))
  
;;; Multidimensional arrays.

(defun create-a-multidimensional-array 
       (dimensions element-type adjustable row-major-data-vector)
  (let ((a (make-array 
            dimensions
            :element-type element-type
            :adjustable adjustable
            )))
    (loop for j fixnum from 0 below (array-total-size a)
          for elem across row-major-data-vector do
          (setf (row-major-aref a j) elem)
          finally (return a)
          )))

(defun array-row-major-data-vector (a)
  (let* ((size (array-total-size a)) (v (make-array (list size))))
    (loop for j fixnum from 0 below size do
          (setf (aref v j) (row-major-aref a j))
          finally (return v)
          )))

(defmethod frame-dump-value ((obj array))
  (with-frame-dump-object-created (obj)
    (let* ((data-vector (array-row-major-data-vector obj))
           (dims (array-dimensions obj))
           (etype (array-element-type obj))
           (adjustable (adjustable-array-p obj))
           (vforms
            (map 'list (lambda (elem) (frame-dump-value elem)) data-vector))
           (anything-needs-to-be-evaluated? (some 'fdeval? vforms))
           )
      (if (not anything-needs-to-be-evaluated?)
          `(create-a-multidimensional-array 
            ',dims ,(maybe-quote etype) ,adjustable ,data-vector)
        `(create-a-multidimensional-array 
          ',dims ,(maybe-quote etype) ,adjustable 
          (vector ,@(mapcar 'strip-fde-or-quote vforms))
          )))))

;;; Lists
;;; Lists are special because they are not hashed.
;;; Lists suck because they can have dotted pairs at the end.

(defmethod frame-dump-value ((obj cons))
  (multiple-value-bind (length type)
      (length-circular-or-dotted? obj)
    length
    (ecase type
      (:proper 
       (let* ((vforms (mapcar 'frame-dump-value obj))
              (anything-needs-to-be-evaluated? (some 'fdeval? vforms)))
         (if (not anything-needs-to-be-evaluated?)
             obj
           `(:frame-dump-eval
             (list ,@(mapcar 'strip-fde-or-quote vforms))
             ))))
      (:dotted
       (let* ((elems (loop for x on obj until (not (consp x)) collect (car x)))
              (vforms (mapcar 'frame-dump-value elems))
              (lastform (frame-dump-value (cdr (last obj))))
              (anything-needs-to-be-evaluated?
               (or (some 'fdeval? vforms) (fdeval? lastform))))
         (if (not anything-needs-to-be-evaluated?)
             obj
           `(:frame-dump-eval
             (let ((list (list ,@(mapcar 'strip-fde-or-quote vforms))))
               (setf (cdr (last list)) ,(strip-fde-or-quote lastform))
               list
               )))))
      (:circular (ierror "Circular lists should already have been caught!"))
      )))

;;; Everything else

(defmethod frame-dump-value ((obj t))
  (cond
   ;; Things that represent themselves
   ((or (symbolp obj) 
        (numberp obj)
        (characterp obj)
        (pathnamep obj)
        (isframe? obj)
        )
    obj)
   ;; Things that can't be dumped, just to make sure.
   ((or (readtablep obj) 
        (streamp obj) 
        (packagep obj) 
        (random-state-p obj)
        (functionp obj)
        )
    (ierror "FRAME-DUMP-VALUE should not receive ~A" obj))
   ;; Clos objects and structure objects without specialized methods.
   (t
    (let ((class-type (type-of (class-of obj))))
      (case class-type
        (standard-class (frame-dump-value-for-clos-object obj))
        (structure-class (frame-dump-value-for-structure-object obj))
        (otherwise (ierror "FRAME-DUMP-VALUE should not receive ~A" obj))
        )))))

(defun frame-dump-value-for-clos-object (obj)
  ;; This might get more complex if one were to take into account
  ;; default initargs; screw it for now.
  (with-frame-dump-object-created (obj)
    (let* ((unique (cons nil nil))
           (names-and-values (utils:slot-names-and-slot-values obj unique))
           (slot-values (mapcar 'second names-and-values))
           (names-and-initargs (utils:slot-names-and-initargs obj))
           (initargs (mapcar 'second names-and-initargs))
           (first-initargs (mapcar 'first initargs))
           )
      (let* ((vforms (mapcar 'frame-dump-value slot-values))
             (anything-needs-to-be-evaluated? (some 'fdeval? vforms)))
        (if (not anything-needs-to-be-evaluated?)
            `(make-instance 
              ',(class-name (class-of obj))
              ,@(apply 'append
                       (loop for initarg in first-initargs
                             for value in slot-values
                             ;; Don't dump slots w/o initargs or which
                             ;; are unbound.
                             when (and initarg (not (eq value unique)))
                             collect (list initarg (maybe-quote value))
                             )))
          `(make-instance 
            ',(class-name (class-of obj))
            ,@(apply 'append
                     (loop for initarg in first-initargs
                           for vform in vforms
                           ;; Don't dump slots w/o initargs or which
                           ;; are unbound.
                           when (and initarg (not (eq vform unique)))
                           collect (list initarg (strip-fde-or-quote vform))
                           ))))))))

(defun frame-dump-value-for-structure-object (obj)
  (declare (ignore obj))
  (ierror "Should never get here"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; METHODS THAT RECURSIVE INTO OBJECTS LOOKING FOR BACKPOINTERS TO
;;; REPLACE.  THIS IS THE SECOND PASS OF THE READING BACK IN PROCESS


(defgeneric frame-dump-bphunter (obj)
  (:documentation
   "Change any backpointers in OBJ to the objects the backpointers identify."
   ))

;;; This is not user, but goes with the subsequent around method which
;;; would be needed if we were returning objects from the table in place.
;;; See: "#+nil ; fend off recursions" elsewhere.

(defvar *bphunter-stack* nil)

#+nil ; fend off recursions
(defmethod frame-dump-bphunter :around ((obj t))
  (unless (member obj *bphunter-stack*)
    (push obj *bphunter-stack*)
    (prog1 
	(call-next-method)
      (pop *bphunter-stack*))))

(defun frame-dump-load-hash (id obj)
  ;; Any object with multiple references in the dump process should
  ;; get written out so that when loaded the load code puts the object
  ;; in the loader hash table using this function.
  (setf (gethash id *frame-dump-load-table*) obj))

(defun frame-dump-object-from-unique-id (fdid)
  ;; The BPHUNTER methods call this function to convert a unique id
  ;; into an existing, loaded object when a FDID object (aka backpointer)
  ;; is found in the recreated data.
  (or (gethash (fdid-id fdid) *frame-dump-load-table*)
      (ierror 
       "Ruh Roh! Big time! Object for unique id ~A not found!!" 
       (fdid-id fdid)
       )))

;;; Hash tables.

(defmethod frame-dump-bphunter ((obj hash-table))
  (let ((mutate-list
         (lmaphashnn
          (lambda (key value)
            (let ((key-is-id? (backpointer? key))
                  (value-is-id? (backpointer? value)))
              ;; Descend into hash table components.
              (unless key-is-id? (frame-dump-bphunter key))
              (unless value-is-id? (frame-dump-bphunter value))
              ;; Return key/value pairs that need to be converted.
              (when (or key-is-id? value-is-id?) (list key value))
              ))
          obj
          )))
    (loop for (key value) in mutate-list do
          (cond
           ((backpointer? key)
            ;; Remove the key and add the converted key with its 
            ;; possibly changed value.
            (remhash key obj)
            (setf (gethash (frame-dump-object-from-unique-id key) obj)
                  (if (typep value 'fdid)
                      (frame-dump-object-from-unique-id value)
                    value
                    )))
           ((backpointer? value)
            ;; Just change the value
            (setf (gethash key obj) (frame-dump-object-from-unique-id value)))
           (t (ierror "This is impossible"))
           ))))


;;; Strings

(defmethod frame-dump-bphunter ((obj string)) obj)

#+temporary
(defmethod frame-dump-bphunter ((obj vector)) (call-next-method))

;;; Arrays.

(defmethod frame-dump-bphunter ((obj array))
  (loop for j fixnum from 0 below (array-total-size obj)
        as elem = (row-major-aref obj j) do
        (if (backpointer? elem)
            (setf (row-major-aref obj j) 
                  (frame-dump-object-from-unique-id elem))
          (frame-dump-bphunter elem)
          )))

;;; Lists

(defmethod frame-dump-bphunter ((obj cons))
  (multiple-value-bind (length type)
      (length-circular-or-dotted? obj)
    length
    (flet ((maybe-replace-car-with-id (list)
             (let ((elem (first list)))
               (if (backpointer? elem)
                   (setf (first list) 
                         (frame-dump-object-from-unique-id elem))
                 (frame-dump-bphunter elem)
                 ))))
      (ecase type
        (:proper (loop for list on obj do (maybe-replace-car-with-id list)))
        (:dotted
         (loop for list on obj until (not (consp (cdr list))) do
               (maybe-replace-car-with-id list)
               finally
               (progn
                 (maybe-replace-car-with-id list)             
                 (let ((elem (cdr list)))
                   (if (backpointer? elem)
                       (setf (cdr list) 
                             (frame-dump-object-from-unique-id elem))
                     (frame-dump-bphunter elem)
                     )))))
        (:circular (ierror "Circular lists should already have been caught!"))
        ))))

;;; Everything else

(defmethod frame-dump-bphunter ((obj t))
  (cond
   ((or (symbolp obj) 
        (numberp obj)
        (characterp obj)
        (pathnamep obj)
        (isframe? obj)
        )
    obj)
   ((or (readtablep obj) 
        (streamp obj) 
        (packagep obj) 
        (random-state-p obj)
        (functionp obj)
        )
    (ierror "FRAME-DUMP-VALUE should not receive ~A" obj))
   (t
    (let ((class-type (type-of (class-of obj))))
      (case class-type
        (standard-class (frame-dump-bphunter-for-clos-object obj))
        (structure-class (frame-dump-bphunter-for-structure-object obj))
        (otherwise (ierror "FRAME-DUMP-BPHUNTER should not receive ~A" obj))
        )))))

(defun frame-dump-bphunter-for-clos-object (obj)
  (let* ((unique (cons nil nil))
         (names-and-values (utils:slot-names-and-slot-values obj unique)))
    (loop for (slot-name slot-value) in names-and-values do
          (if (backpointer? slot-value)
              (setf (slot-value obj slot-name)
                    (frame-dump-object-from-unique-id slot-value))
            (frame-dump-bphunter slot-value)
            ))))

(defun frame-dump-bphunter-for-structure-object (obj)
  (declare (ignore obj))
  (ierror "Should never get here"))

(defun test-frames-for-backpointers (frames)
  #.(one-string-nl
     "Loop over the frames provided, descending into their slot values and"
     "converting backpointers as found.")
  (let ((*bphunter-stack* nil)) ; fend off recursions -- not used -- see comments elsewhere as well
    (dolist
     (f frames)
     (for-each-frame-slot
      (slot-frame slot-value) f
      (frame-dump-bphunter slot-frame)
      (if (backpointer? slot-value)
	  (setf (slotv f slot-frame) 
		(frame-dump-object-from-unique-id slot-value))
	(frame-dump-bphunter slot-value)
	)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FRAME DUMP READER


(defvar *frames-verbose* nil)
(defvar *loaded-frames* nil)

(defun retrieve-user-frames 
       (directory
        &key 
        (return-list? nil)
        (verbose? t))
  #.(one-string-nl
     "Reads frame information from DIRECTORY (presumably written by"
     "(DUMP-USER-FRAMES ...) and recreates the frames stored therein."
     "If RETURN-LIST? is non-nil a list of the reconstructed frames is"
     "returned; otherwise the function is executed for side effect (inserting"
     "each reconstructed frame into *FRAME-TABLE*)."
     "Frames read via this process are destructively modified--no information"
     "from an existing frame, if any, is kept.  E.g., if #$foo has a slot"
     "#$bar with value 1 before this function is called, and #$foo is one of"
     "the frames in FILE, and when #$foo was written to FILE it did not have"
     "a #$bar slot, then when this function returns it will not longer have"
     "a #$bar slot or any associated BAR value.")
  (when verbose?
    (cformatt "Retrieving dumped frames from directory ~A" directory))
  (setq *loaded-frames* nil)
  ;; The initial, final and count files are all very small. 
  ;; No sense even worrying about whether they are compiled.
  (let ((initial-file 
         (merge-pathnames *frame-dump-init-file* directory))
        (final-file 
         (merge-pathnames *frame-dump-final-file* directory))
        (count-file 
         (merge-pathnames *frame-dump-count-file* directory))
        (*frames-verbose* verbose?)
        )
    (let ((count (with-open-file (p count-file :direction :input) (read p))))
      (unless (integerp count)
        (ierror "Frames count file ~A contains ~A, not an integer"
                count-file count))
      (when verbose?
        (cformatt "Restoring dumped frames from ~D file~P" count count))
      ;; Load all the files associated with the frame dump.
      ;; The 'final-file' contains code which calls RESTORE-USER-FRAMES
      (load initial-file)
      ;; These files are very big; make sure they are compiled
      ;; but don't recompile unless necessary.
      (loop for file-number from 0 below count do
            (c/l (merge-pathnames
                  (nth-frame-dump-file-name file-number)
                  directory)
                 nil
                 ))
      (load final-file)
      (if return-list? 
          (prog1 *loaded-frames* (setq *loaded-frames* nil))
        (setq *loaded-frames* nil)
        ))))

;;; The 'final-file' frames dump file, loaded last by RETRIEVE-USER-FRAMES,
;;; has a call in it to RESTORE-USER-FRAMES.

(defun restore-user-frames (frame-specifier-list)
  (with-system-frames-in-hash-table (system-frames-ht)
    (let ((verbose? *frames-verbose*)
          (*warn-about-mistyped-existing-frames* nil))
      (when verbose? (cformatt "Restoring frames."))
      (setq 
       *loaded-frames*
       (loop for fd in frame-specifier-list
             for count fixnum from 1
             collect
             (progn
               (when verbose?
                 (when (zerop (mod count 500)) (format t "."))
                 (when (zerop (mod count 25000)) (terpri)))
               (cond
                ((stringp (first fd))
                 (recreate-generic-frame fd system-frames-ht))
                ((eq :typed (first fd))
                 (recreate-typed-frame fd system-frames-ht))
                (t
                 (error "Internal error: Unknown frame header: ~A" (first fd))
                 )))))
      (when verbose? (cformatt "Checking loaded frames for backpointers."))
      (test-frames-for-backpointers *loaded-frames*)
      (when verbose? (cformatt "~D frames restored." (length *loaded-frames*)))
      )))

(defun recreate-generic-frame (fd system-frames-ht)
  (let ((f (frame-fnamed (first fd) t)))
    (when (gethash f system-frames-ht)
      (error "Ruh roh. System frame ~A stored in frames file!" f))
    (redefine-frame f fd)
    f
    ))

(defun recreate-typed-frame (fd system-frames-ht)
  (let ((frame-type (intern (second fd) :frames)))
    (handler-case 
        (typep 1 frame-type) 
      (error () 
             (error 
              (one-string-nl
               "The frame type FRAMES::~A is not (yet) defined."  
               "In order to restore typed frames all DEF-FRAME-TYPE"
               "definitions for the frames to be restored must"
               "have been already evaluated.")
              frame-type)))
    (let ((f (make-frame-instance frame-type (third fd) t)))
      (when (gethash f system-frames-ht)
        (error "Ruh roh. System frame ~A stored in frames file!" f))
      (redefine-frame f (cddr fd))
      f
      )))

(defun redefine-frame (frame frames-file-frame-descriptor)
  (purge-frame frame)
  (loop for x on (cdr frames-file-frame-descriptor) by #'cddr do
        (let* ((frame-slot-name (first x))
               (frame-slot (frame-fnamed frame-slot-name t))
               (value-form (second x))
               )
          (setf (slotv frame frame-slot)
                (frame-dumper-read-value-form value-form))
          )))

(defmethod frame-dumper-read-value-form ((x t)) x)

(defmethod frame-dumper-read-value-form ((x cons))
  (cond
   ((eq (car x) :frame-dump-eval) (eval (cadr x)))
   (t x)
   ))


#|

#+test
(defclass ftest () ((a :initarg :a) (fred :initarg :f)))

#+test
(defun frame-dump-test ()
  (let* ((x1 #(1 2 3))
         (x2 (make-array '(2 2) :initial-contents '((foo bar) (baz quux))))
         (x3 "This is a string")
         (x4 (cl-user:translate-simple-lp "home:foo.lisp"))
         (x5 :keyword)
         (x6 #C(1.0 2.0))
         (x7 #\x)
         (x8 (create-hash-table 
              '((1 . 2) (3 . 4)) :mode :dotted-pair :test 'eql))
         (x9 (make-instance 'ftest :a 3 :f 10000000000000000000000))
         (x10 '(this is a list 1 2 3 "of random stuff" #\Space))
         (x11 (make-instance 'ftest :a 3 :f x9))
         (x12 (make-instance 'ftest :a 12 :f 2))
         (x13 (make-instance 'ftest :a 13 :f 2))
         (dir (cl-user:translate-simple-lp "webtmp:fdumptest;"))
         )
    (cformatt "Test dump directory: ~S" dir)
    (cformatt "Test of various non-sharing objects")
    (let* ((frame
            (def-frame
             (genframe "TEST")
             #$Slot1 x1 #$Slot2 x2 #$Slot3 x3 #$Slot4 x4 #$Slot5 x5
             #$Slot6 x6 #$Slot7 x7 #$Slot8 x8 #$Slot9 x9 #$Slot10 x10
             )))
      (dump-user-frames 
       dir
       :frames (list frame #$Dog))
      (retrieve-user-frames dir)
      (df frame)
      (df #$Dog)
      )
    (cformatt "Test with list containing objects used in frame elsewhere")
    (setf (first x10) x1 (second x10) x8)
    (let* ((frame
            (def-frame
             (genframe "TEST")
             #$Slot1 x1
             #$slot2 (list x1 x2)
             #$slot3 x10
             )))
      (dump-user-frames 
       dir
       :frames (list frame #$Dog))
      (retrieve-user-frames dir)
      (df frame)
      (df #$Dog)
      )
    (cformatt "Test with recursive structure")
    (setf (slot-value x9 'a) x1)
    (setf (aref x1 0) x9)
    (setf (slot-value x12 'fred) x13)
    (setf (slot-value x13 'a) x12)
    (setf (slot-value x13 'fred) x13)
    (let ((frame
           (def-frame
            (genframe "TEST")
            #$Slot3 x3 #$Slot9 x9 #$Slot1 x1 #$Slot11 x11 #$Slot12 x12
            )))
      (dump-user-frames 
       dir
       :frames (list frame #$Dog))
      (retrieve-user-frames dir)
      (df frame)
      (df #$Dog)
      )))
|#