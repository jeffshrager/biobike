;; -*- Package: frames; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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

;;; Author:  Mike Travers.
;;; Modifications by JP Massar.


(defun frame-fnamed (frame-name &optional force? dummy)
  ;; dummy argument so that sframes calls don't produce compiler warnings
  (declare (ignore dummy))
  #.(one-string-nl
     "The frame whose name is FRAME-NAME, which may be a symbol or a string. "
     "FORCE? determines what to do if the frame doesn't exist: "
     "  -- If FORCE? is T, creates a new frame. "
     "  -- If FORCE? is NIL or omitted, return NIL. "
     "  -- If FORCE? is :ERROR, throw an error."
     "  -- If FORCE? is anything else it is equivalent to T.")
  (let ((string (string frame-name)))
    #.(optimization-declaration)
    (cond 
     ((gethash string *frame-table*))
     ((eq force? t) (make-frame string))
     ((null force?) nil)
     ((eq force? :error) (error "There is no frame named ~A" string))
     (t (make-frame string))
     )))

(defun create-frame-named (string)
  (let ((frame-structure (make-%frame)))
    (intern-frame frame-structure string)
    frame-structure
    ))

(defun make-frame (string)
  (vif (s (legal-frame-name? string))
       (let ((frame (create-frame-named string)))
         ;; Give the frame its #$Fname slot.
         ;; Copy the string to make sure that if someone modifies
         ;; the original all is well.
         (%set-slotv frame *fname-frame* (copy-seq s))
         frame)
       (error "~S is not a legal frame name." string)
       ))

(defun legal-frame-name? (string)
  #.(optimization-declaration)
  (cond 
   ((simple-string-p string)
    (let* ((s string)
           (illegal-chars *illegal-frame-chars*)
           (slen (length s))
           (ilen (length illegal-chars)))
      (declare (type simple-string s illegal-chars))
      (declare (type fixnum slen ilen))
      (when (plusp slen)
        (block exit
          (do ((j 0 (1+ j))) ((= j slen))
            (declare (fixnum j))
            (let ((ch (schar s j)))
              (do ((i 0 (1+ i))) ((= i ilen))
                (declare (fixnum i))
                (when (eql ch (schar illegal-chars i))
                  (return-from exit nil)
                  ))))
          s))))
   ((stringp string) (legal-frame-name? (coerce string 'simple-string)))
   (t nil)
   ))

(defun intern-frame (frame name)
  "Add frame to universe of frames accessible by their FNAME"
  (when (frame-fnamed name) (error "There already is a frame named ~A." name))
  (setf (gethash name *frame-table*) frame))  

(defun unintern-frame (frame)
  "Remove FRAME from the universe of accessible frames."
  (remhash (slotv frame (frame-fnamed "fName")) *frame-table*))

(defun delete-frame (frame)
  "Remove FRAME from the universe of accessible frames."
  (unintern-frame frame))

(defun purge-frame (frame)
  "Remove all SLOT information from FRAME except for its FNAME"
  (unless (isframe? frame) (error "~A is not a frame!" frame))
  (setf (%frame-slots frame) 
        (list (find *fname-frame* (%frame-slots frame) :key 'car)))
  frame)

(let ((counter 1000))
  (defun genframe (prefix)
    #.(one-string-nl
       "Return a frame which is unique to this process (not just unique"
       "to this thread).  The frame name begins with PREFIX and continues"
       "with a set of unique digits."
       "Example:  (genframe \"FOO.\") --> #$FOO.1001"
       "Note: unlike GENSYM, the frame is 'interned' in the frame system."
       "Use (MAKE-TEMP-FRAME ...) to create an 'uninterned' frame.")
    (let ((prefix-string (string prefix)))
      (loop
       (let ((candidate-name (formatn "~A~D" prefix-string (incf counter))))
         (unless (frame-fnamed candidate-name nil)
           (return (frame-fnamed candidate-name t))
           ))))))


(declaim (inline framep isframe?))

(defun framep (x) 
  "Whether the object is a frame.  A predicate returning T or NIL."
 (%frame-p x))
(defun isframe? (x) 
  "Whether the object is a frame.  A predicate returning T or NIL."
  (%frame-p x))


(defun fname-frame () (gethash *frame-table* *fname-slot-name*))

(defun boot-frame-system (&key (table-size *default-frame-table-size*))
  "Create basics for frame system, overwriting existing system if present."
  ;; Create hash table for frame system
  (setq *frame-table* (create-a-frame-table table-size))
  ;; Create the #$fName frame and put it into the table
  (setq *fname-frame* (create-frame-named *fname-slot-name*))
  ;; Set its #$Fname slot to be its own name
  (setf (%frame-slots *fname-frame*) 
        (list (cons *fname-frame* (copy-seq *fname-slot-name*))))
  t)

(defun maybe-boot-frame-system 
       (&key (verbose? t) (table-size *default-frame-table-size*))
  (if *frame-table* 
      (progn 
        (when verbose? 
          (cformatt "Frame system exists, not reinitializating..."))
        nil)
    (progn
      (when verbose? (cformatt "Initializing frame system..."))
      (boot-frame-system :table-size table-size)
      (when verbose? (cformatt "Frame system initialized."))
      t
      )))

(defun resize-frame-table (size)
  #.(one-string-nl
     "Used to create a bigger frames hash table in one shot, instead"
     "of relying on the incremental resizing that Lisp does as the"
     "table grows.  If you know approximately how many frames your system"
     "will contain use this to immediately make the frame table that size.")
  (when (< (hash-table-size *frame-table*) size)
    (let ((new-table (make-string-equal-hash-table :size size)))
      (maphash 
       (lambda (key val) (setf (gethash key new-table) val))
       *frame-table*)
      (setq *frame-table* new-table)
      ))
  *frame-table*)

(defun copy-system-frame-table ()
  #.(one-string-nl
     "Make an alternative frames table that contains all the frame system"
     "frames (in the sense of EQ equality) and return it.")
  (let ((new-table (create-a-frame-table (length *frames-system-frames*))))
    (loop for frame in *frames-system-frames* do
          (setf (gethash (%slotv frame *fname-frame*) new-table) frame))
    new-table
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun frame-has-slot? (frame slot)
  #.(one-string-nl
     "Does FRAME actually have a slot SLOT?"
     "(note that (SLOTV FRAME SLOT) will return NIL if the slot does not"
     "exist OR if the slot value is NIL.")
  (member slot (%frame-slots frame) :test 'eq :key 'car))

(defun slot-of-frame? (slot frame)
  #.(one-string-nl
     "Does FRAME actually have a slot SLOT?"
     "(note that (SLOTV FRAME SLOT) will return NIL if the slot does not"
     "exist OR if the slot value is NIL.")
  (member slot (%frame-slots frame) :test 'eq :key 'car))

(defun frame-slots-of (frame)
  "A list of frames which are the slots of FRAME."
  (mapcar #'car (%frame-slots frame)))

(defmacro fff (string)
  "Returns the frame named STRING or if none exists creates it."
  `(frame-fnamed ,string t))


(defun frames-equalp (x y)
  "If X and Y are frames, true only if (EQ X Y), else true if (EQUALP X Y)"
  (cond
   ((and (isframe? x) (isframe? y)) (eq x y))
   (t (equalp x y))
   ))