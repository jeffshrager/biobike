; -*- Package: aframes; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :aframes)

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

;;; FRAME CREATION


(defun frame-fnamed (frame-name &optional (force? nil))
  #.(one-string-nl
     "The frame whose Fname is the frame-canonical form of FRAME-NAME."
     "FRAME-NAME is parsed into its framespace and name components,"
     "and the frame is created in that framespace.  The framespace is created"
     "if it does not exist.  The fname of the frame is the"
     "framespace name concatenated with the name component."
     "FORCE? determines what to do if the frame doesn't exist:"
     "  -- If FORCE? is T, creates a new frame."
     "  -- If FORCE? is NIL or omitted, return NIL."
     "  -- If FORCE? is :ERROR, throw an error."
     "  -- If FORCE? is anything else it is equivalent to T.")
  (make-frame-instance '%aframe frame-name force?))

(defun make-frame-instance (frame-type frame-name force?)
  (let ((pretty-name (string frame-name)))
    (declare (string pretty-name))
    (if (not (simple-string-p pretty-name))
        (make-frame-instance 
         frame-type (coerce pretty-name 'simple-string) force?)
      (let ((rframe nil) (existing-frame? nil))
        #.(utils:optimization-declaration)
        ;; DO NOT PUT ANY CODE THAT COULD CAUSE AN ERROR INSIDE FORM!!!
        (
	 #+allegro mp:with-process-lock #+allegro (*frame-system-lock*)
         #-:allegro progn
         (multiple-value-bind 
             (existing-frame fname existing-framespace framespace-name)
             (framestring->frame *framespace-table* pretty-name)
           (if existing-frame 
               (progn (setq existing-frame? t) (setq rframe existing-frame))
             (cond 
              ((eq force? t) 
               (setq rframe
                     (make-frame 
                      pretty-name fname 
                      framespace-name existing-framespace frame-type)))
              ((null force?) nil)
              ((eq force? :error) (setq existing-frame? :error))
              (t 
               (setq rframe
                     (make-frame 
                      pretty-name fname 
                      framespace-name existing-framespace frame-type)))
              ))))
        (cond
         ((eq existing-frame? :error)
          (error "There is no frame named ~S" frame-name))
         (existing-frame?
          (handle-existing-frame rframe frame-type))
         (t rframe)
         )))))

(defun framestring->frame (framespace-table pretty-name)
  (declare (simple-string pretty-name))
  (let ((len (length pretty-name)))
    (declare (fixnum len))
    (cond 
     ((zerop len) (error "You cannot have a frame with a null name!"))
     (t 
      (let ((dotpos (position *framespace-separator* pretty-name)))
        (cond
         ((dotpos-is-not-framespace-separator? dotpos len)
          ;; No framespace, use default framespace
          (let ((fname (%substring->fname pretty-name 0 len))
                (framespace (default-framespace framespace-table)))
            (values 
             (fmap-value framespace fname)
             fname
             framespace
             *default-framespace-name*
             )))
         (t 
          (let* ((pos dotpos)
                 framespace-name name-component framespace
                 )
            (declare (fixnum pos))
            (setq framespace-name (%substring->fname pretty-name 0 pos))
            (setq name-component (%substring->fname pretty-name 0 len))
            (setq framespace (fmap-value framespace-table framespace-name))
            (values 
             (and framespace (fmap-value framespace name-component))
             name-component
             framespace
             framespace-name
             )))))))))

(defun frame->framespace (frame &optional (framespace-table *framespace-table*))
  (fmap-value framespace-table (frame->framespace-name frame)))
    
(defun frame->framespace-name (frame)
  (let* ((fname (fname frame))
         (len (length fname))
         (dotpos (position *framespace-separator* fname)))
    (if (dotpos-is-not-framespace-separator? dotpos len)
        *default-framespace-name*
      (#+:allegro mp:with-process-lock #+allegro (*frame-system-lock*)
       #-:allegro progn
       (copy-seq (%substring->fname fname 0 dotpos))))))
    

(defun dotpos-is-not-framespace-separator? (dotpos fname-length)
  (declare (fixnum fname-length))
  #.(utils:optimization-declaration)
  (or (null dotpos) 
      (locally
        (declare (fixnum dotpos))
        (or 
         (= dotpos 0) 
         (> dotpos (the fixnum *max-framespace-name-length*))
         (= dotpos (the fixnum (1- fname-length)))
         ))))
        

      
(defun make-frame
       (pretty-name fname framespace-name existing-framespace frame-type)
  (if (and (legal-frame-name? pretty-name) 
           (legal-framespace-name? framespace-name))
      (let ((frame 
             (create-frame-named 
              fname framespace-name existing-framespace frame-type)))
        ;; Give the frame its #$Fname slot.
        (setf (%aframe-fname frame) (copy-seq fname))
        (unless (string= pretty-name fname)
          (setf (%slotv frame *pretty-name-frame*) (copy-seq pretty-name)))
        (%initialize-frame-instance frame)
        (annotate-new-frame frame)
        frame)
    (error "~S is not a legal frame name." pretty-name)
    ))

(defun legal-frame-name? (string)
  (every 'valid-frame-char? string))

(defun legal-framespace-name? (string)
  (every 'valid-frame-char? string))


(defun create-frame-named (fname framespace-name existing-framespace frame-type)
  (cond 
   (existing-framespace 
    (let ((frame-object (make-instance frame-type)))
      (%intern-frame existing-framespace frame-object (copy-seq fname))
      (when (not (eq frame-type '%aframe))
        (let ((frame-class (frame-class-frame frame-type)))
          (add-instance-to-frame-class frame-object frame-class)
          ))
      frame-object
      ))
   (t 
    (let ((new-framespace (create-framespace-named (copy-seq framespace-name))))
      (create-frame-named fname framespace-name new-framespace frame-type)
      ))))

(defvar *warn-about-mistyped-existing-frames* t)

       
(defun handle-existing-frame (existing-frame frame-type)
  (cond
   ((subtypep (type-of existing-frame) frame-type) existing-frame)
   (t
    (if (eq (type-of existing-frame) 'aframes::%aframe)
        (let ((pretty-name (slotv existing-frame *pretty-name-frame*))
              (fname (fname existing-frame)))
          (when *warn-about-mistyped-existing-frames* 
            (warn 
             (one-string-nl
              "*** Frame named '~A' already exists, but it is NOT of type ~A."
              "*** It is of type ~A"
              "*** The old frame will be uninterned, and replaced"
              "*** with a new frame of type ~A")
              (fname existing-frame) frame-type 
              (type-of existing-frame) frame-type
              ))
          (unintern-frame existing-frame)
          (make-frame-instance frame-type (or pretty-name fname) t)
          )
      (error 
       (one-string-nl
        "Frame named '~A' already exists, but it is NOT of type ~A."
        "It is of type ~A"
        "Since the frame is not of generic type, the system will not"
        " unintern it and replace it.  You must resolve the situation"
        " for yourself, recalling that someone else may be using this frame.")
       (fname existing-frame) frame-type (type-of existing-frame)
       )))))

(defun %make-temp-frame ()
  (let ((f (make-instance '%aframe)))
    (setf (%aframe-fname f) (temp-frame-name))
    (enable-frame-flag f :frame-temp?)
    (annotate-new-frame f)
    f
    ))

(defun annotate-new-frame (frame)
  (when *timestamp-frame*
    (setf (%slotv frame *timestamp-frame*) (timestamp-time)))
  (vwhen (afch *after-frame-creation-hook*) 
    (funcall afch frame)))

(cformatt "*** Note: Don't need pretty name, timestamp for non-Acache system")

(defun timestamp-time ()
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 2005)))


(defmethod %initialize-frame-instance ((frame %aframe))
  nil)

(defun intern-frame (frame name)
  "Add frame to universe of frames accessible by their FNAME"
  (when (frame-fnamed name) (error "There already is a frame named ~A." name))
  (%intern-frame (frame->framespace frame) frame name)
  (clear-frame-flag frame :frame-temp?)
  frame)

(defun unintern-frame (frame)
  "Remove FRAME from the universe of accessible frames."
  (%unintern-frame (frame->framespace frame) (fname frame))
  (clear-frame-flag frame :frame-permanent?)
  (enable-frame-flag frame :frame-temp?)
  frame)

;;; Return T if slots were removed, NIL if not.
(defmethod %purge-frame ((frame %aframe) reset-timestamp? ignore)
  (declare (ignore ignore))
  ;; If frame has no slots, nothing to purge.
  (let ((old-timestamp 
         (if *timestamp-frame* (%slotv frame *timestamp-frame*) nil)))
    (vwhen (obj (%aframe-slots frame))
      ;; If slot list holder is nil, nothing to purge (same as having no slots).
      (when (%obj-value obj)
        (if (and *pretty-name-frame* 
                 (frame-has-slot? frame *pretty-name-frame*))
            ;; The frame has a pretty name slot, we must preserve it.
            (if (= 1 (length (%obj-value obj)))
                ;; Frame has only a pretty name slot, so nothing to purge.
                nil
              ;; Save pretty name, wipe out all slots, then restore pretty name.
              (let ((pretty-name (slotv frame *pretty-name-frame*)))
                (setf (%obj-value obj) nil)
                (set-slotv frame *pretty-name-frame* pretty-name)
                t))
          ;; No pretty name, but other slots.  Clobber them.
          (progn (setf (%obj-value obj) nil) t)
          )))
    (if reset-timestamp? 
        (when *timestamp-frame*
          (setf (%slotv frame *timestamp-frame*) (timestamp-time)))
      (when *timestamp-frame*
        (setf (%slotv frame *timestamp-frame*) old-timestamp))
      )))

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

(defun fname-frame () 
  (%string->frame (default-framespace *framespace-table*) *fname-slot-name*))

(defun boot-frame-system () 
  "Create basics for frame system, overwriting existing system if present."
  (setq *timestamp-frame* nil)
  (setq *pretty-name-frame* nil)
  (setq *fname-frame* nil)
  (initialize-standard-frame-system)
  ;; Create the #$pretty-name frame and put it into the table.  
  (setq *pretty-name-frame* (frame-fnamed *pretty-name-frame-name* t))
  ;; Create the #$fName frame and put it into the table
  (setq *fname-frame* (frame-fnamed *fname-slot-name* t))
  (setq *timestamp-frame* (frame-fnamed *timestamp-frame-name* t))
  t)

(defun maybe-boot-frame-system (&key (verbose? t)) 
  (if *framespace-table* 
      (progn 
        (when verbose? 
          (cformatt "Frame system exists, not reinitializating..."))
        nil)
    (progn
      (when verbose? (cformatt "Initializing frame system..."))
      (boot-frame-system)
      (when verbose? (cformatt "Frame system initialized."))
      t
      )))

(defun resize-frame-table (size)
  (declare (ignore size))
  (warn "Resize-frame-table no longer does anything useful!!"))
        


(defun copy-system-frame-table (new-frame-table-name)
  (declare (ignore new-frame-table-name))
  #.(one-string-nl
     "Make an alternative frames table that contains all the frame system"
     "frames (in the sense of EQ equality) and return it.")
  (error "This no longer works")
  )


