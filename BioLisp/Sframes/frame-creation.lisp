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

#+allegro
;; FIXME 2010-05-28 <peter@gigamonkeys.com> -- there were only a
;; couple places that used without-interrupts. I'm pretty sure that's
;; not right. So if this code is still used, it may need a theading audit.
(defvar *frame-system-lock* (mp:make-process-lock :name "Sframes lock.")
  "The lock to protect a couple sections of code that used to be protected by without-interrupts.")


(defun frame-fnamed (frame-name &optional (force? nil) (type 'aframe))
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
  (make-frame-instance type frame-name force?))

(defun make-frame-instance (frame-type frame-name force?)
  (let ((frame-name (string frame-name)))
    (declare (string frame-name))
    (if (not (simple-string-p frame-name))
        (make-frame-instance 
         frame-type (coerce frame-name 'simple-string) force?)
      (let ((rframe nil) (existing-frame? nil))
        #.(utils:optimization-declaration)
        (let* ((iname (string-downcase frame-name))
               (existing-frame 
                (db.ac:retrieve-from-index* frame-type 'iname iname)))
          (if existing-frame 
              (progn (setq existing-frame? t) (setq rframe existing-frame))
            (cond 
             ((null force?) nil)
             ((eq force? :error) (setq existing-frame? :error))
             (t
              (setq rframe (make-frame frame-name iname frame-type))))))
        (cond
         ((eq existing-frame? :error)
          (error "There is no frame named ~S" frame-name))
         (existing-frame?
          (handle-existing-frame rframe frame-type))
         (t rframe)
         )))))

(defun make-frame (pretty-name iname frame-type)
  (
   #+allegro mp:with-process-lock #+allegro (*frame-system-lock*)
   #-allegro progn
   (if (legal-frame-name? pretty-name) 
       (let ((frame 
	      (make-instance 
		frame-type 
		:fname (copy-seq pretty-name)
		:iname (copy-seq iname)
		:slots nil
		)))
	 (%initialize-frame-instance frame)
	 (annotate-new-frame frame)
	 frame)
       (error "~S is not a legal frame name." pretty-name))))

(defun legal-frame-name? (string)
  (every 'valid-frame-char? string))

(defvar *warn-about-mistyped-existing-frames* t)

       
(defun handle-existing-frame (existing-frame frame-type)
  (cond
   ((subtypep (type-of existing-frame) frame-type) existing-frame)
   (t
    (if (eq (type-of existing-frame) 'aframe)
        (let ((pretty-name (slotv existing-frame *fname-frame*)))
          (when *warn-about-mistyped-existing-frames* 
            (warn 
             (one-string-nl
              "*** Frame named '~A' already exists, but it is NOT of type ~A."
              "*** It is of type ~A"
              "*** The old frame will be deleted, and replaced"
              "*** with a new frame of type ~A")
             pretty-name frame-type 
             (type-of existing-frame) frame-type
             ))
          (%delete-frame existing-frame)
          (make-frame-instance frame-type pretty-name t)
          )
      (error 
       (one-string-nl
        "Frame named '~A' already exists, but it is NOT of type ~A."
        "It is of type ~A"
        "Since the frame is not of generic type, the system will not"
        " unintern it and replace it.  You must resolve the situation"
        " for yourself, recalling that someone else may be using this frame.")
       (slotv existing-frame *fname-frame*) frame-type (type-of existing-frame)
       )))))

(defun annotate-new-frame (frame)
  (vwhen (afch *after-frame-creation-hook*) (funcall afch frame)))

(defmethod %initialize-frame-instance ((frame aframe))
  nil)

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

(defun fname-frame () *fname-frame*)
(defun iname-frame () *iname-frame*)

(defun boot-frame-system () 
  "Create basics for frame system, overwriting existing system if present."
  (cformatt "Booting frame system; deleting temporary frames.")
  (setq *iname-frame* nil)
  (setq *fname-frame* nil)
  ;; Create the #$iName frame
  (setq *iname-frame* (frame-fnamed *iname-slot-name* t))
  ;; Create the #$fName frame 
  (setq *fname-frame* (frame-fnamed *fname-slot-name* t))
  (delete-all-temporary-frames)
  t)

(defun delete-all-temporary-frames ()
  (db.ac:doclass* (x 'temp-frame) (%delete-frame x))
  (db.ac:commit)
  )

(defun maybe-boot-frame-system (&key (verbose? t)) 
  (when verbose? (cformatt "Initializing frame system..."))
  (boot-frame-system)
  (when verbose? (cformatt "Frame system initialized."))
  t
  )



