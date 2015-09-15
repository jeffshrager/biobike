;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb)

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

;;; Nonportable stuff.  Only does something useful in Allegro currently,
;;; but should 'work' by being a noop in Lispworks.

;; XXX could the SBCL mp be used here? -mas 9/22/04 

(defmacro with-timeout-limit ((limit &rest timeout-forms) &body body)
  (declare (ignorable limit timeout-forms))
  #+:allegro
  `(mp:with-timeout (,limit ,@timeout-forms) ,@body)
  #-:allegro
  `(progn ,@body)
  )

(defmacro run-function-as-process 
          (name-or-options function &rest function-args)
  (declare (ignorable name-or-options function function-args))
  #+:allegro
  `(mp:process-run-function ,name-or-options ,function ,@function-args)
  #-:allegro
  `(error "RUN-FUNCTION-AS-PROCESS needs to be implemented")
  )

(defun my-current-process ()
  #+:allegro mp:*current-process*
  #-:allegro nil
  )

(defun all-the-processes ()
  #+:allegro (copy-list mp:*all-processes*)
  #-:allegro nil
  )

(defvar *spp* 0)

(defun set-process-priority (value &optional (p (my-current-process)))
  (declare (ignorable value p))
  #+:allegro
  (progn 
    (incf *spp*)
    (setf (mp:process-priority p) value))
  #-:allegro
  nil
  )

(defun get-process-priority (&optional (p (my-current-process)))
  (declare (ignorable p))
  #+:allegro
  (mp:process-priority p) 
  #-:allegro
  0
  )

(defun set-process-quantum (value &optional (p (my-current-process)))
  (declare (ignorable value p))
  #+:allegro
  (setf (mp:process-quantum p) value)
  #-:allegro
  nil
  )

(defun get-process-quantum (&optional (p (my-current-process)))
  (declare (ignorable p))
  #+:allegro
  (mp:process-quantum p) 
  #-:allegro
  1.0
  )

(defun get-process-name (&optional (p (my-current-process)))
  (declare (ignorable p))
  #+:allegro
  (mp:process-name p)
  #-:allegro
  ""
  )

(defun get-process-cpu (&optional (p (my-current-process)))
  (declare (ignorable p))
  #+:allegro
  (mp:process-cpu-msec-used p)
  #-:allegro
  0
  )

(defun kill-process (&optional (p (my-current-process)))
  (declare (ignorable p))
  #+:allegro 
  (mp:process-kill p)
  #-:allegro
  nil)


;; An Allegro process has a property list.  Use that to mark a process
;; with a PDL structure.

(defun my-process-property-list (&optional (p (my-current-process)))
  (declare (ignorable p))
  #+:allegro
  (let ((plist (mp:process-property-list p)))
    (when (null plist)
      (setf (mp:process-property-list p) (list :nil nil)))
    (mp:process-property-list p))
  #-:allegro
  nil
  )

(defun add-process-property 
    (property value &optional (p (my-current-process)))
  (declare (ignorable property value p))
  #+:allegro
  (setf (getf (mp:process-property-list p) property) value)
  #-:allegro
  nil
  )

(defun clear-process-property (property &optional (p (my-current-process)))
  (declare (ignorable property p))
  #+:allegro
  (setf (getf (mp:process-property-list p) property) nil)
  #-:allegro
  nil
  )

