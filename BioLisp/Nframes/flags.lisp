;; -*- Package: aframes; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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

;;; Author: JP Massar

(defmacro defbit (class flag pos)
  (unless (keywordp flag) (error "Flags must be keywords!"))
  `(progn 
     (setf (get ,flag :mask) (ash 1 ,pos))
     (pushnew ,flag (get ,class :flags))
     ))

(defmacro getmask (flag) `(get ,flag :mask))

(defmacro flagon? (bits flag)
  `(plusp 
    (logand 
     (the fixnum ,bits)
     (the fixnum ,(if (keywordp flag) (get flag :mask) `(getmask ,flag)))
     )))

(defmacro flagoff? (bits flag)
  `(zerop 
    (logand 
     (the fixnum ,bits)
     (the fixnum ,(if (keywordp flag) (get flag :mask) `(getmask ,flag)))
     )))

(defmacro clearflag! (bits flag)
  `(logand (the fixnum ,bits) 
           (the fixnum 
                ,(if (keywordp flag) 
                     (lognot (getmask flag)) 
                   `(lognot (the fixnum (getmask ,flag)))
                   ))))

(defmacro enableflag! (bits flag)
  `(logior 
    (the fixnum ,bits) 
    (the fixnum ,(if (keywordp flag) (getmask flag) `(getmask ,flag)))
    ))

(defun canonicalize-bit-value (value flag)
  #.(optimization-declaration)
  (cond 
   ((eq value t) 1)
   ((eq value nil) 0)
   ((eql value 1) 1)
   ((eql value 0) 0)
   (t (error "Invalid flag value: ~A for flag ~S" value flag))
   ))

(defun setflag (bits flag value)
  #.(optimization-declaration)
  (setq value (canonicalize-bit-value value flag))
  (if (= (the fixnum value) 1) (enableflag! bits flag) (clearflag! bits flag)))

(defmacro setflag! (bits flag value)
  (when (and (constantp flag) (not (keywordp flag)))
    (error "Invalid flag: ~S" flag))
  (when (and (constantp value) (not (or (eql value 1) (eql value 0))))
    (error "Invalid value: ~S" value))
  (cond 
   ((integerp value)
    (cond
     ((eql value 1) `(enableflag! ,bits ,flag))
     ((eql value 0) `(clearflag! ,bits ,flag))
     ))
   (t `(setflag ,bits ,flag ,value))
   ))

(defun multimask (flags)
  (reduce 'logior (mapcar (lambda (x) (getmask x)) flags)))

(defmacro enable-object-location-flag (object accessor flag)
  (let ((fr (gensym "OBJECT-")) 
        (info (gensym "INFO-")))
    `(let* ((,fr ,object) 
            (,info (,accessor ,fr)))
       (setf (,accessor ,fr) (enableflag! ,info ,flag)))))

(defmacro clear-object-location-flag (object accessor flag)
  (let ((fr (gensym "OBJECT-")) 
        (info (gensym "INFO-")))
    `(let* ((,fr ,object) 
            (,info (,accessor ,fr)))
       (setf (,accessor ,fr) (clearflag! ,info ,flag)))))

(defmacro set-object-location-flag (object accessor flag value)
  (let ((fr (gensym "OBJECT-")) 
        (info (gensym "INFO-")))
    `(let* ((,fr ,object) 
            (,info (,accessor ,fr)))
       (setf (,accessor ,fr) (setflag! ,info ,flag ,value)))))
  
        

