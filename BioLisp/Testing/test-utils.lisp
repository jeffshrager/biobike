;;; -*- Package: test-mechanism; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :test-mechanism) 

;;; +=========================================================================+
;;; | copyright (c) 2005 jp massar, jeff elhai, mark slupesky, peter seibel   |
;;; |                                                                         |
;;; | permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "software"), to deal in the software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the software, and to      |
;;; | permit persons to whom the software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | the above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the software.                  |
;;; |                                                                         |
;;; | the software is provided "as is", without warranty of any kind,         |
;;; | express or implied, including but not limited to the warranties of      |
;;; | merchantability, fitness for a particular purpose and noninfringement.  |
;;; | in no event shall the authors or copyright holders be liable for any    |
;;; | claim, damages or other liability, whether in an action of contract,    |
;;; | tort or otherwise, arising from, out of or in connection with the       |
;;; | software or the use or other dealings in the software.                  |
;;; +=========================================================================+

;;; Author: JP Massar.


#-:allegro
(defmacro without-redefinition-warnings (&rest body) `(progn ,@body))
#+:allegro
(defmacro without-redefinition-warnings (&rest body) 
  `(excl::without-redefinition-warnings ,@body))


(defun keywordize (string) 
  #+:weblistener
  (utils::keywordize string)
  #-:weblistener 
  (intern string (find-package :keyword)))

(defun fformat (&rest args)
  (apply #'format t args) (terpri t) (force-output t))

(defun symbol= (s1 s2)
  #+:weblistener 
  (utils::symbol= s1 s2)
  #-:weblistener
  (and (symbolp s1) (symbolp s2)
       (string= (symbol-name s1) (symbol-name s2))))

;;; Side effects a variable to possibly have a new value
;;; within the bounds specified.

;;; Example usage, keeping a value between 1 and 10; when it
;;; exceeds 10, it goes back to 1.
;;; (dotimes (j 100000) (let ((c j)) (constrain c 1 10 :wrap t) ...))

(defmacro constrain (variable low-inclusive high-inclusive &key (wrap nil))
  (unless (symbolp variable) (error "Must be symbol, not place."))
  `(setf ,variable 
         (constraint ,variable ,low-inclusive ,high-inclusive ,wrap)))

;;; Returns a new number X such that if VALUE is within the 
;;; inclusive bounds (LOW HIGH) then X <- VALUE, otherwise
;;; X is either LOW or HIGH.

;;; If WRAP? is enabled, and VALUE > HIGH, then X <- LOW.
;;; If WRAP? is enabled, and VALUE < LOW, then X <- HIGH.

(defun constraint (value low high wrap?)
  (cond
   ((and (>= value low) (<= value high)) value)
   ((< value low) (if wrap? high low))
   ((> value high) (if wrap? low high))
   ))

(defun make-function-object (form)
  (cond
   ((null form) nil)
   ((functionp form) form)
   (t `(function (lambda () ,form)))
   ))

