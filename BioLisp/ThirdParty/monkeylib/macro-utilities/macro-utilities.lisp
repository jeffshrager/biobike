;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.macro-utilities)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (make-symbol ,(string n))))
     ,@body))

(defmacro with-multiple-gensyms ((&rest names) value-producing-form &body body)
  `(with-gensyms (,@names)
     (multiple-value-bind (,@names) ,value-producing-form
       ,@body)))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym (string n)))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defun spliceable (value)
  (if value (list value)))

(defmacro ppme (form &environment env)
  (progn
    (write (macroexpand-1 form env)
           :length nil
           :level nil
           :circle nil
           :pretty t
           :gensym nil
           :right-margin 83
           :case :downcase)
    nil))

