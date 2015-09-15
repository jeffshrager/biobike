;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl) 

;;; +=========================================================================+
;;; | Copyright (c) 2005 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

;;; Author: JP Massar.

;; We use the type LISP-BOOLEAN in the templates instead of T 
;; to denote something that returns a boolean result (eg, <), or
;; is supposed to be a boolean value (like a keyword whose values 
;; should either T or NIL).  (Of course Lisp accepts any non-nil value as T.)
;; This isn't actually useful currently, but could serve some purpose 
;; in the future.  

(deftype lisp-boolean () t)

(defmacro define-template (name type return-type &body itemplate)
  (let ((namestring (string name))
        (text-class 
         (ecase type 
           (:define-function :define-function-name)
           (:macro :macro-name)
           (:function :function-name)
           )))
    `(progn
       (assign-new-template-id ',name)
       (setf (get ',name :vpl-template) 
             `(,',name 
               ,',type
               ((:literal ,',namestring :class ,',text-class) ,@',itemplate)
               ,',return-type 
               )))))

(defun assign-new-template-id (symbol)
  (unless (symbol->template-id symbol nil) 
    (let ((newid (new-unique-id :template-id)))
      (setf (gethash symbol *template-symbol->tid*) newid)
      (setf (gethash newid *tid->template-symbol*) symbol)
      )))

(defun symbol->template-id (symbol &optional (error-if-not-found? t))
  (multiple-value-bind (tid found?)
      (gethash symbol *template-symbol->tid*)
    (if (or tid found? (not error-if-not-found?))
        (values tid found?)
      (error "TID not found for symbol ~S" symbol)
      )))

(defun symbol->template (symbol &optional (error-if-not-found? t))
  (let ((template (get symbol :vpl-template)))
    (if (or template (not error-if-not-found?))
        template
      (error "TEMPLATE not found for symbol ~S" symbol)
      )))

(defun template-id->symbol (tid &optional (error-if-not-found? t))
  (multiple-value-bind (symbol found?)
      (gethash tid *tid->template-symbol*)
    (if (or symbol found? (not error-if-not-found?))    
        (values symbol found?)
      (error "Template symbol not found for template ID ~S" tid)
      )))
    
(defun template-id->template (id &optional (error-if-not-found? t))
  (symbol->template 
   (template-id->symbol id error-if-not-found?)
   error-if-not-found?
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun show-templates (&key (limit nil))
  (when (null limit) (setq limit (hash-table-count *template-symbol->tid*)))
  (terpri)
  (maphash
   (lambda (symbol tid)
     (formatt "~S ~D" symbol tid)
     (pprint (symbol->template symbol))
     (terpri) (terpri)
     )
   *template-symbol->tid*
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Stuff to take information from DEFINE-FUNCTION data and turn it 
;; into a template

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun df-symbol->define-template-form (df-symbol)
    (let ((parse (get df-symbol :define-function-parse)))
      (unless parse 
        (error 
         "Internal error: No parse information for assumed DF function: ~S"
         df-symbol))
      (flet ((grab (key) (cadr (assoc key parse))))
        (let ((required-data (grab :required))
              (syntactic-tokens (get df-symbol :syntactic-tokens))
              (keyword-args (grab :keyword))
              (flag-args (grab :flag))
              (return-info (grab :return))
              (map-info 
               (or (grab :map) (grab :maptree) 
                   (grab :mapcarnn) (grab :mapcar)
                   (grab :cross-product)))
              (aliases (grab :aliases))
              )
          `(progn 
             ,@(mapcar 
                (lambda (name-or-alias) 
                  `(define-template ,name-or-alias :define-function 
                       ;; hack to make sure return-type includes LIST
                       ;; when function is mapped
                       ,(let ((return-type (get-return-type return-info)))
                          (when (null return-type) (setq return-type t))
                          (if map-info 
                              `(or list ,return-type)
                            return-type
                            ))
                     ,@(create-required-and-token-templates 
                        parse required-data syntactic-tokens map-info)
                     ,@(create-keyword-and-flag-templates 
                        parse keyword-args flag-args)
                     ))
                (cons df-symbol aliases)
                )
             ))))) 

  (defun get-return-type (return-info) (caar return-info))

  (defun create-required-and-token-templates 
         (parse required-data syntactic-tokens map-info) 
    (let ((arglist (mapcar 'car required-data)))
      (loop while arglist  
            for arg = (first arglist)
            as mapped? = (find arg map-info :key 'first :test 'symbol=)
            collect
            (if (symbolp arg)          
                ;; then it's a required argument
                `(:form 
                  ,(string-downcase (symbol-name arg)) 
                  ;; hack to insure type allows list for mapped arguments 
                  ,(if mapped?
                       `(or list ,(get-argument-type arg parse))
                     (get-argument-type arg parse)
                     ))
              ;; otherwise it's a list of token-args
              (let ((first-arg (first arg)))
                (if (member first-arg syntactic-tokens :test 'symbol=)
                    (let ((next-arg (second arglist)))
                      (if (and next-arg (symbolp next-arg))
                          (prog1
                              `(:progn
                                 (:literal 
                                  ,(string-downcase first-arg) 
                                  :class :define-function-token)
                                 (:form 
                                  ,(string-downcase (symbol-name next-arg)) 
                                  ,(get-argument-type next-arg parse)
                                  )
                                 :splice)
                            (pop arglist))
                        `(:literal 
                          ,(string-downcase first-arg) 
                          :class :define-function-token :splice
                          )))
                  `(:token
                    ,(string-downcase first-arg)
                    ,(mapcar 'keywordize arg)
                    :splice
                    ))))
            do 
            (pop arglist)
            )))

  (defun create-keyword-and-flag-templates (parse keyword-args flag-args)
    (let ((key-info (mapcar 'first keyword-args)))
      (when (or key-info flag-args)
        `((:keys-and-flags 
           ,(append 
             (mapcar 
              (lambda (x) (list :flag (keywordize x)))
              (mapcar 'first flag-args))
             (loop for (name nil default) in key-info collect 
                   (list 
                    :keyword (keywordize name) default
                    (get-argument-type name parse) nil
                    ))))))))
         
  (defun get-argument-type (arg parse)
    ;; get the type specifications 
    (let ((type-specs (cadr (assoc :type parse))))
      (let ((type-record
             (find (string arg) type-specs
                   :key (lambda (x) (string (caar x))) :test 'string-equal)))
        (if type-record 
            (second (first type-record))
          t
          )))) 
           
  (defun all-define-function-symbols-in-bbl-package ()
    (loop for symbol being the external-symbols of (find-package :bbl)
          when (and (get symbol :define-function-parse)
                    (not (get symbol :define-macro?)))
          collect symbol
          ))

  )

(defun bbl-define-function-templates ()
  (loop for symbol in (all-define-function-symbols-in-bbl-package) collect
        (eval (df-symbol->define-template-form symbol))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun defun-symbol->define-template-form (defun-symbol)
    (vwhen (code (get defun-symbol :procedure-definition))
      (let ((arglist (third code)))
        `(define-template ,defun-symbol :function t
           ,@(mapcar (lambda (arg) `(:form ,(string arg) t)) arglist)
           ))))
  
  )