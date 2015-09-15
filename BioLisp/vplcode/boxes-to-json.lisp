;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl) 

;;; +=========================================================================+
;;; | Copyright (c) 2010 Peter Seibel                                         |
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

;;; Author: Peter Seibel

;;; Temporary (yeah, right) kludge to convert from the old sexp/xexp
;;; representation of boxes to json-exps. Ultimately the right thing
;;; to do is to change the snippet->boxes code to generate json-exps
;;; straight away. But since the snippet to xexp code already exists,
;;; if we can translate from that to json in a general way, we
;;; probably have to touch less code.

(defparameter *jbml-element-types* 
  '(:jbml-cr
    :jbml-dotdotdot
    :jbml-exec-icon
    :jbml-go-icon
    :jbml-close-icon
    :jbml-menu-icon
    :jbml-menu-entry
    :jbml-icon
    :jbml-delete
    :jbml-clear
    :jbml-clear-delete
    :jbml-input-text
    :jbml-hole
    :jbml-hole-opened
    :jbml-multiline-hole
    :jbml-multiline-hole-opened
    :jbml-options-menu
    :jbml-options-menu2
    :jbml-main-box-menu))

(defparameter *jbml-menu-types*
  '(:jbml-options-menu
    :jbml-options-menu2
    :jbml-kf-multiselect-menu
    :jbml-main-box-menu))

(defparameter *jbml-simple-modifiers*
  '(:jbml-b
    :jbml-i
    :jbml-ul
    :jbml-courier
    :jbml-left-justify
    :jbml-center-justify
    :jbml-right-justify
    :jbml-thick
    :jbml-medium
    :jbml-thin
    :jbml-no-outline
    :jbml-dotted
    :jbml-dotted-blink
    :jbml-button
    :jbml-outdent
    :jbml-dnd-drag
    :jbml-dnd-no-drag
    :jbml-dnd-drop
    :jbml-dnd-no-drop))

(defparameter *jbml-modifiers-with-arg*
  '(:jbml-box-color
    :jbml-color
    :jbml-background-color
    :jbml-name))

(defun jbml-element-type-p (x)
  (member x *jbml-element-types*))

(defun jbml-modifier-p (x)
  (or (jbml-simple-modifier-p x) (jbml-modifier-with-arg-p x)))

(defun jbml-simple-modifier-p (x)
  (member x *jbml-simple-modifiers*))

(defun jbml-modifier-with-arg-p (x)
  (member x *jbml-modifiers-with-arg*))

(defun jbml-menu-type-p (x)
  (member x *jbml-menu-types*))

(defun boxes->json (boxes)
  (labels 
      ((walk (list modifiers children)
         ;; (print (list 'list list 'mods modifiers 'ch children))
         (cond
          ((null list)
           `("modifiers" ,(nreverse modifiers)
                         "children" ,(coerce (nreverse children) 'vector)))
          (t
           (destructuring-bind (first . rest) list
             (cond
              ((jbml-simple-modifier-p first)
               (walk rest (list* t (string-downcase first) modifiers) children))

              ((jbml-modifier-with-arg-p first)
               (walk
                (cdr rest)
                (list* 
                 (first rest) (string-downcase first) modifiers) children))

              ((jbml-element-type-p first)
               (walk
                rest
                modifiers
                (cons `("type" ,(string-downcase first)) children)))

              ((consp first)
               (walk rest modifiers (cons (boxes->json first) children)))

              ((or (stringp first) (symbolp first))
               (multiple-value-bind (text-element rest) 
                   (extract-text-element (string first) rest)
                 (walk rest modifiers (cons text-element children))))

              (t
               (error 
                "Don't know what to do with ~s which is of type ~A"
                first (type-of first))))))))

       (extract-text-modifiers (list modifiers)
         (cond
          ((or (null list) (not (jbml-modifier-p (first list))))
           (values (nreverse modifiers) list))
          (t
           (destructuring-bind (first . rest) list
             (cond
              ((jbml-simple-modifier-p first)
               (extract-text-modifiers
                rest (list* t (string-downcase first) modifiers)))
		    
              ((jbml-modifier-with-arg-p first)
               (extract-text-modifiers 
                (cdr rest)
                (list* (first rest) (string-downcase first) modifiers)))

              (t (error "Can't get here.")))))))

       (extract-text-element (first rest)
         (multiple-value-bind (modifiers rest) (extract-text-modifiers rest ())
           (values
            `("type" "text" "value" ,first "modifiers" ,modifiers) rest)
           )))

    (destructuring-bind (id type . rest) boxes
      (cond
       ((jbml-menu-type-p type)
        (menu->json type boxes))
       ((not (jbml-element-type-p type))
        ;; FIXME 2010-02-23 <peter@gigamonkeys.com> -- this is a
        ;; kludge to deal with boxe sexps like this:
        ;;
        ;; (25553
        ;;  :JBML-BACKGROUND-COLOR "#ffc000"
        ;;  :JBML-OUTDENT
        ;;  :JBML-NO-OUTLINE
        ;;  (5407 :JBML-OPTIONS-MENU2 "More..."
        ;;        ((381 :JBML-MENU-ENTRY "Help")
        ;; 	(56 :JBML-MENU-ENTRY "Add another")
        ;; 	(61 :JBML-MENU-ENTRY "Add two more"))
        ;;        ("ICON" "whitearrowgreen_16x16.gif")))
        ;;
        `("id" ,id "type" "anonymous" ,@(walk (cons type rest) nil nil)))
       (t
        `("id" ,id "type" ,(string-downcase type) ,@(walk rest nil nil)))))))

(defgeneric menu->json (type boxes)
  (:documentation "Convert the box representation of a menu into JSON."))

(progn

(defmethod menu->json ((type (eql :jbml-options-menu)) boxes)
  (normal-menu->json boxes))

(defmethod menu->json ((type (eql :jbml-main-box-menu)) boxes)
  (normal-menu->json boxes))

(defmethod menu->json ((type (eql :jbml-kf-multiselect-menu)) boxes)
  (destructuring-bind (id type title single-action-items multiselect-items)
      boxes
    `(
      "id" ,id
      "type" ,(string-downcase type)
      "title" ,(string title)
      "entries" ,(jsonify-box-menu-entries single-action-items)
      "multientries" ,(jsonify-box-menu-entries multiselect-items)
      )))

(defmethod normal-menu->json (boxes)
  (destructuring-bind (id type title . entries) boxes
    `(
      "id" ,id
      "type" ,(string-downcase type)
      "title" ,(string title)
      "entries" ,(jsonify-box-menu-entries entries))))

(defmethod menu->json ((type (eql :jbml-options-menu2)) boxes)
  (destructuring-bind (id type title entries (icon-label icon-src)) boxes
    (assert (string= icon-label "ICON"))
    `(
      "id" ,id
      "type" ,(string-downcase type)
      "title" ,(string title)
      "entries" ,(jsonify-box-menu-entries entries)
      "iconSrc" ,icon-src)))

(defun jsonify-box-menu-entries (entries)
  (map 'vector
       #'(lambda (e)
	   (destructuring-bind (id type title) e
	     `("id", id "type",(string-downcase type) "title" ,(string title))))
       entries))

;;; Written for top-level menus

(defun package-menu-into-json (data &optional (color "black"))
  (destructuring-bind (id title submenus entries . rest) data
    ;; REVIEW 2010-02-22 <peter@gigamonkeys.com> 
    ;; -- as far as I can tell the old XML code ignored it too
    (declare (ignore rest))
    `(
      "id" ,id
      "title" ,(string title)
      "color" ,color
      "submenus" ,(jsonify-submenus submenus)
      "entries" ,(jsonify-menu-entries entries))))

(defun jsonify-submenus (submenus)
  (coerce
   (loop for m in submenus collect
         (destructuring-bind (title submenus entries . rest) m
           ;; REVIEW 2010-02-22 <peter@gigamonkeys.com>
           ;; -- as far as I can tell the old XML code ignored it too
           (declare (ignore rest))
           `(
             "title" ,(string title)
             "submenus" ,(jsonify-submenus submenus) 
             "entries" ,(jsonify-menu-entries entries))))
   'vector))

(defun jsonify-menu-entries (entries)
  (coerce
   (loop for e in entries collect 
         (destructuring-bind (title id) e 
           `("title" ,(string title) "id" ,id)))
   'vector))

)

;;; End written for top-level menus


;;;; For debugging messages going out.

(defparameter *save-boxes-counter* 0)

(defun save-boxes (boxes &optional (type :boxes))
  (with-open-file (out (format nil "/tmp/sexps/~(~a~)-~6,'0d.sexp" type (incf *save-boxes-counter*)) :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print boxes out))))
