;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.foo.java)

;;; Object representation of Java source-level constructs.

(defclass package ()
  ((name :initarg :name :accessor name)
   (types :initform nil :accessor types)))

(defclass compilation-unit ()
  ((package :initarg :package :accessor package)
   (type :initarg :type :accessor type)))

(defclass java-type ()
  ((package          :initform nil :accessor package)
   (name             :initform nil :accessor name)
   (interfaces       :initform nil :accessor interfaces)
   (fields           :initform nil :accessor fields)
   (methods          :initform nil :accessor methods)
   (inner-types    :initform nil :accessor inner-types)))

(defclass java-class (java-type)
  ((superclass       :initform 'object :accessor superclass)
   (constructors     :initform nil :accessor constructors)
   (methods          :initform nil :accessor methods)
   (inner-classes    :initform nil :accessor inner-classes)
   (access-modifiers :initform nil :accessor access-modifiers)))

(defclass java-interface (java-type)
  ())

(defclass member () 
  ((modifiers :initform nil :accessor modifiers)))

(defclass method-or-field (member)
  ((name :initarg :name :accessor name)
   (type :initform nil :accessor type)))  

(defclass method-or-constructor (member)
  ((parameters :initform nil :accessor parameters)
   (throws :initform nil :accessor throws)
   (body :initform nil :accessor body)))

(defclass field (method-or-field)
  ((initializer :initform nil :accessor initializer)))

(defclass method (method-or-field method-or-constructor)
  ())

(defclass constructor (method-or-constructor)
  ())

(defclass top-level-block (block)
  ((static-p :initform nil :accessor static-p)))

(defclass block ()
  ((statements :initform nil :accessor statements)))

(defclass identifier ()
  ((package :initarg :package :accessor package)
   (name :initarg :name :accessor name)))

(defclass literal ()
  ((value :initarg :value :accessor value)))

(defclass integer-literal (literal) ())

(defclass floating-point-literal (literal) ())

(defclass character-literal (literal) ())

(defclass string-literal (literal) ())

(defclass boolean-literal (literal) ())

(defclass null-literal (literal) ())

