(defpackage "JAVA-PARSER"
  (:use "COMMON-LISP" "PARSER"))

(in-package "JAVA-PARSER")

;; 4.1 The Kinds of Types and Values

(defprod type () (/ primitive-type reference-type))

;; 4.2 Primitive Types and Values

(defprod primitive-type () (/ numeric-type "boolean"))
(defprod numeric-type () (/ integral-type floating-point-type))
(defprod integral-type () (/ "byte" "short" "int" "long" "char"))
(defprod floating-point-type () (/ "float" "double"))

;; 4.3 Reference 

(defprod reference-type () (/ class-or-interface-type array-type))
(defprod class-or-interface-type () (/ class-type interface-type))
(defprod class-type () (type-name))
(defprod interface-type () (type-name))
(defprod array-type () (type #\[ #\]))

;; 6.5 Determining the Meaning of a Name

(defprod package-name ()         (identifier (* #\. identifier)))
(defprod type-name ()            (/ identifier (package-or-type-name #\. identifier)))
(defprod expression-name ()      (/ identifier (ambiguous-name #\. identifier)))
(defprod method-name ()          (/ identifier (ambiguous-name #\. identifier)))
(defprod package-or-type-name () (/ identifier (* #\. identifier)))
(defprod ambiguous-name ()       (/ identifier (* #\. identifier)))


;; 7.3 Compilation Units

(defprod compilation-unit ()
  ((? package-declaration) (* import-declaration) (* type-declaration)))


;; 7.4.1 Package Declarations

(defprod package-declaration () ("package" package-name))

;; 7.5 Import Declarations

(defprod import-declaration ()
  (/ single-type-import-declaration type-import-on-demand-declaration))

;; 7.5.1

(defprod single-type-import-declaration () ("import" type-name ";"))

;; 7.5.2

(defprod type-import-on-demand-declaration ()
  ("import" package-or-type-name "." "*" ";"))

;; 7.6 Top Level Type Declarations

(defprod type-declaration ()
  (/ class-declaration interface-declaration ";"))
  
  
