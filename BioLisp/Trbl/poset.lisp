;;; -*- Package:pb; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :pb)


;;; Poset code itself moved to .../Bioutils/poset.lisp


(defun go-parents (n &key (isa? t) (partof? t) (obsolete? nil))
  (let ((parents 
         (cond 
          ((and isa? partof?) 
           (append (#^isa n) (#^partof n)))
          (isa? (#^isa n))
          (partof? (#^partof n))
          (t (error "Must specify either isa or partof")))))
    (when obsolete? 
      (setq parents (remove-duplicates parents)))
    parents
    ))

(defun go-children (n &key (subclasses? t) (parts? t) (obsolete? nil))
  (let ((children 
         (cond 
          ((and subclasses? parts?) 
           (append (#^subclasses n) (#^parts n)))
          (subclasses? (#^subclasses n))
          (parts? (#^parts n))
          (t (error "Must specify either subclasses or parts")))))
    (when obsolete? 
      (setq children (remove-duplicates children)))
    children
    ))


(defun go-parents-f1 (n) (#^isa n))
(defun go-children-f1 (n) (#^subclasses n))

(defun go-parents-f2 (n) (#^partof n))
(defun go-children-f2 (n) (#^parts n))

(defun go-parents-f3 (n) (append (go-parents-f1 n) (go-parents-f2 n)))
(defun go-children-f3 (n) 
  (let ((c1 (go-children-f1 n)) (c2 (go-children-f2 n))) 
    (cond
     ((and c1 c2) (remove-duplicates (append c1 c2)))
     (t (or c1 c2))
     )))


(defun go-parents-f4 (n)
  (remove-if (lambda (x) (#^obsolete x)) (go-parents-f3 n)))

(defun go-children-f4 (n)
  (remove-if (lambda (x) (#^obsolete x)) (go-children-f3 n)))


(defun go-ancestor? (g1 g2)
  (ancestor? g1 g2 :parent-function 'go-parents-f3 :if= t))

(defun go-ancestor-exclusive? (g1 g2)
  (ancestor? g1 g2 :parent-function 'go-parents-f3 :if= nil))


(defun next-go-level (nodes generator-function)
  (purge-duplicates
   (mapcan
    (lambda (x) (copy-list (funcall generator-function x)))
    nodes)))
  


