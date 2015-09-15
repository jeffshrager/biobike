;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi) 

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

(defun df-mapcar (f &rest mapargs)
  #.(one-string-nl
     "Map F over MAPARGS in a way similar to MAPCAR except"
     "that if a) none of the arguments are lists F is called on the arguments"
     "and the result is returned instead of a list of results; b) if"
     "some of the arguments are lists and some aren't, then every argument"
     "which is not a list is treated as if it were a list whose length"
     "is as long as the longest list argument, and whose value is"
     "the original value of the argument; c) if all the arguments are lists,"
     "then DF-MAPCAR is identical to MAPCAR.")
  (cond
   ((null mapargs) (error "Internal error."))
   ;; special case each of one argument, two arguments, and three arguments
   ;; for efficiency as these are the most common cases
      ((null (cdr mapargs))
    (let ((a (first mapargs)))
      (if (listp a)
          (mapcar f a)
        (funcall f a))))
   ((null (cddr mapargs))
    (let* ((a (first mapargs))
           (b (second mapargs))
           (a? (listp a))
           (b? (listp b)))
      (cond
       ((and a? b?) (mapcar f a b))
       ((and (not a?) (not b?)) (funcall f a b))
       (t 
        (loop 
         until (or (and a? (null a)) (and b? (null b)))
         as a-atom = (if a? (pop a) a)
         as b-atom = (if b? (pop b) b)
         collect (funcall f a-atom b-atom)
         )))))
   ((null (cdddr mapargs))
    (let* ((a (first mapargs))
           (b (second mapargs))
           (c (third mapargs))
           (a? (listp a))
           (b? (listp b))
           (c? (listp c)))
      (cond
       ((and a? b? c?) (mapcar f a b c))
       ((and (not a?) (not b?) (not c?)) (funcall f a b c))
       (t 
        (loop 
         until (or (and a? (null a)) (and b? (null b)) (and c? (null c)))
         as a-atom = (if a? (pop a) a)
         as b-atom = (if b? (pop b) b)
         as c-atom = (if c? (pop c) c)
         collect (funcall f a-atom b-atom c-atom)
         )))))
   ;; general case of arbitrary number of arguments
   (t
    (let ((lists? (mapcar 'listp mapargs)))
      (cond
       ((every 'identity lists?) (apply 'mapcar f mapargs))
       ((every 'null lists?) (apply f mapargs))
       (t
        (loop 
         until (some (lambda (list? value) (and list? (null value)))
                      lists? mapargs)
         as atomic-args = 
         (loop for list? in lists? 
               for remaining-mapargs on mapargs
               as next = (first remaining-mapargs)
               as value = (if list? (first next) next)
               do
               (when list? (setf (first remaining-mapargs) (cdr next)))
               collect value
               )
         collect
         (apply f atomic-args)
         )))))))

(defun df-mapcarnn (f &rest mapargs)
  (cond
   ((null mapargs) (error "Internal error."))
   ;; special case each of one argument, two arguments, and three arguments
   ;; for efficiency as these are the most common cases
   ((null (cdr mapargs))
    (let ((a (first mapargs)))
      (if (listp a)
          (mapcarnn f a)
        (funcall f a))))
   ((null (cddr mapargs))
    (let* ((a (first mapargs))
           (b (second mapargs))
           (a? (listp a))
           (b? (listp b)))
      (cond
       ((and a? b?) (mapcarnn f a b))
       ((and (not a?) (not b?)) (funcall f a b))
       (t 
        (loop 
         until (or (and a? (null a)) (and b? (null b)))
         as a-atom = (if a? (pop a) a)
         as b-atom = (if b? (pop b) b)
         as result = (funcall f a-atom b-atom)
         when result collect result
         )))))
   ((null (cdddr mapargs))
    (let* ((a (first mapargs))
           (b (second mapargs))
           (c (third mapargs))
           (a? (listp a))
           (b? (listp b))
           (c? (listp c)))
      (cond
       ((and a? b? c?) (mapcarnn f a b c))
       ((and (not a?) (not b?) (not c?)) (funcall f a b c))
       (t 
        (loop 
         until (or (and a? (null a)) (and b? (null b)) (and c? (null c)))
         as a-atom = (if a? (pop a) a)
         as b-atom = (if b? (pop b) b)
         as c-atom = (if c? (pop c) c)
         as result = (funcall f a-atom b-atom c-atom)
         when result collect result
         )))))
   (t
    (let ((lists? (mapcar 'listp mapargs)))
      (cond
       ((every 'identity lists?) (apply 'mapcarnn f mapargs))
       ((every 'null lists?) (apply f mapargs))
       (t
        (loop 
         until (some (lambda (list? value) (and list? (null value)))
                     lists? mapargs)
         as atomic-args = 
         (loop for list? in lists? 
               for remaining-mapargs on mapargs
               as next = (first remaining-mapargs)
               as value = (if list? (first next) next)
               do
               (when list? (setf (first remaining-mapargs) (cdr next)))
               collect value
               )
         as result = (apply f atomic-args)
         when result collect result
         )))))))

(defun df-crossmap (f a b)
  (cond
   ((and (not (listp a)) (not (listp b)))
    (funcall f a b))
   ((not (listp a))
    (df-crossmap f (list a) b))
   ((not (listp b))
    (df-crossmap f a (list b)))
   (t
    (loop for a-atom in a 
          nconc 
          (loop for b-atom in b
                collect
                (funcall f a-atom b-atom))))))


(defun df-maptree (f tree &rest trees)
  (flet ((maptree-argnames () 
           (mapcar 
            'first 
            (find-df-clause-data 
             :maptree (get *df-name* :define-function-parse))))
         (oops-not-equivalent-string ()
           (one-string-nl
            "The specification for ~S says that some of its required"
            "arguments are to be mapped over using the 'maptree' algorithm."
            "This 'maptree' algorithm requires that each of these arguments,"
            "assuming they are lists, have equivalent list (tree) structure."
            "But at least one of the lists you provided as a value for"
            "one of these mapped arguments is not equivalent in structure"
            "to one or more of the others."
            "The arguments to ~S specified as mapped are named:")))
    (cond
     ((or (null tree) (some 'null trees)) nil)
     ((and (consp tree) (every 'consp trees))
      (when *safety* 
        (unless (apply 'equivalent-tree-structure? tree trees)
          (dfee
           (one-string-nl
            (oops-not-equivalent-string)
            "~S"
            (apply 
             's+ 
             (loop for treearg in (cons tree trees)
                   for j from 1 
                   collect 
                   (formatn "The ~:R mapped value is: ~S~%" j treearg))))
           *df-name* *df-name* (maptree-argnames))))
      (apply 'maptree f tree trees))
     ((and (atom tree) (every 'atom trees))
      (apply f tree trees))
     (t 
      (let* ((df-args (cons tree trees))
             (tree-args (remove-if 'atom df-args))
             (atomic-args (mapcar (lambda (x) (if (atom x) x nil)) df-args))
             (calling-args (make-list (length df-args))))
        (when *safety* 
          (unless (apply 'equivalent-tree-structure? tree-args)
            (dfee
             (one-string-nl
              (oops-not-equivalent-string)
              "~S"
              (apply 
               's+ 
               (loop for treearg in df-args
                     for j from 1 
                     collect 
                     (if (consp treearg) 
                         (formatn
                          "The value of the ~:R mapped argument is: ~S~%"
                          j treearg)
                       (formatn 
                        (one-string-nl
                         "The value of the ~:R mapped argument, ~S,"
                         "is atomic and is therefore not being mapped over.")
                        j treearg)))))
             *df-name* *df-name* (maptree-argnames))))
        (case (length tree-args) 
          (0 (error "This is impossible."))
          (1 
           (maptree 
            (lambda (t1) 
              (merge-atomic-and-one-tree-arg calling-args atomic-args t1)
              (apply f calling-args))
            (first tree-args)))
          (2 
           (maptree
            (lambda (t1 t2)
              (merge-atomic-and-two-tree-args calling-args atomic-args t1 t2)
              (apply f calling-args))
            (first tree-args) (second tree-args)))
          (otherwise 
           (apply 
            'maptree 
            (lambda (&rest args) 
              (merge-atomic-and-tree-args calling-args atomic-args args)
              (apply f calling-args))
            tree-args
            ))))))))
           
           
                                                
 
(defun merge-atomic-and-one-tree-arg (calling-args atomic-args tree-arg)
  (loop for aa in atomic-args 
        for remaining-args on calling-args
        do 
        (setf (first remaining-args) (or aa tree-arg))))

(defun merge-atomic-and-two-tree-args (calling-args atomic-args t1 t2)
  (loop for aa in atomic-args 
        for remaining-args on calling-args
        do 
        (setf (first remaining-args) 
              (or aa (prog1 t1 (setq t1 t2))))))

(defun merge-atomic-and-tree-args (calling-args atomic-args tree-args)
  (loop for aa in atomic-args
        for remaining-args on calling-args
        do
        (setf (first remaining-args) (or aa (pop tree-args)))
        ))

           

          
      
                 
    
  