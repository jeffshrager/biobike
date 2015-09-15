;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cl-user; -*-

(in-package :cl-user)

;; All this comes from information from the alisp8 image.

(ff:def-foreign-call memory_status_dump ((filename (* :char)))
                     :arg-checking nil :strings-convert t)

;; This prints out the layout of memory to standard output.
;; You need this to figure out where to start the lisp heap.  
;; You must find a free contiguous chunk of at least LISP-HEAP-SIZE + 
;; C-HEAP-SIZE.  
(defun show-memory-layout () (memory_status_dump 0))

;; The value used for the LISP-HEAP-START was for a memory region following
;; the last alisp8.dxl entry in the printout.  

(defparameter lisp-heap-start #x2000100000)

;; Originally we made the heap size 1 gigabyte (expt 2 30) for Ixion.
;; Ixion has 4 gigs of memory, so I thought it would be reasonable to make it
;; 1.5 gigabytes since we usually only have two big weblisteners running.

(defparameter lisp-heap-size (+ (expt 2 30) (expt 2 29)))

;; We took the c-heap-size from the printout from (room) by taking 
;; the difference of the last column (resrve) from the first column (the
;; start of the heap).  This turned out to be a megabyte.  

(defparameter c-heap-size (expt 2 20))


;; Builds an image with the c heap immediately above the lisp heap.  This
;; causes allegro to signal a memory condition if the lisp heap tries to grow 
;; above LISP-HEAP-SIZE, instead of allocating more virtual memory and eventually
;; thrashing because the image starts using swap space.  

;; NAME should be a .dxl file
(defun build-restricted-image (name lisp-heap-start lisp-heap-size c-heap-size)
  (let ((c-heap-start (+ lisp-heap-start lisp-heap-size)))
    (excl:build-lisp-image 
     name
     :lisp-heap-start lisp-heap-start
     :lisp-heap-size lisp-heap-size
     :c-heap-start c-heap-start
     :c-heap-size c-heap-size
     )))

