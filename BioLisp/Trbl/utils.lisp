;;; -*- Package:pb; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :pb)

(defun pubid->pubframe (id)
  (frame-fnamed (formatn "Pub.~D" id)))

(defun year->year-frame (year)
  (frame-fnamed (format nil "year.~a" year)))
