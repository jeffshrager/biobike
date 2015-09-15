;;; -*- Package:pb; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :user)

(defpackage :pb 
  (:use :Biolisp :bioutils :wlisp :utilities :frames :genericdb :webuser 
   :published-protocols :net.html.generator :net.aserve)
  (:import-from :bio *go-frames*)
  )


