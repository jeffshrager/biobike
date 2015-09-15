;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Peter Seibel |
;;; | |
;;; | Permission is hereby granted, free of charge, to any person obtaining |
;;; | a copy of this software and associated documentation files (the |
;;; | "Software"), to deal in the Software without restriction, including |
;;; | without limitation the rights to use, copy, modify, merge, publish, |
;;; | distribute, sublicense, and/or sell copies of the Software, and to |
;;; | permit persons to whom the Software is furnished to do so, subject to |
;;; | the following conditions: |
;;; | |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software. |
;;; | |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. |
;;; +=========================================================================+

;;; Authors: Emily Niman, Bogdan Mihai, Arnaud Taton, Jeff Elhai, JP Massar, Peter Seibel

;;======= examples =====

(def-topic "BioBIKE Examples"
(:text
(:p ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "BioBIKE Examples"))) "BioBIKE Examples"))
(:table :align "middle" :width "80%"
(:tr
(:td (:small
(:ol :type "1"
(:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to sequences"))) (:b "Examples related to sequences")))

(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to proteins and translation"))) (:b "Examples related to proteins and translation")))

(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to orthologous relationships"))) (:b "Examples related to orthologous relationships")))

(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to searching for queries"))) (:b"Examples related to searching for queries")))

(:p) (:li ((:a :style "color:#0000EE" :href (:print (make-help-topic-url :name "Examples related to microarray data"))) (:b "Examples related to microarray data")))

)))))
(:p)
(:table :align "middle" :width "80%"
(:tr (:b
(:td :width "26.7%" :align "left" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "How to get started")))(:button " Previous Page ")))
(:td :width "26.7%":align "middle" ((:a :style "text-decoration:none" :href (:print (make-help-topic-url :name "Help for BBL Users")))(:button "Help for BBL Users")))
(:td :width "26.7%":align "right" ((:a :style "text-decoration:none" :href (:print(make-help-topic-url :name "Examples related to sequences")))(:button " Next Page ")))
)))))









