 ;;; -*- Package: data-editor; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :data-editor)

;;; +=========================================================================+
;;; | Copyright (c) 2009 JP Massar                                            |
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

;;; Author: JP Massar


(defparameter *object-editor-css-file* "frame-editor5")

(defvar *edit-object* nil)

(defvar *display-edit-box?* nil)

(defvar *object-key* nil)
(defvar *object-display-type* nil)
(defvar *user-base* nil)

;; For parent link mechanism
(defvar *key-for-this-page* nil)
(defvar *parent-key* nil)

(defparameter *data-editor-slot-value-length* 35)

(defparameter *sequence-editor-value-length* 35)
(defparameter *sequence-max-rows-to-show* 40)

(defparameter *data-editor-cache-duration* 30 "in minutes")

(defvar *sequence-object-first-index* nil)
(defvar *sequence-object-last-index* nil)
(defvar *sequence-object-size* nil)
(defvar *sequence-start* nil)
(defvar *sequence-end* nil)
(defvar *sequence-ntoshow* nil)
(defvar *sequence-ncolumns* nil)
(defvar *sequence-edit-element* nil)
(defvar *sequence-edit-element-type* nil)


(defparameter *default-fesn-delimiter* #\! "FESN -- Frame Editor Slot Name")
(defparameter *frame-editor-slot-name-length* 25)
(defparameter *frame-editor-column-split* 15)
(defparameter *sequence-editor-url* "sequence-editor")
(defparameter *array-editor-url* "array-editor")

(defvar *edit-slot-name* nil)
(defvar *noshow-slot-names* nil)
(defvar *slots-to-show* nil)
(defvar *edit-slot* nil)
(defvar *frame-editor-error-message* nil)
(defvar *hidden-box-class* nil)

(defparameter *caps-reduction-factor* 1/3)

