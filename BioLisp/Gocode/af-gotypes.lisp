;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bio; -*-

(in-package :aframes)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
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

;;; Author:  JP Massar. 


;;; Definition of GONODE frametype.

;; The Go.Isa slot is used in almost every go frame but it cannot be
;; a fixed slot because the frame system does not allow you to place
;; daemons on fixed slots and we need a daemon to make go.isa and 
;; go.subclasses to be inverses. Ditto for Go.Partof and Go.Parts,
;; although that slot is used by only a small number of go frames. 

;; There is also a Go.ecref slot but it is not used
;; by the vast majority of go nodes.
;; There is also a slot called Go.synonym which is apparently never
;; provided.  
;; There is also a slot called Go.namespace which is filled in somewhere
;; but we can't figure out where...

(def-frame-class 
 #$Go.GoNode ()
 ((#$Go.goid :domain integer :allocation :instance :initform 0)
  (#$Go.definition :allocation :instance :initform nil)
  (#$Go.prettyname :allocation :instance :initform nil)
  (#$Go.dbxrefs :allocation :instance :initform nil)
  ))
               

(def-frame-class #$Go.ReactionNode (#$Go.GoNode) ())

(def-frame-class #$Go.Molecule () ())

(def-frame-class #$Go.Ec.Enzyme () ())
 

(defmethod wob-html :after ((frame Go.GoNode))
  (let ((*current-object* frame))
    (flet ((title (s) (html :p (:b (:i (:princ-safe s))) :br)))
    (when (#^go.isa frame)
      (title "Go Isa Hierarchy")
      (emit-ancestor-frames frame #^go.isa #^go.subclasses))
    (when (#^go.partof frame)
      (title "Go PartOf Hierarchy")
      (emit-ancestor-frames frame #^go.partof #^go.parts))
    (when (#^go.subclasses frame)
      (title "Go Subclasses Tree")
      (emit-descendant-frames frame #^go.subclasses))
    (when (#^go.parts frame)
      (title "Go Parts Tree")
      (emit-descendant-frames frame #^go.parts))
    )))
