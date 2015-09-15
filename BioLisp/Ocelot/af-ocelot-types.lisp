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

(def-frame-class #$OC.OcelotNode () ())

(defmethod wob-html :after ((frame OC.OcelotNode))
  (let ((*current-object* frame))
    (flet ((title (s) (html :p (:b (:i (:princ-safe s))) :br)))
      (when (#^oc.parents frame)
        (title "BioCyc Parents Hierarchy")
        (emit-ancestor-frames frame #^oc.parents #^oc.subclasses))
      (when (#^oc.component-of frame)
        (title "BioCyc Components Hierarchy")
        (emit-ancestor-frames frame #^oc.component-of #^oc.components))
      (when (#^oc.subclasses frame)
        (title "Biocyc Subclasses Tree")
        (emit-descendant-frames frame #^oc.subclasses))
      (when (#^oc.components frame)
        (title "BioCyc Components Tree")
        (emit-descendant-frames frame #^oc.components))
      )))
    
