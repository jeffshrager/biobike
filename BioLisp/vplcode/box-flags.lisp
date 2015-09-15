;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl) 

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

(defmethod text-flags ((snippet snippet)) nil)

(defmethod text-flags ((snippet literal-snippet))
  (vpl-internal-error "text-flags on literal-snippet should never get called!"))

(defmethod text-flags ((snippet toplevel-output-snippet))
  (vif (p (get-snippet-property snippet :printout))
       (if (plusp (length p))
           (list :jbml-color "#0000FF" :jbml-b)
         nil)
       nil))

(defmethod text-flags ((snippet call-snippet)) 
  (cond
   ((get-snippet-property snippet :monitoring-enabled)
    (list :jbml-color "#bb0000" :jbml-b))
   (t (list :jbml-b))
   ))

(defmethod text-flags ((snippet form-snippet))
  (list :jbml-color "#B7410E" :jbml-i))

(defmethod text-flags ((snippet symbol-snippet))
  (when (get-snippet-property snippet :monitoring-enabled)
    (list :jbml-color "#BB0000")
    ))

(defun single-output-value-text-flags () nil)

(defmethod box-flags ((snippet snippet)) (list :jbml-outdent))

(defmethod box-flags ((snippet keyword-snippet))
  (list :jbml-outdent 
        :jbml-background-color *elhai-key-and-flag-background-color*
        ))

(defmethod box-flags ((snippet flag-snippet))
  (list :jbml-outdent 
        :jbml-background-color *elhai-key-and-flag-background-color*
        ))

(defmethod box-flags ((snippet progn-snippet))
  (let ((htype (get-snippet-property snippet :htype))
        (standard-flags '(:jbml-outdent :jbml-no-outline)))
    (if htype 
        (or (htype-box-flags htype) (copy-list standard-flags))
      (copy-list standard-flags)
      )))

(defmethod box-flags ((snippet constant-snippet))
  (list :jbml-outdent :jbml-background-color "#eeeeee"))

(defmethod box-flags ((snippet symbol-snippet))
   (list :jbml-outdent :jbml-background-color "#eeeeee"))

(defmethod box-flags ((snippet form-snippet))
  (if (get-snippet-property snippet :hole-open)
      (list :jbml-outdent :jbml-background-color "#ebebeb")
    (list :jbml-outdent :jbml-background-color *unopened-hole-background-color*)
    ))

(defmethod box-flags ((snippet aggregate-snippet))
  (let ((htype (get-snippet-property snippet :htype))
        (standard-flags '(:jbml-outdent :jbml-no-outline)))
    (if htype 
        (or (htype-box-flags htype) (copy-list standard-flags))
      (copy-list standard-flags)
      )))

(defmethod box-flags ((snippet choice-snippet))
  (let ((htype (get-snippet-property snippet :htype))
        (standard-flags '(:jbml-outdent :jbml-no-outline)))
    (if htype 
        (or (htype-box-flags htype) (copy-list standard-flags))
      (copy-list standard-flags)
      )))

(defmethod htype-box-flags ((htype t)) nil)

(defmethod htype-box-flags ((htype (eql :for-each-primary-iter)))
  (list :jbml-background-color "#00FF00" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :additional-loop-controls)))
  (list :jbml-background-color "#00FF00" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :loop-initializations)))
  (list :jbml-background-color "#00FF00" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :loop-iterator)))
  (list :jbml-background-color "#00FF00" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :loop-condition)))
  (list :jbml-background-color "#00FF00" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :loop-variable-update)))
  (list :jbml-background-color "#08E8DE" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :loop-action)))
  (list :jbml-background-color "#08E8DE" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :loop-aggregate)))
  (list :jbml-background-color "#08E8DE" :jbml-outdent :jbml-no-outline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod htype-box-flags ((htype (eql :df-required)))
  (list :jbml-background-color "#00FF00" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :df-optional-clauses)))
  (list :jbml-background-color "#00FF00" :jbml-outdent))


(defmethod htype-box-flags ((htype (eql :loop-finally)))
  (list :jbml-background-color "#FF007F" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :df-summary)))
  (list :jbml-background-color "#00FF00" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :df-flags)))
  (list :jbml-background-color "#00DD00" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :df-keys)))
  (list :jbml-background-color "#00DD00" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :df-types)))
  (list :jbml-background-color "#00DD00" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :df-conversions)))
  (list :jbml-background-color "#00DD00" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :df-map)))
  (list :jbml-background-color "#00DD00" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :df-initializations)))
  (list :jbml-background-color "#abcdef" :jbml-outdent :jbml-no-outline))

(defmethod htype-box-flags ((htype (eql :df-body)))
  (list :jbml-background-color "#08E8DE" :jbml-outdent :jbml-no-outline))

