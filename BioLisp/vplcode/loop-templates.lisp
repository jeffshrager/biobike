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

;; Format is (iter-clause-name template menu-item-listing)
;; The actual menu operator is one of the CHOICE selection operators

(defparameter *loop-as-clause-info* 
  '(
    #+wrong
    ;; This is actually a loop control clause, not an initialization or 
    ;; variable update.  In the FOR-EACH template it was under variable update,
    ;; while in LOOP it was under Controls.  If it were to go anywhere it should
    ;; be under Controls for LOOP and under Additional Controls for FOR-EACH.
    ;; But Elhai suggested we just take it out and forget about users being able
    ;; to iterate on successive sublists.  
    (:for-on-single
     (:sprogn (:literal "assign" :class :loop-tag) (:arg "var")
      (:literal "on" :class :loop-tag) (:form "list" list)
      (:optional-labeled-arg "by"
       (("by"
         (:sprogn (:literal "by" :class :loop-tag) (:arg "step function")))))
      :delete-box
      :newline
      )
     "ASSIGN variable = list-part"
     )

    (:for-equal-single
     (:sprogn (:literal "assign" :class :loop-tag) (:arg "var") 
      (:literal "=" :class :equal-sign) (:form "initial value" t) 
      (:optional-labeled-arg "then"
       (("then"
         (:sprogn (:literal "then" :class :loop-tag) (:form "next value" t)))))
      :delete-box
      :newline
      )
     "ASSIGN variable = value" 
     )))

(defparameter *loop-control-clause-info*
  `(
    (:for-in-single 
     (:sprogn (:literal "for" :class :loop-tag) (:arg "var")
      (:literal "in" :class :loop-tag) 
      (:form "collection" ,*loop-control-collection-types*)
      (:optional-labeled-arg "by" 
       (("by" 
         (:sprogn (:literal "by" :class :loop-tag) (:arg "step function"))
         )))
      :delete-box
      :newline
      )
     "FOR variable IN collection"
     )

    (:for-in-multiple
     (:sprogn (:literal "for" :class :loop-tag)
      (:one "vars" (:arg "var"))
      (:literal "in" :class :loop-tag)
      (:form "list" list)
      (:optional-labeled-arg "by"
       (("by"
         (:sprogn (:literal "by" :class :loop-tag) (:arg "step function")))))
      :delete-box
      :newline
      )
     "FOR (var1 var2...) IN list"
     )

    (:for-equal-multiple
     (:sprogn (:literal "for" :class :loop-tag) 
      (:one "vars" (:arg "var")) 
      (:literal "=" :class :equal-sign) (:form "initial values" list) 
      (:optional-labeled-arg "then"
       (("then" 
        (:sprogn 
         (:literal "then" :class :loop-tag) 
         (:form "next values" list)
         ))))
      :delete-box
      :newline
      )
     "FOR (var1 var2...) = value-list" 
     )

    (:for-from-to
     (:sprogn (:literal "for" :class :loop-tag) (:arg "var") 
      (:literal "from" :class :loop-tag) (:form "first value" integer) 
      (:literal "to" :class :loop-tag) (:form "last value" integer) 
      (:optional-labeled-arg "by"
       (("by"
         (:sprogn (:literal "by" :class :loop-tag) (:form "step" integer)))))
      :delete-box
      :newline
      )
     "FOR variable FROM n1 TO n2" 
     )

    (:for-from-below
     (:sprogn (:literal "for" :class :loop-tag) (:arg "var") 
      (:literal "from" :class :loop-tag) (:form "first value" integer) 
      (:literal "below" :class :loop-tag) (:form "last value" integer) 
      (:optional-labeled-arg "by"
       (("by" 
         (:sprogn (:literal "by" :class :loop-tag) (:form "step" integer)))))
      :delete-box
      :newline
      )
     "FOR variable FROM n1 BELOW n2" 
     )

    (:for-from
     (:sprogn (:literal "for" :class :loop-tag) (:arg "var") 
      (:literal "from" :class :loop-tag) (:form "first value" integer) 
      (:optional-labeled-arg "by"
       (("by" 
        (:sprogn 
         (:literal "by" :class :loop-tag)
         (:form "step function" integer)
         ))))
      :delete-box
      :newline
      )
     "FOR variable FROM n1 (without limit)" 
     )       
    
    (:for-from-downto
     (:sprogn (:literal "for" :class :loop-tag) (:arg "var") 
      (:literal "from" :class :loop-tag) (:form "first value" integer) 
      (:literal "downto" :class :loop-tag) (:form "last value" integer) 
      (:optional-labeled-arg "by"
       (("by"
        (:sprogn 
         (:literal "by" :class :loop-tag)
         (:form "step" integer)
         ))))
      :delete-box
      :newline
      )
     "FOR variable FROM n1 DOWNTO n2" 
     )
    
    (:while
     (:sprogn
      (:literal "while" :class :loop-tag)
      (:form "condition" lisp-boolean)
      :delete-box
      :newline
      )     
     "WHILE (condition)" 
     )

    (:until
     (:sprogn 
      (:literal "until" :class :loop-tag)
      (:form "condition" lisp-boolean)
      :delete-box
      :newline
      )
     "UNTIL (condition)" 
     )

    ))

(defun loop-info->choice-clause (data) (list (third data) (second data)))

(defparameter *loop-as-clause-templates* 
  (mapcar 'second *loop-as-clause-info*))

(defparameter *loop-as-clause-multiple-complex-choice-clauses* 
  (mapcar 'loop-info->choice-clause *loop-as-clause-info*))

(defparameter *loop-control-clause-templates*
  (mapcar 'second *loop-control-clause-info*))

(defparameter *loop-control-clause-multiple-complex-choice-clauses* 
  (mapcar 'loop-info->choice-clause *loop-control-clause-info*))

(defparameter *loop-iter-info* 
  (append *loop-control-clause-info* *loop-as-clause-info*))

(defparameter *loop-iter-templates*
  (mapcar 'second *loop-iter-info*))

(defparameter *loop-iter-multiple-complex-choice-clauses* 
  (mapcar 'loop-info->choice-clause *loop-iter-info*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *for-each-initial-iter-info*
  `(
    (:for-in-single 
     (:sprogn (:arg "var") (:literal "in" :class :loop-tag)
      (:form "collection" ,*loop-control-collection-types*)
      (:optional-labeled-arg "by"
       (("by"
         (:sprogn (:literal "by" :class :loop-tag) (:arg "step function")))))
      :delete-box
      :newline
      )
     "variable IN collection"
     )

    (:for-from-to
     (:sprogn (:arg "var") 
      (:literal "from" :class :loop-tag) 
      (:form "first value" integer) (:literal "to" :class :loop-tag) 
      (:form "last value" integer) 
      (:optional-labeled-arg "by"
       (("by" 
         (:sprogn (:literal "by" :class :loop-tag) (:form "step" integer)))))
      :delete-box
      :newline
      )
     "number FROM n1 TO n2"
     )
    
    (:for-from-downto
     (:sprogn (:arg "var") (:literal "from" :class :loop-tag) 
      (:form "first value" integer) (:literal "downto" :class :loop-tag) 
      (:form "last value" integer) 
      (:optional-labeled-arg "by"
       (("by" 
         (:sprogn (:literal "by" :class :loop-tag) (:form "step" integer)))))
      :delete-box
      :newline
      )
     "number FROM n1 DOWN TO n2"
     )

    (:for-from
     (:sprogn (:arg "var") (:literal "from" :class :loop-tag)
      (:form "first value" integer) 
      (:optional-labeled-arg "by"
       (("by" 
         (:sprogn 
          (:literal "by" :class :loop-tag)
          (:form "step function" integer)
          ))))
      :delete-box
      :newline
      )
     "number FROM n1 (without limit)"
     )

    (:for-from-below
     (:sprogn (:arg "var") (:literal "from" :class :loop-tag) 
      (:form "first value" integer) (:literal "below" :class :loop-tag) 
      (:form "last value" integer) 
      (:optional-labeled-arg "by"
       (("by" 
         (:sprogn (:literal "by" :class :loop-tag) (:form "step" integer)))))
      :delete-box
      :newline
      )
     "number FROM n1 BELOW n2"
     )

    (:for-in-multiple
     (:sprogn
      (:one "vars" (:arg "var")) 
      (:literal "in" :class :loop-tag)
      (:form "list" list)
      (:optional-labeled-arg "by"
       (("by" 
         (:sprogn (:literal "by" :class :loop-tag) (:arg "step function")))))
      :delete-box
      :newline
      )
     "(var1 var2 ...) IN list"
     )

    ))

(defparameter *for-each-initial-iter-templates*
  (mapcar 'second *for-each-initial-iter-info*))

(defparameter *for-each-initial-iter-complex-choice-clauses* 
  (mapcar (lambda (x) (list (third x) (second x))) 
          *for-each-initial-iter-info*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *loop-aggregate-choice-clauses*
  '(
    ("append" 
     (:sprogn 
      (:literal "append" :class :loop-tag)
      (:form "value" t)
      :delete-box))
    ("when ... append"
     (:sprogn
      (:literal "when" :class :loop-tag)
      (:form "condition" lisp-boolean)
      (:literal "append" :class :loop-tag)
      (:form "value" t)
      :delete-box))
    ("collect" 
     (:sprogn 
      (:literal "collect" :class :loop-tag)
      (:form "value" t) 
      :delete-box))
    ("when ... collect"
     (:sprogn
      (:literal "when" :class :loop-tag)
      (:form "condition" lisp-boolean)
      (:literal "collect" :class :loop-tag)
      (:form "value" t)
      :delete-box))
    ("min"
     (:sprogn 
      (:literal "min" :class :loop-tag)
      (:form "value" t)
      :delete-box))
    ("when ... min"
     (:sprogn
      (:literal "when" :class :loop-tag)
      (:form "condition" lisp-boolean)
      (:literal "min" :class :loop-tag)
      (:form "value" t)
      :delete-box))
    ("max" 
     (:sprogn
      (:literal "max" :class :loop-tag)
      (:form "value" t)
      :delete-box))
    ("when ... max"
     (:sprogn
      (:literal "when" :class :loop-tag)
      (:form "condition" lisp-boolean)
      (:literal "max" :class :loop-tag)
      (:form "value" t)
      :delete-box))
    #+not-wanted
    ("nconc"
     (:sprogn
      (:literal "nconc" :class :loop-tag)
      (:form "value" t)
      :delete-box))
    ("sum" 
     (:sprogn 
      (:literal "sum" :class :loop-tag)
      (:form "value" t)
      :delete-box))
    ("when ... sum"
     (:sprogn
      (:literal "when" :class :loop-tag)
      (:form "condition" lisp-boolean)
      (:literal "sum" :class :loop-tag)
      (:form "value" t)
      :delete-box))
    ("count" 
     (:sprogn 
      (:literal "count" :class :loop-tag)
      (:form "value" t)
      :delete-box))
    ("when ... count"
     (:sprogn
      (:literal "when" :class :loop-tag)
      (:form "condition" lisp-boolean)
      (:literal "count" :class :loop-tag)
      (:form "value" t)
      :delete-box))
    ))

