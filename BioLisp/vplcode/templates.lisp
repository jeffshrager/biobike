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

;;; Author: JP Massar.  Fred.

;;; compute templates for all DEFINE-FUNCTION forms 

(bbl-define-function-templates)

;;; These are all the templates for the system except those defined
;;; via Exported BBL define-function forms.  

;;; To add a new template and have it show up in the palette, 
;;; you must:

;;; 1) Create a definition for the desired function or macro
;;; 2) Create the appropriate define-template form as below
;;; 3) Put the name of the function or macro into a module; 
;;;    This means putting it one of the documentation modules defined
;;;    in .../Doc/bbldf/
;;; 4) Export the symbol from the BBL package if it isn't already so 
;;;    exported by modifying .../bike/bbl-package.lisp (you may have to 
;;;    import it first if it isn't appropriate to put in one of the 
;;;    lists of symbols defined in bbl-package.lisp; see the defpackage
;;;    form)
;;; 5) Recompile bbl-package.lisp 
;;; 6) Recompile or reload this file
;;; 7) (setq vpl::*force-vpl-sysinit* t)
;;; 8) Refresh the VPL window -- the new symbol should show up in the 
;;;    palette corresponding to the module you put the symbol in in step 3, 
;;;    and when you bring it down, you should see the box structure
;;;    corresponding to the template you defined in step 2.  

;;; Lisp Macros and special forms


#||

PROGN
WITH-OPEN-FILE 
AND 
DEFUN 
PUSHNEW 
RETURN 
COND 
UNLESS 
POP 
PUSH 
OR 
WHEN 

||#

(defun ttests ()
  (do-external-symbols (s (find-package :bbl))
    (let ((oops-list nil))
      (handler-case 
          (when (get s :vpl-template) (tt s))
        (error () (push s oops-list)))
      (when oops-list 
        (loop for x in oops-list do (formatt "~A~%" x))
        ))))      

(defun tt (name) 
  (create-snippet-from-template nil (get name :vpl-template)))

(define-template progn :macro t (:body))

(define-template when :macro t 
  (:form "condition" lisp-boolean) (:body))

(define-template unless :macro t 
  (:form "condition" lisp-boolean) (:body))

(define-template let :macro t
  (:agg
   ""
   (:progn (:arg "local var") (:form "initialization" t)))
  (:body)
  )

(define-template with-open-file :macro t
  (:progn
    (:arg "stream symbol")
    (:form "stream" stream)
    (:keys-and-flags
     ((:keyword :if-exists :error (or keyword null)
       (:error :new-version :rename :rename-and-delete 
        :overwrite :append :supersede nil))
      (:keyword :if-does-not-exist :error 
       (or keyword null) (:error :create nil))
      (:keyword :direction :input keyword (:input :output :io :probe))
      )))
  (:body))

(define-template and :macro t
  (:zero-espliced "conditions" (:form "condition" lisp-boolean)))

(define-template or :macro t
  (:zero-espliced "conditions" (:form "condition" lisp-boolean)))

(define-template wlisp:pushnew :macro t
  (:form "value" t) (:place "place" t))

(define-template wlisp:push :macro t
  (:form "value" t) (:place "place" t))

(define-template wlisp:pop :macro t (:place "place" t))

(define-template cond :macro t
  (:zero-espliced "clauses" 
   (:progn (:form "condition" lisp-boolean) (:body))))

(define-template return :macro t (:form "return value" t))

(define-template return-from :macro t 
  (:arg "return-label") (:form "return value" t))

(define-template wlisp:defun :macro symbol 
  (:arg "name")
  (:zero "args" (:arg "arg") :display-one-hole t)
  (:body))

(define-template case :macro t
  (:form "discriminant" t)
  (:one-espliced "clauses" 
   (:progn (:one "constants" (:form "constant" t)) (:body))
   ))

(define-template typecase :macro t
  (:form "typed-object" t)
  (:one-espliced "clauses" 
   (:progn (:one "types" (:arg "type")) (:body))
   ))

;;; BBL macros

#||

BBL:LAST 
BBL:FIRST 
BBI:IF-TRUE 
BBI:IF-FALSE 
*** BBI:DEFCONVERSION 
*** BBI:ITEM 
*** BBI:ITEMS 
BBI:LOOP
BBI:FOR-EACH 
*** BBI:TABLE-FORMAT 
BBI:DEFINE-FUNCTION 
BBI:ASSIGN 
BBI:ORDER
BBI:JOIN
BBI:BBL-CODE

;; define-macro forms
convert
enter
swap
define
both
either
apply-function
insert
element-of-table

||#

(define-template bbl::program :macro t
  (:zero-espliced "forms" (:form "form" t) :display-one-hole t))

(define-template bbl::first :macro t
  (:optional-arg 
   "number" (("number" (:form "n" integer))) 
   :default :=splice-no-value=)
  (:token "in" (in in-each))
  (:form "entity" sequence)
  (:keys-and-flags ((:flag :nonstrict) (:flag :strict))))

(define-template BBL::everyones :macro t
  (:form "variable" symbol)
  (:keys-and-flags ((:flag :labeled) (:flag :simplify)))
  )

(define-template bbl::last :macro t
  (:optional-arg
   "number" (("number" (:form "n" integer))) 
   :default :=splice-no-value=)
  (:token "in" (in in-each))
  (:form "entity" sequence)
  (:keys-and-flags ((:flag :nonstrict) (:flag :strict))))

(define-template bbl::if-true :macro t
  (:form "value1" T)
  (:token "is" 
     (:IS :IS-NOT))
  (:token "TEST?" 
     (:same-as :greater-than :less-than 
      :between 
      := :< :> 
      :contained-in :a-subset-of 
      :matched-by-pattern :true-per :non-nil)
     )
  (:optional-arg "value2" (("value2" (:form "value2" t))) 
        :display-one-hole t :show-first-choice t :default :=splice-no-value=)
  (:progn (:literal "then" :class :loop-tag) (:body) :splice :newline)
  (:progn 
    (:literal "else" :class :loop-tag) (:body) 
    :reveal-label "else clauses" 
    :splice
    :newline
    )
  (:keys-and-flags 
   ((:flag :case-sensitive) (:keyword :by-position 1 number nil)
    (:flag :T-if-all-T) (:flag :T-if-any-T)
    )))
    
(define-template bbl::if-false :macro t
  (:form "value1" T)
  (:token "is" 
     (:IS :IS-NOT))
  (:token "TEST?" 
     (:same-as :greater-than :less-than 
      :between 
      := :< :> 
      :contained-in :subset-of 
      :matched-by-pattern :true-per :non-nil)
     )
  (:optional-arg "value2" (("value2" (:form "value2" t))) 
        :display-one-hole t :show-first-choice t :default :=splice-no-value=)
  (:progn (:literal "then" :class :loop-tag) (:body) :splice :newline)
  (:progn 
    (:literal "else" :class :loop-tag) (:body) 
    :reveal-label "else clauses" 
    :splice
    :newline
    )
  (:keys-and-flags 
   ((:flag :case-sensitive) (:keyword :by-position 1 number nil)
    (:flag :T-if-all-T) (:flag :T-if-any-T))))
   
(define-template bbl::condition :macro t
  (:one-espliced "clauses"
   (:progn (:form "condition" t) (:body))
   ))

(define-template bbl::loop :macro t
  
  ;; Initializations
  (:agg 
   "initializations"
   (:sprogn 
    (:literal "init" :class :loop-tag)
    (:arg "var")
    (:literal "=" :class :loop-tag)
    (:form "value" t)
    :delete-box
    :main-menu
    :htype :loop-init-clause
    )
   :splice
   :reveal-label "inits"
   :htype :loop-initializations
   :hidden-node
   :newline 
   :description "INITIALIZATION SECTION: Variables preset before loop begins"
   :options-label "inits"
   )

  ;; Iteration controls
  (:loop-iterator 
   "controls" 
   #.*loop-iter-multiple-complex-choice-clauses*
   :htype :loop-iterator :newline
   :description "CONTROLS: Determine how the loop executes and terminates"
   )

  ;; Body
  (:optional-labeled-clause "action"
   (("body" (:sprogn 
           (:literal "body" :class :loop-tag) 
           (:one-espliced "forms" (:form "form" t))
           :delete-box
           )))
   :reveal-label "action clause"
   :htype :loop-action :newline 
   :description "BODY: Forms to be iterated")
  
  ;; Aggregation
  (:optional-labeled-clause 
   "results"
   #.*loop-aggregate-choice-clauses*
   :reveal-label "aggregation clause" 
   :htype :loop-aggregate
   :hidden-node :newline 
   :description "RESULT SECTION: Determines result returned by the loop")
  
  ;; Finally
  (:optional-labeled-clause "finally" 
   (("finally" (:sprogn 
                (:literal "finally" :class :loop-tag) 
                (:form "action" t)
                :delete-box
                )))
   :reveal-label "final action"
   :htype :loop-finally
   :hidden-node :newline
   :description "FINAL ACTION: Performed after last iteration")
  )

(define-template bbl::for-each :macro t

  ;; Primary control
  (:for-each-primary-iterator
   "Primary"
   #.*for-each-initial-iter-complex-choice-clauses* 
   :htype :for-each-primary-iter ;; :newline
   :description "PRIMARY CONTROL FOR LOOP")

  ;; Additional loop controls 
  (:loop-iterator 
   "update" 
   #.*loop-control-clause-multiple-complex-choice-clauses*
   :htype :additional-loop-controls :newline 
   :reveal-label "additional controls"
   :description "ADDITIONAL CONTROLS FOR LOOP")

  ;; Initializations
  (:agg
   "initializations"
   (:sprogn 
    (:literal "INITIALIZE" :class :loop-tag)
    (:arg "var")
    (:literal "=" :class :loop-tag)
    (:form "value" t)
    :delete-box
    :main-menu
    :htype :loop-init-clause
    )
   :reveal-label "inits"
   :htype :loop-initializations
   :newline :splice
   :description "INITIALIZATION SECTION: Variables preset before iteration begins."
   :options-label "initial"
   )

  (:loop-iterator 
   "update" 
   #.*loop-as-clause-multiple-complex-choice-clauses*
   :htype :loop-variable-update :newline 
   :reveal-label "var update"
   :description "VARIABLE UPDATE SECTION: Resets loop variables each iteration")

  ;; Body
  (:optional-labeled-clause "action"   
   (("body" (:sprogn 
           (:literal "body" :class :loop-tag) 
           (:one-espliced "forms" (:form "form" t))
           :delete-box
           )))
   :reveal-label "action clause"
   :htype :loop-action :newline
   :description "BODY: Forms to be iterated")

  ;; Results
  (:optional-labeled-clause 
   "results"
   #.*loop-aggregate-choice-clauses*
   :reveal-label "aggregation clause" 
   :htype :loop-aggregate
   :newline
   :description "RESULTS SECTION: Determines result returned by loop")

  ;; Finally
  (:optional-labeled-clause "finally" 
   (("finally" (:sprogn 
                (:literal "finally" :class :loop-tag) 
                (:form "action" t)
                :delete-box
                )))
   :reveal-label "finally clause"
   :htype :loop-finally
   :newline
   :description "FINAL ACTION: Performed after last iteration")
  )


(define-template bbl::increment :macro number 
  (:form "place" t) 
  (:keys-and-flags ((:keyword :by 1 number nil))))

(define-template bbl::decrement :macro number 
  (:form "place" t) 
  (:keys-and-flags ((:keyword :by 1 number nil))))

(define-template bbl::assign :macro t
  (:one-espliced 
   "assignments" 
   (:sprogn 
    (:var-or-list-of-vars 
     "mode"
     (("single var" (:place "var" t))
      ("multiple vars"
      (:sprogn 
       (:place "var" t) (:place "var" t) 
       (:&rest-noshow "vars" (:place "var" t))))))
    (:literal "=" :class :equal-sign)
    (:form "value" t)
    :delete-box
    ))
  (:keys-and-flags ((:flag :display-off)))
  )

(define-template bbl::define :macro t
  ;; maybe someday.  problem with execute trapping this form and
  ;; the fact that it always produces a list.  
  #+not-yet
  (:var-or-list-of-vars 
   "mode"
   (("single var" (:place "var" t))
      ("multiple vars"
      (:sprogn 
       (:place "var" t) (:place "var" t) 
       (:&rest-noshow "vars" (:place "var" t))))))
  (:place "var" t)
  (:literal "=" :class :define-function-token)
  (:form "value" t)
  (:keys-and-flags 
    ((:flag :display-off) (:flag :labeled))))

(define-template bbl::order :macro t
  (:form "any" t) 
  (:simple-required-choice 
   "comparison" 
   (:< :<= := :>= :> :same :greater-than :less-than :equal :equalp))
  (:form "any" t)
  (:agg
   "more comparisons"
   (:sprogn
    (:simple-required-choice 
     "comparison" 
     (:< :<= := :>= :> :same :greater-than :less-than :equal :equalp))
    (:form "any" t))
   :splice
   ))

(define-template bbl::make :macro t
  (:simple-required-choice  
   "What?"
   (:list :string :table))
  (:keys-and-flags 
   ((:keyword :how-big 1 t nil)
    (:keyword :initial-element nil t nil)
    )))

#||

(:keys-and-flags 
   ((:flag :after-end) 
    (:flag :relaxed)
    (:keyword :before 0 number nil)
    (:keyword :replacing 0 number nil)
    (:keyword :after 0 number nil)
    (:keyword :repeating-from 0 number nil)
    (:keyword :to 0 number nil)))

||#



(define-template bbl::convert :macro t
  (:token "each" (:each))
  (:form "entity" t)
  (:literal "to" :class :define-function-token)
  (:form "type" symbol)
  )

(define-template bbl::time-space-usage :macro t
  (:form "form" t))

(define-template bbl::enter :macro t
  (:form "package-name" (or symbol string)))

(define-template bbl::define-function :macro symbol
  
  (:arg "name") 

  ;; summary
  (:optional-labeled-clause "summary" 
   (("summary" 
     (:sprogn 
      (:literal "summary" :class :define-function-tag)
      (:form "short description" string :insert-type :string)
      :delete-box
      )))
   :htype :df-summary
   :reveal-label "summary" 
   :newline 
   :description "SUMMARY: Description of function"
   )

  ;; required
  (:sprogn 
   (:literal "required" :class :define-function-tag)
   (:zero "parameters" (:arg "arg")
    :description "REQUIRED ARGUMENTS: Their names"
    :htype :df-required)
   :newline)

  (:sprogn 
  
   ;; flags
   (:optional-labeled-clause "flags" 
    (("flags" 
      (:sprogn 
       (:literal "flags" :class :define-function-tag)
       (:zero "flags" (:arg "boolean-var"))
       :delete-box
       )))
    :htype :df-flags
    :reveal-label "flag arguments"
    :hidden-node 
    :newline
    :description "Options (T or F)"
    )
  
   ;; keys
   (:optional-labeled-clause "keywords"
    (("keys"
      (:sprogn
       (:literal "keywords" :class :define-function-tag)
       (:zero
        "keys" 
        (:sprogn 
         (:arg "var")
         (:literal "=" :class :loop-tag) 
         (:form "default value" t)
         :delete-box
         ))
       :delete-box
       )))
    :htype :df-keys
    :reveal-label "keyword arguments"
    :hidden-node
    :newline
    :description "Options governing values"
    )
  
   ;; types
   (:optional-labeled-clause "types"
    (("types"
      (:sprogn
       (:literal "type" :class :define-function-tag)
       (:zero
        "type" 
        (:sprogn 
         (:arg "var")
         (:literal "=" :class :loop-tag)
         (:arg "type")
         :delete-box
         ))
       :delete-box
       )))
    :hidden-node
    :reveal-label "type declarations"
    :newline
    :description "Type specifications for arguments and option values"
    :htype :df-types
    )
  
   ;; conversions
   (:optional-labeled-clause "conversions"
    (("conversions"
      (:sprogn
       (:literal "convert" :class :define-function-tag)
       (:zero
        "convert" 
        (:sprogn
         (:arg "var")
         (:literal "from" :class :loop-tag)
         (:arg "type")
         (:literal "to" :class :loop-tag)
         (:arg "type")
         :delete-box
         ))
       :delete-box
       )))
    :hidden-node
    :reveal-label "argument conversion specifications"
    :newline
    :description "Conversions of arguments and option values"
    :htype :df-conversions
    )

   ;; map
   (:optional-labeled-clause "map"
    (("map"
      (:sprogn
       (:simple-required-choice "map type" (:map :maptree :mapcarnn :crossmap))
       (:zero "map" (:arg "df-required-arg") :display-one-hole t)
       :delete-box
       )))
    :hidden-node
    :reveal-label "mapping of required arguments"
    :newline
    :description "specifies mapping mode for one or more required arguments"
    :htype :df-map
    )

   ;; initializations of local variables 
   (:optional-labeled-clause "inits"
    (("initializations" 
      (:sprogn 
       (:literal "init" :class :define-function-tag) 
       (:one-espliced
        "inits"
        (:sprogn
          (:var-or-list-of-vars 
           "mode"
           (("single var" (:place "var" t))
            ("multiple vars"
             (:sprogn 
              (:place "var" t) (:place "var" t) 
              (:&rest-noshow "vars" (:place "var" t))))))
          (:literal "=" :class :equal-sign)
          (:form "value" t))
        ))))
    :hidden-node 
    :reveal-label "local initializations"
    :newline
    :description "initializations of local variables"
    :htype :df-initializations
    )

   :newline
 ; :hidden-node 
   :reveal-label "detailed specs"
   :htype :df-optional-clauses
   :main-menu
   :description "SPECIFICATIONS (optional): of arguments and options"
   )
  
  ;; body
  (:sprogn
   (:literal "body" :class :define-function-tag)
   (:zero-espliced "body-forms" (:form "form" t)
    :description "BODY: Forms to be executed"
    :htype :df-body)
   :newline
   )
  )

(define-template bbl::both :macro t
  (:form "form" t) (:literal "and" :class :loop-tag) (:form "form" t) 
  (:agg 
   "more forms" 
   (:sprogn (:literal "and" :class :loop-tag) (:form "form" t))
   :splice
   ))

(define-template bbl::either :macro t
  (:form "form" t) (:literal "or" :class :loop-tag) (:form "form" t) 
  (:agg 
   "more forms" 
   (:sprogn (:literal "or" :class :loop-tag) (:form "form" t))
   :splice
   ))

(define-template bbl::swap :macro t
  (:form "form" t) (:literal "and" :class :loop-tag) (:form "form" t))

(define-template bbl::union-of :macro list 
  (:form "list1" list) (:form "list2" list)
  (:agg "more-lists" (:form "list" list) :splice))

(define-template bbl::intersection-of :macro list
  (:form "list1" list) (:form "list2" list)
  (:agg "more-lists" (:form "list" list) :splice))

(define-template bbl::interleave :macro list 
  (:form "list" list)
  (:zero-espliced "list" (:form "list" list))
  (:keys-and-flags ((:flag :truncate) (:flag :simplify))))

(define-template bbl::repeat-function :macro t
 ;(:form "function" (or symbol list))
  (:form "function" t)
  (:form "number" number)
  (:literal "times" :class :loop-tag)
  )

(define-template bbl::apply-function :macro t
  (:form "function" t)
  (:literal "replacing" :class :loop-tag)
  (:one "argnames" (:arg "variable"))
  (:literal "with" :class :loop-tag) 
  (:one "arguments" (:form "list" t) :splice)
  (:keys-and-flags 
   ((:flag :+display) (:flag :-display)))
  )

(define-template bbl::join :macro t
  (:form "list-or-string" (or list string))
  (:zero-espliced "more items" (:form "item" t))
  (:keys-and-flags 
   ((:flag :as-string) (:flag :as-list) (:keyword :by "" t nil))))

(define-template bbl::true? :macro boolean
  (:token "" (:each :the-entity))
  (:form "value1" T)
  (:token "is" 
     (:IS :IS-NOT))
  (:token "TEST?" 
     (:same-as :greater-than :less-than 
      :between 
      := :< :> 
      :contained-in :a-subset-of 
      :matched-by-pattern :true-per :non-nil)
     )
  (:token "" (:each :the-entity))
  (:optional-arg "value2" (("value2" (:form "value2" t))) 
        :display-one-hole t :show-first-choice t :default :=splice-no-value=)
  (:keys-and-flags 
   ((:flag :case-sensitive) (:flag :inclusive) 
    (:flag :T-if-all-T) (:flag :T-if-any-T)
    (:flag :count)
    (:keyword :by-position 1 number nil))))


(define-template bbl::filter :function list
  (:form "list" List)
  (:literal "keeping-items" :class :loop-tag)
  (:token "--/NOT" (:NOT))
  (:simple-required-choice "test"
     (:same-as :greater-than :less-than 
      :between 
      := :< :> 
      :contained-in :containing
      :a-subset-of :containing-the-subset
      :matched-by-pattern :true-per)
     )
  (:form "value" T)
  (:keys-and-flags 
   ((:flag :case-sensitive) (:flag :inclusive) 
    (:keyword :by-position 1 number nil))))


(define-template bbl::bbl-code :macro t
  (:form "code" t :insert-type :code))

(define-template bbl::insert :macro t
  (:token "each" (:each))
  (:form "string-or-item" t)
  (:token "into" (:into :into-each :into-copy-of :into-each-copy-of))
  (:form "target" (or string list))
  (:keys-and-flags 
   ((:flag :after-end) 
    (:flag :relaxed)
    (:keyword :before 0 number nil)
    (:keyword :replacing 0 number nil)
    (:keyword :after 0 number nil)
    (:keyword :repeating-from 0 number nil)
    (:keyword :to 0 number nil))))

(define-template bbl::element/s-of-table :macro t
  (:form "table" (or sequence bbl::table))
  (:zero "indices" (:form "index" t)))
  
(define-template bbl::element/s-of :function t
  (:token "" (:each :the-entity))
  (:form "table/list/string/frame" (or string bbl::table list bbi::frame))
  (:one-espliced "more indices" (:form "index(s)" t))
  (:keys-and-flags 
   ((:flag :-display) (:flag :+display) 
         (:flag :+label-entities) (:flag :+label-items))))


(define-template bbl::table :function utilities::garray 
  #| (:optional-arg "specs" 
       (:sprogn (:form "dimension spec" T))
       :default NIL) 
  (:&rest-noshow "specs"
     (:sprogn (:form "dimension spec" T))
     :splice
     ) |#
  (:keys-and-flags 
     ((:flag :numeric-indices) 
      (:flag :not-adjustable)
      (:flag :embedded-labels)
      (:keyword :dimension/s NIL number nil)
      (:keyword :default-value nil T nil)
      (:keyword :initialize-to nil list nil)
      (:keyword :labeled-with 3 string nil)
      (:keyword :specs NIL T nil)
      ))
)

;; special case for []s 

;; The "" as the first element is some hack whose purpose I don't
;; recall, but apparently necessary.

(progn 
  (assign-new-template-id 'ref)
  (setf (get 'ref :vpl-template) 
        '(ref :macro 
              ((:constant "") 
               (:form "table" array) 
               (:literal "[" :class :ref-tag)
               (:one-espliced "indices" (:form "index" t))
               (:literal "]" :class :ref-tag))
              t)))

(progn 
  (assign-new-template-id 'bbl::range-ref)
  (setf (get 'bbl::range-ref :vpl-template) 
        '(ref :macro 
              ((:constant "") 
               (:form "table" array) 
               (:literal "[" :class :ref-tag)
               (:form "from-index" integer)
               (:literal "->" :class :ref-tag)
               (:form "to-index" integer)
               (:literal "]" :class :ref-tag))
              t)))

(defmacro bbl::range-ref (&rest goo)
  `(ref ,@goo))

(progn 
  (assign-new-template-id 'bbi::%curly-list%)
  (setf (get 'bbi::%curly-list% :vpl-template) 
        '(bbi::%curly-list% 
          :macro 
          ((:literal "{" :class :curly-tag)
           (:&rest "items" (:form "item" t))
           (:literal "}" :class :curly-tag))
          list
          )))



;;; Lisp functions

(define-template + :function number 
  (:form "number" number) (:form "number" number)
  (:&rest-noshow "numbers" (:form "number" number)))

(define-template * :function number 
  (:form "number" number) (:form "number" number)
  (:&rest-noshow "numbers" (:form "number" number)))

(define-template - :function number 
  (:form "number" number) (:&rest "numbers" (:form "number" number)))

(define-template / :function number 
  (:form "number" number) (:&rest "numbers" (:form "number" number)))

(define-template > :function boolean
  (:form "number" real) (:form "number" real)
  (:&rest-noshow "numbers" (:form "number" real)))

(define-template < :function boolean
  (:form "number" real) (:form "number" real)
  (:&rest-noshow "numbers" (:form "number" real)))

(define-template <= :function boolean
  (:form "number" real) (:form "number" real)
  (:&rest-noshow "numbers" (:form "number" real)))

(define-template >= :function boolean
  (:form "number" real) (:form "number" real)
  (:&rest-noshow "numbers" (:form "number" real)))

(define-template = :function boolean
  (:form "number" number) (:form "number" number)
  (:&rest-noshow "numbers" (:form "number" number)))

(define-template /= :function boolean
  (:form "number" number) (:form "number" number)
  (:&rest-noshow "numbers" (:form "number" number)))

(define-template bbl::abs :function number 
  (:form "number" (or number bbi::number-list)))

#|
(define-template mod :function number 
  (:form "number" number) (:form "divisor" number))

(define-template exp :function number (:form "number" number))
|#
(define-template bbl::mod :function number 
  (:form "number" (or number bbi::number-list)) 
  (:form "divisor" (or number bbi::number-list)))

(define-template bbl::exp :function number 
  (:form "exponent" (or number bbi::number-list)))

(define-template bbl::log :function number 
  (:form "number" (or number bbi::number-list))
  (:optional-arg "base" (("base" (:form "base" number))) :default 2.7182817))

(define-template bbl::exp :function number 
  (:form "number" (or number bbi::number-list)))

(define-template bbl::sqrt :function number 
  (:form "number" (or number bbi::number-list)))

(define-template floor :function number
  (:form "number" real) 
  (:optional-arg "divisor" (("divisor" (:form "divisor" real))) :default 1))

(define-template cos :function number (:form "number" number))

(define-template sin :function number (:form "number" number))

(define-template tan :function number (:form "number" number))

(define-template acos :function number (:form "number" number))

(define-template asin :function number (:form "number" number))

(define-template atan :function number (:form "number" number))

(define-template list :function list (:&rest "items" (:form "item" t))) 

(define-template print :function t
  (:form "any" t)
  (:optional-arg "stream" 
   (("stream" 
     (:form "stream" (or (member t) stream)))) 
   :default lisp:*standard-output*
   ))

#+not-used
(define-template substitute :function sequence
  (:form "new element" t) 
  (:form "old element" t)
  (:form "sequence" sequence)
  (:keys-and-flags  
   ((:keyword :from-end nil lisp-boolean (t nil))
    (:keyword :test eql function-designator t)
    (:keyword :start 0 integer t)
    (:keyword :end (length sequence) (or integer nil) t)
    )))

(define-template reverse :function sequence (:form "sequence" sequence))

(define-template not :function boolean (:form "any" t))

(define-template identity :function t (:form "any" t))

(define-template sleep :function t (:form "seconds" integer))

;;; BBL functions (defined with DEFUN)

#|
(define-template bbl::sum-of :function number 
  (:form "number" (or number list)) 
  (:&rest "numbers" (:form "number" (or number list))))

(define-template bbl::product-of :function number 
  (:form "number" (or number list)) 
  (:&rest "numbers" (:form "number" (or number list))))

(define-template bbl::difference-of :function number 
  (:form "number" (or number list)) 
  (:&rest "numbers" (:form "number" (or number list))))

(define-template bbl::quotient-of :function number 
  (:form "number" (or number list)) 
  (:&rest "numbers" (:form "number" (or number list))))
|#


(define-template bbl::sum-of :function number 
  (:form "number" (or number list)) 
  (:&rest "numbers"
     (:sprogn (:literal "+" :class :loop-tag)
              (:form "number" (or number list)))
     :splice)
  )

(define-template bbl::product-of :function number 
  (:form "number" (or number list)) 
  (:&rest "numbers"
     (:sprogn (:literal "*" :class :loop-tag)
              (:form "number" (or number list)))
     :splice)
  )

(define-template bbl::difference-of :function number 
  (:form "number" (or number list)) 
  (:&rest "numbers"
     (:sprogn (:literal "-" :class :loop-tag)
              (:form "number" (or number list)))
     :splice)
  )

(define-template bbl::quotient-of :function number 
  (:form "number" (or number list)) 
  (:&rest "numbers"
     (:sprogn (:literal "/" :class :loop-tag)
              (:form "number" (or number list)))
     :splice)
  )

(define-template bbl::max-of :function number 
  (:form "number" (or number list)) 
  (:&rest "numbers" (:form "number" (or number list))))

(define-template bbl::min-of :function number 
  (:form "number" (or number list)) 
  (:&rest "numbers" (:form "number" (or number list))))

(define-template 
 bbl::negation-of :function number
 (:form "number" (or number list)))
                 

(define-template bbl::previous-result :function t)

(define-template bbl::result :function t
  (:form "result-index" integer) 
  (:optional-arg "index" (("index" (:form "value-index" integer))) :default 1))

(define-template bbl::is-nonnegative? :function boolean
  (:form "any" t))

(define-template bbl::is-positive-integer? :function boolean
  (:form "any" t))

(define-template bbl::is-positive-number? :function boolean
  (:form "any" t))
  
(define-template bbl::is-negative-number? :function boolean
  (:form "any" t))
  
(define-template bbl::is-even? :function boolean
  (:form "any" t))
  
(define-template bbl::is-odd? :function boolean
  (:form "any" t))
  
(define-template bbl::is-integer? :function boolean
  (:form "any" t))

(define-template bbl::is-gene? :function boolean
  (:form "any" t))

(define-template bbl::is-protein? :function boolean
  (:form "any" t))

(define-template bbl::is-simple-list? :function boolean (:form "list" list))

(define-template bbl::my-functions :function t
  (:keys-and-flags 
   ((:keyword :as-list t boolean nil) (:keyword :verbose t boolean nil))))

(define-template bbl::my-variables :function t
  (:keys-and-flags 
   ((:keyword :as-list t boolean nil) (:keyword :verbose t boolean nil))))

(define-template bbl::display :function null 
  (:form "object" t)
  (:agg "objects" (:form "object" t) :splice))

(define-template bbl::display-line :function null 
  (:one-espliced "objects" (:form "object" t)))

(define-template bbl::display-list :function null 
  (:token "" (:each :the-entity))
  (:form "list or object" t)
  (:agg "objects" (:form "list or object" t) :splice)
  (:keys-and-flags 
     ((:flag :flush-left) 
      (:flag :centered)
      (:flag :flush-right)
      (:keyword :alignment NIL list nil)
      (:keyword :columns-of-length nil (OR number list) nil)
      (:keyword :labels nil list nil)
      (:keyword :padding 3 number nil)
      )))

#||

  (:form "object" t)
  (:agg "objects" (:form "object" t) :splice))

||#

(define-template bbl::display-data :function null 
  (:form "object" t)
  (:agg "objects" (:form "object" t) :splice))

;;; VPL functions replacing BBL functions defined with DEFINE-FUNCTION

(define-template bbl::sort :function t
  (:form "list" list)
  (:keys-and-flags 
   ((:flag :ascending)
    (:flag :descending)
    (:flag :case-sensitive)
    (:keyword :by-position nil integer nil)
    (:keyword :then-sort-ascending-by nil integer nil)
    (:keyword :then-sort-descending-by nil integer nil))
   ))

;;; Biolisp functions (defined with DEFUN)

(define-template bio::aa-to-1-letter-code :function t
  (:form "amino acid" t) 
  (:optional-arg "as" (("as" (:form "as" keyword))) :default :string))

(define-template bio::aa-to-3-letter-code :function t
  (:form "amino acid" t) 
  (:optional-arg "as" (("as" (:form "as" keyword))) :default :string))

(define-template bio::aa-to-codons :function t
  (:form "amino acid" t) 
  (:optional-arg "as" (("as" (:form "as" keyword))) :default :strings))

(define-template bio::aa-to-long-name :function t
  (:form "amino acid" t) 
  (:optional-arg "as" (("as" (:form "as" keyword))) :default :string))

(define-template bio::aa-to-mw :function t
  (:form "amino acid" t) 
  (:optional-arg "as" (("as" (:form "as" keyword))) :default :integer))

(define-template bio::codon-to-aa :function t
  (:form "codon" t) 
  (:optional-arg "as" (("as" (:form "as" keyword))) :default :string))

(define-template bio::my-stuff :function t)

(define-template bio::email-me :function t 
  (:form "object" t)
  (:keys-and-flags 
   ((:keyword :to nil string nil) (:keyword :subject nil string nil))))

(define-template google :function t)   
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-template bbl::comment :macro t
  (:form "comment" string :insert-type :string))

(define-template bbl::help :macro t
  (:form "name" t))

(define-template
    bbl::replace :macro t
  (:token "into" (:into :into-each :into-copy-of :into-copy-of-each))
  (:form "target" t) 
  (:keys-and-flags 
   ((:keyword :replacing-every nil t nil)
    (:keyword :replacing-each nil t nil)
    (:keyword :replacing-first nil t nil)
    (:keyword :with nil t nil)
    (:keyword :with-each nil t nil)
    (:keyword :from nil (or null integer) nil)
    (:keyword :to nil (or null integer) nil)
    (:keyword :at nil (or null integer) nil)
    (:flag :relaxed) 
    (:flag :strict)
    (:flag :case-sensitive)
    )))

(define-template
    bbl::insert :macro t
  (:token "into" (:into :into-each :into-copy-of :into-copy-of-each))
  (:form "target" t) 
  (:token "each" (:each))
  (:form "insert-stuff" t) 
  (:keys-and-flags 
   (
    (:keyword :before nil t nil)
    (:keyword :after nil t nil)
    (:flag :after-end)
    (:flag :relaxed) 
    (:flag :strict)
    (:flag :case-sensitive)
    )))

(define-template wb::create-vpl-web-service :macro t
  (:one "keyword argument names" (:form "argname" t))
  (:form "code body or function name" t)
  (:keys-and-flags 
   ((:keyword :service-name nil t nil)
    (:keyword :service-class nil t nil)
    )))

(define-template wb::help-me-program-this :function t
  (:form "Code description" string :insert-type :string)
  (:optional-arg 
   "code-block" (("code-block" (:form "code-block" t))) 
   :default nil))

    
(define-template wb::in-sequence :macro t
  (:progn 
    (:arg "result-symbol")
    (:keys-and-flags 
     ((:keyword :under-construction nil t nil))))
  (:body)
  )
  
(define-template lisp:go :macro t
  (:arg "goto-tag"))

(define-template wb::in-parallel :macro t
  (:progn
    (:arg "result-symbol")
    (:keys-and-flags 
     ((:keyword :wait-for :every t nil)
      (:keyword :combine-with :list t nil))))
  (:body)
  )

(define-template wb::exit-workflow :function t
  (:form "Value" t))

(define-template bbl::when-value-of :macro t
  (:form "Any" t)
  (:one-espliced "Tests and action clauses" 
   (:sprogn
    (:literal "is" :class :loop-tag)
    (:form "value" t)
    (:&rest-noshow "ors" 
     (:sprogn
      (:literal "or" :class :loop-tag)
      (:form "value" t)
      ))
    (:literal "then" :class :loop-tag)
    (:one-espliced "action" (:form "action" t))
    :htype :loop-action
    :newline
    ))
  (:optional-labeled-clause "otherwise" 
   (("otherwise" 
     (:sprogn
      (:literal "otherwise" :class :loop-tag)
      (:one-espliced "action" (:form "action" t)))))
   :htype :loop-action
   :newline)
  (:keys-and-flags ((:flag :case-sensitive)))
  )

(define-template bbi::sequence-viewer :function t
  (:keys-and-flags 
   ((:keyword :organism nil t nil)
    (:keyword :contig nil t nil)
    (:keyword :from nil t nil)
    (:keyword :to nil t nil)
    (:keyword :rows nil t nil)
    (:keyword :columns nil t nil)
    (:keyword :search nil t nil)
    )))

(define-template bbi::calc :macro t
  (:form "infix-expression" (or string list))
  (:token "minus-action" 
   (:always :only-with-whitespace :unary-and-whitespace)
   ))

(define-template
    bbl::share :macro t
  (:form "name" t) 
  (:keys-and-flags 
   (
    (:keyword :functions nil t nil)
    (:keyword :variables nil t nil)
    (:keyword :docstring nil t nil)
    (:flag :remove)
    )))

(define-template 
    bbl::unshare :macro t
  (:form "name" t)
  )