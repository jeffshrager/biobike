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

;;; Author: JP Massar, Mark Slupesky.

(defmacro hsafe (string) `(html (:princ-safe ,string)))

;; Create a popup window which contains a help text based on either the node
;; type or a specific label provided in a subtemplate

(defun-opcode
    help-function (snippet-id)
  (not-modified!)
  (let* ((snippet (find-snippet-in-workspace-or-output-history snippet-id))
         (htype (get-snippet-property snippet :htype)))
    (typecase snippet 
      (call-snippet 
       (let ((symbol (real-call-snippet-function-name snippet)))
         (ulog "Help request for ~S~%" symbol)))
      (otherwise 
       (ulog "Help request on node of type ~A~%" (type-of snippet))
       ))
    (let ((catch-result 
           (catch :call-snippet-transfer 
             (progn
               (create-and-use-unique-file 
                (user-temp-vpl-dir)
                (lambda (file p)
                  (declare (ignore file))
                  (with-html-to-stream
                      p 
                    (:html 
                     (:pre 
                      (if htype 
                          (html-for-htype-help htype snippet)
                        (html-for-snippet-help snippet)
                        )))))
                (lambda (file) (show-created-help-page file))
                :name (s+ "help-" (string wb::*sessionid*))
                :type "html")
               nil
               ))))
      (when catch-result (show-immediate-help-page catch-result))
      )))

(defun show-help-for-symbol (symbol)
  (let ((catch-result 
         (catch :call-snippet-transfer 
           (progn
             (create-and-use-unique-file 
              (user-temp-vpl-dir)
              (lambda (file p)
                (declare (ignore file))
                (with-html-to-stream
                    p 
                  (:html 
                   (:pre 
                    (html-for-symbol-help symbol :toplevel)
                    ))))
              (lambda (file) (show-created-help-page file))
              :name (s+ "help-" (string wb::*sessionid*))
              :type "html")
             nil
             ))))
    (when catch-result (show-immediate-help-page catch-result))
    ))

(defun show-immediate-help-page (url)
  (show-vpl-popup-URL-window
   url
   :relative-p 0
   :width "800px" :height "800px"
   :menubar "yes" :location
   "yes" :directories "yes" :status "yes"
   ))

(defun show-created-help-page (file)
  (show-vpl-popup-URL-window
   (wb::publish-path-for-file-in-viewable-subdir file)
   :relative-p 0
   :width "800px" :height "800px"
   ))
              

(defmethod html-for-snippet-help ((snippet snippet))
  (vpl-internal-error
   "Need help method for node of type ~A!" (type-of snippet)))

;;; Constant snippet 

(defmethod html-for-snippet-help ((snippet constant-snippet)) 
  (hsafe
   (let ((has-a-hole? (snippet-replaced-hole? snippet)))
     (s+ 
      #.(one-string-nl
         "This is a node that contains a value; generally one"
         "you have typed in or inserted."
         ""
         "The standard operations on this node are discussed in the"
         "VPL tutorial found in Help -> VPL UI.")
      (if has-a-hole? (edit-option-string) "")
      ))))

;;; Symbol snippet

(defmethod html-for-snippet-help ((snippet symbol-snippet)) 
  (hsafe
   (let ((has-a-hole? (snippet-replaced-hole? snippet)))
     (s+ 
      #.(one-string-nl
         "This is a node that contains a symbol; generally one"
         "you have typed in or inserted.  The symbol either has a value"
         "or the code you are creating is giving it a value."
         ""
         "The standard operations on this node are discussed in the"
         "VPL tutorial found in Help -> VPL UI.")
      (if has-a-hole? (edit-option-string) "")
      ))))

(defun edit-option-string ()
  #.(one-string-nl
     ""
     "In addition, you can use the edit option."
     "This option replaces the node with an input box,"
     "which contains the original value.  You can change this value"
     "by normal text editing operations, and then hit ENTER to"
     "create a new node with a new value."))

(defmethod html-for-snippet-help ((snippet progn-snippet))
  (hsafe
   #.(one-string-nl
      "If you see this the system creators failed to tag this node"
      "and give it a help description!  Please report this message"
      "to the system administrators."
      )))

;;; Value-form snippet

(defmethod html-for-snippet-help ((snippet value-form-snippet)) 
  (hsafe
   #.(one-string-nl
      "Clicking on this node brings up a text input box."
      "You can enter into this box:"
      "-Symbols and variables -- something you might"
      "have set with ASSIGN or DEFINE."
      "-Constants, which include numbers, T, and NIL, whose"
      "values you can NOT set."
      "-Strings"
      "-Lists"
      "Common pitfalls here include entering an unbound variable --"
      "one to which no value has been assigned -- or entering a"
      "something like T or NIL into a function that takes only"
      "numbers, like +."  
      ""
      "Instead of typing a value into the hole value it is also"
      "possible to bring down another function to be nested inside"
      "the first.  This new function works just as if you called"
      "it by itself and will of course have its own holes and syntax."
      "Once filled in correctly, its resulting value becomes the"
      "value for the :form node of the outer function.")))

;;; Argument snippet

(defmethod html-for-snippet-help ((snippet argument-snippet))
  (if (not (get-snippet-property snippet :place))
      (hsafe
       #.(one-string-nl
          "Clicking on this node brings up a text input box."
          "You can only enter into this box the name of a variable."
          "If you try to enter anything other than a variable name,"
          "you'll get an error."
          ))
    (hsafe 
     #.(one-string-nl
        "Clicking on this node brings up a text input box."
        "You can enter into this box the name of a variable,"
        "or bring down the special function '[]', which will allow"
        "you to begin to define a table element."
        ""
        "If you try to enter anything other than a variable name or"
        "'[]', you'll get an error."
        ))))

;;; Aggregate snippet

(defmethod html-for-snippet-help ((snippet aggregate-snippet)) 
  (hsafe
   (cond 
    ((null (get-snippet-property snippet :one-form-required))         
     #.(one-string-nl
        "This node allows you to enter any number (including 0)"
        "of additional objects or forms.  Use the"
        "  -- 'Add another' option to create one more hole (at the end),"
        "  -- 'Add two more' option to create two more holes (at the end)."
        "Use the 'Clear' icon in the upper right to delete all the objects"
        "in this box, or individually by clicking the 'Delete' icon"
        "in each of the smaller boxes."
        "You can enter objects between others in this box by using the"
        "'Add left' or 'Add right' menu options attached to each object."
        ))
    (t
     #.(one-string-nl
        "This node allows you to enter any number of additional"
        "objects or forms (at least 1 object is required)."
        "Use the"
        "  -- 'Add another' option to create one more hole (at the end),"
        "  -- 'Add two more' option to create two more holes (at the end)."
        ""
        "Use the 'Clear' icon in the upper right to delete all the objects"
        "in this box except for the first one (which is cleared,"
        "not deleted), or individually by clicking the 'Delete' icon"
        "in each of the smaller boxes."
        ""
        "You can enter objects between other objects in this box by using"
        "the 'Add left' or 'Add right' menu options attached to each object."
        )))))

;;; Uniform-choice snippet

(defmethod html-for-snippet-help ((snippet uniform-choice-snippet)) 
  (let ((choice-type (get-snippet-property snippet :choice-type)))
    (case choice-type 
      (:token 
       (html-for-token-help snippet)
       (return-from html-for-snippet-help nil))
      (:optional-arg 
       (html-for-optional-arg-help snippet)
       (return-from html-for-snippet-help nil))
      (otherwise nil))
    (hsafe
     (case choice-type
       (:optional-labeled-arg 
        #.(one-string-nl
           "This node, optional, allows the user to specify additional"
           "information to a loop or for-each clause controlling the iteration"
           "of the loop.  By selecting the only non-help menu option"
           "a label and an input box are revealed.  The consequences of"
           "filling in the resulting input box depend"
           "on the particular loop subclause being"
           "specified.  For more information see information about LOOP"
           "and/or FOR-EACH."
           ))
       (:simple-choice
        #.(one-string-nl
           "This node gives you a choice of a number of different literal"
           "values to use.  You MUST select one of them before trying to"
           "execute your code.  The meaning of each choice is determined"
           "by the function this node is enclosed in."
           ))
       (:loop-iterator
        ":loop-iterator should be further discriminated using :htype!")
       (:for-each-primary-iterator
        ":for-each-primary should be further discriminated using :htype!")
       (:define-function-options 
        ":define-function-opt should be further discriminated using :htype!")
       (:var-or-list-of-vars 
        #.(one-string-nl
           "You have the choice here of using a single variable (the default),"
           "or a set of variables.  Select the 'multiple vars' option"
           "to allow you to enter two or more variables.  If you clear this"
           "node, all the variables will disappear and you will have to"
           "re-select either 'single var' or 'multiple vars' to get"
           "input boxes to reappear."
           ))
       (otherwise 
        (formatn "Need descriptive text for choice type ~S here!" choice-type)
        )))))

(defun function-symbol-name-from-call-snippet (call-snippet)
  (snippet-value (first (snippet-children call-snippet))))

(defun html-for-token-help (snippet)
  (let ((call-snippet (snippet-parent snippet)))
    (hsafe
     (formatn 
      #.(one-string-nl
         "This is an optional flag you can insert here.  Click on"
         "one of the choices to select that choice, or click"
         "on Clear to remove a choice you made previously."  
         ""
         "To understand the consequences of selecting a particular"
         "choice from this menu consult the documentation for ~A."
         ""
         "------------------------------------------------"
         ""
         )
      (function-symbol-name-from-call-snippet call-snippet)
      ))
    (html-for-call-snippet-help call-snippet :component)
    ))

(defun html-for-optional-arg-help (snippet)
  (let* ((call-snippet (snippet-parent snippet))
         (fsname (function-symbol-name-from-call-snippet call-snippet)))
    (hsafe 
     (formatn
      #.(one-string-nl
         "This node, optional to the user, offers you more"
         "functionality for the encompassing function."
         ""
         "For example, FIRST is perfectly functional without"
         "activating its optional node,"
         "so that calling FIRST on (1 2 3 4 5) returns 1."
         "But if you provide 3 as the optional argument to FIRST"
         "it will return the first 3 elements instead of the first element."
         ""
         "You can expand ~A's functionality by selecting"
         "the sole menu option (besides Help) for"
         "this node, and entering an appropriate value."
         ""
         "To understand the consequences of selecting the optional argument"
         "to ~A and to understand what values are legal for you to provide,"
         "consult the documentation for ~A."
         ""
         "------------------------------------------------"
         ""
         )
      fsname fsname fsname
      ))
    (html-for-call-snippet-help call-snippet :component)
    ))

;;; Keys and flags snippet

(defmethod html-for-snippet-help ((snippet keys-and-flags-snippet))
  (let ((call-snippet (snippet-parent snippet)))
    (hsafe 
     (formatn
      (if *multiselect-enable*
          #.(one-string-nl
             "This node allows you to select as many keys or optional"
             "arguments as you wish.  They are specific to each"
             "function, offering additional controls and functionality."
             ""
             "You can select as many options as you want at any time"
             "by clicking on them...your selected options are"
             "highlighted in green. Once all your desired options"
             "are highlighted, click the APPLY button at either the"
             "top or bottom of the menu. To unselect an option,"
             "click it again, and it will become unhighlighted."
             ""
             "There are no restrictions on how many or which keys and flags"
             "you can select, but it is up to you to avoid"
             "contradictions...for example, don't try to operate"
             "on a sequence FROM its 8th element TO its 4th element."
             ""
             "To delete a key or flag, click the 'Delete' (X) icon in"
             "the upper right of each box, or to remove them all,"
             "use the 'Clear' menu option. You can also minimize,"
             "or collapse, all your selected options"
             "to save space by selecting 'Collapse' from the options menu."
             "To see the options again, select 'Expand' from the"
             "options menu."
             ""
             "To understand the consequences of selecting a particular option"
             "from this menu consult the documentation for ~A."
             ""
             "------------------------------------------------"
             ""
             )
        #.(one-string-nl
           "This node allows you to select as many keys or optional"
           "arguments as you wish.  They are specific to each"
           "function, offering additional controls and functionality."
           "Hover your mouse over the green box and click one to add it."
           ""
           "There are no restrictions on how many or which keys and flags"
           "you can select (but you can't select one more than once)."
           "It is up to you to avoid contradictions...for example,"
           "don't try to operate on a sequence FROM its 8th element TO"
           "its 4th element."
           ""
           "To delete a key or flag, click the 'Delete' (X) icon in"
           "the upper right of each box, or to remove them all,"
           "use the 'Clear' menu option."
           "You can also minimize, or collapse, all your selected options"
           "to save space by selecting 'Collapse' from the optins menu."
           "To see the options again, select 'Expand' from the"
           "options menu."
           ""
           "To understand the consequences of selecting a particular option"
           "from this menu consult the documentation for ~A."
           ""
           "------------------------------------------------"
           ""
           ))
      (function-symbol-name-from-call-snippet call-snippet)
      ))
    (html-for-call-snippet-help call-snippet :component)
    ))
      

;;; Toplevel output

(defmethod html-for-snippet-help ((snippet toplevel-output-snippet))
  (hsafe 
   #.(one-string-nl
      "This node displays the result of executing something in the workspace."
      "The resulting value is shown after a prompt, '>', with an index value"
      "preceding the prompt."
      ""
      "You can copy this result and use it in the workspace, and you can"
      "delete this node.  If the execution that created this node also"
      "resulted in some printout, the prompt is a different color, and you"
      "can see the printout by selecting the 'Show printout' menu option."
      ""
      "You can refer to this result in subsequent workspace operations"
      "by using the RESULT function which takes as its argument the index"
      "value preceding the prompt of the result you wish to access."
      )))

;;; Output value

(defmethod html-for-snippet-help ((snippet output-value-snippet))
  (hsafe 
   #.(one-string-nl
      "This node displays one of the values resulting from the execution"
      "of something in the workspace.  You will only encounter a node of"
      "this type if something you execute returns multiple values (not"
      "typical of BBL functions)."
      ""
      "You can copy this result and use it in the workspace."
      "You can refer to this result in subsequent workspace operations"
      "by using the RESULT function which takes as its arguments the index"
      "value preceding the prompt of the result you wish to access, and"
      "the index of this node (e.g., to reference the second value of the"
      "result indexed by the prompt '6>' use (RESULT 6 2))."
      ""
      "If the execution that created this node also"
      "resulted in some printout, the prompt of the outer box is a"
      "different color, and you can see the printout by selecting"
      "the 'Show printout' menu option of the outer box."
      )))

;;; Call

(defmethod html-for-snippet-help ((snippet call-snippet))
  (html-for-call-snippet-help snippet :toplevel))

(defun real-call-snippet-function-name (snippet)
  (let ((symbol-name (function-symbol-name-from-call-snippet snippet)))
    (cond 
     ((string= "{" symbol-name) 'bbl::{})
     ((string= "" symbol-name) 'bbl::[])
     (t (intern symbol-name *package*))
     )))

(defun html-for-call-snippet-help (snippet mode)
  (let ((symbol (real-call-snippet-function-name snippet)))
    (html-for-symbol-help symbol mode)
    ))

(defun html-for-symbol-help (symbol mode)
  (let* ((symbol-name (symbol-name symbol))
         (alias-of (get symbol :alias-of))
         (target (or alias-of symbol))
         (prefix (if alias-of (formatn "Alias of ~S. " alias-of) "")))
    (vdbg "Help requested for ~S, package ~S~%" symbol (symbol-package symbol))
    (let ((docurl (help::symbol->doc-url symbol))
          (docobj (help::find-documentation 
                   target 'help::function-documentation)))
      (print docurl)
      (print docobj)
      (cond 
       (docobj 
        (let ((docstring (help::docstring docobj)))
          (print docstring)
          (setq 
           docstring
           (if docstring 
               (formatn "~S: ~A" symbol docstring)
             (formatn "~S: No summary info." symbol)
             ))
          (print docstring)
          (ecase mode
            ;; go directly to biobike documentation page
            (:toplevel
             (throw :call-snippet-transfer (help::docobj->url docobj))
             )
            (:component 
             (html
              "Full documentation for "
              ((:a :href (help::docobj->url docobj)) 
               (:princ-safe (string symbol))
               )
              )))))
       (docurl 
        (let ((docstring (documentation symbol 'function)))
          ;; go directly to hyperspec
          (when (and (eq mode :toplevel) (null docstring))
            (throw 
             :call-snippet-transfer 
             (maybe-hack-lisp-doc-url docurl symbol)))
          (html 
           (:princ-safe 
            (formatn "No Biobike documentation page for ~S." symbol-name))
           ;; see if function has a normal documentation string and if so
           ;; display it
           (when docstring 
             (html 
              :br :br
              (:princ-safe (s+ prefix (formatn "~S: ~A" symbol docstring)))
              ))
           :br :br
           )
          ;; link to original Biobike documentation display mechanism
          (let ((hacked-url (maybe-hack-lisp-doc-url docurl symbol)))
            (html
             ((:a :href hacked-url) "Other documentation")
             ))))
       (t 
        ;; If the VPL finds itself in lisp mode, symbols which are 
        ;; in the BBL package will not be found in the user's package
        ;; and so the help for those symbols won't be found.  
        ;; So see if the symbol the user is asking for help for
        ;; can be found in the bbl package and if so use that symbol.  
        (vif (bbl-symbol (find-symbol (string symbol) :bbl))
             (html-for-symbol-help bbl-symbol mode)
             (html 
              (:princ-safe 
               (s+ prefix (formatn "~S: No known documentation." symbol))
               ))))))))


;; This code only works with respect to the Lispworks hyperspec
;; and could be rendered obsolete at any point.  
(defun maybe-hack-lisp-doc-url (docurl symbol)
  (flet ((hack-url (original new)
           (let ((pos (search original docurl)))
             (if pos 
                 (replace (copy-seq docurl) new :start1 pos)
               docurl
               ))))
    (case symbol
      ((and or) (hack-url "a_" "m_"))
      ((not mod) (hack-url "a_" "f_"))
      ((+ - * /) (hack-url "a_" "f_"))
      ((list) 
       (let ((pos (search "a_list" docurl)))
         (if pos 
             (s+ (subseq docurl 0 pos) "f_list_" (subseq docurl (+ pos 6)))
           docurl)))
      (otherwise docurl)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod html-for-htype-help ((htype t) snippet) 
  (declare (ignore snippet))
  (hsafe (format nil "No help for help type ~A yet!" htype)))

(defmethod html-for-htype-help ((htype (eql :loop-initializations)) snippet) 
  (declare (ignore snippet))
  (hsafe 
   #.(one-string-nl
      "Use this clause to define and initialize variables you will use"
      "in your loop body but which do not change from iteration to iteration."
      "Use the 'add another' or 'add two more' option to create initialization"
      "nodes which you will then fill in."
      )))

(defmethod html-for-htype-help ((htype (eql :loop-iterator)) snippet) 
  (declare (ignore snippet))
  (hsafe 
   #.(one-string-nl
      "This node controls how many times the loop executes and when"
      "it terminates."
      ""
      "Move your mouse over the green box with the white arrow"
      "labeled \"Controls\" to see the options available."
      "You can select as many of them as you like."
      ""
      "You can delete a control by clicking the 'Clear' icon on the"
      "upper right."
      ""
      "The options are:"
      ""
      " -- 'FOR variable IN list' and 'For (var1 var2...) IN list'"
      "allow you to step across each element of a list."
      ""
      " -- 'AS variable = list-part' allows you to set a variable"
      "to be a list, then each time through the loop, the variable"
      "is set to the remainder of the list after the first element is"
      "removed."
      " -- 'AS variable = value' allows you to set a variable"
      "to a value, then optionally change it to something else the"
      "next time through the loop (using the THEN clause)."
      ""
      " -- 'FOR (var1 var2...) = value-list' allows you to set multiple"
      "variables to the constituents of a list, then optionally change"
      "the value of all the variables to the constituents of a different"
      "list the next time through the loop (using the THEN clause)."
      ""
      " -- 'FOR variable FROM n1 TO n2' sets the variable to be the"
      "the value of n1 and increments it each time through the loop"
      "until its value exceeds n2."
      ""
      " -- 'FOR variable FROM n1 BELOW n2' sets the variable to be the"
      "the value of n1 and increments it each time through the loop"
      "until its value equals n2."
      ""
      " -- 'FOR variable FROM n1 (without limit)' sets the variable"
      "to be the value of n1 and increments it each time through the loop."
      "Unless there is some other terminating condition for the loop"
      "the value will keep getting incremented forever."
      ""
      " -- 'FOR variable FROM n1 DOWNTO n2' sets the variable to be the"
      "the value of n1 and decrements it each time through the loop"
      "until its value equals n2."
      ""
      " -- 'WHILE (condition)' and 'UNTIL (condition)' allow the loop"
      "to continue as long as a certain condition is true or false,"
      "respectively."
      ""
      "Note: The 'FOR variable FROM ... ' nodes also allow"
      "an optional BY clause, which allows the variable to be"
      "incremented or decremented by the value provided instead of"
      "by 1."
      )))

(defmethod html-for-htype-help ((htype (eql :loop-condition)) snippet) 
  (declare (ignore snippet))
  (hsafe
   #.(one-string-nl
      "If you don't want your loop body code to be run on each iteration,"
      "you can select the WHEN or UNLESS option."
      "If the WHEN condition is true, the body code is run,"
      "otherwise not."
      "If the UNLESS condition is true, the body code is not run,"
      "otherwise it is."  
      "To clear the condition, click the 'Clear' icon in the upper"
      "right of the 'form' box, or, to deactivate the Cond node"
      "entierly, click the 'Clear' icon in the upper right of the"
      "box labeled 'When' or 'Unless', or of the immediately"
      "surrounding 'Cond' box.  See the help in the 'action' box"
      "for more.")))

(defmethod html-for-htype-help ((htype (eql :loop-action)) snippet) 
  (declare (ignore snippet))
  (hsafe 
   #.(one-string-nl
      "Use this clause to create statements which will be executed each time"
      "the loop iterates.  When the loop finishes, the loop DOES NOT return"
      "the value of the last statement in this clause.  You must use"
      "the 'Results' clause or a RETURN statement if you want your loop"
      "to return anything other than NIL."
      ""
      "To use this clause select the 'do' option from the menu and insert"
      "a statement into the 'form' box.  If you want multiple statements you"
      "can create more by using the 'add another' selection from the"
      "options menu."
      )))

(defmethod html-for-htype-help ((htype (eql :loop-aggregate)) snippet) 
  (declare (ignore snippet))
  (hsafe 
   #.(one-string-nl
      "Use this clause to specify what happens each time the loop"
      "executes and to return a value (or a list of values) when the loop"
      "finishes."
      ""
      "To use this clause, select a verb (e.g., append, max) from the"
      "menu which will then tell the loop what to do with a value you will"
      "also specify (the value will usually be either one of your loop"
      "variables or some expression computed from one or more of your"
      "loop variables)."
      ""
      "At the end of the loop, the result of the operation as performed"
      "on all the values in sequence is returned."
      )))

(defmethod html-for-htype-help ((htype (eql :loop-finally)) snippet) 
  (declare (ignore snippet))
  (hsafe
   #.(one-string-nl
      "Once the actual iteration of the loop finishes,"
      "you may want to execute more code.  You can do this"
      "by activating the 'finally' node and entering whatever you want"
      "into the resulting box."
      )))

(defmethod html-for-htype-help ((htype (eql :for-each-primary-iter)) snippet) 
  (declare (ignore snippet))
  (hsafe
   #.(one-string-nl
      "This node is the primary iteration control for FOR-EACH."
      ""
      "The options, one of which you must select, are:"
      ""
      " -- 'variable IN list' and '(var1 var2...) IN list'"
      "allow you to step across each element of a list."
      ""
      " -- 'FOR (var1 var2...) = value-list' allows you to set multiple"
      "variables to the constituents of a list, then optionally change"
      "the value of all the variables to the constituents of a different"
      "list the next time through the loop (using the THEN clause)."
      ""
      " -- 'number FROM n1 TO n2' sets the variable to be the"
      "the value of n1 and increments it each time through the loop"
      "until its value exceeds n2."
      ""
      " -- 'number FROM n1 BELOW n2' sets the variable to be the"
      "the value of n1 and increments it each time through the loop"
      "until its value equals n2."
      ""
      " -- 'number FROM n1 DOWNTO n2' sets the variable to be the"
      "the value of n1 and decrements it each time through the loop"
      "until its value equals n2."
      ""
      " -- 'number FROM n1 (without limit)' sets the variable to be"
      "the value of n1 and increments it each time through the loop."
      "Unless there is some other terminating condition for the loop"
      "the value will keep getting incremented forever."
      ""
      "Note: The 'number FROM ... ' nodes also allow"
      "an optional BY clause, which allows the variable to be"
      "incremented or decremented by the value provided instead of"
      "by 1."
      ""
      "You can delete the control you selected by clicking the 'Clear' icon"
      "on the upper right of the resulting box."
      ""
      )))

(defmethod html-for-htype-help ((htype (eql :loop-init-clause)) snippet) 
  (declare (ignore snippet))
  (hsafe 
   #.(one-string-nl
      "This clause allows you to initialize a variable with a value before"
      "the loop starts to execute, and this variable will remain with the value"
      "you give it until the loop ends (unless you explicitly change it)."
      ""
      "You can use this variable within the subsequent clauses you will be"
      "creating for the loop.   When you initialize this variable, you can"
      "refer to the variables you have initialized in previous initialization"
      "clauses, e.g."
      ""
      " ... init x = 3 init y = (+ x 1) ... "
      )))

(defmethod html-for-htype-help 
           ((htype (eql :additional-loop-controls)) snippet) 
  (declare (ignore snippet))
  (hsafe 
   #.(one-string-nl
      "You can define other loop variables and/or set conditions"
      "on when and whether the loop will terminate here."
      "Use the various 'FOR' options to define additional loop variables."
      "Use the 'WHILE' or 'UNTIL' options to control loop termination."
      )))

(defmethod html-for-htype-help ((htype (eql :loop-variable-update)) snippet) 
  (declare (ignore snippet))
  (hsafe 
   #.(one-string-nl
      "Use the 'AS variable = value' option to define loop variables which"
      "are initialized the first time through the loop, and then set to"
      "a potentially different value each time through the loop."
      "Without the optional THEN clause, the variable is set to the"
      "value of VALUE each time through the loop (VALUE may be an expression,"
      "so what it evaluates to may change as the loop progresses)."
      "With the optional THEN clause, the first time through the loop the"
      "variable is set to VALUE; subsequent iterations set the value to"
      "the THEN expression."
      ""
      "Use the 'AS variable = list-part' option to cause a loop variable"
      "to take on values which are successive sublists of a list."
      "For example,"
      "For x on '(1 2 3) ..."
      "will cause x to first have the value (1 2 3), then (1 2), and finally"
      "(1)."
      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod html-for-htype-help ((htype (eql :df-summary)) snippet) 
  (declare (ignore snippet))
  (hsafe
   #.(one-string-nl
      "This clause allows you to provide a brief description of what your"
      "new function is intended to do.  The description is not part of the"
      "function itself; it is not executed."
      )))

(defmethod html-for-htype-help ((htype (eql :df-required)) snippet) 
  (declare (ignore snippet))
  (hsafe
   #.(one-string-nl
      "Put the names of the arguments to your new function here."
      "If you have more than one argument, use the 'add another' option"
      "from the Options menu to create more 'arg' boxes."
      ""
      "Note: In the VPL version of DEFINE-FUNCTION, there is no way to"
      "specify token arguments."
      )))

(defmethod html-for-htype-help ((htype (eql :df-body)) snippet) 
  (declare (ignore snippet))
  (hsafe
   #.(one-string-nl
      "The executable statements that compose your new function go here."
      "If you have more than one executable statement, use the 'add another'"
      "option from the Options menu to create 'form' boxes.  Enter function"
      "calls and data into the form boxes just as you would anywhere else."
      )))

(defmethod html-for-htype-help ((htype (eql :df-flags)) snippet) 
  (declare (ignore snippet))
  (hsafe
   #.(one-string-nl
      "Additional arguments to your new function (known as 'flags arguments')"
      "go here.  When your function executes, your flag arguments will"
      "have either the value 'T' or the value 'Nil' depending on whether"
      "the caller explicitly provided the flag argument in the call."
      )))

(defmethod html-for-htype-help ((htype (eql :df-keys)) snippet) 
  (declare (ignore snippet))
  (hsafe
   #.(one-string-nl
      "Additional arguments to your new function (known as 'keyword arguments')"
      "go here.  Keywords consist of a name and a default value; you must"
      "fill both boxes for each keyword.  When your function executes, your"
      "keyword arguments will have the default value unless the caller"
      "explicitly provides the key name and a different value in the call."
      )))

(defmethod html-for-htype-help ((htype (eql :df-types)) snippet) 
  (declare (ignore snippet))
  (hsafe
   #.(one-string-nl
      "This clause allows you to tell the system that one or more of the"
      "arguments to your new function are restricted in the values that they"
      "allowed to hold.  To declare this you enter the name of your variable"
      "and a type that describes the restriction (e.g., integer, string)."
      "If your function is called with a value for an argument specified"
      "with a type and that value is not valid, the system will error."
      )))

(defmethod html-for-htype-help ((htype (eql :df-conversions)) snippet) 
  (declare (ignore snippet))
  (hsafe
   #.(one-string-nl
      "This clause allows you to tell the system to automatically convert"
      "the value of an argument from one type to another, before"
      "the body of your new function executes.  You must specify the name"
      "of the argument you want automatically converted, the type it is"
      "to be converted from, and the type it is to be converted to."
      "Only certain conversions are legal; consult the BBL manual for details."
      )))

(defmethod html-for-htype-help ((htype (eql :df-map)) snippet) 
  (declare (ignore snippet))
  (hsafe
   #.(one-string-nl
      "This clause allows you to tell the system to"
      "apply the function you are defining to the elements of the required"
      "variable you specify.  If you provide a non-list as the value"
      "of the required argument you specify, the function will work normally."
      "But if you provide a list as a value of the required argument,"
      "the function will be applied to each element of that list, and"
      "a list of results will be returned."
      ""
      "The MAP option works as explained above."
      "The MAPCARNN option works similarly, but any NIL result will be removed"
      "from the list of returned results."
      "The MAPTREE option recursively descends list structure, calling"
      "the function for each leaf of the list you provide, whereas"
      "the MAP option only calls the function for each toplevel element"
      "of the list you provide."
      )))


(defmethod html-for-htype-help ((htype (eql :df-initializations)) snippet) 
  (declare (ignore snippet))
  (hsafe
   #.(one-string-nl
      "This clause allows you to specify variables and their initial"
      "values for subsequent use in the body of your define-function"
      "definition.  You can initialize individual variables to individual"
      "values, or a set of variables all to the same value."
      )))

