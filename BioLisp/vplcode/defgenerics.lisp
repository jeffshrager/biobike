;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)

;;; +=========================================================================+
;;; | Copyright (c) 2006 JP Massar, John Myers                                |
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

;; Author: JP Massar.

(defgeneric copy-snippet-method (snippet) 
  (:documentation 
   #.(one-string-nl
      "Copies a snippet to the clipboard (*current-clipboard-box*)."
      "If the snippet cannot be copied the user is beeped."
      "If the snippet being copied is an output value, a constant node"
      "is created using the value, and that new node is put in the clipboard."
      )))

(defgeneric paste-snippet-into-hole (snippet-to-paste hole-snippet-to-replace)
  (:documentation 
   #.(one-string-nl
      "Takes a snippet on the clipboard and substitutes it for a hole."
      "Special case when snippet being pasted is a constant snippet and its"
      "value is a symbol or a list.  See method for constant-snippet."
      )))
      
(defgeneric process-single-vpl-return-value (value)
  (:documentation 
   #.(one-string-nl
      "Additional handling for any returned type to be treated specially"
      "by VPL.  Normally does nothing.  Executed strictly for side effect."
      )))

(defgeneric handle-insert-type (insert-type snippet user-input)
  (:documentation 
   #.(one-string-nl
      "Handles insertion of a value into a hole-snippet which has an"
      ":insert-type property.  By default, USER-INPUT is interpreted as"
      "a lisp constant or symbol, a constant or symbol node is created,"
      "and the hole snippet is replaced by this node.  Special methods"
      "exist to treat USER-INPUT as the name of a protein or a gene."
      )))

(defgeneric html-for-snippet-help (snippet)
  (:documentation
   #.(one-string-nl
      "Creates and outputs via the HTML macro a help string explaining"
      "the purpose of a given node on the display, and the actions"
      "the user can take to affect this node."
      )))

(defgeneric html-for-htype-help (htype snippet)
  (:documentation
   #.(one-string-nl
      "A snippet instantiated from a template can have an :htype property,"
      "which labels it as special with respect to HELP.  If it does, this"
      "function is called instead of HTML-FOR-SNIPPET-HELP to output via"
      "the HTML macro a help description."
      )))

(defgeneric another-subform-of-snippet (snippet)
  (:documentation
   #.(one-string-nl
      "Adds another child to an aggregate snippet's children.  This"
      "probably shouldn't be a generic function because it is only"
      "applicable to aggregate snippets.  The default method generates"
      "an internal error."
      )))

(defgeneric describe-vpl-output (obj) 
  (:documentation 
   #.(one-string-nl
      "Methods to handle the describe option for special types of return"
      "values.  If a type of return value is handled by one of the methods,"
      "the method must return T.  This function is only called if a form"
      "returns a single value, not multiple values.  If it is called and it"
      "returns T, the description is assumed to have been done, and no"
      "further action to describe the value is done."
      )))

(defgeneric describe-output-node (snippet &optional index)
  (:documentation 
   #.(one-string-nl
      "Handles the DESCRIBE option for a toplevel output node or an output"
      "value node.  Generates an internal error if it is called with any"
      "other snippet type.  The INDEX specifies which value of a"
      "toplevel-output-node should be described (if NIL, all returned values)."
      )))

(defgeneric find-or-create-wrapee-hole (snippet)
  (:documentation 
   #.(one-string-nl
      "Attempts to find a hole suitable to be replaced by a wrapee snippet."
      "Only called if there is no hole found in the immediate children of"
      "the wrap snippet created from the template the user selected."
      "Currently it is only called on the first child argument."
      "This is used for functions which have an &rest argument as the"
      "initial child."
      )))

;;; initialize-instance (snippet)

#||

There is an :after initialize-instance method for the SNIPPET class.
It assigns the new snippet an ID, stores it in the hash table of snippets,
and sets the snippet's parent and property list.

||#


(defgeneric xcopy-snippet (original-snippet snippet-copy)
  (:documentation
   #.(one-string-nl
      "Provides lowlevel copy functionality for different snippet types."
      "Called within PCOPY-SNIPPET."
      )))

(defgeneric find-original-template (parent snippet)
  (:documentation
   #.(one-string-nl
      "Returns the template or subtemplate that was used to create SNIPPET"
      "or the hole that SNIPPET replaced in PARENT."
      )))

;;; print-object (snippet)

#||

Simply pretty prints a snippet without showing its children.
The function TREEPRINT-SNIPPET prints out a more elaborate indented 
display of the snippet and its descendants.

||#

(defgeneric snippet-label (snippet)
  (:documentation
   #.(one-string-nl
      "Returns a string used to represent SNIPPET on the VPL display."
      "Some snippet types have a LABEL slot; others have a specific method."
      )))

(defgeneric snippet-menu-title (snippet)
  (:documentation
   #.(one-string-nl
      "Returns a string used as a label for the options icon."
      "If a node is not visualized as an options icon this should return"
      "the null string (all menus must be given a title even if null)."
      "Used as part of the construction of a menu for a snippet."
      )))

(defgeneric node-required-menu-items (snippet)
  (:documentation
   #.(one-string-nl
      "Returns all the required menu items for a snippet."
      )))

(defgeneric node-optional-menu-items (snippet)
  (:documentation
   #.(one-string-nl
      "Returns a set of menu items for a snippet which may or may not"
      "be appropriate for that snippet given its context."
      )))

(defgeneric unwrap-needed? (snippet)
  (:documentation
   #.(one-string-nl
      "Returns whether SNIPPET should have an UNWRAP menu option."
      "Currently only certain call snippets can have this option."
      )))

(defgeneric delete-needed? (snippet)
  (:documentation
   #.(one-string-nl
      "Returns whether SNIPPET should have either a box menu DELETE icon"
      "or a DELETE menu entry."
      )))

(defgeneric clear-needed? (snippet)
  (:documentation
   #.(one-string-nl
      "Returns whether SNIPPET should have either a box menu CLEAR icon"
      "or a CLEAR menu entry."
      )))

(defgeneric node-set-menu-items (snippet)
  (:documentation
   #.(one-string-nl
      "Returns a set of menu items specific to a particular snippet."
      "For example, returns the currently available choices for a"
      "choice node."
      )))

(defgeneric snippet-to-code-method (snippet)
  (:documentation
   #.(one-string-nl
      "Returns two values: the code that SNIPPET represents, and"
      "whether or not the code should be spliced into higher level code"
      )))

(defgeneric provide-background-colors (snippet)
  (:documentation
   #.(one-string-nl
      "Stores a value of either :elhai-yellow or :elhai-orange in the"
      ":background-color property of a snippet and all its descendants."
      "The color is toggled as the structure descends through function calls."
      )))

(defgeneric snippet->box-method (snippet)
  (:documentation
   #.(one-string-nl
      "Takes a snippet that is to be visualized as a box with a main menu"
      "and turns it into a client representation of that box."
      )))

(defgeneric text-flags (snippet)
  (:documentation
   #.(one-string-nl
      "Returns a list of text attributes for the primary text of a snippet"
      "which is to be at least partially visualized with text."
      )))

(defgeneric box-flags (snippet)
  (:documentation
   #.(one-string-nl
      "Returns a list of attributes modifying the appearance of a snippet"
      "on the client."
      )))

(defgeneric htype-box-flags (htype)
  (:documentation
   #.(one-string-nl
      "Returns a list of attributes modifying the appearance of a snippet"
      "which is tagged as being of type HTYPE."
      )))

(defgeneric snippet->text-info (snippet)
  (:documentation
   #.(one-string-nl
      "Returns a string representation of a snippet for display."
      "Only literal snippets currently have such a representation."
      )))

(defgeneric snippet-needs-clear-icon? (snippet)
  (:documentation
   #.(one-string-nl
      "Should this snippet be shown with a clear icon in the upper right?"
      )))

(defgeneric needs-delete-icon? (snippet parent)
  (:documentation
   #.(one-string-nl
      "Should this snippet be shown with a delete icon in the upper right?"
      "To determine this the parent may also come into play."
      )))

(defgeneric new-clear-snippet (snippet)
  (:documentation
   #.(one-string-nl
      "Implements clear semantics for snippets of particular types."
      )))

(defgeneric new-delete-snippet (snippet parent)
  (:documentation
   #.(one-string-nl
      "Implements delete semantics for snippets of particular types that"
      "have as their parent a snippet of some particular type."
      )))

(defgeneric clipboard-status-message (snippet verb)
  (:documentation
   #.(one-string-nl
      "Creates and sends a client message to the mode line when a snippet"
      "of a particular type is copied or cut."
      )))

(defgeneric snippet-visualization-type (snippet)
  (:documentation
   #.(one-string-nl
      "Determines which snippets are rendered as boxes and which are rendered"
      "as arrow icons with additional goo."
      )))

(defgeneric wrap-consistency-check (wrapee wrapper template)
  (:documentation
   #.(one-string-nl
      "Verifies that a snippet can be wrapped by a particular wrapping snippet."
      )))

(defgeneric unwrap-parent-preserving-child (parent child)
  (:documentation
   #.(one-string-nl
      "Implements the unwrap (unsurround) algorithm which removes a snippet"
      "and replaces it in the workspace structures by one of its children."
      )))

(defgeneric vpl-error-to-string (error)
  (:documentation 
   #.(one-string-nl
      "Converts an error into a display string suitable for the VPL."
      )))

   
