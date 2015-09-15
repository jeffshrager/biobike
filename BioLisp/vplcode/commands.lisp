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

;; Author: JP Massar, John Myers.  

;;;;;;;;;;;;;;;;;;;; HANDLE ROUTINES (Client Commands) ;;;;;;;;;;;;


(defun handle-browser-page-closed ()
  ;; Save current session here.
  (let ((savefile (s+ "_System-save-" (make-timestamp-string :mode :stdfull))))
  (usyslog "Browser Page has closed. User session saved to ~A~%" savefile)
  (format t "Browser Page has closed. User session saved to ~A~%" savefile)
  (save-named-user-session savefile)
  ))

(defun handle-click-results ()
  (when *current-selected-boxid*
    (unflashing-hilight-box *current-selected-boxid*)
    (setq *current-selected-boxid* nil)
    ))

(defun handle-click-workspace ()
  (when *current-selected-boxid*
    (unflashing-hilight-box *current-selected-boxid*)
    (setq *current-selected-boxid* nil)
    ))

(defun handle-clear-execution-history ()
  (ulogdbg "Clearing execution history...~%")
  (let* ((toplevel-results-snippet (results-root-node))
         (execution-history (snippet-children toplevel-results-snippet)))
    (case (length execution-history)
      (0 (clear-results))
      (1 
       (clear-results)
       (setq execution-history nil))
      (otherwise
       (let ((remaining-node (first execution-history)))
         (setq execution-history (list remaining-node))
         (clear-results)
         (add-results (toplevel-snippet->client-representation remaining-node))
         (show-status "Clear again to remove last result.")
         )))
    (clear-vpl-output-history-variables)
    (setf (snippet-children toplevel-results-snippet) execution-history)
    ))

(defun clear-vpl-output-history-variables ()
  (setq *** nil) (setq ** nil) (setq * nil)
  (setq /// nil) (setq // nil) (setq / nil))

(defun handle-clear-workspace ()
  (ulogdbg "Clearing all snippets and workspace history.~%")
  (setf (snippet-children (workspace-root-node)) nil)
  (clear-workspace)
  (clear-status)
  )

(defun handle-vpl-version (version)
  ;; FIXME: implement
  ;; version is a string representing the client version.  The version should
  ;; be treated as an opaque string.  It will contain numbers and possibly other
  ;; characters.
  (declare (ignore version))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; HANDLE DND EVENTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-dnd-box (source-id target-id)
  ;; source-id is the box-id of the dragged box. 
  ;; target-id is the box-id of the box where source-id was dragged.
  (let ((sid (string->int source-id))
        (tid (string->int target-id)))
    (drag-and-drop sid tid nil)
    ))

(defun handle-dnd-box-after (source-id target-id)
  ;; source-id is the box-id of the dragged box. 
  ;; target-id is the box-id of the box after which source-id was dragged.
  (let ((sid (string->int source-id))
        (tid (string->int target-id)))
    (drop-between sid tid :after)
    ))

(defun handle-dnd-box-before (source-id target-id)
  ;; source-id is the box-id of the dragged box. 
  ;; target-id is the box-id of the box before which source-id was dragged.
  (let ((sid (string->int source-id))
        (tid (string->int target-id)))
    (drop-between sid tid :before)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Server currently doesn't care about mouse overs or mouse outs.

(defun handle-box-mouse-over (id)
  (declare (ignore id))
  nil)

(defun handle-box-mouse-out (id)
  (declare (ignore id))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;; ROUTINES TO HANDLE MOUSE CLICKS  ;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant left-click 1)
  (defconstant middle-click 2)
  (defconstant right-click 4)
  )

(defun decode-mouse-single-click (mouse-click-string-code)
  (let ((code (string->int mouse-click-string-code)))
    (case code
      ((#.left-click #.right-click) code)
      (0 
       (vpl-internal-error 
        "Undecodable mouse-click code string: ~S" mouse-click-string-code
        ))
      (otherwise
       (vpl-internal-error "Unknown mouse-click code: ~S" code)
       ))))

(defun handle-box-mouse-click (boxid-string mouse-code &aux boxid)
  #.(one-string-nl
     "This function handles boxes being clicked once."
     "mouse-code='1': left; '3': right.")
  (vdbg "In handle-box-mouse-click...~%")
  (not-modified!)
  (setq boxid (idstring->id boxid-string :snippet-id))
  (vdbg "box ID = ~D~%" boxid)
  (let ((mouse-code (decode-mouse-single-click mouse-code)))
    (vdbg
     "Got ~A mouse click for boxid ~D~%"
     (case mouse-code (1 "left") (3 "right") (otherwise "unknown"))
     boxid)
    (case mouse-code
      (#.left-click (do-box-selected boxid))
      ;; the vpl doesn't associate any semantics with right clicks currently
      (#.right-click nil)
      )))

;; This is to be new functionality.  Currently the client intercepts
;; single clicks on holes and doesn't send the message to the server.  
;; The client now takes it upon itself to turn the hole into an input
;; box, but in VPL 2.0, the server will take on that responsibility.  
(defun handle-hole-mouse-click (hole-IDnum)
  "Incoming. This function handles holes being clicked once."
  (declare (ignore hole-IDnum))
  )

;; the client doesn't currently pass us a double right click (assuming
;; such a thing is even allowed), so we can just ignore the mouse code and
;; assume it is a double left click
(defun handle-box-mouse-doubleclick (boxid mouse-code)
  (declare (ignore mouse-code))
  (vdbg "Handling double click...~%")
  (setq boxid (idstring->id boxid :snippet-id))
  (block exit
    ;; special hack to catch double-clicks on constant and symbol nodes.
    ;; The problem is that the single-click gets sent immediately, 
    ;; causing the constant or symbol node to be replaced by a
    ;; hole, then the double-click gets sent on the now-pruned 
    ;; constant or symbol node, which was causing a vpl internal error.  
    ;; Now it will just be a noop.
    ;; It is also possible that between the time the node
    ;; gets pruned and the double-click is processed that 
    ;; the node will be garbage-collected; hence the test for null.
    (let ((snippet (gethash boxid (uvs-hash *vpl-state*))))
      (when (or (null snippet)
                (and (null (snippet-parent snippet))
                     (or (eq (type-of snippet) 'constant-snippet)
                         (eq (type-of snippet) 'symbol-snippet))))
        (show-status "Double click ignored.")
        (return-from exit nil)
        ))
    (when (find-snippet-in-results-area boxid nil)
      (return-from exit nil))
    (when (find-snippet-in-workspace boxid nil)
      (handle-eval-request boxid))
    ;; ignore any double click not inside a box.  
    ;; currently these don't even come through to the server.
    nil
    ))

;;;;;;;;;;;;;;;;;;;;;;;; ROUTINES TO HANDLE MENU SELECTIONS ;;;;;;;;;;;;;;;

;; When the user selects a menu option, either
;; from an OPTIONS box or from the main menu associated with a box,
;; the client sends this message to the server.  The message information
;; is the box ID of the box the menu is associated with, and the operation
;; (encoded as an opcode) the user is requesting.  

(defun handle-main-menu-select (opcode boxid) 
  (vdbg "Main menu select: opcode ~D, boxid ~D~%" opcode boxid)
  (execute-opcode boxid opcode))

(defun handle-options-menu-select (opcode boxid)
  (vdbg "Options menu select: opcode ~D, boxid ~D~%" opcode boxid)
  (execute-opcode boxid opcode))

(defun handle-boxmenu-mouse-click (opcode boxid)
  (vdbg "Got mouseclick from menu for box ~S, opcode ~S~%" boxid opcode)
  (execute-opcode boxid opcode))

(defun handle-boxmenu-multiselect (boxid indices)
  (vdbg "Got multiselect from menu for box ~S, indices ~S~%" boxid indices)
  (choose-a-set-of-flags-and-keys boxid indices))

(defun execute-opcode (boxid opcode)
  (setq boxid (idstring->id boxid :snippet-id))
  (setq opcode (idstring->id opcode :operator-id))
  (vdbg "Received opcode ~D, operating on snippet ID ~D~%" opcode boxid)
  (let ((operator (opcode->operator opcode)))
    (vdbg "Calling ~S on snippet ID ~D~%" operator boxid)
    (funcall operator boxid)
    ))

(defun handle-palettemenu-mouse-click (id)
  ;; (sleep 5)
  (vdbg "Got palette mouseclick! id=~S~%" id)
  (setq id (idstring->id id nil))
  (block exit 
    (let ((id-type (unique-id-type id)))
      (vdbg "ID is of type ~S" id-type)
      (case id-type 
        (:template-id 
         (let ((symbol (template-id->symbol id t)))
           (vdbg "Calling template-insertion...~%")
           (handle-template-insertion symbol id)
           (augment-favorites-info symbol)
           (return-from exit nil)
           ))
        (:operator-id 
         (let ((operator (opcode->operator id t)))
           (vdbg "Calling operator ~A~%" operator)
           (handle-palette-command operator id)
           (return-from exit nil)
           ))
        (:data-id
         ;; is it a known piece of data?
         (multiple-value-bind (datum found?)
             (get-datum-from-id id)
           ;; is it a system piece of data?
           (when found? 
             (vdbg "Inserting system data ~S~%" datum)
             (handle-data-insert-command datum)
             (return-from exit nil)
             ))
         (multiple-value-bind (variable found?)
             (get-user-variable-from-id id)
           ;; is it a user's variable?
           (when found? 
             (vdbg "Inserting user variable ~S~%" variable)
             (handle-data-insert-command variable)
             (return-from exit nil)
             ))
         (vpl-internal-error 
          "Data ID ~D from palette but not in system or user data hash!" id
          ))
        (:organism-id 
         (if user::*master-list-enabled*
             (handle-organism-id id)
           (vpl-internal-error "Organism ID not implemented yet!")
           ))
        (:subsystem-id 
         (if user::*master-list-enabled*
             (forward-funcall 'handle-subsystem-id id)
           (vpl-internal-error "Subsystem id exists in non-seed acache!")
           ))
        ((:snippet-id :menu-id)
         (vpl-internal-error 
          "Palette menu mouse click got ID of type ~S.  Should be impossible!"
          id
          ))
        (otherwise 
         (vpl-internal-error 
          (formatn "Totally unknown ID ~D.  This should be impossible!" id)
          ))))))

(defun handle-palette-menu-item-help (id)
  (vdbg "Got palette menu item help request! id=~S~%" id)
  (not-modified!)
  (multiple-value-bind (symbol found?)
      (template-id->symbol id nil)
    (when found? (show-help-for-symbol symbol))
    ))

;; Handles 1) taking template and making it a toplevel snippet,
;; 2) taking a template and inserting the corresponding snippet into
;; a hole, and 3) taking a template and wrapping the corresponding snippet
;; around an existing snippet, depending on whether there is a currently 
;; selected box and whether that box is wrap-enabled 

(defun handle-template-insertion (symbol id)
  (vdbg "Creating snippet from template for ~S~%" symbol)
  (vdbg "CSB: ~S, CWT: ~S~%" *current-selected-boxid* *current-wrap-target*)
  (let ((csb *current-selected-boxid*))
    (cond
     (csb 
      (with-snippet-from-id 
       (snippet location csb)
       (case location
         (:workspace 
          (cond
           (*current-wrap-target* (wrap-template id csb))
           ((snippet-is-hole? snippet) (insert-template id csb))
           (t (create-new-toplevel-ws-snippet (symbol->template-id symbol)))
           ))
         ;; If for some reason a result is highlighted, ignore that fact
         ;; and bring the palette function into the workspace at toplevel
         ;; while unhighlighting the results box.  
         (:results 
          (create-new-toplevel-ws-snippet (symbol->template-id symbol))
          (unflashing-hilight-box (snippet-id snippet)))
         (:neither 
          (vpl-internal-error
           (one-string-nl
            "The snippet id being operated on, ~A,"
            "either does not exist or is no longer in the workspace or"
            "results areas.")
           id           
           )))))
     (t 
      (create-new-toplevel-ws-snippet (symbol->template-id symbol))
      ;; hack to allow system not to redraw everything when just adding 
      ;; a node to the toplevel 
      (setq *function-from-palette-to-workspace-toplevel?* t)
      ))))

(defun handle-palette-command (operator id)
  (declare (ignore id))
  (funcall operator nil))

;; used when the user has selected an item off of the DATA palette menu.
;; The datum is an actual lisp value and is usually a symbol.

(defun handle-data-insert-command (datum)
  (flet ((create-snippet-to-insert (parent) 
           (if (and (symbolp datum) (not (eq t datum)) (not (eq nil datum)))
               (create-subtemplate-snippet 
                parent (copy-list `(:symbol ,datum)))
             (create-subtemplate-snippet 
              parent (copy-list `(:constant ,datum))))))
    (let ((csb *current-selected-boxid*))
      (cond
       (csb 
        (with-snippet-from-id-in-workspace 
            (hole-snippet csb "Cannot insert data into results!")
          (unless (snippet-is-hole? hole-snippet)
            (vpl-user-error 
             "Selected object to insert datum into is not a hole!"
             ))
          (let* ((parent (snippet-parent hole-snippet))
                 (snippet-to-insert (create-snippet-to-insert parent)))
            (insert-new-snippet-into-snippet-hole
             hole-snippet snippet-to-insert)
            (setq *modified-snippet* parent)
            (redraw!)
            )))
       ;; insert datum at toplevel
       (t 
        (let* ((top (workspace-root-node))
               (snippet-to-insert (create-snippet-to-insert top)))
          (add-to-workspace-toplevel snippet-to-insert)
          ;; hack to allow system not to redraw everything when just adding 
          ;; a node to the toplevel 
          (setq *function-from-palette-to-workspace-toplevel?* t)
          ;; (redraw!)
          ))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ROUTINES TO HANDLE KEY PRESSES ;;;;;;;;;;;;;

(defun handle-global-keypress (which-key cntrl-key)
  "Incoming. This function handles keys being pressed on the keyboard."
  (vdbg "Handle Global Keypress of key ~A with cntrl=~A~%" which-key cntrl-key)
  (let ((*command-origin* :keypress))
    (if (equalp (string cntrl-key) "true")
        (progn
          (vdbg "Cntrl Key Hit! Dispatching.~%")
          (case (string->int which-key) 
            ;; Q is the only one that doesn't have an additional meaning.
            (#.(char-code #\e) (test-workspacepipe))
            (#.(char-code #\c) (handle-copy-snippet)) 
            (#.(char-code #\z) (handle-paste-shortcut))
            (#.(char-code #\x) (handle-cut-snippet))
            (#.(char-code #\,) (handle-test-boxes))
            (otherwise  
             (not-modified!)
             (case (string->int which-key)
               (#.(char-code #\v) nil)
               (#.(char-code #\r) nil)
               (otherwise (ulogdbg "Unknown command Cntrl-~A.~%" which-key))
               ))))
      (not-modified!)
      ))) 

(defun handle-test-boxes ()
  (vdbg "Handle test boxes,~%")
  (test-workspacepipe)
  )



;;;;;;;;;;;;;;; CODE FOR CLICKS ON CLEAR/DELETE ICONS ;;;;;;;;;;;;;;;;;;;;

(defun handle-box-delete-icon (boxid-string mouse-code)
  "This function does the right thing when a box delete icon is clicked."
  (vdbg "Box delete...~%")
  (handle-box-click-on-clear-delete :delete boxid-string mouse-code))

(defun handle-box-clear-icon (boxid-string mouse-code)
  "This function does the right thing when a box clear icon is clicked."
  (vdbg "Box clear...~%")
  (handle-box-click-on-clear-delete :clear boxid-string mouse-code))

(defun handle-box-clear-delete-icon (boxid-string mouse-code)
  "This function does the right thing when a box clear-delete-icon is clicked."
  (vdbg "Box clear/delete...~%")
  (handle-box-click-on-clear-delete :clear-delete boxid-string mouse-code))

(defun handle-box-clear-hole-icon (boxid-string mouse-code)
  "Incoming. This function handles holes being cleared."
  (vdbg "Box hole clear...~%")
  (handle-box-click-on-clear-delete :clear boxid-string mouse-code)
  )

(defun handle-box-click-on-clear-delete 
       (type boxid-string mouse-code &aux boxid)
  "This function does the right thing when a box delete icon is clicked."
  (setq boxid (idstring->id boxid-string :snippet-id))
  (case (decode-mouse-single-click mouse-code)
    (#.left-click 
     (let ((f (ecase type 
                (:delete 'handle-delete-snippet)
                (:clear 'handle-clear-snippet)
                (:clear-delete 'handle-clear-delete-snippet)
                ))
           (s (find-snippet-in-workspace-or-output-history boxid)))
       (vdbg "Calling ~S~%" f)
       (funcall f s)
       ))
    (#.right-click (user-info-message "No right-click options available!"))
    ))

#+notyet
(defun handle-box-click-on-clear-delete 
       (type boxid-string mouse-code &aux boxid)
  "This function does the right thing when a box delete icon is clicked."
  (vdbg "In HANDLE-BOX-CLICK-ON-CLEAR-DELETE")
  (setq boxid (idstring->id boxid-string :snippet-id))
  (block exit
    (case (decode-mouse-single-click mouse-code)
      (#.left-click 
       (let ((f (ecase type 
                  (:delete 'handle-delete-snippet)
                  (:clear 'handle-clear-snippet)
                  (:clear-delete 'handle-clear-delete-snippet)
                  ))
             (s 
              (handler-case 
                  (find-snippet-in-workspace-or-output-history boxid)
                (vpl-internal-error 
                 ()
                 (show-status "Oops!")
                 (return-from exit nil)
                 ))))
         (vdbg "Calling ~S~%" f)
         (funcall f s)
         ))
      (#.right-click (user-info-message "No right-click options available!"))
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Boxes get unselected when the screen is redrawn (and possibly elsewise)
;;; The routine REDRAW-EVERYTHING-IN-WORKSPACE sets the currently
;;; selected box to NIL.

;; xxxxxxxxxxxxx

(defun do-box-selected (id)
  #.(one-string-nl
     "If a box which is currently selected is clicked by the user again"
     "(and therefore 'reselected'), we unselect it."
     "If a different box than the one that is currently selected is clicked,"
     "and therefore selected, we 'deselect' the old one and select the new one."
     "If a box is a constant box, then instead of selecting it or deselecting"
     "it we turn it into an input box."
     )
  (vdbg "Do-Box-Selected: boxID: ~A, *csb*: ~A~%" 
        id *current-selected-boxid*)
  (let ((snippet (find-snippet-in-workspace-or-output-history id)))
    (vdbg "Box selected on snippet of type ~S~%" (snippet-type snippet))
    (vdbg "Box contents: ~S~%" (get-snippet-property snippet :contents))
    (do-box-selected-method snippet)
    ))

(defmethod do-box-selected-method ((snippet snippet))
  (vdbg "box selected generic~%")
  (typecase snippet
    ((or choice-snippet aggregate-snippet 
         literal-snippet flag-snippet progn-snippet)
     nil)
    (otherwise (highlight-snippet-box snippet))
    ))

(defun highlight-snippet-box (snippet)
  (let ((id (snippet-id snippet)))
    (if (equalp *current-selected-boxid* id)
        (progn
          (vdbg "Clicking on box of type ~S~%" (type-of snippet))
          (unless (or (get-snippet-property snippet :hole-open)
                      (get-snippet-property snippet :hole-open-multiline))
            (unflashing-hilight-box *current-selected-boxid*)
            (setq *current-selected-boxid* nil)
            (setq *current-wrap-target* nil)
            ))
      (progn
        (when *current-selected-boxid* 
          (unflashing-hilight-box *current-selected-boxid*))
        (flashing-hilight-box id)
        (vdbg "Selecting box ~D~%" id)
        (setq *current-selected-boxid* id)
        ))))

(defmethod do-box-selected-method ((snippet constant-snippet))
  (vdbg "DBS snippet : ~S~%" snippet)
  (edit-box-selected snippet))

(defmethod do-box-selected-method ((snippet symbol-snippet))
  (edit-box-selected snippet))

(defun edit-box-selected (snippet)
  (vdbg "EB snippet : ~S~%" snippet)
  (if (snippet-replaced-hole? snippet)
      (progn
        ;; unhilight the currently highlighted box if any
        (unless (equal (snippet-id snippet) *current-selected-boxid*)
          (when *current-selected-boxid*
            (unflashing-hilight-box *current-selected-boxid*)))
        (Let ((Replacing-hole-snippet (edit-snippet (snippet-id snippet))))
          (vdbg "after Edit new snippet ~S~%" replacing-hole-snippet)
          (setq *modified-snippet* (snippet-parent replacing-hole-snippet))
          (setq *current-selected-boxid* (snippet-id replacing-hole-snippet))
          (setq *disable-selected-box-on-redraw?* nil)
          (redraw!)))
    (highlight-snippet-box snippet)
    ))

(defmethod do-box-selected-method ((snippet form-snippet))
  (vdbg "In do-box-selected-method for form-snippet!~%")
  (vdbg "Box contents: ~S~%" (get-snippet-property snippet :contents))
  (let ((open-already? 
         (or (get-snippet-property snippet :hole-open)
             (get-snippet-property snippet :hole-open-multiline))))
    (if open-already? 
        (do-open-box-selected snippet)
      (do-closed-box-selected snippet)
      )))

(defun do-closed-box-selected (snippet)
  ;; when it's currently closed, open it
  (vdbg "Opening hole...~%")
  (set-snippet-property snippet :hole-open t)
  (set-snippet-property snippet :contents nil)
  ;; highlight it
  (highlight-snippet-box snippet)
  ;; make sure it gets redrawn and focused on
  (setq *modified-snippet* snippet)
  (setq *focus-snippet* snippet)
  (vdbg "****** Focus snippet ID = ~D~%" (snippet-id snippet))
  (setq *vpl-workspace-modified?* t) 
  (setq *disable-selected-box-on-redraw?* nil)
  (redraw!)
  )

(defun do-open-box-selected (snippet)
  ;; if it's already open and not highlighted, highlight it. 
  (unless (equal (snippet-id snippet) *current-selected-boxid*)
    (highlight-snippet-box snippet))
  ;; Do NOT close it, do NOT redraw it.  In other words,
  ;; don't do anything else!
  ;; (setq *modified-snippet* snippet)
  ;; (setq *focus-snippet* snippet)
  ;; (vdbg "****** Focus snippet ID = ~D~%" (snippet-id snippet))
  )
  

(defun hole-open-multiline (form-snippet)
  (vdbg "Opening hole in multiline mode...~%")
  (set-snippet-property form-snippet :hole-open nil)
  (set-snippet-property form-snippet :hole-open-multiline t)
  (set-snippet-property form-snippet :contents nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OTHER ROUTINES;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *optimized-box-redraw* t)


;;; *** Need to test whether *modified-snippet* is the root of 
;;; the workspace or the results area as well as NIL.

(defun maybe-redraw-everything-in-workspace ()
  (vdbg "In maybe-redraw-everything-in-workspace...~%")
  (vdbg "Modified snippet: ~S~%" *modified-snippet*)
  (vdbg "disable: ~S~%" *disable-selected-box-on-redraw?*)
  (cond 
   (*modified-snippet* 
    ;; make sure we don't have 2 or more selected boxes at the same time
    ;; maybe not needed anymore?
    (when *current-selected-boxid* 
      (unflashing-hilight-box *current-selected-boxid*))
    (if (workspace-root-node? *modified-snippet*)
        (redraw-everything-in-workspace)
      (redraw-modified-box)
      ))
   (*function-from-palette-to-workspace-toplevel?*
    (add-one-more-toplevel-node))
   (t 
    (redraw-everything-in-workspace)
    ))
  (vdbg "Snippet boxes sent...~%")
  (if *disable-selected-box-on-redraw?* 
      (setq *current-selected-boxid* nil)
    (when *current-selected-boxid* 
      (flashing-hilight-box *current-selected-boxid*)
      )))

(defun redraw-modified-box () 
  (vdbg "In redraw-modified-box...~%")
  (labels ((root-of (snippet) 
             (if (toplevel-snippet? snippet)
                 snippet 
               (root-of (snippet-parent snippet))
               )))
    (let ((root (root-of *modified-snippet*)))
      (vdbg "Root : ~S~%" root)
      (parent-descendants-consistency-check root)
      (if *optimized-box-redraw* 
          (progn
            (vdbg "Redrawing using only modified snippet ~S~%"
                  *modified-snippet*)
            (provide-background-color-for-snippet-and-children root)
            (let ((box (modified-snippet->client-representation
                        *modified-snippet*)))
              (if-vpl-debug (pprint box))
              (redraw-box box)
              (let ((*focus-snippet* (or *focus-snippet* *modified-snippet*)))
                (do-focus)
                )))
        (let* ((box (toplevel-snippet->client-representation root)))
          (vdbg "Redrawing using root-snippet ~S~%" root)
          (if-vpl-debug (pprint box))
          (redraw-box box)
          )))))

(defun redraw-everything-in-workspace ()

  (vdbg "In redraw-everything-in-workspace...~%")
  
  (block exit 
    
    (vdbg "Sending clear workspace message to client...~%")
    (clear-workspace)

    (vdbg "Creating box representations from snippets...~%")

    (let* ((workspace-root (workspace-root-node))
           (toplevel-nodes (snippet-children workspace-root)))
      
      (when (get wb::*sessionid* :vpl-workspace-order-reversed?) 
        (vdbg "In child reverse...~%")
        (setq toplevel-nodes (reverse toplevel-nodes)))
      
      (parent-descendants-consistency-check workspace-root)

      (let ((box-representations 
             (handler-case 
                 (mapcar 
                  'toplevel-snippet->client-representation
                  toplevel-nodes)
               (error 
                (c)
                (vpl-internal-error 
                 #.(one-string-nl
                    "Problem converting snippet to box...~%"
                    "Actual error: ~A~%")
                 c)
                ;; (return-from exit nil)
                ))))      
        (vdbg "Sending ~D snippet box representations to client...~%"
              (length toplevel-nodes))
        (when *vpl-console-log*
          (loop for box in box-representations do (pprint box))
          (terpri))
	(apply 'add-workspace box-representations)
        (do-focus)
        ))))

(defun add-one-more-toplevel-node ()

  (vdbg "In add-one-more-toplevel-node...~%")
  
  (block exit 

    (when (get wb::*sessionid* :vpl-workspace-order-reversed?)
      (redraw-everything-in-workspace)
      (return-from exit nil))

    (vdbg "Creating box representation of new node...~%")

    (let* ((workspace-root (workspace-root-node))
           (toplevel-nodes (snippet-children workspace-root))
           (new-node (lastelem toplevel-nodes)))

      (when (= (length toplevel-nodes) 1)
        (redraw-everything-in-workspace)
        (return-from exit nil))

      (parent-descendants-consistency-check workspace-root)

      (let ((box-representation
             (handler-case 
                 (toplevel-snippet->client-representation new-node)
               (error 
                (c)
                (vpl-internal-error 
                 #.(one-string-nl
                    "Problem converting snippet to box...~%"
                    "Actual error: ~A~%")
                 c)
                ))))      
        (vdbg "Sending new snippet box representation to client...~%")
        (ppvdbg box-representation)
        (add-workspace box-representation)
        (do-focus)
        ))))

(defun do-focus ()
  (when *focus-snippet* 
    (vdbg "Calling focus-box on ~D...~%" 
          (snippet-id *focus-snippet*))
    (focus-box (snippet-id *focus-snippet*))
    (setq *focus-snippet* nil)
    ))
       

(defun redraw-everything-in-results ()
  
  (block exit 

    (vdbg "Sending clear results message to client...~%")
    (clear-results)
    
    (let* ((order-reversed? (get wb::*sessionid* :vpl-results-order-reversed?))
           (toplevel-nodes (snippet-children (results-root-node)))
           (focus-node (first toplevel-nodes)))
      
      (vdbg "Creating result box representations from snippets...~%")
      (let ((box-representations 
             (handler-case 
                 (mapcar 
                  'toplevel-snippet->client-representation
                  (if order-reversed?
                      toplevel-nodes 
                    (reverse toplevel-nodes)))
               (error 
                (c)
                (vpl-internal-error 
                 #.(one-string-nl
                    "Problem converting snippet to box...~%"
                    "Actual error: ~A~%")
                 c)
                ))))      
        (vdbg "Sending ~D snippet box representations to client...~%"
              (length toplevel-nodes))
        (apply 'add-results box-representations)
        ;; make sure the focus is always on the output snippet 
        ;; that was most recently created.  
        (when focus-node (focus-box (snippet-id focus-node)))
        ))
    
    (vdbg "Result snippet boxes sent...~%")
          
    ))

(defun redisplay-toplevel-box-variable-values ()
  (loop for toplevel-snippet in (snippet-children (workspace-root-node))
        do
        (typecase toplevel-snippet
          (symbol-snippet 
           (let ((box 
                  (modified-snippet->client-representation toplevel-snippet)))
             (redraw-box box)
             ))
          (otherwise nil)
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-user-popup-input (id user-input)
  (vdbg "In handle-user-popup-input, ID: ~S, user-input: ~S~%" id user-input)
  (let ((id-handler (intern (string-upcase id) *vpl-package*)))
    (unless (fboundp id-handler)
      (vpl-internal-error "~S has no function binding!" id-handler))
    (funcall id-handler user-input)
    ))

(defun handle-dump (obj)
  (vdbg "VPL sexp dump: ~s~%" obj)
  (send-out `(:vpl (:dump ,(format nil " ~s" obj)))))
                                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-update-open-box-contents (boxes)
  (vdbg "In JSON handle-update-open-box-contents, boxes = ~S~%" boxes)
  (loop for box-info across boxes do
       (destructuring-bind (&key boxid type value) (parse-json-object box-info)
	 (let* ((box-id (idstring->id boxid :snippet-id))
		(snippet (find-snippet-in-workspace box-id)))
	   (unless (typep snippet 'form-snippet)
	     (vpl-internal-error "Cannot box update non-form!"))
	   (cond
	     ((string-equal "input" type)
	      (unless (get-snippet-property snippet :hole-open)
		(vpl-internal-error 
		 "Should not be updating box contents of non-open hole!"))
	      (set-snippet-property snippet :contents value))
	     ((string-equal "multiline-input" type) 
	      (vdbg "In multiline update~%")
	      (unless (get-snippet-property snippet :hole-open-multiline)
		(vpl-internal-error 
		 "Should not be updating box contents of non-open hole!"))
	      (set-snippet-property snippet :contents value)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code to get various URLs that we will eventually want the user
;;; to be able to go to.

(defun handle-help-url-request ()
  (let ((url (help::help-for-bbl-users-url)))
    (declare (ignore url))
    nil
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;UTILITIES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string->int (numstring &optional (unreadable-return-code 0))
  #.(one-string-nl
     "Converts a string into a number.  Uses that number if already a number."
     "If string is unreadable as a number returns UNREADABLE-RETURN-NODE (0).")
  (cond
   ((numberp numstring) numstring)
   ((stringp numstring)
    (or (parse-integer numstring :junk-allowed t) unreadable-return-code))
   (t 
    (vpl-internal-error 
     "STRING->INT called with ~S, neither a string nor an integer."
     numstring
     ))))

(defun handle-palettemenu-tooltips ()
  (vdbg "In handle-palettemenu-tooltips")
  (send-json "palettemenutooltips" :data (create-palettemenu-tooltips))
  )

(defun create-palettemenu-tooltips ()
  (flatten
   (nconc 
    ;; get all the summary strings associated with bbl functions
    (lmaphashnn
     (lambda (id symbol)
       (vwhen (docstring (get-symbol-summary-string symbol))
         (list id docstring)
         ))
     *tid->template-symbol*
     )
    ;; get any summary strings associated with palette menu operators 
    (lmaphashnn 
     (lambda (id defun-opcode-name)
       (when (integerp id)
         (let ((docstring (get defun-opcode-name :defun-opcode-docstring)))
           (when docstring
             (list id docstring))
           )))
     *vpl-opcode-hash*
     ))))
   
(defun get-symbol-summary-string (symbol)
  (let ((docobj 
         (help::find-documentation symbol 'help::function-documentation)))
    (if docobj
        (cond
         ((eq :alias-of (help::explicitly-documented-p docobj))
          (let ((aliased-to (get symbol :alias-of)))
            (get-symbol-summary-string aliased-to)
            ))
         (t (help::docstring docobj))
         )
      (let ((d (documentation symbol 'function)))
        (or d (special-tooltip symbol))
        ))))

(defparameter *special-tooltips* 
  '(
    (not "Boolean negation.  Nil -> T, anything else -> Nil")
    (and "Boolean AND...all arguments must be non-nil to return non-nil")
    (case "Select one of multiple code branches")
    (comment "Enter a comment.  Returns nil")
    (defun "Define a lisp function")
    (identity "Returns the argument provided")
    (or "Boolean OR...Returns the first non-nil argument, or nil")
    (print "Lisp basic printing function")
    (progn "Execute a set of function calls")
    (return "Exit immediately out of a loop or other control structure")
    (return-from "Exit immediately, returning a value to a block label")
    (sleep "Pause execution for n seconds")
    (sequence-viewer "Brings up the argument in the sequence viewer")
    (sqrt "Returns the square root of a number")
    (typecase "Select one of multiple code branches based on type of argument")
    (unless "Equivalent to (when (not ...) ...)")
    (when "Execute code only if a condition is true")
    (* "Multiplication")
    (+ "Addition")
    (- "Subtraction")
    (/ "Division or reciprocal")
    (bbi::%curly-list% "Create a list")
    (bbl::range-ref "A range of indices")
    ))
    
    
      

(defun special-tooltip (symbol)
  (cadr (assoc symbol *special-tooltips*)))

(defun handle-search-box-request (text)
  (help-search-for-function text))
