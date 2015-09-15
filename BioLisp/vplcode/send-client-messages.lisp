;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)

;;; +=========================================================================+
;;; | Copyright (c) 2006 John Myers, JP Massar                                |
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

;; Author: John Myers, JP Massar.

(defun enable-new-hole-mode ()
  (change-client-settings '(:new-hole-opening-mode "on")))

(defun disable-new-hole-mode ()
  (change-client-settings '(:new-hole-opening-mode "")))

(defun enable-extended-sessionid-info ()
  (change-client-settings '(:extended-sessionid-info "on")))

(defun disable-extended-sessionid-info ()
  (change-client-settings '(:extended-sessionid-info "")))

(defun change-client-settings (settings)
  (vdbg "Changing client settings: ~S~%" settings)
  (send-json "change-client-settings" :settings settings))

(defvar *enable-browser-title-init* t)

(defun initialize-vpl-browser-title ()
  (when *enable-browser-title-init* 
    (vdbg "Sending VPL browser title message~%")
    (send-json 
     "browser-title"
     :title  
     (formatn "VPL version ~A ~A ~A ~A ~A ~A" 
              user::*vpl-version*
              user::*weblistener-machine-name*
              wb::*current-weblistener-port*
              #+:allegro
              (excl::getpid)
              #-:allegro
              ""
              (if wb::*username* (string wb::*username*) "")
              (if wb::*sessionid* 
                  (wb::sessionid->title-display wb::*sessionid*)
                ""
                )))))

(defvar *execution-message-mode* t)
(defvar *execution-message-timeout* t)
(defvar *palette-init-message* nil)

(defun start-execution-message () 
  (when *execution-message-mode* 
    (if (null *execution-message-timeout*)
        (send-json 
         "start-execution"
         :opcode
         (formatn "~D" (operator->opcode 'kill-process-operator))
         )
      (send-json 
       "start-execution"
       :opcode
       (formatn "~D" (operator->opcode 'kill-process-operator))
       :timeout (formatn "~D" (or wb::*execution-timelimit* 0))
       ))))

(defun update-execution-status (status)
  (send-json "update-execution-status" status))

(defun end-execution-message () 
  (when *execution-message-mode* (send-json "end-execution")))

(defun start-palette-init-message ()
  (when *palette-init-message* (send-json "start-palette-init")))

(defun end-palette-init-message ()
  (when *palette-init-message* (send-json "end-palette-init")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   ERROR MESSAGE POPUPS AND NEW WINDOW COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun show-vpl-error-message (message)
  (send-json "error-message" :error message))

(defun show-dialog (id title label dialog-type &optional dialog-type-options)
  (ulog "Showing Dialog ~A, ~A~%" id title)
  (when (symbolp id)
    (setq id (string-downcase (string id))))
  (send-json
   "show-dialog"
   "id" id 
   "title" title
   "label" label
   "dialogType" (formatn "~A" dialog-type)
   "dialogTypeOptions" dialog-type-options))

(defun show-status (message)
  (vdbg "In show-status...~%")
  (send-json "show-status" "message" message))

;;;;;;;;;;;;;;;;;;;;;;; Palette commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Currently supported as of Oct '06:  "blue" "red" "green" "black".
;; Actual colors found in vpl/DropDownMenuX.css .
;; For complex reasons it's currently necessary to have codes, 
;; not hex values, for menu colors.

(defun initialize-a-palette-menu (menu-data color)
  (let ((menu-id (first menu-data)))
    (vdbg "Sending palettemenu ~A~%" menu-id)
    (send-json "make-menu" "menu" (package-menu-into-json menu-data color))
    (track-menu-id menu-id)))

(defun initialize-many-palette-menus (menu-data-and-color-list)
  (vdbg "Sending multiple palette menu definitions to server...~%")
  (send-json "make-menus" "menus" 
	     (coerce
	      (mapcar 
	       (lambda (mdc) 
		(destructuring-bind (menu-data color) mdc
		  (package-menu-into-json menu-data color)))
	       menu-data-and-color-list)
	      'vector))
  (loop for (menu-data nil) in menu-data-and-color-list do
       (track-menu-id (first menu-data))))


(defun track-menu-id (id)
  (let ((session-menu-ids (get wb::*sessionid* :session-menu-ids)))
    (pushnew id session-menu-ids)
    (setf (get wb::*sessionid* :session-menu-ids) session-menu-ids)
    ))
  

(defun replace-a-palette-menu (menu-data color)
  ;; need this UNLESS otherwise debug output would become part of user's 
  ;; print out when evaluating 
  (unless wb::*vpl-evaluating?* 
    (vdbg "Sending replace palettemenu ~A~%" (first menu-data)))
  (send-json "replace-menu" "menu" (package-menu-into-json menu-data color)))

(defun add-to-user-palette-menu (menu-id menu-option id)
  (declare (ignore menu-id menu-option id))
  ;; the command to add MENU-OPTION to the user's palette menu 
  ;; (and return ID when that option is selected) goes here.
  )

(defun remove-a-palette-menu (menu-id)
  (vdbg "Sending :remove-menu command to client...~%")
  (send-json "remove-menu" "menuID" menu-id)
  (let ((session-menu-ids (get wb::*sessionid* :session-menu-ids)))
    (setf (get wb::*sessionid* :session-menu-ids) 
          (remove menu-id session-menu-ids)))
  menu-id)

(defun remove-all-palette-menus ()
  (vdbg "In remove-all-palette-menus...~%")
  (send-json "clear-palette"))

;;;;;;;;;;;;;;;;;;;;;;; Workspace commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clear-workspace ()
  #.(one-string-nl
     "Server -> Client. Sends a :clear-workspace command to clear out"
     "the workspace presentation.")
  (send-json "clear-workspace"))

(defun clear-status () (show-status ""))

(defun add-workspace (&rest boxes)
  #.(one-string-nl
     "Server -> Client. Sends multiple :add-workspace commands, each consisting"
     "of one JBML SEXP, to the workspace, to get added onto the end.")
    (loop for box in boxes do
	 (vdbg "Sending JBML '~A'~%" box)
	 (send-json "add-workspace" "boxes" (boxes->json box))))

(defun redraw-box (&rest boxes)
  ;;Boxes get replaced based on the ID number of the topmost box.
  #.(one-string-nl
     "Server -> Client. Sends multiple :redraw-box commands, each consisting"
     "of one JBML SEXP, to the workspace, to get replaced in the middle.")
  
  (vdbg "In redraw-box...~%")
  (ppvdbg (first boxes))

  (loop for box in boxes do (send-json "redraw-box" "boxes" (boxes->json box))))

(defun focus-box (box-id)
  #.(one-string-nl
     "Server -> Client. Sends a :focus-box command, consisting"
     "of one JBML SEXP, to the workspace, to get scrolled into view.")
  (vdbg "In focus-box...~%")
  (send-json "focus-box" "boxID" box-id))

(defun redraw-workspace (&rest sexps)
  #.(one-string-nl
     "Sends one or more JBML SEXPs to the workspace, replacing the"
     "current presentation.")
  (clear-workspace)
  (apply 'add-workspace sexps))


;;;;;;;;;;;;;;;;;;;;;;;;; Results Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-results (&rest boxes)
  #.(one-string-nl
     "Server -> Client. Sends multiple :add-results commands, each consisting"
     "of one JBML SEXP, to the results space, to get added onto the end.")
    (loop for box in boxes do
	 (vdbg "Sending JBML '~A'~%" box)
	 (send-json "add-results" "boxes" (boxes->json box))))


(defun clear-results ()
  ;; Clears the results presentation.
  (send-json "clear-results"))

(defun redraw-results ()
  (redraw-everything-in-results))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flashing-hilight-box (box-ID)
  "Visually turns box into flashing box, indicating selection."
  (setq box-ID (string->int box-ID))
  (if (> box-ID 0)
      ;;do stuff
      (progn 
        (vdbg "FlashHilight Box sending out boxID ~A~%" box-ID)
	(send-json "flashing-hilight-box" "boxID" box-ID)
        )
    (usyslogdbg "FlashHilight Box ID less than one ~A~%" box-ID)
    ))

(defun unflashing-hilight-box (box-ID)
  #.(one-string-nl
     "Server -> Client. Sends an :unflashing-hilight-box command,"
     "which gets rid of visual strong hilight around the given box.")
  (vdbg "Unflashhilight boxID: ~A~%" box-ID )
  (setq box-ID (string->int box-ID))
  ;; (vdbg "(> boxID 0): ~A~%" (> box-ID 0) )
  (if (> box-ID 0)
      (progn 
        (vdbg "Unhilight Box sending out boxID ~A~%" box-ID)
	(send-json "unflashing-hilight-box" "boxID" box-ID)
        )
    (usyslogdbg "Unflashinghilight Box ID less than one ~A~%" box-ID)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-and-increment-cascade-step ()
  (let ((step (get wb::*sessionid* :cascade-step)))
    (if (null step) 
        (progn
          (setf (get wb::*sessionid* :cascade-step) 1)
          (get-and-increment-cascade-step))
      (progn
        (if (= 10 step)
            (setf (get wb::*sessionid* :cascade-step) 1)
          (incf (get wb::*sessionid* :cascade-step)))
        step
        ))))
          
;; Returns the client window ID of the newly created window.

(defun show-vpl-popup-URL-window 
       (URL &key
            (window-id nil)
	    (relative-p 1 )
            (toolbar "yes")
            (location "no")
            (directories "no")
            (status  "no")
            (menubar "no")
            (scrollbars "yes")
            (resizable "yes")
            (width "700px") 
            ;;Note:  px for pixels is required, unless % used or something.
            (height "400px")  ;;see previous.
            (top "30px" top-provided?)
            (left "30px" left-provided?)
            (bottom "auto")
            (right "auto")
            (margin-left "auto")
            (margin-right "auto")
            (border-style "double")
            (background "#ffffff")
            (font-family "'Courier New', Courier, monospace")
            (z-index "1000")  
            ;;Sorting stack parameter. 0 is flat. Minus puts in back.
            (position "absolute")
            (extras "")
            )
  (unless (stringp window-id)
    (setq window-id (string (gensym (s+ (string wb::*sessionid*) "-")))))
  (let* ((computed-top
          (if (= 1 relative-p)
              (if (null top-provided?)
                  (formatn "~Dpx" (* 30 (get-and-increment-cascade-step)))
                top)
            top))
         (computed-left 
          (if (and (= 1 relative-p) (null left-provided?))
              computed-top
            left))      
         (specs
          (concatenate 
           'string
           "toolbar="  toolbar
           ", location="  location 
           ", directories="  directories
           ", status=" status 
           ", menubar="  menubar 
           ", scrollbars="   scrollbars 
           ", resizable="  resizable 
           ", width="  width 
           ", height="  height 
           ", top="  computed-top
           ", left="  computed-left 
           ", bottom="  bottom 
           ", right="  right 
           ", margin-left="  margin-left 
           ", margin-right="  margin-right 
           ", border-style="  border-style 
           ", background="  background 
           ", font-family="  font-family 
           ", z-index="  z-index 
           ", position=" position 
           extras 
           )))
    (ulog "Creating new window using URL ~A~%  window-id: ~A~%" URL window-id)
    (send-json "popup-url-window"
	       "windowID" window-id
	       "url" URL
	       "specs" specs
	       "relative" relative-p))
  window-id
  )

(defun kill-vpl-window (window-num)
  (ulog "Killing VPL window number ~A~%" window-num)
  (send-json "kill-window" "windowID" (princ-to-string window-num)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-test
(defun test-workspacepipe () nil)

#+test
(defun test-workspacepipe ()
  (vdbg "Test workspacepipe *Channel* = ~A~%" *channel* )

  (show-dialog 1 "hello world" "something goes here!" "input")
  (show-dialog 2 "goodbye world" "something to think about..." "textarea")
  (show-dialog
   3 
   "choose something" "choices" "select" 
   #((:label "apples" :value 1)
     (:label "oranges" :value 2)
     (:label "grapes" :value 3 :selected t)
     (:label "pears" :value 4)))
  (replace-a-palette-menu 
   '(40 
     "REPLACED HELP"
     (("Rep Example Code" NIL (("Rep Arithmetic" 321) ("Rep Simple Loop" 326))))
     (("Rep Help (general)" 216) ("Rep Help (VPL UI)" 221)))
   palette-tools-menus-color  )



  (show-vpl-popup-URL-window "http://www.seqcon.com" 
                             :toolbar "yes"
                             :location "yes"
                             :directories "yes"
                             :status  "yes"
                             :menubar "yes"

                             :width "800px"
                             :height "200px"
                             :top "10"
                             :left "10"
                             ;;				      :left "30%"
                             :position "static"
                             )
  (add-workspace  

   '(1 MyWorkspace :jbml-b foo :jbml-b (2 Cleared))
   '(3 Testing Add (4 Nested boxes))

  ;(redraw-workspace

   '(2 (3 Nested boxes))
;;Note close-icon comes BEFORE go-icon.
   '(4 :jbml-i :jbml-b :jbml-ul :jbml-color "#ff0000" :jbml-exec-icon Red Italics bold underline box default :jbml-menu-icon (5 but subboxes :jbml-cr start over :jbml-cr :jbml-icon) :jbml-close-icon :jbml-go-icon)
   '(6  :jbml-CLEAR An-Outdent :jbml-outdent followed by :jbml-CR carriage return followed by :jbml-cr another carriage return)
   '(7 :jbml-options-menu "Menu Widget" (15 :jbml-menu-entry "option 1")(16 :jbml-menu-entry "option 2"))
   '(8 :jbml-thick :jbml-box-color "#00ff00" (10 :jbml-options-menu "Menu Widget in box" 
                                      (15 :jbml-menu-entry "option 1")(16 :jbml-menu-entry "option 2"))) 
   '(9 :jbml-thick :jbml-box-color "#00ffbb" (11 :jbml-options-menu "Menu Widget in box" 
                                      (15 :jbml-menu-entry "option 1")(16 :jbml-menu-entry "option 2")) with flags (9 :jbml-dotted " "))
   '(21 :jbml-CLEAR-DELETE :jbml-input-text long-symbol) 
   '(22 :jbml-input-text "long-string") 
   '(23 :jbml-input-text "1234567890")
   '(24 :jbml-dotted-blink :jbml-BOX-COLOR "#FF00ff" "dotted blink")
   '(25 function-hole (26 :jbml-hole))
   '(26 "Right-click here!")
   '(27 opened-hole (29 :jbml-hole-opened "prompt!"))

   '(28 default-opened-hole (30 :jbml-hole-opened))

   '(29 (30 :jbml-main-box-menu "Main Box Menu Title"
	    (15 :jbml-menu-entry "option 1" )(16 :jbml-menu-entry "option 2"))
"Main Box Menu" "more"
(31 :jbml-options-menu "Options Menu Title"
	    (15 :jbml-menu-entry "option 1" )(16 :jbml-menu-entry "option 2"))
)


   ) 

;  (redraw-box '(3 :jbml-BOX-COLOR "#FF00ff" "Redrawn box!" :jbml-CR (123 "further nest")))
  (focus-box '(3))

;(show-vpl-display-message "This illustrates a link in text: <a href='http://www.seqcon.com/caseJPL.html'>Robot movie!</a>")

;(kill-vpl-window 0)  ;;Kills main window!
;(kill-vpl-window 1)


)

(defun test-results ()
  (add-results  
   '(101  :jbml-left-justify Left "This is a very long line that is almost practically guaranteed to wrap and then the questions is which direction does it go in" My-Results "Text" :jbml-b foo :jbml-b (102 Cleared))
   '(103  :jbml-right-justify Right "This is a very long line that is almost practically guaranteed to wrap and then the questions is which direction does it go in" Testing Add "Text" (104 Nested boxes))
   '(101 :jbml-center-justify :jbml-courier Courier Center "This is a very long line that is almost practically guaranteed to wrap and then the questions is which direction does it go in"  Courier Results "Text" :jbml-b foo :jbml-b (102 Cleared))
   '(103 :jbml-courier Testing Add "Text" (104 Nested boxes)))
  )
