;;; -*-lisp-*-
;;
;;  All log entries for this file moved to vplls-comments.txt
;;  Please add further log entries there. 
;;
;;  PIPE INTERFACE
;;  Incoming:   function (defun vpl-on-message (message))  handles dispatching.
;;  Outgoing:
;;              ;; Sends command "<vpl>hello</vpl>"
;;              ;; and sets client status to "Sent hello.".
;;              (package-simple-command "hello")
;;              ;; Sends command "<vpl>my-command<number>54</number><string>Test</string></vpl>"
;;              ;; and sets client status to "Status-message".
;;              (.send (.append-string (.append-number (new Packaged-command "my-command" "Status-message") 54) "Test")) 
;;  Other end of pipe:  vpl.lisp, (defun vpl (channel message))
;;
;;  SYSTEM SETUP
;;  function setup, which calls message "hello".
;;  - Menu gets displayed upon receipt of an event coded "makemenu".
;;
;;  UNPACKING XML ELEMENTS
;;  first-child  (@ message first-child) gives the first subnode.  Save it.
;;  next-sibling (@ current-child next-sibling) gives the brother node, until...
;;  node-name    (@ message node-name) gives the XML tag for a tree node
;;  Dom-library.get-text-content (.get-text-content Domlibrary node) gives the contents of a node (includes any children such as text nodes).
;;  node-type=3  (= (@ mynode node-type) 3) defines text nodes.
;;  "There can be spurious text nodes if italics or extra spaces are in parsed input string."
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar global-default-text-color "#000000")  ;;Haha.  Can't do *global-names* in LispScript, thpppp.
(defvar hole-color   "#EBEBEB")  ;;"#e0e0ff")
(defvar box-flash-color "#00AA00")
(defvar last-mouse-x  "0")
(defvar last-mouse-y  "0")

(defvar main-expression false)
(defvar selected-hole false)
(defvar menu-visible false)		; menu being shown
(defvar saved-text-node null)		; text to be restored if nothing entered
(defvar copied-item null)		; copy of a node we might paste
(defvar copied-sexp null)		; copy of a sexp we might paste
(defvar empty-sexp null)
(defvar DDmenuID  0)                 ;uniquification for menus.
(defvar DDmenus (new Array))

;; Fixup in generated code that works around an IE bug.
(defvar lispscript-fix-up-elements false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings-storage
;; 
;; Author: Johnny Casey
;;
;; Provides a class to manage registering, storing and accessing application
;; client settings.
(defvar Settings-storage
        ;; Private static functions
  (let ((default-update
	  (lambda (value)
	    ;; Generic client-setting variable update function.
	    ;; Either get a value or set to false
	    (or value false)
	  ))
	(settings-storage-constructor null)
	(empty-setting (new Object))
	)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Constructor
    (set settings-storage-constructor
      (lambda ()
              ;; Instance variables
	(let ((my-self this)
	      (my-settings (new Object))
	      )
	  ;; Instance functions
	  (define-class-method (my-self initialize) (name initial-value value-update)
	    ;; Initializes a client-setting variable.
	    (let ((setting (new Object)))
	      (set (@ setting value) (or initial-value false))
	      (set (@ setting update) (or value-update default-update))
	      (unless (ref my-settings name)
	        (set (ref my-settings name) setting)
	      )
	    ))

	  (define-class-method (my-self get) (name default-value)
	    ;; Returns the requested client-setting.
	    (or (@ (or (ref my-settings name) empty-setting) value)
	        default-value))

	  (define-class-method (my-self set) (name value)
	    ;; Updates a client-setting to the specified value using an appropriate updater.
	    (let ((setting (ref my-settings name)))
	      (when setting
	        (set (@ setting value) ((@ setting update) value))
	      )
	    ))
	  (set (@ my-self normalize-name) (@ settings-storage-constructor normalize-name))
	  ;; Return the Settings-storage object
	  my-self
	)))

    ;; Public static functions
    (define-class-method (settings-storage-constructor normalize-name) (name)
      (.to-lower-case name))

    ;; Return the Settings-storage constructor object
    settings-storage-constructor
  )
)


(defvar client-settings (new Settings-storage))
(.initialize client-settings "new-hole-opening-mode")
(.initialize client-settings "show-status-overflow-link" true)
(.initialize client-settings "default-jbml-dnd-drag" true)
(.initialize client-settings "default-jbml-dnd-drop")
(.initialize client-settings "single-click-delay" 100)
(.initialize client-settings "drag-click-delay" 500)
;; Additional client-setting following Packaged-command


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-args-from-URL ()
  ;; Collects an associative array of values from the URL.
  (let* ((myargs (new Object))                         ; new Object();???
         (query (.substring (@ location search) 1))  ;location.search.substring(1)  ;;Rip off initial question-mark.
         (pairs (.split query ","))                  ;break apart by commas.
         )
    (dotimes (i (@ pairs length))
      (let (
            (pos (.index-of (ref pairs i) "=")  )    ;;Find position of "=".
            (argname "")
            (value "")
            )
        (when (> pos -1)
          (set argname (.substring (ref pairs i) 0 pos))
          (set value   (.substring (ref pairs i) (+ pos 1)))
          (set (ref myargs argname) (unescape value))
        )))
    myargs
  )
)


(defun get-session-id ()
  ;; Retrieve the session-id from the current URL or "0" if not found
  (or (ref (get-args-from-URL) "PKG") "0")
)


;;;;;;;;;;;;;Client side incoming message dispatch function;;;;;;;;;;;;;

(defun vpl-on-message (message)
  ;; This is the function for dispatching incoming pipe messages.
  ;; skip empty text foo
  (while (= (@ message node-type) 3)         ;;spurious text message?
    (.set-session-counter Packaged-command (.get-text-content Dom-library message))
    (set message (@ message next-sibling))   ;;flush it; move to next.
  )
  ;;
  ;;  "message" is now set to the first non-text node in the XML tree.
  ;;   Now it's time to set "message-type".
  ;;
  (clear-client-status)
  (let ((message-type (@ message node-name)))
    (case (+ "" message-type)

     ;;;;;;;      result      ;;;;;;;;;;
      ("result"
         (xml-unpack-handle-result message))

     ;;;;;;;       dump       ;;;;;;;;;;
      ("dump"
       (debug "sexp contents: " (.get-text-content Dom-library message)))

     ;;;;;;;  change-client-settings ;;;;;;;;;
      ("change-client-settings"
        (xml-unpack-handle-change-client-settings message))

     ;;;;;;;   client version   ;;;;;;;;
      ("vpl-version"
        (package-vpl-version))

     ;;;;;;;  popup-URL-window  ;;;;;;;;;;   Dec 03 JKM used!
      ("popup-url-window"
         (xml-unpack-handle-popup-URL-window message))

     ;;;;;;;    kill-window      ;;;;;;;;;;
      ("kill-window"
         (xml-unpack-handle-kill-window message))

;;;;;;;;;;;;;;;;;;;;;;;Workspace Commands;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;  clear-workspace  ;;;;;;;;;;
      ("clear-workspace"
       (clear-workspace))

     ;;;;;;;  add-workspace  ;;;;;;;;;;
      ("add-workspace"
         (xml-unpack-handle-add-workspace-box message))

     ;;;;;;;  redraw-box  ;;;;;;;;;;
      ("redraw-box"
         (xml-unpack-handle-redraw-box message))

     ;;;;;;;  focus-box  ;;;;;;;;;;
      ("focus-box"
         (xml-unpack-handle-focus-box message))

;;;;;;;;;;;;;;;;;;;;;Results Commands;;;;;;;;;;;;;;;;;;;;;;;;

     ;;;;;;;  clear-results  ;;;;;;;;;;
      ("clear-results"
       (clear-results))

     ;;;;;;;  add-results  ;;;;;;;;;;
      ("add-results"
         (xml-unpack-handle-add-results-box message))

;;;;;;;;;;;;;;;;;;;;;Palette Commands;;;;;;;;;;;;;;;;;;;;;;;;

     ;;;;;;;  clear-palette  ;;;;;;;;;;
      ("clear-palette"
       (clear-palette))

     ;;;;;;;    add-palette-button      ;;;;;;;;;;
      ("add-palette-button"
         (xml-unpack-handle-add-palette-button message))

     ;;;;;;;    make-menus      ;;;;;;;;;;
      ("make-menus"
         (xml-unpack-handle-make-menus message))

     ;;;;;;;    makemenu      ;;;;;;;;;;
      ("makemenu"
         (xml-unpack-handle-makemenu message false))

     ;;;;;;;    replacemenu      ;;;;;;;;;;  ;;Note: Only for palette menus!  Use redraw-box for others!
      ("replacemenu"
         (xml-unpack-handle-makemenu message true))

     ;;;;;;;    remove menu      ;;;;;;;;;;
      ("remove-menu"
         (xml-unpack-handle-remove-menu message))
;;;;;;;;;;;;;;;;;Overall General Commands;;;;;;;;;;;;;;;;;;;;;

     ;;;;;;;    query contents      ;;;;;;;;;;
      ("query-open-box-contents"
         (xml-unpack-handle-query-open-box-contents message))

   ;;;;;;;      flying box    ;;;;;;;;;;
      ("flying-box"
       (debug "sexp contents: " (.get-text-content Dom-library message)))

     ;;;;;;;    hilight box      ;;;;;;;;;;
      ("hilight-box"
         (xml-unpack-handle-hilight-box message))

     ;;;;;;;    unhighlight box      ;;;;;;;;;;
      ("unhilight-box"
         (xml-unpack-handle-unhilight-box message))

     ;;;;;;;    flashing-hilight box      ;;;;;;;;;;
      ("flashing-hilight-box"
         (xml-unpack-handle-flashing-hilight-box message))

     ;;;;;;;    unflashing-highlight box      ;;;;;;;;;;
      ("unflashing-hilight-box"
         (xml-unpack-handle-unflashing-hilight-box message))

     ;;;;;;;    add-boxmenu      ;;;;;;;;;;
      ("add-boxmenu"
         (xml-unpack-handle-makeboxmenu message))

     ;;;;;;;    attach-menu-to-box      ;;;;;;;;;;
      ("attach-menu-to-box"
         (xml-unpack-handle-attach-menu-to-box message))

     ;;;;;;;    error-message   ;;;;;;;;;;
      ("error-message"
         (xml-unpack-handle-error-message message))

     ;;;;;;;    display-message   ;;;;;;;;;;
      ("display-message"
         (xml-unpack-handle-display-message message))

     ;;;;;;;	show-dialog	;;;;;;;
      ("show-dialog"
        (xml-unpack-handle-show-dialog message))

     ;;;;;;;    show-status     ;;;;;;;
      ("show-status"
        (xml-unpack-handle-show-status message))

;;;;;;;;;;;;;;;;;;;;;VPL Windows

     ;;;;;;;  ...otherwise    ;;;;;;;;;;
      (t
       (debug "Unrecognized message type stamp: " message-type " with content: " (@ message textContent))))))

;;;;;;;;;;;;;;;;;;;;;;end of dispatch function;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gecko-revision ()
  (and (eq "Gecko" (@ navigator product))
       (parse-float (.substring (@ navigator user-agent) (+ (.index-of (@ navigator user-agent) "rv:") 3)))
       )
)


(defun redirect-on-incompatible-browser ()
  ;; See http://www.seehowitruns.org/index.php for an online database of browser javascript signatures.
  ;; See https://addons.mozilla.org/firefox/59/ for "User Agent Switcher" by Chris Pederick addon to test old code (only used appName/appVersion).
  ;; See http://developer.mozilla.org/en/docs/CVS_Tags for the dates of various Mozilla/Firefox releases.  Firefox 1.5 release date is 2005/11/11.
  (let (
	;; JJC 2007/01/21
        ;; Keep this simple.  Only use builtin JavaScript 1.2 compatible code.
	;; If the test fails, the redirection does not happen.
	;; No string-equals.
	(browser-name (@ navigator app-name))
	;; Use parseFloat, which is a builtin Javascript function which converts
	;; a string to a float.  Trailing characters are ignored, must begin
	;; with a number (which is what the original call to .substring() did).
	(browser-vers (parse-float (@ navigator app-version)))
	(browser-product (@ navigator product))
	(browser-product-vers (parse-int (@ navigator product-sub)))
	)
    (unless (and (eq browser-name "Netscape")
		 (>= browser-vers 5.0)
		 (eq browser-product "Gecko")
		 (or (<= 1.8 (gecko-revision))
		     )
	         )
      (set (@ window location) "vpl-other.html")
    ))
)


(defun handle-document-keypress (e)
  (let ((e (.get-event Event-library e)))
    (when (@ e ctrl-key)
      (package-global-keypress (.get-keypress-char-code Event-library e) (@ e ctrl-key))
    ))
  ;; Allow the event to propagate to the browser
)


(defun handle-document-resize (e)
  (update-toolboxes)
)


;;; JP 06/18/06
;;; finally figured out a way to make the menu clear by attaching
;;; an onclick method to the underlying object representing the workspace.
(defun setup ()
  ;; This is the function for getting the server to set up the client properly.
  (redirect-on-incompatible-browser)

  ;;Get server to set up.
  (.set-session-id Packaged-command)
  (package-simple-command "hello")

  (setup-palette)
  (setup-workspace)
  (setup-results)
  (update-toolboxes)

  (set (@ document onkeypress) handle-document-keypress)
  (set (@ document onresize) handle-document-resize)

  ;; (drag-and-drop-setup)
)


(defun setup-workspace ()
  (let ((dummy (new Object))
  	(workspace (.get-element-by-id document "workspace"))
	(undo (.get-element-by-id document "left-undo"))
	(redo (.get-element-by-id document "right-redo"))
	(clear (.get-element-by-id document "workspace-clear"))
	(expand (.get-element-by-id document "workspace-expand"))
	(shade (.get-element-by-id document "workspace-shade"))
        )
    (set (@ (or workspace dummy) onclick) package-click-workspace)

    (set (@ (or undo dummy) onclick) package-left-undo-click)
    (set (@ (or redo dummy) onclick) package-right-redo-click)
    (set (@ (or clear dummy) onclick) package-clear-workspace)
    (set (@ (or expand dummy) onclick) handle-workspace-expand)
    (set (@ (or shade dummy) onclick) handle-workspace-shade)
    (update-workspace-shade-expand)
  )
)


(defun setup-results ()
  (let ((dummy (new Object))
  	(results (.get-element-by-id document "results"))
  	(clear (.get-element-by-id document "results-clear"))
	(expand (.get-element-by-id document "results-expand"))
  	(shade (.get-element-by-id document "results-shade"))
        )
    (set (@ (or results dummy) onclick) package-click-results)

    (set (@ (or clear dummy) onclick) package-clear-results)
    (set (@ (or expand dummy) onclick) handle-results-expand)
    (set (@ (or shade dummy) onclick) handle-results-shade)
    (update-results-shade-expand)
  )
)


(defun setup-palette ()
  (let ((logo (html ((:img :id "palette-logo" :width "122px" :height "107px" :style "float: right; position: relative;" :src "Bike.gif" :border "0px" )))))
    (set (@ logo onclick) (lambda () (alert (+ "VPL Client Version: " vpl-version))))
    (.append-child (.get-element-by-id document "palette") logo)
  )
)


(defun no-op ())


(.register-message-handler channel "vpl" vpl-on-message)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Click handler functions

(defun get-mouse-delay-timer (node)
  ;; Retrieves the timer stored on a node
  (or (@ node x-mouse-delay-timer) false)
)


(defun set-mouse-delay-timer (node timer)
  ;; Stores a timer on a node for later so it can be cleared.
  (set (@ node x-mouse-delay-timer) timer)
)


(defun clear-mouse-delay-timer (node)
  ;; Clears a timer stored in a node and removes it.
  (when (get-mouse-delay-timer node)
    ;Stop single click count down, if any.
    (clear-timeout (get-mouse-delay-timer node))
  )
  (set-mouse-delay-timer node null)
)


(defun single-click-wrapper (node callback click-delay)
  ;;; Wraps a single click for conflict against double clicks on same object.
  ;; NOTE: the second argument must be a function object
  ;; Because this is node-based, it handles different objects at same time w/ no problem.
  (let ((augmented-callback
	  (lambda ()
	    (callback)
	    (clear-mouse-delay-timer node)
	  ))
	(click-delay (or click-delay (.get client-settings "single-click-delay")))
	)
    ;Wipe out previous single click count down, if any.
    (clear-mouse-delay-timer node)
    (set-mouse-delay-timer node (set-timeout augmented-callback click-delay))
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;PACKAGE ROUTINES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packaged Command Base
;; 
;; Author: Johnny Casey
;;
;; Base class of a server command
;; Expects the this.message and this.status-message to be defined by
;; calling setup.
(defvar Packaged-command-base
  ;; This relies on let creating a closure
  (let ((package-constructor (lambda () (and "Packaged-command-base" this)))
	;; Append the child element and return the object
	(append-element
	  (lambda (object element)
	    (.append-child (@ object message document-element) element)
	    object
	  ))
	)

    ;; Instance functions
    ;; Set up the instance, meant to be called by constructors.
    ;; status-message is optional (false value replaced by "Sent command.")
    ;; This breaks the defmethod macro, because it returns a value
    (set (@ package-constructor prototype setup)
      (lambda (command status-message message)
        (set (@ this status-message) (or status-message (+ "Sent " command ".")))
	(set (@ this message) message)
	this
      ))
    ;; This breaks the defmethod macro, because it returns a value from let
    (set (@ package-constructor prototype append-number) 
      (lambda (value)
	(let ((value (+ "" (parse-int value)))
	      (message (@ this message))
	      )
	  (append-element this (xml-element (message) (:number value))))))
    ;; This breaks the defmethod macro, because it returns a value from let
    (set (@ package-constructor prototype append-string)
      (lambda (value)
	(let ((value (escape-string (+ "" value)))
	      (message (@ this message))
	      )
	  (append-element this (xml-element (message) (:string value))))))
    (defmethod (package-constructor append-jbml-value) (jbml value)
      (append-element this (.package-value jbml (@ this message) value)))
    (defmethod (package-constructor append-jbml-objects) (jbml object)
      (append-element this (.package-objects jbml (@ this message) object)))

    ;; This breaks the defmethod macro, because it calls multiple functions
    (set (@ package-constructor prototype send)
      (lambda ()
	(show-client-status (@ this status-message))
	(.send channel (@ this message))
      ))

    ;; Return the Packaged-command-base class object
    package-constructor
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packaged Command Basic
;; 
;; Author: Johnny Casey
;;
;; A class to represent a server command
(defvar Packaged-command-basic
  ;; This relies on let creating a closure
        ;; Static variables
  (let ((our-session-id "96"))
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; Constructor
	  ;;
	  ;; Takes the command and an optional status-message.
    (let ((package-constructor
	    (lambda (command status-message)
	      (.setup this command status-message (xml (:vpl command (:session-id our-session-id))))
	    ))
	  )
      ;; Inherit from Packaged-command-base
      (set (@ package-constructor prototype) (new Packaged-command-base))
      
      ;; Static functions
      (define-class-method (package-constructor set-session-id) (session-id)
	(set our-session-id (or session-id (get-session-id))))
      (define-class-method (package-constructor set-session-counter) (counter))

      ;; Return the Packaged-command-basic class object
      package-constructor
    ))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packaged Command Extended
;; 
;; Author: Johnny Casey
;;
;; A class to represent an extended server command
(defvar Packaged-command-extended
  ;; This relies on let creating a closure
        ;; Static variables
  (let ((our-session-id "96")
	(our-session-counter "0")
	)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; Constructor
	  ;;
	  ;; Takes the command and an optional status-message.
    (let ((package-constructor
  	    (lambda (command status-message)
	      (.setup this command status-message (xml (:vpl command 
	    						 (:list
							   (:session-id our-session-id)
							   (:session-counter our-session-counter)
							 )
						       )))
	    ))
	  )
      ;; Inherit from Packaged-command-base
      (set (@ package-constructor prototype) (new Packaged-command-base))
    
      ;; Static functions
      (define-class-method (package-constructor set-session-id) (session-id)
	(set our-session-id (or session-id (get-session-id))))
      (define-class-method (package-constructor set-session-counter) (counter)
        (set our-session-counter (or (+ "" counter) our-session-counter)))

      ;; Return the Packaged-command class object
      package-constructor
    ))
)


;; Default to basic commands for now
(defvar Packaged-command Packaged-command-basic)


(.initialize client-settings "extended-sessionid-info"
  ;; Set the current value
  (eq Packaged-command Packaged-command-extended)
  ;; Update the Packaged-command variable on change
  (lambda (value)
    (set Packaged-command (or (and value Packaged-command-extended)
    			      Packaged-command-basic
			      ))
    ;; Ensure the session-id is updated
    (.set-session-id Packaged-command)
    (or value false)
  ))


(defun package-simple-command (command status-message)
  ;;; Generalized function taking a command to send to the server.
  ;;; These commands take no arguments.  Called by other package functions.
  (.send (new Packaged-command command status-message))
)


(defun package-code (command code status-message)
  ;; Generalized function taking a command and a code to send to the server.
  ;; These commands take only a numeric code.
  (.send (.append-number (new Packaged-command command status-message) code))
)


(defun package-binary-command (command arg1 arg2 status-message)
  ;;; Generalized function taking a command and some common arguments to send to the server.
  ;; These commands take two numeric arguments.
  (.send (.append-number (.append-number (new Packaged-command command status-message) arg1) arg2))
)


(defun package-binary-text-command (command arg1 arg2 status-message)
  ;;; Generalized function taking a command and some common arguments to send to the server.
  ;; These commands take two string arguments.
  (.send (.append-string (.append-string (new Packaged-command command status-message) arg1) arg2))
)


(defun package-input-command (command jbml box-id value status-message)
  ;;; Generalized function taking a jbml box with a value to send to the server.
  ;; These commands take a box-id and a structure describing the value.
  (.send (.append-jbml-value (.append-number (new Packaged-command command status-message) box-id) jbml value))
)


(defun package-vpl-version ()
  (.send (.append-string (new Packaged-command "vpl-version") vpl-version))
)


(defun package-click-workspace ()
  (.close-menus Menu)
  (package-simple-command "click-workspace" "Clicked on Workspace.")
)


(defun package-click-results ()
  (.close-menus Menu)
  (package-simple-command "click-results" "Clicked on Results.")
)


(defun package-clear-workspace ()
  ;;; Sends a 'clear-workspace' message to the server.
  ;;; Currently sent when the user selects the clear workspace icon.
  (package-update-open-box-contents (.get-tracked-objects Jbml-tracked (.get-element-by-id document "workspace")))
  (package-simple-command "clear-workspace" "Clear Workspace.")
)


(defun package-clear-results ()
  ;;; Sends a 'clear-results' message to the server.
  ;;; Currently sent when the user selects the clear results icon.
  (package-update-open-box-contents (.get-tracked-objects Jbml-tracked (.get-element-by-id document "results")))
  (package-simple-command "clear-results" "Clear Results.")
)


(defun package-left-undo-click ()
  ;;; Sends a 'left-undo' message to the server.
  ;;; Currently sent when the user selects the undo workspace icon.
  (package-update-open-box-contents (.get-tracked-objects Jbml-tracked (.get-element-by-id document "workspace")))
  (package-simple-command "left-undo" "Undo.")
)


(defun package-right-redo-click ()
  ;;; Sends a 'right-redo' message to the server.
  ;;; Currently sent when the user selects the redo workspace icon.
  (package-simple-command "right-redo" "Redo.")
)


(defun package-menu-mouse-click (menu-id title)
  ;; Sends a 'palettemenumouseclick' message containing a menuentry code to the server.
  (package-code "palettemenumouseclick" menu-id (+ "Selected " (or title "Palette Menu") "."))
)


(defun package-boxmenu-mouse-click (menu-id box-id title)
  ;; Sends a 'boxmenumouseclick' message containing a menuentry code and box ID code to the server.
  (package-binary-command "boxmenumouseclick" menu-id box-id (+ "Clicked " (or title "Box Menu") "."))
)


(defun package-update-open-box-contents (boxes)
  (when (@ boxes length)
    (.send (.append-jbml-objects (new Packaged-command "update-open-box-contents" "Updated Box Contents.") Jbml-tracked boxes))
  )
)


(defun package-input-text (box-id value)
  ;;; Sends an 'input-text' message containing a box ID and value to the server.
  ;;; Currently sent when an open hole receives a carriage return.
  (package-update-open-box-contents (.get-parent-tracked-objects Jbml-tracked (.get-element-by-id document box-id) null box-id))
  (package-input-command "input-text" Jbml-hole box-id value "Entered Input.")
)


(defun package-input-text-tab (box-id value)
  ;;; Sends an 'input-text-tab' message containing a box ID and value to the server.
  ;;; Currently sent when an open hole receives a tab.
  (package-update-open-box-contents (.get-parent-tracked-objects Jbml-tracked (.get-element-by-id document box-id) null box-id))
  (package-input-command "input-text-tab" Jbml-hole box-id value "Entered Input + Tab.")
)


(defun package-input-multiline-text (box-id value)
  ;;; Sends an 'input-multiline-text' message containing a box ID and a list of line values to the server.
  ;;; Currently sent when an open multiline hole receives a send signal.
  (package-update-open-box-contents (.get-parent-tracked-objects Jbml-tracked (.get-element-by-id document box-id) null box-id))
  (package-input-command "input-multiline-text" Jbml-multiline-hole box-id value "Entered Multiline Input.")
)


(defun package-input-multiline-text-tab (box-id value)
  ;;; Sends an 'input-multiline-text' message containing a box ID and a list of line values to the server.
  ;;; Currently sent when an open multiline hole receives a send signal.
  (package-update-open-box-contents (.get-parent-tracked-objects Jbml-tracked (.get-element-by-id document box-id) null box-id))
  (package-input-command "input-multiline-text-tab" Jbml-multiline-hole box-id value "Entered Multiline Input + Tab.")
)


(defun package-user-popup-input (id value)
  ;;; Sends an 'user-popup-input' message containing a dialog ID 
  ;;; and value to the server.
  ;;; Currently sent when a dialog is confirmed, through a buttor or enter key.
  (package-binary-text-command "user-popup-input" id value "Entered Popup Input.")
)


(defun package-test-click (test-IDcode)
  ;; Takes a test code, wraps it up in a wrapper, shoves it down the pipe.
  (package-code "testclick" test-IDcode)
)


;;; This just clutters up the console.  These commands are not
;;; processed by the server.  Please don't put them back until we
;;; need them.

(defun package-box-mouse-over (box-id)
  ;;; Sends a 'box-over' message containing a box ID to the server.
  ;;; Currently disabled.
#+not-needed
  (package-code "box-over" box-id)
)


;;; This just clutters up the console.  These commands are not
;;; processed by the server.  Please don't put them back until we
;;; need them.

(defun package-box-mouse-out (box-id)
  ;;; Sends a 'box-out' message containing a box ID to the server.
  ;;; Currently disabled.
#+not-needed
  (package-code "box-out" box-id)
)


(defun package-box-mouse-click (box-id mouse-code)
  ;; Sends a 'box-click' message to the server.
  ;; Disable all right-click messages to server.  Server treats them as
  ;; a noop now anyway.  No sense even sending them. -- JP
  (unless (eq (@ Event-library right-mouse-button) mouse-code)
    (package-update-open-box-contents (.get-tracked-objects Jbml-tracked (.get-element-by-id document box-id)))
    (package-binary-command "box-click" box-id mouse-code "Clicked Box.")
  )
)


(defun package-closed-hole-mouse-click (box-id mouse-code)
  ;; This routine is called when a closed hole box region is clicked.
  ;; It swaps the label out for a hole by itself locally.
  ;; ! Must turn itself off when opening hole!!

  ;;IF YOU CLICK ON A CLOSED HOLE, THEN IT AUTOEXPANDS INTO AN INPUTBOX *LOCALLY*.

  (unless (.get client-settings "new-hole-opening-mode")
    (let* ((box (.get-element-by-id document box-id))
    	   (setup (@ (.get-jbml Jbml box) open))
	   )
      (when setup
	(setup box "")
      )
    ))
  ;; There should be nothing to track if the hole is "closed"
  (package-binary-command "box-click" box-id mouse-code "Clicked Closed Hole.")
)


(defun package-box-clear-hole-mouse-click (box-id mouse-code)
  (unless (.get client-settings "new-hole-opening-mode")
    (let* ((box (.get-element-by-id document box-id))
    	   (setup (@ (.get-jbml Jbml box) close))
           )
      (when setup
	(setup box)
      )
    ))
  (package-update-open-box-contents (.get-tracked-objects Jbml-tracked (.get-element-by-id document box-id)))
  (package-binary-command "box-clear-hole-click" box-id mouse-code "Clear Hole.")
)


(defun package-box-mouse-double-click (box-id mouse-code)
  ;;; Sends a 'box-double-click' message containing a box ID and mouse button to the server.
  (package-update-open-box-contents (.get-tracked-objects Jbml-tracked (.get-element-by-id document box-id)))
  (package-binary-command "box-double-click" box-id mouse-code "Double Clicked Box.")
)


(defun package-box-icon-click (box-id icon-code)
  ;;; Sends an 'icon-click' message containing a box ID and icon code to the server.
  (package-update-open-box-contents (.get-parent-tracked-objects Jbml-tracked (.get-element-by-id document box-id)))
  (package-binary-command "icon-click" box-id icon-code "Clicked Icon.")
)


(defun package-box-delete-mouse-click (box-id mouse-code)
  ;;; Sends a 'box-delete-click' message containing a box ID and mouse button to the server.
  (package-update-open-box-contents (.get-parent-tracked-objects Jbml-tracked (.get-element-by-id document box-id)))
  (package-binary-command "box-delete-click" box-id mouse-code "Clicked Delete Box.")
)


(defun package-box-clear-mouse-click (box-id mouse-code)
  ;;; Sends a 'box-clear-click' message containing a box ID and mouse button to the server.
  (package-update-open-box-contents (.get-parent-tracked-objects Jbml-tracked (.get-element-by-id document box-id)))
  (package-binary-command "box-clear-click" box-id mouse-code "Clicked Clear Box.")
)


(defun package-box-clear-delete-mouse-click (box-id mouse-code)
  ;;; Sends a 'box-clear-delete-click' message containing a box ID and mouse button to the server.
  (package-update-open-box-contents (.get-parent-tracked-objects Jbml-tracked (.get-element-by-id document box-id)))
  (package-binary-command "box-clear-delete-click" box-id mouse-code "Clicked Clear + Delete Box.")
)


(defun package-dnd-box (source-id box-id source-box)
  (package-update-open-box-contents (.get-tracked-objects Jbml-tracked source-box (.get-parent-tracked-objects Jbml-tracked (.get-element-by-id document box-id))))
  (package-binary-command "dnd-box" source-id box-id
    (+ "Dropped " (.get-box-name Jbml source-box) " on " (.get-box-name Jbml (.get-element-by-id document box-id))))
)


(defun package-dnd-box-after (source-id box-id source-box)
  (package-update-open-box-contents (.get-tracked-objects Jbml-tracked source-box (.get-parent-tracked-objects Jbml-tracked (.get-element-by-id document box-id))))
  (package-binary-command "dnd-box-after" source-id box-id
    (+ "Dropped " (.get-box-name Jbml source-box) " after " (.get-box-name Jbml (.get-element-by-id document box-id))))
)


(defun package-dnd-box-before (source-id box-id source-box)
  (package-update-open-box-contents (.get-tracked-objects Jbml-tracked source-box (.get-parent-tracked-objects Jbml-tracked (.get-element-by-id document box-id))))
  (package-binary-command "dnd-box-before" source-id box-id
    (+ "Dropped " (.get-box-name Jbml source-box) " before " (.get-box-name Jbml (.get-element-by-id document box-id))))
)


(defun package-global-keypress (which-key  ctrl-key)
  ;;; Sends a 'global-keypress' message containing a key and control code to the server.
  ;; NOTE: for historical reasons there is no N in ctrl-key!!!
  (.send (.append-string (.append-number (new Packaged-command "global-keypress" "Pressed Global Key.") which-key) ctrl-key))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HANDLER METHODS FOR INCOMING DISPATCH
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "change-client-settings" message code

(defun xml-unpack-handle-change-client-settings (xml-message)
  ;;; Sets values in the client-settings object
  (let ((settings-node (@ xml-message first-child first-child)))
    ;;; If all settings are inside a single list, we need to open that up too
    ;;(set settings-node (@ settings-node first-child))
    (while settings-node
      (let ((setting-node (@ settings-node first-child)))
	(when (and setting-node
		   (.get-text-content Dom-library setting-node))
	  (let ((setting (.normalize-name client-settings (.get-text-content Dom-library setting-node)))
		(value-node (@ setting-node next-sibling))
		)
	    (.set client-settings setting (and value-node (.get-text-content Dom-library value-node)))
	  )))
      (set settings-node (@ settings-node next-sibling))
    ))
)


(defun xml-unpack-handle-make-menus (xml-message)
  (let ((menu-node (@ xml-message first-child)))
    (while menu-node
      (attach-jbml-menu (@ menu-node first-child) false)
      (set menu-node (@ menu-node next-sibling))
    )
  )
)


(defun xml-unpack-handle-makemenu (xml-message replace-old-boxnode)
  ;;; This routine is given a top-level node of type makemenu.  It unpacks and creates the menu.
  ;; USED FOR PALETTE N-LEVEL DDMX MENUS.
  ;; (MAKEMENU (LIST (SYMBOL:MENU (TEXT:MENU)) (STRING:MenuTitle (TEXT:MenuTitle))
  ;;                 (LIST (SYMBOL:MENU (TEXT:MENU)) (STRING:SubMenuTitle (TEXT:SubMenuTitle)) (LIST (SYMBOL:MENUENTRY))))
  ;;                 (LIST (SYMBOL:MENUENTRY (TEXT:MENUENTRY)) (STRING:READF (TEXT:READF)) (NUMBER:1 (TEXT:1))))
  ;; The last parameter is a boolean that tells whether to replace the old palette menu
  ;; that is already there (true) or whether we're simply adding a new menu entry (false).

  (let ((menu-node (@ xml-message first-child)))
    (attach-jbml-menu (@ menu-node first-child) replace-old-boxnode)
  )
)


(defun attach-jbml-menu (menu-node replace-menu)
  (let* ((menu-id-node (@ menu-node next-sibling))
  	 (menu-id (.get-text-content Dom-library menu-id-node))
	 (replaced (and replace-menu (.get-element-by-id document menu-id)))
	 (container (or (and replaced (@ replaced parent-node))
	 		(.get-element-by-id document "palette")
			))
	 (menu (expand-jbml-menu menu-node))
	 )
    ;; All done.  Stick the resulting menu onto the palette area. ...do this last so it doesn't spaz?
    ;;Hang box off of super.  Are we REPLACING?
    (if replaced
      (replacing-child container menu replaced)
      ;else
      (.append-child container menu))

    (.quickinit (new Menu menu-id))
  )
)


(defun expand-jbml-menu (menu-node)
  (let* ((menu-id-node (@ menu-node next-sibling))
	 (title-node (@ menu-id-node next-sibling))
	 (color-node (@ title-node next-sibling))
	 (menu-entry-node (@ color-node next-sibling))
	 )
    (let ((menu-id (.get-text-content Dom-library menu-id-node))
	  (title (.get-text-content Dom-library title-node))
    	  (menu-class (+ "palette-menu menu menu-centered-text " (or (.get-text-content Dom-library color-node) "")))
	  (entries (.make-container Menu))
	  )
      (expand-jbml-submenu menu-entry-node (.open-container Menu entries) 0) ;; 0 for palette...
      (html ((:div :id menu-id :class menu-class)
	      ((:a :class "menu-section-header menu-header" :href "javascript:void(0)")
		title)
	      entries
	    ))))
)


(defun expand-jbml-submenu (menu-entry-node menu box-id parent-title)
  ;;; Eats up a sequence of nodes for palette multi-level menus.
  ;;Called with a sequence of lists at the top level.
  ;;themenu is a DIV of class "menu-section".
  (let ((box-id (or box-id 0))
	(parent-title (or parent-title ""))
	)
    (while menu-entry-node                                   ;;sequence of Lists.
      (let ((jbml-or-menu-id-node (@ menu-entry-node first-child)))
        (case (.get-text-content Dom-library jbml-or-menu-id-node)
          ("JBML-MENUENTRY"     ;;Old style palette menus.  Duplicate code changes below.
	    (let* ((title-node (@ jbml-or-menu-id-node next-sibling))
	    	   (menu-id-node (@ title-node next-sibling))
		   )
	      (let ((title (.get-text-content Dom-library title-node))
	      	    (menu-id (.get-text-content Dom-library menu-id-node))
		    )
	        (create-jbml-menu-entry menu title menu-id parent-title)
	      )))
          ("JBML-MENU-ENTRY"   ;;New style main-menu icons. Sigh.  Duplicate code changes above.
	    (let* ((title-node (@ jbml-or-menu-id-node next-sibling))
	    	   (menu-id-node (@ title-node next-sibling))
		   )
	      (let ((title (.get-text-content Dom-library title-node))
	      	    (menu-id (.get-text-content Dom-library menu-id-node))
		    )
	        (create-jbml-menu-entry menu title menu-id parent-title)
	      )))
          ("JBML-MENU"
	    (let* ((title-node (@ jbml-or-menu-id-node next-sibling))
	    	   (submenu-node (@ title-node next-sibling))
		   )
	      (let ((title (.get-text-content Dom-library title-node)))
	        (create-jbml-menu menu title submenu-node box-id parent-title)
	      )))
	  (t
	    (let* ((jbml-node (@ jbml-or-menu-id-node next-sibling))
	    	   (title-node (@ jbml-node next-sibling))
		   )
	      (let ((menu-id (.get-text-content Dom-library jbml-or-menu-id-node))
	      	    (title (.get-text-content Dom-library title-node))
		    )
	        (case (.get-text-content Dom-library jbml-node)
		  ("JBML-MENUENTRY"     ;;Old style palette menus.  Duplicate code changes below.
		    (create-jbml-boxmenu-entry menu title menu-id box-id parent-title)
		  )
		  ("JBML-MENU-ENTRY"   ;;New style main-menu icons. Sigh.  Duplicate code changes above.
		    (create-jbml-boxmenu-entry menu title menu-id box-id parent-title)
		  )
		  ("JBML-MENU"       ;;Untested.  Should work.
		    (let ((submenu-node (@ title-node next-sibling)))
		      (create-jbml-menu menu title submenu-node box-id parent-title)
		    ))
		  (t true)
		  )
		)))))
      ; done, so...now iterate on sequence.
      (set menu-entry-node (@ menu-entry-node next-sibling))        ;;LIST   (MENUENTRY ...
    )     ;end of while menu-entry-node lists
  )
)


(defun create-jbml-menu-entry (menu title menu-id parent-title)
  (let ((entry (html ((:a :class "menu-section-header menu-subheader" :href "javascript:void(0)") title))))
    (set (@ entry onclick) (make-menu-click-handler menu-id title))
    (.append-child menu entry)
  )
)


(defun create-jbml-boxmenu-entry (menu title menu-id box-id parent-title)
  (let ((entry (html ((:a :class "menu-section-header menu-subheader" :href "javascript:void(0)") title))))
    (set (@ entry onclick) (make-boxmenu-click-handler menu-id box-id title))
    (.append-child menu entry)
  )
)


(defun create-jbml-menu (menu title menu-entries box-id parent-title)
  (let ((entries (.make-container Menu)))
    (expand-jbml-submenu menu-entries (.open-container Menu entries) box-id (+ parent-title title " >> "))
    (.append-child menu (html ((:a :class "menu-section-header menu-subheader menu-arrow" :href "javascript:void(0)")
    				title
				(:img :src "arrow1.gif" :width "10px" :height "12px" :alt " ")
				)))
    (.append-child menu entries)
  )
)


(defun xml-unpack-handle-remove-menu (xml-message)
  (let* ((menu-id-node (@ xml-message first-child))
  	 (menu-id (.get-text-content Dom-library menu-id-node))
	 (removed (.get-element-by-id document menu-id))
	 )
    (when removed
      (removing-child (@ removed parent-node) removed)
    )
  )
)


(defun xml-unpack-handle-redraw-box (xml-message)
  ;;; This routine is given a top-level node of type redraw-box.  It unpacks and replaces a box on the workspace.
  ;; (ADD-WORKSPACE (LIST (NUMBER:1 (TEXT:1)) (SYMBOL:SYM (TEXT:SYM))
  ;;                      (LIST (NUMBER:1 (TEXT:1)) (SYMBOL:MENUENTRY (TEXT:MENUENTRY)) ...

                                                   ;;ADD-WORKSPACE  (LIST  (NUMBER ...
  (let* ((xml-node (@ xml-message first-child))   ;;LIST  (NUMBER ...
	 (sub-node (@ xml-node first-child))         ;;NUMBER  1
	 (box-id (.get-text-content Dom-library sub-node))
	 (parent (.get-element-by-id document "workspace") )  ;;backup in case not there already.
	 (old-node (.get-element-by-id document box-id))
	 (subbox "")
	 )
    (when old-node
    (set parent (@ old-node parent-node)))

    ;;Note that deleting and then adding will shove it onto end, not what we want.
    (set subbox (recursively-display-list-to-space xml-node parent false "0" old-node))  ;;swap out (last code)

    (scroll-jbml-box-into-view subbox)   ;;Could be problem for results or palette??
  )
)


(defun xml-unpack-handle-focus-box (xml-message)
  ;;; This routine is given a top-level node of type redraw-box.  It unpacks and replaces a box on the workspace.
  ;; (ADD-WORKSPACE (LIST (NUMBER:1 (TEXT:1)) (SYMBOL:SYM (TEXT:SYM))
  ;;                      (LIST (NUMBER:1 (TEXT:1)) (SYMBOL:MENUENTRY (TEXT:MENUENTRY)) ...

  (let* ((xml-node (@ xml-message first-child))
	 (sub-node (@ xml-node first-child))
	 (box-id (.get-text-content Dom-library sub-node))
	 (focus-node (.get-element-by-id document box-id))
	 )
    (when focus-node
      (scroll-jbml-box-into-view focus-node)
    ))
)


(defun xml-unpack-handle-popup-URL-window (xml-message)
  ;;; This routine is given a URL.  It pops up a window for the URL.
  ;; (POP-UP-WINDOW (STRING:URL (TEXT:URL))
					        ;;POPUP-URL-WINDOW  (LIST (STRING URL) (STRING DATASTRING) (STRING REL-P))
  (let* ((list-node (@ xml-message first-child))   ;;LIST (STRING URL
  	 (xml-node-window-id (@ list-node first-child))
	 (xml-node-url (@ xml-node-window-id next-sibling))    ;; STRING URL
	 (xml-node-specs (@ xml-node-url next-sibling))
	 (xml-node-relative-p (@ xml-node-specs next-sibling))
	 )
    (let ((window-id (.get-text-content Dom-library xml-node-window-id))
    	  (url (.get-text-content Dom-library xml-node-url))
	  (specs (.get-text-content Dom-library xml-node-specs))
	  (relative-p (parse-int (.get-text-content Dom-library xml-node-relative-p)))
          )
      (popup-URL-window window-id url specs relative-p)
    ))
)


(defun xml-unpack-handle-add-palette-button (xml-message)
  ;;; This routine is given a top-level node of type add-palette-button.  It unpacks and creates a button on the palette.
  ;; (ADD-PALETTE-BUTTON (LIST (NUMBER:1 (TEXT:1)) (SYMBOL:SYM (TEXT:SYM))
  ;;(MAKEMENU (LIST (SYMBOL:MENU (TEXT:MENU)) (STRING:MenuTitle (TEXT:MenuTitle))
  ;;                      (LIST (NUMBER:1 (TEXT:1)) (SYMBOL:MENUENTRY (TEXT:MENUENTRY)) ...
						    ;;ADD-WORKSPACE  (LIST  (NUMBER ...
  (let ((xml-node (@ xml-message first-child))   ;;LIST  (NUMBER ...
        (workspace  (.get-element-by-id document "workspace") )
        )
    (recursively-display-list-to-space xml-node workspace false "0" false)
  )
)


(defun xml-unpack-handle-add-workspace-box (xml-message)
  ;;; This routine is given a top-level node of type add-workspace.  It unpacks and creates a box on the workspace.
  ;; (ADD-WORKSPACE (LIST (NUMBER:1 (TEXT:1)) (SYMBOL:SYM (TEXT:SYM))
  ;;                      (LIST (NUMBER:1 (TEXT:1)) (SYMBOL:MENUENTRY (TEXT:MENUENTRY)) ...

                                                   ;;ADD-WORKSPACE  (LIST  (NUMBER ...
  (let ((xml-node (@ xml-message first-child))   ;;LIST  (NUMBER ...
        (workspace  (.get-element-by-id document "workspace") )
        )
    (recursively-display-list-to-space xml-node workspace false "0" false)
    (update-toolboxes) ;; TODO Use something better
  )
)


(defun xml-unpack-handle-add-results-box (xml-message)
  ;;; This routine is given a top-level node of type add-results.  It unpacks and creates a box on the results.
  ;; (ADD-WORKSPACE (LIST (NUMBER:1 (TEXT:1)) (SYMBOL:SYM (TEXT:SYM))
  ;;                      (LIST (NUMBER:1 (TEXT:1)) (SYMBOL:MENUENTRY (TEXT:MENUENTRY)) ...

                                                   ;;ADD-WORKSPACE  (LIST  (NUMBER ...
  (let ((xml-node (@ xml-message first-child))   ;;LIST  (NUMBER ...
	(space (.get-element-by-id document "results"))
	(results-message (.get-element-by-id document "results-message"))
	)
    ;;Hack to clear out msg, if it's still hanging around.
    (when results-message
      (clear-results)
    )

    (recursively-display-list-to-space xml-node space false "0" false true)
    (update-toolboxes) ;; TODO Use something better
  )
)


(defun recursively-display-list-to-space (xml-node space menu-mode superbox-id replaced-box do-prepend)
  (let ((box (attach-jbml-boxes xml-node space superbox-id menu-mode replaced-box do-prepend)))
    box
  )
)


(defun attach-jbml-boxes (xml-node parent-box superbox-id menu-mode replaced-box do-prepend)
  (let* ((box-id-node (@ xml-node first-child))
	 (box-id (.get-text-content Dom-library box-id-node))
	 (jbml-node (@ box-id-node next-sibling))
	 (box (.create-box Jbml box-id))
	 (child (.create-box-container Jbml box parent-box))
	 )
    ;;Hang box off of super.  Are we REPLACING?
    (cond
      (replaced-box
	(.replacing-child Jbml parent-box child replaced-box)
      )
      ((and (.has-child-nodes parent-box)
	;; If the list we are working on has Main Box Menu as its first atom,
	;; then push the new box onto the front of the space's list.
	    (or do-prepend ;; Are we prepending the box?
	        (eq (.get-text-content Dom-library jbml-node) "JBML-MAIN-BOX-MENU")) ;;Is it a Main Menu?
	    )
	;push onto front of superbox, no matter what.  Takes care of Open Holes, which have
	;already added an Input box at the beginning.  We want Main Menu to come before Input box.
	(.insert-before parent-box child (@ parent-box first-child))
      )
      (true
;else, regular addition; append onto back of list.
	(.append-child parent-box child)
      )
    )
    (expand-jbml-boxes box box-id jbml-node superbox-id menu-mode)
  )
)


(defun expand-jbml-boxes (box box-id xml-node superbox-id menu-mode)
  ;;; Given a JBML list (xml nodes format), displays results to given space.
  ;;This hack is necessary to return the enclosing box of the enclosing menu of the menuentry.
  ;; (LIST (NUMBER:1 (TEXT:1)) (SYMBOL:SYM (TEXT:SYM))
  ;;       (LIST (NUMBER:1 (TEXT:1)) (SYMBOL:MENUENTRY (TEXT:MENUENTRY)) ...

  (let (
         (current-word false)
         (box-text-justify "left")
         (themenu "")
         (input false)
	 (name false)
         )
    (when (.get client-settings "default-jbml-dnd-drag")
      (.enable-drag Jbml box)
    )
    (when (.get client-settings "default-jbml-dnd-drop")
      (.enable-drop Jbml box)
    )

    (while xml-node

      ;;Because we're eating an entire list from open paren to close paren,
      ;;if we get into something special like a new options-menu
      ;;we should simply return after finishing.  I think.  Unless the input is sequential flat,
      ;;not hierarchical.

      ;;Have we run into a sublist, or do we simply process an atom in the current list?
      (if (string= (@ xml-node node-name) "list")  ;;was LIST, didn't work!
        ;;DAMN.  node-names have to be lower case; text-content has to be upper case.  go figure.
        (if menu-mode    ;;...is this sublist a menu-entry inside a classic pull-down menu definition?
          (eat-pulldown-menuentry xml-node themenu superbox-id)
          ;else
          (attach-jbml-boxes xml-node box box-id))

	;else
	(case (.get-text-content Dom-library xml-node)

	  ("JBML-B"	;; Applied to words, bold
	    (set (@ (or current-word box) style font-weight) "bold")
	  )
	  ("JBML-I"	;; Applied to words, italic
	    (set (@ (or current-word box) style font-style) "italic")
	  )
	  ("JBML-UL"	;; Applied to words, underline
	    (set (@ (or current-word box) style text-decoration) "underline")
	  )
	  ("JBML-COURIER"                                          ;;  Courier font
	    (set (@ (or current-word box) style font-family) "'Courier New', Courier, monospace")
	  )

	  ("JBML-LEFT-JUSTIFY"
	    (set box-text-justify (set (@ box style text-align) "left"))
	  )
	  ("JBML-CENTER-JUSTIFY"
	    (set box-text-justify (set (@ box style text-align) "center"))
	  )
	  ("JBML-RIGHT-JUSTIFY"
	    (set box-text-justify (set (@ box style text-align) "right"))
	  )

	  ("JBML-THICK"
	    (.set-border-width Jbml box "jbml-thick")
	  )
	  ("JBML-MEDIUM"
	    (.set-border-width Jbml box "jbml-medium")
	  )
	  ("JBML-THIN"
	    (.set-border-width Jbml box "jbml-thin")
	  )

	  ("JBML-NO-OUTLINE"
	    (.set-border-style Jbml box "jbml-no-outline")
	    (.disable-drag Jbml box)
	  )
	  ("JBML-DOTTED"
	    (.set-border-style Jbml box "jbml-dotted")
	  )
	  ("JBML-DOTTED-BLINK"
	    (dotted-blinkify-box box)
	  )

	  ("JBML-BOX-COLOR"
	    (let ((color-node (@ xml-node next-sibling)))
	      (.set-border-color Jbml box (.get-text-content Dom-library color-node))
	      (set xml-node color-node)
	    )
	  )
	  ("JBML-COLOR"	;; Applied to words
	    (let ((color-node (@ xml-node next-sibling)))
	      (set (@ (or current-word box) style color) (.get-text-content Dom-library color-node))
	      (set xml-node color-node)
	    )
	  )
	  ("JBML-BACKGROUND-COLOR"
	    (let ((color-node (@ xml-node next-sibling)))
	      (set (@ box style background-color) (.get-text-content Dom-library color-node))
	      (set xml-node color-node)
	    )
	  )

	  ("JBML-BUTTON"
	    (set (@ box style background-color) "#880")
	  )

	  ("JBML-OUTDENT"
	    ;; FIXME!
	    "FIXME: Unimplemented"
	  )

	  ("JBML-CR"
	    (.append-child box (html ((:div :class "cr-jbml"))))
	  )

	  ("JBML-DOTDOTDOT"
	    (.append-child box (html ((:div :class "a-jbml jbml-dotdotdot") ". . .") ))
	  )

	  ("JBML-EXEC-ICON"
	    (.append-child box (html ((:img :class "icon-jbml jbml-exec-icon" :width "22" :height "22" :src "greenarrowright_button_22x22.gif"))))
	  )
	  ("JBML-GO-ICON" 
	    (.append-child box (html ((:img :class "icon-jbml jbml-go-icon" :width "16" :height "16" :src "whitearrowgreen_16x16.gif")) ))
	  )
	  ("JBML-CLOSE-ICON"
	    (.append-child box (html ((:img :class "icon-jbml jbml-close-icon" :width "16" :height "16" :src "redX.jpg")) ))
	  )
	  ("JBML-MENU-ICON"
	    (.append-child box (html ((:img :class "icon-jbml jbml-menu-icon" :width "16" :height "16" :src "greenarrowdown_16x16.gif"))))
	  )
	  ("JBML-ICON"
	    (.append-child box (html ((:img :class "icon-jbml jbml-icon" :width "22" :height "22" :src "greenarrowright_button_22x22.gif"))))
	  )

	  ("JBML-DELETE"
	    (.attach-icon Jbml-icon-delete box)
	  )
	  ("JBML-CLEAR"
	    (.attach-icon Jbml-icon-clear box)
	  )
	  ("JBML-CLEAR-DELETE"
	    (.attach-icon Jbml-icon-clear-delete box)
	  )

	  ("JBML-NAME"
	    (let ((name-node (@ xml-node next-sibling)))
	      (set name (.set-box-name Jbml box (.get-text-content Dom-library name-node)))
	      (set xml-node name-node)
	    )
	  )

	  ("JBML-DND-DRAG"
	    (.enable-drag Jbml box)
	  )
	  ("JBML-DND-NO-DRAG"
	    (.disable-drag Jbml box)
	  )
	  ("JBML-DND-DROP"
	    (.enable-drop Jbml box)
	  )
	  ("JBML-DND-NO-DROP"
	    (.disable-drop Jbml box)
	  )

	  ("JBML-INPUT-TEXT"
	    (.init-hole Jbml-input-text box)
	    (.open Jbml-input-text box "")
	  )

	  ("JBML-HOLE"     ;; This one is the Potential Hole, waiting to be clicked.
	    (.init-hole Jbml-hole box)
	    (.close Jbml-hole box)
	  )
	  ("JBML-HOLE-OPENED"
	    (.init-hole Jbml-hole box)
	    (set input (.open Jbml-hole box ""))
	  )

	  ("JBML-MULTILINE-HOLE"
	    (.init-hole Jbml-multiline-hole box)
	    (.close Jbml-multiline-hole box)
	  )
	  ("JBML-MULTILINE-HOLE-OPENED"
	    (let ((entries-node (@ xml-node next-sibling)))
	      (.init-hole Jbml-multiline-hole box)
	      (.open Jbml-multiline-hole box entries-node)
	      (set xml-node entries-node)
	    )
	  )

	  ("JBML-OPTIONS-MENU"                         ;;New green menu, appears at the end.
	    (let* ((title-node (@ xml-node next-sibling))
	    	   (entries-node (@ title-node next-sibling))
		   )
	      (.disable-drag Jbml box)
	      (create-jbml-options-menu box (.get-text-content Dom-library title-node) entries-node superbox-id)
	      (return "")  ;;blow away the rest of the submenus, so we don't redisplay them.
	    )
	  )
	  ("JBML-OPTIONS-MENU2"
	    (let* ((title-node (@ xml-node next-sibling))
		   (entries-list-node (@ title-node next-sibling))
		   (options-list-node (@ entries-list-node next-sibling))
		   )
	      (let ((option-node (@ options-list-node first-child))
		    (icon-src "")
		    )
	        (while option-node
		  (case (.get-text-content Dom-library option-node)
		    ("ICON"
		      (let ((icon-node (@ option-node next-sibling)))
			(set icon-src (.get-text-content Dom-library icon-node))
			(set option-node icon-node)
		      )
		    )
		    (t)
		  )
		  (set option-node (@ option-node next-sibling))
		)
		(.disable-drag Jbml box)
		(create-jbml-options-menu box (.get-text-content Dom-library title-node) (@ entries-list-node first-child) superbox-id icon-src)
		(set xml-node options-list-node)
	      )
	    )
	  )

	  ("JBML-PULLDOWN-MENU"                                ;;CLASSIC Old school style.
	    ;;"Pull-down Menu" is an upper-level box modifier, can stand alone by itself.
	    (set (@ box style border-style) "none")  ;;make box enclosing pulldown menu invisible.
	    (set (@ box style margin) "0px")         ;;and shrink-wrap it.
	    (set menu-mode true)                     ;; and custom-suck the rest of this box, including title/menuenetries
	    (set themenu (html ((:select :class "menu" :name "Options Menu" :size 1))))
	    (.append-child box themenu)
	    (set (@ box onclick)     (@ Event-library stop-propagation))
	    (set (@ box ondblclick)  (@ Event-library stop-propagation))
	    (set (@ box oncontextmenu) (@ Event-library stop-propagation))
	    (set (@ box onmousedown)   (@ Event-library stop-propagation))
	  )

	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ("JBML-MAIN-BOX-MENU"                           ;;Appears in the upper left.
	    (let* ((title-node (@ xml-node next-sibling))
	    	   (entries-node (@ title-node next-sibling))
		   )
	      (.disable-drag Jbml box)
	      (create-jbml-main-box-menu box (.get-text-content Dom-library title-node) entries-node superbox-id)
	      (return "")  ;;blow away the rest of the submenus, so we don't redisplay them.
	    )
	  );;;;;;;;;;;;;;;;;END OF NEW BOX MENU;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          (t                            ;Otherwise, simply the text of the display node.
           (if menu-mode
             (let ((menutitle (.get-text-content Dom-library xml-node)))
               (.append-child themenu  (html ((:option :value "command-id 0") menutitle))  )
             )
             ;else
             (if input
               (let ((contents (.get-text-content Dom-library xml-node)))
		 (set (@ input value) contents)
		 (set (@ input size) (@ (+ contents " ") length))
		 (.track-object Jbml-hole input contents)
               )
               ;else
	       (let ((contents (.get-text-content Dom-library xml-node)))
                 (set current-word (html ((:div :class "a-jbml boxtext-jbml") contents) ))

	         (set (@ current-word style text-align) box-text-justify)

                 (.append-child box current-word)  ;Must insert word first before modifications.
		 (unless name
		   (set name (.set-box-name Jbml box contents))
		 )
               ) 
	     ) ;if input-mode
	   ) ;if menu-mode
          )  ;t
        )  ;case
      ) ;if
      (set xml-node (@ xml-node next-sibling))
    )  ;while

    box
  ) ;let*
)


(defun create-jbml-options-menu (box title menu-entries-node superbox-id icon-src)
  ;;Box is now the invisible box that surrounds the menu.  Hang everything off of this.
  (set (@ box style border-style) "none")  ;;Make box invisible.
  (set (@ box style background) "inherit")
  (set (@ box style margin) "0px")         ;;Shrink-wrap box to fit.
  (set (@ box onclick) (@ Event-library stop-propagation))
  ;; JP 01/28/07 -- Stopped dblclick from causing internal error
  (set (@ box ondblclick) (@ Event-library stop-propagation))

  (unless icon-src
    (set icon-src "greenarrowright_box.gif")
  )

  (let ((menu-id (+ "ddmenu" (++ DDmenuID)))
  	;; NOTE: DDMX menu classes will override other classes at the momement
  	(menu-icon (html ((:img :class "menu-section-header menu-header boxmenu" :src icon-src :border "0" :style "top: 0px; border-style: none; margin: auto;" )) ))
	(menu-title (html ((:div :class "menu-title-jbml" :style "clear: both;") title)))
	(entries (.make-container Menu))
	)
    (set (@ menu-icon onclick) (@ Event-library stop-propagation))

    (expand-jbml-submenu menu-entries-node (.open-container Menu entries) superbox-id)

    (let ((menu (html ((:div :id menu-id :class "menu-menu-jbml menu boxmenu menu-centered-text")
    			menu-icon
			entries
			menu-title
			)))
	  )
      (.append-child box menu)        ;;Menublock goes inline filled in larger long greater box
      (.quickinit (new Menu menu-id true))
    )
  )
)


(defun create-jbml-main-box-menu (box title menu-entries-node superbox-id)
  ;;BECAUSE MAIN-BOX-MENU IS A *SUBLIST*, IT ALREADY HAS ALLOCATED AN ADDITIONAL BOX THAT'S BEEN
  ;;HUNG OFF THE MAIN PARENT-BOX (NOW OUT OF SCOPE), NOW CALLED "BOX".  THEN THE MENU GOES INSIDE THIS.
  ;;Box is now the invisible box that surrounds the menu.  Hang everything off of this.
  (set (@ box style border-style) "none")  ;;Make box invisible.
  (set (@ box style margin) "0px")         ;;Shrink-wrap box to fit.
  (set (@ box style padding) "0px")         ;;Shrink-wrap box to fit.
  (set (@ box style background) "inherit")
  (set (@ box onclick) (@ Event-library stop-propagation))
  (set (@ box ondblclick) (@ Event-library stop-propagation))

  ;;BOX NEEDS TO BE GIVEN A MYMENUID.  BUT IT HAS AN ID ALREADY.  HUMH.
  ;; it's thus necessary to make yet another sub div to hold the menuID for the menu.
  (let ((menu-id (+ "ddmenu" (++ DDmenuID)))
  	;; NOTE: DDMX menu classes will override other classes at the momement
        (menu-icon (html ((:img :class "menu-section-header menu-header boxmenu" :width "14" :height "11" :src "greenarrowright_button_18x18.gif" :style "margin: 0px; margin-right: 3pt; vertical-align: bottom;"))))
        #+obsolete
	(menu-icon (html ((:img :class "menu-section-header menu-header boxmenu" :width "18" :height "18" :src "greenarrowright_button_18x18.gif" :style "margin: 0px;")) ))
	#+notyet
	(menu-icon (html ((:img :class "menu-section-header menu-header boxmenu" :width "17" :height "17" :src "foo.gif" :style "margin: 0px; top: 1pt; left: 1pt")) ))
	(menu-title (or (and title (html ((:div :class "menu-title-jbml" :style "clear: left;") title))) ""))
	(entries (.make-container Menu))
	)
    (set (@ menu-icon onclick) (@ Event-library stop-propagation))

    (expand-jbml-submenu menu-entries-node (.open-container Menu entries) superbox-id)

    (let ((menu (html ((:div :id menu-id :class "menu-menu-jbml menu boxmenu")
    			menu-icon
			entries
			)))
	  )
      (when menu-title
 	(.append-child menu menu-title)
      )
      (.append-child box menu)
      (.quickinit (new Menu menu-id true))
    )
  )
)


(defun eat-pulldown-menuentry (xml-node themenu superbox-ID)
  ;;; Called with a list of a single menuenty definition in the middle of a menu in the middle of a box.
  ;;Args are the beginning of the menu-entry, and the higher-level menu itself.
  (let* (
	 (xml-node (@ xml-node first-child))         ;;NUMBER  1
	 (menuentryID (.get-text-content Dom-library xml-node))
	 (contents "")
	 (default-text-color-for-box global-default-text-color)
	 ;;(current-word "")
	 )
    ;;eat the nodes.
    (while (set xml-node (@ xml-node next-sibling))        ;;STRING  "MenuTitle Name"

      ;;Have we run into a sublist, or do we simply process an atom in the current list?
      (if (string= (@ xml-node node-name) "list")  ;;was LIST, didn't work!
         ;;DAMN.  node-names have to be lower case; text-content has to be upper case.  go figure.
         ;else
         (progn
           ;Menuentry Recursion not supported yet.
         )
;;TO DO:  HANG CHILDREN OFF BOX'S ARRAY.

        ;else
        (case (.get-text-content Dom-library xml-node)
          ("JBML-MENU-ENTRY"                                        ;;  :b doesn't work, : got trimmed before XML pipe somehow.
             ;;good, it should be here.
          )
          (t                            ;Otherwise, simply use the text of the display node for menuentry.
            (let* ((menuentry-title (.get-text-content Dom-library xml-node))
                   (menuentry (html ((:option :value menuentryID) menuentry-title))  )
                   )
              (.append-child themenu  menuentry)
              (set (@ menuentry style color) default-text-color-for-box)
              (set (@ menuentry onclick) (make-menuentry-click-handler superbox-ID menuentry-title))  ;called on the MENU ENTRIES.
            )
            ;else
          )  ;t rue
        )  ;case
      ) ;if
    )  ;while
  )
)


(defun undotted-blinkify-box (box)
  (stop-animation box)
  (set (@ box style border-style) "")
  (set (@ box style border-width) "")
)


(defun dotted-blinkify-box (box)
  (stop-animation box)
  (set (@ box x-animation-state)  0)
  (set (@ box x-animation-timer)
    (set-interval
      (lambda ()
        (let ((border "solid")
	      (next-state (% (+ 1 (@ box x-animation-state)) 4))
	      )
          (case (@ box x-animation-state)
            (0     ;;Solid
            )
            (1     ;;Dashed
              (set border "dashed")
            )
            (2     ;;Dotted
              (set border "dotted")
            )
            (3     ;;Dashed
              (set border "dashed")
            )
            (t     ;;Solid
              (set next-state 0)
            ))
          (set (@ box x-animation-state) next-state)
	  (set (@ box style border-style) border)
        ))  ;end of lambda

      ;; 300 kinda frenetic, 400 brisk walk, 450 quiet walk, 500 too stately.
      ;;500 tends to be hypnotic and put me to sleep.  Very quiet.  Sucks too much attention.
      450))   ;;Msec.
)


(defun stop-animation (box)
  (when (@ box x-animation-timer)
    (clear-interval (@ box x-animation-timer))
  )
  (set (@ box x-animation-timer) null)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ROLLER ANIMATION;;;;;;;;;;;;;;;;;

(defun update-toolbox (toolbox-id box-id container-id)
  (let ((toolbox (.get-element-by-id document toolbox-id))
  	(box (.get-element-by-id document box-id))
	(container (.get-element-by-id document container-id))
	)
    (when (and toolbox box container)
      (set (@ toolbox style right) (+ (- (@ box client-width) (@ container client-width)) "px"))
    )
  )
)


(defun update-toolboxes ()
  (update-toolbox "workspace-toolbox" "workspace-box" "workspace")
  (update-toolbox "results-toolbox" "results-box" "results")
)


(defun set-ratio-previous (node ratio)
  (set (@ node x-ratio-previous) ratio)
)


(defun get-ratio-previous (node default-ratio)
  (let ((ratio (@ node x-ratio-previous)))
    (or (and (not (is-number ratio))
	     default-ratio
	     )
	ratio
	))
)


(defun get-split-workspace-results ()
  (let ((workspace-bottom (parse-int
	    (.get-style-property Dom-library (.get-element-by-id document "workspace-bigbox") "bottom")
	    ))
	(results-top (parse-int
	    (.get-style-property Dom-library (.get-element-by-id document "results-bigbox") "top")
	    ))
        )
    (/ results-top (+ results-top workspace-bottom))
  )
)


(defun set-split-workspace-results (ratio)
  (cond
    ((> ratio 1.0)
     (set ratio 1.0))
    ((< ratio 0.0)
     (set ratio 0.0)))
  (let (
  	(workspace-style (@ (.get-element-by-id document "workspace-bigbox") style))
        (results-style  (@ (.get-element-by-id document  "results-bigbox") style))
	(value (+ (* 100 ratio) "%"))
	(remaining (+ (* 100 (- 1.0 ratio)) "%"))
        )
    (set (@ workspace-style max-height) value)
    (set (@ workspace-style bottom) remaining)
    (set (@ results-style min-height) remaining)
    (set (@ results-style top) value)
    (update-toolboxes)
  )
)


(defun update-results-shade-expand ()
  (let ((ratio (get-split-workspace-results)))
    (display-element "results-shade" (< ratio 0.85))
    (display-element "results-expand" (> ratio 0.00))
  )
)


(defun handle-results-shade ()
  (let ((results (.get-element-by-id document "results-bigbox"))
  	(ratio (get-split-workspace-results))
	)
    (when (and results (< ratio 0.85))
      (set-ratio-previous results ratio)
      (set-split-workspace-results (or (and (>= ratio 0.70) 0.85) 0.70))
      (update-results-shade-expand)
    )
  )
)


(defun handle-results-expand ()
  (let ((results (.get-element-by-id document "results-bigbox"))
  	(ratio (get-split-workspace-results))
	)
    (when (and results (> ratio 0.00))
      (set-ratio-previous results ratio)
      ;; Use 0.00 last to avoid fall through with ||
      (set-split-workspace-results (or (and (>= ratio 0.85) 0.70) 0.00))
      (update-results-shade-expand)
    )
  )
)


(defun update-workspace-shade-expand ()
  (let ((ratio (get-split-workspace-results)))
    (display-element "workspace-shade" (not (< ratio 1.00)))
    (display-element "workspace-expand" (< ratio 1.00))
  )
)


(defun handle-workspace-shade ()
  (let ((results (.get-element-by-id document "results-bigbox"))
        (ratio (get-split-workspace-results))
	)
    (when (and results (not (< ratio 1.00)))
      (display-element "results-bigbox" true)
      ;; Use 0.85 first in case ratio-previous is 0.00 (probably never)
      (set-split-workspace-results (get-ratio-previous results 0.85))
      (set-ratio-previous results 1.00)
      (update-workspace-shade-expand)
    )
  )
)


(defun handle-workspace-expand ()
  (let ((results (.get-element-by-id document "results-bigbox"))
        (ratio (get-split-workspace-results))
	)
    (when (and results (< ratio 1.00))
      (display-element "results-bigbox" false)
      (set-split-workspace-results 1.00)
      (set-ratio-previous results ratio)
      (update-workspace-shade-expand)
    )
  )
)


;;;;;;;;;;;;;;;ERROR MODAL POP-UPS

(defun remove-popup-handler (e)
  (.remove-child (.get-element-by-id document "palette") this)
)


(defun create-display-error-modal-popup (message-string)
  ;; <br> comes through literally for some reason.
  ;; Go figure. This is ugly but works; will fix later.
  (let ((popup
         (html ((:div :class "modal-popup modal-popup-error")
                (:pre message-string) (:br) " (Click to get rid of window.)"))))
    (.append-child (.get-element-by-id document "palette") popup)
    (set (@ popup onclick) remove-popup-handler)
  )
)


(defun create-display-display-modal-popup (message-string)
  ;; <br> comes through literally for some reason.
  ;; Go figure. This is ugly but works; will fix later.
  (let ((popup
         (html ((:div :class "modal-popup modal-popup-normal")
                (:pre message-string) (:br) " (Click to get rid of window.)"))))
    (.append-child (.get-element-by-id document "palette") popup)
    (set (@ popup onclick) remove-popup-handler)
  )
)


(defun xml-unpack-handle-kill-window (xml-message)
  ;;; This routine is given a kill-window ID.  It kills the window, if there.
  ;; KILL-WINDOW  of NUMBER:1
                                                   ;;HILIGHT-BOX  (NUMBER ...
  (let* ((xml-node (@ xml-message first-child))   ;;(NUMBER 1 ...
	 (windowID (.get-text-content Dom-library xml-node))
	 )
    (kill-window-id windowID)
  )
)


(defun xml-unpack-handle-query-open-box-contents (xml-message)
  ;; This routine is given a list of box-ids or an empty list.
  (let ((xml-node (@ xml-message first-child))
	(boxes (new Array))
	)
    (unless xml-node
      (.get-tracked-objects Jbml-tracked (.get-element-by-id document "workspace") boxes)
    )
    (while xml-node
      (.get-tracked-objects Jbml-tracked (.get-element-by-id document (.get-text-content Dom-library xml-node)) boxes)
      (set xml-node (@ xml-node next-sibling))
    )
    (package-update-open-box-contents boxes)
  )
)


;;;;;;;;;;;;;;;HILIGHT

(defun xml-unpack-handle-hilight-box (xml-message)
  ;;; This routine is given a .  It .
  ;; HILIGHT-BOX  of NUMBER:1
                                                   ;;HILIGHT-BOX  (NUMBER ...
  (let* ((xml-node (@ xml-message first-child))   ;;(NUMBER 1 ...
	 (box-id (.get-text-content Dom-library xml-node))
	 )
    (hilight-box-ID box-id)
  )
)


(defun xml-unpack-handle-unhilight-box (xml-message)
  ;;; This routine is given a .  It unhilights the box.
  ;; (HILIGHT-BOX (NUMBER:1)
                                                   ;;HILIGHT-BOX  (NUMBER ...
  (let* ((xml-node (@ xml-message first-child))   ;;(NUMBER 1 ...
	 (box-id (.get-text-content Dom-library xml-node))
	 )
    (unhilight-box-ID box-id)
  )
)


(defun xml-unpack-handle-flashing-hilight-box (xml-message)
  ;;; This routine is given a .  It .
  ;; HILIGHT-BOX  of NUMBER:1
                                                   ;;HILIGHT-BOX  (NUMBER ...
  (let* ((xml-node (@ xml-message first-child))   ;;(NUMBER 1 ...
	 (box-id (.get-text-content Dom-library xml-node))
	 )
    (flashing-hilight-box-ID box-id)
  )
)


(defun xml-unpack-handle-unflashing-hilight-box (xml-message)
  ;;; This routine is given a .  It unhilights the box.
  ;; (HILIGHT-BOX (NUMBER:1)
                                                   ;;HILIGHT-BOX  (NUMBER ...
  (let* ((xml-node (@ xml-message first-child))   ;;(NUMBER 1 ...
	 (box-id (.get-text-content Dom-library xml-node))
	 )
    (unflashing-hilight-box-ID box-id)
  )
)


(defun xml-unpack-handle-error-message (xml-message)
  ;;; This routine is given an error message xml structure.  It displays the results.
  ;; (ERROR-MESSAGE (STRING: MSG)
                                                   ;;ERROR-MESSAGE  (STRING ...
  (let* ((xml-node (@ xml-message first-child))   ;;(STRING 1 ...
	 (error-message (.get-text-content Dom-library xml-node))
	 )
    (create-display-error-modal-popup error-message)
  )
)


(defun xml-unpack-handle-display-message (xml-message)
  ;;; This routine is given a display message xml structure.  It displays the results.
  ;; (DISPLAY-MESSAGE (STRING: MSG)
                                                   ;;ERROR-MESSAGE  (STRING ...
  (let* ((xml-node (@ xml-message first-child))   ;;(STRING 1 ...
	 (error-message (.get-text-content Dom-library xml-node))
	 )
    (create-display-display-modal-popup error-message)
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "show-status" message code

(defun show-status (text status-id prefix)
  ;;; Displays a message to a status node and the status bar with a given prefix.
  (set prefix (or prefix ""))
  (let ((status (.get-element-by-id document status-id)))
    (when status
      (clear-element status-id)
      (when text
        (let ((text-block (html ((:p :class "workspace-status-message"))))
	      (alert-full-text (+ "javascript:alert('" text "'); return false;"))
	      )
          (set (@ text-block innerHTML) text)
	  (.append-child status text-block)
	  (when (and (.get client-settings "show-status-overflow-link")
	  	     (or (< (@ status client-width) (@ status scroll-width))
	                 (< (@ status client-height) (@ status scroll-height))
		         ))
	    (set (@ text-block style padding-right) "2em")
	    (.append-child text-block
	      (html ((:a :class "workspace-status-ellipsis" :href "#" :onclick alert-full-text) "...")))
          ))
      )
    ))
  (if text
    (progn
      (unless (@ window x-default-status)
        (set (@ window x-default-status) (@ window default-status))
      )
      (set (@ window status) (set (@ window default-status) (+ prefix text)))
    )
    ;else
    (when (@ window x-default-status)
      (set (@ window status) (set (@ window default-status) (@ window x-default-status)))
    ))
)


(defvar clear-client-status 
  (let ((timer false)
	(clear-status (lambda () (show-client-status "")))
	)
    (lambda (reset)
      ;;; Clears the client status line after a timeout.
      (when timer
        (clear-timeout timer)
      )
      (set timer (and (not reset) (set-timeout clear-status 3000)))
    ))
)


(defun show-client-status (text)
  ;;; Displays a message to the client portion of the status line.
  (clear-client-status true)
  (show-status text "workspace-status-client" "client: ")
)


(defun show-server-status (text)
  ;;; Displays a message to the server portion of the status line.
  (show-status text "workspace-status-server" "server: ")
)


(defun xml-unpack-handle-show-status (xml-message)
  ;;; Processes a status update message.
  (let* ((xml-node-message-text (@ xml-message first-child))
         (message-text (or (.get-text-content Dom-library xml-node-message-text) ""))
	 )
    (show-server-status message-text)
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "show-dialog" message code

(defun make-dialog-submit-processor (dialog id unique-id)
  ;;; Creates a function to process completing a dialog and submitting the input.
  (lambda (form e)
    ;; Prevent recursion (if used)
    (set (@ dialog onclose) no-op)
    (.close dialog)
    ;; Must not fail, so protect against this case
    (when form
      (package-user-popup-input id (@ (ref form unique-id) value))
    )
    (.stop-propagation Event-library e)
  )
)


(defun make-dialog-form-handler (process)
  ;;; Makes an event handler for the dialog's form.
  (lambda (e)
    (let ((form this)) ;; Must come first
      (process form e)
    ))
)


(defun make-dialog-form-button-handler (process button-id)
  ;;; Creates an event handler for the submit button of the dialog's form.
  (lambda (e)
    (let ((form this)) ;; Must come first
      (if (eq (@ (.get-target Event-library e) id) button-id)
        (process form e)
	(.stop-propagation Event-library e)
      ))
  )
)


(defun unpack-selection-list (select xml-message)
  (let* ((xml-node-options (@ xml-message next-sibling))
	 (xml-node-option (@ xml-node-options first-child))
	 )
    (set xml-node-option (@ xml-node-option first-child))
    (while xml-node-option
      (let* ((xml-node-option-text (@ xml-node-option first-child))
	     (xml-node-option-value (@ xml-node-option-text next-sibling))
	     (xml-node-option-options ;; a value is optional
	       (or (and xml-node-option-value (@ xml-node-option-value next-sibling))
	           null))
	     )
	(set xml-node-option (@ xml-node-option next-sibling))
	(let ((option-text (.get-text-content Dom-library xml-node-option-text))
	      (option-value (.get-text-content Dom-library (or xml-node-option-value xml-node-option-text)))
	      )
	  (cond
	    ((and xml-node-option-options
	          (eq "selected" (.to-lower-case (.get-text-content Dom-library xml-node-option-options))))
	      (.append-child select
		(html ((:option :value option-value :selected "selected") option-text))))
	    ((and xml-node-option-options
	    	  (eq "disabled" (.to-lower-case (.get-text-content Dom-library xml-node-option-options))))
	      (.append-child select
		(html ((:option :value option-value :disabled "disabled") option-text))))
	    (true
	      (.append-child select
	      	(html ((:option :value option-value) option-text))))
	  ))))
    (@ xml-node-options next-sibling)
  )
)


(defun xml-unpack-handle-show-dialog (xml-message)
  ;;; Displays a Dialog using a given title, label and form element.
  (let* ((list-node (@ xml-message first-child)) ;; This is a LIST
         (xml-node-id (@ list-node first-child))
	 (xml-node-title (@ xml-node-id next-sibling))
	 (xml-node-label (@ xml-node-title next-sibling))
	 (xml-node-dialog-type (@ xml-node-label next-sibling))
	 )
    (let ((id (.get-text-content Dom-library xml-node-id))
	  (title (.get-text-content Dom-library xml-node-title))
	  (label (.get-text-content Dom-library xml-node-label))
	  (dialog-type (.get-text-content Dom-library xml-node-dialog-type))
	  )
      (let ((dialog (or (.get-dialog-by-id Dialog id) (new Dialog id)))
	    ;; These must be globally unique, what ever they are
            (unique-id (+ "dialog-input1-" id))
	    (button-id (+ "dialog-button1-" id))
	    )
	(let* ((process (make-dialog-submit-processor dialog id unique-id))
	      ;; Also used as the action for the form element
	      (form-handler (make-dialog-form-handler process))
	      (form-element
	        (case (.to-lower-case dialog-type)
		  ("single-select"
		    (let* ((select (html ((:select :name unique-id :id unique-id))))
			   (xml-node-select-options (unpack-selection-list select xml-node-dialog-type))
			   )
		      select
		    ))
		  ("select"
		    (let* ((select (html ((:select :name unique-id :id unique-id :multiple "multiple" :size 6))))
			   (xml-node-select-options (unpack-selection-list select xml-node-dialog-type))
			   )
		      ;; TODO: Use an option to specify the size
		      (when (and (@ select length)
				 (> (@ select size) (@ select length)))
			(set (@ select size) (@ select length))
		      )
		      select
		    ))
	          ("1" ;; Obsolete, creates a textarea
		    (html ((:textarea :name unique-id :id unique-id :cols "40" :rows "4"))))
	          ("textarea"
		    (html ((:textarea :name unique-id :id unique-id :cols "40" :rows "4"))))
	          ("0" ;; Obsolete, creates a single line input box
		    (html ((:input :type "input" :name unique-id :id unique-id :size "40"))))
	          ("input"
		    (html ((:input :type "input" :name unique-id :id unique-id :size "40"))))
		  (t
		    (let ((value (+ "invalid-dialog-type:" dialog-type))
		          (text (+ "Server sent invalid dialog type: " dialog-type))
			  )
		      (html ((:div) ((:input type "hidden" :name unique-id :id unique-id :value value)) ((:p) text)))
		    ))
		))
	      (form
	        (html
		  ((:form :name id :action form-handler :style "margin: 0px; margin-top: 5px;")
		    ((:div :style "text-align: center; margin-bottom: 5px;")
		      ((:label :for unique-id :style "vertical-align: top;") label)
		      " "
		      form-element
		    )
		    ((:div :style "text-align: right;")
		      ((:input :type "button" :id button-id :value "Submit"))
		    )
		  )))
	      (contents
		(html
		  ;; This centers the content vertically
		  ((:div :style "display: table; height: 100%; position: relative; width: 100%;")
		    ((:div :style "display: table-cell; vertical-align: middle;")
		      ((:div :style "text-align: center")
		        form)))))
	      )
	  (set (@ form style margin) "0px auto")
	  (set (@ form onsubmit) form-handler)
	  (set (@ form onclick) (make-dialog-form-button-handler process button-id))
          (.set-title dialog title)
	  (.set-contents dialog contents)
	  (.show dialog)
	  (set (@ contents style min-width) (.get-style-property Dom-library form "width"))
	  (set (@ contents style min-height) (.get-style-property Dom-library form "height"))
	  ;; Once the dialog is shown, measure the contents
	  (.size-to-contents dialog)
	  (.center dialog)
	))))
)


;;;;;;;;;BOX MENU AND CLOTHES-PINS

(defun make-change-background-color-handler (color)
  (lambda (e)
    (set (@ this style background-color) color)
    (.stop-propagation Event-library e)
  )
)


(defun xml-unpack-handle-makeboxmenu (xml-message)
  ;;; This routine is given a top-level node of type ?makeboxmenu.  It unpacks and creates the menu.
  ;; (MAKEMENU (LIST (SYMBOL:MENU (TEXT:MENU)) (STRING:IN-OUT (TEXT:IN-OUT))
  ;;                 (LIST (SYMBOL:MENUENTRY (TEXT:MENUENTRY)) (STRING:READF (TEXT:READF)) (NUMBER:1 (TEXT:1))))
                                                   ;;MAKEMENU  (LIST  (MENU ...
  (let*  ((xml-node (@ xml-message first-child))   ;;LIST  (MENU ...
          (titlename "")
          (themenu "")
          (menuID "0")
          (menu-entry-node "")                ;;  ! It doesn't like nil!!! Use "".
          (menu-entry-id-number    0)
	  ;;This table had cellspacing and cellpadding in the style, it was choking for some reason on the Java console.
          (themenublock (html ((:table :boxid "0" :style "z-index: -1000; left: -300; position: absolute; border: solid;"))))      ;;create a new table, just for grins.
          (tablerow    "")
          (tablecell  "")
          )
    ;;create a new table, just for grins.
    (.append-child (.get-element-by-id document "palette")   themenublock )

;; TBD: Put in remaining super-safe bulletproof checks later.
    (set xml-node (@ xml-node first-child))         ;;SYMBOL  MENU
    (set xml-node (@ xml-node next-sibling))        ;;STRING  "MenuTitle Name"
    (set titlename (.get-text-content Dom-library xml-node))
    (set xml-node (@ xml-node next-sibling))        ;;NUMBER MENUID   LIST   (MENUENTRY ...
    (set menuID   (.get-text-content Dom-library xml-node))   ;;  Grab the id#.
    (set (@ themenublock id) menuID)

    ;;Hack:  Pulldown Menus only trigger on CHANGE.  Thus, if you have a one-entry menu,
    ;;even when you select the entry, it doesn't change...so no trigger.
    ;;Because of this, we're putting the name of the menu up top, with code 0.

    (set menu-entry-node (@ xml-node next-sibling))        ;;LIST   (MENUENTRY ...

    (while menu-entry-node
      (set xml-node (@ menu-entry-node first-child))         ;;SYMBOL  MENUENTRY
      (set xml-node (@ xml-node next-sibling))               ;;STRING  "MenuEntry Title Name"
      (set titlename (.get-text-content Dom-library xml-node))              ;;  Grab the title, "MenuEntry Title Name".
      (set xml-node (@ xml-node next-sibling))               ;;NUMBER  id#
      (set menu-entry-id-number (.get-text-content Dom-library xml-node))   ;;  Grab the id#.

      ;;insert menu entry
      (set tablerow    (html (:tr :style "border: solid; border-color: #333333; background-color: #ffffff; margin: 0px;")))
      (set tablecell  (html
                       ((:td :id menu-entry-id-number :style "text-align: center; border:solid; margin: 0px; padding: 4px; ")
                        titlename)))
      (.append-child themenublock tablerow)
      (.append-child tablerow     tablecell)
      ;; Take care of the mouse clicks, as necessary.
      (set (@ tablerow onmouseover) (make-change-background-color-handler "#e0ffe0"))
      (set (@ tablerow onmouseout)  (make-change-background-color-handler "#ffffff"))
      (set (@ tablecell onclick) (make-boxmenuentry-click-handler themenublock titlename))  ;called on the MENU ENTRIES.
      (set (@ tablecell onmouseover) (make-change-background-color-handler "#e0e0ff"))
      (set (@ tablecell onmouseout)  (make-change-background-color-handler "#ffffff"))

      ;...aaaand, pop to the next.
      (set menu-entry-node (@ menu-entry-node next-sibling))        ;;LIST   (MENUENTRY ...

    )     ;end of while menu-entry-node lists.

    ;; All done.  Stick the resulting menu onto the palette area.
    (.append-child (.get-element-by-id document "palette")   themenublock )
  )  ;end of let.
)


(defun xml-unpack-handle-attach-menu-to-box (xml-message)
  ;;; This routine is given a top-level node of type ?makeboxmenu.  It unpacks and creates the menu.
  ;; (ATTACH-MENU-TO-BOX (LIST (NUMBER:menuid (TEXT:menuid)) (STRING:IN-OUT (TEXT:IN-OUT))
  ;;                 (LIST (SYMBOL:MENUENTRY (TEXT:MENUENTRY)) (STRING:READF (TEXT:READF)) (NUMBER:1 (TEXT:1))))
                                                   ;;MAKEMENU  (LIST  (MENU ...
  (let*  ((xml-node (@ xml-message first-child))   ;;LIST  (MENU ...
	  (menuID "0")
	  (box-id "0")
	  (themenu "")
	  )
    (set xml-node (@ xml-node first-child))         ;;NUMBER  MENU
    (set menuID (.get-text-content Dom-library xml-node))
    (set xml-node (@ xml-node next-sibling))        ;;NUMBER  "BOXID
    (set box-id (.get-text-content Dom-library xml-node))

    (set themenu (.get-element-by-id document menuID))    ;;Find the menu.
    (when themenu
      (set (@ themenu style z-index) "1000")
      (set (@ themenu boxid) box-id)
      (set (@ themenu style left) (- last-mouse-x 10))
      (set (@ themenu style top ) (- last-mouse-y 10))
    ))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MENU STUFF;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-menuentry-click-handler (superbox-id title)
  ;; This function handles taking care of the mouse click on a pulldown menu item inside a box.
  ;;Note this function is called with the MENU-ENTRY object, NOT the MENU,
  ;;but it must change the MENU around and reset it to the top.

  ;;returns the following lambda function:
  (lambda (e)
    ;  Turn the background light green temporarily.
    (let* ((themenuentry this)
	   (themenu (@ themenuentry parent-node))
	   )
      (set (@ themenu style background-color) "#e0ffe0")
      (set (@ themenu style background-color) "#00ff00")
      (set (@ themenu style background-color) "#e0ffe0")

      ;  Send the mouse click results down the pipe.
      (package-boxmenu-mouse-click (@ themenuentry value) superbox-id title)
      ;; if you wanted to return the name for some reason, you could stick it here as well.
      ;;

      ;  Turn the background back to white.
      (set (@ themenu style background-color) "#ffffff")
      ;  Reset the element item to display the top element, the title.
      (set (@ themenu selected-index) 0)

      ; let the system know you handled the event, don't try anything else.
      (.stop-propagation Event-library e)
    ))
)


(defun make-boxmenuentry-click-handler (menu title)
  ;; This function handles taking care of the mouse click on a popup boxmenu item attached to a box.
  ;;Note this function is called with the MENU and MENUENTRY objects.
  ;;It must change the MENUENTRY around and reset it to the top.
  ;; Menu's "boxid" attribute must be set to current box.
  ;; Menu only allowed to be attached to one box at a time.
  ;;MOSTLY OBSOLETE, WITH NEW INLINE MENU STYLES.

  ;;returns the following lambda function:
  (lambda (e)
    ;  Turn the background light green temporarily.
    (let ((menu-entry this)
	  (menu-id (@ menu boxid))
	  )
      (set (@ menu-entry style background-color) "#e0ffe0")
      (set (@ menu-entry style background-color) "#00ff00")
      (set (@ menu-entry style background-color) "#e0ffe0")

      ;  Send the mouse click results down the pipe.
      (package-boxmenu-mouse-click (@ menu-entry id) menu-id title)

      ;  Turn the background back to white.
      (set (@ menu-entry style background-color) "#ffffff")

      ; Bury the menu, make it disappear.
      (.set-visible Dom-library menu false)

      ; let the system know you handled the event, don't try anything else.
      (.stop-propagation Event-library e)
    ))
)


(defun make-menu-click-handler (menu-id title)
  ;; This function handles taking care of the mouse click on a pulldown menu item.
  ;; BoxID is super-box-id, or 0 for palette menus.

  ;;returns the following lambda function:
  (lambda (e)
    ;  Send the mouse click results down the pipe.
    (package-menu-mouse-click menu-id title)
    ;; if you wanted to return the name for some reason, you could stick it here as well.
  )
)


(defun make-boxmenu-click-handler (menu-id box-id title)
  ;; This function handles taking care of the mouse click on a pulldown menu item.
  ;; BoxID is super-box-id, or 0 for palette menus.

  ;;returns the following lambda function:
  (lambda (e)
    ;  Send the mouse click results down the pipe.
    (package-boxmenu-mouse-click menu-id box-id title)
    ;; if you wanted to return the name for some reason, you could stick it here as well.

    ; let the system know you handled the event, don't try anything else.
    ; WHY WAS THIS COMMENTED OUT?
    (.stop-propagation Event-library e)  ;necessary to close the menu.  Sigh.
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun dump-xml-to-workspace (xml-message)
  ;;;Take care of dumping the XML
  (let ((workspace (.get-element-by-id document "workspace"))
	(xml-string (dump-xml-to-string xml-message))
	)
    (when workspace
      (.append-child workspace (html ((:pre :class "debug-workspace-dump") xml-string)))
      (update-toolboxes)
    )
  )
)


(defvar dump-xml-to-string
  ;; Prints out the xml text representation of a DOM node.
  (let ((our-has-attributes null)
	)
    (let ((has-attributes null) ; recursive
	  (expand-node null) ; recursive
	  (expand-children null) ; recursive
	  (expand-attributes
	    (lambda (xml-attributes)
	      ;; Prints out the attributes of the node
	      (let ((attributes (new Array)))
	        (dotimes (i (@ xml-attributes length))
		  (let ((item (.item xml-attributes i)))
		    (.push attributes (+ " " (@ item node-name) "=\"" (or (@ item node-value) (@ item node-name)) "\""))
		  ))
	        (.join attributes "")
	      )))
	  )
      (set has-attributes
	(lambda (xml-node)
	  ;; Checks if a node has attributes (possible Dom-library candidate)
	  (cond
	    (our-has-attributes
	      (.has-attributes xml-node)
	    )
	    ((eq our-has-attributes false)
	      (and (@ xml-node attributes) (< 0 (@ xml-node attributes length)))
	    )
	    (true
	      (set our-has-attributes (not (not (@ xml-node has-attributes))))
	      (has-attributes xml-node)
	    ))))
      (set expand-node
	(lambda (xml-node indent lines)
	  ;; Adds lines for this node with indent to lines
          (case (@ xml-node node-type)
	    (1 ;; Element
	      (let ((has-two-child-nodes
		      (and (.has-child-nodes xml-node)
		      	   ;; Is it exactly one text node?
		      	   (or (< 1 (@ xml-node child-nodes length))
			       (not (== 3 (@ xml-node first-child node-type)))
			       )
			   ))
		    (node-name (.to-lower-case (@ xml-node node-name)))
		    )
                (.push lines (+ indent "<" node-name
				(or (and (has-attributes xml-node) 
					 (expand-attributes (@ xml-node attributes)))
				    "")
			        (or (and has-two-child-nodes
					 ;; Has child nodes (more than 1)
					 ">")
				    (and (.has-child-nodes xml-node)
				    	 (not has-two-child-nodes)
					 ;; Display the text immediately between the start and end tags
					 (+ ">" (.get-text-content Dom-library xml-node) "</" node-name ">"))
				    ;; Empty node
			            " />")
				))
	        (when has-two-child-nodes
		  (expand-children (@ xml-node child-nodes) (+ indent "   ") lines)
		  (.push lines (+ indent "</" node-name ">"))
	        )
	      )
	    )
	    (3 ;; Text
	      (.push lines (+ indent (@ xml-node node-value)))
	    )
	    (8 ;; Comment
	      (.push lines (+ "<!-- " (@ xml-node node-value) "-->"))
	    )
	    (9 ;; Document
	      (when (.has-child-nodes xml-node)
	        (expand-children (@ xml-node child-nodes) indent lines)
	      )
	    )
	  )))
      (set expand-children
	(lambda (xml-children indent lines)
          (dotimes (i (@ xml-children length))
	    (expand-node (.item xml-children i) indent lines)
	  )))
      
      ;; Return actual function
      (lambda (xml-node)
        (let ((result (new Array)))
	  (expand-node xml-node "" result)
	  ;; Compine the lines for the result
	  (.join result #\Newline)))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;WORKSPACE COMMANDS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clear-workspace ()
  (clear-jbml-element "workspace")
  (update-toolboxes)
)


(defun clear-results ()
  (clear-jbml-element "results")
  (update-toolboxes)
)


(defun clear-palette ()
  (clear-element "palette")
  (setup-palette)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;HILIGHT COMMANDS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hilight-box-ID (box-id)
  ;;; Temporarily highlights the box for rollover effects.
  (let ((box (.get-element-by-id document box-id)))
    (when box
      (set (@ box style border-color) box-flash-color)
    ))
)


(defun unhilight-box-ID (box-id)
  (let ((box (.get-element-by-id document box-id)))
    (when box
      (.reset-border-color Jbml box)
    ))
)


(defun flashing-hilight-box-ID (box-id)
  ;;; Highlights the box for Selection.
  (let ((box (.get-element-by-id document box-id)))
    (when box
      (.add-class Dom-library box "hilight-jbml")
    ))
)


(defun unflashing-hilight-box-ID (box-id)
  (let ((box (.get-element-by-id document box-id)))
    (when box
      (.remove-class Dom-library box "hilight-jbml")
    ))
)


;;;;;SCROLLING

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar get-browser-window
  ;; This relies on let creating a closure
  (let ((our-windows (new Object))
  	(our-window-count 0)
	(our-dialog-count 0)
	(our-non-blocked-count 0)
	(test-id (lambda (id) (or id (is-number id))))
	)
    (set (ref our-windows "0") window)
    (let ((verify-window
	    (lambda (window-id)
	      (cond
		((not (and (test-id window-id) (ref our-windows window-id)))
	          false)
		((@ (ref our-windows window-id) closed)
		  (set (ref our-windows window-id) null))
		(true
		  true))))
	  (clean-up-windows
	    (lambda ()
	      (doslots ((i value) our-windows)
	        (when (or (not value)
			  (@ value closed)
			  )
		  ;; Window as closed elsewhere
		  (set (ref our-windows i) null)
		  (delete (ref our-windows i))
		)
	      )
	    ))
	  (make-clicked-popup
	    (lambda (window-id url title specs setup)
	      (set our-dialog-count (+ our-dialog-count 1))
	      (let ((dialog (new Dialog (+ "popup-clicked-" our-dialog-count)))
		    (link (html ((:a :href url) "here")))
		    )
		(set (@ link onclick)
		  (lambda ()
		    (get-browser-window window-id url title specs setup true)
		    (.close dialog)
		    false
		  ))
		(let* ((message
		         (or (and our-non-blocked-count
			          (html ((:div :class "blocked-popup-instructions")
				          ((:p) ((:em) "NOTE:") " Your browser enforces a limit on VPL opened windows.")
					  ((:p) "You may want to increase this limit by modifying the " ((:code) "about:config") " variable " ((:code) "dom.popup_maximum") ".")
					  )))
			     (html ((:div :class "blocked-popup-instructions")
			             ((:p) ((:em) "NOTE:") " Your browser is using a popup blocker.")
				     ((:p) "You may want to unblock this site.")
				     ((:p) "You might have other VPL windows already open.")
				     ))
			     ))
		       (contents
			 (html
			   ((:div :class "blocked-popup-message")
			     ((:p) "The popup window for " ((:span :class "blocked-popup-title") title) " was blocked.")
			     ((:p) "Please click " link " to continue.")
			     message
			     )))
		       )
		  (.set-title dialog "Blocked Popup Window")
		  (.set-contents dialog contents)
		  (.show dialog)
		  (.size-to-contents dialog)
		  (.center dialog)
		))))
	  )
      (lambda (window-id url title specs setup failed-open)
	(if (or (verify-window window-id) (== 1 (@ arguments length)))
	  ;; Only one argument or the window already exists...
	  (ref our-windows window-id)
	  ;else
	  (let ((my-window null))
	    (clean-up-windows)
	    (ignore-errors (set my-window (.open window url title specs)))
	    (if my-window
	      (let* ((my-window-close (@ my-window close))
		     (my-window-index (set our-window-count (+ our-window-count 1)))
		     (my-window-id (if (test-id window-id) window-id my-window-index))
		     )
		(define-class-method (my-window get-window-id) () my-window-id)
		(define-class-method (my-window get-window-index) () my-window-index)
		(define-class-method (my-window close) ()
		  (let ()
		    (.call my-window-close my-window)
		    (set (ref our-windows my-window-id) null)
		  ))
		(when (not (@ my-window opener))
		  (set (@ my-window opener) self)
		)
		(when setup
		  (setup my-window)
		)
		(unless failed-open
		  (++ our-non-blocked-count)
		)
		(set (ref our-windows my-window-id) my-window)
              )
	      ;else
	      (progn
	        (make-clicked-popup window-id url title specs setup)
		null
	      )))))))
)


;;; The original code had (when relative-p ...), which was true for both
;;; 1 and 0.  We changed it to (when (= relative-p 1) ...)
;;; so now when we specify 0 on the server it pops up the window absolute to the screen.
;;; (Note that relative-p is actually a string; the string "1" is equal to
;;; the number 1 in javascript, likewise for 0, but while the number 0
;;; is considered false, the string "0" is not considered false.

;;; -- JP
(defun setup-popup-URL-window (browser-window window-id relative-p)
  (unless window-id
    (+= (@ browser-window name) (.get-window-index browser-window))
  )
  (when (= relative-p 1)
    (.move-by browser-window (@ window screen-x) (@ window screen-y))
  )
  ;; JP 1/28/06 -- Added this to try to make sure that popup windows
  ;; come up visible instead of sometimes hidden.
  ;; Don't know whether this really works, but it doesn't
  ;; seem to do any harm.
  (.focus browser-window)
)


(defun popup-URL-window (window-id url specs relative-p)
  (let ((title (+ "VPL Message Window: " window-id))
	(setup
          (lambda (browser-window)
	    (setup-popup-URL-window browser-window window-id relative-p)
	  ))
	)
    (get-browser-window window-id url title specs setup)
    false
  )
)


(defun kill-window-id (windowID)
  (let ((browser-window (get-browser-window windowID)))
    (when browser-window
      (.close browser-window)
    ))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;SCROLL ROUTINES;;;;;;;;;;;;;;;;;;;;

(defun scroll-jbml-box-into-view (box)
  (while (not (.has-class Dom-library box "a-jbml"))
    (set box (@ box parent-node))
  )
  (let ((container (@ box parent-node)))
    (while (not (.has-class Dom-library container "contains-jbml"))
      (set container (@ container parent-node))
    )
    (scroll-box-into-view box container)
  )
)


(defun scroll-box-into-view (box container)
  (set (@ container scroll-top) (compute-scroll-box box container))
)


(defun compute-scroll-box (box container)
  ;; Ensures the given box is as completely visible as possible.
  ;; The returned value will be clipped according to the browser
  (if (> (@ container scroll-height) 
	 (@ container client-height))
    ;; Container is scrolled
    (let ((box-top (- (@ (.compute-absolute-position Dom-library box) top)
    		      (@ (.compute-absolute-position Dom-library container) top)))
	  (scroll-top (@ container scroll-top))
	  (padding-top (parse-int (.get-style-property Dom-library container "paddingTop" "padding-top")))
	  (padding-bottom (parse-int (.get-style-property Dom-library container "paddingBottom" "padding-bottom")))
	  )
      (cond 
        ((< (- box-top padding-top) scroll-top)
          ;; Box is above the client area
	  (- box-top padding-top))
	((> (+ box-top padding-bottom (@ box client-height))
	    (+ scroll-top (@ container client-height)))
	  ;; Box is below the client area
	  (- box-top (.min Math padding-top (- (+ (@ box client-height) padding-bottom) (@ container client-height)))))
	(true
	  ;; Box is visible
	  scroll-top)))
    ;else
    ;; Container fits on the screen (minor optimization)
    0)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defvar escape-string
  (let* ((slash (.from-char-code String 92))
         (zeros "0000")
  	 (escape-char
           (lambda (code)
	     (let ((hex-digits (.toString code 16)))
	       (+ slash "u" (.substring zeros (@ hex-digits length)) hex-digits)
	     )))
	 )
    (if (@ window RegExp)
      (let ((match-unsafe (new RegExp (+ "[" slash "x00-" slash "x08" slash "x0e-" slash "x1f" slash "x7f]"))))
        (lambda (string)
          (let ((position (.search string match-unsafe)))
	    (while (<= 0 position)
	      (set string (+ (.substring string 0 position) (.substring string (+ position 1))))
	      (set position (.search string match-unsafe))
	    )
	    string
	  )))
      ;else
      (lambda (string)
        string)))
)


(defun display-element (element-id is-shown)
  (let ((element (.get-element-by-id document element-id)))
    (when element
      (when (not (@ element x-initial-display))
        (set (@ element x-initial-display) (.get-style-property Dom-library element "display"))
      )
      (.set-visible Dom-library element is-shown (@ element x-initial-display))
    )
  )
)


(defun clear-element (element-id)
  (let ((element (.get-element-by-id document element-id)))
    (when (and element (@ element child-nodes))
      (while (@ element child-nodes length)
	(removing-child element (@ element first-child))
      )
    ))
)


(defun clear-jbml-element (box-id)
  (let ((element (.get-element-by-id document box-id)))
    (when (and element (@ element child-nodes))
      (while (@ element child-nodes length)
        (.removing-child Jbml element (@ element first-child))
      )
    ))
)


(defun replacing-child (container new-node old-node)
  (when (is-visible-menu-inside old-node)
    (.close-menus Menu)
  )
  (.replace-child container new-node old-node)
)


(defun removing-child (container child)
  (when (is-visible-menu-inside child)
    (.close-menus Menu)
  )
  (.remove-child container child)
)


(defun is-visible-menu-inside (node)
  (let* ((menu-id (.get-visible-id Menu))
  	 (menu-element (and menu-id (.get-element-by-id document menu-id)))
	 )
    (while (and menu-element
		(not (eq node menu-element))
		)
      (set menu-element (@ menu-element parent-node))
    )
    menu-element
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base-library
;; 
;; Author: Johnny Casey
;;
;; Some simple functions to capture common JavaScript tasks
(defun is-number (a)
  (eq "number" (typeof a))
)


(defun is-string (a)
  (eq "string" (typeof a))
)


(defun is-function (a)
  (eq "function" (typeof a))
)


(defun is-object (a)
  (or (and a (eq "object" (typeof a))) (is-function a))
)


(defun is-array (a)
  (and (is-object a) (eq (@ a constructor) Array))
)


(defun is-undefined (a)
  (eq "undefined" (typeof a))
)


(defun find-property (get aliases offset)
  (let ((retval "")
	(i (or offset 0))
	)
    (while (and (< i (@ aliases length))
		(not retval)
		)
      (set retval (or (get (ref aliases i)) retval))
      (set i (1+ i))
    )
    retval
  )
)


(defun get-property (object aliases) ;; Additional arguments processed
  (if (and (== 2 (@ arguments length))
	   (is-array aliases)
	   )
    (find-property (lambda (property) (ref object property)) aliases)
    ;else
    (find-property (lambda (property) (ref object property)) arguments 1))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-clone
;;
;; Takes as parameter a JavaScript object to clone.  The resulting object is a
;; deep copy of the original object.  Part of Base-library
(defun get-clone (object)
  (if (or (== null object)
          (not (is-object object))
          (== window object)
	  (@ object node-type))
    object
    (let ((copy (if (is-array object) (new Array) (new Object))))
      (doslots ((i value) object)
	(set (ref copy i)
	  (or (ignore-errors (get-clone value))
	      value))
      )
      copy
    ))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Position
;; 
;; Author: Johnny Casey
;;
;; A class to represent a position.
(defvar Position
	;; Private static variables
  (let ((our-dummy (new Object))
	(our-key-counter 0)
	)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; Constructor
	  ;;
	  ;; Takes an optional initial position.
    (let ((position-constructor 
	    (lambda (initial-value)
	      (let ((my-self this) ;; Must come first
		    (my-key (+= our-key-counter 1))
		    (copy (or initial-value our-dummy))
		    )
		(set (@ my-self left) (or (@ copy left) 0))
		(set (@ my-self top) (or (@ copy top) 0))

		(define-class-method (my-self get-key) () my-key)

		(set copy (set initial-value null)) ;; Release these references
		my-self
	      )))
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; Private static functions
	  (add
	    (lambda (object dx dy)
	      (+= (@ object left) dx)
	      (+= (@ object top) dy)
	      object
	    ))
	  (sub
	    (lambda (object dx dy)
	      (-= (@ object left) dx)
	      (-= (@ object top) dy)
	      object
	    ))
	  )
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Public instance functions

      ;; Converts the position to a string.
      (defmethod (position-constructor to-string) ()
	(+ "(" (@ this left) "," (@ this top) ")"))
      ;; Adds an offset to the position (both values optional).
      (defmethod (position-constructor add) (dx dy)
	(add this (or dx 0) (or dy 0)))
      ;; Subtracts an offset from the position (both values optional).
      (defmethod (position-constructor sub) (dx dy)
	(sub this (or dx 0)) (or dy 0))

      ;; Adds a position-like object to this position.  prefix is an optional
      ;; string to prepend to "Left" and "Top".
      ;; Otherwise uses "left" and "top".
      (defmethod (position-constructor add-position) (other prefix)
	(add this (or (ref other (or (and prefix (+ prefix "Left")) "left")) 0)
		  (or (ref other (or (and prefix (+ prefix "Top")) "top")) 0)
		  ))
      ;; Subtracts a position-like object from this position.  prefix is an
      ;; optional string to prepend to "Left" and "Top".
      ;; Otherwise uses "left" and "top".
      (defmethod (position-constructor sub-position) (other prefix)
	(sub this (or (ref other (or (and prefix (+ prefix "Left")) "left")) 0)
		  (or (ref other (or (and prefix (+ prefix "Top")) "top")) 0)
		  ))

      ;; Adds a size-like object to this position.  prefix is an optional
      ;; string to prepend to "Width" and "Height".
      ;; Otherwise uses "width" and "height".
      (defmethod (position-constructor add-dimension) (other prefix)
	(add this (or (ref other (or (and prefix (+ prefix "Width")) "width")) 0)
		  (or (ref other (or (and prefix (+ prefix "Height")) "height")) 0)
		  ))

      ;; Subtracts a size-like object from this position.  prefix is an optional
      ;; string to prepend to "Width" and "Height".
      ;; Otherwise uses "width" and "height".
      (defmethod (position-constructor sub-dimension) (other prefix)
	(sub this (or (ref other (or (and prefix (+ prefix "Width")) "width")) 0)
		  (or (ref other (or (and prefix (+ prefix "Height")) "height")) 0)
		  ))

      ;; Compares two positions vertically then horizontally.
      (defmethod (position-constructor compare) (other)
	(or (- (@ this top) (@ other top)) (- (@ this left) (@ other left))))

      ;; Return the Position class object
      position-constructor
    ))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dom-library
;; 
;; Author: Johnny Casey
;; Dependencies: Base-library
;;
;; Provides a cross platform interface for accessing and manipulating Document
;; Object Model information.
;;
;; This class is a collection of static methods.
(defvar Dom-library
        ;; Private static variables
  (let ((our-root-element null)
        (our-dummy (new Object))
	(our-whitespace (or (and (@ window RegExp) (new RegExp (+ (.from-char-code String 92) "s+"))) " "))
        (our-class (new Object))
	(our-last-st-id 0)
	(our-hack-broken-overflow-stacking-context null) ;; navigator not defined yet
	(our-has-get-computed-style false)
	)
	  ;; Private static functions
    (let ((get-root-element
	    (lambda ()
	      (set our-root-element (or our-root-element
					(and (<= 0 (.index-of (+ "" (@ document compat-mode)) "CSS"))
					     (@ document document-element))
					(@ document body)
					))
	    ))
	  (has-get-computed-style
            (lambda ()
              (set our-has-get-computed-style (or our-has-get-computed-style
                                                  (and (@ document default-view)
						       (@ document default-view get-computed-style)
						       )
						  ))
            ))
	  (is-stacking-context
	    (lambda (node)
	      (when (eq null our-hack-broken-overflow-stacking-context)
	        ;; Firefox before 3.0 brokenly assumed that overflowed elements created stacking contexts
		(set our-hack-broken-overflow-stacking-context (== 1.8 (gecko-revision)))
	      )
	      (or (and (not (eq "static" (.get-style-property our-class node "position")))
		       (not (eq "auto" (.get-style-property our-class node "zIndex" "z-index")))
		       )
		  (and our-hack-broken-overflow-stacking-context
		       (< (@ node client-height) (@ node scroll-height))
		       )
		  (eq node (.get-root-element our-class))
		  )
	    ))
	  (has-class
	    (lambda (node class-name)
	      (let ((words (.split (+ "" (@ node class-name)) our-whitespace))
	      	    (retval false)
		    )
		(dotimes (i (@ words length))
		  (when (eq (ref words i) class-name)
		    (set retval true)
		    ;; Leave the loop
		    (set i (@ words length))
		  )
		)
		retval
	      )))
	  (show-node
	    (lambda (node display)
	      (set (@ node style visibility) "visible")
	      (set (@ node style display) (or display ""))
	    ))
	  (hide-node
	    (lambda (node)
	      (set (@ node style visibility) "hidden")
	      (set (@ node style display) "none")
	    ))
	  (add-enclosing-offset-borders
	    (lambda (node position)
	      (let ((top 0)
		    (left 0)
		    )
	        (when node
		  (while node
		    (+= left (or (parse-int (.get-style-property our-class node "borderLeftWidth" "border-left-width")) 0))
		    (+= top (or (parse-int (.get-style-property our-class node "borderTopWidth" "border-top-width")) 0))
		    (set node (@ node offset-parent))
		  )
		)
		(when position
		  (+= (@ position top) top)
		  (+= (@ position left) left)
		)
	      )
	    ))
	  )
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Public static functions

      ;; Return the root element of the document
      (define-class-method (our-class get-root-element) () (get-root-element))

      ;; Retrieve the stacking context of a DOM node
      (set (@ our-class get-stacking-context)
        (lambda (node)
	  (let ((retval null))
	    (while node
	      (set retval node)
	      (set node (and (not (is-stacking-context node))
	                     (@ node parent-node)))
	    )
	    (or retval (get-root-element))
	  )))

      ;; Lowers the stacking context of a DOM node, causing it to appear behind
      ;; other stacking contexts.
      (define-class-method (our-class lower-stacking-context) (node)
        (let ((root (get-root-element))
	      (context (.get-stacking-context our-class node))
	      )
	  (while (not (eq context root))
	    (set node context)
	    (when (not (eq "undefined" (typeof (@ node x-normal-z-index))))
	      (set (@ node style z-index) (@ node x-normal-z-index))
	    )
	    (set context (.get-stacking-context our-class (@ node parent-node)))
	  )
	))

      ;; Raises the stacking context of a DOM node, causing it to appear above
      ;; other stacking contexts.
      (define-class-method (our-class raise-stacking-context) (node)
        (let ((root (get-root-element))
	      (context (.get-stacking-context our-class node))
	      )
	  (while (not (eq context root))
	    (set node context)
	    (set context (.get-stacking-context our-class (@ node parent-node)))
	    ;; Save initial z-index
	    (when (eq "undefined" (typeof (@ node x-normal-z-index)))
	      (set (@ node x-normal-z-index) (.get-style-property Dom-library node "zIndex" "z-index"))
	      ;; Ensure that the node has an id
	      (when (eq "undefined" (typeof (@ node id)))
	        (++ our-last-st-id) ;; Avoid "+ ++" in resulting Javascript
	        (set (@ node id) (+ "domlib-st-" our-last-st-id))
	      )
	      ;; Ensure that the context has a list of children
	      (when (not (is-array (@ context x-children-contexts)))
	        (set (@ context x-children-contexts) (new Array))
	      )
	      (.push (@ context x-children-contexts) (@ node id))
	    )
	    (let ((i 0)
	          (sibling-id null)
		  (sibling null)
	    	  (children (@ context x-children-contexts))
		  )
	      (while (< i (@ children length))
	        (set sibling-id (ref children i))
		(set sibling (.get-element-by-id document sibling-id))
		(if (and sibling-id
		         sibling
			 )
		  (let ((z-index (.get-style-property Dom-library sibling "zIndex" "z-index")))
		    (if (!= 2000 z-index)
		      (set (@ sibling x-normal-z-index) z-index)
		      ;else
		      (set (@ sibling style z-index) (@ sibling x-normal-z-index)))
		    (++ i)
		  )
		  ;else
		  (let ((last (ref children (- (@ children length) 1))))
		    (set (ref children i) last)
		    (-- (@ children length))
		  )
		)
	      )
	    )
	    ;; Raise the node
	    (set (@ node style z-index) 2000)
	  )
	))

      ;; Retrieve the width of the browser display area.
      (define-class-method (our-class get-browser-width) ()
        (parse-int (or (@ self inner-width)
		       (@ (or (get-root-element) our-dummy) client-width)
		       0
		       )))
      ;; Retrieve the height of the browser display area.
      (define-class-method (our-class get-browser-height) ()
        (parse-int (or (@ self inner-height)
		       (@ (or (get-root-element) our-dummy) client-height)
		       0
		       )))

      ;; Retrieve the current horizontal scroll position.
      (if (is-number (@ window page-x-offset))
          (define-class-method (our-class get-scroll-left) ()
	    (@ window page-x-offset))
	  ;else
	  (define-class-method (our-class get-scroll-left) ()
	    (or (@ (or (get-root-element) our-dummy) scroll-left) 0)))
      ;; Retrieve the current vertical scroll position.
      (if (is-number (@ window page-y-offset))
          (define-class-method (our-class get-scroll-top) ()
	    (@ window page-y-offset))
	  ;else
	  (define-class-method (our-class get-scroll-top) ()
	    (or (@ (or (get-root-element) our-dummy) scroll-top) 0)))

      ;; Computes the style of a DOM node.
      (define-class-method (our-class compute-style) (node)
        (or (@ node current-style)
	    (and (has-get-computed-style)
		 (.get-computed-style (@ document default-view) node null))
	    ;; TODO: Try to recursively build the style by looking at style sheets, ugh!
	    (@ node style)
	    ))
      ;; Copy the computed style of a DOM node.
      ;; This breaks the define-class-method macro, because it doesn't properly return values
      (set (@ our-class copy-computed-style) 
	(lambda (node)
          (cond
            ((@ node current-style)
              (get-clone (@ node current-style))
	    )
            ((has-get-computed-style)
              (let ((computed-style (.get-computed-style (@ document default-view) node null))
                    (style (new Object))
		    )
                 (dotimes (i (@ computed-style length))
                  (let ((property (.item computed-style i)))
                    (set (ref style property) (.get-property-value computed-style property))
		  )
                )
                style
              )
	    )
            (true
              (get-clone (@ node style))
	    )
	  )
	))
      ;; Retrieve a style from a node given a list of possible style names.
      ;; This breaks the define-class-method macro, because it doesn't properly return values
      (set (@ our-class get-style-property)
	(lambda (node aliases)
	  (let ((args arguments) ;; Must come first
		(getter
		  (progn
	            (cond
		      ((@ node current-style)
		        (lambda (property) (ref (@ node current-style) property))
	              )
	              ((has-get-computed-style)
	      	        (let ((computed-style (.get-computed-style (@ document default-view) node null)))
		          (lambda (property) (.get-property-value computed-style property))
	      	        )
	              )
	              (true
	      	        (lambda (property) (ref (@ node style) property))
                      )
		    )))
		)
	    (if (and (== 2 (@ args length))
		     (is-array aliases)
		     )
	      (find-property getter aliases)
	      ;else
	      (find-property getter args 1))
	  )))

      ;; Retrieve the text content of a DOM node.
      ;; This includes the concatenation of all the text nodes inside this DOM
      ;; node.
      (define-class-method (our-class get-text-content) (node) 
        (or (@ node text-content) 
	    (@ node inner-text) 
	    (@ node text) 
	    ""
	    ))

      ;; Determine if a DOM node has a particular class.
      (define-class-method (our-class has-class) (node class-name) (has-class node class-name))

      ;; Adds a class to a DOM node.
      (define-class-method (our-class add-class) (node class-name)
        (let ()
	  (unless (has-class node class-name)
	    (set (@ node class-name) (+ (or (and (@ node class-name) (+ (@ node class-name) " ")) "") class-name))
	  )))
      ;; Removes a class from a DOM node.
      (define-class-method (our-class remove-class) (node class-name)
        (let ((words (.split (+ "" (@ node class-name)) our-whitespace)))
	  (let ((initial-length (@ words length))
	        (i (@ words length))
		)
	    (while (<= 0 (-- i))
	      (when (eq (ref words i) class-name)
	        ;; Erase the removed entry with one from the end (which has already been tested)
	        (set (ref words i) (ref words (- (@ words length) 1)))
		;; Reduce the length of the words *AFTER* replacing the element...
		(-- (@ words length))
	      )
	    )
	    (when (< (@ words length) initial-length)
	      (set (@ node class-name) (or (.join words " ") ""))
	    )
	  )))

      (set (@ our-class show) show-node)
      (set (@ our-class hide) hide-node)
      ;; Changes the visibility of a DOM node.
      (define-class-method (our-class set-visible) (node is-visible display)
        (let ()
	  (if is-visible
	    (show-node node display)
	    (hide-node node))
	))

      ;; Change the alpha transparency of a DOM node.
      (set (@ our-class set-alpha)
        (lambda (node alpha)
	  (set (@ node style opacity) (/ alpha 10))
	  (set (@ node style filter) (+ "alpha(opacity=" (* alpha 10) ")"))
	))

      ;; Removes any IDs from a DOM node tree.
      (set (@ our-class clear-ids)
        (lambda (node)
	  (let ((stack (new Array))
	  	(child null)
		(children null)
		)
	    (.push stack node)
	    (while (@ stack length)
	      (set child (.pop stack))
	      (set (@ child id) "")
	      (set children (@ child child-nodes))
	      (dotimes (i (@ children length))
	        (.push stack (ref children i))
	      )
	    )
	    node
	  )
	))

      
      ;; Determine the absolute position of a DOM node.
      (set (@ our-class compute-absolute-position)
        (lambda (node)
	  (let ((retval (new Position)))
	    (add-enclosing-offset-borders node retval)
	    (while node
	      ;; Firefox sometimes sets offset-top/left to negative
	      ;; values which should be ignored.
	      (+= (@ retval top) (.max Math 0 (@ node offset-top)))
	      (+= (@ retval left) (.max Math 0 (@ node offset-left)))
	      (set node (@ node offset-parent))
	    )
	    retval
	  )))

      ;; Determine the scrolled position of a DOM node.
      (set (@ our-class compute-scrolled-position)
        (lambda (node)
	  (if (@ node get-bounding-client-rect)
	    (.add-position (progn (new Position (.get-bounding-client-rect node))) (get-root-element) "scroll")
	    ;else	    
	    (let ((retval (.compute-absolute-position our-class node))
		  (node (@ node parent-node))
		  )
	      (while node
		(.sub-position retval node "scroll")
		(set node (@ node parent-node))
	      )
	      retval
	    ))))

      ;; Locate the nearest child in a container to a given position.
      (set (@ our-class find-nearest-child)
	(lambda (container x y)
	  (if (.has-child-nodes container)
	    (let ((children (@ container child-nodes))
		  (position (.compute-scrolled-position our-class container))
		  (retval (new Object))
		  ;; The following values are offset by 1
		  (candidate-above 0)
		  (candidate-below 0)
		  (candidate-before 0)
		  (candidate-after 0)
		  )
	      (let* ((candidate-above-max (.sub-position (progn (new Position position)) container "scroll"))
		     (candidate-below-min (.add-dimension (progn (new Position candidate-above-max)) container "scroll"))
		     (candidate-before-max candidate-above-max)
		     (candidate-after-min candidate-below-min)
		     )
	        (dotimes (i (@ children length))
		  (set position (.compute-scrolled-position our-class (ref children i)))
		  (cond 
		    ((< y (@ position top)) ;; position is below point
		      ;; Find the left-most minimum position
		      (when (< 0 (.compare candidate-below-min position))
		        (set candidate-below (1+ i))
			(set candidate-below-min position)
		      )
		    )
		    ((> y (+ (@ position top) (@ (ref children i) offset-height))) ;; position + height is above point
		      ;; Find the right-most maximum position
		      (.add position (@ (ref children i) offset-width) 0) ;; ignore the offset-height
		      (when (> 0 (.compare candidate-above-max position))
		        (set candidate-above (1+ i))
			(set candidate-above-max position)
		      )
		    )
		    ((< x (@ position left)) ;; position is after point
		      (when (< 0 (.compare candidate-after-min position))
			(set candidate-after (1+ i))
			(set candidate-after-min position)
		      )
		    )
		    ((> x (+ (@ position left) (@ (ref children i) offset-width))) ;; position + width is before point
		      (.add position (@ (ref children i) offset-width) 0) ;; ignore the offset-height
		      (when (> 0 (.compare candidate-before-max position))
			(set candidate-before (1+ i))
			(set candidate-before-max position)
		      )
		    )
		    (true ;; x,y is inside the child...  should not happen!
		      ;(alert "Found point in a child, while trying to find nearest child!")
		    )
		  )
		)
		(when (== 0 (+ candidate-before candidate-after))
		  ;; Only use the fallbacks when there are no candidates.
		  (set candidate-before candidate-above)
		  (set candidate-after candidate-below)
		)
		(set (@ retval before) (and candidate-before (ref children (1- candidate-before))))
		(set (@ retval after) (and candidate-after (ref children (1- candidate-after))))
	      )
	      retval
	    )
	    ;else
	    false)))

      ;; Return the Dom-library class object
      our-class
    ))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Position-cache
;; 
;; Author: Johnny Casey
;; Dependencies: Position, Dom-library
;;
;; Calculates the position of objects and caches the absolute position for the
;; duration of this object.
(defvar Position-cache
	;; Private static variables
  (let ((our-dummy (new Object)))
  	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; Constructor
    (let ((cache-constructor
	    (lambda ()
	      (let ((my-self this))
	        (set (@ my-self positions) (new Object))
		my-self
	      )
	    ))
	  ;; Private static functions
	  (get-cache
	    (lambda (cache)
	      (@ cache positions)))
	  (set-key
	    (lambda (node key)
	      (set (@ node x-position-key) key)))
	  (get-key
	    (lambda (node)
	      (or (@ (or node our-dummy) x-position-key) 0)))
	  )
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Public instance functions

      ;; Computes the abosulte position of a DOM node.
      (defmethod (cache-constructor compute-absolute-position) (node)
	(or (ref (get-cache this) (get-key node))
	    (let ((retval (new Position))
		  (cache (get-cache this))
		  (position-node node)
		  )
	      (while (and node
			  (not (ref cache (get-key node)))
			  )
		(.add retval (or (parse-int (.get-style-property Dom-library node "border-left-width" "borderLeftWidth")) 0)
			     (or (parse-int (.get-style-property Dom-library node "border-top-width" "borderTopWidth")) 0)
			     )
		;; Firefox sometimes sets offset-top/left to negative
		;; values which should be ignored.
		(.add retval (.max Math 0 (@ node offset-left))
			     (.max Math 0 (@ node offset-top))
			     )
		(set node (@ node offset-parent))
	      )
	      (when node
		(.add-position retval (ref cache (get-key node)))
	      )
	      (set (ref cache (set-key position-node (.get-key retval))) retval)
	    )))

      ;; Compute the scrolled position of a DOM node.
      (defmethod (cache-constructor compute-scrolled-position) (node)
	(or (and (@ node get-bounding-client-rect)
		 (.add-position (progn (new Position (.get-bounding-client-rect node))) (.get-root-element Dom-library) "scroll")
		 )
	    (let ((retval (new Position (.compute-absolute-position this node)))
		  (node (@ node parent-node))
		  )
	      (while node
		(.sub-position retval node "scroll")
		(set node (@ node parent-node))
	      )
	      retval
	    )))

      ;; Finds the child of a container nearest a point.
      (defmethod (cache-constructor find-nearest-child) (container x y)
	(or (and (.has-child-nodes container)
		 (let ((children (@ container child-nodes))
		       (position (.compute-scrolled-position this container))
		       (retval (new Object))
		       ;; The following values are offset by 1
		       (candidate-above 0)
		       (candidate-below 0)
		       (candidate-before 0)
		       (candidate-after 0)
		       )
		   (let* ((candidate-above-max (.sub-position (progn (new Position position)) container "scroll"))
			  (candidate-below-min (.add-dimension (progn (new Position candidate-above-max)) container "scroll"))
			  (candidate-before-max candidate-above-max)
			  (candidate-after-min candidate-below-min)
			  )
		     (dotimes (i (@ children length))
		       (set position (.compute-scrolled-position this (ref children i)))
		       (cond 
			 ((< y (@ position top)) ;; position is below point
			   ;; Find the left-most minimum position
			   (when (< 0 (.compare candidate-below-min position))
			     (set candidate-below (1+ i))
			     (set candidate-below-min position)
			   )
			 )
			 ((> y (+ (@ position top) (@ (ref children i) offset-height))) ;; position + height is above point
			   ;; Find the right-most maximum position
			   (when (> 0 (.compare candidate-above-max position))
			     (set candidate-above (1+ i))
			     (set candidate-above-max position)
			   )
			 )
			 ((< x (@ position left)) ;; position is after point
			   (when (< 0 (.compare candidate-after-min position))
			     (set candidate-after (1+ i))
			     (set candidate-after-min position)
			   )
			 )
			 ((> x (+ (@ position left) (@ (ref children i) offset-width))) ;; position + width is before point
			   (when (> 0 (.compare candidate-before-max position))
			     (set candidate-before (1+ i))
			     (set candidate-before-max position)
			   )
			 )
			 (true ;; x,y is inside the child...  should not happen!
			   ;(alert "Found point in a child, while trying to find nearest child!")
			 )
		       )
		     )
		     (when (== 0 (+ candidate-before candidate-after))
		       ;; Only use the fallbacks when there are no candidates.
		       (set candidate-before candidate-above)
		       (set candidate-after candidate-below)
		     )
		     (set (@ retval before) (and candidate-before (ref children (1- candidate-before))))
		     (set (@ retval after) (and candidate-after (ref children (1- candidate-after))))
		   )
		   retval
		 ))
	    false
	    ))

      ;; Return the Position-cache class object
      cache-constructor
    ))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event-library
;; 
;; Author: Johnny Casey
;; Dependencies: Base-library, Dom-library
;;
;; Provides a cross platform interface for accessing and manipulating event
;; information.
;;
;; This class is a collection of static methods.
(defvar Event-library
        ;; Private static variables
  (let ((our-use-event-page-xy false)
	(our-use-event-client-xy false)
	(our-mousedown-sane-button false)
	(our-mousedown-weird-which false)
	(our-class (new Object))
	;; Private static functions
	(get-event (lambda (e) (or e (@ window event))))
	)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Public static functions

    ;; Add Event Listeners
    (cond
      ((@ document add-event-listener)
        (define-class-method (our-class add-event-listener) (node e listener capturing)
	  (.add-event-listener node e listener capturing)))
      ;; TODO: Provide a means to access the registered element for IE
      ((@ document attach-event)
        (define-class-method (our-class add-event-listener) (node e listener)
	  (.attach-event node e listener)))
      (true
        (define-class-method (our-class add-event-listener) (node e listener)
	  (set (ref node (+ "on" e)) listener))))

    ;; Remove Event Listeners
    (cond
      ((@ document remove-event-listener)
        (define-class-method (our-class remove-event-listener) (node e listener capturing)
	  (.remove-event-listener node e listener capturing)))
      ;; TODO: Provide a means to access the registered element for IE
      ((@ document detach-event)
        (define-class-method (our-class remove-event-listener) (node e listener)
	  (.detach-event node e listener)))
      (true
        (define-class-method (our-class remove-event-listener) (node e listener)
	  (set (ref node (+ "on" e)) null))))

    ;; Wrap functions using this fixup to ensure that get-event is called appropriately
    (let ((fixup-event (lambda (f) (lambda (e) (f (get-event e)))))
	  )
      (define-class-method (our-class get-event) (e) (get-event e))

      (set (@ our-class get-target)
        (fixup-event 
          (lambda (e)
            (let ((target (or (@ e target)
	       		      (@ e src-element)
			      ))
	          )
	      (or (and target
	               ;; Avoid a safari (and early mozilla) bug
		       (== 3 (@ target node-type))
		       (@ target parent-node)
		       )
	          target
	          )))))

      ;; Retrieve the event target.
      (define-class-method (our-class get-current-target) (event-this e)
        (or (and (not (eq window event-this))
	         event-this)
	    (@ (get-event e) current-target)
	    null))

      ;; Retrieve the keypress character code.
      (set (@ our-class get-keypress-char-code)
        (fixup-event 
          (lambda (e)
            (if (eq "keypress" (+ "" (@ e type)))
	      (or (@ e key-code)
		  (@ e char-code)
		  (@ e which)
		  )
	      0))))

      ;; Mouse button constants
      (set (@ our-class left-mouse-button) 1)
      (set (@ our-class middle-mouse-button) 2)
      (set (@ our-class right-mouse-button) 4)

      ;; Retrieve the mouse button that was pushed.
      (set (@ our-class get-button)
        (fixup-event 
          (lambda (e)
            (let ((retval null)
	          )
	      (cond
	        (our-mousedown-sane-button
	          (set retval (or (@ e button) 0))
	        )
	        (our-mousedown-weird-which
	          (set retval (or (<< 1 (- (@ e which) 1)) 0)) ;; Converts 1 2 3 to 1 2 4
	        )
	        (true
	          (if (is-number (@ e button)) ;; May be 0
	            (if (@ e which) ;; Always > 0
		      (if (== (@ e button) (@ e which))
		        ;; Safari (but could also be weird Opera <8b, oh well)
		        (set our-mousedown-sane-button true)
		        ;else
		        ;; Firefox / Opera 8+
		        (set our-mousedown-weird-which true))
		      ;else
		      ;; IE
		      (set our-mousedown-sane-button true))
		    ;else
		    (if (eq "mousedown" (+ "" (@ e type)))
		      ;; N4
		      (set our-mousedown-weird-which true)
		      ;else
		      ;; unknown, probably wasn't a mouse event...
		      (set retval 0)))
	          ;; Call ourself to get the actual value
	          (unless (== 0 retval)
	            (set retval (.get-button our-class e))
	          )))
	      retval
	    ))))

      ;; Mouse event position information
      (set (@ our-class get-page-x)
        (fixup-event
          (lambda (e)
            ;; Returns the X coordinate of the event
	    (cond
	      (our-use-event-page-xy (@ e page-x))
	      (our-use-event-client-xy (+ (@ e client-x) (.get-scroll-left Dom-library)))
	      ((is-number (@ e client-x))
		(if (is-number (@ e page-x))
	            (set our-use-event-page-xy true)
		    ;else
		    (set our-use-event-client-xy true))
		(.get-page-x our-class e))
	      (true 0)))))
      (set (@ our-class get-page-y)
        (fixup-event
          (lambda (e)
            ;; Returns the Y coordinate of the event
	    (cond
	      (our-use-event-page-xy (@ e page-y))
	      (our-use-event-client-xy (+ (@ e client-y) (.get-scroll-top Dom-library)))
	      ((is-number (@ e client-y))
	        (if (is-number (@ e page-y))
	          (set our-use-event-page-xy true)
		  ;else
		  (set our-use-event-client-xy true))
	        (.get-page-y our-class e))
	      (true 0)))))

      ;; Retrieve the to-element of an event.
      (set (@ our-class get-to-element)
        (fixup-event
          (lambda (e)
	    (or (@ e related-target) 
                (@ e to-element)
	        ))))
      ;; Retrieve the from-element of an event.
      (set (@ our-class get-from-element)
        (fixup-event
          (lambda (e)
	    (or (@ e related-target) 
                (@ e from-element)
                ))))

      ;; Determine if the mouse event indicates the mouse left an element.
      ;; This breaks the define-class-method macro, because it doesn't properly return values
      (set (@ our-class is-mouseexit)
        (lambda (e)
          (let ((target (.get-target our-class e))
	        (from-element (.get-from-element our-class e))
	        )
	    (while (and from-element
		        (not (eq from-element target))
		        )
	      (set from-element (@ from-element parent-node))
	    )
	    (not from-element)
	  )
        ))

      ;; Event propagation
      (set (@ our-class stop-propagation)
        (fixup-event
          (lambda (e)
            (set (@ e return-value) false)
	    (set (@ e cancel-bubble) true)
	    (when (@ e stop-propagation)
	      (.stop-propagation e)
	    )
	    false
	  )))
      (set (@ our-class prevent-default)
        (fixup-event
          (lambda (e)
	    (when (@ e prevent-default)
	      (.prevent-default e)
	    )
          )))
    )

    ;; Return the Event-library class object
    our-class
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drag code
;;
;; Author: Johnny Casey
;; Dependencies: Base-library, Dom-library, Event-library
;;
;; Provides a class to manipulate position by dragging a dom element.
(defvar Drag
  ;; This relies on let creating a closure
        ;; Private static variables
  (let (
	(our-last-z-index 1000)
	)
	  ;; Private static functions
    (let (
	  (default-drag-xy
	    ;; Basic drag function
	    (lambda (node style x y)
	      (set (@ node style left)
		   (+ (parse-int (@ style left)) x "px"))
	      (set (@ node style top)
		   (+ (parse-int (@ style top)) y "px"))
	    ))
	  (drag-constructor null)
	  )
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Constructor
      ;;
      ;; Creates a drag object for a node (optional) with an update function
      ;; (a default is used if not provided).
      (set drag-constructor
	;; Constructs a Drag object
	(lambda (node drag-xy)
	  (let ((my-self this) ;; Must come first
	        (my-state null)
		(my-is-dragging false)
		(my-drag-xy (or drag-xy default-drag-xy))
		(go null) ;; Defined below so stop can deregister it
		(stop null) ;; Defined below so stop can deregister it
		)
	    ;; Private instance functions
	    (set go
	      ;; Called during the mouse move event
	      (lambda (e)
		(.drag-xy my-state
		      (@ my-state node)
		      (@ my-state style)
		      (- (.get-page-x Event-library e)
		         (@ my-state cursor-start-x))
		      (- (.get-page-y Event-library e)
		         (@ my-state cursor-start-y)))
		(.prevent-default Event-library e)
	      ))
	    (set stop
	      ;; Called during the mouse up event
	      (lambda (e)
		(set my-is-dragging false)
		(.remove-event-listener Event-library document "mousemove" go true)
		(.remove-event-listener Event-library document "mouseup" stop true)
		(when (@ my-self on-stop)
		  (.call (@ my-self on-stop) this e)
		)
		;; Allow on-stop to access getter functions of the drag operation
		(set my-state null)
	      ))
	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    ;; Public instance functions

	    ;; Prepares dragging information
	    (define-class-method (my-self prep) (event-this e drag-xy)
	      (let ((e (.get-event Event-library e)))
		(set my-state (new Object))
		(set (@ my-state node) (or node (.get-current-target Event-library event-this e)))
		(set (@ my-state drag-xy) (or drag-xy my-drag-xy))
		(set (@ my-state cursor-start-x) (.get-page-x Event-library e))
		(set (@ my-state cursor-start-y) (.get-page-y Event-library e))
		(set (@ my-state style)
		  (.copy-computed-style Dom-library (@ my-state node)))
	      ))
	    ;; Begins the drag by assigning listeners and raising the z-index.
	    ;; This breaks the define-class-method macro, because it tries to return the first statement
	    (set (@ my-self prep-start)
	      (lambda ()
		(set (@ my-state node style z-index)
		  (set our-last-z-index (1+ our-last-z-index)))
		(.add-event-listener Event-library document "mousemove" go true)
		(.add-event-listener Event-library document "mouseup" stop true)
		(set my-is-dragging true)
	      ))

	    ;; Event handler that begins dragging, takes an optional callback.
	    ;; This breaks the define-class-method macro, because it tries to return the first statement
	    (set (@ my-self start)
	      (lambda (event-this e drag-xy)
		(.prep this event-this e drag-xy)
		(.prep-start this)
		(.stop-propagation Event-library e)
	      ))

	    ;; Retrieve the dragged node.
	    (define-class-method (my-self get-dragged) ()
	      (and my-state (@ my-state node)))

	    ;; Deterimine if the mouse is currently dragging /any/ node.
	    (define-class-method (my-self is-dragging) ()
	      my-is-dragging)

	    ;; Return the new Drag object
	    my-self
	  )))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Public static functions

      ;; Retrieve the last used z-index of a dragged object.
      (define-class-method (drag-constructor get-z-index) ()
        ;; Retrieves the last used z-index
        our-last-z-index)

      ;; Return the Drag constructor object
      drag-constructor
    ))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scroll-bar
;; 
;; Author: Johnny Casey
;; Dependencies: Base-library, Dom-library, Event-library, Drag
;;
;; Creates a scroll bar using HTML.
(defvar Scroll-bar
	;; Private static variables
  (let ((scroll-bar-constructor null)
	(our-drag (new Drag))
	(our-scroll-bar-id 0)
	(our-dummy-node (new Object))
	(our-scrolling (new Object))
	(our-default-type "generic")
	)
    (set (@ our-dummy-node parent-node) false)
	  ;; Private static accessor functions
    (let ((set-content-id
	    (lambda (node content-id)
	      (set (@ node x-scroll-content-id) content-id)))
	  (get-content-id
	    (lambda (node)
	      (or (@ node x-scroll-content-id) 0)))
	  (set-horizontal
	    (lambda (node is-horizontal)
	      (set (@ node x-scroll-horizontal) is-horizontal)))
	  (get-horizontal
	    (lambda (node)
	      (or (@ node x-scroll-horizontal) false)))
	  (set-type
	    (lambda (node type)
	      (set (@ node x-scroll-type) type)))
	  (get-type
	    (lambda (node)
	      (or (@ node x-scroll-type) our-default-type)))
	  (get-bar-id
	    (lambda (scroll-id)
	      (+ scroll-id "-bar")))
	  )
	    ;; Private static functions
      (let (
	    (copy-scroll-style-properties
	      (lambda (node)
		(let ((style (new Object)))
		  (set (@ style left) (.get-style-property Dom-library node "left"))
		  (set (@ style top) (.get-style-property Dom-library node "top"))
		  style
		)))
	    (scroll-vertical
	      (lambda (node style x y)
	        (let ((content (.get-element-by-id document (get-content-id node)))
		      (scroll (@ node parent-node))
		      )
		  (when (and content scroll
			     (< 0 (@ content client-height))
			     (< (@ content client-height) (@ content scroll-height))
			     )
			   ;; Determine the current height of the scroll bar
		    (let* ((visible (* (/ (@ content client-height) (@ content scroll-height)) (@ scroll client-height)))
			   ;; Determine the max delta of the scroll bar
			   (max-pos (- (@ scroll client-height) visible 1))
			   ;; Determine the top of the scroll bar
			   (pos (.min Math max-pos (.max Math 0 (+ (parse-int (@ style top)) y))))
			   (scroll-pos (* (or (and max-pos (/ pos max-pos)) 0) (- (@ content scroll-height) (@ content client-height))))
			   )
		      (set (@ node style height) (+ visible "px"))
		      (set (@ node style top) (+ pos "px"))
		      (set (@ content scroll-top) scroll-pos)
		    )
		  )
	        )))
	    (scroll-horizontal
	      (lambda (node style x y)
	        (let ((content (.get-element-by-id document (get-content-id node)))
		      (scroll (@ node parent-node))
		      )
		  (when (and content scroll
			     (< 0 (@ content client-width))
			     (< (@ content client-width) (@ content scroll-width))
			     )
		    (let* ((visible (* (/ (@ content client-width) (@ content scroll-width)) (@ scroll client-width)))
			   (max-pos (- (@ scroll client-width) visible 1))
			   (pos (.min Math max-pos (.max Math 0 (+ (parse-int (@ style left)) x))))
			   (scroll-pos (* (or (and max-pos (/ pos max-pos)) 0) (- (@ content scroll-width) (@ content client-width))))
			   )
		      (set (@ node style width) (+ visible "px"))
		      (set (@ node style left) (+ pos "px"))
		      (set (@ content scroll-left) scroll-pos)
		    )
		  )
	        )))
	    (scroll-stop
	      (lambda ()
		(let ((bar (.get-dragged our-drag)))
		  (when bar
		    (set (ref our-scrolling (get-type bar)) false)
		    (set (@ bar style z-index) "")
		  )
		)
	      ))
	    )
	      ;; Event Handlers
	(let ((mousedown-bar-handler
	        (lambda (e)
		  (let ((bar (.get-current-target Event-library this e)))
		    (set (ref our-scrolling (get-type bar)) true)
		    (.start our-drag this e (or (and (get-horizontal bar) 
						     scroll-horizontal)
						scroll-vertical))
		  )
	        ))
	      (mousedown-scroll-less-handler
	        (lambda (e)
		  (let ((bar (.get-element-by-id document (get-content-id (.get-current-target Event-library this e)))))
		    (if (get-horizontal bar)
		      (scroll-horizontal bar (copy-scroll-style-properties bar) (- 0 (/ (parse-int (.get-style-property Dom-library bar "width")) 4)) 0)
		      ;else
		      (scroll-vertical bar (copy-scroll-style-properties bar) 0 (- 0 (/ (parse-int (.get-style-property Dom-library bar "height")) 4))))
		    (.stop-propagation Event-library e)
		  )
	        ))
	      (mousedown-scroll-more-handler
	        (lambda (e)
		  (let ((bar (.get-element-by-id document (get-content-id (.get-current-target Event-library this e)))))
		    (if (get-horizontal bar)
		      (scroll-horizontal bar (copy-scroll-style-properties bar) (/ (parse-int (.get-style-property Dom-library bar "width")) 4) 0)
		      ;else
		      (scroll-vertical bar (copy-scroll-style-properties bar) 0 (/ (parse-int (.get-style-property Dom-library bar "height")) 4)))
		    (.stop-propagation Event-library e)
		  )
	        ))
	      )
	  (let* ((create-scroll-bar
		   (lambda (scroll-id is-horizontal type)
		     (let* ((bar-id (get-bar-id scroll-id))
			    (bar (html ((:div :id bar-id :class "scroll-bar-button scroll-bar-bar")
			    		 (:div :class "scroll-bar-cap scroll-bar-begin")
					 (:div :class "scroll-bar-cap scroll-bar-end")
					 )))
			    (scroll-less (html ((:div :class "scroll-bar-button scroll-bar-less"))))
			    (scroll-more (html ((:div :class "scroll-bar-button scroll-bar-more"))))
			    (scroll-bar (html ((:div :id scroll-id :class "scroll-bar")
					        ((:div :class "scroll-bar-track")
						  bar
						  )
					        scroll-less
					        scroll-more
					        )))
			    )
		       (set-horizontal bar is-horizontal)
		       (set-type bar type)
		       (set (@ bar onmousedown) mousedown-bar-handler)

		       ;; The buttons refer to the bar which references the content
		       (set-content-id scroll-less bar-id)
		       (set (@ scroll-less onmousedown) mousedown-scroll-less-handler)
		       (set-content-id scroll-more bar-id)
		       (set (@ scroll-more onmousedown) mousedown-scroll-more-handler)
		       (+= (@ scroll-bar class-name) (or (and is-horizontal 
							      " scroll-horizontal")
							 " scroll-vertical"))
		       scroll-bar
		     )))
		 (update-position
		   ;; Must come after we have attached the scroll-bar
		   (lambda (scroll-id is-horizontal)
		     (let ((bar (.get-element-by-id document (get-bar-id scroll-id))))
		       (when bar
			 (if is-horizontal
			   (scroll-horizontal bar (copy-scroll-style-properties bar) 0 0)
			   ;else
			   (scroll-vertical bar (copy-scroll-style-properties bar) 0 0))
		       )
		     )
		   ))
	         (show
		   (lambda (scroll-id content-id is-horizontal)
		     (let ((scroll-bar (.get-element-by-id document scroll-id))
			   (content (or (.get-element-by-id document content-id)
					our-dummy-node
					))
			   )
		       (.set-visible Dom-library scroll-bar (not (if is-horizontal
								   (eq (@ content client-width) (@ content scroll-width))
								   ;else
								   (eq (@ content client-height) (@ content scroll-height)))))
		       (update-position scroll-id is-horizontal)
		     )
		   )
		 )
	         (attach
	           (lambda (scroll-id content-id is-horizontal type)
		     (let* ((scroll-bar (or (.get-element-by-id document scroll-id) 
					    (create-scroll-bar scroll-id is-horizontal type)
					    ))
			    (content (or (.get-element-by-id document content-id) 
				         our-dummy-node
				         ))
			    (container (@ content parent-node))
			    )
		       (when container
		         (cond
			   ((not (@ scroll-bar parent-node)) ;; Scroll-bar doesn't have a parent
			     (.insert-before container scroll-bar content)
			   )
			   ((or (not (eq container (@ scroll-bar parent-node)))
			        (not (eq content (@ scroll-bar next-sibling)))
			        )
			     (.remove-child (@ scroll-bar parent-node) scroll-bar) ;; We don't want to kill menus if possible
			     (.insert-before container scroll-bar content)
			   )
		         )
		         ;; Must come after we have attached the scroll-bar
			 (set-content-id (.get-element-by-id document (get-bar-id scroll-id)) content-id)
		       )
		    )))
	         (detach
	           (lambda (scroll-id content-id)
		     (let ((scroll-bar (.get-element-by-id document scroll-id))
			   (content (.get-element-by-id document content-id))
			   )
		       (when scroll-bar
			 (.remove-child (@ scroll-bar parent-node) scroll-bar)
		       )
		       (when content
		         ;; FIXME Remove onmousescroll and DOMMouseScroll events
		       )
		     )))
	         )
          
	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    ;; Constructor
	    ;;
	    ;; Takes a flag to make the scrollbar horizontal and an optional
	    ;; type of scrollbar.
	    (set scroll-bar-constructor
	      (lambda (is-horizontal type)
	        (let ((my-self this) ;; Must come first
		      (my-scroll-id (+ "scroll-bar-" our-scroll-bar-id))
		      (my-content-id "")
		      (my-type (or type our-default-type))
		      )
		  (++ our-scroll-bar-id)

		  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		  ;; Public instance functions

		  ;; Attaches the scrollbar.
		  (define-class-method (my-self attach) (content-id)
		    (attach my-scroll-id (set my-content-id content-id) is-horizontal my-type))
		  ;; Shows the scrollbar.
		  (define-class-method (my-self show) ()
		    (show my-scroll-id my-content-id is-horizontal))
		  ;; Detaches the scrollbar.
		  (define-class-method (my-self detach) ()
		    (detach my-scroll-id my-content-id))

		  ;; Return new Scroll-bar
		  my-self
	        )))
            
	    (set (@ our-drag on-stop) scroll-stop)
	  ))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Public static functions

    ;; Retrieve the fixed dimension of the scrollbar.
    (define-class-method (scroll-bar-constructor get-size) () 20)
    ;; Determines if a particular type of scrollbar is scrolling.
    (define-class-method (scroll-bar-constructor is-scrolling) (type)
      (or (ref our-scrolling (or type our-default-type)) false))

    ;; Return the Scroll-bar constructor object
    scroll-bar-constructor
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dialog
;;
;; Author: Johnny Casey
;; Dependencies: Dom-library, Drag
;;
;; This class implements an in window dialog class.  The dialog can contain
;; arbitrary HTML and is dismissible.
;;
;; The dialog is draggable using the Drag class.
(defvar Dialog
  ;; This relies on let creating a closure...
        ;; Private static variables
  (let ((our-dialogs (new Object))
	(our-borders (new Object))
	(dialog-constructor null)
	)
	  ;; Border related private functions
    (let (
	  (make-move-min
	    (lambda (edge-name size-name min-size-names)
	      (lambda (node style pos)
	        (let ((size (- (parse-int (ref style size-name)) pos))
		      (min (parse-int (get-property style min-size-names)))
		      )
		  (when (< size min)
		    (-= pos (- min size))
		    (set size min)
		  )
		  (set (ref (@ node style) edge-name) (+ (parse-int (ref style edge-name)) pos "px"))
		  (set (ref (@ node style) size-name) (+ size "px"))
	        )
	      )))
	  (make-move-max
	    (lambda (edge-name size-name)
	      (lambda (node style pos)
	        (set (ref (@ node style) edge-name) (+ (parse-int (ref style edge-name)) pos "px"))
	        (set (ref (@ node style) size-name) (+ (parse-int (ref style size-name)) pos "px"))
	      )))
	  (fixup-heights
	    (lambda (f)
	      (lambda (node)
		(.apply f this arguments)
		(.x-fix-heights node)
	      )))
	  )
      (let (
	    (move-top-edge (fixup-heights (make-move-min "top" "height" (new Array "minHeight" "min-height"))))
	    (move-bottom-edge (fixup-heights (make-move-max "bottom" "height")))
	    (move-left-edge (make-move-min "left" "width" (new Array "minWidth" "min-width")))
	    (move-right-edge (make-move-max "right" "width"))
	    )
	;; Borders and functions to drag with
	;; we iterate over each of these when creating the borders.
	(define-class-method (our-borders n) (node style x y)
	  (move-top-edge node style y))
	(define-class-method (our-borders w) (node style x y)
	  (move-left-edge node style x))
	(define-class-method (our-borders s) (node style x y)
	  (move-bottom-edge node style y))
	(define-class-method (our-borders e) (node style x y)
	  (move-right-edge node style x))
	(define-class-method (our-borders nw) (node style x y)
	  ;; An empty let is needed to prevent a bug from creating
	  ;; a broken function
	  (let ()
	    (move-left-edge node style x)
	    (move-top-edge node style y)
	  ))
	(define-class-method (our-borders sw) (node style x y)
	  ;; An empty let is needed to prevent a bug from creating
	  ;; a broken function
	  (let ()
	    (move-left-edge node style x)
	    (move-bottom-edge node style y)
	  ))
	(define-class-method (our-borders se) (node style x y)
	  ;; An empty let is needed to prevent a bug from creating
	  ;; a broken function
	  (let ()
	    (move-right-edge node style x)
	    (move-bottom-edge node style y)
	  ))
	(define-class-method (our-borders ne) (node style x y)
	  ;; An empty let is needed to prevent a bug from creating
	  ;; a broken function
	  (let ()
	    (move-right-edge node style x)
	    (move-top-edge node style y)
	  ))
      )
    )
    
    (let (
	  ;; Private static functions
	  (attach-dialog
	    (lambda (id dialog-element dialog)
	      (.append-child (.get-root-element Dom-library) dialog-element)
	      (set (ref our-dialogs id) dialog)
	    ))
	  (remove-dialog
	    (lambda (id dialog-element)
	      (when (ref our-dialogs id)
		(set (ref our-dialogs id) null)
		(.remove-child (.get-root-element Dom-library) dialog-element)
	      )))
	  (size-box
	    (lambda (box width height min-width min-height)
	      (let ((browser-width (.get-browser-width Dom-library))
		    (browser-height (.get-browser-height Dom-library))
		    )
		(let ((width (.min Math width (/ (* browser-width))))
		      (height (.min Math height (/ (* browser-height))))
		      (min-width (.max Math (or min-width 0)
					    (or (parse-int (@ box style min-width)) 0)
					    0))
		      (min-height (.max Math (or min-height 0)
					     (or (parse-int (@ box style min-height)) 0)
					     0))
		      )
		  (set (@ box style width) (+ width "px"))
		  (set (@ box style height) (+ height "px"))
		  (set (@ box style min-width) (+ (.min Math width min-width) "px"))
		  (set (@ box style min-height) (+ (.min Math height min-height) "px"))
		))))
	  (center-box
	    (lambda (box)
	      (let ((browser-width (.get-browser-width Dom-library))
		    (browser-height (.get-browser-height Dom-library))
		    )
	        (let ((left (/ (- browser-width (@ box client-width)) 2))
		      (top (/ (- browser-height (@ box client-height)) 2))
		      )
		  (set (@ box style left) (+ left "px"))
		  (set (@ box style top) (+ top "px"))
		))))

	  ;; Slightly more complicated elements
	  (create-buttons
	    (lambda (dialog)
	      (let ((close (html ((:img :src "mauveX16x16.gif"))))
		    (close-onclick (lambda (e) (.close dialog e)))
		    )
		(set (@ close onclick) close-onclick)
		(set (@ close onmousedown) (@ Event-library stop-propagation))
		(html ((:div :class "dialog-buttons") close))
	      )))

	  (make-header-mousedown
	    (lambda (drag)
	      (lambda (e)
		(.start drag this e)
	      )))
	  )

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Constructor
      ;;
      ;; Takes the ID to use with this dialog.
      (set dialog-constructor
	(lambda (dialog-id)
	  (let* ((my-self this) ;; Must come first
		 ;; Header elements
		 (my-buttons (create-buttons my-self))
		 (my-buttons-balance (html ((:div :class "dialog-buttons-balance") " ")))
		 (my-title (html ((:div :class "dialog-title"))))
		 (my-header (html ((:div :class "dialog-header") my-buttons my-buttons-balance  my-title)))
		 ;; Contents elements
		 (my-contents (html ((:div :class "dialog-contents"))))
		 (my-body (html ((:div :class "dialog-body") my-contents)))
		 ;; Main frame
		 (my-interior (html ((:div :class "dialog-interior") my-header my-body)))
		 (my-dialog (html ((:div :class "dialog" :id dialog-id) my-interior)))
		 (my-drag (new Drag my-dialog))

		 (fix-heights
		   (lambda ()
		     (let ((dialog-width (parse-int (.get-style-property Dom-library my-dialog "width")))
			   (interior-width (parse-int (.get-style-property Dom-library my-interior "width")))
			   (dialog-height (parse-int (.get-style-property Dom-library my-dialog "height")))
			   (header-height (parse-int (.get-style-property Dom-library my-header "height")))
			   )
		       (set (@ my-interior style height) (+ (- dialog-height (- dialog-width interior-width)) "px"))
		       (set (@ my-body style height) (+ (- dialog-height (- dialog-width interior-width) header-height) "px"))
		     )))
		 )
	    ;; Put the dialog on top
	    (set (@ my-dialog style z-index) (.get-z-index Drag))
	    ;; Make available for later
	    (set (@ my-dialog x-fix-heights) fix-heights)

	    ;; Allow the dialog header for dragging
	    (set (@ my-header onmousedown) (make-header-mousedown my-drag))

	    ;; Attach the borders
	    (doslots ((i adjust) our-borders)
	      (let ((class-name (+ "dialog-border dialog-border-" i)))
	        (let ((border (html ((:div :class class-name)))))
		  (define-class-method (border onmousedown) (e)
		    (.start my-drag this e adjust))
		  (.append-child my-dialog border)
		)))

	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    ;; Public instance functions

	    ;; Set the title of the dialog.
	    (define-class-method (my-self set-title) (title)
	      (set (@ my-title innerHTML) title))
	    ;; Set the contents of the dialog.
	    (define-class-method (my-self set-contents) (contents)
	      (let ((contents (or contents "")))
		(set (@ my-contents innerHTML) "")
		(if (is-string contents)
		  (set (@ my-contents innerHTML) contents)
		  (.append-child my-contents contents))
	      ))

	    ;; Size the dialog to the contents with an optional width and
	    ;; height.
	    (define-class-method (my-self size-to-contents) (width height)
	      ;; An empty let is needed to prevent a bug from creating
	      ;; a broken function
	      (let ()
		(set (@ my-dialog style min-width) "0px")
		(set (@ my-dialog style min-height) "0px")
		(set (@ my-buttons-balance style width) (.get-style-property Dom-library my-buttons "width"))
		(set (@ my-buttons-balance style height) (.get-style-property Dom-library my-buttons "height"))

		(let ((dialog-width (parse-int (.get-style-property Dom-library my-dialog "width")))
		      (dialog-height (parse-int (.get-style-property Dom-library my-dialog "height")))
		      (contents-width (parse-int (.get-style-property Dom-library my-contents "width")))
		      (contents-height (parse-int (.get-style-property Dom-library my-contents "height")))
		      (title-width (parse-int (.get-style-property Dom-library my-title "width")))
		      (header-height (parse-int (.get-style-property Dom-library my-header "height")))
		      )
		  (let ((contents-edge-height (- dialog-height (+ contents-height header-height))))
		    ;; Set the width
		    (size-box my-dialog
			      (.max Math (+ dialog-width (- (@ my-contents scroll-width) (@ my-contents client-width)))
				         (+ (- dialog-width contents-width) (or width 0))
					 (+ dialog-width (- (@ my-title scroll-width) (@ my-title client-width)))
					 (+ (- dialog-width title-width) (* (parse-int (@ my-buttons-balance style width)) 5))
					 )
			      dialog-height
			      ;; Minimum width is determined by the contents and the size of the buttons
			      (.max Math (+ dialog-width (- (@ my-contents scroll-width) (@ my-contents client-width)))
					 (+ (- dialog-width title-width) (* (parse-int (@ my-buttons-balance style width)) 5))
					 )
			      )
		    ;; Update values
		    (set dialog-width (parse-int (.get-style-property Dom-library my-dialog "width")))
		    (set contents-height (parse-int (.get-style-property Dom-library my-contents "height")))
		    (set header-height (parse-int (.get-style-property Dom-library my-header "height")))

		    ;; Set the height
		    (size-box my-dialog
			      dialog-width
			      (+ contents-edge-height
				 header-height
				 (or height
				     (+ contents-height (- (@ my-contents scroll-height) (@ my-contents client-height)))
				     )
				 )
			      0 ;; ignored
			      (+ contents-edge-height
				 header-height
				 contents-height
				 (- (@ my-contents scroll-height) (@ my-contents client-height)))
			      )
		    (fix-heights)
		  ))))
	    ;; Centers the dialog in the browser window.
	    (define-class-method (my-self center) ()
	      (center-box my-dialog))
	    ;; Shows the dialog.
	    (define-class-method (my-self show) ()
	      (attach-dialog dialog-id my-dialog my-self))
	    ;; Closes the dialog.
	    (define-class-method (my-self close) (e)
	      ;; An empty let is needed to prevent a bug from creating
	      ;; a broken function
	      (let ()
		(and (@ my-self onclose) (.onclose my-self e))
		(remove-dialog dialog-id my-dialog)
	      ))

	    ;; Return the new Dialog object
	    my-self
	  )))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Public static functions
      (define-class-method (dialog-constructor get-dialog-by-id) (id)
        (ref our-dialogs id))

      ;; Return the Dialog constructor object
      dialog-constructor
    ))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu
;;
;; Author: Johnny Casey (based on DropDownMenuX)
;; Dependencies: Dom-library, Event-library
;;
;; Nestable pop-up menus.
(defvar Menu
	;; Private static constants
  (let ((our-delay-hide 500) ;; Delay in Millisecs before closing a menu
	(our-delay-show 200) ;; Delay in Millisecs before displaying a menu

	(our-scrollbar-width (.get-size Scroll-bar))
	(our-browser-padding 5) ;; Border of browser window the menu will avoid
	(our-remaining-overlap-min 40) ;; Minimum space to leave when overlapping another menu
	(our-sub-left-offset -5) ;; Distance submenus overlap previous menu, negative values work best
	(our-sub-top-offset 5) ;; Distance submenus overlap previous menu, positive values work best

	(our-dummy-node (new Object)) ;; Used by get-node-header and get-node-section

	;; Private static data
	(our-sections (new Object)) ;; Guard values for each section.  Reset when a new menu is created.
	(our-sections-hide-guard 0) ;; Guard value for hiding all sections
	(our-header-shown-guard 0) ;; Guard value for showing headers
	(our-visible (new Array)) ;; List of visible sections
	(our-timers (new Array)) ;; List of currently counting down timers
	(our-scroll-bars (new Array)) ;; List of unused scrollbars
	)
    (set (@ our-dummy-node class-name) "")
    (set (@ our-dummy-node offset-left) 0)
    (set (@ our-dummy-node offset-width) 0)
    (set (@ our-dummy-node offset-top) 0)
    (set (@ our-dummy-node offset-height) 0)
    (set (@ our-dummy-node style) (new Object))
    (define-class-method (our-dummy-node has-child-nodes) () false)

	  ;; Private static id functions
    (let ((get-node-id
	    (lambda (node)
	      (+ (@ node x-menu-id) (@ node x-header-id))))
	  (get-node-section-id
	    (lambda (node)
	      (+ (@ node x-menu-id) (@ node x-header-id) "-section")))
	  (get-node-contents-id
	    (lambda (node)
	      (+ (@ node x-menu-id) (@ node x-header-id) "-contents")))
	  )
	    ;; Private static functions
      (let ((menu-constructor null)
	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
	    ;; Node relationship functions

	    ;; Gets the header for the given header/section
	    (get-node-header
	      (lambda (node)
		(if node
		  (let ((id (get-node-id node)))
		    (cond
		      ((eq id (@ node id))
			node)
		      ((and (@ node previous-sibling) (eq id (@ node previous-sibling id)))
			(@ node previous-sibling))
		      (true
			(or (.get-element-by-id document id) 
			    our-dummy-node))))
		  ;else
		  our-dummy-node)))
	    ;; Gets the section for the given header/section
	    (get-node-section
	      (lambda (node)
		(if node
		  (let ((id (get-node-section-id node)))
		    (cond
		      ((eq id (@ node id))
			node)
		      ((and (@ node next-sibling) (eq id (@ node next-sibling id)))
			(@ node next-sibling))
		      (true
			(or (.get-element-by-id document id)
			    our-dummy-node))))
		  ;else
		  our-dummy-node)))
	    ;; Gets the container for the given header/section
	    (get-node-container
	      (lambda (node)
		(if node
		  (let ((levels (.split (@ node x-header-id) "-")))
		    (.pop levels)
		    (let ((id (+ (@ node x-menu-id) (.join levels "-") "-section")))
		      (if (and (@ node parent-node) (eq id (@ node parent-node id)))
			(@ node parent-node))
			;else
			(or (.get-element-by-id document id)
			    our-dummy-node)))
		  ;else
		  our-dummy-node)))

	    ;; Gets the scrollbar attached to a node, finds an unused one or allocates a new one.
	    (get-scroll-bar
	      (lambda (node)
		(set (@ node x-scroll-bar) (or (@ node x-scroll-bar) (.pop our-scroll-bars) (new Scroll-bar false "menu-scroll-bar")))))

	    ;; Retrieve the ids of containing sections
	    (get-parent-sections
	      (lambda (node)
		(let ((parents (new Object)))
		  (when (and node (@ node x-header-id))
		    (let ((levels (.split (@ node x-header-id) "-"))
		  	  (id (@ node x-menu-id))
			  )
		      (.shift levels)
		      (.pop levels)
		      (dotimes (i (@ levels length))
			(+= id (+ "-" (ref levels i)))
			(set (ref parents (+ id "-section")) true)
		      )
		    )
		  )
		  parents
		)
	      ))

	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
	    ;; parse-menu functions
	    (setup-parsed-node
	      (lambda (handlers menu-id base-id node header-id type)
		(set (@ node x-menu-id) menu-id)
		(set (@ node x-header-id) base-id)
		(set (@ node x-item-type) type)
		(set (@ node id) (+ menu-id header-id))
		(set (@ node onmouseover) (ref handlers (+ type "Mouseover")))
		(set (@ node onmouseout) (ref handlers (+ type "Mouseout")))
		(when (ref handlers (+ type "Hide"))
		  (set (@ node x-onhide) (ref handlers (+ type "Hide")))
		)
		(when (ref handlers (+ type "Show"))
		  (set (@ node x-onshow) (ref handlers (+ type "Show")))
		)
	      ))
	    (new-guard
	      (lambda ()
		(let ((retval (new Object)))
		  (set (@ retval show) 0)
		  (set (@ retval hide) 0)
		  retval
		)))
	    (parse-menu null) ;; Recursive

	    (copy-menu-style-properties
	      (lambda (node)
		(let ((style (new Object)))
		  (set (@ style border-top-width) (or (parse-int (.get-style-property Dom-library node "borderTopWidth" "border-top-width")) 0))
		  (set (@ style border-bottom-width) (or (parse-int (.get-style-property Dom-library node "borderBottomWidth" "border-bottom-width")) 0))
		  (set (@ style border-left-width) (or (parse-int (.get-style-property Dom-library node "borderLeftWidth" "border-left-width")) 0))
		  style
		)))

	    (reset-section-node
	      (lambda (section)
		(set (@ section style display) "")
		;; Reset modified style values
		(set (@ section style width) "")
		(set (@ section style height) "")
		(when (@ section x-scroll-bar)
		  (let ((scroll-bar (@ section x-scroll-bar)))
		    (set (@ section x-scroll-bar) false)
		    (.detach scroll-bar)
		    (.push our-scroll-bars scroll-bar)
		  )
		)
	      ))
	    (hide-section-node
	      (lambda (section)
		(.set-visible Dom-library section false)
		(when (@ section x-onhide)
		  (.x-onhide section)
		)
	      ))
	    (show-section-node
	      (lambda (section)
		(.set-visible Dom-library section true)
		(when (@ section x-scroll-bar)
		  (.show (@ section x-scroll-bar))
		)
		(when (@ section x-onshow)
		  (.x-onshow section)
		)
	      ))

	    #+obsolete
	    ;; Basic structure for visiting all sections at a given level...
	    (visit-sections
	      (lambda (container visitor)
		(let ((node-stack (new Array)))
		  (.push node-stack (@ container first-child))
		  (while (@ node-stack length)
		    (let ((node (.pop node-stack)))
		      (while node
			(when (== 1 (@ node node-type))
			  (if (eq "section" (@ node x-item-type))
			    (visitor node)
			    ;else
			    (when (.has-child-nodes node)
			      (.push node-stack (@ node first-child))
			    ))
			)
			(set node (@ node next-sibling))
		      )
		    )
		  )
		)
	      ))

	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
	    ;; Mouse handler functions
	    (clear-timers 
	      (lambda (timers)
		(while (@ timers length)
		  (clear-timeout (.pop timers))
		)
	      ))
	    (hash-position
	      (lambda (node)
		(+ (.to-string (.compute-scrolled-position Dom-library node)) 
		   ";(" (.get-browser-width Dom-library) "," (.get-browser-height Dom-library) ")"
		   )
	      ))
	    (layout-top-section null) ;; Possibly recursive
	    (layout-sub-section null) ;; Possibly recursive
	    )
	(let* ((make-node-active
		 (lambda (node)
		   (.add-class Dom-library (get-node-header node) "menu-active")
		 ))
	       (make-node-inactive
		 (lambda (node)
		   (.remove-class Dom-library (get-node-header node) "menu-active")
		 ))

	       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
	       ;; Hide sections
	       (hide-section
		 (lambda (id section)
		   (let ((section (or section (.get-element-by-id document id))))
		     (if section
		       (let ((visible-id (.pop our-visible)))
			 (make-node-inactive section)
			 (hide-section-node section)
			 ;; TODO: IE <select> bug fix: hide iframe
			 (if (eq id visible-id)
			   ;; Prevent other calls from trying to hide the section
			   (++ (@ (ref our-sections id) hide))
			   ;else
			   ;; Put it back on the stack.
			   (.push our-visible visible-id)
			 )
			 true
		       )
		       ;else
		       ;; section no longer part of the document
		       false
		     )
		   )
		 ))
	       (hide-section-guarded
		 (lambda (id section guard guard-all)
		   (when (eq guard (@ (ref our-sections id) hide))
		     (if (and (eq guard-all our-sections-hide-guard)
			      (eq id (ref our-visible (1- (@ our-visible length))))
			      )
		       (while (@ our-visible length)
			 (unless (hide-section (ref our-visible (1- (@ our-visible length))))
			   (.pop our-visible)
			 )
		       )
		       ;else
		       (hide-section id section)
		     )
		   )
		 ))
	       (hide-section-callback
		 ;; Returns a callback that calls hide-section-guarded
		 ;; saves the current value of our-sections[ id ].hide for comparison
		 (lambda (id node)
		   (let ((guard (@ (ref our-sections id) hide))
			 (guard-all our-sections-hide-guard)
			 (section (and node (get-node-section node)))
			 )
		     (lambda () (hide-section-guarded id section guard guard-all)))))
	       (hide-non-ancestor-sections
		 ;; Hide visible sections which aren't parents of this node
		 (lambda (node)
		   (let ((i (@ our-visible length)))
		     (when i
		       (let ((parents (get-parent-sections node)))
			 (while (and (< 0 i)
				     (not (ref parents (ref our-visible (-- i))))
				     )
			   (hide-section (ref our-visible i))
			 )
		       )
		     )
		   )
		 ))

	       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
	       ;; Show sections
	       (show-section-guarded
		 (lambda (id section guard)
		   (when (eq guard (@ (ref our-sections id) show))
		     (let ((section (or section (.get-element-by-id document id))))
		       ;; Prevent current show-section calls from running
		       (++ (@ (ref our-sections id) show))
		       (hide-non-ancestor-sections section)
		       (show-section-node section)
		       ;; TODO: IE <select> bug fix: show iframe
		       (.push our-visible id)
		     )
		   )
		 ))
	       (show-section-callback
		 ;; Returns a callback that calls show-section-guarded
		 ;; saves the current value of our-sections[ id ].show for comparison
		 (lambda (id node)
		   (let ((guard (@ (ref our-sections id) show))
			 (section (and node (get-node-section node)))
			 )
		     (lambda () (show-section-guarded id section guard)))))

	       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
	       ;; Show headers
	       (show-header-guarded
		 (lambda (id node guard)
		   (when (eq guard our-header-shown-guard)
		     (++ our-header-shown-guard)
		     (hide-non-ancestor-sections node)
		   )
		 ))
	       (show-header-callback
		 ;; Returns a callback that calls show-section-guarded
		 ;; saves the current value of our-header-shown-guard for
		 ;; comparison
		 (lambda (id node)
		   (let ((guard our-header-shown-guard))
		     (lambda () (show-header-guarded id node guard)))))

	       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
	       ;; Common mouse handlers
	       (header-mouseover-handler
		 (lambda (e)
		   (when (not (.is-scrolling Scroll-bar "menu-scroll-bar"))
		     (let ((id (+ (@ this id) "-section")))
		       ;; Prevent any previous calls to show-header
		       (++ our-header-shown-guard)
		       (when (@ our-visible length)
			 (make-node-inactive (.get-element-by-id document (ref our-visible (1- (@ our-visible length)))))
		       )
		       (cond
			 ((ref our-sections id)
			   (clear-timers our-timers)
			   ;; Prevent the section from being hidden
			   (++ (@ (ref our-sections id) hide))
			   ;; Prevent hidding other sections too
			   (++ our-sections-hide-guard)
			   (.push our-timers (set-timeout (show-section-callback id this) our-delay-show))
			 )
			 ((@ our-visible length)
			   (clear-timers our-timers)
			   (.push our-timers (set-timeout (show-header-callback (@ this id) this) our-delay-show))
			 )
		       )
		     )
		   )
		 ))
	       (header-mouseout-handler
		 (lambda (e)
		   (when (and (not (.is-scrolling Scroll-bar "menu-scroll-bar"))
			      (.is-mouseexit Event-library e)
			      )
		     (let ((id (+ (@ this id) "-section")))
		       ;; Prevent headers from being shown
		       (++ our-header-shown-guard)
		       (when (ref our-sections id)
			 ;; Prevent the section from being shown
			 (++ (@ (ref our-sections id) show))
			 ;; See if it is visible
			 (dotimes (i (@ our-visible length))
			   (when (eq id (ref our-visible i))
			     (.push our-timers (set-timeout (hide-section-callback id this) our-delay-hide))
			     ;; Exit the loop
			     (set i (@ our-visible length))
			   )
			 )
		       )
		     )
		   )
		 ))
	       (section-mouseover-handler
		 (lambda (e)
		   (when (not (.is-scrolling Scroll-bar "menu-scroll-bar"))
		     (let ((id (@ this id)))
		       ;; Prevent the section from being hidden
		       (++ (@ (ref our-sections id) hide))
		       ;; Prevent hidding other sections too
		       (++ our-sections-hide-guard)
		       (make-node-active this)
		       (.stop-propagation Event-library e)
		     )
		   )
		 ))
	       (section-mouseout-handler
		 (lambda (e)
		   (when (and (not (.is-scrolling Scroll-bar "menu-scroll-bar"))
			      (.is-mouseexit Event-library e)
			      )
		     (let ((id (@ this id)))
		       ;; Prevent the section from being shown
		       (++ (@ (ref our-sections id) show))
		       (.push our-timers (set-timeout (hide-section-callback id this) our-delay-hide))
		       (.stop-propagation Event-library e)
		     )
		   )
		 ))
	       )

	  ;; Recursive functions...
	  (set parse-menu
	    (lambda (menu-id handlers container id tree)
	      (let ((id (or id ""))
		    (tree (or tree (new Array)))
		    (node-stack (new Array))
		    )
		(.push node-stack (@ container first-child))
		(while (@ node-stack length)
		  (let ((node (.pop node-stack))
			(header-id null)
			(header-type "subheader")
			(header-tree null)
			(is-section false)
			)
		    (while node
		      (when (== 1 (@ node node-type))
			(set is-section false)
			(cond
			  ((.has-class Dom-library node "menu-header")
			    (set header-id (+ id "-" (@ tree length)))
			    (set header-type "header")
			    (.push tree (set header-tree (new Array)))
			    (setup-parsed-node handlers menu-id header-id node header-id "header")
			  )
			  ((.has-class Dom-library node "menu-subheader")
			    (set header-id (+ id "-" (@ tree length)))
			    (set header-type "subheader")
			    (.push tree (set header-tree (new Array)))
			    (setup-parsed-node handlers menu-id header-id node header-id "subheader")
			  )
			  ((.has-class Dom-library node "menu-section")
			    ;; NOTE: Sections *follow* the headers they are associated with
			    (hide-section-node node)
			    (set is-section true)
			    ;; Must have an header-id
			    (when header-id
			      (setup-parsed-node handlers menu-id header-id node (+ header-id "-section") (+ header-type "Section"))
			      (set (ref our-sections (@ node id)) (new-guard))

			      ;; TODO: Apply an <SELECT> IE bug fix

			      ;; Ensure that the menu has at least one element
			      (unless (.has-child-nodes (.open-container menu-constructor node))
				(.append-child (.open-container menu-constructor node) (html ((:span :class "menu-empty menu-section-header menu-subheader") "(empty)")))
			      )
			      ;; Tag the container as having a submenu
			      (set (@ (get-node-container node) x-has-sub-sections) true)

			      ;; Visit the submenu
			      (parse-menu menu-id handlers node header-id header-tree)
			    )
			  )
			  ((.has-class Dom-library node "menu-section-contents")
			    ;; Only present as a direct child of a
			    ;; "menu-section" when using the Scroll-bar class.
			    ;; Will automatically descend into and parse
			    ;; the menu entries.
			    (set (@ node id) (get-node-contents-id (@ node parent-node)))
			  )
			)
			(when (and (not is-section) (.has-child-nodes node))
			  (.push node-stack (@ node first-child))
			)
		      )
		      (set node (@ node next-sibling))
		    )
		  )
		)
	      )
	    ))

	  ;; Layout the top level menu.  The "section" is below an
	  ;; "menu-header".  Current behaviour is specific to FireFox's
	  ;; box model.
	  (set layout-top-section
	    (lambda (node use-fixed-position)
	      (when use-fixed-position
		(set (@ node style position) "fixed")
	      )
	      ;; Root of a menu, place it above or below the header (menu-header).
	      (let ((browser-height (- (.get-browser-height Dom-library) (* our-browser-padding 2)))
		    (browser-width (- (.get-browser-width Dom-library) (* our-browser-padding 2)))
		    (header (get-node-header node))
		    (style (copy-menu-style-properties node))
		    )
		(reset-section-node node)
		(let ((header-position (.compute-scrolled-position Dom-library header))
		      (header-width (@ header offset-width))
		      (header-height (@ header offset-height))
		      (node-width (@ node offset-width))
		      (node-height (@ node scroll-height))
		      (border-height (+ (@ style border-top-width)
					(@ style border-bottom-width)))
		      )
		  (let ((header-top (@ header-position top))
			(header-left (@ header-position left))
			(node-top (+ (or (and use-fixed-position (@ header-position top))
		      			 (@ header offset-top)
					 )
				     header-height))
			(node-left (or (and use-fixed-position (@ header-position left))
		      		       (@ header offset-left)
				       ))
			)
		    ;; Determine vertical placement and height
		    (cond
		      ((> (+ our-browser-padding browser-height our-browser-padding) (+ header-top header-height node-height))
			;; Menu is below the header
		      )
		      ((> our-browser-padding (- header-top node-height))
			;; Menu is below the header with a scroll bar
			(+= node-width our-scrollbar-width)
			(set (@ node style height) (+ (- browser-height (+ header-top header-height border-height)) our-browser-padding "px"))
			(set (@ node style width) (+ node-width "px"))
			(.attach (get-scroll-bar node) (get-node-contents-id node))
		      )
		      (true
			;; Menu is above the header
			(-= node-top (+ header-height node-height))
		      ))

		    (set (@ node style top) (+ node-top "px"))

		    ;; Determeine horizontal placement
		    ;; TODO: Special case IE 5
		    (cond
		      ((> browser-width (+ header-left node-width))
			;; Menu flows to the right
		      )
		      ((> (+ our-browser-padding node-width) (+ header-left header-width))
			;; Menu won't fit either way, so just put it at the edge of the window. 
			(+= node-left (- our-browser-padding header-left))
		      )
		      (true
			;; Menu flows to the left
			(+= node-left (- header-width node-width))
		      ))

		    ;; Hack to ensure proper rendering when the width of the
		    ;; title is longer than the width of the top menu.
		    ;; Only applied to palette menus ATM.
		    (when (and (not use-fixed-position) 
		  	       (> (- header-width node-left) node-width)
			       )
		      (set (@ node style width) (+ (- header-width node-left) "px"))
		    )

		    (set (@ node style left) (+ node-left "px"))

		    ;; Recursively proccess the submenus
		    #+obsolete
		    (when (.has-child-nodes node)
		      (visit-sections node (lambda (node) (layout-sub-section node node-width style)))
		    )
		  )
		)
		(hide-section-node node)
	      )
	    ))
	  ;; Layout sub level menu.  The "section" is below an "menu-subheader".
	  ;; Current behaviour is specific to FireFox's box model.
	  (set layout-sub-section
	    (lambda (node section-width section-style)
	      (let ((browser-height (- (.get-browser-height Dom-library) (* our-browser-padding 2)))
		    (browser-width (- (.get-browser-width Dom-library) (* our-browser-padding 2)))
		    (left-offset (- 0 (@ section-style border-left-width)))
		    (top-offset (- 0 (@ section-style border-top-width)))
		    (header (get-node-header node))
		    (style (copy-menu-style-properties node))
		    )
		(reset-section-node node)
		(let ((header-position (.compute-scrolled-position Dom-library header))
		      (node-width (@ node offset-width))
		      (node-height (@ node scroll-height))
		      (border-height (+ (@ style border-top-width)
					(@ style border-bottom-width)))
		      )
		  (let ((header-top (+ (@ header-position top) our-sub-top-offset top-offset))
			(header-left (+ (@ header-position left) our-sub-left-offset left-offset))
			(node-top (- (@ header offset-top) (@ header parent-node scroll-top)))
			(node-left (@ header offset-left))
			)
		    ;; Determine vertical placement and height
		    (cond
		      ((> browser-height (+ header-top node-height))
			;; Menu is next to the header
			(+= node-top (+ our-sub-top-offset top-offset))
		      )
		      ((> browser-height node-height)
			;; Menu is above the header
			(+= node-top (+ (- browser-height (+ header-top node-height border-height)) our-sub-top-offset top-offset our-browser-padding))
		      )
		      (true
			;; Menu is at the top of the screen with a scrollbar
			(+= node-width our-scrollbar-width)
			(+= node-top (+ (- 0 header-top border-height) our-sub-top-offset top-offset our-browser-padding))
			(set (@ node style height) (+ (- browser-height border-height) "px"))
			(set (@ node style width) (+ node-width "px"))
			(.attach (get-scroll-bar node) (get-node-contents-id node))
		      ))

		    (set (@ node style top) (+ node-top "px"))

		    ;; Determine horizontal placement
		    (cond
		      ((> browser-width (+ header-left section-width node-width))
			;; Menu flows to the right
			(+= node-left (+ section-width our-sub-left-offset left-offset))
		      )
		      ((> our-browser-padding (- header-left node-width))
			;; Menu overlaps the original menu
			(let ((overlap (.max Math our-remaining-overlap-min (- browser-width (+ header-left node-width)))))
			  (+= node-left (+ overlap our-sub-left-offset left-offset))
			)
		      )
		      (true
			;; Menu flows to the left
			(+= node-left (- left-offset node-width our-sub-left-offset))
		      ))

		    ;; NOTE: If the top menu is position: fixed and node-left
		    ;; is less than about 10, then the submenu appears
		    ;; distored...
		    ;; Why 10?  I don't know...
		    (if (> 10 node-left)
		      (progn
			(set (@ node style left) "")
			(set (@ node style right) (+ (@ header offset-width) node-width node-left our-sub-left-offset our-sub-left-offset "px"))
		      )
		      ;else
		      (progn
			(set (@ node style right) "")
			(set (@ node style left) (+ node-left "px"))
		      ))

		    ;; Recursively process any submenus
		    #+obsolete
		    (when (.has-child-nodes node)
		      (visit-sections node (lambda (node) (layout-sub-section node node-width style)))
		    )
		  )
		)
		(hide-section-node node)
	      )
	    ))

	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; Constructor
	  ;;
	  ;; Takes the ID of the menu and whether to use a fixed position
	  ;; (non-palette only).
	  ;; There are a lot of menus, so keep this brief.
	  (set menu-constructor 
	    (lambda (menu-id use-fixed-position)
	      (let ((my-self this) ;; Must come first
		    (my-menu-id menu-id)
		    (my-handlers (new Object))
		    (my-displayed "")
		    (my-use-fixed-position use-fixed-position)
		    (my-modify-stacking-context use-fixed-position)
		    )
		(define-class-method (my-handlers header-mouseover) (e)
		  (let* ((menu (.get-element-by-id document my-menu-id))
			 (displayed (hash-position menu))
			 )
		    ;; Check that the root of the menu hasn't moved or the display adjusted
		    (unless (eq my-displayed displayed)
		      (set my-displayed displayed)
		      ;; TODO: If IE call (fix-sections menu)
		      (let ((section (get-node-section this)))
			(unless (eq section our-dummy-node)
			  (layout-top-section section my-use-fixed-position)
			)
		      )
		    )
		    (unless (.is-dragging Jbml)
		      ;; Call with the original parameters
		      (.call header-mouseover-handler this e)
		    )
		  ))
		(define-class-method (my-handlers subheader-mouseover) (e)
		  (let ((this-displayed (or (@ this x-displayed) ""))
			(displayed-scrolled (+ my-displayed ";" (or (@ this parent-node scroll-top) 0)))
			)
		    ;; Check that the root of the menu hasn't moved or the display adjusted
		    (unless (eq this-displayed displayed-scrolled)
		      (set (@ this x-displayed) displayed-scrolled)
		      (let ((section (get-node-section this))
		    	    ;; Will always have a container
			    (container (get-node-container this))
			    )
			(unless (eq section our-dummy-node)
			  (layout-sub-section section (@ container offset-width) (copy-menu-style-properties container))
			)
		      )
		    )
		    ;; Call with the original parameters
		    (.call header-mouseover-handler this e)
		  ))
		(set (@ my-handlers header-mouseout) header-mouseout-handler)
		(set (@ my-handlers subheader-mouseout) header-mouseout-handler)

		(set (@ my-handlers header-section-mouseover) section-mouseover-handler)
		(set (@ my-handlers subheader-section-mouseover) section-mouseover-handler)
		(set (@ my-handlers header-section-mouseout) section-mouseout-handler)
		(set (@ my-handlers subheader-section-mouseout) section-mouseout-handler)

		(when my-modify-stacking-context
		  (set (@ my-handlers header-section-show)
		    (lambda ()
		      (let ((menu (.get-element-by-id document my-menu-id)))
			(.raise-stacking-context Dom-library menu)
		      )
		    ))
		  (set (@ my-handlers header-section-hide)
		    (lambda ()
		      (let ((menu (.get-element-by-id document my-menu-id)))
			(.lower-stacking-context Dom-library menu)
		      )
		    ))
		)

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;; Public instance functions

		;; Initialize the menu.
		(define-class-method (my-self quickinit) ()
		  (let ((menu (.get-element-by-id document my-menu-id)))
		    (unless menu
		      (throw (+ "Unable to locate menu : " my-menu-id))
		    )
		    ;; TODO: If IE5 call (fix-text-wrapping ...)
		    (set (@ menu onmousedown) (@ Event-library stop-propagation)) ;; Prevent others from getting a hold of our events
		    (parse-menu my-menu-id my-handlers menu)
		  ))
		
		;; Return the Menu object
		my-self
	      )))

	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; Public static functions

	  ;; Creates a container to use for a menu.
	  (set (@ menu-constructor make-container) 
	    (lambda ()
	      (html ((:div :class "menu-section")
		      ((:div :class "menu-section-contents")
			)
		      ))))

	  ;; Locate the inner node of the menu container.
	  (define-class-method (menu-constructor open-container) (node)
	    (or (@ node last-child) node))

	  ;; Close all visibile menus.
	  (define-class-method (menu-constructor close-menus) ()
	    (let ((i (@ our-visible length)))
	      (when i
		(while (< 0 i)
		  (-- i)
		  (hide-section (ref our-visible i))
		)
	      )
	    ))

	  ;; Retrieve the ID of the currently visible menu.
	  (define-class-method (menu-constructor get-visible-id) ()
	    (and (@ our-visible length) (ref our-visible 0)))

	  ;; Return the Menu constructor object
	  menu-constructor
	))))
)


(defvar Jbml-base
	 ;; Private static variables
  (let ((our-class (lambda () (and "Jbml-base" this))) ;; Class constructor
	(our-invalid-id "-1")
	)
    (let ((our-instance (new our-class)) ;; Class singleton instance
	  (our-dummy (new Object))
	  (our-default-border-style "jbml-solid")
	  (our-default-border-width "jbml-thin")
	  )
      (defmethod (our-class get-invalid-id) () our-invalid-id)
      
      ;; Provide accessors for accessing static Jbml class instance stored on Jbml elements
      (defmethod (our-class set-jbml) (node) ;; Private function
	(set (@ (or node (new Object)) x-jbml) this))
      (defmethod (our-class get-jbml) (node)
	(or (@ (or node our-dummy) x-jbml) our-instance))

      ;; Accessor for the "box-id"
      (defmethod (our-class set-box-id) (node box-id)
	(set (@ (or node (new Object)) x-box-id) box-id))
      (defmethod (our-class get-box-id) (node)
	(or (@ (or node our-dummy) x-box-id) our-invalid-id))
      ;; Event-centric accessor for "box-id"
      (defmethod (our-class get-event-box-id) (event-this e node)
	(.get-box-id this (or (.get-current-target Event-library event-this e) node)))

      (defmethod (our-class set-box-name) (node name)
	(set (@ (or node (new Object)) x-box-name) name))
      (defmethod (our-class get-box-name) (node)
	(or (@ (or node our-dummy) x-box-name) (.get-box-id this node)))

      ;; This breaks the defmethod macro, because it tries to return an if statement
      (set (@ our-class prototype update-class) 
	(lambda (box old-class new-class)
	  (unless (eq old-class new-class)
	    (.remove-class Dom-library box old-class)
	    (+= (@ box class-name) (+ " " new-class))
	  )
	))

      (defmethod (our-class set-border-color) (box border-color)
	(set (@ box x-box-border-color) (set (@ box style border-color) (or border-color ""))))
      (defmethod (our-class reset-border-color) (box)
	(.set-border-color this box (@ box x-box-border-color)))
      ;; This breaks the defmethod macro, because it can't handle multiple statements
      (set (@ our-class prototype set-border-style) 
	(lambda (box border-style)
	  (.update-class this box (or (@ box x-box-border-style) our-default-border-style) border-style)
	  (set (@ box x-box-border-style) border-style)
	))
      ;; This breaks the defmethod macro, because it can't handle multiple statements
      (set (@ our-class prototype set-border-width)
	(lambda (box border-width)
	  (.update-class this box (or (@ box x-box-border-width) our-default-border-width) border-width)
	  (set (@ box x-box-border-width) border-width)
	))

      ;; This breaks the defmethod macro, because it can't handle multiple statements
      (set (@ our-class prototype init)
	(lambda (box box-id)
	  (.set-jbml this box)
	  (.set-box-id this box box-id)
	))

      (defmethod (our-class make-single-click-handler) (js-func-name) ;; Protected function
	(lambda (e)
	  (let ((box-id (.get-event-box-id our-instance this e))
		(button (.get-button Event-library e))
		)
	    (single-click-wrapper this (lambda () (js-func-name box-id button)))
	    (.stop-propagation Event-library e)
	  )))

	    ;; Event handlers
      (let ((single-click-handler 
	      (.make-single-click-handler our-instance package-box-mouse-click))
	    (double-click-handler 
	      (lambda (e)
		(let ((box-id (.get-event-box-id our-instance this e))
		      (button (.get-button Event-library e))
		      )
		  (clear-mouse-delay-timer this)
		  (package-box-mouse-double-click box-id button)
		  (.stop-propagation Event-library e)
		)))
	    (contextmenu-handler
	      (lambda (e)
		(let ((box-id (.get-event-box-id our-instance this e))
			(button (@ Event-library right-mouse-button))
			)
		  (package-box-mouse-click box-id button)
		  (set last-mouse-x (.get-page-x Event-library e))
		  (set last-mouse-y (.get-page-y Event-library e))
		  (.stop-propagation Event-library e)
		)))
	    (mousedown-handler
	      (lambda (e)
	        (when (or (eq (@ Event-library right-mouse-button) (.get-button Event-library e))
			  (.has-class Dom-library this "hilight-jbml")
			  )
		  (.stop-propagation Event-library e)
		)
		true
	      ))
	    )
	;; This breaks the defmethod macro, because it doesn't properly return values
	(set (@ our-class prototype create-box) 
	  (lambda (box-id)
	    (let* ((class-name (+ "a-jbml box-jbml " our-default-border-style " " our-default-border-width))
		   (box (html ((:div :id box-id :class class-name))))
		   )
	      (.init this box box-id)
	      (set (@ box style color) global-default-text-color)

	      (set (@ box onclick)       single-click-handler)
	      (set (@ box ondblclick)    double-click-handler)
	      (set (@ box oncontextmenu) contextmenu-handler)
	      (set (@ box onmousedown)   mousedown-handler)

	      box
	    )))

	(defmethod (our-class create-box-container) (box parent-box) box)

	(defmethod (our-class removing-child) (box child) (removing-child box child))
	(defmethod (our-class replacing-child) (box new-child old-child) (replacing-child box new-child old-child))
      )

      (.set-box-id our-instance our-dummy our-invalid-id)
      (.set-box-name our-instance our-dummy "unknown")

      ;; Using prototype inheritance with static methods, returns a singleton
      (set (@ our-instance constructor) our-class)
      our-instance
    ))
)


(defvar Jbml-base-structure
  (let ((our-class (lambda () (and "Jbml-base-structure" this))) ;; Class constructor
	(our-parent-class Jbml-base)
	(our-invalid-id (.get-invalid-id Jbml-base))
	)
    (set (@ our-class prototype) (new (@ our-parent-class constructor))) ;; Set parent class

    (let ((our-instance (new our-class)) ;; Class singleton instance
	  (our-dummy (new Object))
	  )
      (defmethod (our-class set-parent-id) (node box-id)
	(set (@ (or node (new Object)) x-parent-id) box-id))
      (defmethod (our-class get-parent-id) (node)
	(or (@ (or node our-dummy) x-parent-id) our-invalid-id))

      (set (@ our-class prototype get-parent)
	(lambda (node)
	  (let ((parent-id (.get-parent-id this node)))
	    (if (or (eq parent-id our-invalid-id)
		    (eq parent-id (@ node id))
		    )
	      false
	      ;else
	      (progn
		(while (and node
			    (not (eq (@ node id) parent-id))
			    )
		  (set node (@ node parent-node))
		)
		(or node
		    (and parent-id (.get-element-by-id document parent-id))
		    false
		    )
	      )))))

      (let* ((get-jbml-box
	       (lambda (box)
	         (let ((box-id (.get-box-id our-instance box)))
		   (if (eq box-id our-invalid-id)
		     our-dummy
		     ;else
		     (or (and (eq box-id (@ box id))
			      box
			      )
			 (and (@ box first-child)
		     	      (eq box-id (@ box first-child id))
			      (@ box first-child)
			      )
		         (.get-element-by-id document box-id)
			 )
		     ))))
	     (get-child-boxes
	       (lambda (box)
		 (or (@ box x-child-boxes)
		     (or (set (@ box x-child-boxes) (new Object)))
		     )))
	     (get-child-box-type
	       (lambda (box type)
		 (let ((boxes (get-child-boxes box)))
		   (or (ref boxes type)
		       (or (set (ref boxes type) (new Object)))
		       )
		 )
	       ))
	     (set-value
	       (lambda (node box type value)
		 (set (ref (get-child-box-type node type) (@ box id)) value)
	       ))
	     (remove-values
	       (lambda (node box)
		 (doslots ((type boxes) (get-child-boxes box))
		   (let ((node-boxes (get-child-box-type node type)))
		     (doslots ((child value) boxes)
		       (delete (ref node-boxes child))
		     )
		   )
		 )
	       ))
	     ) 
	(set (@ our-class prototype create-box-container)
	  (lambda (box parent-box)
	    (let ((node parent-box))
	      (while (and node
			  (or (eq "" (@ node id))
			      (eq (.get-box-id this node) (.get-box-id this box))
			      )
			  )
		(set node (@ node parent-node))
	      )
	      (when node
		(.set-parent-id this box (@ node id))
	      )
	    )
	    (.create-box-container our-parent-class box parent-box)
	  ))

	(set (@ our-class prototype removing-child)
	  (lambda (box child)
	    (.remove-box-value this (get-jbml-box child))
	    (.removing-child our-parent-class box child)
	  ))

	(set (@ our-class prototype replacing-child)
	  (lambda (box new-child old-child)
	    (.remove-box-value this (get-jbml-box old-child))
	    (.replacing-child our-parent-class box new-child old-child)
	  ))

	(set (@ our-class prototype set-box-value)
	  (lambda (box type value)
	    (let ((node box))
	      (while node
		(set-value node box type value)
		(set node (.get-parent this node))
	      )
	    )
	  ))

	(set (@ our-class prototype remove-box-value)
	  (lambda (box)
	    (when (and box 
		       (@ box x-child-boxes)
		       )
	      (let ((node (.get-parent this box)))
		(while node
		  (remove-values node box)
		  (set node (.get-parent this node))
		)
		;; Must come last so that there is something to remove
		(remove-values box box)
	      )
	    )
	  ))

	(set (@ our-class prototype visit-child-boxes)
	  (lambda (node type visit)
	    (doslots ((box-id value) (get-child-box-type node type))
	      (visit box-id value)
	    )
	  ))
      )
      
      (.set-parent-id our-instance our-dummy our-invalid-id)
      ;; Using prototype inheritance with static methods, returns a singleton
      (set (@ our-instance constructor) our-class)
      our-instance
    ))
)


(defvar Jbml-base-dnd
	 ;; Private static variables
  (let ((our-class (lambda () (and "Jbml-base-dnd" this))) ;; Class constructor
	(our-parent-class Jbml-base-structure)
	(our-invalid-id (.get-invalid-id Jbml-base))
	)
    (set (@ our-class prototype) (new (@ our-parent-class constructor))) ;; Set parent class

    (let ((our-instance (new our-class)) ;; Class singleton instance
	  (our-dummy (new Object))
	  (our-drag (new Drag))
	  (our-position-cache null)
	  (our-dragged-id our-invalid-id)
	  (our-drop-id our-invalid-id)
	  (our-drop-where "")
	  (our-forced-set-drop-id false)
	  
	  (drop-container-mousemove-handler null) ;; Needs to reference itself
	  )
      (defmethod (our-class is-dragging) ()
	(.is-dragging our-drag))

	     ;; Private Drag-n-Drop related functions
      (let* ((set-drop-id
	       (lambda (box-id where)
		 (when (and our-forced-set-drop-id
		 	    (not (eq our-drop-id our-invalid-id))
			    )
		   (let ((top-level (@ (.get-element-by-id document our-drop-id) parent-node)))
		     (set our-forced-set-drop-id false)
		     (.remove-class Dom-library top-level (+ "dragged-" our-drop-where))
		     (.remove-class Dom-library (@ top-level parent-node) "drag-forced")
		   )
		 )
		 (set our-drop-id (or box-id our-invalid-id))
		 (set our-drop-where (or where ""))
	       ))

	     (force-drop ;; Calls set-drop-id
	       (lambda (before after)
		 (when (.is-dragging our-instance)
		   (let ((box-id (.get-box-id our-instance (or after before)))
			 (where (or (and after "after") 
			 	    (and before "before")
				    ""
				    ))
			 )
		     (unless (and (eq box-id our-drop-id)
		     		  (eq where our-drop-where)
				  our-forced-set-drop-id
				  )
		       (let ((top-level (@ (.get-element-by-id document box-id) parent-node)))
		         (set-drop-id box-id where)
			 (set our-forced-set-drop-id true)
			 (.add-class Dom-library top-level (+ "dragged-" our-drop-where))
			 (.add-class Dom-library (@ top-level parent-node) "drag-forced")
		       )
		     )
		   )
		 )
	       ))

	     (get-drag-timer 
	       (lambda (node)
		 (@ node x-drag-timer)))

	     (set-draggable
	       (lambda (node draggable)
		 (set (@ (or node (new Object)) x-box-draggable) draggable)))
	     (get-draggable
	       (lambda (node)
		 (or (@ (or node our-dummy) x-box-draggable) false)))

	     (get-droppable
	       (lambda (node)
		 (or (@ (or node our-dummy) x-box-droppable) false)))
	     (set-droppable ;; Calls get-droppable
	       (lambda (node droppable)
		 (let ((node (or node (new Object))))
		   (set (@ node x-box-previous-droppable) (get-droppable node))
		   (set (@ node x-box-droppable) droppable)
		 )))
	     (reset-droppable ;; Calls set-droppable
	       (lambda (node)
		 (set-droppable node (@ (or node our-dummy) x-box-previous-droppable))))

	     (make-get-containing-box-id
	       (lambda (test)
		 (lambda (node)
		   (while (or (eq our-invalid-id (.get-box-id our-instance node))
			      (not (test node))
			      )
		     (set node (@ node parent-node))
		   )
		   (.get-box-id our-instance node)
		 )
	       ))
	     
	     (get-draggable-box-id (make-get-containing-box-id get-draggable))
	     (get-droppable-box-id (make-get-containing-box-id get-droppable))

	     (disable-child-drop
	       (lambda (box-id value)
		 (when value
		   (let ((box (.get-element-by-id document box-id)))
		     (.disable-drop our-instance box)
		     (.set-box-value our-instance box "dnd-drop" true)
		   )
		 )
	       ))
	     (reset-child-drop
	       (lambda (box-id value)
		 (.reset-drop our-instance (.get-element-by-id document box-id))
	       ))

	     ;; Begins dragging after a drag-click-delay.
	     ;; Closes any open menus, otherwise their event handlers would fire during DND.
	     ;; Creates a clone of the dragged item and attaches it to the "drag-proxy" layer.
	     ;; Adds the "dragging" CSS class to the document root.  Adds an event handler
	     ;; for selectstart to prevent IE from selecting text during DND.  Starts the
	     ;; dragging.  However, it creates thet dragged object immediately.
	     (start-drag
	       (lambda (box-id e)
		 (clear-element "drag-proxy")
		 (.close-menus Menu)
		 (let ((box (.get-element-by-id document box-id))
		       (proxy (.get-element-by-id document "drag-proxy"))
		       (root (.get-root-element Dom-library))
		       )
		   (.set-box-id our-instance proxy (set our-dragged-id box-id))
		   (set (@ proxy style left) (+ (.get-page-x Event-library e) 5 "px"))
		   (set (@ proxy style top) (+ (.get-page-y Event-library e) 5 "px"))
		   (set (@ proxy style width) (+ (@ box offset-width) 2 "px"))
		   ;; For some bizarre reason, all of the final styles must be applied before
		   ;; the style is copied by the Drag code.  However, we remove the style
		   ;; immediately since we aren't quite ready to drag yet.
		   (.add-class Dom-library root "dragging")
		   (.prep our-drag proxy e)
		   ;; Remove the dragging class.
		   (.remove-class Dom-library root "dragging")

		   (single-click-wrapper (get-drag-timer box)
		     (lambda ()
		       (.append-child proxy (.clear-ids Dom-library (.clone-node box true)))
		       (.visit-child-boxes our-instance box "dnd-drop" disable-child-drop)
		       (.add-class Dom-library box "drag-source")
		       (.add-class Dom-library root "dragging")
		       (.prep-start our-drag)
		       (show-client-status (+ "Dragging " (.get-box-name our-instance box)))
		     )
		     (.get client-settings "drag-click-delay")
		   )
		 )))

	     (make-drop-mouseover-handler ;; Calls set-drop-id
	       (lambda (where)
		 ;; While dragging, updates our-drop-id.
		 (lambda (e)
		   (when (.is-dragging our-instance)
		     (set-drop-id (.get-event-box-id our-instance this e) where)
		     (.stop-propagation Event-library e)
		   )
		 )))
	     (make-drop-mouseout-handler ;; Calls set-drop-id
	       (lambda (where)
		 (lambda (e)
		   (when (and (.is-dragging our-instance)
			      (eq where our-drop-where)
			      (eq our-drop-id (.get-event-box-id our-instance this e))
			      (eq our-drop-id (.get-box-id our-instance (.get-target Event-library e)))
			      )
		     (set-drop-id)
		     (.stop-propagation Event-library e)
		   ) 
		 )))
	     
	     (create-position-cache
	       (lambda ()
		 (set our-position-cache (new Position-cache))))
	     )

	      ;; Event handlers
	(let (
	      ;; Cancel any dragging
	      (drag-mouseup-handler
		(lambda (e)
		  (clear-mouse-delay-timer (get-drag-timer this))
		))
	      ;; Cancel any dragging
	      (drag-mousemove-handler
		(lambda (e)
		  (clear-mouse-delay-timer (get-drag-timer this))
		))
	      ;; Performs the same functions as mousedown-handler.
	      (drag-mousedown-handler
		(lambda (e)
		  (let ((box-id (.get-event-box-id our-instance this e))
			)
		    (when (and (eq (@ Event-library left-mouse-button) (.get-button Event-library e))
			       (eq box-id (get-draggable-box-id (.get-target Event-library e)))
			       (not (.has-class Dom-library this "hilight-jbml"))
			       )
		      (start-drag box-id e)
		      (.stop-propagation Event-library e)
		    )
		  )
		))
	      ;; Called when dragging is finished.  Undoes drag-mousedown-handler and checks 
	      ;; if our-drop-id is valid.
	      (stop-drag-handler
		(lambda (e)
		  (.remove-class Dom-library (.get-root-element Dom-library) "dragging")
		  (clear-element "drag-proxy")
		  (when (not (eq our-dragged-id our-invalid-id))
		    (let ((box (.get-element-by-id document our-dragged-id)))
		      (.remove-class Dom-library box "drag-source")
		      (.visit-child-boxes our-instance box "dnd-drop" reset-child-drop)
		      (when (not (eq our-drop-id our-invalid-id))
			(case our-drop-where
			  ("on"
			    (package-dnd-box our-dragged-id our-drop-id box)
			  )
			  ("after"
			    (package-dnd-box-after our-dragged-id our-drop-id box)
			  )
			  ("before"
			    (package-dnd-box-before our-dragged-id our-drop-id box)
			  )
			  (t
			    ;; FIXME: Animate the box returning on failed drop
			  )
			)
		      )
		      (set-drop-id)
		    )
		    (set our-dragged-id our-invalid-id)
		  )
		))
	      (drop-mouseover-handler (make-drop-mouseover-handler "on"))
	      (drop-mouseout-handler (make-drop-mouseout-handler "on"))
	      (drop-after-mouseover-handler (make-drop-mouseover-handler "after"))
	      (drop-after-mouseout-handler (make-drop-mouseout-handler "after"))
	      (drop-before-mouseover-handler (make-drop-mouseover-handler "before"))
	      (drop-before-mouseout-handler (make-drop-mouseout-handler "before"))
	      
	      (drop-container-mouseover-handler
		(lambda (e)
		  (when (and (.is-dragging our-instance)
		  	     (eq (.get-current-target Event-library this e) (.get-target Event-library e))
			     )
		    (let ((nearest (.find-nearest-child (create-position-cache) this (.get-page-x Event-library e) (.get-page-y Event-library e))))
		      (when nearest
			(force-drop (@ nearest after) (@ nearest before))
		      )
		    )
		    (.add-event-listener Event-library this "mousemove" drop-container-mousemove-handler)
		    (.stop-propagation Event-library e)
		  )
		))
	      (drop-container-mouseout-handler
		(lambda (e)
		  (when (and (.is-dragging our-instance)
		  	     our-forced-set-drop-id
		  	     (.is-mouseexit Event-library e)
			     )
		    (set-drop-id)
		    (.remove-event-listener Event-library this "mousemove" drop-container-mousemove-handler)
		    (set (@ this x-drag-last-x) -1)
		    (set (@ this x-drag-last-y) -1)
		    (.stop-propagation Event-library e)
		  )
		))
	      )
	  (set drop-container-mousemove-handler
	    (lambda (e)
	      (when (and (.is-dragging our-instance)
			 (eq (.get-current-target Event-library this e) (.get-target Event-library e))
			 )
		(let ((x (.get-page-x Event-library e))
		      (y (.get-page-y Event-library e))
		      )
		  (unless (and (eq x (@ this x-drag-last-x))
		  	       (eq y (@ this x-drag-last-y))
			       )
		    (let ((nearest (.find-nearest-child (or our-position-cache (create-position-cache)) this x y)))
		      (when nearest
			(set (@ this x-drag-last-x) x)
			(set (@ this x-drag-last-y) y)
			(force-drop (@ nearest after) (@ nearest before))
		      )
		    )
		  )
		)
	      )
	    ))

	  (set (@ our-drag on-stop) stop-drag-handler)

	  ;; This breaks the defmethod macro, because it can't handle multiple statements
	  (set (@ our-class prototype enable-drag)
	    (lambda (box)
	      (set-draggable box true)
	      (set (@ box x-drag-timer) (or (@ box x-drag-timer) (new Object)))
	      (.add-event-listener Event-library box "mouseup" drag-mouseup-handler)
	      (.add-event-listener Event-library box "mousemove" drag-mousemove-handler)
	      (.add-event-listener Event-library box "mousedown" drag-mousedown-handler)
	    ))
	  ;; This breaks the defmethod macro, because it can't handle multiple statements
	  (set (@ our-class prototype disable-drag)
	    (lambda (box)
	      (set-draggable box false)
	      (.remove-event-listener Event-library box "mouseup" drag-mouseup-handler)
	      (.remove-event-listener Event-library box "mousemove" drag-mousemove-handler)
	      (.remove-event-listener Event-library box "mousedown" drag-mousedown-handler)
	    ))

	  ;; This breaks the defmethod macro, because it can't handle multiple statements
	  (set (@ our-class prototype enable-drop)
	    (lambda (box)
	      (set-droppable box true)
	      (.add-class Dom-library box "drag-target")
	      (.add-event-listener Event-library box "mouseout" drop-mouseout-handler)
	      (.add-event-listener Event-library box "mouseover" drop-mouseover-handler)
	      (.set-box-value this box "dnd-drop" true)
	    ))
	  ;; This breaks the defmethod macro, because it can't handle multiple statements
	  (set (@ our-class prototype disable-drop)
	    (lambda (box)
	      (set-droppable box false)
	      (.remove-class Dom-library box "drag-target")
	      (.remove-event-listener Event-library box "mouseout" drop-mouseout-handler)
	      (.remove-event-listener Event-library box "mouseover" drop-mouseover-handler)
	      (.set-box-value this box "dnd-drop" false)
	    ))
	  ;; Sets the drop state to the previous drop state.
	  ;; This breaks the defmethod macro, because it can't handle multiple statements
	  (set (@ our-class prototype reset-drop)
	    (lambda (box)
	      (reset-droppable box)
	      (if (get-droppable box)
		(.enable-drop this box)
		;else
		(.disable-drop this box))
	    ))

	  ;; This breaks the defmethod macro, because it doesn't properly return values
	  (set (@ our-class prototype create-box-container) 
	    (lambda (box parent-box)
	      (let ((box (.create-box-container our-parent-class box parent-box)))
	        (if (not (.has-class Dom-library parent-box "contains-jbml"))
		  box
		  ;else
		  (let ((top (html ((:div :class "top-level-edge top-level-horiz top-level-top"))))
			(left (html ((:div :class "top-level-edge top-level-vert top-level-left")
				      ((:div :class "top-level-hilight"))
				      )))
			(bottom (html ((:div :class "top-level-edge top-level-horiz top-level-bottom"))))
			(right (html ((:div :class "top-level-edge top-level-vert top-level-right")
				       ((:div :class "top-level-hilight"))
				       )))
			(box-id (.get-box-id this box))
			)
		    ;; May get set multiple times
		    (set (@ parent-box onmouseout) drop-container-mouseout-handler)
		    (set (@ parent-box onmouseover) drop-container-mouseover-handler)
		    (.init this top box-id)
		    (.init this left box-id)
		    (set (@ left onmouseout) (set (@ top onmouseout) drop-before-mouseout-handler))
		    (set (@ left onmouseover) (set (@ top onmouseover) drop-before-mouseover-handler))
		    (.init this bottom box-id)
		    (.init this right box-id)
		    (set (@ right onmouseout) (set (@ bottom onmouseout) drop-after-mouseout-handler))
		    (set (@ right onmouseover) (set (@ bottom onmouseover) drop-after-mouseover-handler))
		    (let ((child (html ((:div :class "top-level-jbml")
					 box
					 top
					 left
					 bottom
					 right
					 )))
			  )
		      (.init this child box-id)
		      child
		    ))
		))))
	)

	(.set-box-id our-instance our-dummy our-invalid-id)
	(set-draggable our-dummy false)
	(set-droppable our-dummy false)
      )

      ;; Using prototype inheritance with static methods, returns a singleton
      (set (@ our-instance constructor) our-class)
      our-instance
    ))
)


(defvar Jbml
  (let ((our-class (lambda () (and "Jbml" this)))) ;; Class constructor
    (set (@ our-class prototype) (new (@ Jbml-base-dnd constructor))) ;; Set parent class

    (let ((our-instance (new our-class))) ;; Class singleton instance
      ;; Using prototype inheritance with static methods, returns a singleton
      (set (@ our-instance constructor) our-class)
      our-instance
    ))
)


(defvar Jbml-icon-base
	;; Private static variables
  (let ((our-class (lambda () (and "Jbml-icon-base" this))) ;; Class constructor
	)
    (set (@ our-class prototype) (new (@ Jbml constructor))) ;; Set parent class

    (let ((our-instance (new our-class))) ;; Class singleton instance
      ;; Overrideable functions
      (defmethod (our-class get-icon-src) () (alert "Override Jbml-icon-base.get-icon-src"))
      (defmethod (our-class get-icon-class-name) () "icon-jbml action-icon-jbml")
      (defmethod (our-class single-click-handler) (e) (alert "Override Jbml-icon-base.single-click-handler"))

      ;; This breaks the defmethod macro, because it doesn't properly return values
      (set (@ our-class prototype attach-icon)
        (lambda (box)
	  (let ((src (.get-icon-src this))
		(class-name (.get-icon-class-name this))
		)
	    (let ((icon (html ((:img :width "8" :height "8" :src src :class class-name)))))
	      (.init this icon (.get-box-id this box))
	      (set (@ icon onclick) (@ this single-click-handler))
	      (.append-child box icon)
	      icon
	    ))))

      ;; Using prototype inheritance with static methods, returns a singleton
      (set (@ our-instance constructor) our-class)
      our-instance
    ))
)


(defvar Jbml-icon-delete
	;; Private static variables
  (let ((our-class (lambda () (and "Jbml-icon-delete" this))) ;; Class constructor
	)
    (set (@ our-class prototype) (new (@ Jbml-icon-base constructor))) ;; Set parent class

    (let* ((our-instance (new our-class)) ;; Class singleton instance
	   (class-name (+ (.get-icon-class-name our-instance) " jbml-delete"))
	   )
      (defmethod (our-class get-icon-src) () "redXhandlebox.gif")
      (defmethod (our-class get-icon-class-name) () class-name)
      (set (@ our-class prototype single-click-handler) 
        (.make-single-click-handler our-instance package-box-delete-mouse-click))

      ;; Using prototype inheritance with static methods, returns a singleton
      (set (@ our-instance constructor) our-class)
      our-instance
    ))
)


(defvar Jbml-icon-clear
	;; Private static variables
  (let ((our-class (lambda () (and "Jbml-icon-clear" this))) ;; Class constructor
	)
    (set (@ our-class prototype) (new (@ Jbml-icon-base constructor))) ;; Set parent class

    (let* ((our-instance (new our-class)) ;; Class singleton instance
	   (class-name (+ (.get-icon-class-name our-instance) " jbml-clear"))
	   )
      (defmethod (our-class get-icon-src) () "whitemauveXhandlebox.gif")
      (defmethod (our-class get-icon-class-name) () class-name)
      (set (@ our-class prototype single-click-handler) 
        (.make-single-click-handler our-instance package-box-clear-mouse-click))

      ;; Using prototype inheritance with static methods, returns a singleton
      (set (@ our-instance constructor) our-class)
      our-instance
    ))
)


(defvar Jbml-icon-hole-clear
	;; Private static variables
  (let ((our-class (lambda () (and "Jbml-icon-hole-clear" this))) ;; Class constructor
	)
    (set (@ our-class prototype) (new (@ Jbml-icon-clear constructor))) ;; Set parent class

    (let ((our-instance (new our-class))) ;; Class singleton instance
      (set (@ our-class prototype single-click-handler) 
        (.make-single-click-handler our-instance package-box-clear-hole-mouse-click))

      ;; Using prototype inheritance with static methods, returns a singleton
      (set (@ our-instance constructor) our-class)
      our-instance
    ))
)


(defvar Jbml-icon-clear-delete
	;; Private static variables
  (let ((our-class (lambda () (and "Jbml-icon-clear-delete" this))) ;; Class constructor
	)
    (set (@ our-class prototype) (new (@ Jbml-icon-base constructor))) ;; Set parent class

    (let* ((our-instance (new our-class)) ;; Class singleton instance
	   (class-name (+ (.get-icon-class-name our-instance) " jbml-clear-delete"))
	   )
      (defmethod (our-class get-icon-src) () "whiteXhandlebox.gif")
      (defmethod (our-class get-icon-class-name) () class-name)
      (set (@ our-class prototype single-click-handler) 
        (.make-single-click-handler our-instance package-box-clear-delete-mouse-click))

      ;; Using prototype inheritance with static methods, returns a singleton
      (set (@ our-instance constructor) our-class)
      our-instance
    ))
)


(defvar Jbml-tracked
	;; Private static variables
  (let ((our-class (lambda () (and "Jbml-tracked" this))) ;; Class constructor
	(our-dummy (new Object))
	)
    (set (@ our-class prototype) (new (@ Jbml constructor))) ;; Set parent class

    (let ((our-instance (new our-class)) ;; Class singleton instance
	  )
      ;; Overrideable methods
      (defmethod (our-class get-object-type) () (alert "Override Jbml-stateful.get-state-type"))
      (defmethod (our-class extract-value) (object) (alert "Override Jbml-stateful.extract-value"))
      (defmethod (our-class package-value) (message value) (alert "Override Jbml-stateful.package-value"))

      ;; .init and .set-parent-id must have been called for object
      (defmethod (our-class track-object) (object value)
        (.set-box-value this object "tracked" (or value "")))

      ;; .init must have been called for object
      ;; This breaks the defmethod macro, because it doesn't properly return values
      (set (@ our-class prototype package-object)
        (lambda (message object)
	  (let ((jbml (.get-jbml this object)))
	    (let ((box-id (.get-box-id this object))
		  (type (.get-object-type jbml))
		  (packaged-value (.package-value jbml message (.extract-value jbml object)))
		  )
	      (xml-element (message) (:list (:number box-id) (:string type) packaged-value))))))

      ;; This breaks the defmethod macro, because it doesn't properly return values
      (set (@ our-class prototype package-objects)
        (lambda (message list)
	  (let ((retval (xml-element (message) (:list))))
	    (dotimes (i (@ list length))
	      (.append-child retval (.package-object this message (ref list i)))
	    )
	    retval
	  )))

      ;; This breaks the defmethod macro, because it doesn't properly return values
      (set (@ our-class prototype get-tracked-objects)
	(lambda (node retval excluded-box-id)
	  (let ((retval (or retval (new Array))))
	    (when node
	      (.visit-child-boxes this node "tracked"
		(lambda (box-id value)
		  (let ((box (.get-element-by-id document box-id)))
		    (when (and box
			       ;; If excluding a box, check the box-id
			       (or (not excluded-box-id)
				   (not (eq (.get-box-id our-instance box) excluded-box-id))
				   )
			       )
		      (let ((extracted-value (.extract-value (.get-jbml our-instance box) box)))
			(when (not (eq value extracted-value))
			  (.set-box-value our-instance box "tracked" extracted-value)
			  (.push retval box)
			)
		      )
		    )
		  )
		)))
	    retval
	  )))
      
      (defmethod (our-class get-parent-tracked-objects) (child retval excluded-box-id)
        (.get-tracked-objects this (and child (.get-parent this child)) retval excluded-box-id))

      ;; Using prototype inheritance with static methods, returns a singleton
      (set (@ our-instance constructor) our-class)
      our-instance
    ))
)


(defvar Jbml-hole-base
	;; Private static variables
  (let ((our-class (lambda () (and "Jbml-hole-base" this))) ;; Class constructor
	(our-input-index 0)
	(our-dummy (new Object))
	)
    (set (@ our-class prototype) (new (@ Jbml-tracked constructor))) ;; Set parent class
    (set (@ our-dummy value) "")

    (let ((our-instance (new our-class)) ;; Class singleton instance
	  (remove-input-and-visit-boxtext
	    (lambda (parent input-type visit)
	      (let ((child (@ parent first-child)))
		(while child
		  (let ((next-child (@ child next-sibling))) ;; incase child gets deleted
		    (if (or (and (@ child node-name)
				 (eq (@ child node-name) input-type)
				 ) ;; Remove input boxes (either INPUT or TEXTAREA)
			    (.has-class Dom-library child "jbml-clear") ;; Remove jbml-clear icons
			    )
		      (.remove-child parent child)
		      ;else
		      (when (or (.has-class Dom-library child "boxtext-jbml")
				(and (@ child style) 
				     (eq (@ child style font-style) "italic")
				     ) ;; apparently not used anymore?
				(and (@ child node-name) 
				     (eq (@ child node-name) "I")
				     ) ;; OBSOLETE
				)
			(visit child)
		      ))
		    (set child next-child)
		  )
		)
	      )))
	  (set-input-id
	    (lambda (node input-id)
	      (set (@ node x-jbml-input-id) input-id)))
	  (get-input-id
	    (lambda (node)
	      (or (@ node x-jbml-input-id) false)))
	  )
      (defmethod (our-class extract-value) (object) 
        (@ (or (and (get-input-id object) 
		    (.get-element-by-id document (get-input-id object))
		    )
	       object
	       our-dummy) value))

      ;; Overrideable methods
      (defmethod (our-class get-input-type) () (alert "Override jbml-hole-base.get-input-type"))
      (defmethod (our-class create-open-hole-input) (box value) (alert "Override jbml-hole-base.create-open-hole-input"))

	    ;; Event handlers
      (let ((hole-mouseover-handler
              (lambda (e)
	        (let ((box-id (.get-event-box-id our-instance this e)))
		  (package-box-mouse-over box-id)
		  (.stop-propagation Event-library e)
		)
	      ))
	    (hole-mouseout-handler
	      (lambda (e)
	        (let ((box-id (.get-event-box-id our-instance this e)))
		  (package-box-mouse-out box-id)
		  (.stop-propagation Event-library e)
		)
	      ))
	    (double-click-handler 
	      (lambda (e)
		(let ((box-id (.get-event-box-id our-instance this e))
		      (button (.get-button Event-library e))
		      )
		  (clear-mouse-delay-timer this)
		  (package-box-mouse-double-click box-id button)
		  (.stop-propagation Event-library e)
		)))
	    (open-hole-click-handler
	      (.make-single-click-handler our-instance package-box-mouse-click))
	    (closed-hole-click-handler
	      (.make-single-click-handler our-instance package-closed-hole-mouse-click))
	    )
	;; This breaks the defmethod macro, because it doesn't properly return values
	(set (@ our-class prototype init-hole)
	  (lambda (box)
	    (.init this box (.get-box-id this box))
	    (.set-border-style this box "jbml-dotted")
	    (set (@ box style background-color) hole-color)
	    (set (@ box ondblclick)  double-click-handler)
	    (set (@ box onmouseover) hole-mouseover-handler)
	    (set (@ box onmouseout)  hole-mouseout-handler)
	    ;; NOTE: A hidden box is no longer used
	    ;(.append-child box (html ((:div :class "a-jbml boxtext-jbml" :style "font-style: italic; display: none;") "input")))
	    (.enable-drop this box)
	    (.disable-drag this box)
	  ))

	;; Turns a hole into an open hole
	;; This breaks the defmethod macro, because it doesn't properly return values
	(set (@ our-class prototype open)
	  (lambda (hole value)
	    (remove-input-and-visit-boxtext hole (.get-input-type this) (@ Dom-library hide))
	    (set (@ hole onclick) open-hole-click-handler)
	    (set (@ hole ondblclick) (@ Event-library stop-propagation))

	    (let ((input (.create-open-hole-input this hole value)))
	      (.init this input (.get-box-id this hole))
	      (unless (@ input id)
	        ;; Ensure the input has an ID.
		(++ our-input-index)
		(set (@ input id) (+ "jbml-input-" our-input-index))
	      )
	      (.set-parent-id this input (.get-box-id this hole))
	      (.track-object this input value)
	      (set-input-id hole (@ input id))
	      (.attach-icon Jbml-icon-hole-clear hole)
	      (.focus input)
	      ;; In some cases we might not already know "value"
	      input
	    )))

	;; Turns a hole into a closed hole
	;; This breaks the defmethod macro, because it can't handle multiple statements
	(set (@ our-class prototype close)
	  (lambda (hole)
	    (remove-input-and-visit-boxtext hole (.get-input-type this) (@ Dom-library show))
	    (set (@ hole onclick) closed-hole-click-handler)
	    (set (@ hole ondblclick) double-click-handler)
	  ))
      )

      ;; Using prototype inheritance with static methods, returns a singleton
      (set (@ our-instance constructor) our-class)
      our-instance
    ))
)


(defvar Jbml-hole
	;; Private static variables
  (let ((our-class (lambda () (and "Jbml-hole" this))) ;; Class constructor
	)
    (set (@ our-class prototype) (new (@ Jbml-hole-base constructor))) ;; Set parent class

    (let ((our-instance (new our-class))) ;; Class singleton instance
      (defmethod (our-class get-object-type) () "input")
      ;; This breaks the defmethod macro, because it doesn't properly return values
      (set (@ our-class prototype package-value)
	(lambda (message value)
	  (let ((value (escape-string (+ "" value))))
	    (xml-element (message) (:string value)))))

      (defmethod (our-class get-input-type) () "INPUT")

	    ;; Event handlers
      (let ((input-keypress-handler
              (lambda (e)
	        (let* ((input (.get-current-target Event-library this e))
		       (box-id (.get-box-id our-instance input))
		       )
		  ;; TODO: TAB, ENTER and ESC are supposedly not passed by IE, we might need a keyup/down handler instead...
		  (case (.get-keypress-char-code Event-library e)
		    (9 ;; TAB
		      (package-input-text-tab box-id (@ input value))
		    )
		    (13 ;; ENTER
		      (package-input-text box-id (@ input value))
		    )
		    (27 ;; ESC
		      (package-box-clear-hole-mouse-click box-id (@ Event-library left-mouse-button))
		      (.stop-propagation Event-library e)
		    )
		    (t true)
		  ))))
	    )
	;; This breaks the defmethod macro, because it doesn't properly return values
	(set (@ our-class prototype create-open-hole-input)
	  (lambda (box value)
	    (let ((input (html ((:input :type "text" :name "Text Input box" :size 5 :value value)))))
	      (.init this input (.get-box-id this box))
	      (set (@ input onkeypress) input-keypress-handler)
	      (.append-child box input)
	      input
	    )))
      )

      ;; Using prototype inheritance with static methods, returns a singleton
      (set (@ our-instance constructor) our-class)
      our-instance
    ))
)


(defvar Jbml-input-text
	;; Private static variables
  (let ((our-class (lambda () (and "Jbml-input-text" this))) ;; Class constructor
	)
    (set (@ our-class prototype) (new (@ Jbml-hole constructor))) ;; Set parent class

    (let ((our-instance (new our-class))) ;; Class singleton instance
      (set (@ our-class prototype init-hole)
	(lambda (box)
	  (.init this box (.get-box-id this box))
	  (set (@ box style border-style) "none")
	  (set (@ box style margin) "0px")
	))

      ;; Using prototype inheritance with static methods, returns a singleton
      (set (@ our-instance constructor) our-class)
      our-instance
    ))
)


(defvar Jbml-multiline-hole
	;; Private static variables
  (let ((our-class (lambda () (and "Jbml-multiline-hole" this))) ;; Class constructor
	(our-max-cols 80)
	)
    (set (@ our-class prototype) (new (@ Jbml-hole-base constructor))) ;; Set parent class
	   ;; Private static functions
    (let* ((our-instance (new our-class)) ;; Class singleton instance
	   (set-additional-rows
	     (lambda (textarea rows)
	       (set (@ textarea x-additional-rows) rows)))
	   (get-additional-rows
	     (lambda (textarea)
	       (or (@ textarea x-additional-rows) 0)))
	   (get-event-textarea
	     (lambda (event-this e)
	       (.get-element-by-id document (+ (.get-event-box-id our-instance event-this e) "-textarea"))))
	   (prepare-value
	     (lambda (entries-node)
	       (let ((entry-node (or (and entries-node (@ entries-node first-child)) null))
		     (content (or (and (is-string entries-node) entries-node) ""))
		     (join "") ;; Set after the first iteration of the loop to "\n"
		     )
		 (while entry-node
		   (+= content (+ join (.get-text-content Dom-library entry-node)))
		   (set join #\Newline)
		   (set entry-node (@ entry-node next-sibling))
	         )
	         content
	       )))
	   (split-rows
	     (lambda (textarea)
	       (unless (eq (@ textarea value) (@ textarea x-prev-value))
		 (set (@ textarea x-prev-value) (@ textarea value))
		 (let ((rows (.split (+ "" (@ textarea value)) #\Newline))
		       (i 0)
		       (max-cols (+ our-max-cols 2)) ;; The space for the vertical scroll bar adds 2 columns
		       )
		   (while (< i (@ rows length))
		     (when (< our-max-cols (@ (ref rows i) length))
		       (let* ((row (ref rows i))			       
			      ;; Find a place to break the line moving back from
			      ;; max-cols, or after max-cols (scrolled), or we cannot
			      ;; break the line
			      (split (or (+ (.last-index-of row " " max-cols) 1)
					 (+ (.index-of row " " max-cols) 1)
					 (@ row length)
					 ))
			      )
			 ;; If the line was broken
			 (when (< split (@ row length))
			   (set (ref rows i) (.substr row 0 split))
			   (if (< (+ i 1) (@ rows length))
			     (set rows (.concat (.slice rows 0 (+ i 1)) (.substr row split) (.slice rows (+ i 1))))
			     ;else
			     ;; We were at the end of the text, so just push it
			     (.push rows (.substr row split)))
			 )
		       )
		     )
		     (++ i)
		   )
		   (set (@ textarea x-split-rows) rows)
		 ))
               (@ textarea x-split-rows)
             ))
	   (update-rows-and-cols
	     (lambda (textarea)
	       (let ((rows (split-rows textarea))
		     (cols 10)
		     )
		 (dotimes (i (@ rows length))
		   (when (< cols (@ (ref rows i) length))
		     (set cols (@ (ref rows i) length))
		   )
		 )
		 (when (< our-max-cols cols)
		   (set cols our-max-cols)
		 )
		 (set (@ textarea cols) cols)
		 (set (@ textarea rows) (+ (@ rows length) (get-additional-rows textarea)))
	       )))
	   )
      (defmethod (our-class get-object-type) () "multiline-input")
      (defmethod (our-class get-input-type) () "TEXTAREA")

	    ;; Event handlers
      (let ((textarea-keypress-handler
	      (lambda (e)
		(when (@ (.get-event Event-library e) ctrl-key)
		  (.stop-propagation Event-library e)
		  true
	        )
	      ))
	    (textarea-keydown-handler
	      (lambda (e)
		(let* ((textarea (.get-current-target Event-library this e))
		       (box-id (.get-box-id our-instance textarea))
		       )
		  (case (@ (.get-event Event-library e) key-code)
		    (9 ;; TAB
		      (package-input-multiline-text-tab box-id (@ textarea value))
		    )
		    (27 ;; ESC
		      (package-box-clear-hole-mouse-click box-id (@ Event-library left-mouse-button))
		      (.stop-propagation Event-library e)
		    )
		    (t true)
		  )
		)))
	    (textarea-value-modified-handler
	      (lambda (e)
	        (update-rows-and-cols (.get-current-target Event-library this e))
	      ))
	    (button-enter-handler
	      (lambda (e)
		(let ((textarea (get-event-textarea this e)))
		  (when textarea
		    (package-input-multiline-text (.get-box-id our-instance textarea) (@ textarea value))
		  )
		  (.stop-propagation Event-library e)
		)))
	    (button-expand-handler
	      (lambda (e)
		(let ((textarea (get-event-textarea this e)))
		  (when textarea
		    (set-additional-rows textarea (+ (get-additional-rows textarea) 1))
		    (update-rows-and-cols textarea)
		    (.focus textarea)
		  )
		  (.stop-propagation Event-library e)
		)))
	    (button-resize-handler
	      (lambda (e)
		(let ((textarea (get-event-textarea this e)))
		  (when textarea
		    (set-additional-rows textarea 0)
		    (update-rows-and-cols textarea)
		    (.focus textarea)
		  )
		  (.stop-propagation Event-library e)
		)))
            )
	;; This breaks the defmethod macro, because it doesn't properly return values
	(set (@ our-class prototype create-open-hole-input)
	  (lambda (box value)
	    (let ((value (prepare-value value))
		  (button-enter (html ((:input :type "button" :value "Enter"))))
		  (button-expand (html ((:input :type "button" :value "More"))))
		  (button-resize (html ((:input :type "button" :value "Resize"))))
		  (textarea-id (+ (.get-box-id this box) "-textarea"))
		  (box-id (.get-box-id this box))
		  )
	      (.set-box-id this button-enter box-id)
	      (set (@ button-enter onclick)    button-enter-handler)
	      (set (@ button-enter ondblclick) (@ Event-library stop-propagation))

	      (.set-box-id this button-expand box-id)
	      (set (@ button-expand onclick)    button-expand-handler)
	      (set (@ button-expand ondblclick) (@ Event-library stop-propagation))

	      (.set-box-id this button-resize box-id)
	      (set (@ button-resize onclick)    button-resize-handler)
	      (set (@ button-resize ondblclick) (@ Event-library stop-propagation))

	      (let ((input (html ((:textarea :id textarea-id :style "overflow: visible; white-space: nowrap;") value))) ;; TODO: Move to CSS class
		    )
		(.init this input box-id)
		(set (@ input onkeyup)    textarea-value-modified-handler)
		(set (@ input onkeydown)  textarea-keydown-handler)
		(set (@ input onkeypress) textarea-keypress-handler)
		;; This is not supported on all browsers
		(.add-event-listener Event-library input "DOMAttrModified" textarea-value-modified-handler)

		(.append-child box (html ((:div :style "clear: none; float: left; text-align: right;")
					   button-enter
					   (:br :style "clear: none;")
					   button-expand
					   (:br :style "clear: none;")
					   button-resize
					   )))
		(.append-child box (html ((:span :style "white-space: pre;") " ")))
		(.append-child box input)
		(update-rows-and-cols input)
		input
              ))))
      )

      ;; This breaks the defmethod macro, because it doesn't properly return values
      (set (@ our-class prototype package-value) 
        (lambda (message value)
	  (let ((lines (.split (+ "" value) #\Newline))
		(retval (xml-element (message) (:list)))
		)
	    (dotimes (i (@ lines length))
	      (let ((line (escape-string (ref lines i))))
		(.append-child retval (xml-element (message) (:string line)))
	      )
	    )
	    retval
	  )))

      ;; Using prototype inheritance with static methods, returns a singleton
      (set (@ our-instance constructor) our-class)
      our-instance
    ))
)
