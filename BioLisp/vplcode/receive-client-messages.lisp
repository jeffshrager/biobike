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

;; INCOMING MESSAGE DISPATCHER

(defparameter *use-separate-execution-thread?* t)
(defparameter *in-execution-process?* nil)

(defparameter *session-state-id* nil)

(defparameter *break-on-toplevel-vpl-error* nil)

#+allegro
(defvar *execution-process-list-lock* 
  (mp:make-process-lock :name "Execution process list"))


(defun vpl-executing? () wb::*vpl-executing?*)
  

#-:lispworks
(progn
  (register-message-handler :vpl 'vpl)
  (register-message-handler :vpl-json 'vpl-json))

(defvar *channel* nil 
  #.(utils::one-string-nl
     "Bound to the channel argument of the VPL dispatch function below,"
     "so that we don't have to pass around the channel argument everywhere."
     ))

(defun vpl-toplevel-internal-error (c)
  (let ((msg (formatn 
              (one-string-nl
               "Unknown VPL system problem!  Untrapped"
               "random error caught by toplevel VPL function."
               "Actual error: ~A")
              c)))
    (user-error-message  
     (s+ msg "Please report this to the sys admins!"))
    (when *break-on-toplevel-vpl-error* (break "Toplever VPS error. ~s" c))
    (usyslog msg)
    (vdbg! msg)
    ))

(defun dispatch-message (channel message)
  (ecase (car message)
    (:vpl (vpl channel message))
    (:vpl-json (vpl-json channel message))))

(defun vpl (channel message)
  (vpl-handler channel message #'vpl-decode-message-and-execute-it))

(defun vpl-json (channel message)
  (vpl-handler channel message #'vpl-json-decode-message-and-execute-it))

(defun vpl-handler (channel message message-decoder)
  
  (let ((*channel* channel) 
        (wb::*vpl-executing?* t)
        (*in-execution-process?* *in-execution-process?*)
        (return-code t)
        )
    
    #+debug
    (sleep 2)

    ;; Catch all possible errors.  Errors specific to the VPL should be 
    ;; caught at a lower level.  Therefore only 2 kinds of errors can get
    ;; through: internal VPL errors, and random code errors.
    ;; Report each of these to the user's VPL window, to the system log, 
    ;; and to the console log.

    ;; handler-bind is necessary so that USER-ERROR-MESSAGE can have
    ;; access to the proper wb::*sessionid*

    (block exit

      (handler-case 

          (handler-bind
              ((vpl-internal-error 
                (lambda (c) 
                  (flet ((real-internal-error () 
                           (when wb::*sessionid* 
                             (user-error-message (formatn "~A" c)))
                           (let ((msg (formatn "VPL system problem: ~A, ~A"
                                               (or wb::*sessionid* "") c)))
                             (usyslog msg)
                             (return-from exit (setq return-code nil))
                             )))
                    (typecase c 
                      (vpl-null-parent-error 
                       (let ((client-sent-state-id *session-state-id*) 
                             (last-server-sent-state-id 
                              (get wb::*sessionid* :session-state-id)))
                         (if (eql client-sent-state-id
                                  last-server-sent-state-id)
                             (real-internal-error)
                           (progn
                             #+debug
                             (print "foo")
                             (usyslog "Ignoring state inconsistency")
                             (return-from exit (setq return-code nil))
                             ))))
                      (otherwise (real-internal-error))
                      ))))
               (vpl-user-error
                (lambda (c)
                  (user-error-message (formatn "~A" c))
                  (ulogdbg (formatn "Vpl interface error: ~%~A~%" c))
                  (return-from exit (setq return-code nil))
                  ))
               (error 
                (lambda (c)
                  (vpl-toplevel-internal-error c)
                  (return-from exit (setq return-code nil))
                  ))
               (storage-condition 
                (lambda (c) 
                  (create-and-use-unique-file 
                   (user-temp-vpl-dir)
                   (lambda (file p)
                     (declare (ignore file))
                     (with-html-to-stream p 
                       (wb::html-for-storage-condition c)
                       ))
                   (lambda (file) 
                     (show-vpl-popup-URL-window
                      (wb::publish-path-for-file-in-viewable-subdir file)
                      :relative-p 0
                      :width "800px" :height "800px" :menubar "yes"
                      ))
                   :name (s+ "storage-condition-" (string wb::*sessionid*))
                   :type "html"
                   )
                  (return-from exit (setq return-code nil))
                  )))
            (setq return-code (funcall message-decoder message))
            (vdbg "Returning from toplevel VPL function...~%")
            )
        
        (vpl-execution-condition 
         (c)
         (start-execution-process c channel message)
         )
        
        (error 
         (c)
         (vpl-toplevel-internal-error c)
         (return-from exit (setq return-code nil))
         )
         
        ))

    return-code
    
    ))

(defun start-execution-process (c channel message)
    (let ((session-id (vpl-execution-condition-data c)))
      (run-execution-process session-id channel message)
      ))

(defun run-execution-process (session-id channel message)
    (let ((ename nil) (execution-queue nil) (immediately? nil)
          (unique-id (gensym "EXECUTING-PROCESS-")))
      (with-lock (*execution-process-list-lock*)
        (setq execution-queue (get session-id :execution-process-list))
        (setq ename (string unique-id))
        ;; (formatn "xp-~A-~A" session-id (1+ (length execution-queue)))
        (cond
         ((null execution-queue)
          (setf (get session-id :execution-process-list) 
                (list unique-id))
          (setq immediately? t)
          )
         (t 
          (setf (get session-id :execution-process-list)
                (append (get session-id :execution-process-list)
                        (list unique-id)
                        ))
          (setq immediately? nil)
          )))
      (ulogdbg "process ~A to run: ~A~%" ename (if immediately? "now" "later"))
      ;; (formatt "process queue: ~S~%" execution-queue)
      (wb::run-function-as-process
       ename 
       (lambda (channel message pname unique-id immediately?)
         (setf (get unique-id :process) mp::*current-process*)
         (unless immediately? 
           (formatt "calling process-wait...~%")
           (mp::process-wait 
            "Waiting..." 
            (lambda () 
              (eq 
               unique-id
               (first (get session-id :execution-process-list))
               ))))
         ;; (formatt "Starting process ~A~%" pname)
         (ulogdbg ";; Starting execution process ~A...~%" pname)
         (unwind-protect 
             (let ((*in-execution-process?* t))
               (dispatch-message channel message)
               ;; (formatt "Process ~A finished~%" pname)
               (ulogdbg ";; Execution process ~A finished~%" pname))
           (progn
             #+debug
             (formatt
              "Removing process ~A from execution queue of length ~D~%" 
              unique-id (length (get session-id :execution-process-list)))
             (with-lock (*execution-process-list-lock*)
               (pop (get session-id :execution-process-list))
               ))))
       channel message ename unique-id immediately?
       )))

;; returns the process that was killed or nil if no process was killed
(defun kill-execution-process (&key (show-status? t))
    (let ((elist (get wb::*sessionid* :execution-process-list)))
      (if elist 
          (let* ((process-id-to-kill (first elist))
                 (process-to-kill (get process-id-to-kill :process))
                 (process-name (mp::process-name process-to-kill)))
            (ulog ";; Killing process ~A" process-name)
            (mp::process-kill process-to-kill :wait t)
            (when show-status?
              (show-status
               (formatn "Execution process ~A terminated." process-name)
               ))
            process-to-kill
            )
        (progn
          (when show-status? (show-status "No process is executing..."))
          nil
          ))))

;;; defines using a data table all the commands from the client, 
;;; how to extract their arguments and what function to call to 
;;; implement the command.  

;;; The VPL-NEW-MESSAGE-DISPATCH function implements the actual
;;; dispatch to the handlers. 

;;; Last argument is 'workspace potentially modified?'

(def-hash-table 
    vpl-json-dispatch 
    '(
      ;; Alphabetical order please...

      ;; "message type" (function-name (&rest args) modifies-workspace-p)
      ("box-clear-click" (handle-box-clear-icon (boxid mousecode) t))
      ("box-clear-delete-click" 
       (handle-box-clear-delete-icon (boxid mousecode) t))
      ("box-clear-hole-click" (handle-box-clear-hole-icon (boxid mousecode) t))
      ("box-click" (handle-box-mouse-click (boxid mousecode) t))
      ("box-delete-click" (handle-box-delete-icon (boxid mousecode) t))
      ("box-double-click" (handle-box-mouse-doubleclick (boxid mousecode) nil))
      ("boxmenumultiselect" 
       (handle-boxmenu-multiselect (boxid opcode) t))
      ("box-out" (handle-box-mouse-out (boxid) nil))
      ("boxmenumouseclick" (handle-boxmenu-mouse-click (opcode boxid) t))
      ("browser-page-closed" (handle-browser-page-closed () nil))
      ("clear-results" (handle-clear-execution-history () nil))
      ("clear-workspace" (handle-clear-workspace () t))
      ("click-results" (handle-click-results () nil))
      ("click-workspace" (handle-click-workspace () nil))
      ("dnd-box" (handle-dnd-box (sourceid boxid) t))
      ("dnd-box-after" (handle-dnd-box-after (sourceid boxid) t))
      ("dnd-box-before" (handle-dnd-box-before (sourceid boxid) t))
      ("global-keypress" (handle-global-keypress (key ctrl) t))
      ;; ("icon-click" (handle-box-icon-click (boxid iconcode) nil))
      ("input-multiline-text" (handle-multiline-input-text (boxid value) t))
      ("input-multiline-text-tab" 
       (handle-multiline-input-text-tab (boxid value) t))
      ("input-text" (handle-input-text (boxid value) t))
      ("input-text-tab" (handle-input-text-tab (boxid value) t))
      ("left-undo" (handle-workspace-back () nil))
      ("palettemenuitemhelp" (handle-palette-menu-item-help (id) nil))
      ("palettemenumouseclick" (handle-palettemenu-mouse-click (id) t))
      ("palettemenutooltips" (handle-palettemenu-tooltips () nil))
      ("right-redo" (handle-workspace-forward () nil))
      ("search-box-request" (handle-search-box-request (text) nil))
      ("sharerequest" (handle-share-request () nil))
      ("update-open-box-contents" (handle-update-open-box-contents (boxes) nil))
      ("user-popup-input"  (handle-user-popup-input (id userinput) t))
      )
  :test 'equal 
  :mode :list
  )

(def-hash-table 
 vpl-dispatch 
 '(

   ;; Sharing a session
   ("sharerequest" (handle-share-request () nil))

   ;; ("clear-results" (handle-clear-execution-history () nil))
   ;; ("clear-workspace" (handle-clear-workspace () t))


   ;; Alphabetical order please...

   ;; ("box-clear-click" (handle-box-clear-icon (vpl-first-arg vpl-second-arg) t))
   ;; ("box-clear-delete-click" (handle-box-clear-delete-icon (vpl-first-arg vpl-second-arg) t))
   ;; ("box-clear-hole-click" (handle-box-clear-hole-icon (vpl-first-arg vpl-second-arg) t))
   ;; ("box-click" (handle-box-mouse-click (vpl-first-arg vpl-second-arg) t))
   ;; ("box-delete-click" (handle-box-delete-icon (vpl-first-arg vpl-second-arg) t))
   ;; ("box-double-click" (handle-box-mouse-doubleclick (vpl-first-arg vpl-second-arg) nil))
   ;; ("box-out" (handle-box-mouse-out (vpl-first-arg) nil))
   ;; ("boxmenumouseclick" (handle-boxmenu-mouse-click (vpl-first-arg vpl-second-arg) t))
   ;; ("click-results" (handle-click-results () nil))
   ;; ("click-workspace" (handle-click-workspace () nil))
   ;; ("dnd-box" (handle-dnd-box (vpl-first-arg vpl-second-arg) t))
   ;; ("dnd-box-after" (handle-dnd-box-after (vpl-first-arg vpl-second-arg) t))
   ;; ("dnd-box-before" (handle-dnd-box-before (vpl-first-arg vpl-second-arg) t))
   ;; ("global-keypress" (handle-global-keypress (vpl-first-arg vpl-second-arg) t))
   ;; ("icon-click" (handle-box-icon-click (vpl-first-arg vpl-second-arg) nil))
   ;; ("input-multiline-text" (handle-multiline-input-text (vpl-first-arg vpl-second-multi-arg) t))
   ;; ("input-multiline-text-tab" (handle-multiline-input-text-tab (vpl-first-arg vpl-second-multi-arg) t))
   ;; ("input-text" (handle-input-text (vpl-first-arg vpl-second-arg) t))
   ;; ("input-text-tab" (handle-input-text-tab (vpl-first-arg vpl-second-arg) t))
   ;; ("left-undo" (handle-workspace-back () nil))
   ;; ("palettemenumouseclick" (handle-palettemenu-mouse-click (vpl-first-arg) t))
   ;; ("right-redo" (handle-workspace-forward () nil))
   ;; ("update-open-box-contents" (handle-update-open-box-contents (cdar) nil))
   ;; ("user-popup-input"  (handle-user-popup-input (vpl-first-arg vpl-second-arg) t))

   ;; --- Not found in vpl.js ---

   ;;("box-over" (handle-box-mouse-over (vpl-first-arg) nil))
   ;;("boxmenumultiselect" (handle-boxmenu-multiselect (vpl-first-arg vpl-second-arg) t))
   ;;("dump" (handle-dump (first) nil))
   ;;("eval" (handle-eval-request (vpl-second-arg) nil))
   ;;("help-url-request" (handle-help-url-request () nil))
   ;;("main-menu-select" (handle-main-menu-select (vpl-first-arg vpl-second-arg) t))
   ;;("options-menu-select" (handle-options-menu-select (vpl-first arg vpl-second-arg) t))
   ;;("palettemenutooltips" (handle-palettemenu-tooltips () nil))
   ;;("paste-box-into-hole" (handle-paste-box-into-hole (vpl-first-arg) t))
   ;;("testclick" (handle-test-mouse-click (vpl-first-arg) nil))
   ;;("vpl-version" (handle-vpl-version (vpl-first-arg) nil))
   )
 :test 'equal 
 :mode :list
 )

(defun vpl-first-arg (body) (cadar body))
(defun vpl-second-arg (body) (cadr (second body)))
(defun vpl-second-multi-arg (body) (second body))

(defun hack-xml-to-concatenate-string-parts (x)
  (cond
   ((atom x) x)
   ((not (eq (first x) :string))
    (mapcar 'hack-xml-to-concatenate-string-parts x))
   (t 
    (unless (every 'stringp (cdr x)) 
      (vpl-internal-error "Unexpected XML message format: ~S" x))
    `(:string ,(apply 's+ (cdr x)))
    )))


(defun parse-json-object (json-exp)
  (loop for (k v) on json-exp by #'cddr collect (keywordize k) collect v))

(defun vpl-json-decode-message-and-execute-it (message)
  (multiple-value-bind (type body session-id *session-state-id*) 
      (vpl-json-decode-message message)
    (setf (get session-id :last-activity-time) (get-universal-time))
    (setf (get session-id :autosave-status) :not-saved)
    (vpl-json-execute-message type body session-id)
    ))

(defun vpl-json-decode-message (message)
  "Return: verb body sessionid-symbol session-state-id"
  (vdbg "Got VPL-JSON message, Channel: ~A~%" *channel*)
  (destructuring-bind (vpl-tag json-text) message
    (unless (string-equal vpl-tag "vpl-json")
      (vpl-internal-error
       "Wrong outer tag. Got ~a, expecting VPL-JSON" vpl-tag))

    (let ((json (parse-json-object (parse-json json-text))))
      (destructuring-bind 
          (&key type sessionid sessioncounter &allow-other-keys)
          json
	(let ((session-id (and sessionid (keywordize sessionid)))
	      (session-state-id (or sessioncounter -1)))
	  (cond
	    ((string-equal type "Hello")
	     (vdbg "Got Hello...~%")
	     (vdbg "Session ID: ~S~%" session-id)
	     (values type nil session-id nil))

	    (t
	     (unless session-id
	       (syslogdbg "No session ID!~%")
	       (vpl-internal-error "No session ID!"))

	     (unless session-state-id
	       (syslogdbg "No state ID!~%")
	       (vpl-internal-error "No session state ID!"))

	     (vdbg "Session ID: ~S, session state ID: ~S~%"
                   session-id session-state-id)
	     
	     (values type json session-id session-state-id))))))))

(defun vpl-json-execute-message (verb body session-id)
  (block exit
    (let ((*more-client-commands* nil))

      (when (null (get session-id :username))
        ;; This should always be true for now.  Once you're logged out,
        ;; that's it.  You're not going to get any more messages about
        ;; being logged out.  This basically freezes your VPL window 
        ;; and any other VPL window connected to the same session.  
        (when (get session-id :logged-out)
          (return-from exit nil))
	(syslogdbg "Not logged in???  Cannot continue!~%")
	(vpl-not-logged-in-error session-id)
	(return-from exit nil))

      (wb::with-protected-globals-bound 
	  session-id 

        ;; *vpl-session-info* is a per-session variable.
        ;; Must initialize it BEFORE we ever use the 
        ;; WITH-USER-VPL-SESSION-INFO-BOUND macro because
        ;; that macro extracts data out of the structure 
        ;; it assumes it is the value of this variable.
        (when (null wb::*vpl-session-info*) 
          (create-vpl-session-info)
          (setf (get session-id :vpl-session?) t))

        (with-user-vpl-session-info-bound (wb::*vpl-session-info*)
          (let ((*vpl-workspace-modified?* t)
                (*modified-snippet* nil)
                (*focus-snippet* nil))
            ;; execute user's command, special case initialization message
            (cond
	      ((string-equal verb "hello")
	       (handle-hello))
              (t
	       ;; check for any system or user messages and show them 
	       (wb::output-announcements
		cl-user:*ai* :msg-handler 'vpl-external-message-handler)
	       (vpl-json-message-dispatch verb body)))

            (vdbg "*current-selected-boxid* : ~A~%" *current-selected-boxid*))

          (when *more-client-commands* 
            (loop for command in *more-client-commands* do (eval command))))

        ;; successful execution, return T
        t))))

(defun vpl-json-message-dispatch (verb body)
  (vdbg "In dispatch, body: ~S~%" body)
  (let ((data (gethash verb vpl-json-dispatch)))

    (unless data 
      (vpl-internal-error 
       (formatn "No JSON dispatch entry for client command ~S !" verb)))

    (destructuring-bind 
        (fn (&rest arg-names) workspace-potentially-modified?) 
        data

      ;; If the user has made his VPL session shared with another
      ;; session then don't allow him to do anything except a browser
      ;; refresh, which will automatically get him out of shared mode.
      (when (get wb::*sessionid* :slave-shared-mode)
        (return-from vpl-json-message-dispatch nil))

      (let ((args (mapcar (lambda (n) (getf body (keywordize n))) arg-names)))
	(vdbg "Received ~A command, calling ~A: ~{~S~^, ~}~%" verb fn args)

        (let ((*disable-selected-box-on-redraw?* t))

          (if workspace-potentially-modified? 
	      (with-workspace-modified-and-redrawn () (apply fn args))
	      (apply fn args))

          ;; Always clear the stored results (which only get
          ;; set on a browser refresh before the results get blown
          ;; away).
          (setf (get wb::*sessionid* :stored-results) nil)
          (when *snippet-gc-enabled* 
            (incf (get wb::*sessionid* :vpl-gc-count)))
          (when (>= (get wb::*sessionid* :vpl-gc-count)
                    *vpl-gc-count-threshold*)
            (syslog "Calling snippet GC ...~%")
            (gc-snippets)
            (syslog "Snippet GC finished...~%")
            (setf (get wb::*sessionid* :vpl-gc-count) 0)
            ))))))

(defun vpl-decode-message-and-execute-it (message)
  (vdbg "Got VPL message, Channel: ~A~%" *channel*)
  (multiple-value-bind (verb body sessionid-symbol session-state-id)
      (vpl-decode-message message)
    (let ((*session-state-id* session-state-id))
      (vpl-execute-message verb body sessionid-symbol)
      )))

(defun vpl-decode-message
       (message &aux sessionid-string sessionid-symbol session-state-id sids)
  (destructuring-bind (vpl-tag verb &body body) message
    (declare (ignore vpl-tag))
    ;; (:vpl "<command>" 
    ;;   (:list (:session-id "foo123") (:sessionp-counter "1")) ...)
    (vdbg "Message: ~S~%" message)
    (if-vpl-debug 
      (vdbg "~A VPL message: ~S, ~S~%" 
            (make-timestamp-string) 
            verb
            (let ((x (car body)))
              (if (and (listp x) (eq :session-id (first x)))
                  (second x) 
                x
                ))))
    (if (string-equal verb "Hello")
        (progn
          (vdbg "Got Hello...~%")
          (setq sessionid-string (second (first body)))
          (setq sessionid-symbol (keywordize sessionid-string))
          (vdbg "Session ID: ~S~%" sessionid-symbol)
          (values verb nil sessionid-symbol nil)
          )
      ;; body = ((:list (:session-id "foo123") (:session-counter "24")) ...
      (progn
        (setq sessionid-string (second (second (first body))))
        (when (null sessionid-string)
          (syslogdbg "No session ID!~%")
          (vpl-internal-error "No session ID!"))
        (setq sessionid-symbol (keywordize sessionid-string))
        (setq sids (second (third (first body))))
        (when (null sids)
          (syslogdbg "No state ID!~%")
          (vpl-internal-error "No session state ID!"))
        (let ((ssid (ignore-errors (parse-integer sids))))
          (setq session-state-id (or ssid -1)))
        (pop body)
        (vdbg "  Unhacked body: ~S~%" body)
        (setq body (hack-xml-to-concatenate-string-parts body))
        (vdbg "  BODY: ~S~%" body) 
        (when (null sessionid-string)
          (syslogdbg "No session ID!~%")
          (vpl-internal-error "No session ID!"))
        (vdbg "Session ID: ~S, session state ID: ~S~%" 
              sessionid-symbol session-state-id)
        (values verb body sessionid-symbol session-state-id)
        ))))

(defun vpl-execute-message (verb body sessionid-symbol)
  (block exit
    (let ((*more-client-commands* nil))
      (if (null (get sessionid-symbol :username))
          (progn
            (syslogdbg "Not logged in???  Cannot continue!~%")
            (vpl-not-logged-in-error sessionid-symbol)
            (return-from exit nil)
            ))
      (wb::with-protected-globals-bound
          sessionid-symbol 
        ;; *vpl-session-info* is a per-session variable.
        ;; Must initialize it BEFORE we ever use the 
        ;; WITH-USER-VPL-SESSION-INFO-BOUND macro because
        ;; that macro extracts data out of the structure 
        ;; it assumes it is the value of this variable.
        (when (null wb::*vpl-session-info*) 
          (create-vpl-session-info)
          (setf (get sessionid-symbol :vpl-session?) t))
        (with-user-vpl-session-info-bound (wb::*vpl-session-info*)
          (let ((*vpl-workspace-modified?* t)
                (*modified-snippet* nil)
                (*focus-snippet* nil))
            ;; execute user's command, special case initialization message
            (if (string-equal verb "hello")
                (handle-hello)
              (progn
                ;; check for any system or user messages and show them 
                (wb::output-announcements
                 cl-user:*ai* :msg-handler 'vpl-external-message-handler)
                (vpl-new-message-dispatch verb body)
                ))
            (vdbg "*current-selected-boxid* : ~A~%" *current-selected-boxid*))
          (when *more-client-commands* 
            (loop for command in *more-client-commands* do (eval command))
            ))
        ;; successful execution, return T
        t
        ))))

(defun vpl-new-message-dispatch (verb body)
  (vdbg "In dispatch, body: ~S~%" body)
  (let ((data (gethash verb vpl-dispatch)))
    (unless data 
      (vpl-internal-error 
       (formatn "No dispatch entry for client command ~S !" verb)))
    (destructuring-bind
        (function (&rest arg-extractors) workspace-potentially-modified?)
        data
      ;; If the user has made his VPL session shared with another session
      ;; then don't allow him to do anything except a browser refresh, 
      ;; which will automatically get him out of shared mode.  
      (when (get wb::*sessionid* :slave-shared-mode)
        (return-from vpl-new-message-dispatch nil)
        )
      (let ((args (mapcar (lambda (f) (funcall f body)) arg-extractors)))
        (if-vpl-debug
          (vdbg "Received ~A command, calling ~A:" verb function)
          (loop for remaining on args do 
                (vdbg
                 " ~S~A" (first remaining) (if (cdr remaining) "," "")))
          (vdbg "~%"))
        (let ((*disable-selected-box-on-redraw?* t))
          (if (null workspace-potentially-modified?) 
              (apply function args)
            (with-workspace-modified-and-redrawn () 
              (apply function args)
              ))
          ;; Always clear the stored results (which only get
          ;; set on a browser refresh before the results get blown
          ;; away).
          (setf (get wb::*sessionid* :stored-results) nil)
          (when *snippet-gc-enabled* 
            (incf (get wb::*sessionid* :vpl-gc-count)))
          (when (>= (get wb::*sessionid* :vpl-gc-count)
                    *vpl-gc-count-threshold*)
            (syslog "Calling snippet GC ...~%")
            (gc-snippets)
            (syslog "Snippet GC finished...~%")
            (setf (get wb::*sessionid* :vpl-gc-count) 0)
            ))))))



(defun vpl-external-message-handler (message)
  (vdbg "In vpl-external-message-handler~%")
  (vdbg "~A" message)
  (show-external-message message)
  )

(defun show-external-message (msg)
  (let* ((format (wb::sysmsg-format msg))
         (output-string 
          (if (eq :text (first format)) 
              (second format) 
            (apply 'format nil format)))
         (color (wb::sysmsg-color msg))
         (from (wb::sysmsg-from msg)))
    (create-and-use-unique-file 
     (user-temp-vpl-dir)
     (lambda (file p)
       (declare (ignore file))
       (with-html-to-stream p 
         (flet ((do-msg ()
                  (html 
                   (:princ "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
                   (:i
                    (:princ-safe (format nil ";; Message from ~A: " from)))
                   :br :br
                   (:princ-safe output-string)
                   :br
                   )))
           (if (null color)
               (do-msg)
             (html ((:font :color color) (do-msg)))
             ))))
     (lambda (file) 
       (show-vpl-popup-URL-window
        (wb::publish-path-for-file-in-viewable-subdir file)
        :relative-p 0
        :width "500px" :height "400px"
        ))
     :name (s+ "external-message-" (string wb::*sessionid*))
     :type "html"
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun construct-pseudo-message-from-client 
       (type id &key (sessionid wb::*sessionid*) (session-counter 1))
  `(vpl-json 
    ,(s+
      "{"
      "\"type\""
      ":"
      (s+ "\"" type "\"")
      ","
      "\"id\""
      ":"
      (formatn "~D" id)
      ","
      "\"sessionID\""
      ":"
      (s+ "\"" (string sessionid) "\"")
      ","
      "\"sessionCounter\""
      ":"
      (formatn "~D" session-counter)
      "}"
      )))
