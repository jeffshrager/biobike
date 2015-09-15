;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb) 

;;; +=========================================================================+
;;; | Copyright (c) 2005 JP Massar, Jeff Shrager
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

;;; Authors:  JP Massar, Jeff Shrager.

(publish
 :path *prefs-gui-url*
 :content-type cl-user::*html-publish-content-type*
 :function 
 (lambda (req ent)
   (let* ((input (request-query req))
          (pkgname (url-parameter-value :pkg input))
          (package-symbol (keywordize pkgname))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (prefs-gui-function package-symbol input)))
     )))

(defun prefs-gui-function (pkg input)
  (let* ((title (formatn "Your Preferences"))
         (session-default-prefs (get pkg :default-prefs))
         (immediately-previous-prefs (get pkg :previous-prefs))
         (system-set? (url-parameter-value :system-set input))
         (user-set? (url-parameter-value :user-set input))
         (previous-set? (url-parameter-value :previous-set input))
         (apply? (url-parameter-value :apply input))
         (save? (url-parameter-value :save input))
         (user-action? (or system-set? user-set? apply? previous-set? save?))
         (bad-values-list nil)
         (no-previous? nil))
    (when user-action? 
      (unless (exactly-one-of? system-set? user-set? apply? previous-set? save?)
        (error "Internal error."))
      (multiple-value-setq 
          (bad-values-list no-previous?) 
          (change-prefs
           input session-default-prefs immediately-previous-prefs
           user-action? system-set? user-set? previous-set? apply? save?
           ))
      (setq session-default-prefs (get pkg :default-prefs))
      (setq immediately-previous-prefs (get pkg :previous-prefs)))
    (javascript-for-prefs-page-popups)
    (html
     (:style "BODY {font-family: Verdana, Geneva, Arial; font-size: 10pt;}")
     (:body 
      :br
      (:h2 (:center (:princ-safe title)))
      :br 
      (html-for-prefs-notifications bad-values-list no-previous? save?)
      ((:form :method "post" :action (subseq *prefs-gui-url* 1))
       :br
       (html-for-prefs-table session-default-prefs immediately-previous-prefs)
       ((:input :type "HIDDEN" :name "pkg" :value pkg))
       :br 
       (html-for-prefs-action-buttons)
       )
      :br
      ))))

(defparameter *prefs-page-popups*
  '((:*EXECUTION-TIMELIMIT* "After *EXECUTION-TIMELIMIT* (seconds) are reached for a given expression, a break occurs. (valid range???)")
    (:*OUTPUT-LINES-LIMIT* "After *OUTPUT-LINES-LIMIT* lines are displayed from a given expression, output is truncated. (valid range???)")
    (:*PRINT-LEVEL* "When a nested (recursive) data structure is being displayed, structures deeper than the *PRINT-LEVEL* depth are abbreviated by replacing them with a hash (#). Usually *PRINT-LEVEL* is an integer, but a value of NIL means: Never abbreviate at any depth.")
    (:*PRINT-LENGTH* "When printing a long list, the printer will add ... after *PRINT-LENGTH* elements have been printed. (valid range???)")
    (:*PRINT-PRETTY* "Turns pretty printing (indentation and other pretty stuff) off (NIL) and on (T).")
    (:*SAFETY* "The weblistener automatically compiles most expressions. *SAFETY* tells the automatic expression compiler (as well as the file compiler) whether to tend towards slower but safer (i.e., with better error checking), or faster but less safe code. (T = slow/safe, NIL = fast/less safe)")
    ))

(defun change-prefs 
       (input session-default-prefs immediately-previous-prefs
              user-action? system-set? user-set? previous-set? apply? save?)

  (when (null session-default-prefs)
    (error "Internal error.  No stored session defaults."))

  (when user-action? 

    (cond

     ;; Set the user's prefs variables to the system defaults
     (system-set? 
      (create-previous-prefs)
      (loop for (var nil nil system-default) in *weblistener-prefs-variables*
            do (set var system-default))
      nil)

     ;; Set the user's prefs variables to the values stored in the
     ;; user's prefs.lisp file and stored internally on the user's
     ;; session's plist.
     (user-set? 
      (create-previous-prefs)
      (loop for (var) in *weblistener-prefs-variables*
            do 
            (set var (cadr (assoc var session-default-prefs))))
      nil)

     ;; Set the user's prefs variables to the values stored
     ;; as the previous values stored on the user's session's plist.
     (previous-set? 
      (if immediately-previous-prefs
          (let ((current-bindings (create-current-prefs-bindings)))
            (loop for (var) in *weblistener-prefs-variables*
                  do
                  (set var (cadr (assoc var immediately-previous-prefs))))
            (create-previous-prefs current-bindings)
            (values nil nil))
        (values nil t)
        ))
     
     ;; set the user's prefs variables to the new values the user provided
     ;; in the input form.  If APPLY?, then do not save them in the prefs.lisp
     ;; file or make them the defaults on the property list.  If SAVE?,
     ;; do save them as well as setting the variables.
     ((or apply? save?) 
      (multiple-value-prog1 
          (let ((invalid-bindings nil)
                (changed-settings-list nil)
                (invalid-value (cons nil nil)))
            (loop 
             for (var nil test nil) in *weblistener-prefs-variables*
             as paramstring = (url-parameter-value (keywordize var) input nil)
             as new-value = 
             (handler-case
                 (read-from-string paramstring)
               (error () invalid-value))
             do 
             (cond 
              ((eq new-value invalid-value) 
               (push (list var paramstring) invalid-bindings))
              ((and test (not (funcall test new-value))) 
               (push (list var new-value) invalid-bindings))
              ((not (equalp new-value (symbol-value var)))
               (push (list var new-value) changed-settings-list))
              (t nil)
              ))
            (when (and changed-settings-list (null invalid-bindings))
              (create-previous-prefs))
            (unless invalid-bindings (create-previous-prefs))
            (or invalid-bindings 
                (loop for (var value) in changed-settings-list
                      do (set var value))))
        (when save? (save-prefs :create-saved-bindings t))
        ))
   
     (t (error "Internal error.  Should never get here."))
     
     )))

(defun default-pref-value (var session-default-prefs)
  (cadr (assoc var session-default-prefs)))

(defun previous-pref-value (var immediately-previous-prefs)
  (if immediately-previous-prefs 
      (cadr (assoc var immediately-previous-prefs))
    '-))
  
(defun create-previous-prefs (&optional bindings)
  (setf (get *sessionid* :previous-prefs) 
        (or bindings (create-current-prefs-bindings))))

            
(defun html-for-prefs-table (session-default-prefs immediately-previous-prefs)
  (macrolet ((td-center (&body body) `((:td :align "center") ,@body))
             (th-center (&body body) `((:th :align "center") ,@body)))
    (html
     (:table
      (:tr
       (th-center (:b (:princ-safe "Setting") "&nbsp;&nbsp;"))
       (th-center (:b (:princ-safe "System Defaults") "&nbsp;&nbsp;"))
       (th-center (:b (:princ-safe "Your Defaults") "&nbsp;&nbsp;"))
       (th-center (:b (:princ-safe "Previous Values") "&nbsp;&nbsp;"))
       (th-center (:b (:princ-safe "Current Values"))))
      (loop 
       for (var nil nil system-default) in *weblistener-prefs-variables* do
       (html 
	(:tr 
	 (:td (pref-popup var))
	 (td-center (:princ-safe (formatn "~s" system-default)))
	 (td-center 
	  (:princ-safe 
	   (formatn "~S" (default-pref-value var session-default-prefs))))
	 (td-center  
	  (:princ-safe 
	   (formatn 
	    "~S" (previous-pref-value var immediately-previous-prefs))))
	 (td-center  
	  ((:input :type "text" :name var  
		   :value (formatn "~S" (symbol-value var)) 
		   :size 10
		   )))
	 )))
      ;; Buttons at the base of each column
      (:tr   
       (:td ((:font :color "brown") "[mouse above for help]"))
       (:td (:center ((:input :name :system-set :type "submit" 
			      :value "Use These"))))
       (:td (:center ((:input :name :user-set :type "submit" 
			      :value "Use These"))))
       (:td
	(if immediately-previous-prefs
	    (html (:center ((:input :name :previous-set :type "submit" 
				    :value "Use These"))))
	  (html (:center (:princ-safe "(No Values)")))))
       (:td (:center ((:input :name :apply :type "submit" :value "Set These"))))
       )))))

(defun pref-popup (var)
  (let* ((string-name (format nil "~a" var))
	 (popup-arg (second (find (keywordize string-name) *prefs-page-popups* :key #'car))))
    (if popup-arg
	(html (:princ (format nil "<a ONMOUSEOVER=\"popup('~a','lightgreen')\"; ONMOUSEOUT=\"kill()\">~a</a>" popup-arg var)))
      (html (:princ-safe (formatn "~a" var))))))

(defun javascript-for-prefs-page-popups ()
  (html
   (:princ
    "<head>
<STYLE TYPE=\"text/css\">
<!--
#dek {POSITION:absolute;VISIBILITY:hidden;Z-INDEX:200;}
//-->
</STYLE>
</head>
<body>
<DIV ID=\"dek\"></DIV>

<SCRIPT TYPE=\"text/javascript\">
<!--

Xoffset=-60;    // modify these values to ...
Yoffset= 20;    // change the popup position.

var old,skn,iex=(document.all),yyy=-1000;
var ns4=document.layers
var ns6=document.getElementById&&!document.all
var ie4=document.all

if (ns4)
skn=document.dek
else if (ns6)
skn=document.getElementById(\"dek\").style
else if (ie4)
skn=document.all.dek.style
if(ns4)document.captureEvents(Event.MOUSEMOVE);
else{
skn.visibility=\"visible\"
skn.display=\"none\"
}
document.onmousemove=get_mouse;

function popup(msg,bak){
var content=\"<TABLE  WIDTH=350 BORDER=1 BORDERCOLOR=black CELLPADDING=2 CELLSPACING=0 \"+
\"BGCOLOR=\"+bak+\"><TD ALIGN=center><FONT COLOR=black SIZE=2>\"+msg+\"</FONT></TD></TABLE>\";
yyy=Yoffset;
 if(ns4){skn.document.write(content);skn.document.close();skn.visibility=\"visible\"}
 if(ns6){document.getElementById(\"dek\").innerHTML=content;skn.display=''}
 if(ie4){document.all(\"dek\").innerHTML=content;skn.display=''}
}

function get_mouse(e){
var x=(ns4||ns6)?e.pageX:event.x+document.body.scrollLeft;
skn.left=x+Xoffset;
var y=(ns4||ns6)?e.pageY:event.y+document.body.scrollTop;
skn.top=y+yyy;
}

function kill(){
yyy=-1000;
if(ns4){skn.visibility=\"hidden\";}
else if (ns6||ie4)
skn.display=\"none\"
}

//-->
</SCRIPT>")))

(defun html-for-prefs-action-buttons ()
  (macrolet ((td-brown (&body body) `(:td ((:font :color "brown") ,@body))))
    (html
     (:table
      (:tr
       (:td ((:input :name :save :type "submit" :value "Make Changes Permanent")))
       (td-brown " ;; Apply your changes and save them for future sessions."))
      )
     :br
     ((:font :color "brown") 
"

Edit the settings as desired, and then press [Set These] to make the
changes in the current listener session.  When you do this, your
previous settings will be entered into the \"Previous\" column so you
can retreive them if you don't like the new settings.  You can
retreive the settings from any column using the [Use These] button
below that column.

<p>

This window will remain open until you manually close it (using the
Red \"X\"). This lets you to try out various settings in the listener
without having to open and close this window all the time.

<p>

If you want the settings to be permanent, press [Make Changes
Permanent]. If you don't press [Make Changes Permanent] then your
latest settings will only remain for the current listener session.

<p>

Permanent changes only affect the current session and future
sessions. To change settings in another running listener session you
have to open this window from the other session, although once you
have done that, the \"My defaults\" column will contain your permanent
settings, so that you can use the [Use these] button to copy them to
the new session.

<p>

Here are examples of what the various printing settings do:

<pre>
>> '(this is (a (very (very deeply) (nested (long (complex (and (very very very) odd) list))))))

With *PRINT-PRETTY* = T, *PRINT-LEVEL* = NIL, and *PRINT-LENGTH* = 100:

(THIS IS
 (A
  (VERY (VERY DEEPLY)
   (NESTED (LONG (COMPLEX (AND (VERY VERY VERY) ODD) LIST))))))

With *PRINT-PRETTY* = NIL:

  (THIS IS (A (VERY (VERY DEEPLY) (NESTED (LONG (COMPLEX (AND (VERY VERY VERY) ODD) LIST))))))

With *PRINT-LEVEL* = 3:

  (THIS IS (A (VERY # #)))

With *PRINT-LENGTH* = 2:

  (THIS IS ...)
</pre>

")
     )))

(defun html-for-prefs-notifications (bad-values-list no-previous? save?)
  (when (and save? (null no-previous?) (null bad-values-list))
    (html
     (:big 
      (:b ((:font :color "green") (:princ-safe "Preferences saved."))))))
  (when bad-values-list 
    (html
     (:big 
      (:b ((:font :color "red") (:princ-safe "Preferences change failed!"))))
     :br :br
     (:ul 
      (loop for (var bad-value) in bad-values-list do
            (html 
             (:li 
              (:princ-safe 
               (formatn "  '~S' is not a legal value for ~A" bad-value var)
               )))))
     (when save? 
       (html 
        :br 
        ((:font :color "red") (:princ-safe "Preferences NOT saved!"))))))
  (when no-previous? 
    (html
     (:big 
      (:b ((:font :color "red") 
           (:princ-safe "No previous preference values exist!")))))))
