;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
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

;;; Author:  JP Massar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generic functions and the default methods defined for the web listener.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Create a class whose name is the name of the application.
;;; And create a class for the name of the instance if it is different
;;; than the name of the application which is a subclass of the
;;; application class.  Finally, create an instance of the application
;;; class or instance class and store it in user:*ai*.

;;; We do this so that methods written for the application class (e.g.
;;; user:biolingua), will work for the instance class, so that a single
;;; object, the value of user:*ai*, will uniquely identify the 
;;; weblistener application/instance.  Using EQL methods would not allow
;;; us to do this.

;;; This function gets called by the toplevel load file for the application.
;;; E.g., blload.lisp or xshload.lisp

(defun make-application-instance-class-and-object ()
  (let ((user-app-symbol 
         (intern (symbol-name cl-user:*application*) :cl-user))
        (user-instance-symbol 
         (intern (symbol-name cl-user:*application-instance*) :cl-user)))
    (eval `(defclass ,user-app-symbol () ()))
    (export user-app-symbol :cl-user)
    (when (not (eq user-app-symbol user-instance-symbol))
      (eval `(defclass ,user-instance-symbol (,user-app-symbol) ()))
      (export user-instance-symbol :cl-user))
    (setq cl-user:*ai* (make-instance user-instance-symbol))
    ))


;;; See .../biodefs/webmethods.lisp for methods specific to BioLingua.

;;; Basic generic functions.

(defgeneric application-version (app &key mode)
  (:documentation
   #.(one-string-nl
      "Returns a version string."
      "If MODE is :long, should return something like 'Weblistener v5.1"
      "If MODE is :short, should return something like 'v5.1'"
      "If MODE is :number, should return something like '5.1'"
      )))

(defmethod application-version ((app t) &key (mode :long))
  (let ((current-version "6.0"))
    (case mode
      (:short (one-string "v" current-version))
      (:number current-version)
      (otherwise (one-string "Generic Weblistener v" current-version))
      )))


(defgeneric application-initializations (app)
  (:documentation 
   "Run at system startup to do any application-specific initializations."))

;; Must use :AFTER method instead of regular method to define initializations
;; for a specific application.  That way the standard weblistener
;; initializations (done in this method) get run, then the application's
;; initializations run.
(defmethod application-initializations ((app t))
  (cformatt "Running generic Weblistener initializations")
  (forward-funcall 'define-login-webpage-urls)
  )

(defgeneric application-instance-initializations (app)
  (:documentation
   #.(one-string-nl
      "Run at system startup time to do application-instance-specific"
      "initializations.  The BioLingua method loads a file from the startup"
      "account's home directory called <instance-name>-instance-init.lisp"
      "if it exists.")))

(defmethod application-instance-initializations ((app t))
  (load-instance-modules))

(defun load-instance-modules ()
  (loop for module in cl-user::*instance-module-list*
	do 
	(cformatt "Loading instance module ~a." module)
        (forward-funcall 'load-module module)
        ))


(defgeneric application-name (app)
  (:documentation "Returns string which is name of application"))

(defmethod application-name ((application t)) 
  (cond
   ((member application '(:generic t)) "The Generic Weblistener")
   (t (string-capitalize (string-downcase (string application))))
   ))


;;; Generic functions related to the login procedure.

(defgeneric login-webpage-url (app)
  (:documentation "The local name of the webpage to go to for user login"))

(defgeneric new-login-webpage-url (app)
  (:documentation "The local name of the webpage to go to for user login"))

(defgeneric guru-login-webpage-url (app)
  (:documentation "The local name of the webpage to go to for guru login")) 

(defmethod login-webpage-url ((app t)) "/weblogin")

(defmethod guru-login-webpage-url ((app t)) "/gweblogin")

(defgeneric application-packages-to-use (app)
  (:documentation "Packages to be used by the user's package"))

(defmethod application-packages-to-use ((app t)) 
  (list :wlisp :utils :frames :genericdb :webuser :pp))

(defgeneric application-symbols-to-shadowing-import (app)
  (:documentation 
   #.(one-string
      "Any symbols that need to be shadowing imported into the user's "
      "package.  Can be used, e,g, to 'redefine' Common Lisp functionality "
      "as is done with SORT for the BioLingua application."
      )))

(defmethod application-symbols-to-shadowing-import ((app t)) nil)


(defgeneric application-readtable (app)
  (:documentation "The readtable to be used by the Weblistener"))

;; To be able to use frames notation (#$ and #^) you need to
;; define this method to return (frames:frames-readtable)
(defmethod application-readtable ((app t)) *readtable*)


(defgeneric application-after-user-login-actions (app)
  (:documentation 
   #.(one-string
      "Anything application specific that needs to happen at the end "
      "of the login procedure for a user.")))

(defmethod application-after-user-login-actions ((app t))
  ;; Create the *MY-CURRENT-DB* symbol in the logged-in user's package
  ;; if it does not exist, and give it a value of NIL if it has no value.
  (eval 
   `(defvar 
        ,(symbol-of-package 
          :*my-current-db* (forward-funcall 'wb::user-session-package-name)
          :if-does-not-exist :intern)
      nil)))


(defgeneric login-header (app)
  (:documentation 
   "HTML to produce what appears at the very top of the login page"))

(defmethod login-header ((app t))
  (html 
   (:head 
    ((:font :size 6) 
     (:center 
      (:princ-safe 
       (s+ 
        (formatn
         "Welcome to ~A " 
         (or *application-instance-pretty-name* "the Generic Weblistener"))
        (application-version app :mode :short)
        ))))
    :br
    )))


(defgeneric login-addenda (app)
  (:documentation 
   "HTML to produce what appears immediately after the LOGIN/PASSWORD boxes"))

(defmethod login-addenda ((app t)) nil)


(defgeneric login-credits (app)
  (:documentation 
   "HTML to produce what appears below the ADDENDA, presumably credits."))

(defmethod login-credits ((app t))
  (html
   :hr
   :p
   ((:font :color "blue")
    "The original conception of the WebListener was done by Jeff Shrager "
    "and Dan Siroker in 2002.  JP Massar designed and implemented the current "
    "WebListener in 2003/2004 as part of the BioLingua project, with coding, "
    "assistance and comments from Jeff Shrager, Mike Travers and Jeff Elhai."
    )))


(defgeneric login-disclaimer (app)
  (:documentation 
   "HTML to produce what appears below the CREDITS, presumably a disclaimer."))

(defmethod login-disclaimer ((app t))
  (html
   :hr
   ((:font :color "red")
    "The Weblistener and its code are made available AS IS, with no warranty "
    "expressed or implied.  Use this tool at your own risk."
    :br
    "Your input and output may be logged and examined by whomever is "
    "running this Weblistener instance."
    )))


(defgeneric new-weblogin-function (app req ent)
  (:documentation
   "The function that creates the HTML that displays the login page"
   ))

;; Generic defmethod defined in login.lisp
(defgeneric weblogin-function (app req ent)
  (:documentation 
   "The function that creates the HTML that displays the login page"
   ))


;; Generic defmethod defined in login.lisp
(defgeneric a-login-form-response-function (app req ent)
  (:documentation
   "The function that handles what comes back from the form on the login page."
   ))



;;; Generic functions pertaining to 'Print' part of the Weblistener's 
;;; Read-Eval-Print loop.

;; Generic defmethod defined in redisplay-listener.lisp
(defgeneric weblistener-onload-action (app)
  (:documentation 
   "Returns string used as javascript command for action when page is loaded"))

;; Generic defmethod defined in redisplay-listener.lisp
(defgeneric weblistener-redisplay-page-title (app)
  (:documentation
   "Returns string used as <title> for weblistener page."))

;; Generic defmethod defined in redisplay-listener.lisp
(defgeneric weblistener-input-marker (app history-number)
  (:documentation 
   "Returns the prefix string for user input printout (e.g., \"<1>>\")"))


;; Generic defmethod defined in redisplay-listener.lisp
(defgeneric weblistener-output-marker (app history-number)
  (:documentation 
   "Returns the prefix string for user output printout (e.g., \"::\")"))


;; Generic defmethod defined in redisplay-listener.lisp
(defgeneric weblistener-input-forms (app)
  (:documentation
   "Generates html defining the input boxes as html forms."))


(defgeneric html-for-weblistener-title (app)
  (:documentation
   #.(one-string-nl
    "Creates HTML for text appearing at very bottom of Weblistener display"
    "e.g., \"BioLingua Listener v2.0\""
    )))

(defmethod html-for-weblistener-title ((app t))
  (html 
   (:big 
    (:i (:b (:princ-safe 
             (s+ "WebListener " (application-version app :mode :short))
             ))))))


(defgeneric html-for-weblistener-links (app)
  (:documentation 
   "Creates HTML for links appearing just above title at bottom of display"))

(defmethod html-for-weblistener-links ((app t))
  (flet ((pad () (html :newline (:td) (:td) (:td) (:td) (:td) (:td))))
    (html
     (:table
      (:tr 
       :newline
       (:td ((:a :href 
              (forward-funcall 'make-application-primitives-url) 
              :target "_blank")
             (:big "Available Primitives")))
       (pad)
       :newline
       (:td ((:a :href 
              (forward-funcall 'make-weblistener-toplevel-directories-url))
             (:big "Browse Files")))
       (pad)
       :newline
       (:td ((:a :href (forward-funcall 'make-web-widget-index-url))
             (:big "Web Tools")))
       (pad)
       :newline
       (:td ((:a :href (forward-funcall 'make-previous-sessions-url)) 
             (:big "Session Logs")))
       :newline
       )
      (:tr
       :newline
       (:td ((:a :href (forward-funcall 'make-upload-form-url)) 
             (:big "Upload File")))
       (pad)
       :newline
       (:td ((:a :href *hyperspec-url* :target "_blank")
             (:big "Lisp Hyperspec")))
       (pad)
       :newline
       (:td ((:a :href (frames::make-toplevel-frames-url))
             (:big "Frame Browser")))
       :newline
       )
      ))))


;; Generic defmethod defined in announce.lisp
(defgeneric output-announcements (app &key (msg-handler nil))
  (:documentation "Output via HTML any messages targeted to the current user"))


;;; Generic functions pertaining to the 'Read-Eval' part of the Weblistener's
;;; Read-Eval-Print loop.


;; Generic defmethod defined in rep-listener.lisp
(defgeneric weblistener-verify-evalstring (app string)
  (:documentation
   #.(one-string-nl
      "A hook to test the input STRING for problems."
      "Must error out (hopefully with a reasonable error message)"
      "to signal a problem with STRING.  If it returns the string is read"
      "to create a form which is fed to the evaluator."
      "This function should NOT generate output as such output is not"
      "captured for display."
      "The default method for this function does nothing, returning NIL."
      )))


;; Generic defmethod defined in rep-listener.lisp
(defgeneric weblistener-verify-evalform (app form)
  (:documentation
   #.(one-string-nl
      "A hook to test the form to be evaluated for problems."
      "Must error out (hopefully with a reasonable error message)"
      "to signal a problem with FORM.  If it returns evaluation proceeds."
      "(The default method calls WEBLISTENER-VERIFY-EVALFORM-COMPONENT,"
      "which for non-list forms returns immediately, while for lists calls"
      "WEBLISTENER-VERIFY-FUNCTION-CALL-FORM on the first"
      "element of the list, and then recurses on the subsequent elements.)"
      )))


;; Generic defmethod defined in rep-listener.lisp
(defgeneric weblistener-verify-function-call-form (app form main-form)
  (:documentation
   #.(one-string-nl
      "A hook to test a list subform of the form being evaluated"
      "for problems.  FORM is the subform to be tested and MAIN-FORM is"
      "the toplevel form being evaluated.  The default method insures that a"
      "specific set of functions is not called (directly)"
      "by the form.  This method needs to call ERROR to halt the read-eval"
      "process.  This method calls APPLICATION-VERIFY-FUNCTION-CALL-FORM"
      "after it is done with its own verifications.  See the generic method"
      "code for more details on what is checked for and how."
      )))


;; Generic defmethod defined in rep-listener.lisp
(defgeneric application-verify-function-call-form (app form main-form)
  (:documentation
   #.(one-string-nl
      "A hook to test a list subform of the form being evaluated"
      "for problems.  FORM is the subform to be tested and MAIN-FORM is"
      "the toplevel form being evaluated.  The default method does nothing."
      "This could also have been implemented as an :AFTER method to"
      "WEBLISTENER-VERIFY-FUNCTION-CALL-FORM."
      )))
   

;; Generic defmethod defined in rep-listener.lisp
(defgeneric weblistener-evaluate-form-with-timeout (app form)
  (:documentation
   #.(one-string-nl
      "A wrapper around the basic evaluation of FORM.  The default method"
      "implements a priority-setting algorithm and a timeout test before"
      "calling WEBLISTENER-EVALUATE-FORM to do the actual evaluation."
      )))


;; Generic defmethod defined in rep-listener.lisp
(defgeneric weblistener-evaluate-form (app form)
  (:documentation
   #.(one-string-nl
      "The actual evaluation of FORM.  The default method simply calls"
      "LISP:EVAL on FORM.  By implementing your own method you can alter"
      "the evaluation of FORM in any way you like."
      )))



;;; Generic functions pertaining to the auxiliary webpage displays.
;;; Auxiliary pages are, for instance, pages which show function
;;; lists or function/file source, the upload facility page, the
;;; frame browser, etc.


(defgeneric webpage-head-html (app title)
  (:documentation "HTML for title to appear on auxiliary web pages"))

(defmethod webpage-head-html ((app t) title)
  (html (:title "WebListener" (:princ-safe title))))


(defgeneric webpage-style-html (app)
  (:documentation 
   "HTML for the STYLE attribute for the body of an auxiliary web page"))

(defmethod webpage-style-html ((app t))
  (html 
   (:style "BODY {font-family: Verdana, Geneva, Arial; font-size: 10pt;}")))


(defgeneric webpage-font-faces (app) 
  (:documentation "A string defining the valid font faces"))
  
(defmethod webpage-font-faces ((app t)) "verdana, arial, helvetica")


(defgeneric webpage-toplinks (app &key (listener? t))
  (:documentation 
   #.(one-string-nl
      "HTML for anything (presumably links) you wish to appear below the"
      "title and above the body of an auxiliary web page."
      )))

(defmethod webpage-toplinks ((application t) &key (listener? t))
  (html
   (when listener? 
     (html 
      ((:a :href (forward-funcall 'return-hack-url)) "Listener") " "
      :newline))
   ((:a :href (make-weblistener-frame-find-url)) "FindFrames") " "
   :newline
   ((:a :href (forward-funcall 'make-weblistener-toplevel-directories-url))
    "File Viewer") " "
   :newline
   ))


;;; miscellaneous methods 

(defgeneric viewable-application-toplevel-directories (app)
  (:documentation "List of toplevel directories user is allowed to peruse"))

(defmethod viewable-application-toplevel-directories ((app t)) nil)


;; An application method can either return its own list
;; or append a list on to the result of calling CALL-NEXT-METHOD

(defparameter *apropos-packages* '(:utils :webuser)
  "Packages we search for exported functions and variables wrt HELP")

(defun weblistener-apropos-packages () *apropos-packages*)

(defgeneric application-apropos-packages (application)
  (:documentation 
   "Packages specific to the application to search through vis a vis HELP"))

(defmethod application-apropos-packages ((application t)) 
  (forward-funcall 'weblistener-apropos-packages))


(defgeneric application-from-email-address (app)
  (:documentation
   #.(one-string-nl
      "That address that will appear for the FROM field in email that"
      "a user sends from the weblistener to themselves using the EMAIL-ME"
      "command"
      )))

(defmethod application-from-email-address ((app t)) nil)


(defgeneric application-user-init-file (app)
  (:documentation 
   #.(one-string-nl
      "Returns a string naming a potential file in a user's application"
      "home directory.  This is just a file name, not a full path.")))

(defmethod application-user-init-file ((app t)) "weblistener.ini")


(defgeneric application-shared-files-directories (app)
  (:documentation
   #.(one-string-nl
      "Returns a list of directory pathnames which are 'user-shareable':"
      "e.g., any user can create, read, modify and delete files, and"
      "create subdirectories within, any of these directories and their"
      "subdirectories.  Furthermore, these directories are guarenteed to"
      "exist -- ENSURE-DIRECTORIES-EXIST is called on each one before"
      "returning the list.")))

(defmethod application-shared-files-directories ((app t))
  (let ((shared-directories
         (list
          (append-subdir *home-directory* "shared-files")
          )))
    (loop for dir in shared-directories do (ensure-directories-exist dir))
    shared-directories
    ))


(defgeneric application-toplevel-frames (app)
  (:documentation
   #.(one-string-nl
      "Returns a list of what the application considers its toplevel"
      "frames. A standard Weblistener link displays this information."
      "The list is in the form a sublists, each sublist of the form"
      "(category &rest frames)"
      )))

(defmethod application-toplevel-frames ((app t)) nil)


(defgeneric after-allegroserve-start-action (app)
  (:documentation
   #.(one-string-nl
      "This is called as the final action of wb:start-weblistener."
      "Define this method if you want something to happen when the"
      "weblistener (aka Allegroserve) is started.")))

(defmethod after-allegroserve-start-action ((app t)) nil)


(defgeneric after-allegroserve-stop-action (app)
  (:documentation
   #.(one-string-nl
      "This is called as the final action of wb:stop-weblistener."
      "Define this method if you want something to happen when the"
      "weblistener (aka Allegroserve) is stopped.")))

(defmethod after-allegroserve-stop-action ((app t)) nil)

(defgeneric initial-user-repl-mode (app)
  (:documentation 
   #.(one-string-nl
      "Put the user into a default REPL mode.")))

(defmethod initial-user-repl-mode ((app t)) nil)  

(defgeneric weblistener-repl-mode (app)
  (:documentation
   #.(one-string-nl
      "Describe the REPL mode the user is currently in and how to change it."
      )))

(defmethod weblistener-repl-mode ((app t)) nil)

(defgeneric user-mode-internal (app) 
  (:documentation 
   #.(one-string-nl
      "Determine the execution mode the weblistener is in."
      "Currently there are two modes the weblistener understands:"
      "  :bbl, the mode for Jeff Elhai's BBL language, and"
      "  anything else (which includes :biolisp).")))
  

(defmethod user-mode-internal ((app t)) t)




