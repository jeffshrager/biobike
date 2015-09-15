;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

(in-package :weblistener)

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

;;; author: JP Massar

(defvar *sessionids-enabled?* t)
(defvar *guru-login-mode* nil)
(defvar *weblogin-form-response-url* "/weblogin-form-response.html")
(defvar *forgotten-password-response-url* "/forgotten-password-response.html")
(defvar *old-user-new-session-url* "/old-user-new-session.html")
(defvar *show-all-sessions-url* "/show-all-sessions.html")

(define-condition login-failed (error)
  ((reason :initarg :reason :reader login-failed-reason))
  (:report 
   (lambda (condition stream)
     (format stream "Login attempt failed: ~A" (login-failed-reason condition))
     )))

(defun user-session-id () *sessionid*)
(defun user-session-id-symbol () '*sessionid*)
(defun user-session-package ()
  (find-package (user-session-package-name)))
(defun user-session-package-name () (get *sessionid* :username))

;;;;;;;;;;;;;;;;;;; LOGIN FACILITY USING STANDARD FORM


;; This has to be a function (as opposed to just doing a PUBLISH at toplevel)
; because it needs to be called
;; after the application has been loaded.  But this code gets
;; loaded before the application has been defined.

;; It's called by the generic initialization method called
;; APPLICATION-INITIALIZATIONS

(defun define-login-webpage-urls ()
  (publish 
   :path (login-webpage-url cl-user:*ai*)
   :content-type cl-user::*html-publish-content-type*
   :function 
   (lambda (req ent) (new-weblogin-function cl-user:*ai* req ent))
   )
  (publish 
   :path (new-login-webpage-url cl-user:*ai*)
   :content-type cl-user::*html-publish-content-type*
   :function 
   (lambda (req ent) (new-weblogin-function cl-user:*ai* req ent))
   )
  (publish 
   :path (guru-login-webpage-url cl-user:*ai*)
   :content-type cl-user::*html-publish-content-type*
   :function 
   (lambda (req ent) 
     (let ((*guru-login-mode* t))
       (weblogin-function cl-user:*ai* req ent))
     )))
  
;;; Mike Travers magic to make, e.g. http://localhost:8000/
;;; be the same as http://localhost:8000/biologin
;;; I don't really understand what this is doing, but it seems to work.
(publish
 :path "/" 
 :function 
 #'(lambda (req ent)
     (with-http-response 
      (req ent :response *response-found*)
      (setf (reply-header-slot-value req :location) 
            (login-webpage-url cl-user:*ai*))
      (with-http-body (req ent))
      )))

(defmethod new-weblogin-function ((app t) req ent)
  (weblogin-function app req ent))
  

(defmethod weblogin-function ((app t) req ent)
  (with-http-response-and-body (req ent)
    (html 
     (:title (:princ-safe (useful-weblistener-title)))
     :br
     (:body 
      (login-header cl-user:*ai*)
      ((:form :method "post" :action *weblogin-form-response-url*)
       ((:input :type "hidden" :name "guru" :value *guru-login-mode*))
       :br
       (:table
        (if cl-user:*allow-anonymous-logins*
            (html
             (:tr
              (:td (:big "LOGIN NAME: "))
              (:td ((:input :type "text" :name "loginname")))
              (:td (:i ((:font :color "green")
                        "(prior registration not required)")))
              )
             ;; Guru is supposed to type password into email box now.
             (:tr
              (:td (:big "EMAIL ADDRESS: "))
              (:td ((:input :type "text" :name "email")))
              (:td (:i "(optional but advised)"))
              )
             )
          (html
           (:tr
            (:td (:big "LOGIN NAME: "))
            (:td ((:input :type "text" :name "loginname")))
            )
           (:tr
            (:td (:big "PASSWORD  : "))
            (:td ((:input :type "password" :name "password"))))))
        (:tr
         (:td ((:input :type "submit" :value "Previous Session" 
                :name "prevsession"))))
        (:tr 
         (:td ((:input :type "submit" :value "New Login" :name "newlogin")))
         )
        
        ))
      
      (:table 
       (:tr
        (:td (login-addenda cl-user:*ai*))))
      (login-credits cl-user:*ai*)
      (login-disclaimer cl-user:*ai*)
      
      ))))

;;; Handle the form data coming back from the LOGIN page.
;;; Either log the user in and go to the Weblistener or
;;; print out an error message and ask the user to try again.

(publish 
 :path *weblogin-form-response-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent) (a-login-form-response-function cl-user:*ai* req ent)))

(defvar *user-provided-email* nil)

(defun login-error-page 
       (message &key (login-page-url-function 'login-webpage-url))
  (html
   :br (:big (:princ "USER LOGIN ERROR:")) :br
   (let ((lines (string-split message #\Newline)))
     (loop for line in lines do (html (:princ-safe line) :br)))
   :br :br
   ((:a :href (funcall login-page-url-function cl-user:*ai*)) "Try again")
   ))
  
(defmethod a-login-form-response-function ((app t) req ent)
  (with-http-response-and-body (req ent)
    (let* ((input (request-query req))
           (login (space-trim (url-parameter-value :loginname input)))
           (password (or (url-parameter-value :password input) ""))
           (email (url-parameter-value :email input))
           (newlogin? (url-parameter-value :newlogin input))
           (prevsession? (url-parameter-value :prevsession input))
           (*user-provided-email* email)
           (*guru-login-mode* (url-parameter-value :guru input))
           )
      (setq *guru-login-mode* 
            (and *guru-login-mode* (string-equal "T" *guru-login-mode*)))
      (flet ((oops (msg &optional (password-message? nil))
               (declare (ignore password-message?))
               (login-error-page msg)
               ))
        (handler-case
            (if (or (null login) (equal login ""))
                (oops "No login name provided!")
              (multiple-value-bind (visitor-package-symbol existing?)
                  (progn
                    (login-and-create-package login password email))
                (cond
                 ((null visitor-package-symbol)
                  (oops "Ierror! LOGIN-AND-CREATE-PACKAGE returned NIL!"))
                 (newlogin?
                  (let ((new-sessionid
                         (progn
                           (connect-to-new-session
                            visitor-package-symbol existing? nil))))
                    (html (:princ (indirect-to-redisplay 0 new-sessionid)))
                    ))
                 (prevsession?
                  (progn
                    (previous-session-page visitor-package-symbol existing?)))
                 (t
                  (oops "Internal error: Impossible to reach clause in cond"))
                 )))
          (login-failed 
           (c)
           (let ((reason (login-failed-reason c)))
             (cond
              ((eq reason :invalid-password) (oops "Invalid password" t))
              (t (oops (login-failed-reason c)))
              )))
          (error (c) (oops (formatn "Unknown failure: ~A" c)))
          )))))

(publish 
 :path *forgotten-password-response-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent) (forgotten-password-response-function cl-user:*ai* req ent)))

(defmethod forgotten-password-response-function ((app t) req ent)
  (with-http-response-and-body (req ent)
    (let* ((input (request-query req))
           (login (space-trim (url-parameter-value :loginname input))))
      (let* ((login-record (find-login login))
             (super-secret-password (user-drowssap login-record))
             (email (user-email login-record)))
        ;; no email if null or null string
        (if (plusp (length email)) 
            (progn
              (forward-funcall
               'email-me-my-password
               email login super-secret-password)
              (html 
               "Your password has been emailed to you at:"
               :br :br
               (:princ-safe email)
               ))
          (html 
           (:big "The BioBike system has no email address for you!"
            :br :br 
            "Contact the system administrator for this site."
            )))))))


(defun previous-session-page (user-symbol existing?)
  (let ((sessions (user-session-info user-symbol)))
    (cond
     ;; user asked to see previous sessions but there aren't any
     ((null sessions) 
      (let ((new-sessionid 
             (connect-to-new-session
              user-symbol
              existing? 
              (formatn ";; No previous sessions (System reboot at ~A)"
                       (make-timestamp-string 
                        :universal-time *system-startup-time*
                        :mode :mmddyyhhmm
                        ))
              )))
        (html (:princ (indirect-to-redisplay 0 new-sessionid)))
        ))
     ;; there's at least one session the user could choose from,
     ;; and we give the user the option of starting a new session
     (t
      (html 
       (:h3 
        :p
        (:b 
         (:center 
          (:princ-safe (formatn "Existing Sessions for User ~A" user-symbol)
           ))))
       (:big
        ((:font :color "green")
         (:ul
          (:li 
           (:princ-safe 
            (one-string
             "To choose a previous session click on the appropriate link "
             "under the 'Session Login Time' or 'Session #' columns.")))
          (:li 
           "To start a new session, click on 'Start a New Session'")
          (:li
           (:princ-safe
            (one-string
             "To log out of a previous session select that session "
             "and type '(logout)' at the Weblistener.")))
          (:li 
           (:princ-safe
            (one-string
             "If you select a VPL session, make sure you kill any other "
             "VPL window associated with that session.")))
          )))
       :p
       (:table 
        (:tr 
         (:td (:b "Session # &nbsp"))
         (:td (:b "Session Login Time &nbsp;" ))
         (:td (:b "Last Activity at &nbsp"))
         (:td (:b "Link to Session Log &nbsp;"))
         (:td (:b ""))
         (:td (:b "Last Form Evaluated"))
         )
        (loop for session-info in sessions 
              as id = (first session-info)
              as info = (second session-info)
              as session-number = (getf info :session-number)
              as etime = (getf info :last-execution-time)
              as login-time = (getf info :session-creation-time)
              as log-file = (getf info :log-file)
              as last-input = (getf info :last-input)
              as vpl-session? = (get id :vpl-session?)
              do 
              (html 
               (:tr 
                ;; session number (with link)
                ((:td :align "center")
                 ((:a :href (old-session-url id))
                  (:princ-safe (formatn "#~2D" session-number))))
                ;; login time (with link)
                ((:td :align "center")
                 (if (null login-time)
                     (html (:princ-safe "unknown"))
                   (html 
                    ((:a :href (old-session-url id))
                     (:princ-safe 
                      (make-timestamp-string
                       :universal-time login-time :mode :mmddyyhhmm))))))
                ;; last execution time
                (:td
                 (:princ-safe 
                  (if etime 
                      (make-timestamp-string 
                       :universal-time etime :mode :mmddyyhhmm)
                    "unknown")))
                ;; session log file (with link)
                ((:td :align "left")
                 ((:a :href (session-log-url id log-file))
                  (:princ-safe 
                   (if log-file (file-namestring log-file) "none"))))
                (:td)
                ;; last command executed 
                ((:td :align "left")
                 (:princ-safe 
                  (if last-input (limited-string last-input 30) "-")))
                ))
              (when vpl-session? 
                (html
                 (:tr 
                  (:td " ")
                  ((:td :align "left")
                   ((:a :href (make-vpl-start-url :pkg id))
                    (:princ-safe "--> VPL session (Reset).")
                    )))
                 #+does-not-work
                 (:tr
                  (:td " ")
                  ((:td :align "left")
                   ((:a :href (make-vpl-share-url :pkg id))
                    (:princ-safe "--> VPL session (Share).")
                    )))
                 ))))
       :p
       ((:a :href 
         (one-string 
          *old-user-new-session-url* (formatn "?user=~A" user-symbol)))
        "Start a New Session")  

       )))))

(defun previous-vpl-session-page (user-symbol existing?)
  (let ((sessions (user-session-info user-symbol)))
    (cond
     ;; user asked to see previous sessions but there aren't any
     ((null sessions) 
      (let ((new-sessionid 
             (connect-to-new-session
              user-symbol
              existing? 
              (formatn ";; No previous sessions (System reboot at ~A)"
                       (make-timestamp-string 
                        :universal-time *system-startup-time*
                        :mode :mmddyyhhmm
                        ))
              )))
        (html (:princ (indirect-to-redisplay 0 new-sessionid)))
        ))
     ;; there's at least one session the user could choose from,
     ;; and we give the user the option of starting a new session
     (t
      (html 
       (:h3 
        :p
        (:b 
         (:center 
          (:princ-safe (formatn "Existing Sessions for User ~A" user-symbol)
           ))))
       (:big
        ((:font :color "green")
         (:ul
          (:li 
           (:princ-safe
            "CHOOSE A SESSION: Click the session number of an existing session"
            ))
          (:li 
           "VIEW A SESSION LOG: Click "
           ((:font :color "blue") "log")
           " for the desired session"
           )
          (:li
           (:princ-safe
            "CLOSE A SESSION: Go into the session and click EXIT"
            ))
          (:li 
           "OPEN A SAVED SESSION: Click "
           ((:font :color "blue") "Start a New Session")
           " then select "
           :br
           (:tt "SESSION --> Workspaces (list), and use the 'Restore' link")
           )
          )))
       :p
       (:table 
        (:tr 
         (:td ((:font :size "+1")
               (:u (:b "Session")) "&nbsp;&nbsp;"))
         (:td ((:font :size "+1")
               (:u (:b "Session Login Time")) "&nbsp;&nbsp;"))
         (:td ((:font :size "+1") 
               (:u (:b "Last Activity at")) "&nbsp;&nbsp;"))
         (:td ((:font :size "+1")
               (:u (:b "Link to Session Log")) "&nbsp;&nbsp;"))
         )

        (loop for session-info in sessions 
              as id = (first session-info)
              as info = (second session-info)
              as session-number = (getf info :session-number)
              as etime = (getf info :last-execution-time)
              as login-time = (getf info :session-creation-time)
              as log-file = (getf info :log-file)
              as vpl-session? = (get id :vpl-session?)
              when vpl-session?
              do 
              (html 
               (:tr 
                ;; session number (with link)
                ((:td :align "center")
                 ((:a :href (make-vpl-start-url :pkg id))
                  (:princ-safe (formatn "Session ~2D" session-number))))
                ;; login time (with link)
                ((:td :align "center")
                 (if (null login-time)
                     (html (:princ-safe "unknown"))
                   (html 
                    (:princ-safe 
                     (make-timestamp-string
                      :universal-time login-time :mode :mmddyyhhmm)))))
                ;; last execution time
                ((:td :align "center")
                 (:princ-safe 
                  (if etime 
                      (make-timestamp-string 
                       :universal-time etime :mode :mmddyyhhmm)
                    "unknown")))
                ;; link to session log file 
                ((:td :align "center")
                 ((:a :href (session-log-url id log-file))
                  (:princ-safe (if log-file "log" "none"))))
                (:td)
                ))
              ))
       :p
       ((:a :href 
         (one-string 
          *old-user-new-session-url* (formatn "?user=~A" user-symbol)))
        "Start a New Session ")
       :br :br
       (:b
        (:princ-safe
         (one-string
          "WARNING!  Sessions may be lost without notice!  Consider saving "
          "your session permanently.  "
          )))
       :br
       ((:a :href "ajax/usersessions.html") 
        (:b "Here's how."))
       :br :br
       ((:a :href (formatn "~A?user=~A" *show-all-sessions-url* user-symbol))
        "Show all my sessions"
        ))))))
       


(publish 
 :path *old-user-new-session-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent) 
   (with-http-response-and-body (req ent)
     (let* ((input (request-query req))
            (user (url-parameter-value :user input))
            (user-symbol (keywordize user))
            (nsessions (length (gethash user-symbol *user->sessionids-ht*))))
       (let ((new-sessionid 
              (connect-to-new-session 
               user-symbol t 
               (formatn 
                ";; New session begun (~D previous session~P)"
                nsessions nsessions
                )))
             (code-creation-mode (get user-symbol :login-code-creation-mode)))
         ;; This stuff should be methodized for BioLingua 
         (cond 
          ((or (null code-creation-mode)
               (eq code-creation-mode :weblistener))
           (html (:princ (indirect-to-redisplay 0 new-sessionid))))
          ((eq code-creation-mode :vpl)
           (forward-funcall 'init-vpl)
           (html (:princ (indirect-to-vpl new-sessionid))))
          ))))))

(publish 
 :path *show-all-sessions-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent) 
   (with-http-response-and-body (req ent)
     (let* ((input (request-query req))
            (user (url-parameter-value :user input))
            (user-symbol (keywordize user))
            )
       (previous-session-page user-symbol t)
       ))))

(defun old-session-url (sessionid)
  (make-weblistener-evalstring-url 
   :pkg sessionid :evalstring ":session-reattached"))
       
(defun sessionid-variable (variable sessionid)
  (saved-variable-value sessionid variable :if-no-key nil))
              
(defun session-log-url (sessionid logfile)
  (make-log-viewer-url 
   :pkg sessionid
   :file (url-safe-string (namestring logfile))))


(defun user-session-info (user)
  (let* ((sessionids (copy-list (gethash user *user->sessionids-ht*)))
         (sessions 
          (sort sessionids '> 
                :key (lambda (x) (or (get x :last-execution-time) 0)))))
    (loop for id in sessions collect 
          (list 
           id
           (list 
            :session-number (get id :session-number)
            :last-execution-time (get id :last-execution-time)
            :session-creation-time (get id :session-creation-time)
            :log-file (sessionid-variable '*log-file* id)
            :last-input 
            (vif (last-input (first (sessionid-variable '*in-history* id)))
                 (inhist-string last-input)
                 "")
            )))))

(defun some-vpl-session? (sessions)
  (loop for session-info in sessions 
        as id = (first session-info)
        as vpl-session? = (get id :vpl-session?)
        do 
        (when vpl-session? (return t))
        ))

(defun some-weblistener-session? (sessions)
  (loop for session-info in sessions 
        as id = (first session-info)
        as vpl-session? = (get id :vpl-session?)
        do 
        (unless vpl-session? (return t))
        ))
          
          

          
       
     


