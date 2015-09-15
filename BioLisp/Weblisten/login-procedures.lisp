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

;;; Author: JP Massar.

(defvar *after-new-login-actions* nil 
  #.(one-string-nl
     "Expressions to execute when someone logs in for the first time to"
     "a weblistener process."))

(defvar *after-new-session-actions* 
  (list 'log-new-session)
  #.(one-string-nl
     "Expressions to execute when someone logs in and creates a new session."))

(defvar *before-session-logout-actions* nil
  #.(one-string-nl
     "Expressions to execute immediately before a user logs out of a session."))

(defun add-action-item (form symbol)
  (set symbol (append (symbol-value symbol) (list form))))

(defun clear-action-items (symbol) (set symbol nil))

(defun execute-action-items (symbol) 
  (loop for action in (symbol-value symbol) do
        (cond
         ((symbolp action) (funcall action))
         ((listp action) (eval action))
         (t (error "Action ~A for ~A not a symbol or list." action symbol)))))
        
(defun login-failed (reason)
  (error (make-condition 'login-failed :reason reason)))

;;; Canonical representation of a user's package is the keyword
;;; naming the package.  The name of the package is the uppercase login name
;;; for the user, found in the USER-LOGIN slot of a login-record.

(defun canonicalize-user-package-designator (pd)
  (cond
   ((stringp pd) (keywordize (string-upcase pd)))
   ((symbolp pd) (keywordize (string-upcase (symbol-name pd))))
   ((packagep pd) (keywordize (package-name pd)))
   ))

(defun package-symbol-of-a-user (pd) 
  (canonicalize-user-package-designator pd))
(defun package-string-of-a-user (pd) 
  (symbol-name (canonicalize-user-package-designator pd)))
(defun package-of-a-user (pd)
  (find-package (canonicalize-user-package-designator pd)))



;;; The toplevel interface.




(defun login-and-create-package (username password email &aux real-login)

  (verify-a-username username)

  (setq real-login (verify-an-account username password email))

  (let* ((user-string (package-string-of-a-user real-login))
         (lowername (string-downcase user-string))
         (visitor-package-symbol (package-symbol-of-a-user real-login))
         (visitor-homedir (visitor-directory lowername))
         (existing? nil)
         )

    (verify-a-user-directory real-login visitor-homedir)

    (maybe-check-user-and-email-address real-login visitor-homedir)

    ;; Create a package for the visitor.  Its name is the login
    ;; name of the visitor.  It might already exist, because the
    ;; user might have logged in previously using the same or
    ;; a different session.

    (if (find-package visitor-package-symbol)
        (progn
          (when (member (find-package visitor-package-symbol) 
                        (symbol-value 'cl-user::*all-non-user-packages*))
            (login-failed 
             (formatn 
              #.(one-string-nl
                 "The name '~A' is already in use by the system."
                 "Please log in with a different name.")
              visitor-package-symbol)))
          (when (null (package-use-list (find-package visitor-package-symbol)))
            (login-failed 
             (formatn 
              (one-string-nl
               "Your package has been corrupted!!"
               "Your package does not now use any packages!!"
               "You cannot log in until this is fixed."
               "Either the system will have to be rebooted or you"
               "can contact a system administrator, or you can try"
               "to fix this yourself by logging in as someone else and"
               "doing package surgery (but only if you know exactly what"
               "you're doing!)"))))
          (setq existing? t))
      (handler-case
          (set-up-user-package visitor-package-symbol)
        (error 
         (c)
         (login-failed
          (formatn
           (one-string-nl
            "System failure:  Could not create user package ~A. "
            "Actual error is ~A "
            "Please notify system administrators. "
            "You cannot log in until this is rectified.")
           visitor-package-symbol c
           )))))

    ;; Initialize completion system for user
    
    (when (not existing?)
      (multiple-value-bind (okay? error-condition)
          (initialize-cih-for-user visitor-package-symbol)
        (unless okay?
          (login-failed
           (formatn
            (one-string-nl
             "System failure: Could not initialize your completion directory."
             "Please notify the system administrators."
             "You cannot log in until this is rectified."
             "Actual error: ~A")
            error-condition)))))

    
#||

    ;; Not used anymore since all allegrocache use is with sframes
    
    ;; Initialize database connection for user if appropriate
    #-:sframes
    (when (allegrocache-loaded?)
      (ecase (symbol-value 'cl-user::*acache-connection-mode*) 
        (:single nil) 
        (:multi 
         (handler-case 
             (setf (get visitor-package-symbol :acache-db-connection)
                   (forward-package-funcall 
                    :db.ac :open-network-database 
                    "localhost" (symbol-value 'cl-user::*allegrocache-port*)
                    ))
           (error 
            (c)
            (login-failed 
             (formatn 
              (one-string-nl
               "System failure: Could not open allegrocache database"
               "connection.  Please notify the system administrators."
               "You cannot login until this situation is rectified."
               "Actual error: ~A")
              c)))))))

||#

    (execute-action-items '*after-new-login-actions*)

    (values visitor-package-symbol existing?)

    ))

(defun set-up-user-package (visitor-package-symbol)
  (create-user-package visitor-package-symbol)
  (setf (get visitor-package-symbol :packages-used) 
        (package-use-list visitor-package-symbol))
  (setf (get visitor-package-symbol :login-time) (get-universal-time))
  (setf (get visitor-package-symbol :max-session-number) 0))

(defun maybe-check-user-and-email-address (real-login visitor-homedir)
  (when (and (eq cl-user::*allow-anonymous-logins* :with-email)
             (not (weblistener-guru-p real-login)))
    (unless (get real-login :login-email)
      (login-failed
       (formatn 
        (one-string-nl
         "Sorry, you must provide your email address in order to log on."
         "If you have logged on before to this server, the email address"
         "you provided at that time needs to be the same as the one you"
         "will now use. If your email address has changed, you need to"
         "contact the system administrator at"
         "~A"
         "and ask that they reset the email address we have for you."
         )
        *default-support-email-address*
        )))
    (let* ((email-file (merge-pathnames "bike-email.txt" visitor-homedir))
           (email-file-exists? (probe-file email-file))
           (supplied-email-address 
            (string-trim *whitespace* (get real-login :login-email))))
      (cond
       (email-file-exists? 
        (let ((stored-email-address 
               (string-trim *whitespace* (file-to-string email-file))))
          (unless (string-equal supplied-email-address stored-email-address)
            (login-failed 
             (formatn 
              (one-string-nl
               "Sorry, you provided your email address as ~A,"
               "but previously the system recorded an email address for"
               "this account which is different than the one you just"
               "provided."  
               ""
               "This could be because someone else is using the account"
               "named ~A, and you should choose a different login name."
               ""
               "Or, your email address has changed.  In this case, you need"
               "to contact the Biobike system administrator at ~A"
               "and have them change the email address that the system"
               "has stored for you.  Make sure you include the"
               "following information in your email message:"
               ""
               "Machine: ~A"
               "Port: ~A"
               "Biobike login name: ~A"
               )
              supplied-email-address 
              real-login 
              cl-user::*default-support-email-address*
              cl-user::*weblistener-machine-name*
              cl-user::*weblistener-port*
              real-login
              )))))
       ;; no email-address file
       (t
        (with-open-file (p email-file :direction :output :if-exists :supersede)
          (format p "~A" supplied-email-address)
          ))))))
        
         

    

(defvar *forced-sessionid-number* nil)

(defun new-sessionid (package)
  (let ((candidate 
         (keywordize 
          (string-upcase 
           (formatn "~A~D" package 
                    (or *forced-sessionid-number* 
                        (mod (get-universal-time) 100000)))
           ))))
    (if (member candidate (gethash package *user->sessionids-ht*))
        (progn (sleep 1) (new-sessionid package))
      candidate 
      )))

(defun connect-to-new-session (visitor-package-symbol existing? message)

  ;; The visitor's package exists, and his directory exists, and the user
  ;; has requested that he be connected to a new session (or there is no
  ;; existing session he could connect to).  
  
  ;; Store away copies of all the state variables we will need
  ;; to maintain this session's Lisp state, along with their current
  ;; values, after loading the user's init file, if any.

  (let* ((*package* (package-of-a-user visitor-package-symbol))
         (*username* visitor-package-symbol)
         (*sessionid* (new-sessionid visitor-package-symbol))
         (*in-history* nil)
         (*out-history* nil)
         (*oneline-form-data* "")
         (*multiline-form-data* "")
         (*user-display-id* 0)
         (*login-messages* nil)
         ;; Generate a new log file when someone initiates a new session
         (*log-file* (verify-a-user-log-file *username* *sessionid*))
         (*default-pathname-defaults* 
          (pathname (visitor-directory *username*)))
         ;; Should copy the readtable so one user cannot modify
         ;; his readtable and cause global changes?
         (*readtable* (application-readtable cl-user:*ai*))
         (*print-length* (find-pref-value '*print-length*))
         (*output-chars-per-line-limit* 
          (find-pref-value '*output-chars-per-line-limit*))
         (*output-lines-limit* (find-pref-value '*output-lines-limit*))
         (*execution-timelimit* (find-pref-value '*execution-timelimit*))
         (*safety* (find-pref-value '*safety*))
         )

    (log-user-event "Log file path: ~A" *log-file*)

    (progv 
        nil
        nil
      
      (setf (get *sessionid* :username) *username*)
      (setf (get *sessionid* :session-number) 
            (incf (get *username* :max-session-number)))
      (setf (get *sessionid* :session-creation-time) (get-universal-time))
      (setf (get *sessionid* :last-execution-time) (get-universal-time))
      (setf (get *sessionid* :logfile) *log-file*)
      (setf (get *sessionid* :user-provided-email) *user-provided-email*)
      (setf (get *sessionid* :truncation-message-mode) :all)

      (when (fboundp 'initialize-history-index) 
        (forward-funcall 'initialize-history-index))

      ;; Store away the value of all the user's state variables.

      (initialize-saved-variables 
       *sessionid* *weblistener-protected-global-vars*)

      ;; Load the user's initialization files and run the AFTER-USER-LOGIN
      ;; actions in the user's environment, and keep track of anything
      ;; that's printed as a result, and set *login-messages* so that
      ;; this output will get shown to the user when the Weblistener appears.

      (with-saved-variables-values (*sessionid*)

        (initial-user-repl-mode cl-user:*ai*)

        (record-initfile-message
         (formatn
          ";; Welcome~A to ~A, ~A.~A"
          (if existing? " back" "")
          (application-name cl-user:*ai*) 
          (get *username* :full-name)
          (if (get *username* :pseudouser) 
              (formatn "~%;; You are logged in with a guest account.")
            ""
            )))
        (record-initfile-message (s+ ";; " (connected-to-message)))
        (record-initfile-message 
          (formatn ";;  User ~A, session ID ~A." *username* *sessionid*))

        (record-initfile-message
         (with-output-to-string (z)
           (let ((*standard-output* z) 
                 (*error-output* z) 
                 (*trace-output* z))
             (handle-a-user-init-file)
             (read-prefs :create-saved-bindings t)
             (application-after-user-login-actions cl-user:*ai*)
             )))

        (when message (record-initfile-message (formatn "~A" message)))

        ;; This just prints out info about the REPL modes.

        (weblistener-repl-mode cl-user:*ai*)  

        ;; The login messages don't actually get shown to the user 
        ;; until a bit later

        (dolist (m (reverse *login-messages*)) (log-user-event "~A~%" m))
        
        (execute-action-items '*after-new-session-actions*)

        )

      (if *user-provided-email*
          ;; guru is providing his password in the email slot!
          ;; don't write it to the log!
          (if (weblistener-guru-p) 
              (log-user-event "Logged in as guru...~%")
            (log-user-event "Email: ~A~%" *user-provided-email*))
        (log-user-event "No email address provided.~%"))

      ;; Create a file of arglist information for all the functions
      ;; the user has access to.

      (handler-case 
          (progn
            (create-user-specific-arglist-data *username*)
            (log-user-event "Arglist information file created.~%"))
        (error
         (c)
         (record-initfile-message 
          (formatn "Could not create arglist data: ~A" c))
         ))

      ;; Initialize the input history / completion mechanism
      (when (initialize-cih-for-session *username* *sessionid*)
        (log-user-event "History cache initialized.~%"))

      ;; Success.  Add this session ID to the set of active session IDs
      ;; Return name of visitor's package.

      (pushnew *username* *logins*)
      (push *sessionid* (gethash *username* *user->sessionids-ht*))

      (log-system-event "User ~A, sessionID ~A login successful.~%"
                        *username* *sessionid*)

      *sessionid*

      )))

(defun connected-to-message ()
  (if user::*application-instance-pretty-name*
      (formatn
       "Connected to ~A (~A, port ~D)."
       user::*application-instance-pretty-name*
       (useful-machine-name-for-title)
       cl-user::*weblistener-port*
       )
    (formatn
     "Connected to ~A, port ~D."
     (useful-machine-name-for-title) 
     cl-user::*weblistener-port*
     )))

(defun valid-username? (username)
  (and (stringp username)
       (not (string-equal "NIL" username))
       (not (string= username ""))
       (not (= (length username) 1))
       (alpha-char-p (char username 0))
       (every #'(lambda (x) (alphanumericp x)) username)
       (every #'(lambda (x) (< (char-code x) 128)) username)
       ))

(defun valid-email? (email-address)
  (and (stringp email-address)
       (= (count #\@ email-address) 1)
       (every (lambda (x) (or (alphanumericp x) (find x "@._-"))) email-address)
       (let ((stuff (string-split email-address #\@)))
         (every (lambda (x) (plusp (length x))) stuff))
       ))
                
(defun verify-a-username (username)
  (unless (valid-username? username)
    (login-failed
     (formatn
      (one-string-nl
       "Login name '~A' is not valid. "
       "It must contain only English alphabetic or numeric characters, "
       "and the first character must be alphabetic.  It must be more than one"
       "character long, and may not be the word 'nil'.")
      username
      )))
  username)

(defparameter *system-gods* '(:massar :jpm :shrager :jshrager))

(defun verify-an-account (username password email &aux visitor-package-symbol)
  (let ((login-record (find-login username)))
      
    (cond

     ((and (listp login-record) (eq (car login-record) :password-file-error))
      (let ((error-condition (second login-record)))
        (login-failed
         (formatn
          (one-string-nl
           "*** SYSTEM ERROR !!! "
           "Password file has been corrupted!!! "
           "Please contact the system administrators and report this error."
           "Actual error: ~A"
           ""
           "(You will not be able to log in until this is fixed.) ***"
           )
          error-condition
          ))))

     (login-record
      (setq visitor-package-symbol 
            (canonicalize-user-package-designator (user-login login-record)))
      (let* ((super-secret-password (user-drowssap login-record))
             (status (user-status login-record))
             (expiration (user-expire login-record)))
        (cond
         ((not cl-user:*allow-anonymous-logins*)
          (when *guru-login-mode* 
            (unless (eq status :guru)
              (login-failed "You are not a guru!  Use normal login URL!")))
          (when (not (password-ok? password super-secret-password))
            (log-system-event "Invalid login: ~A & ~A~%" username password)
            (login-failed :invalid-password))
          (when (user-expired? expiration)
            (log-system-event 
             "Login try from expired account: ~A~%" visitor-package-symbol)
            (login-failed "Account has expired. Sorry.")
            ))
         (cl-user:*allow-anonymous-logins* 
          (cond
           ((null *guru-login-mode*)
            (when (eq status :guru)
              ;; There is no password box when anonymous logins
              ;; are allowed.  Instead a guru is supposed to know to
              ;; type his password into the email box.  
              (when (and 
                     (not (password-ok? email super-secret-password))
                     (not (member visitor-package-symbol *system-gods*))
                     )
                (login-failed
                 (one-string
                  "As a Guru, you must provide your password "
                  "when anonymous logins are allowed."
                  )))))
           (*guru-login-mode* 
            (unless (eq status :guru)
              (login-failed "You are not a guru!  Use normal login URL!"))
            (when (and 
                   (not (password-ok? password super-secret-password))
                   (not (member visitor-package-symbol *system-gods*))
                   )
              (login-failed :invalid-password)
              )))))
        ))

     (cl-user:*allow-anonymous-logins*
      (setq visitor-package-symbol 
            (canonicalize-user-package-designator username))
      (log-system-event "Anonymous login, user name given: ~A" username)
      )

     (t 
      (log-system-event "Invalid login: ~A &~A~%" username password)
      (login-failed "Unrecognized login or password")
      ))

    ;; Store things away for future reference.
    ;; The user may have an email address stored in the login file, 
    ;; and/or can provide one via the login page.  
    (let ((pwf-email (and login-record (user-email login-record)))
          (login-email email))
      ;; make sure if user did not provide email address it gets stored
      ;; as NIL instead of "".  Also get rid of any whitespace.  
      (cond
       ((null login-email) nil)
       (t 
        (setq login-email (string-trim *whitespace* login-email))
        (when (zerop (length login-email)) (setq login-email nil))
        ))
      (setf (get visitor-package-symbol :password-file-email) pwf-email)
      (setf (get visitor-package-symbol :login-email) login-email)
      (setf (get visitor-package-symbol :email) (or pwf-email login-email))
      )
    (setf (get visitor-package-symbol :full-name)
          (if login-record 
              (user-fullname login-record)
            (package-string-of-a-user username)
            ))
    (setf (get visitor-package-symbol :initial-login-time)
          (get-universal-time))

    visitor-package-symbol 

    ))

(defun password-ok? (password super-secret-password)
  (cond
   (*encrypted-passwords* 
    (or
     (equal (biobike-encrypted-password password) super-secret-password)
     (equal (biobike-encrypted-password "*") super-secret-password)))
   (t
    (or 
     (string= password super-secret-password)
     (string= "*" super-secret-password)
     ))))


(defun verify-a-user-directory (visitor-package-symbol visitor-homedir)

  ;; If necessary, create a directory for the user.
  ;; It will already exist if the user has logged on before
  ;; using the same login name.

  ;; If we can't create it, or can't get to it, then we have a problem.

  (handler-case
      (ensure-directories-exist 
       (merge-pathnames (make-pathname :name "foo") visitor-homedir))
    (error 
     () 
     (log-system-event 
      "User ~A login, could not create or access ~A~%"
      visitor-package-symbol visitor-homedir)
     (login-failed (formatn "Could not create or access ~A" visitor-homedir))
     )))


(defun verify-a-user-log-file (visitor-package-symbol new-sessionid)

  (when cl-user:*logs-directory*

    (let ((log-file 
           (make-timestamp-logfile 
            visitor-package-symbol 
            :suffix (subseq (reverse (string new-sessionid)) 0 2))
           ))

      ;; Make sure user log directory exists.

      (handler-case
          (ensure-directories-exist log-file)
        (error
         (c)
         (log-system-event
          (one-string
           "User ~A login, could not create or access ~A~%."
           "Actual error: ~A")
          visitor-package-symbol log-file c)
         (login-failed 
          (formatn 
           (one-string-nl
            "Could not create log file ~A. " 
            "Please inform the system administrators. "
            "You cannot log in until this is fixed.")
           log-file
           ))))

      ;; Create index file of previous user sessions

      (handler-case
          (create-log-files-index-for-user visitor-package-symbol)
        (error
         (c)
         (log-system-event
          (one-string
           "User ~A login, could not create index file for log directory."
           "~%Actual error: ~A")
          visitor-package-symbol c
          )))
        
      (let ((n-deleted (purge-log-directory visitor-package-symbol)))
        (when (and n-deleted (plusp n-deleted))
          (record-initfile-message
           (formatn 
            ";; ~D session log files more than ~D days old removed."
            n-deleted cl-user:*days-until-user-logs-purged*
            ))))

      log-file

      )))

(defun load-file-oops (file condition file-description notify?)
  (cformatt "Error loading ~A." file-description)
  (cformatt "File is: ~A" file)
  (cformatt "Actual error is: ~A" condition)
  (when notify? (cformatt "Please notify the system administrators.")))

(defun load-an-init-file (file)
  (let ((*load-verbose* nil) (*load-print* nil)) (load file)))

(defun user-initialization-file ()
  (merge-pathnames 
   (application-user-init-file cl-user:*ai*)
   (visitor-directory *username*)))
                                
(defun handle-a-user-init-file ()
  (let ((visitor-init-path (user-initialization-file)))
    (if (probe-file visitor-init-path) 
        (handler-case
            (progn
              (cformatt "Loading user init file ~A" visitor-init-path)
              (load-an-init-file visitor-init-path)
              (cformatt "Loaded user init file ~A" visitor-init-path)
              (log-user-event "Loaded user init file ~A~%" visitor-init-path)
              t)
          (error 
           (c)
           (load-file-oops
            visitor-init-path c "your initialization file" nil)
           nil))            
      (handler-case
          (with-open-file 
              (p visitor-init-path 
                 :direction :output :if-does-not-exist :create)
            (format p "(in-package ~S)~%" (keywordize *username*))
            (cformatt "Created empty initialization file ~A" visitor-init-path))
        (error
         (c)
         (cformatt "Could not create file ~A !!" visitor-init-path)
         (cformatt "Actual error: ~A" c)
         (cformatt "This should not be possible!")
         (cformatt "Please notify the system administrators.  Thanks.")
         nil
         )))))
      
(defun repl-login (name password)
  (setq name (string name))
  (multiple-value-bind (visitor-package-symbol existing?)
      (login-and-create-package name password nil)
    (cond
     ((null visitor-package-symbol) 
      (error "Internal error! LOGIN-AND-CREATE-PACKAGE returned NIL!"))
     (t
      (let ((new-sessionid
             (connect-to-new-session
              visitor-package-symbol existing? nil))) 
        (store-saved-variables-global-values new-sessionid)
        (utils::instantiate-saved-variables-values new-sessionid)
        )))))


(defun repl-logout ()
  (restore-saved-variables-global-values *sessionid*))


(defun store-saved-variables-global-values (key)
  (let* ((val (utils::get-saved-variable key))
         (vars (first val))
         (unique (utils::get-saved-variable :unique-value)))
    (setf (get key :saved-variables-global-values) 
          (list vars (loop for var in vars collect
			  (if (boundp var) (symbol-value var)  unique)))
          )))
                

(defun restore-saved-variables-global-values (key)
  (let* ((unique (utils::get-saved-variable :unique-value))
         (saved (get key :saved-variables-global-values))
         (vars (first saved))
         (values (second saved)))
    (loop for var in vars for val in values do
          (if (eq val unique) 
              (makunbound var)
            (setf (symbol-value var) val)
            ))))

(defun create-user-package 
       (package-name
        &key
        (nicknames nil)
        (symbol-names-to-shadow nil)
        (symbols-to-shadowing-import
         (application-symbols-to-shadowing-import cl-user:*ai*))
        (additional-symbols-to-shadowing-import nil)
        (enable-workspace? t)
        (packages-to-use (application-packages-to-use cl-user:*ai*))
        (additional-packages-to-use nil)
        (symbols-to-import nil)
        (symbol-names-to-intern nil)
        (symbol-names-to-export nil)
        )
  #.(one-string-nl
     "Create the user's home package or a package that is, by default,"
     "structured equivalently to"
     "a user's home package, save that it has a different name."
     "The various options allow the package's structure to be altered in"
     "what is hoped to be the obvious manner.")
  (let ((package (make-package package-name :use nil :nicknames nicknames)))
    (shadow symbol-names-to-shadow package)
    (shadowing-import symbols-to-shadowing-import package)
    (shadowing-import additional-symbols-to-shadowing-import package)    
    (when enable-workspace? (forward-funcall 'enable-workspace package))
    (use-package packages-to-use package)
    (when additional-packages-to-use 
      (use-package additional-packages-to-use package))
    (import symbols-to-import package)
    (dolist (s symbol-names-to-intern) (intern s package))
    (export symbol-names-to-export package)
    package
    ))

(defun make-sort-be-bbl-sort ()
  (vwhen (sort-symbol (find-symbol "SORT" *package*))
    (unintern sort-symbol *package*)
    (shadowing-import (list (find-symbol "SORT" :bbl)) *package*)))

(defun make-sort-be-shadowlisp-sort ()
  (vwhen (sort-symbol (find-symbol "SORT" *package*))
    (unintern sort-symbol *package*)
    (shadowing-import (list (find-symbol "SORT" :shadowlisp)) *package*)))

;; this function should actually be moved into bbl stuff
(defun user-package-to-bbl (package-name destroy-conflicting-symbols verbose?)
  (block exit
    (when (bbl-mode?)
      (when verbose? (cformatt "Currently in BBL mode..."))
      ;; In case user logs out and then logs in...
      (setq *print-pprint-dispatch* 
            (symbol-package-value :*bbl-print-pprint-dispatch* :bbi))
      (return-from exit nil))
    (let ((user-used-packages (package-use-list package-name)))
      (setf (get package-name :packages-used) user-used-packages)
      (loop for pkg in user-used-packages do 
            (unuse-package pkg package-name))
      (handler-case 
          (progn 
            (use-package :bbl package-name)
            (make-sort-be-bbl-sort)
            (setq *print-pprint-dispatch* 
                  (symbol-package-value :*bbl-print-pprint-dispatch* :bbi))
            )
        (error 
         (c) 
         (let* ((potential-conflicts
                 (symbols-in-conflict package-name :bbl (list :sort)))
                (real-conflicts              
                 (loop for s in potential-conflicts 
                       when (or (boundp s) (fboundp s)) 
                       collect s
                       )))
           (cond
            ((and real-conflicts (not destroy-conflicting-symbols)) 
             (loop for pkg in user-used-packages do
                   (use-package pkg package-name))
             (error 
              (one-string-nl
               ""
               "Problem(s) encountered attempting to put you in BBL mode."
               "***> YOU ARE STILL IN BIOLISP MODE <***"
               "The symbols listed below have function definitions or values"
               "(which presumably you gave them, because they are symbols"
               "in your package), but they conflict with symbols in the"
               "BBL package and therefore would be obliterated if you were to"
               "really use the BBL package.  You must resolve these conflicts"
               "before you can use the BBL package.  One way to do this is"
               "to rename the functions and variables and then unintern the"
               "conflicting symbols.  Another way to do this is to type"
               ""
               "(bbl-mode :destroy-my-symbols? t)"
               ""
               "Please ask a system administrator for help if you are"
               "not confident you know what you are doing."
               ""
               "Problematic symbols:"
               ""
               "~A"
               )
              (string-join (mapcar 'symbol-name real-conflicts) #\newline)
              ))
            (potential-conflicts 
             (loop for p in potential-conflicts do 
                   (when verbose?
                     (cformatt 
                      (one-string-nl
                      "Warning: Symbol ~S being removed,"
                      "replaced by BBL symbol.")
                      p))
                   (unintern p package-name))
             (loop for pkg in user-used-packages do 
                   (use-package pkg package-name))
             (user-package-to-bbl package-name nil verbose?))
            (t 
             (error 
              (one-string-nl
               "Unhandled error encountered attempting to put you in BBL mode."
               "Please report this error to the system administrators."
               "The actual error is:"
               "~A")
              c)))))))))

(defun user-package-to-biolisp (package-name)
  (let ((user-packages (get package-name :packages-used)))
    (when (or (null user-packages) 
              (null (find (find-package :wlisp) user-packages)))
      (cformatt "*** Internal inconsistency!!!!")
      (cformatt "*** Somehow the saved packages for BioLisp mode")
      (cformatt "*** got smashed to: ~S" user-packages)
      (cformatt "*** Trying to restore used-packages list to original...")
      (cformatt "*** Please report this to the system maintainers!!!!")
      (setq user-packages (application-packages-to-use cl-user::*ai*))
      )
    (handler-case 
        (progn 
          (unuse-package :bbl package-name)
          (do-symbols (v package-name) 
            (when (eq (symbol-package v) (find-package package-name))
              (loop named ploop for pkg in user-packages do
                    (do-external-symbols (e pkg) 
                      (when (and (symbol= v e) (not (eq v e))) 
                        (cformatt 
                         "Conflicting user's symbol ~A with package ~A."
                         v (if (packagep pkg) (package-name pkg) pkg))
                        (cformatt "User symbol is being uninterned.")
                        (unintern v package-name)
                        (return-from ploop nil)
                        )))))
          (let ((packages-to-use user-packages))
            (loop for pkg in packages-to-use do
                  (use-package pkg package-name))
            (make-sort-be-shadowlisp-sort)
            (setq *print-pprint-dispatch* 
                  (symbol-package-value :*biolisp-print-pprint-dispatch* :bbi)
                  )))
      (error 
       (c)
       (user-package-to-bbl package-name nil t)
       ;; must restore info about which packages the user was using 
       ;; since we were not able to restore all the previously used
       ;; packages to again be used, and USER-PACKAGE-TO-BBL stores
       ;; the packages currently being used.  
       (setf (get package-name :packages-used) user-packages)
       (cformatt "Problem reverting to Biolisp mode.  Staying in BBL mode.")
       (cformatt "Actual error: ")
       (error c)
       ))))

(defun user-mode (&optional (app cl-user:*ai*))
  "Returns the execution mode the Weblistener is in."
  (user-mode-internal app))

(defun bbl-mode? ()
  "Returns T if user is in the weblistener's BBL evaluation mode."
  (not (null 
        (member (find-package :bbl) 
                (if *username* 
                    (package-use-list *username*) 
                  (package-use-list *package*))))))

(defun biolisp-mode? ()
  "Returns T if user is in the weblistener's biolisp evaluation mode."
  (not (bbl-mode?)))

(defun bbl-mode (&key (permanent? nil) (destroy-my-symbols? nil) (verbose? t))
  #.(one-string-nl
     "If the user is not in BBL mode, puts the user in BBL mode"
     "or may error out with symbol conflicts (use (lisp:unintern <symbol>)"
     "to remove each such symbol conflict)."
     "If PERMANENT? is non-NIL, the command (bbl-mode) is appended"
     "to the end of the user's initialization file (biolisp.ini),"
     "unless it is already present in the file.")
  (user-package-to-bbl *username* destroy-my-symbols? verbose?)
  (setf (get wb::*sessionid* :session-language) :bbl)
  (when permanent? (add-form-if-not-present-to-ini-file '(bbl-mode)))
  :bbl)
  
(defun bbl (&rest args) (apply 'bbl-mode args))

(defun biolisp-mode (&key (permanent? nil))
  #.(one-string-nl
     "Returns the user to standard biolisp mode from bbl mode."
     "If PERMANENT? is non-NIL, the command (biolisp-mode) is appended"
     "to the end of the user's initialization file (biolisp.ini),"
     "unless it is already present in the file.")
  (if (biolisp-mode?)
      (cformatt "Currently in BioLisp mode...")
    (progn
      (user-package-to-biolisp *username*)
      (setf (get wb::*sessionid* :session-language) :lisp)
      ))
  (when permanent? (add-form-if-not-present-to-ini-file '(biolisp-mode)))
  :biolisp)

(defun lisp (&rest args) (apply 'biolisp-mode args))  

(defun ensure-bbl-mode ()
   (bbl-mode :permanent? t)
   (cformatt "You are now in BBL mode")
   (values))

(defun add-form-if-not-present-to-ini-file (form)
  (let ((uif (user-initialization-file))
        (form-text (formatn "~S" form)))
    (unless (probe-file uif) 
      (error "Initialization file ~A does not exist!" uif))
    (let ((uif-text (file-to-string uif)))
      (if (search form-text uif-text) 
          (cformatt "Initialization file already contains ~S command" form)
        (progn
          (with-open-file (p uif :direction :output :if-exists :append)
            (format p "~%~A~%" form-text)
            )
          (cformatt "Command ~S added to end of initialization file." form)
          )))))

(defun reload ()
  "Reload user's initialization files"
  (setq *login-messages* nil)
  (handle-a-user-init-file)
  )

(defun record-initfile-message (message) (push message *login-messages*))
(defun record-initfile-load (file) 
  (record-initfile-message (formatn ";; Loaded ~A" (namestring file))))
(defun record-initfile-load-error (file)
  (record-initfile-message 
   (formatn ";; Error loading ~A" (namestring file))))

  
(defun logout (&key (sessionid wb:*sessionid*) (verbose? t))
  #.(one-string-nl
     "By default logs you out of your current session."
     "If SESSIONID is a number, it will log you out of the session identified"
     "by that number as with the output of (SESSIONS)."
     "If SESSIONID is an actual sessionid (eg something like :massar83521)"
     "it will log you out of that session."
     "If SESSIONID is :all it will log you out of all of your sessions."
     "If SESSINOID is :all-others it will log you out of every existing session"
     "except the one you are typing at. (Use this to clean up sessions you"
     "may have lying around that you don't need anymore.")
  (flet ((info->id (session-info) (getf (second session-info) :session-number)))
    (let ((all-sessions-info (user-session-info wb:*username*)))
      (cond
       ((eq sessionid :all)
        (let ((sessions 
               (mapcar (lambda (id) (logout :sessionid id :verbose? nil)) 
                       (mapcar 'first all-sessions-info))))
          (cformatt "You are now logged out from all ~D of your sessions."
                    (length sessions))
          (when verbose? 
            (cformatt "Terminated sessions: ~S" sessions))
          (cformatt 
           "Histories for these sessions are available in your session logs.")
          (cformatt "No further execution of forms is possible.")
          (cformatt "Hit ENTER to bring up a login prompt.") 
          ))
       ((eq sessionid :all-others)
        (let* ((all-other-sessions 
                (remove-if 
                 (lambda (x) (eq (first x) wb:*sessionid*)) all-sessions-info))
               (logged-out-sessions 
                (mapcar (lambda (id) (logout :sessionid id :verbose? nil))
                        (mapcar 'first all-other-sessions)
                        )))
          (cformatt 
           "You are now logged out from all ~D of your other sessions."
           (length all-other-sessions))
          (cformatt 
           "Histories for these sessions are available in your session logs.")
          (when verbose? 
            (cformatt "Terminated sessions: ~S" logged-out-sessions))
          nil))
       ((symbolp sessionid) 
        (if (null (find sessionid all-sessions-info :key 'first))
            (progn
              (cformatt 
               "No current session identified by session ID ~S found" sessionid)
              (cformatt 
               "Current sessionids: ~S" (mapcar 'first all-sessions-info))
              (cformatt "No action taken."))
          (progn 
            ;; Remove association between sessionid and user
            (setf (get sessionid :username) nil)
            ;; Remove this session from list of user's sessions
            (setf (gethash *username* *user->sessionids-ht*)
                  (remove sessionid (gethash *username* *user->sessionids-ht*)))
            (if (eq sessionid *sessionid*)
                (progn
                  (clear-history :logout)
                  (execute-action-items '*before-session-logout-actions*))
              (with-saved-variables-values (sessionid) 
                (setq *in-history* nil) 
                (setq *out-history* nil)
                (setq * nil) (setq ** nil) (setq *** nil)
                (setq / nil) (setq // nil) (setq /// nil)
                (setq + nil) (setq ++ nil) (setq +++ nil)
                (execute-action-items '*before-session-logout-actions*)))
            (if (eq sessionid *sessionid*)
                (when verbose?
                  (cformatt "You are now logged out from this session.")
                  (cformatt 
                   "History for the session is available in your session logs.")
                  (cformatt "No further execution of forms is possible.")
                  (cformatt "Hit ENTER to bring up a login prompt.") 
                  )
              (when verbose?
                (cformatt "You are now logged out from session ~S." sessionid)
                (cformatt 
                 "History for that session is available in your session logs.")
                ))
            sessionid)))
       ((integerp sessionid)
        (let ((actual-sessionid 
               (first (find sessionid all-sessions-info :key #'info->id))))
          (if (null actual-sessionid) 
              (progn
                (cformatt 
                 "No session identified by session number ~S found" sessionid)
                (cformatt "Current session numbers: ~S" 
                          (mapcar #'info->id all-sessions-info))
                (cformatt "No action taken."))
            (logout :sessionid actual-sessionid))))
       (t (error "Invalid sessionid argument to LOGOUT"))
       ))))
        
(defun log-new-session ()
  (handler-case 
      (with-open-file
          (p *system-login-file* 
             :direction :output :if-exists :append :if-does-not-exist :create)
        (format p "~A : " (make-timestamp-string))
        (format 
         p "~A" (s+ "User " wb::*username* " ; new session " wb::*sessionid*))
        (force-output p)
        )
    (error () nil)
    ))
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar cl-user::*programatic-sessionid* nil)
(defvar cl-user::*docuser-sessionid* nil)

(defun dummyuser (user)
  (multiple-value-bind (visitor-package-symbol existing?)
      (wb::login-and-create-package (string user) "" nil)
    (cond
     (existing? nil)
     ((null visitor-package-symbol) 
      (error 
       "Internal error! LOGIN-AND-CREATE-PACKAGE returned NIL for ~A!" user))
     (t 
      (let ((wb::*forced-sessionid-number* 0))
        (wb::connect-to-new-session
         visitor-package-symbol existing? nil
         ))))))

(defun evaluser 
       (&optional (username cl-user:*enable-default-user-for-programatic-eval*))
  (format t "~%~%;; ~A user being logged in (for programatic access)~%"
          username)
  (setq cl-user::*programatic-sessionid* (dummyuser username))
  (format t "~%;; ~A user logged in with session ID ~S" 
          username cl-user::*programatic-sessionid*
          ))

(defun docuser (&optional (username :docuser))
  (format
   t "~%~%;; ~A user being logged in (for documentation access)~%" username)
  (setq cl-user::*docuser-sessionid* (dummyuser username))
  (format t "~%;; ~A user logged in with session ID ~S" 
          username cl-user::*docuser-sessionid*
          ))

(defun system-dummy-users ()
  (let ((evaluser cl-user:*enable-default-user-for-programatic-eval*)
        (docuser :docuser))
    (when evaluser (evaluser evaluser))
    (when t (docuser docuser))
    ))


