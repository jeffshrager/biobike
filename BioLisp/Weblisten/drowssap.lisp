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

(defparameter *xyzzy* nil)

#+allegro
(defvar *password-file-lock* (mp:make-process-lock :name "Password file lock")
  "Lock used to protect region where we swap in a new password file.")


(defparameter *report-accounts-stats-form*
 '(format 
   t "~&;; Passwords (re)loaded. Number of accounts: ~D~%" (length *xyzzy*)))

;;;; FORMAT OF ENTRIES IN PASSWORD FILE:

;; (POSSIBLE-MATCHES LOGIN-NAME PASSWORD FULL-NAME
;;   EMAIL EXPIRATION STATUS &REST OTHER-STUFF)
  
;; SAMPLE PASSWORD ENTRIES:

;; A user with many aliases and a null password and no expiration.

;;  (("ME" "MYSELF" "I" "THE-TOP-DOG" "PRETZEL")  PRETZEL "*" "Pretzel Dog" 
;;    "pretzel@itsadogslife.com" :never :standard)

;; A user with no aliases, a password, and an expiration.

;;  (("GEORGEB") GEORGEB "saddam" "George W. Bush" 
;;   "<mrbig@whitehouse.gov>" (11 05 2008) :guru)

 


;;;; NOTES ABOUT PASSWORD FILE AND RELATED ISSUES

;; *** WARNING:
;; the system finds the user's standard init file
;; based on the login name (in (translate-simple-lp "home:login-name;")).
;; And the package the user is placed in is based on the login name.
;; So if a user normally uses a nickname (one of the 'possible matches'
;; that is not his login name) to log in, he might be confused!

;; In the actual password file these two lines should exist, and they
;; should be uncommented (assuming you derive the password file from
;; a template).

;; (format t "~&;; Passwords (re)loaded. Number of accounts: ~D~%"
;;         (length *xyzzy*))


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun drowssap-file ()
    (merge-pathnames
     (make-pathname :name "drowssap" :type "lisp")
     cl-user:*password-directory*
     ))

  (defun old-drowssap-file ()
    (merge-pathnames
     (make-pathname 
      :name (s+ "drowssap-" (make-timestamp-string :mode :stdfull))
      :type "lisp")
     cl-user:*password-directory*
     ))

  )

(defun sample-drowssap-file ()
  (translate-simple-lp "biol:sample-drowssap.lisp"))

;;; Load the actual password file at startup.

(load (drowssap-file))

(defmacro with-password-file-read (&body body)
  `(let ((*xyzzy* nil))
     (load (drowssap-file))
     ,@body
     ))

(defmacro with-password-file-read-and-locked (&body body)
  `(let ((*xyzzy* nil))
     (#+:allegro 
      mp:with-process-lock
      #+allegro
      (*password-file-lock*)
      #-:allegro 
      progn
      (load (drowssap-file))
      ,@body
      )))
                 

;;; Accessors for the records in the password list.

(defun user-matches (ur) (first ur))
(defun user-login (ur) (second ur))
(defun user-drowssap (ur) (third ur))
(defun user-fullname (ur) (fourth ur))
(defun user-email (ur) (fifth ur))
(defun user-expire (ur) (sixth ur))
(defun user-status (ur) (seventh ur))
(defun user-other-stuff (ur) (nthcdr 7 ur))

(defun set-user-drowssap (ur value) (setf (third ur) value))

(defun add-new-login 
       (match login password fullname email expire status other-stuff)
  (setq *xyzzy*
	(remove login *xyzzy*
		:test #'string-equal
		:key #'second))
  (push 
   (append (list match login password fullname email expire status) other-stuff)
   *xyzzy*))

(defun find-login (loginname)
  ;; Reload in case someone's added a new user, 
  ;; or changed user passwords, or somesuch.
  (handler-case
      (progn 
        (load (drowssap-file))
        (encrypted-check)
        (find-login-record loginname))
    (error (c) (list :password-file-error c))
    ))

(defun find-login-record (loginname)
  (block exit
    (loop for entry in *xyzzy* do
          (when (member loginname (user-matches entry) :test 'equalp)
            (return-from exit entry)
            ))    
    nil))

(defun find-user-login-record (&optional (user *username*))
  (block exit 
    (loop for entry in *xyzzy* do 
         (when (symbol= user (user-login entry))
            (return-from exit entry)
            ))))

(defun find-user-login-datum (datum value? &optional (user *username*))
  (let ((entry (find-user-login-record user)))
    (unless entry (error "No user login ~A" user))
    (let ((data (user-other-stuff entry)))
      (loop for rest-of-data on data 
            when (eq datum (first rest-of-data)) 
            do 
            (return (values (if value? (second rest-of-data) t) t))
            finally (values (return nil) nil)
            ))))
            


(defun user-expired? (expiration)
  (and expiration (not (eql expiration :never))
       (multiple-value-bind (second minute hour date month year)
           (get-decoded-time)
         (declare (ignore second minute hour))
         (let ((expire-year (third expiration))
               (expire-month (first expiration))
               (expire-date (second expiration)))
           (or (< expire-year year)
               (and (= expire-year year) 
                    (< expire-month month))
               (and (= expire-year year) 
                    (= expire-month month) 
                    (< expire-date date)))))))

(defun cl-user::password-file-sanity-check ()
  (if (null *xyzzy*)
      (error "*XYZZY* didn't get defined correctly; Something's wrong!")
    (let ((uidtbl (make-hash-table :test 'equal))
	  (pkgtbl (make-hash-table :test 'equal)))
      (loop 
       for (uids pkg pwd real-name email expires status) in *xyzzy*
       for k fixnum from 1
       do 
       (if email 
           (if user::*allow-anonymous-logins* 
               (unless (and pkg real-name email status)
                 (cformatt "*** real account ~a is missing something!" pkg))
             (unless (and pkg pwd real-name email status)
               (cformatt "*** real account ~a is missing something!" pkg))
             )
         (unless (and pkg status)
           (cformatt "*** pseudo account ~A is missing something!" pkg)
           ))
       (unless (or (numberp expires) (eq :never expires) (null expires))
         (cformatt "*** ~a has an invalid expiration: ~a" pkg expires))
       (loop for uid in uids 
             do (if (gethash uid uidtbl)
                    (error "~a appears twice in the uid list!" uid)
                  (setf (gethash uid uidtbl) t)))
       (if (gethash pkg pkgtbl)
           (error "~a appears twice in the packages list!" pkg)
         (setf (gethash pkg pkgtbl) t))
       finally 
       (cformatt "~a entries examined; okay!" k)
       ))))

(defun user-status-from-login (&optional (login *username*))
  "Returns whether user has :guru, :standard, or :demo status"
  (user-status (find-login-record (string login))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun preliminary-lines-from-sample-drowssap-file ()
  (let ((lines (file-to-string-list (sample-drowssap-file))))
    (let ((last-preliminary-line-index 
           (position-if 
            (lambda (x) 
              (and 
               (> (length x) 8) 
               (string-equal (subseq x 0 8) ";;; XXXX")))
            lines
            )))
      (unless last-preliminary-line-index 
        (error 
         (one-string-nl
          "Sample accounts file doesn't have ';;; XXXX' terminator line!"
          "Please notify the system administrators!"
          )))
      (subseq lines 0 (1+ last-preliminary-line-index))
      )))

(defun rewrite-drowssap-file (outfile new-account-info preliminary-lines)
  (let ((*package* (find-package :weblistener))
        (*print-length* nil)
        (*print-level* nil))
    (with-open-file (p outfile :direction :output :if-exists :supersede)
      (loop for line in preliminary-lines do 
            (format p "~A~%" line))
      (terpri p)
      (pprint `(setq *xyzzy* ',new-account-info) p)
      (terpri p)
      (pprint '(eval *report-accounts-stats-form*) p)
      )))

(defun change-password (old-password new-password &key (user wb::*username*))

  (when cl-user::*allow-anonymous-logins* 
    (error 
     (one-string-nl
      "You are not allowed to change passwords on a demo server."
      "You must edit the accounts file directly or if you don't know"
      "how to do that, contact a system administrator for help."
      )))
  
  ;; verify the arguments and canonicalize 
  (unless (and (stringp old-password) (stringp new-password))
    (error "Passwords provided must be strings!"))
  (unless (or (symbolp user) (stringp user))
    (error "User must be a string or a symbol!"))
  (setq user (keywordize user))
  
  ;; make sure operation is legal 
  (unless (symbol= user wb::*username*) 
    (unless (weblistener-guru-p)
      (error "You cannot change someone else's password!")))

  
  (#+:allegro 
   mp:with-process-lock
   #+allegro
   (*password-file-lock*)
   #-:allegro 
   progn
   (load (drowssap-file))
   )

  (let* ((drowssap-file (drowssap-file))
         (account-info-write-date-1 (file-write-date drowssap-file))
         (file-changed-by-someone-else? nil)
         (rename-successful? nil)
         (error-condition nil)
         (old-drowssap-file (old-drowssap-file)))
  
    ;; change the password in a copy until we are sure everything is a go, 
    ;; and only then modify the in memory data structure 
    (let ((xyzzy-copy (copy-tree *xyzzy*))
          (real-account-info (find-user-login-record user)))
      (unless real-account-info (error "~S does not have an account!" user))
      (cond
       (*encrypted-passwords* 
        (unless (equal (user-drowssap real-account-info) 
                       (biobike-encrypted-password old-password))
          (error "Old password is not correct!")))
       (t 
        (unless (string= (user-drowssap real-account-info) old-password)
          (error "Old plaintext password is not correct!"))))
      (unless (every (lambda (x) (or (alphanumericp x) (char= #\* x))) 
                     new-password)
        (error "Your password must only consist of numbers and letters!")) 
      (let ((*xyzzy* xyzzy-copy))
        (let ((copied-account-info (find-user-login-record user)))
          
          ;; change the password in the copy 
          (when *encrypted-passwords* 
            (setq new-password (biobike-encrypted-password new-password)))
          (set-user-drowssap copied-account-info new-password)
          ;; write out a new temporary password file, then rename it to be
          ;; the real one.  Make sure we don't get caught in an error within 
          ;; the critical section.  
          (let ((account-file-header-lines
                 (preliminary-lines-from-sample-drowssap-file))
                (temp-drowssap-file 
                 (merge-pathnames "temp-drowssap.lisp" drowssap-file)))
            (formatt "Writing out new accounts file...~%")
            (rewrite-drowssap-file 
             temp-drowssap-file *xyzzy* account-file-header-lines)
            (block exit
              (#+:allegro mp:with-process-lock #+allegro (*password-file-lock*)
               #-:allegro 
               progn
               (let ((account-info-write-date-2
                      (file-write-date drowssap-file)))
                 (unless (= account-info-write-date-2 account-info-write-date-1)
                   (setq file-changed-by-someone-else? t)
                   (return-from exit nil)
                   ))
               (handler-case 
                   (progn 
                     (formatt "Renaming new file to real accounts file...~%")
                     (rename-file drowssap-file old-drowssap-file)
                     (rename-file temp-drowssap-file drowssap-file)
                     ;; we have a new password file!  Modify the
                     ;; real in memory data structure
                     (set-user-drowssap real-account-info new-password)
                     (setq rename-successful? t)
                     )
                 (error (c) (setq error-condition c))
                 )))
            (cond
	      (rename-successful? nil)
	      (file-changed-by-someone-else? 
	       (error 
		(one-string-nl
		 "Someone changed the account info file while you were"
		 "changing your password!  Please re-execute the command."
		 "Your password has NOT been changed!"
		 )))
	      (t 
	       (error
		(one-string-nl
		 "Could not replace existing account file with modified"
		 "password information.  Actual error: "
		 "~S"
		 "Please notify the system administrators."
		 "Your password has NOT been changed."
		 "If you re-execute the command it may work."
		 )
		error-condition
		)))))))

    (formatt "Password change successful!")))


(defun encrypt-password-file 
       (&key (output-file (drowssap-file)) (force? nil) (only-encrypt-new? nil))
  (unless (or *encrypted-passwords* force?)
    (error 
     (one-string-nl
      "Configuration does not specify encrypted passwords!"
      "Use the :force? keyword if you really want to do this."
      )))
  ;; This stores all the password data into *xyzzy* 
  (load (drowssap-file))
  (let ((temp-xyzzy (copy-tree *xyzzy*)))
    (loop for entry in temp-xyzzy do
          (let ((plaintext (user-drowssap entry)))
            (unless (or (stringp plaintext) only-encrypt-new?)
              (error "Plaintext password for user ~A is not a string!" 
                     (user-login entry)))
            ;; only encrypt anything that isn't already encrypted 
            (when (stringp plaintext)
              (set-user-drowssap entry (biobike-encrypted-password plaintext))
              )))
    (formatt 
     "Creating or overwriting ~A with encrypted password information.~%"
     output-file
     )
    (rewrite-drowssap-file 
     output-file 
     temp-xyzzy
     (preliminary-lines-from-sample-drowssap-file)
     )
    (formatt "Encrypted password file ~A created!" output-file)
    ))

(defun biobike-encrypted-password (plaintext)
  (coerce (excl::blowfish-encrypt plaintext :key plaintext) 'list))
     
(defun encrypted-check ()
  (loop for entry in *xyzzy* do
        (let ((password (user-drowssap entry)))
          (if *encrypted-passwords*
              (unless (listp password)
                (error 
                 (one-string
                  "Internal system error!  Passwords are supposed to be "
                  "encrypted but they appear to be plaintext!"
                  )))
            (when (and password (not (stringp password)))
              (error 
               (one-string
                "Internal system error!  Passwords are supposed to be "
                "plaintext but they appear to be encrypted!"
                )))))))
            
(defun add-new-account
       (&key (aliases nil) (login nil) (password nil) (fullname nil)
             (email nil) (expiration nil) (status :standard) (other nil))
  (block exit
    (setq login (keywordize login))
    (let ((new-account-record 
           (list* 
            aliases login password fullname email expiration status other
            ))
          (header-goo (preliminary-lines-from-sample-drowssap-file)))
      (with-password-file-read-and-locked 
        (when (find-user-login-record login) (return-from exit nil))
        (setq *xyzzy* (append *xyzzy* (list new-account-record)))
        (rewrite-drowssap-file 
         (drowssap-file)
         *xyzzy* 
         header-goo
         ))
      new-account-record
      )))
                  
(defun add-new-pseudo-account (username &key (email nil))
  (if email 
      (add-new-account :login username :email email)
    (add-new-account :login username)
    ))

;; (POSSIBLE-MATCHES LOGIN-NAME PASSWORD FULL-NAME
;;   EMAIL EXPIRATION STATUS &REST OTHER-STUFF)

(defun account-exists? (s &key (by :login))
  (with-password-file-read 
    (ecase by
      (:email 
       (find s wb::*xyzzy* :key 'user-email 
             :test (lambda (x y) (and y (string-equal x y)))
             ))
      (:login
       (or 
        (find s wb::*xyzzy* :key 'user-login
              :test (lambda (x y) (and y (string-equal x (string y)))))
        (find s wb::*xyzzy* :key 'user-matches 
              :test (lambda (x y) (member x y :test 'string-equal))
              ))))))
    
(defun modify-existing-account 
       (login &key (fullname nil) (email nil) (affiliation nil))
  (let ((account-record (account-exists? (string login) :by :login)))
    (unless account-record 
      (error "Account ~A does not exist!" login))
    (let ((new-record 
           (list* 
            (user-matches account-record)
            (user-login account-record)
            (user-drowssap account-record)
            (or fullname (user-fullname account-record))
            (or email (user-email account-record))
            (user-expire account-record)
            (user-status account-record)
            (if affiliation 
                (let* ((x (user-other-stuff account-record))
                       (pos (position :affiliation x)))
                  (if pos 
                      (progn (setf (nth (1+ pos) x) affiliation) x)
                    (append x (list :affiliation affiliation))
                    ))
              (user-other-stuff account-record)
              ))))
      (with-password-file-read-and-locked 
        (setq 
         *xyzzy* 
         (delete (user-login account-record) *xyzzy* :key 'user-login))
        (setq *xyzzy* (append *xyzzy* (list new-record)))
        (let ((header-goo (preliminary-lines-from-sample-drowssap-file)))
          (rewrite-drowssap-file 
           (drowssap-file)
           *xyzzy* 
           header-goo
           ))))))
      
(defun change-email (email &key (login *username*))
  (unless (eq login *username*)
    (unless (weblistener-guru-p *username*) (error "You cannot do this!")))
  (modify-existing-account login :email email)
  )

(defun change-fullname (fullname &key (login *username*))
  (unless (eq login *username*)
    (unless (weblistener-guru-p *username*) (error "You cannot do this!")))
  (modify-existing-account login :fullname fullname)
  )

(defun change-affiliation (affiliation &key (login *username*))
  (unless (eq login *username*)
    (unless (weblistener-guru-p *username*) (error "You cannot do this!")))
  (modify-existing-account login :affiliation affiliation)
  )