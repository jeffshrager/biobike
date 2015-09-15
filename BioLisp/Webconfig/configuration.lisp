;;; -*- Package: cl-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :cl-user) 

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

;;; Author:  JP Massar.

; WARNING, WARNING!  DANGER, WILL ROBINSON, DANGER!!

;;; You cannot do an Emacs Compile Buffer on this file, you must do
;;; a Compile File, because it depends on having a non-nil value for
;;; *load-truename*.

;;;; ******     ALL THE WEBLISTENER CONFIGURATION VARIABLES     *******

;;;;  *****> IF YOU ADD A CONFIGURATION VARIABLE, ADD AN EXAMPLE INTO
;;;;  *****> SAMPLE-CONFIG.LISP TOO.


(defmacro defwebcfgvar (name doc value)
  `(defcfgvar ,name ,value 
             :belongs-to :weblistener
             :parameter-type :standard
             :doc ,doc
             ))

(defmacro defwebcfgdir (name doc value)
  `(defcfgvar ,name ,value 
             :belongs-to :weblistener
             :parameter-type :directory
             :doc ,doc           
             ))

(defwebcfgvar 
 *weblistener-machine-name* 
 "Name of host machine as a full Telnet or SSH or Web address"
 (ecase (os?)
   (:unix "nostoc.stanford.edu")
   (:windows "localhost")
   ))

(defwebcfgdir 
 *instance-home-directory*
 "The /home/... if any where the instance is started, and where
  instance-specific modules, etc. will be found."
 (ecase (os?)
   (:unix "/home/biobike/")
   (:windows "C:\\jpm\\")
   ))

(defwebcfgdir
 *source-root* 
 "The topmost directory for all source code. Usually the *WEBLISTENER-DIRECTORY*
   and the *APPLICATION-DIRECTORY* will be identical to this, but conceivably
   they could be under this."
 (ecase (os?)
   (:unix "/usr/local/BioLisp/")
   (:windows "C:/Lispcode/BioLisp/")))
   
(defwebcfgvar
 *application*
 "The name of the application running under the Weblistener.
  Used to write methods on various generic functions."
 :biolingua)

(defwebcfgvar 
 *application-instance*
 "The instance of the application we are running.
  E.g., :biolingua vs :biodemo.  Used to write methods specific to an instance."
 :biolingua
 )

(defwebcfgvar 
 *application-instance-pretty-name*
 "The pretty name of the instance of the application we are running.
  E.g., 'the Original BioBIKE server' or 'the PhAnToMe BioBike/SEED server'."
 nil
 )

;;; Uses the fact that *load-truename* is bound to a pathname
;;; identifying the file being loaded (this one!) to compute
;;; the directory where the biolisp files have been installed.
;;; DIRPATH-FROM-PATH is defined by the BOOTSTRAP code.

(defwebcfgdir 
 *weblistener-directory*
 "Where the root of the Weblistener source and documentation tree is installed"
 (ecase (os?)
   ((:unix :windows) 
    ;; One level up from where we are now.
    (let* ((current-dir (dirpath-from-path *load-truename*))
           (dir-component (pathname-directory current-dir))
           (updir-component (butlast dir-component 1)))
      (let ((result
             (namestring 
              (make-pathname :directory updir-component :defaults current-dir)
              )))
        result
        )))))

(defwebcfgdir
 *application-directory*
 "The toplevel directory for the application source code.
  This should already be defined either by an initialization file
  or the application load file."
 (ecase (os?)
   (:unix "/usr/local/BioLisp/")
   (:windows "C:/Lispcode/BioLisp/")
   ))

(defwebcfgdir 
 *ppcre-directory*
 "Where the code for the latest version of PPCRE lives."
 (ecase (os?)
   (:unix "/usr/local/BioLisp/ppcre/cl-ppcre-1.2.3/")
   (:windows "C:\\Lispcode\\BioLisp\\ppcre\\cl-ppcre-1.2.3\\")
   ))

(defwebcfgdir
 *portableaserve-directory*
 "Where PortableAserve is installed. Only needed for non-Allegro."
 (ecase (os?)
   (:unix #+allegro nil #-:allegro (error "Need to have PortableAserve"))
   (:windows "C:/Lispcode/portableaserve-1.2.35/portableaserve/")
   ))

(defwebcfgdir
 *etc-directory*
 "Where data files (in data subdir) and miscellaneous other files go"
 (ecase (os?)
   (:unix "/usr/local/bioetc/")
   (:windows "C:\\Biomisc\\Bioetc\\")
   ))

(defwebcfgdir
 *password-directory*
 "The directory where the system looks for the password file"
  (ecase (os?)
    (:unix "/home/biobike/etc/")
    (:windows "C:\\Biomisc\\Bioetc\\")
    ))

(defwebcfgdir
 *tmp-directory*
 "Where temporary files can be created. (Need write permission!)"
 (ecase (os?)
   (:unix "/home/biobike/tmp/")
   (:windows "C:/tmp/")
   ))

(defwebcfgdir
 *home-directory*
 "Directory above weblistener users' home directories.
  E.g., if the 'massar' home directory is in /home/visitors/massar/ 
  then this value should be /home/visitors/"
 (ecase (os?)
   (:unix "/home/biobike/users/")
   (:windows "C:\\Biomisc\\Visitors\\")
   ))

;; NOTE FOR BioLingua: The logs directory is also referenced by the 
;; startup script file.  It is passed in as an argument to the startup
;; script .../Scripts/runbwl-intance.sh, so if you change it here you'd
;; better change whomever calls the startup script (e.g., another
;; script in the BioLingua directory)

(defwebcfgdir 
 *logs-directory*
 "Directory two levels above where log files are stored.
  E.g., if this value is /home/vistors/ then the log files for user massar
  are stored in /home/visitors/massar/session-logs/ .  This is also true
  for the system log files:  /home/visitors/system/session-logs/"
 (ecase (os?)
   (:unix "/home/biobike/users/")
   (:windows "C:\\Biomisc\\Visitors\\")
   ))

(defwebcfgdir
 *webtmp-directory*
 "A directory where files can be placed that can be accessed by
   using the standard (port 80) URL for the host machine.  You may want to 
   set up a symbolic link on Unix instead of pointing directly to somewhere
   that Apache can access, because temporary files potentially build up here."
 (ecase (os?)
   (:unix "/home/biobike/webtmp/")
   (:windows "C:/tmp/")
   ))

(defwebcfgvar
 *host-machine-apache-url*
 "The toplevel URL that gets to the standard (port 80, usually Apache) web
  server on the host machine. (For Windows we assume we are running locally,
  so the URL is a local URL, independent of the web)"
 (ecase (os?)
   (:unix "http://nostoc.stanford.edu/")
   (:windows "FILE:///C:/")
   ))

;;  Note that on NOSTOC currently, http://nostoc.stanford.edu/weblogs/
;;  resolves via APACHE to /tmp/weblogs/ which is a symbolic link to
;;  /home/visitors/"

(defwebcfgvar
 *host-machine-apache-logs-url*
 "How to get to the log files using *HOST-MACHINE-APACHE-URL*.
  The logs are two levels deep from this location, first in a subdirectory
  named for a user, then in a subdirectory called 'session-logs'.  You must 
  set up a symbolic link on Unix to make this work."
 (ecase (os?)
   (:unix 
    (concatenate 'string *host-machine-apache-url* "weblogs/"))
   (:windows 
    (concatenate 'string *host-machine-apache-url* "Biomisc/Visitors/"))
   ))

;; E.g., currently on NOSTOC, if you create a file /tmp/foo.html,
;; then you can access that file from a web browser by using the
;; URL http://nostoc.stanford.edu/foo.html

(defwebcfgvar
 *webtmp-url* 
 "The URL to use to get to the above *WEBTMP-DIRECTORY*."
 (ecase (os?)
   (:unix (concatenate 'string *host-machine-apache-url* "biobikewww/"))
   (:windows nil)
   ))

(defwebcfgvar
 *weblistener-port*
 "The HTTP port the WebListener listens on by default.
   I.e., the port that AllegroServe is started up with."
 (ecase (os?)
   (:unix 8002)
   (:windows 8000)
   ))

(defwebcfgvar
 *smtp-server* 
 "The mail server to be used to send email out the door."
 (ecase (os?)
   (:unix "smtp.stanford.edu")
   (:windows "smtp.comcast.net")
   ))

;; How long (in days) log files sit around before being deleted.

(defwebcfgvar 
 *days-until-system-logs-purged* 
 "How long in days before system script and log files are deleted."
 30)

(defwebcfgvar
 *days-until-user-logs-purged*
 "How long in days before user log files are deleted."
 90)

(defwebcfgvar 
 *enable-default-user-for-programatic-eval*
 "The name of a generic user which remote programs can use as the PACKAGE
  argument when sending form strings to be evaluated (either directly via
  HTTP or via XMLRPC).  If NIL, no generic user is created."
 :evaluser)

;; Note: The XMLRPC server is actually a 'subserver' which runs under
;; AllegroServe.

(defwebcfgvar
 *enable-xml-rpc-at-startup?*
 "Whether to start up an XMLRPC server."
 t)

;; Note: It is possible to put commands into the background (into 
;; a separate thread) to avoid timeout problems).

(defwebcfgvar 
 *default-execution-timelimit*
 "Time in seconds a single user command is allowed to execute."
 600)

(defwebcfgvar 
 *maximum-execution-timelimit*
 "Maximum timelimit a user is allowed to set his single command timelimit to."
 3600)

;; Note: If this is set the execution timelimit above should probably be
;; reduced significantly.

(defwebcfgvar
 *allow-anonymous-logins*
 "Whether to allow any login access to the WebListener, regardless
  of whether they have an account/password.  Used for Demo servers."
 nil)

(defwebcfgvar
 *encrypted-passwords*
 "Whether passwords are encrypted or not.  Encryption is one way.
  It is impossible to determine a user's password if he/she has forgotten it."
 nil)

;; According to Jeff E, if you make this a list of email addresses,
;; it will also work.  
(defwebcfgvar
 *default-support-email-address*
 "The address to send messages to system support staff."
 "biobike@googlegroups.com"
 )

(defwebcfgvar
 *readable-directories-writeable?*
 "Whether directories which must be readable should also be writeable"
 t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwebcfgvar
 *default-db-connection-methodology*
 "The default database software the server will talk to."
 :mysql)

;;; THESE SQL PARAMETERS ARE FOR THE NEW MECHANISM TO ACCESS MYSQL
;;; FOUND IN DBACCESS/MYSQL-DB-ACCESS.LISP.
;;; NOTE THAT MYSQL IS DEFINED AS THE DEFAULT DATABASE METHODOLOGY (OR NOT)
;;; ABOVE.  IF SOME OTHER DATABASE ACCESS WERE THE DEFAULT WE'D NEED ANOTHER
;;; SET OF CONFIGURATION VARIABLES FOR THAT DATABASE.

(defwebcfgvar
 *default-mysql-local-file-arg*
 "The Unix path to use when accessing MYSQL via a local socket."
 "/var/lib/mysql/mysql.sock")

(defwebcfgvar
 *default-mysql-local-user-name*
 "The login name to use to access MYSQL on the local machine"
 "biobike")

(defwebcfgvar
 *default-mysql-local-password*
 "The password to use to access MYSQL on the local machine"
 nil)

(defwebcfgvar
 *default-mysql-remote-machine*
 "The machine MYSQL is running on when not being run locally."
 "nostoc.stanford.edu")

(defwebcfgvar
 *default-mysql-remote-user-name*
 "The login name to use to access MYSQL on a remote machine."
 "root")

(defwebcfgvar
 *default-mysql-remote-password*
 "The password to use to access MYSQL on a remote machine."
 nil)

(defwebcfgvar
 *default-mysql-database-name*
 "The name of the MYSQL database to access by default."
 "cyanobacteria")

(defwebcfgvar
 *default-mysql-local-or-remote-default*
 "Whether to, by default, access MYSQL locally (:local) or remotely (:remote).
  If the value is :os, :local is assumed in Unix, :remote on Windows."
 :os)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 



;; Currently :old or :Sframes are actually supported, although
;; :new and :Nframes still exist in the code.
(defwebcfgvar 
 *frame-system-version* 
 "Which frame system, the old one or the AllegroCache version to load." 
 :old)

;; :acache or :pseudo-acache or :ask
(defwebcfgvar 
 *acache-frame-system-mode* 
 "Whether to use AllegroCache itself, or just in-memory frames."
 :ask)

(defwebcfgvar 
 *acache-file-for-require*
 "The second argument to (require :acache ...) that is currently demanded"
 #+:ALLEGRO-V8.1
 "acache-2.1.5.fasl"
 #+:ALLEGRO-V8.2
 "acache-2.1.12.fasl"
 #+:ALLEGRO-V9.0
 "acache-2.1.21.fasl" 
 #+:lispworks
 nil
 )

#||

(defwebcfgvar 
 *acache-fasl-dir* 
 "Where the .fasl for acache you want to load in lives (full directory path)."
 nil
 ;; (translate-logical-pathname "sys:code;")
 )

(defwebcfgvar
 *acache-version*
 "Current version of .fasl file (without the .fasl extension)"
 "acache-1.1.12")

(defwebcfgvar 
 *acache-connection-mode* 
 "Whether to connect in single user mode (:single) or multi-user mode (:multi)"
 :single)

||#

(defwebcfgdir
 *acache-database-dir*
 "The directory where the Allegro Cache database will be kept"
 (ecase (os?)
   (:unix (concatenate 'string  *etc-directory* "acachedb/"))
   (:windows "C:/lispcode/acdb/")
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwebcfgdir
 *ajax-directory*
 "The directory containing .fml and .ls files used in AJAX applications."
 (ecase (os?)
   (:unix (concatenate 'string *instance-home-directory* "ajax/"))
   (:windows (concatenate 'string *instance-home-directory* "ajax\\"))
   ))

(defwebcfgvar
 *instance-module-list*
 "A list of subdirs of the instance directory. Each subdir
  must contain a load.lisp file which will be loaded by the instance
  configuation method.  This is used to load software specific to
  an instance."
  nil)

(defwebcfgvar 
 *html-publish-content-type* 
 "The default for the :content-type keyword argument to aserve:publish
 and friends."
 "text/html"
 )
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Switch to http://www.phantome.org/PhageSeed/wsdl_seed.cgi eventually

(defwebcfgvar 
 *seed-wsdl-url*
 "The URL from which to obtain the SOAP wsdl interface to the SEED database"
 "http://bioseed.mcs.anl.gov/~redwards/FIG/wsdl_seed_complex.cgi"
 )

(defwebcfgvar 
 *seed-mysql-data-root* 
 "The root of the mysql data for the SEED, when accessing the SEED locally."
 "/home/fig/FIGdisk/"
 )

(defwebcfgvar 
 *seed-access-mode*
 "How to access the SEED; either using a SOAP interface or directly querying
  the SEED's mysql database." 
 :soap
 )


;;;;  *****> IF YOU ADD A CONFIGURATION VARIABLE, ADD AN EXAMPLE INTO
;;;;  *****> .../biolisp/sample-config.lisp TOO.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Functions to create all the logical pathnames for the weblistener.

(defun create-weblistener-logical-pathname-translations ()

  ;; Must have the toplevel source directory, the ppcre directory, 
  ;; a temp file directory, and an 'etc' directory (which is only used
  ;; by the protocol stuff to store published protocols)

  (create-simple-logical-pathname-host 
   "websrc" '*weblistener-directory* 
   :nil-ok? nil :directory-must-be-writeable? nil)
  (create-simple-logical-pathname-host 
   "ppcre" '*ppcre-directory* 
   :nil-ok? nil :directory-must-be-writeable? nil)
  (create-simple-logical-pathname-host 
   "tmp" '*tmp-directory* 
   :nil-ok? nil :directory-must-be-writeable? t)
  (create-simple-logical-pathname-host 
   "etc" '*etc-directory*
    :nil-ok? nil :directory-must-be-writeable? nil)

  ;; The logs directory is theoretically not needed.
  ;; A separate directory for temporary files that can be linked to is
  ;; also theoretically optional

  (create-simple-logical-pathname-host 
   "logs" '*logs-directory*
   :nil-ok? t :directory-must-be-writeable? t)
  (create-simple-logical-pathname-host 
   "home" '*home-directory* 
   :nil-ok? nil :directory-must-be-writeable? t)
  (create-simple-logical-pathname-host 
   "webtmp" '*webtmp-directory*
   :nil-ok? t :directory-must-be-writeable? t)
  (create-simple-logical-pathname-host 
   "appsrc" '*application-directory* 
   :nil-ok? nil :directory-must-be-writeable? nil)
  
  (terpri)

)

(defun configure-weblistener ()
  (create-weblistener-logical-pathname-translations)
  (pushnew :weblistener *features*)
  )

;;; THE CRITICAL STEP!!  DEFINE THE TRANSLATIONS AS THIS FILE
;;; GETS LOADED AND DO ANYTHING ELSE THAT NEEDS TO BE DONE AT
;;; CONFIGURATION TIME.

(configure-weblistener)

(display-configuration-variables :weblistener)
(display-simple-lp-translations)

