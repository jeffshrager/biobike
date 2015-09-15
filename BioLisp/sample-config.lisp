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

(format t ";; If you see this, your Weblistener configuration file~%")
(format t ";; is being loaded!!~%")

;; Unless you know what you are doing, you probably want to keep this in
;; :root-directory-mode, as it is set up to be in immediately below.
;; If you do want to change it, uncomment the line you want and
;; comment out the line that isn't commented out. 

#-(or developer-mode nostoc-8002-mode)
  (push :root-directory-mode *features*)
;; (push :nostoc-8002-mode *features*)
;; (push :developer-mode *features*)

(setq *WEBLISTENER-MACHINE-NAME* "mymachine.mysite.edu")

;; Directory where non-source subdirectories are created. (The subdirectories will
;; be created when this config file is loaded if they don't already
;; exist.)

(setq *INSTANCE-HOME-DIRECTORY*
      (or
       #+root-directory-mode "/home/biobike/"
       #+developer-mode "/tmp/biobike/"
       #+nostoc-8002-mode "/usr/local/BioLisp/"
       (error "Must specify a configuration mode.")))

;; Directory containing the source for BioLisp and the web
;; listener. In root-directory-mode and nostoc-8002-mode the source
;; lives under *instance-home-directory*. A BioBike developer may wish
;; to have the source live in their CVS workspace while the data
;; directories needed to run BioBike live elsewhere.

(setq *SOURCE-ROOT* 
  (or
   #+root-directory-mode 
   (merge-pathnames "BioLisp/" *instance-home-directory*)
   #+developer-mode 
   (make-pathname :directory (pathname-directory *load-truename*))
   #+nostoc-8002-mode 
   (merge-pathnames "BioLisp/" *instance-home-directory*)
   (error "Must specify a configuration mode.")))


;; Directory that will contain user directories. Created relative to
;; *instance-home-directory* except in nostoc-8002-mode.  
(defvar *users-directory*
  (or
   #+developer-mode "users/"
   #+root-directory-mode "users/"
   #+nostoc-8002-mode "/home/visitors/"
   (error "Must specify a configuration mode.")))

(setq *APPLICATION* :biolingua)
(setq *APPLICATION-INSTANCE* :biolingua)
(setq *APPLICATION-INSTANCE-PRETTY-NAME* "Biolingua Server")

(format t ";; APPPLICATION to be run: ~S~%" *application*)
(format t ";; APPLICATION INSTANCE set to ~S~%" *application-instance*)
(format t ";; INSTALLATION MODE set to ~S~%" (first *features*))

;;;; THE WEBLISTENER IS CONFIGURED USING THIS STUFF:

;;; Weblistener configuration variables that define directories are
;;; defined here in one of three modes.

;;; In the first two modes, these directories are all subdirectories
;;; of *INSTANCE-HOME-DIRECTORY*. The main difference is that in
;;; root-directory-mode, the *INSTANCE-HOME-DIRECTORY* is the home
;;; directory of a user named BioLingua while in nostoc-8002-mode,
;;; where the directories are basically as they exist on Nostoc for
;;; the 8002 server, *INSTANCE-HOME-DIRECTORY* is
;;; /usr/local/bio. Additionally the sub-directory containing the
;;; source code is named "BioLisp" in root-directory-mode and
;;; "biolisp" in nostoc-8002-mode and the sub-directory containing
;;; per-user files is "users" in root-directory-mode and "home" in
;;; nostoc-8002-mode.

;;; The third mode, developer-mode, separates the application data
;;; files from the source tree, using the source found relative to the
;;; config-file itself. Thus a developer can copy or symlink the file
;;; sample-config.lisp found in the source root to a file in the same
;;; directory named config.lisp and then set *INSTANCE-HOME-DIRECTORY*
;;; to whatever they want and can run the system directly from the CVS
;;; sources.

;;; See the installation instructions for more details.  


(defun datadir (relative &optional (ensure? t))
  (if ensure?
      (namestring (ensure-directories-exist 
                   (merge-pathnames relative *instance-home-directory*)))
    (namestring (merge-pathnames relative *instance-home-directory*))
    ))

(defun srcdir (relative)
  (namestring (merge-pathnames relative *source-root*)))

(setq *WEBLISTENER-DIRECTORY* *source-root*)
(setq *APPLICATION-DIRECTORY* *source-root*)
(setq *PPCRE-DIRECTORY*       (srcdir "ppcre/cl-ppcre-1.2.3/"))
(setq *PORTABLEASERVE-DIRECTORY* nil)
(setq *ETC-DIRECTORY*         (datadir "etc/"))
(setq *PASSWORD-DIRECTORY*    (datadir "etc/"))
(setq *TMP-DIRECTORY*         (datadir "tmp/"))
(setq *HOME-DIRECTORY*        (datadir *users-directory*))
(setq *LOGS-DIRECTORY*        (datadir *users-directory*))
(setq *WEBTMP-DIRECTORY*      (datadir "webtmp/"))

;;; Other weblistener configuration variables that are not directories
;;; or are probably located elsewhere than *instance-home-directory*

(setq *HOST-MACHINE-APACHE-URL* "http://www.mymachine.edu/")
(setq *HOST-MACHINE-APACHE-LOGS-URL* "weblogs/")
(setq *WEBTMP-URL*
      (concatenate 'string *HOST-MACHINE-APACHE-URL* "biobikewww/"))

(setq *WEBLISTENER-PORT* 8002)
(setq *SMTP-SERVER* "nostoc.stanford.edu")
(setq *DAYS-UNTIL-SYSTEM-LOGS-PURGED* 30)
(setq *DAYS-UNTIL-USER-LOGS-PURGED* 90)

(setq *ENABLE-DEFAULT-USER-FOR-PROGRAMATIC-EVAL* :evaluser)
(setq *ENABLE-XML-RPC-AT-STARTUP?* t)
(setq *DEFAULT-EXECUTION-TIMELIMIT* 600)
(setq *MAXIMUM-EXECUTION-TIMELIMIT* 3600)
(setq *ALLOW-ANONYMOUS-LOGINS* NIL)
(setq *ENCRYPTED-PASSWORDS* NIL)

(setq *DEFAULT-SUPPORT-EMAIL-ADDRESS* "biobike@googlegroups.com")

(setq *READABLE-DIRECTORIES-WRITEABLE?* T)

;;;; GENERIC DATABASE ACCESS INFORMATION (NOT FULLY IMPLEMENTED)

(setq *DEFAULT-DB-CONNECTION-METHODOLOGY* :mysql)
(setq *DEFAULT-MYSQL-LOCAL-FILE-ARG* "/var/lib/mysql/mysql.sock")
(setq *DEFAULT-MYSQL-LOCAL-USER-NAME* "biobike")
(setq *DEFAULT-MYSQL-LOCAL-PASSWORD* nil)
(setq *DEFAULT-MYSQL-REMOTE-MACHINE* nil)
(setq *DEFAULT-MYSQL-REMOTE-USER-NAME* "root")
(setq *DEFAULT-MYSQL-REMOTE-PASSWORD* nil)
(setq *DEFAULT-MYSQL-DATABASE-NAME* "parasites")
(setq *DEFAULT-MYSQL-LOCAL-OR-REMOTE-DEFAULT* :local)

;;;; ALLEGROCACHE DATABASE INFORMATION

(setq *FRAME-SYSTEM-VERSION* :old)
(setq *ACACHE-FRAME-SYSTEM-MODE* :ask)  
(setq *ACACHE-VERSION* "acache-0.7.0")
(setq *ACACHE-CONNECTION-MODE* :single) 
(setq *ACACHE-DATABASE-DIR*   (datadir "etc/acachedb/"))

(setq *AJAX-DIRECTORY*        (datadir "ajax/"))

(setq *INSTANCE-MODULE-LIST* nil)

(setq *HTML-PUBLISH-CONTENT-TYPE* "text/html")

(setq *seed-wsdl-url* 
      "http://bioseed.mcs.anl.gov/~redwards/FIG/wsdl_seed_complex.cgi")
;; You almost certainly need to change this if you're using
;; mysql to talk to a seed database.  
(setq *seed-mysql-data-root* "/mnt/u03/robs_data/FIGdisk/")
;; Change this to :mysql if you're using mysql to talk to a seed database.  
(setq *seed-access-mode* :soap)

(format t ";; Weblistener configuration complete.~%")
(format t ";; Now configuring BioLingua.~%")

;;;; BIOLINGUA IS CONFIGURED USING THIS STUFF

(setq *BIOTOOLS-DIRECTORY* (datadir "biotools/"))
(setq *BIOETC-DIRECTORY* (datadir "bioetc/"))
(setq *INSTANCE-BIOETC-DIRECTORY* (datadir "bioetc/"))

;; *** This will not work!  You must change this! ***
(setq *BIOLINGUA-EMAIL-ADDRESS* "biolingua@mymachine.edu")
(setq *BIOWEBSERVER* T)

;; *** This will not work!  You must change this! ***
;; See installation instructions.***
(setq *DEFAULT-BIO-DATABASE-NAME* "mydatabase")

(setq *ORGANISMS-DESCRIPTOR* :cyanobacteria)

(setq *ORGANISMS-TO-LOAD-LIST* nil)

(setq *KEEP-SEQUENCE-STREAMS-OPEN* t)

(setq *BIOTUTORIALS-DIRECTORY* (srcdir "Doc/livetutorials/"))

;; Blast seems to insist on being installed here?
(setq *BLAST-EXECUTABLE-TOPLEVEL-DIR* "/usr/bin/")
(setq *BLAST-LOOKUP-DATABASE* :SEED)
;; NIL if not using a lookup table for protein-protein blasts
(setq *CLUSTAL-EXECUTABLE-DIR* 
      (concatenate 'string *BIOTOOLS-DIRECTORY* "clustalw/clustalw1.83/"))
(setq *R-EXECUTABLE-DIRECTORY* 
      (concatenate 'string *BIOTOOLS-DIRECTORY* "R/R-2.1.1./bin/"))
(setq *PHYLIP-EXECUTABLE-DIRECTORY*
      (concatenate 'string *BIOTOOLS-DIRECTORY* "phylip/current/exe/"))
(setq *MEME-EXECUTABLE-DIRECTORY*
      (concatenate 'string *BIOTOOLS-DIRECTORY* "meme/meme.3.0.4/bin/"))
(setq *MEME-VERSION* "3.5.7")
(setq *GNUPLOT-PATH* 
      (concatenate 
       'string *biotools-directory* "gnuplot/gnuplot-4.0.0/src/gnuplot"))

(setq *DEFAULT-DOTTY-FONT* "courier")

(setq *REPL-MODE* :biolisp)

(setq *INCLUDE-BBL* t)

(setq *CODE-CREATION-MODE* :weblistener) 

(setq *VPL-VERSION* 2)

;; This should ONLY be uncommented when BioLingua is run from
;; a pre-built executable.

;; (configure-biolingua)

(format t ";; BioLingua configuration complete.~%")

(format t ";; The application configuration file has been loaded~%")
