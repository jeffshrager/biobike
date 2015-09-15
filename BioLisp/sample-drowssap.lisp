;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

(in-package :weblistener)

;;; The password file will get rewritten out each time someone changes their 
;;; password.  Therefore comments placed in the password file itself will 
;;; vanish whenever this happens.  

;;; You can place comments in the file .../sample-drowssap.lisp and these
;;; will be copied from there into the revised password file.  

;;; It is useless to put comments within the accounts list (*xyzzy*) itself, 
;;; because they will vanish as soon as someone changes a password.  
;;; Put any comments above the "XXXX *** ..." line, even those that might
;;; pertain to an individual.  

;;; FORMAT OF EACH ENTRY

;;; (ALIAS-LIST LOGIN-NAME PASSWORD FULL-NAME EMAIL EXPIRATION STATUS)


;;; XXXX *** DON'T REMOVE THIS LINE OR PUT ANY TEXT BETWEEN HERE AND *xyzzy* ***



(setq *xyzzy*

  ;; *** WARNING: 
  ;; the system find's the user's standard init file
  ;; based on the login name (e.g. in /home/visitors/<login-name>/).
  ;; And the package the user is placed in is based on the login name.
  ;; So if a user normally uses a nickname (one of the permissible aliases
  ;; that is not his login name) to log in, he might be confused!

  '(
    ;; An example account with an alias
    (("FREDF" "FLINTSTONE") FLINTSTONE "bedrock" "Fred Flintstone"
     "fredf@hanna-barbara.com" :never :standard)
    ;; An example account with GURU status
    (("ROOT") ROOT "secret" "Super User" "biolingua@nostoc.stanford.edu"
     :never :guru)
    ;; An example demo account with an expiration date
    (("BETTYR") BRUBBLE "wife" "Betty Rubble" "bettyr@foo.net" (12 31 2005) :demo)
    ;; The account entry if you want programmatic access.
    (("EVALUSER") EVALUSER "*" "Program access" "" :never :standard)
    ;; the account entry if you want access from the outside to documentation
    (("DOCUSER") DOCUSER "*" "Documentation access" "" :never :standard)
    ;; An example pseudouser account
    (NIL PSEUDO1 NIL NIL NIL NIL :STANDARD)
    ;; An example standard account with affiliation
    (("Barney" "BRubble") RUBBLE "bedrock" "Barney Rubble"
     "barney@hanna-barbara.com" :never :standard :affiliation "Bedrock biology")
    ))

(eval *report-accounts-stats-form*)

