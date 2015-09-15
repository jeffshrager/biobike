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


(defvar *my-functions-menu-id* (new-unique-id :menu-id))
(defvar *my-variables-menu-id* (new-unique-id :menu-id))
(defvar *favorites-menu-id* (new-unique-id :menu-id))

(defparameter *system-nixed-favorites* 
  '(if and or bbl:if-true bbl:if-false not))

(defparameter *favorites-threshold* 5)
(defparameter *favorites-write-every* 1)
(defparameter *favorites-menu-entries-limit* 15)
(defparameter *vpl-favorites-file-name* "vpl-favorites.lisp")
     
(defparameter *original-my-functions-menu* 
  `(,*my-functions-menu-id* 
    Functions () (("> Expunge all" remove-all-my-functions))))

(defparameter *original-my-variables-menu* 
  `(,*my-variables-menu-id* 
    Variables () 
    (("> Expunge all" remove-all-my-variables))))

(defun gup (property) (get wb::*username* property))
(defun sgup (property value) (setf (get wb::*username* property) value))

(defun gsp (property) (get wb::*sessionid* property))
(defun sgsp (property value) (setf (get wb::*sessionid* property) value))

(defun initialize-my-variables-menu (&optional (force? nil))
  ;; set up a hash table for user variables if it doesn't already exist.
  ;; if it does exist, we want it to persist across refreshes and new sessions,
  ;; hence it is attached to the user id and not to the session id.  
  (let ((user-variables-hash (get wb::*username* :opcodes-to-variables-hash)))
    (unless user-variables-hash 
      (sgup :opcodes-to-variables-hash (make-hash-table :test 'eql))
      ))
  (let ((menu (gup :my-vpl-variables)))
    (when (null menu)
      (when force? (setq menu *original-my-variables-menu*)))
    (when menu 
      (setq menu (compile-palette-menu menu))
      (sgup :my-vpl-variables menu)
      (initialize-a-palette-menu menu palette-data-menus-color)
      )))

(defun initialize-my-functions-menu (&optional (force? nil))
  (let ((menu (gup :my-vpl-functions)))
    (when (null menu)
      (when force? (setq menu *original-my-functions-menu*)))
    (when menu 
      (setq menu (compile-palette-menu menu))
      (sgup :my-vpl-functions menu)
      (initialize-a-palette-menu menu palette-data-menus-color)
      )))

(defun augment-my-functions-menu (fname newid)
  (augment-my-menu
   fname newid :my-vpl-functions 'initialize-my-functions-menu))

(defun augment-my-variables-menu (name newid)
  (augment-my-menu 
   name newid :my-vpl-variables 'initialize-my-variables-menu))

(defun remove-item-from-my-functions-menu (fname)
  (remove-item-from-my-menu fname :my-vpl-functions))

(defun remove-item-from-my-variables-menu (name)
  (remove-item-from-my-menu name :my-vpl-variables))

(defun augment-my-menu 
       (name newid menu-property init-function 
             &key (color palette-data-menus-color))
  (let ((menu (gup menu-property)))
    (when (null menu)
      (progn
        (funcall init-function t)
        (setq menu (gup menu-property))
        ))          
    (destructuring-bind (menuid menu-name submenus entries)
        menu
      (declare (ignore submenus))
      (setq entries (delete name entries :test 'string-equal :key 'first))
      (setq entries 
            (sort 
             (copy-list
              (append entries (list (menu-item name newid))))
             'string-lessp
             :key 'first
             ))
      (let ((expunge-submenu
             (create-submenu-for-my-menu (cdr entries) menu-property)))
        (setq 
         menu
         (create-palette-menu
          menuid (string menu-name)
          (when expunge-submenu (list expunge-submenu)) 
          entries))
        (replace-a-palette-menu menu color)
        (sgup menu-property menu)
        ))))
  
(defun create-submenu-for-my-menu (entries menu-property)
  (when entries
    (create-palette-submenu 
     "Expunge" nil 
     (loop for entry in entries
           for j from 0
           collect
           (menu-item 
            (formatn "Expunge ~A" (first entry))
            (ecase menu-property 
              (:my-vpl-variables (create-expunge-variable-operator j))
              (:my-vpl-functions (create-expunge-function-operator j))
              ))))))

(defun remove-item-from-my-menu 
       (name menu-property &key (color palette-data-menus-color))
  (let ((menu (get wb::*username* menu-property)))
    (when menu
      (destructuring-bind (menuid menu-name submenus entries)
          menu
        (declare (ignore submenus))
        (setq entries (delete name entries :test 'string-equal :key 'first))
        (let ((expunge-submenu 
               (create-submenu-for-my-menu (cdr entries) menu-property)))
        (setq 
         menu
         (create-palette-menu
          menuid menu-name 
          (when expunge-submenu (list expunge-submenu)) 
          entries))
        (replace-a-palette-menu menu color)
        (sgup menu-property menu)
        )))))

(defun clear-all-items-from-my-functions-menu ()
  (clear-all-items-from-my-menu 
   :my-vpl-functions (compile-palette-menu *original-my-functions-menu*))
  )

(defun clear-all-items-from-my-variables-menu ()
  (clear-all-items-from-my-menu 
   :my-vpl-variables (compile-palette-menu *original-my-variables-menu*))
  )    

(defun clear-all-items-from-my-menu 
       (menu-property original-menu &key (color palette-data-menus-color))
  (declare (ignore original-menu color))
  (let* ((menu (gup menu-property))
         (menuid (first menu)))
    (unless menu 
      (vpl-internal-error "Clearing items from non-existent menu!"))
    (remove-a-palette-menu menuid)
    (sgup menu-property nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tool to add a define-function to the user's function palette 

;;; 1. Function must be a define-function it it does not have a template already
;;; 2. We must create a template and a template ID, and add the template ID
;;;    to the hash
;;; 3. Then we AUGMENT-MY-FUNCTIONS-MENU 

(defun add-function-to-my-functions-palette (symbol)
  (unless (get symbol :vpl-template) 
    (unless (eq 'bbi::define-function 
                (first (get symbol :procedure-definition)))
      (error 
       #.(one-string-nl
          "The symbol ~S is not defined with DEFINE-FUNCTION nor does it"
          "have a template.  One of these conditions must hold to use"
          "this function."
          )))
    (eval (df-symbol->define-template-form symbol)))
  ;; At this point we know the function has a template 
  (let ((id (symbol->template-id symbol t)))
    (ulogdbg "Adding ~S with ID ~D~%" symbol id)
    (augment-my-functions-menu symbol id)
    (shadowing-import (list symbol) wb::*username*)
    ))

(defun add-functions-to-my-functions-palette (symbols)
  (mapcar 'add-function-to-my-functions-palette symbols))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code to save and restore user favorites info

(defun initialize-favorites-palette-menu (&optional (force? nil))
  (declare (ignore force?))
  (ulogdbg "Initializing favorites menu...~%")
  (let ((favorites-hash (gup :vpl-favorites-info)))
    (vdbg "Favorites hash: ~A...~%" favorites-hash)
    (if favorites-hash 
        (progn
          (vdbg "Favorites hash exists...~%")
          (maybe-initialize-favorites-menu 
           palette-favorites-menu-color 
           ))
      (progn
        (vdbg "No favorites hash exists...~%")
        (load-user-favorites-info)
        (maybe-initialize-favorites-menu 
         palette-favorites-menu-color 
         )))))

(defun maybe-initialize-favorites-menu 
       (&optional (color palette-favorites-menu-color))
  (let ((favorites-menu-items (compute-favorites-menu-items)))
    (ulogdbg "Favorites list: ~S...~%" favorites-menu-items)
    (initialize-a-palette-menu 
     (create-favorites-palette-menu favorites-menu-items)
     color
     )))

(defun recompute-and-redisplay-favorites-menu 
       (&optional (color palette-favorites-menu-color))
  (let ((favorite-items (compute-favorites-menu-items)))
    (replace-a-palette-menu 
     (create-favorites-palette-menu favorite-items) color)
    ))

(defun compute-favorites-menu-items ()
  ;; First get all the favorites that exceed the threshold to be shown
  (let* ((usage-stats-hash (gup :vpl-favorites-info))
         (favorites-info
          (remove-if-not 
           (lambda (x) (>= (second x) *favorites-threshold*))
           (lmaphash (lambda (key value) (list key value)) usage-stats-hash)
           ))
         (max-count 
          (if favorites-info (reduce 'max favorites-info :key 'second) 1))
         (required (gup :vpl-specified-favorites))
         (nixed (gup :vpl-nixed-favorites))
         )
    ;; Remove any that the user doesn't want shown
    (loop for f in nixed do
          (setq favorites-info (delete f favorites-info :key 'first)))
    ;; Add all that the user insists on being shown, and add them with 
    ;; the highest incidence count 
    (loop for f in required do 
          (pushnew (list f (1+ max-count)) favorites-info :key 'first))
    (when favorites-info 
      ;; Sort the favorites by incidence count, and remove the incidence count
      ;; info
      (setq favorites-info 
            (mapcar 'first (sort favorites-info '> :key 'second)))
      ;; If necessary truncate the favorites list so only a certain maximum
      ;; number are shown. 
      (when (> (length favorites-info) *favorites-menu-entries-limit*)
        (setq favorites-info 
              (subseq favorites-info 0 *favorites-menu-entries-limit*)
              ))
      ;; Sort the favorites alphabetically
      (setq favorites-info (sort favorites-info 'string-lessp :key 'string)))
    (vdbg "Computed favorites list: ~S...~%" favorites-info)
    favorites-info 
    ))
   
(defun create-favorites-palette-menu (favorites)
  (create-palette-menu 
   *favorites-menu-id*
   "FAVORITES"
   (list 
    (create-palette-submenu
     "Manage favorites" 
     ()
     (create-menu-items
      `(("Add favorite" add-favorite-function)
        ("Delete favorite" delete-favorite-function)
        ("Clear favorites" clear-favorites-function)
        ("Forbid favorite" forbid-favorite-function)
        ))
     ))
   (loop for f in favorites nconc
         (multiple-value-bind (tid found?)
             (symbol->template-id f nil)
           (if (not found?)
               (progn
                 (ulog 
                  (one-string
                   "No template ID for favorite ~A; "
                   "not including in favorites" #\Newline)
                  f)
                 nil)
             (list (menu-item (string f) tid))
             )))))

(defun user-favorites-file-pathname ()
  (let ((user-vpl-dir (user-vpl-user-sessions-dir)))
    (merge-pathnames *vpl-favorites-file-name* user-vpl-dir)
    ))

(defun favorites-record-writeable? (record)
  (let ((function (first record))
        (count (second record)))
    (and (plusp count) 
         (or (find-symbol (symbol-name function) :bbl) 
             (find-symbol (symbol-name function) wb::*username*))
         (not (eq 'bbi::ref function))
         )))
                
(defun save-user-favorites-info 
       (&key 
        (required (gup :vpl-specified-favorites))
        (nixed (gup :vpl-nixed-favorites))
        (usage-stats-hash (gup :vpl-favorites-info)))
  (vdbg "In save-user-favorites-info")
  (let ((user-favorites-file (user-favorites-file-pathname)))
    (ensure-directories-exist user-favorites-file)
    (with-open-file (p user-favorites-file 
                       :direction :output :if-exists :supersede)
      (let ((*print-length* nil)
            (*print-level* nil)
            (*package* *vpl-package*))
        (format p "(in-package :vpl)~%")
        (pprint 
         `(setf (get ,wb::*username* :vpl-specified-favorites) ',required) p)
        (pprint
         `(setf (get ,wb::*username* :vpl-nixed-favorites) ',nixed) p)
        (pprint 
         `(setf
           (get ,wb::*username* :vpl-favorites-info) 
           (create-hash-table 
            ',(remove-if-not
               'favorites-record-writeable?
               (lmaphash 
                (lambda (key value) (list key value))
                usage-stats-hash
                ))))
         p
         )))))

(defun reset-user-favorites-info (&key (user wb::*username*))
  (let ((wb::*username* user))
    (save-user-favorites-info 
     :required nil
     :nixed nil
     :usage-stats-hash (create-hash-table nil)
     )
    (sgup :vpl-favorites-info nil)
    ))

(defun purge-usage-stats (&key (user wb::*username*))
  (let ((wb::*username* user))
    (sgup :vpl-favorites-info (make-hash-table))
    ))

(defun clear-usage-stats (&key (user wb::*username*))
  (let ((wb::*username* user))
    (sgup :vpl-favorites-info nil)
    ))

(defun delete-vpl-favorites-file (&key (user wb::*username*))
  (let ((wb::*username* user))
    (delete-file (user-favorites-file-pathname))
    ))

;; If we get an error loading the favorites file, get rid of it and start off
;; with a fresh, null favorites file.  Make sure that we don't get into
;; an infinite attempt to load the favorites file.  
;; We can get an error trying to load the favorites file because there
;; might be a symbol which is unreadable in the current system.

(defun load-user-favorites-info ()
  (block exit 
    (loop 
     while t 
     with count = 0
     do
     (handler-case 
         (let ((user-favorites-file (user-favorites-file-pathname)))
           (ensure-directories-exist user-favorites-file)
           (if (probe-file user-favorites-file) 
               (progn
                 (ulog "Favorites file exists, loading...~%")
                 (load user-favorites-file))
             (progn
               (ulog "Creating null favorites file...~%")
               (reset-user-favorites-info)
               (load user-favorites-file)
               ))
           (sgup :vpl-favorites-write-every-count 0)
           (return-from exit nil)
           )
       (error
        (c)
        (if (plusp count) 
            (vpl-internal-error 
             (formatn
              (one-string-nl
              "Recursive error trying to load favorites file!"
              "User: ~A, session: ~A"
              "Please report this to the system administrators!"
              "Your user ID is unusable until this is rectified!")
              wb::*username* wb::*sessionid*
              ))
          (progn
            (incf count)
            (usyslog "Problem loading favorites file.  Actual error: ~A~%" c)
            (ulog "Deleting favorites file...~%")
            (delete-vpl-favorites-file)
            )))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun augment-favorites-info (function-clicked-on)
  ;; don't do anything if user forbid this function from becoming a favorite
  (unless (member function-clicked-on (get wb::*username* :vpl-nixed-favorites))
    (vdbg "Augmenting usage stat for ~S...~%" function-clicked-on)
    (let ((hash (gup :vpl-favorites-info))
          (write-every-count (gup :vpl-favorites-write-every-count)))
      (incf write-every-count)
      (let ((current-use-count (incf (gethash function-clicked-on hash 0))))
        (when (>= write-every-count *favorites-write-every*) 
          (save-user-favorites-info)
          (setq write-every-count 0)
          )
        (when (= current-use-count *favorites-threshold*)
          (recompute-and-redisplay-favorites-menu)
          (save-user-favorites-info)
          (show-status 
           (formatn 
            (one-string
             "~A added to favorites because you've used it "
             "at least ~D times.")
            function-clicked-on *favorites-threshold*
            ))))
      (sgup :vpl-favorites-write-every-count write-every-count)
      )))
    
(defun clear-favorites-data ()
  (reset-user-favorites-info)
  (sgup :vpl-favorites-info (make-hash-table))
  (sgup :vpl-specified-favorites nil)
  (sgup :vpl-nixed-favorites nil)
  (sgup :vpl-favorites-write-every-count 0)
  (show-status "Favorites list cleared.")
  (recompute-and-redisplay-favorites-menu)
  )

(defun add-user-supplied-favorite (function-name)
  (let ((fname (string-trim *whitespace* function-name)))
    (if (plusp (length fname))
        (add-favorites-item (string-upcase function-name))
      (show-status "Nothing entered.")
      )))

(defun add-favorites-item (function-name)
  (block exit 
    (let ((bbl-symbol (find-symbol function-name :bbl))
          (specified (gup :vpl-specified-favorites))
          (nixed (gup :vpl-nixed-favorites)))
      (unless (and bbl-symbol (get bbl-symbol :vpl-template))
        (user-error-message 
         (formatn
          (one-string-nl
           "The symbol name you typed, ~A, is not a known VPL operator."
           "You can only add as a favorite a name that is found on one of the"
           "green palette menus."
           )
          function-name 
          ))
        (return-from exit nil))
      (unless (gethash bbl-symbol *template-symbol->tid*)
        (vpl-internal-error 
         (one-string-nl
          "The symbol ~A has a :vpl-template property but has no"
          "entry in the template->tid hash.  Please report this"
          "to the system administrators."
          )
         function-name
         ))
      (when (>= (length specified) *favorites-menu-entries-limit*)
        (user-error-message 
         (formatn
          (one-string-nl
           "You already have ~D favorites!"
           "Currently you are limited to no more than ~D."
           "In order to add ~A as a new favorite, you must remove"
           "one of your existing ones."
           )
          *favorites-menu-entries-limit* 
          *favorites-menu-entries-limit*
          function-name
          ))
        (return-from exit nil)
        )
      (pushnew bbl-symbol (get wb::*username* :vpl-specified-favorites))
      (setf (get wb::*username* :vpl-nixed-favorites) (remove bbl-symbol nixed))
      (recompute-and-redisplay-favorites-menu)
      (save-user-favorites-info)
      (show-status (formatn "~A added to favorites" function-name))
      )))

(defun delete-user-supplied-favorite (function-name)
  (let ((fname (string-trim *whitespace* function-name)))
    (if (plusp (length fname))
        (delete-favorites-item (string-upcase function-name))
      (show-status "Nothing entered.")
      )))

(defun delete-favorites-item (function-name)
  (block exit 
    (let ((bbl-symbol (find-symbol function-name :bbl))
          (specified (gup :vpl-specified-favorites))
          (hash (gup :vpl-favorites-info))
          (zapped? nil))
      (when (member bbl-symbol specified)
        (setf (get wb::*username* :vpl-specified-favorites) 
              (remove bbl-symbol specified))
        (recompute-and-redisplay-favorites-menu)
        (save-user-favorites-info)
        (show-status (formatn "~A deleted from Favorites" bbl-symbol))
        (setq zapped? t)
        )
      (when (gethash bbl-symbol hash)
        (remhash bbl-symbol hash)
        (recompute-and-redisplay-favorites-menu)
        (save-user-favorites-info)
        (show-status (formatn "~A deleted from favorites" bbl-symbol))
        (setq zapped? t)
        )
      (when zapped? (return-from exit nil))
      (user-error-message 
       (formatn
        (one-string-nl
         "The operator ~A is not currently one of your favorites."
         "Your favorites menu has not been modified.")
        function-name
        )))))

(defun forbid-user-supplied-favorite (function-name)
  (let ((fname (string-trim *whitespace* function-name)))
    (if (plusp (length fname))
        (forbid-favorites-item (string-upcase function-name))
      (show-status "Nothing entered.")
      )))

(defun forbid-favorites-item (function-name)
  (block exit 
    (let ((bbl-symbol (find-symbol function-name :bbl))
          (specified (gup :vpl-specified-favorites))
          (hash (gup :vpl-favorites-info)))
      (when (member bbl-symbol specified)
        (setf (get wb::*username* :vpl-specified-favorites) 
              (remove bbl-symbol specified))
        (recompute-and-redisplay-favorites-menu)
        (save-user-favorites-info)
        )
      (when (gethash bbl-symbol hash)
        (remhash bbl-symbol hash)
        (save-user-favorites-info)
        )
      (unless (and bbl-symbol (get bbl-symbol :vpl-template))
        (user-error-message 
         (formatn
          (one-string-nl
           "The symbol name you typed, ~A, is not a known VPL operator."
           "You can only forbid a name that is found on one of the"
           "green palette menus."
           )
          function-name 
          ))
        (return-from exit nil)
        )
      (pushnew bbl-symbol (get wb::*username* :vpl-nixed-favorites))
      (recompute-and-redisplay-favorites-menu)
      (save-user-favorites-info)
      (show-status (formatn "~A prohibited from favorites" function-name))
      )))
       
#||

Need a function for each organism.  Set up when organism frames
are created in bioseed goo.

Initial state:

Add all Viruses
Add latest Viruses
Add all Bacteria
Add latest Bacteria
...
Add All Latest (2543 Orgs.)
Add All (4577 Orgs.)


Possible next state 1 (selecting 'Add latest Viruses'):

Latest Viruses -> submenu
Add all Viruses
Remove latest Viruses
Add all Bacteria
Add latest Bacteria
...
Add All Latest (2543 Orgs.)
Add All (4577 Orgs.)


Possible next state 2 (selecting 'Add All Viruses'):

All Viruses -> submenu
Remove Viruses
Add all Bacteria
Add latest Bacteria
...
Add All Latest (2543 Orgs.)
Add All (4577 Orgs.)



Possible next state 3 (selecting 'Add All'):

All Viruses -> submenu
All Bacteria -> submenu
...
Remove all Viruses
Remove all Bacteria
...
Remove All


Possible next state 4 (Selecting 'Add All Latest'):

All Latest Viruses -> submenu
All Latest Bacteria -> submenu
...
Remove Latest Viruses
Remove Latest Bacteria
...
Remove All
Add All Viruses
Add All Bacteria
...
Add All



Possible Components and ordering:
  Latest Submenus set
  All Submenus set
  Remove Latest set
  Remove Complete set
  Remove Every Organism Submenu option
  Add Latest set
  Add Complete set
  Add All Latest option
  Add All Complete option
  Add Every Organism Submenu option

State:

  ((Component1 << nil, (:complete), (:latest) or (:complete :latest) >> ) 
   (Component2 << nil, (:complete), (:latest) or (:complete :latest) >> )
   ...)

Initial state is NIL.  Create state for each sessionid when
menu is initialized and store on plist.  If state exists use it.

To display Latest Submenus set:
  For each component, display submenu iff component state contains :latest

To display All Submenus set:
  For each component, display submenu iff component state contains :complete

To display Remove Latest set:
  For each component, display option iff component state contains :latest

To display Remove Complete set:
  For each component, display option iff component state contains :complete

To display Remove All Organism Submenus option:
  Display option unless every state is nil

To display Add Latest set:
  For each component, display option unless component state contains :latest

To display Add Complete set:
  For each component, display option unless component state contains :complete

To display Add All Latest option:
  Display unless every component's state containa :latest

To display Add All Complete option:
  Display unless every component's state contains :complete 

To display Add All Organism Submenus option:
  Display unless every component's state contains both :complete and :latest

Options for each component should actually be together, with
the Remove All Organisms Submenus, Add All Latest,  Add All  Complete and
Add Every Organism Submenu at the bottom.

State changes when options are clicked:

  Remove Latest component:
    Remove :latest from component's state.

  Remove Complete component
    Remove :complete from component's state

  Remove Every Organism Submenu 
    Change every component state to NIL

  Add Latest component:
    Pushnew :latest onto component's state

  Add Complete component:
    Pushnew :complete onto component's state

  Add All Latest 
    Pushnew :latest onto every component's state

  Add All Complete
    Pushnew :complete onto every component's state

  Add Every Organism Submenu
    Pushnew both :latest and :complete onto every component's state


Need function to create menu from state.

Every state change operator:
  Gets state
  Changes state appropriately
  Stores state
  Computes new menu from stored state
  Calls routine to replace menu on client


||#