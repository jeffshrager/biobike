;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)


;;; Instead of using this we substitute in an entirely new menu
;;; called ORGANISMS, so the tree is one level less deep.
(defmethod create-organism-submenus ((d (eql :seed-organisms)))
  nil
  )

(defvar *seed-organisms-menu-id* (new-unique-id :menu-id))
(defvar *seed-subsystems-menu-id* (new-unique-id :menu-id))
(defvar *ncbi-taxonomy-ids-initialized?* nil)
(defparameter *seed-organisms-menu-color* "blue")

(defparameter *ncbi-taxonomy-menu-toplevel-nodes* 
  '(("Viruses") ("cellular-organisms" "Bacteria")))

(defparameter *ncbi-toplevel->seed-real-domain*
  '(("Viruses" "Bacteriophage") ("Bacteria" "Eubacteria")))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun seed-organism-operator (prefix category)
    (intern (s+ prefix (substitute #\- #\Space (string-upcase category))) :vpl))

  (defun seed-organism-operator-from-category (category)
    (seed-organism-operator "ADD-" category))

  )

;;; This gets called from palette-menus.lisp, from initialize-palette-menus
;;; when the user's VPL window is first created or refreshed.
(defmethod initialize-instance-menus 
           ((app t) (orgdesc (eql :seed-organisms)) &optional (force? nil))
  (when (or force? (not *ncbi-taxonomy-ids-initialized?*))
    (cformatt "Assigning IDs to NCBI taxonomy nodes")
    (assign-ids-to-ncbi-taxonomy)
    (setq *ncbi-taxonomy-ids-initialized?* t))
  ;; set up menu state on a per-session basis if it doesn't exist.
  ;; make it persist across VPL refreshes, but not across forced refreshes.
  (unless (and (not force?) (gsp :seed-org-menu-state))
    (sgsp :seed-org-menu-state (create-initial-seed-organisms-menu-state)))
  ;; initialize user's new or refreshed VPL window's ORGANISM menu 
  ;; using current menu state.
  (recreate-seed-organism-menu-and-send-to-client (gsp :seed-org-menu-state))
  (when bio::*enable-subsystem-menus* (create-initial-subsystems-menu))
  )

(defun create-initial-seed-organisms-menu-state ()
  (loop for data in *ncbi-taxonomy-menu-toplevel-nodes* 
        collect (list (lastelem data) :disabled)))

(defun seed-organisms-count (rcat)
  (let ((ncbi-node (frames::frame-fnamed rcat nil 'bio::ncbi-taxonomy-node)))
    (unless ncbi-node (error "No ncbi taxonomy node named ~A !" rcat))
    (labels ((leaf-count (node) 
               (+ (length (#^leaves node)) 
                  (loop for child in (#^children node)
                        sum
                        (leaf-count child)
                        ))))
      (leaf-count ncbi-node)
      )))

(defun seed-organisms-catlist (rcat)
  (let ((ncbi-node (frames::frame-fnamed rcat nil 'bio::ncbi-taxonomy-node))
        (orgs nil))
    (unless ncbi-node (error "No ncbi taxonomy node named ~A !" rcat))
    (labels ((leaf-count (node) 
               (loop for org in (#^leaves node) do (push org orgs))
               (+ (length (#^leaves node)) 
                  (loop for child in (#^children node)
                        sum
                        (leaf-count child)
                        ))))
      (leaf-count ncbi-node)
      orgs
      )))

(defun add-all-label-and-operator (category)
  (list 
   (formatn 
    "~A (~D entries)"
    category 
    (seed-organisms-count category))
   (seed-organism-operator-from-category category)
   ))

(defun set-category-state (category-state new-state)
  (setf (second category-state) new-state))

(defmacro change-seed-org-menu-category-and-replace 
          ((category category-state-var) &body body)
  `(let ((,category-state-var 
          (find 
           ,category (gsp :seed-org-menu-state) :test 'string-equal :key 'first
           )))
     ,@body
     (recreate-seed-organism-menu-and-send-to-client (gsp :seed-org-menu-state))
     ))

(defun instantiate-seed-category-operators ()
  (funcall (compile nil `(lambda () ,(create-seed-category-operators)))))

(defun create-seed-category-operators ()
  `(progn
     ,@(mapcar
        (lambda (category) 
          (let ((add-operator 
                 (seed-organism-operator-from-category category)
                 ))
            `(defun-opcode ,add-operator (sid)
               (declare (ignore sid))
               (show-status "Creating menu, please wait...")
               (change-seed-org-menu-category-and-replace (,category data)
                 (set-category-state data :enabled))
               (show-status "Ok. Retry Organisms menu.")
               (not-modified!)
               )))
        (mapcar 'lastelem *ncbi-taxonomy-menu-toplevel-nodes*)
        )))

(defun enable-every-category-state (state)
  (loop for component-state in state 
        do
        (set-category-state component-state :enabled)
        ))

(defun disable-every-category-state (state)
  (loop for component-state in state 
        do
        (set-category-state component-state :disabled)
        ))

(defun-opcode add-every-organism-submenu-operator (sid)
  (declare (ignore sid))
  (let ((state (gsp :seed-org-menu-state)))
    (show-status "Creating menus, please wait...")
    (enable-every-category-state state)
    (recreate-seed-organism-menu-and-send-to-client state)
    (show-status "Ok. Retry Organisms menu.")
    ))

(defun-opcode remove-every-organism-submenu-operator (sid)
  (declare (ignore sid))
  (let ((state (get wb::*sessionid* :seed-org-menu-state)))
    (disable-every-category-state state)
    (recreate-seed-organism-menu-and-send-to-client state)
    ))

(defun recreate-seed-organism-menu-and-send-to-client (state)
  (replace-a-palette-menu
   (recreate-seed-organism-menu state) *seed-organisms-menu-color*
   ))

(defun recreate-seed-organism-menu (state)
  (let ((menu-id *seed-organisms-menu-id*))
    (create-palette-menu 
     menu-id
     "ORGANISMS"
     (cons
      (create-misc-seed-organism-submenu)
      ;; submenus to display organisms from each category 
      (create-seed-organism-menu-submenus-from-state state))
     ;; menu items 
     (remove-if 
      'null 
      (append 
       ;; menu items for individual categories
       (create-seed-organism-category-items-from-state state)
       ;; menu items for overall control 
       (create-seed-organism-general-items-from-state state)
       )))))

(defun create-misc-seed-organism-submenu ()
  (let ((misc-orgs (maybe-compute-misc-orgs)))
    (create-palette-submenu 
     "Miscellaneous"
     nil
     (menu-items-for-seed-organisms-list misc-orgs)
     )))

(defun maybe-compute-misc-orgs ()
  (let ((leaves (leaves-of-ncbi-node bio::*ncbi-taxonomy-root-frame*)))
    (set-difference bio::*available-organisms* leaves)
    ))

;;; Create the appropriate submenus based on the session's 
;;; organism's menu state.
(defun create-seed-organism-menu-submenus-from-state (state)
  (remove-if 
   'null 
   (mapcan
    (lambda (category-data) 
      (let* ((category-name (first category-data))
             (category-state (second category-data))
             (submenu-label category-name))
        (when (eq category-state :enabled) 
          (remove-if
           'null
           (list
            (create-seed-organism-category-submenu
             category-name (s+ submenu-label "" " (alpha)") :alpha)
            (create-seed-organism-category-submenu
             category-name (s+ submenu-label " (phylo)") :phylo)
            (when (string-equal category-name "viruses")
              (create-seed-organism-category-submenu
               category-name (s+ submenu-label " (host)") :host
               )))))))
    state
    )))

(defun create-seed-organism-category-items-from-state (state)
  (mapcan
   (lambda (category-data) 
     (let* ((category-name (first category-data))
            (category-state (second category-data)))
       (when (eq category-state :disabled)
         (list (add-all-label-and-operator category-name))
         )))
   state
   ))
     
;;; Create appropriate 'global' menu items
(defun create-seed-organism-general-items-from-state (state)
  (flet ((every-category-is (status)
           (every (lambda (cdata) (eq status (second cdata))) state)
           ))
    (list* 
     ;; If every submenu is to be shown, don't present option to add 
     ;; every submenu
     (unless (every-category-is :enabled)
       (menu-item 
        "Add Every Organism Submenu" 'add-every-organism-submenu-operator
        ))
     ;; If no submenus are being shown, don't present option to remove
     ;; them all.
     #+buggy
     (unless (every-category-is :disabled)
       (menu-item 
        "Remove Every Organism Submenu"
        'remove-every-organism-submenu-operator
        ))
     (create-menu-items 
      (mapcar 
       (lambda (x) 
         (menu-item 
          (symbol-name x)
          (get-id-and-add-vpl-data-to-hash x)))
       '(bbi::*all-phage*
         bbi::*all-bacteria*
         bbi::*mycobacteriophage*
         bbi::*mycobacteria*)
       ))
     )))

;;; Create a particular submenu based on the CATEGORY and the TYPE
(defun create-seed-organism-category-submenu (category-name label type)
  (let* ((ncbi-node 
          (frames::frame-fnamed category-name nil 'bio::ncbi-taxonomy-node)))
    (unless ncbi-node (error "No ncbi taxonomy node named ~A !" category-name))
    (ecase type
      (:alpha 
       (let ((real-domain 
              (cadr (assoc category-name *ncbi-toplevel->seed-real-domain* 
                           :test 'string-equal))))
         (unless real-domain 
           (error "Oops!  No real domain for category name ~A!" category-name))
         (let ((orglist 
                (remove-if-not
                 (lambda (x) (string-equal real-domain (#^real-domain x)))
                 bio::*available-organisms*
                 )))
           (create-palette-submenu 
            label
            nil
            (menu-items-for-seed-organisms-list orglist)
            ))))
      (:phylo
       (create-palette-submenu 
        label
        (make-ncbi-taxonomy-submenus ncbi-node)
        nil
        ))
      (:host
       (create-palette-submenu 
        label
        nil
        (list 
         (menu-item 
          "Cave under construction" 
          'remove-every-organism-submenu-operator
          ))))
      )))

(defun leaves-of-ncbi-node (ncbi-node)
  (append 
   (#^leaves ncbi-node) 
   (mapcan #'leaves-of-ncbi-node (#^children ncbi-node))
   ))

(defun quoted-leaves-of-ncbi-node (ncbi-node)
  (list 'quote (leaves-of-ncbi-node ncbi-node)))
           
;; sort the organisms by their official seed names 
;; and display the gid along with the possibly abbreviated name
(defun menu-items-for-seed-organisms-list (orgframe-list)
  (if user::*master-list-enabled* 
      (menu-items-for-seed-organisms-list-msf orgframe-list)
    (error "Should not get here in menu-items-for-seed-organisms-list!")
    ))

(defun menu-item-for-seed-organism (orgf)
  (let ((abbrev (limited-string (#^fname orgf) 35))
        (organism-id (#^organism-id orgf)))
    (list abbrev organism-id)
    ))

(defun menu-item-for-phylogenetic-branch (node)
  (let ((id (#^orgmenu-id node))
        (abbrev (limited-string (s+ "All " (#^ncbi-taxonomy-string node)) 35)))
    (list abbrev id)
    ))

(defun menu-items-for-seed-organisms-list-msf (orgframe-list)
  (mapcar 
   'menu-item-for-seed-organism
   (sort (copy-list orgframe-list) 'string-lessp :key (lambda (x) (#^fname x)))
   ))

;;; This gets called when a user selects an organism.
;;; If the organism is already loaded, nothing much happens.
;;; If not, the organism data is downloaded from the SEED and loaded
;;; into frames.
(defun seed-organism-menu-select (seed-organism-frame)
  (block exit
    (when (#^organism-loaded? seed-organism-frame)
      (handle-data-insert-command seed-organism-frame)
      (show-status (formatn "~A already loaded!" (#^fname seed-organism-frame)))
      (return-from exit nil)
      )
    (show-status "Loading...")
    (let* ((problem? nil)
           (seed-id (#^seed-id seed-organism-frame))
           (output 
            (with-output-to-string (p) 
              (let ((*standard-output* p)
                    (*error-output* p))
                (handler-case 
                    (bio::load-seed-organism seed-id :verbose? t)
                  (error 
                   (c)
                   (setq problem? t)
                   (formatt "Problem loading ~A !  Actual error: ~A" seed-id c)
                   ))))))
      (when (and output (not (every 'whitespacep output)))
        (create-and-use-unique-file 
         (user-temp-vpl-dir)
         (lambda (file p)
           (declare (ignore file))
           (format p "~A" output))
         (lambda (file) 
           (show-vpl-popup-URL-window
            (wb::publish-path-for-file-in-viewable-subdir file)
            :relative-p 0
            :width "400px" :height "400px"
            ))
         :name (s+ "load-" (#^fname seed-organism-frame))
         :type "txt"
         ))
      (if problem? 
          (show-status 
           (formatn "Problem loading ~A !" seed-id))
        (progn
          (handle-data-insert-command seed-organism-frame)
          (show-status (formatn "Organism ~A loaded" seed-id))
          )))))

(defun make-ncbi-taxonomy-submenus (root)
  (labels 
      ((make-leaf-menu-item (leaf) (menu-item-for-seed-organism leaf))
       (msort (items)
         (sort (copy-list items) 'string-lessp :key 'first))
       (make-child-submenu (child) 
         (let ((children (#^children child))
               (leaves (#^leaves child))
               (node-name (limited-string (#^ncbi-taxonomy-string child) 35)))
           (cond
            ((and children leaves) 
             (create-palette-submenu 
              node-name
              (msort (remove-if 'null (mapcar 'make-child-submenu children)))
              (append 
               (list (menu-item-for-phylogenetic-branch child))
               (msort (mapcar #'make-leaf-menu-item leaves))
               )))
            (children 
             (create-palette-submenu 
              node-name
              (msort (remove-if 'null (mapcar 'make-child-submenu children)))
              (list (menu-item-for-phylogenetic-branch child))
              ))
            (leaves 
             (create-palette-submenu 
              node-name
              nil
              (append 
               (list (menu-item-for-phylogenetic-branch child))
               (msort (mapcar #'make-leaf-menu-item leaves))
               )))))))
    (remove-if
     'null
     (msort (mapcar #'make-child-submenu (#^children root)))
     )))

(defun assign-ids-to-ncbi-taxonomy ()
  (let ((root 
         (frames::frame-fnamed
          bio::*ncbi-taxonomy-root-name* nil 'bio::ncbi-taxonomy-node)))
    (unless root (error "No ncbi taxonomy node named ~A !" root))
    (labels 
        ((assign-id-and-recurse (node)
           (let ((id (get-id-and-add-vpl-data-to-hash node)))
             (wlisp::setf (#^orgmenu-id node) id)
             (wlisp::setf (#^menu-function node) 'quoted-leaves-of-ncbi-node)
             (loop for child in (#^children node) 
                   do
                   ;; hack to deal with corruption of taxonomy
                   ;; tree by Jeff E (?) circa Aug 1.  JP 8/8/12
                   (when child
                     (assign-id-and-recurse child)
                     )))))
      (assign-id-and-recurse root)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-initial-subsystems-menu ()
  (when bio::*enable-subsystem-menus*
    (replace-a-palette-menu 
     (let ((menu-id *seed-subsystems-menu-id*))
       (create-palette-menu 
        menu-id
        "SUBSYSTEMS"
        nil
        ;; menu items 
        (create-menu-items
         `(("Enable subsystem selection" enable-subsystems-selection-operator))
         )))
     *seed-organisms-menu-color*
     )))

(defun create-subsystems-menu ()
  (replace-a-palette-menu 
   (let ((menu-id *seed-subsystems-menu-id*))
     (create-palette-menu 
      menu-id
      "SUBSYSTEMS"
      (list
       (create-palette-submenu
        "By Name"
        (let* ((sframes (bio::existing-subsystem-frames))
               (menu-items 
                (sort  
                 (mapcar 
                  (lambda (sframe) 
                    (list 
                     (limited-string (frames::fname sframe) 25)
                     (#^subsystem-id sframe)
                     ))
                  sframes
                  )
                 'string-lessp
                 :key 'first
                 )))
          (multiple-value-bind (alpha-labels alpha-sublistings)
              (break-listing-into-blocks 
               menu-items (ceiling (sqrt (length sframes))) :key 'first)
            (loop 
             for label in alpha-labels 
             for sframe-set in alpha-sublistings
             when sframe-set
             collect
             (create-palette-submenu 
              label 
              nil
              (create-menu-items sframe-set)
              ))))
        nil
        )
       (create-palette-submenu
        "By Category"
        ;; submenus to display organisms from each category 
        ;; (create-seed-organism-menu-submenus-from-state state)
        (let* ((subsystems-list (bio::existing-subsystem-frames))
               (subsystems-hierarchy 
                (bio::classify-subsystems subsystems-list)))
          (loop 
           for (class1-name subclass-data) in subsystems-hierarchy
           collect
           (create-palette-submenu 
            (limited-string class1-name 25)
            (loop 
             for (class2-name subsystems) in subclass-data
             collect
             (create-palette-submenu 
              (limited-string class2-name 25)
              nil
              (create-menu-items 
               (loop 
                for subsystem in subsystems
                as name = (#^fname subsystem)
                as id = (#^subsystem-id subsystem)
                collect
                (menu-item name id)
                ))))
            nil
            )))
        nil
        ))
      nil
      ))
   *seed-organisms-menu-color*
   ))

(defun handle-subsystem-id (id)
  (let ((sframe (find id (bio::existing-subsystem-frames) 
                      :key #^subsystem-id :test 'equal)))
    ;; autoload the pegs of this subsystem
    ;; autoload code is in Organisms/seed-subsystems.lisp
    (show-status "Loading subsystem features...")
    (when sframe (#^features sframe))
    (show-status "Done")
    (handle-data-insert-command 
     (or sframe (format nil "No subsystem frame with ID ~A!" id))
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                
;;; Hack to make organisms menu creation code work for VPL
;;; given corruption of database.  Run this function once
;;; the system is started up.

;;; JP 8/8/12

(defun remove-nil-children ()
  (let ((root 
         (frames::frame-fnamed
          bio::*ncbi-taxonomy-root-name* nil 'bio::ncbi-taxonomy-node)))
    (unless root (error "No ncbi taxonomy node named ~A !" root))
    (labels 
        ((remove-nil-kids (node)
           (when (member nil (#^children node))
             (wlisp::setf (#^children node) (remove nil (#^children node))))
           (loop for child in (#^children node) 
                 do
                 (remove-nil-kids child)
                 )))
      (remove-nil-kids root)

      )))

(defun find-nil-leaves ()
  (let ((root 
         (frames::frame-fnamed
          bio::*ncbi-taxonomy-root-name* nil 'bio::ncbi-taxonomy-node)))
    (unless root (error "No ncbi taxonomy node named ~A !" root))
    (labels 
        ((find-null-leaves (node)
           (when (member nil (#^leaves node))
             (print (list node (#^leaves node))))
           (loop for child in (#^children node) 
                 do
                 (when child (find-null-leaves child))
                 )))
      (find-null-leaves root)
      )))
