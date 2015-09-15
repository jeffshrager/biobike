;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)

;; Create a simple menu of organisms.  

(defvar *cyano-organisms-menu-id* (new-unique-id :menu-id))

(defmethod initialize-instance-menus 
           ((app t) (orgdesc (eql :cyanobacteria)) &optional (force? nil))
  (declare (ignore force?))
  (create-cyano-organism-menu-and-send-to-client)
  )

(defun create-cyano-organism-menu-and-send-to-client ()
  (replace-a-palette-menu (recreate-cyano-organism-menu) "blue"))

(defun recreate-cyano-organism-menu ()
  (loop for orgf in (bio::available-organisms)
        do 
        (unless (#^organism-id orgf)
          (wlisp::setf 
           (#^organism-id orgf) 
           (forward-package-funcall 
            :vpl :new-unique-id :organism-id
            ))))
  (let ((menu-id *cyano-organisms-menu-id*))
    (create-palette-menu 
     menu-id
     "ORGANISMS"
     nil
     (loop for orgf in 
           (sort
            (copy-list (bio::available-organisms))
            'string-lessp :key #^fname)
           collect
           (menu-item (#^fname orgf) (#^organism-id orgf))
           ))))

(defun cyano-organism-menu-select (orgf)
  (block exit
    (when (#^organism-loaded? orgf)
      (handle-data-insert-command orgf)
      (show-status (formatn "~A already loaded!" (#^fname orgf)))
      (return-from exit nil)
      )
    (show-status "Loading...")
    (let* ((problem? nil)
           (output 
            (with-output-to-string (p) 
              (let ((*standard-output* p)
                    (*error-output* p))
                (handler-case 
                    (bio::load-organism orgf :verbose? t)
                  (error 
                   (c)
                   (setq problem? t)
                   (formatt "Problem loading ~A !  Actual error: ~A" orgf c)
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
         :name (s+ "load-" (#^fname orgf))
         :type "txt"
         ))
      (if problem? 
          (show-status 
           (formatn "Problem loading ~A !" orgf))
        (progn
          (handle-data-insert-command orgf)
          (show-status (formatn "Organism ~A loaded" orgf))
          )))))

;; Create a complicated multilevel menu of organism subsets

(defmethod create-organism-submenus ((d (eql :cyanobacteria)))
  (append 
   (list 
    (create-palette-submenu 
     "Organism subsets" 
     NIL
     (create-other-organism-options)))
   (when (bio::loaded-organisms)
     (list
      ;; Making the menus narrower... -- JP
      (create-palette-submenu
       ;; "Marine unicellular cyanobacteria" 
       "Marine unicellular cyano" 
       (create-subset-of-organism-submenus 
        cl-user::*organisms-descriptor* 
        bbi::*marine-unicellular-cyanobacteria*)
       (create-other-organism-options-marine-unicellular))
      (create-palette-submenu
       ;; "Terrestrial and limnetic unicellular cyanobacteria" 
       "Terr. and limnetic unicellular cyano " 
       (create-subset-of-organism-submenus 
        cl-user::*organisms-descriptor* 
        bbi::*terrestrial-and-limnetic-unicellular-cyanobacteria*)
       (create-other-organism-options-terrestrial-and-limnetic-unicellular))
      (create-palette-submenu 
       ;; "Filamentous cyanobacteria" 
       "Filamentous cyano" 
       (create-subset-of-organism-submenus 
        cl-user::*organisms-descriptor* bbi::*filamentous-cyanobacteria*)
       (create-other-organism-options-filamentous))
      (create-palette-submenu 
       ;; "N fixing cyanobacteria" 
       "N fixing cyano" 
       (create-subset-of-organism-submenus 
        cl-user::*organisms-descriptor* bbi::*n-fixing-cyanobacteria*)
       (create-other-organism-options-n-fixing))


      (create-palette-submenu 
       ;; "Microarrays" 
       "Microarrays" 
       (create-subset-of-microarray-submenus 
        cl-user::*organisms-descriptor* bbi::*microarrays*)
       (create-other-organism-options-microarrays)
       )

      (create-palette-submenu 
       "kegg-pathways"
       nil
       (mapcar 
        (lambda (x) 
          (menu-item (string x) (get-id-and-add-vpl-data-to-hash x)))
        *dummy-pathway-names*
        ))        
      ))))

(defmethod organism-subset-names ((d (eql :cyanobacteria)))
  (copy-list
   '(BBI:*ALL-CYANOBACTERIA*
     BBI:*COMPLETE-GENOMES*
     BBI:*MARINE-CYANOBACTERIA*
     BBI:*TERRESTRIAL-AND-LIMNETIC-CYANOBACTERIA*
     BBI:*UNICELLULAR-CYANOBACTERIA*
     BBI:*FILAMENTOUS-CYANOBACTERIA*
     BBI:*N-FIXING-CYANOBACTERIA*
     )))

(defun create-other-organism-options-marine-unicellular ()
  (loop for subset-name in
        '(BBI:*MARINE-UNICELLULAR-CYANOBACTERIA*
          BBI:*MARINE-PROCHLOROCOCCUS-AND-SYNECHOCOCCUS*
          BBI:*MARINE-PROCHLOROCOCCUS*
          BBI:*MARINE-SYNECHOCOCCUS*)
        collect 
        (hack-organism-related-name-and-add-to-data-hash subset-name)
        ))

(defun create-other-organism-options-terrestrial-and-limnetic-unicellular ()
  (loop for subset-name in 
        '(BBI:*TERRESTRIAL-AND-LIMNETIC-UNICELLULAR-CYANOBACTERIA*
          BBI:*TERRESTRIAL-AND-LIMNETIC-SYNECHOCOCCUS*) 
        collect 
        (hack-organism-related-name-and-add-to-data-hash subset-name)
        ))

(defun create-other-organism-options-filamentous ()
  (loop for subset-name in
        '(BBI:*FILAMENTOUS-CYANOBACTERIA*
          BBI:*HETEROCYSTOUS-CYANOBACTERIA*
          BBI:*MARINE-FILAMENTOUS-CYANOBACTERIA*
          BBI:*TERRESTRIAL-AND-LIMNETIC-FILAMENTOUS-CYANOBACTERIA*) 
        collect 
        (hack-organism-related-name-and-add-to-data-hash subset-name)
        ))

(defun create-other-organism-options-n-fixing ()
  (loop for subset-name in
        '(BBI:*N-FIXING-CYANOBACTERIA*
          BBI:*MARINE-N-FIXING-CYANOBACTERIA*
          BBI:*TERRESTRIAL-AND-LIMNETIC-N-FIXING-CYANOBACTERIA*
          BBI:*UNICELLULAR-N-FIXING-CYANOBACTERIA*
          BBI:*FILAMENTOUS-N-FIXING-CYANOBACTERIA*
          BBI:*HETEROCYSTOUS-CYANOBACTERIA*) 
        collect 
        (hack-organism-related-name-and-add-to-data-hash subset-name)
        ))
(defun create-other-organism-options-microarrays ()
  (loop for subset-name in
        '(BBI:*microarrays*) 
        collect 
        (hack-organism-related-name-and-add-to-data-hash subset-name)
        ))