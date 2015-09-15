;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)

(defmethod create-organism-submenus ((d (eql :photosynthetic-bacteria)))
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
       ;; "All green bacteria" 
       "All green bacteria" 
       (create-subset-of-organism-submenus 
        cl-user::*organisms-descriptor* 
        bbi::*all-green-bacteria*)
       (create-other-organism-options-all-green-bacteria))
      (create-palette-submenu 
       ;; "Chlorobi" 
       "Chlorobi" 
       (create-subset-of-organism-submenus 
        cl-user::*organisms-descriptor* 
        bbi::*chlorobi*)
       (create-other-organism-options-chlorobi))
      (create-palette-submenu 
       ;; "Chloroflexi" 
       "Chloroflexi" 
       (create-subset-of-organism-submenus 
        cl-user::*organisms-descriptor* 
        bbi::*chloroflexi*)
       (create-other-organism-options-chloroflexi))
      (create-palette-submenu 
       ;; "Purple bacteria" 
       "Purple bacteria" 
       (create-subset-of-organism-submenus 
        cl-user::*organisms-descriptor* 
        bbi::*purple-bacteria*)
       (create-other-organism-options-purple-bacteria))
      (create-palette-submenu 
       "microarrays"
       nil
       (mapcar 
        (lambda (x) 
          (menu-item x (get-id-and-add-vpl-data-to-hash x)))
        *dummy-microarray-names*
        ))
      (create-palette-submenu 
       "kegg-pathways"
       nil
       (mapcar 
        (lambda (x) 
          (menu-item x (get-id-and-add-vpl-data-to-hash x)))
        *dummy-pathway-names*
        ))
      ))))

(defmethod organism-subset-names ((d (eql :photosynthetic-bacteria)))
  (copy-list
   '(BBI:*ALL-CYANOBACTERIA*
     BBI:*MARINE-CYANOBACTERIA*
     BBI:*TERRESTRIAL-AND-LIMNETIC-CYANOBACTERIA*
     BBI:*UNICELLULAR-CYANOBACTERIA*
     BBI:*FILAMENTOUS-CYANOBACTERIA*
     BBI:*N-FIXING-CYANOBACTERIA*
     BBI:*ALL-GREEN-BACTERIA*
     BBI:*CHLOROBI*
     BBI:*CHLOROFLEXI* 
     BBI:*PURPLE-BACTERIA*
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

(defun create-other-organism-options-all-green-bacteria ()
  (loop for subset-name in '(BBI:*all-green-bacteria*) 
   collect 
   (hack-organism-related-name-and-add-to-data-hash subset-name)
   ))

(defun create-other-organism-options-chlorobi ()
  (loop for subset-name in '(BBI:*chlorobi*) 
   collect 
   (hack-organism-related-name-and-add-to-data-hash subset-name)
   ))

(defun create-other-organism-options-chloroflexi ()
  (loop for subset-name in '(BBI:*chloroflexi*) 
   collect 
   (hack-organism-related-name-and-add-to-data-hash subset-name)
   ))

(defun create-other-organism-options-purple-bacteria ()
  (loop for subset-name in '(BBI:*purple-bacteria*) 
   collect 
   (hack-organism-related-name-and-add-to-data-hash subset-name)
   ))
