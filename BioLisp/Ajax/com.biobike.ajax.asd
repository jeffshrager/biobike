;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(defsystem com.biobike.ajax
  :name "com.biobike.ajax"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :components	
  (
   
   ;; (:file "packages")
   ;; (:file "ajax" :depends-on ("packages"))
   
   (:file "ajax")

   ;; Apps
   ;; These are other application demos, not relevant to the VPL
   ;; Commenting them out shouldn't effect anything. --JP
   ;; (:file "reverser" :depends-on ("packages"))
   ;; (:file "calc" :depends-on ("packages"))
   
   ;; The load of these files has been moved to the vs-load.lisp file
   ;; (:file "vpl-handle" :depends-on ("packages"))
   ;; (:file "vpl" :depends-on ("packages"))
   ;; (:file "vpl-menus" :depends-on ("packages"))
   ;; (:file "vpl-workspacepipe" :depends-on ("packages"))
   
   )

  :depends-on (:com.gigamonkeys.ajax))

