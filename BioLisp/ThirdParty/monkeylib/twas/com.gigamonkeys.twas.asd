(defsystem com.gigamonkeys.twas
  :components
  ((:file "packages")
   (:file "patches")
   (:file "twas" :depends-on ("packages"))))