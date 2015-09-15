(in-package :aframes)

(def-frame-class 
 #$test-gene () 
 ((#$from :domain integer :allocation :instance :initform 0) 
  (#$to :domain integer :allocation :instance :initform 0)
  (#$component-of :allocation :class :initform #$organism)
  ))



(def-frame-class 
 #$sys.historical ()
 ((#$sys.creator 
   :initform 
   (and (find-package :wb) 
        (symbol-value-in-package 
         :*username* :wb 
         :if-does-not-exist :create
         :if-unbound :set 
         :if-unbound-value nil)))
  (#$sys.creation-date :initform (get-universal-time) :domain integerp)
  (#$sys.original-frame-slots :initform nil)
  (#$sys.change-log :initform nil)
  ))
 
 
(def-frame-class 
 #$test.h (#$historical)
 ((#$test.aslot :initform 3)
  (#$test.bslot :initform #$test.cat)
  ))
 