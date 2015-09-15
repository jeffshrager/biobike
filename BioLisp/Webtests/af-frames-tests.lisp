;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

;;; Author:  JP Massar.

(in-package :weblistener)

;;; Verify error on illegal characters in frame names.
(defframetest frame-syntax-1
  (and (tests:signals-error-of-type (t) (frame-fnamed "abc " t))
       (tests:signals-error-of-type (t) (frame-fnamed "(abc" t))
       (tests:signals-error-of-type (t) (frame-fnamed "a)bc" t)))
  t)

;;; Verify frame names are case-insensitive, that two temporary
;;; frames created successively are not EQ, and that 
;;; the #$Fname slot can be retrieved.
(defframetest 
 frame-basics-1
 (and (isframe? #$test.magic-word)
      (eq (frame-fnamed "TEST.Xyzzy") #$test.xyzzy)
      (eq (frame-fnamed "test.xyzzY") #$test.xyzzy)
      (eq (frame-fnamed "tesT.XYZZY") #$test.xyzzy)
      (eq (frame-fnamed "test.xyzzy") #$test.xyzzy)
      (not (eq (make-temp-frame) (make-temp-frame)))
      (member #$Fname (frame-slots-of #$test.plover))
      t)
   t
   )

;;; Verify that a simple frame and slot can be stored into a retrieved.
(defframetest 
 frame-access-1
 (progn 
   (setf (slotv #$test.xyzzy #$test.magic-word) t)
   (values (slotv #$test.xyzzy #$test.magic-word)))
 t)

;;; Another simple store and retrieve
(defframetest frame-basics-2
  (list
   (progn
     (setf (slotv #$test.plugh #$test.goes-to) 'frames::house)
     (slotv #$test.plugh #$test.goes-to))
   (progn
     (setf (slotv #$test.xyzzy #$test.related-words) (list #$test.plover))
     (slotv #$test.xyzzy #$test.related-words)))
  (list 'frames::house (list #$test.plover))
  :comparison #'equal)

;;; DEF-FRAME mechanism with slot arguments.
;;; Also uninterning of frame when run again.
(defframetest frame-basics-3
  (let ((f (frame-fnamed "Adventure")))
    (when f (unintern-frame f))
    (def-frame "Adventure" 
               #$test.version 1.0 
               #$test.cave-status "under construction")
    (values (slotv (frame-fnamed "Adventure") #$test.version)))
  1.0
  :comparison #'=)

;;; Frame renaming works.
(defframetest 
 rename-frame-1
 (let ((x (frame-fnamed "test.Froogle" t)))
   (unintern-frame (frame-fnamed "test.Google" t))
   (rename-frame x "test.Google")
   (let ((y (frame-fnamed "test.Google")))
     (and (eq x y) 
          (or (string= "test.Google" (slotv x #$Fname))
              (string= "test.Google" (slotv x #$sys.pretty-name))))))
 t)

;;; Frame renaming signals error if new name is an existing frame
(defframetest rename-frame-2
  (tests:signals-error-of-type 
   (t) 
   (rename-frame #$test.xyzzy frames::*timestamp-frame-name*))
  t)

;;; Test that purging simple frame works.  Also tests FOR-EACH-FRAME-SLOT.
(defframetest 
 purge-frame-1
 (progn
   (purge-frame #$test.xyzzy)
   (def-frame #$test.xyzzy #$test.magic-word t #$test.goes-to "house")
   (let ((count1 0) (count2 0))
     (for-each-frame-slot (slot value) #$test.xyzzy slot value (incf count1))
     (purge-frame #$test.xyzzy)
     (for-each-frame-slot (slot value) #$test.xyzzy slot value (incf count2))
     (list count1 count2)
     ))
 (list 4 2)
 :comparison 'equal)

;;; Test that timestamp gets reset and pretty-name is not destroyed.
(defframetest 
 purge-frame-2
 (progn
   (purge-frame #$test.Xqt)
   (sleep 2)
   (def-frame #$test.Xqt #$test.magic-word t #$test.goes-to "house")
   (let ((ts (#^sys.timestamp #$test.Xqt)))
     (purge-frame #$test.Xqt)
     (and (not (= ts (#^sys.timestamp #$test.Xqt))) 
          (or (not user::*acache-running*) 
              (not (null (#^sys.pretty-name #$test.Xqt)))))))
 t
 :comparison 'equal)

;;; Test that DELETE-SLOT works.
(defframetest
    delete-slot-1
  (progn
    (purge-frame #$test.xyzzy)
    (def-frame #$test.xyzzy #$test.magic-word t #$test.goes-to "house")
    (list
     (slotv #$test.xyzzy #$test.magic-word)
     (progn 
       (delete-slot #$test.xyzzy #$test.magic-word) 
       (slotv #$test.xyzzy #$test.magic-word))
     (slotv #$test.xyzzy #$test.goes-to)
     ))
  (list t nil "house")
  :comparison 'equal) 

;;; Make sure the FNAME slot cannot be deleted.
(defframetest
    delete-slot-2
  (tests:signals-error-of-type (t)
    (delete-slot #$test.xyzzy #$fname))
  t)

;;; Simple add/delete elements from list/set slots.
(defframetest add-element-1
  (progn
    (setf (slotv #$test.xyzzy #$test.unique-letter-strings) '("x" "y"))
    (add-element #$test.xyzzy #$test.unique-letter-strings "z" :test 'string=)
    (and
     (has-element? #$test.xyzzy #$test.unique-letter-strings "z" :test 'string=)
     (has-element? #$test.xyzzy #$test.unique-letter-strings "y" :test 'string=)
     t))
  t)

;;; Simple add/delete elements from list/set slots.
(defframetest delete-element-1
  (progn
    (setf (slotv #$test.xyzzy #$test.unique-letter-strings) '("x" "y" "z"))
    (delete-element 
     #$test.xyzzy #$test.unique-letter-strings "y" :test 'string=)
    (and
     (has-element? #$test.xyzzy #$test.unique-letter-strings "z" :test 'string=)
     (not (has-element? #$test.xyzzy #$test.unique-letter-strings "y" 
                        :test 'string=))
     t))
  t)

;;; Tests for a computed slot

;;; Basic test

(defframetest computed-slot-1
  (progn
    (delete-slot #$test.xyzzy #$test.Fname-length)
    (list (slotv #$test.xyzzy #$test.Fname-length) 
          (slotv #$test.xyzzy #$test.Fname-length)))
  '(10 10)
  :comparison #'equal)


;;; Make sure computation only happens the first time.

(defframetest computed-slot-2
  (progn 
    (setq *side-effect-variable* 0)
    (delete-slot #$test.xyzzy #$test.SideEffect)
    (list (slotv #$test.xyzzy #$test.SideEffect) 
          (slotv #$test.xyzzy #$test.SideEffect)))
  '(1 1)
  :comparison #'equal)

;; Make sure slots which are always computed really are.

(defframetest computed-slot-3
  (progn
    (setq *side-effect-variable* 0)
    (slotv #$test.xyzzy #$test.Always-compute-me)
    (values (slotv #$test.xyzzy #$test.Always-compute-me)))
  2)

;;; Tests the inverse slot pair #$test.isA <-> #$test.Subclasses
;;; Note that sys.Isa and sys.Subclasses are NOT defined as inverses.

(defframetest inverse-slot-1
  (progn
    (setf (slotv #$test.magic-word #$test.Subclasses) nil)
    ;; Make each particular magic word note that it is, indeed,
    ;; a #$test.magic-word
    ;; by having #$test.magic-word be the sole element on its #$sys.isa list.
    (dolist (mw (list #$test.xyzzy #$test.plugh #$test.plover))
      (setf (slotv mw #$test.isa) nil)
      (setf (slotv mw #$test.isa) (list #$test.magic-word)))
    ;; Now #$test.magic-word should have recorded on its #$test.subclasses list
    ;; all the magic words we said were magic words.
    ;; Of course #$test.subclasses is inappropriate here, should be #$InstanceOf
    ;; of somesuch, but that's not how basic system works)
    (sort-frames-by-fname (slotv #$test.magic-word #$test.subclasses)))
  (sort-frames-by-fname (list #$test.xyzzy #$test.plugh #$test.plover))
  :comparison #'equal)


;;; This is probably equivalent to the above test now.
;;; Create the inverted slot pair #
;;; $test.IsAnInstanceOf and #$test.InstantiationsOf
;;; and test in the same manner

(defframetest inverse-slot-2
  (progn
    (setf (slotv #$test.magic-word #$test.InstantiationsOf) nil)
    (dolist (mw (list #$test.xyzzy #$test.plugh #$test.plover))
      (setf (slotv mw #$test.IsAnInstanceOf) nil)
      (setf (slotv mw #$test.IsAnInstanceOf) (list #$test.magic-word))
      )
    (sort-frames-by-fname (slotv #$test.magic-word #$test.InstantiationsOf)))
  (sort-frames-by-fname (list #$test.xyzzy #$test.plugh #$test.plover))
  :comparison #'equal)

;; Test standard inheritance through #$sys.isa

(defframetest inherited-slot-1
  (progn
    (pushnew #$test.magic-word (slotv #$test.xyzzy #$sys.isa))
    (delete-slot #$test.xyzzy #$test.part-of-speech)
    (setf (slotv #$test.magic-word #$test.part-of-speech) :noun)
    (values (slotv #$test.xyzzy #$test.part-of-speech)))
  :noun
  )

;; Make sure if we override the override takes effect

(defframetest inherited-slot-2
  (progn
    (setf (slotv #$test.xyzzy #$test.part-of-speech) :adjective)
    (values (slotv #$test.xyzzy #$test.part-of-speech)))
  :adjective
  )


;; Test that non-standard inheritance (through a different slot)
;; works too, and works through multiple levels, and works with
;; more than one thing on the inherits-from list.

(defframetest inherited-slot-3
  (progn
    (setf (slotv #$test.Quadraped #$test.Number-of-legs) 4)
    (setf (slotv #$test.Dog #$test.IsAnInstanceOf) 
          (list #$test.Mammal #$test.Quadraped))
    (setf (slotv #$test.Rover #$test.IsAnInstanceOf) 
          (list #$test.Dog #$test.Pet))
    (values (slotv #$test.Rover #$test.Number-of-legs)))
  4)


(defframetest 
    flags-1
  (progn
    (setf (slotv #$test.fred #$test.wife) #$test.wilma)
    (values (slotv #$test.fred #$sys.frame-modified?)))
  t)

(defframetest 
    flags-2
  (let ((f (genframe "xyz")))
    (values (slotv f #$sys.frame-modified?)))
  nil)

;;; Verify instance slots got initialized properly.
(defframetest
    typed-frame-init-1
  (with-temporary-unique-gene (g)
    (and (zerop (slotv g #$test.from))
         (null (slotv g #$test.to))
         (null (slotv g #$test.proteins))
         ))
  t)

;;; Verify class slot got initialized in class frame.
(defframetest
    typed-frame-init-2
  (eq (slotv #$test.gene #$test.component-of) #$test.organism)
  t)

;;; Test access to defined slot of typed frame.  
(defframetest 
    typed-frame-1
  (with-temporary-unique-gene (g)
    (setf (slotv g #$test.from) 3)
    (values (slotv g #$test.from)))
  3)

;;; Test access to undefined slot of typed frame.  
(defframetest 
    typed-frame-2
  (with-temporary-unique-gene (g)
    (setf (slotv g #$test.description) "gene desc.")
    (values (slotv g #$test.description)))
  "gene desc."
  :comparison 'string-equal)

;;; Test access to class slot in #$test.gene
(defframetest 
    typed-frame-3
  (with-temporary-unique-gene (g)
    (values (slotv g #$test.component-of)))
  #$test.organism
  )

;;; Test inheritance through class frame
(defframetest 
    typed-frame-4
  (with-temporary-unique-gene (g)
    (setf (slotv #$test.gene #$test.fred) 5)
    (values (slotv g #$test.fred)))
  5)

(defframetest
    typed-frame-5
  (let ((c (make-frame-instance 'frames::test.cat "Snagglepuss" t)))
    (list (slotv c #$test.legs) 
          (slotv c #$test.reproductive-method)
          (slotv c #$test.chemical-basis)))
  (list 4 #$test.sexual #$test.carbon)
  :comparison 'equal
  )

;;; Test accessing frame bit as slot.  
(defframetest 
    typed-frame-bit-1
  (with-temporary-unique-gene (g)
    (values (slotv g #$sys.frame-typed?)))
  t)

;;; Test that modified bit is indeed set and accessible
;;; and is NOT inherited through the class frame!
(defframetest 
    typed-frame-bit-2
  (with-temporary-unique-gene (g)
    (frames::clear-frame-flag g :frame-modified?)
    (frames::clear-frame-flag #$test.gene :frame-modified?)
    (setf (slotv g #$test.fred) 5)
    (and (eq 5 (slotv g #$test.fred)) (eq t (slotv g #$sys.frame-modified?))))
  t)

;;; Make sure we cannot delete fixed slots.
(defframetest
    typed-delete-slot-1
  (tests:signals-error-of-type (t) 
    (with-temporary-unique-gene (g)  
      (delete-slot g #$test.from)))
  t)

;;; Test that :domain :set allows us to set to a set
(defframetest 
    typed-frame-domain-1
  (with-temporary-unique-gene (g)
    (progn (setf (slotv g #$test.proteins) '(1 2)) t))
  t)

;;; Test that :domain :set does not allow us to set to an atom
;;; Apparently this functionality is not implemented!
;;; (Why was the test written?)
(defframetest 
    typed-frame-domain-2
  (tests:signals-error-of-type (t) 
    (with-temporary-unique-gene (g)
      (progn (setf (slotv g #$test.proteins) #$test.fred) t)))
  t)

;;; Make sure we are iterating over all the framespaces
(defframetest 
 iter1
 (let ((fsnames nil))
   (if user::*acache-running* 
       (progn 
         (cformatt "Not iterating over all acache frames!!") t)
     (progn 
       (with-frames-iterated (f)
         (pushnew (frames::frame->framespace-name f) fsnames 
                  :test 'string-equal))
       ;; must be at least "sys", "", and "test"...
       (> (length fsnames) 2))))
 t)

;;; Make sure we can iterate over a single framespace 
(defframetest 
 iter2 
 (with-frames-iterated (f :test) (fname f))
 nil)
   
   
(defframetest 
 iter3
 (plusp 
  (length 
   (nmapframes (lambda (x) (and (search "p" (fname x)) x)) :framespace :test)))
 t)

(defframetest 
 search1
 (let ((x #$test.fred))
   (setf (slotv x #$weird-slot) "xxx999")
   (search-frames "xxx999" (list :sys :default :test)))
 (list #$test.fred) 
 :comparison 'equal)

(defframetest 
 search2
 (and (member #$weird-slot (search-frames "weird" '(:default :sys))) t)
 t)

(defframetest 
    frame-iter-1
  (let* ((frame (def-frame 
                 #$test.foo #$test.s1 1 
                 #$test.s2 2 
                 #$test.s3 3))
         (frame2 (def-frame 
                  #$test.foo1 #$test.s1 (list 1) 
                  #$test.s2 (list 2) 
                  #$test.s3 (list 3)))
         (s (iter-init frame2)))
    (declare (ignore frame))
    (loop until (not (iter-next? s)) 
          sum
          (destructuring-bind (slot value) (iter-next s) 
            (declare (ignore slot))
            (if (listp value) (first value) 0)
            )))
  6)

(defframetest 
 frameref-1
 (ref #$test.foo #$test.s1 #$test.s3)
 (list 1 3)
 :comparison 'equal)
(defframetest 
 framecref-1
 (cref #$test.foo #$test.s1 #$test.s3)
 (list 1 3)
 :comparison 'equal)

(defframetest 
 sframeref-1
  (progn 
    (setf (ref #$test.foo #$test.s1 #$test.s3) '(5 6))
    (ref #$test.foo #$test.s1 #$test.s2 #$test.s3))
 (list 5 2 6)
 :comparison 'equal)
(defframetest 
 sframecref-1
  (progn 
    (setf (cref #$test.foo #$test.s1 #$test.s3) '(5 6))
    (cref #$test.foo #$test.s1 #$test.s2 #$test.s3))
 (list 5 2 6)
 :comparison 'equal)





#|


(aframes::def-reciprocal-inverse-slots 
 #$test.might-be-an-instance-of #$test.might-contain)


(defframetest reciprocal-inverse-1
  (progn
    (pushnew #$test.Amphibian (slotv #$test.Frog #$test.might-be-an-instance-of))
    (pushnew #$test.Iguana (slotv #$test.Amphibian #$test.might-contain))
    (and (member #$test.Frog (slotv #$test.Amphibian #$test.might-contain))
         (member #$test.Amphibian 
                 (slotv #$test.Iguana #$test.might-be-an-instance-of))
         t))
  t)


(defframetest transitive-slot-1
  (progn
    (pushnew #$test.word (slotv #$test.magic-word #$test.Isa))
    (let ((all (slotv #$test.xyzzy #$test.AllIsa)))
      (and (member #$test.magic-word all) (member #$test.word all) t)))
  t)

(def-transitive-slot #$test.AllClassesOf #$test.IsAnInstanceOf)

(defframetest transitive-slot-2
  (let ((all-classes (slotv #$test.Rover #$test.AllClassesOf)))
    (and (member #$test.Dog all-classes)
         (member #$test.Pet all-classes)
         (member #$test.Mammal all-classes)
         (member #$test.Quadraped all-classes)
         t
         ))
  t)

(progn
  (defslot #$test.siblings :set-valued? t)
  ;; Simple test
  (defframetest set-valued-1
    (progn
      (setf (slotv #$test.Jeff #$test.siblings) 
            (list #$test.Mary #$test.Beth #$test.Joe))
      (slotv #$test.Jeff #$test.siblings))
    (list #$test.Mary #$test.Beth #$test.Joe)
    :comparison 'equal)
  ;; Test ADD-ELEMENT
  (defframetest set-valued-2
    (progn 
      (setf (slotv #$test.Jeff #$test.siblings) (list #$test.Mary #$test.Beth))
      (add-element #$test.Jeff #$test.siblings #$test.Joe)
      (add-element #$test.Jeff #$test.siblings #$test.John)
      (sort-frames-by-fname (slotv #$test.Jeff #$test.siblings)))
    (sort-frames-by-fname (list #$test.Mary #$test.Beth #$test.Joe #$test.John))
    :comparison 'equal)
  ;; Test that non-list gets converted to list if so specified
  (defframetest set-valued-3
    (let ((*set-valued-non-list-action* :convert-silently))
      (setf (slotv #$test.Jeff #$test.siblings) #$test.Mary)
      (slotv #$test.Jeff #$test.siblings))
    (list #$test.Mary)
    :comparison 'equal)
  ;; Test that non-list errors out if so specified.
  (defframetest set-valued-4
    (let ((*set-valued-non-list-action* :error))
      (handler-case
          (progn (setf (slotv #$test.Jeff #$test.siblings) #$test.Mary) nil)
        (error () t)))
    t))

(progn
  (defslot #$test.Portfolio :set-valued? t)
  (def-indexed-slot #$test.Portfolio :test 'equal)
  (def-indexed-slot #$test.BestPick)
  (defframetest indexed-1
    (let ((*set-valued-non-list-action* :convert-silently))
      (let ((ht (slotv #$test.Portfolio #$sys.hashtable)))
        (when (hash-table-p ht) (clrhash ht))
        (setf (slotv #$test.Massar #$test.Portfolio) 
              (list "COMPAQ" "IBM" "DELL"))
        (setf (slotv #$test.Shrager #$test.Portfolio) 
              (list "DELL" "APPLE" "JETBLUE"))
        (setf (slotv #$test.Travers #$test.Portfolio) "FORD")
        (add-element #$test.Travers #$test.Portfolio "DELL" :test 'string=)
        ;; Since index test is EQUAL, not EQUALP, this will not index with
        ;; "APPLE"
        (add-element #$test.Travers #$test.Portfolio "Apple" :test 'string=)
        (mapcar 'sort-frames-by-fname
                (list
                 (slot-lookup "General Motors" #$test.Portfolio)
                 (slot-lookup "DELL" #$test.Portfolio)
                 (slot-lookup "APPLE" #$test.Portfolio)
                 ))))
    (mapcar 'sort-frames-by-fname
            (list nil (list #$test.Massar #$test.Shrager #$test.Travers) 
                  (list #$test.Shrager)))
    :comparison 'equal)
  (defframetest indexed-2
    (progn
      (let ((ht (slotv #$test.BestPick #$sys.hashtable)))
        (when (hash-table-p ht) (clrhash ht)))
      (setf (slotv #$test.Massar #$test.Bestpick) "DELL")
      (setf (slotv #$test.Shrager #$test.Bestpick) "DELL")
      (setf (slotv #$test.Travers #$test.Bestpick) "FORD")
      (mapcar 'sort-frames-by-fname
              (list
               (slot-lookup "General Motors" #$test.Bestpick)
               (slot-lookup "DELL" #$test.Bestpick)
               (slot-lookup "FORD" #$test.Bestpick)
               )))
      (mapcar 'sort-frames-by-fname
              (list nil (list #$test.Massar #$test.Shrager) 
                    (list #$test.Travers)))
    :comparison 'equal)
  )
  
(defframetest 
    search-frames-1 
  (with-temp-frames (f '("abcdefg" "abcdefgh" "abcdefghi"))
    (let ((results (search-frames "abcdef")))
      (= 3 (length (intersection results f)))))
  t
  )

|#
