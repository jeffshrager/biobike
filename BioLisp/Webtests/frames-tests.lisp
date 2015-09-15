;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

;;; Author:  JP Massar.

(in-package :weblistener)

(defframetest frame-syntax-1
  (and (tests:signals-error-of-type (t) (frame-fnamed "abc " t))
       (tests:signals-error-of-type (t) (frame-fnamed "(abc" t))
       (tests:signals-error-of-type (t) (frame-fnamed "a)bc" t)))
  t)

(defframetest frame-basics-1
  (and (isframe? #$test.magic-word)
       (eq (frame-fnamed "Test.Xyzzy") #$test.xyzzy)
       (eq (frame-fnamed "test.xyzzy") #$test.xyzzy)
       (eq (frame-fnamed "TEST.XYZZY") #$test.xyzzy)
       (eq (frame-fnamed "test.xyzzy") #$test.xyzzy)
       #-:jpmtf
       (not (eq (make-temp-frame) (make-temp-frame)))
       #-:jpmtf
       (member #$Fname (frame-slots-of #$test.plover))
       #+:jpmtf
       (member #$Fname (frame-slots-of #$test.plover) :key 'first)
       t)
  t
  )


(defframetest frame-basics-2
  (list
   (progn
     (setf (slotv #$test.plugh #$test.goes-to) 'house)
     (slotv #$test.plugh #$test.goes-to))
   (progn
     (setf (#^test.related-words #$test.xyzzy) (list #$test.plover))
     (#^test.related-words #$test.xyzzy)))
  (list 'house (list #$test.plover))
  :comparison #'equal)

(defframetest frame-basics-3
  (let ((f (frame-fnamed "Test.Adventure")))
    (when f (unintern-frame f))
    (def-frame "Test.Adventure" 
               #$test.version 1.0 
               #$test.cave-status "under construction")
    (slotv (frame-fnamed "Test.Adventure") #$test.version))
  1.0
  :comparison #'=)


#-:sframes
(defframetest rename-frame-1
  (let ((x (frame-fnamed "Test.Froogle" t)))
    (unintern-frame (frame-fnamed "Test.Google" t))
    (rename-frame x "Test.Google")
    (let ((y (frame-fnamed "Test.Google")))
      (and (eq x y) (string= "Test.Google" (slotv x #$Fname)) t)
      ))
  t)


#-:sframes
(defframetest rename-frame-2
  (tests:signals-error-of-type 
   (t) 
   (rename-frame #$test.xyzzy (frame-fnamed "Isa")))
  t)

(defframetest add-element-1
  (progn
    (setf (slotv #$test.xyzzy #$test.unique-letter-strings) '("x" "y"))
    (add-element #$test.xyzzy #$test.unique-letter-strings "z" :test 'string=)
    (and
     (has-element? #$test.xyzzy #$test.unique-letter-strings "z" :test 'string=)
     (has-element? #$test.xyzzy #$test.unique-letter-strings "y" :test 'string=)
     t))
  t)

(defframetest delete-element-1
  (progn
    (setf (slotv #$test.xyzzy #$test.unique-letter-strings) '("x" "y" "z"))
    (delete-element
     #$test.xyzzy #$test.unique-letter-strings "y" :test 'string=)
    (and
     (has-element? #$test.xyzzy #$test.unique-letter-strings "z" :test 'string=)
     (not 
      (has-element?
       #$test.xyzzy #$test.unique-letter-strings "y" :test 'string=))
     t))
  t)


;;; Tests for a computed slot

;;; Basic test



#-:sframes
(defframetest computed-slot-1
  (progn
    (setf (slotv #$test.xyzzy #$Test.Fname-Length) nil)
    (list (slotv #$test.xyzzy #$Test.Fname-Length)
          (slotv #$test.xyzzy #$Test.Fname-Length)))
  '(10 10)
  :comparison #'equal)


;;; Make sure computation only happens the first time.

#-:sframes
(defframetest computed-slot-2
  (progn 
    (setq *side-effect-variable* 0)
    (setf (slotv #$test.xyzzy #$Test.Sideeffect) nil)
    (list
     (slotv #$test.xyzzy #$Test.Sideeffect)
     (slotv #$test.xyzzy #$Test.Sideeffect)))
  '(1 1)
  :comparison #'equal)

#-:sframes
(defframetest computed-slot-3
  (progn
    (setq *side-effect-variable* 0)
    (slotv #$test.xyzzy #$Test.Always-Compute-Me)
    (slotv #$test.xyzzy #$Test.Always-Compute-Me))
  2)

;;; Tests the inverted slot pair #$isA <-> #$Subclasses

#-:sframes
(defframetest inverse-slot-1
  (progn
    (setf (slotv #$test.magic-word #$Subclasses) nil)
    ;; Make each particular magic word note that it is, indeed,
    ;; a #$test.magic-word
    ;; by having #$test.magic-word be the sole element on its #$Isa list.
    (dolist (mw (list #$test.xyzzy #$test.plugh #$test.plover))
      (setf (slotv mw #$isA) nil)
      (setf (slotv mw #$isA) (list #$test.magic-word)))
    ;; Now #$test.magic-word should have recorded on its #$Subclasses list
    ;; all the magic words we said were magic words.
    ;; (Of course, #$Subclasses is inappropriate here, should be #$InstanceOf
    ;; of somesuch, but that's not how basic system works)
    (sort-frames-by-fname (slotv #$test.magic-word #$Subclasses)))
  (sort-frames-by-fname (list #$test.xyzzy #$test.plugh #$test.plover))
  :comparison #'equal)

#-:sframes
(defframetest inverse-slot-2
  (progn
    (setf (slotv #$test.magic-word #$Test.Instantiationsof) nil)
    (dolist (mw (list #$test.xyzzy #$test.plugh #$test.plover))
      (setf (slotv mw #$Test.IsAnInstanceOf) nil)
      (setf (slotv mw #$test.IsAnInstanceOf) (list #$test.magic-word))
      )
    (sort-frames-by-fname (slotv #$test.magic-word #$Test.Instantiationsof)))
  (sort-frames-by-fname (list #$test.xyzzy #$test.plugh #$test.plover))
  :comparison #'equal)

#-:sframes
(defframetest inherited-slot-1
  (progn
    ;; We've already set up that #$test.xyzzy #$Isa #$Test.Magic-Word
    ;; in a previous test
    (setf (slotv #$test.xyzzy #$test.part-of-speech) nil)
    (setf (slotv #$test.magic-word #$test.part-of-speech) :noun)
    (slotv #$test.xyzzy #$test.part-of-speech))
  :noun
  )

;; Make sure if we override the override takes effect

#-:sframes
(defframetest inherited-slot-2
  (progn
    (setf (slotv #$test.xyzzy #$test.part-of-speech) :adjective)
    (slotv #$test.xyzzy #$test.part-of-speech))
  :adjective
  )

#-:sframes
(defframetest inherited-slot-3
  (progn
    (setf (slotv #$Test.Quadraped #$Test.Number-Of-Legs) 4)
    (setf (slotv #$test.dog #$test.IsAnInstanceOf) 
          (list #$Test.Mammal #$Test.Quadraped))
    (setf (slotv #$Test.Rover #$test.IsAnInstanceOf) 
          (list #$test.dog #$test.Pet))
    (slotv #$Test.Rover #$Test.Number-Of-Legs))
  4)


#-:sframes
(defframetest reciprocal-inverse-1
  (progn
    (pushnew #$Test.Amphibian 
             (slotv #$Test.Frog #$test.might-be-an-instance-of))
    (pushnew #$Test.Iguana (slotv #$Test.Amphibian #$test.might-contain))
    (and (member #$Test.Frog (slotv #$Test.Amphibian #$test.might-contain))
         (member #$Test.Amphibian 
                 (slotv #$Test.Iguana #$test.might-be-an-instance-of))
         t))
  t)

#-:sframes
(defframetest transitive-slot-1
  (progn
    (pushnew #$test.word (slotv #$test.magic-word #$Isa))
    (let ((all (slotv #$test.xyzzy #$AllIsa)))
      (and (member #$test.magic-word all) (member #$test.word all) t)))
  t)


#-:sframes
(defframetest transitive-slot-2
  (let ((all-classes (slotv #$Test.Rover #$test.AllClassesOf)))
    (and (member #$test.dog all-classes)
         (member #$test.pet all-classes)
         (member #$Test.Mammal all-classes)
         (member #$Test.Quadraped all-classes)
         t
         ))
  t)

  ;; Simple test
(defframetest set-valued-1
  (progn
    (setf (slotv #$test.jeff #$test.siblings) 
          (list #$Test.Mary #$Test.Beth #$test.joe))
    (slotv #$test.jeff #$test.siblings))
  (list #$Test.Mary #$Test.Beth #$test.joe)
  :comparison 'equal)
  ;; Test ADD-ELEMENT
(defframetest set-valued-2
  (progn 
    (setf (slotv #$test.jeff #$test.siblings) (list #$Test.Mary #$test.Beth))
    (add-element #$test.jeff #$test.siblings #$test.joe)
    (add-element #$test.jeff #$test.siblings #$test.john)
    (sort-frames-by-fname (slotv #$test.jeff #$test.siblings)))
  (sort-frames-by-fname (list #$Test.Mary #$Test.Beth #$test.joe #$test.john))
  :comparison 'equal)
  ;; Test that non-list gets converted to list if so specified
#-:sframes
(defframetest set-valued-3
  (let ((*set-valued-non-list-action* :convert-silently))
    (setf (slotv #$test.jeff #$test.siblings) #$Test.Mary)
    (slotv #$test.jeff #$test.siblings))
  (list #$Test.Mary)
  :comparison 'equal)
  ;; Test that non-list errors out if so specified.
#-:sframes
(defframetest set-valued-4
  (let ((*set-valued-non-list-action* :error))
    (handler-case
        (progn (setf (slotv #$test.jeff #$test.siblings) #$Test.Mary) nil)
      (error () t)))
  t)

#-:sframes
(defframetest indexed-1
  (let ((*set-valued-non-list-action* :convert-silently))
    (let ((ht (slotv #$Test.Portfolio #$hashtable)))
      (when (hash-table-p ht) (clrhash ht))
      (setf (slotv #$test.Massar #$Test.Portfolio) (list "COMPAQ" "IBM" "DELL"))
      (setf (slotv #$Test.Shrager #$Test.Portfolio)
            (list "DELL" "APPLE" "JETBLUE"))
      (setf (slotv #$Test.Travers #$Test.Portfolio) "FORD")
      (add-element #$Test.Travers #$Test.Portfolio "DELL" :test 'string=)
      ;; Since index test is EQUAL, not EQUALP, this will not index with
      ;; "APPLE"
      (add-element #$Test.Travers #$Test.Portfolio "Apple" :test 'string=)
      (mapcar 'sort-frames-by-fname
              (list
               (slot-lookup "General Motors" #$Test.Portfolio)
               (slot-lookup "DELL" #$Test.Portfolio)
               (slot-lookup "APPLE" #$Test.Portfolio)
               ))))
  (mapcar 'sort-frames-by-fname
          (list nil 
                (list #$Test.Massar #$Test.Shrager #$Test.Travers)
                (list #$Test.Shrager)))
  :comparison 'equal)

#-:sframes
(defframetest indexed-2
  (progn
    (let ((ht (slotv #$Test.Bestpick #$hashtable)))
      (when (hash-table-p ht) (clrhash ht)))
    (setf (slotv #$Test.Massar #$Test.Bestpick) "DELL")
    (setf (slotv #$Test.Shrager #$Test.Bestpick) "DELL")
    (setf (slotv #$Test.Travers #$Test.Bestpick) "FORD")
    (mapcar 'sort-frames-by-fname
            (list
             (slot-lookup "General Motors" #$Test.Bestpick)
             (slot-lookup "DELL" #$Test.Bestpick)
             (slot-lookup "FORD" #$Test.Bestpick)
             )))
  (mapcar 'sort-frames-by-fname
          (list nil (list #$Test.Massar #$Test.Shrager) (list #$Test.Travers)))
  :comparison 'equal)
  
  
(defframetest 
    search-frames-1 
  (with-temp-frames (f '("abcdefg" "abcdefgh" "abcdefghi"))
    (let ((results 
           #-:sframes
           (search-frames "abcdef")
           #+:sframes
           (search-frames "abcdef" t)
           ))
      (= 3 (length (intersection results f)))))
  t
  )

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
            (if (consp value) (first value) 0)
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

(defframetest 
    for-each-1
  (let ((count 0))
    (for-each-frame-slot (slot value) #$test.xyzzy
      (incf count))
    count
    )
  (let ((len (length (frames::generic-frame-slots #$test.xyzzy))))
    #+:sframes (1+ len) 
    #-:sframes len
    ))

(defframetest 
    for-each-2
  (let ((count 0))
    (for-each-frame-slot (slot) #$test.xyzzy
      (incf count))
    count
    )
  (let ((len (length (frames::generic-frame-slots #$test.xyzzy))))
    #+:sframes (1+ len) 
    #-:sframes len
    ))

(defframetest 
    for-each-3
  (let ((slist nil))
    (for-each-frame-slot (slot) #$test.xyzzy
      (unless (eq slot #$fname) (push slot slist)))
    (utils::set-equal 
     slist 
     (remove #$fname (mapcar 'car (frames::generic-frame-slots #$test.xyzzy)))
     ))
  t
  )
