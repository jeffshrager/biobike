(in-package :wb)

(defssvtest=
 update0
 (progn
   (wb::test-sseqview 
    '(("org" . "Mark") ("contig" . "Mark.chromosome") ("goto" . "") 
      ("submit" . "Go") ("search" . "") ("PKG" . "DOCUSER0"))
    :docuser0
    )
   t)
 t)

(defssvtest=
 ss1
 (let* ((line (first wb::*linemap*))
        (num (wb::svline-num line))
        (seg (first (wb::svline-segments line))))
   (and 
    line
    (= num 1)
    (eq (wb::svsegment-start seg) 1)
    (eq (wb::svsegment-end seg) 60)
    (eq (wb::svsegment-color seg) :black)
    (eq (wb::svsegment-genes seg) nil)
    ))
 t
 )

(defssvtest=
 ss2
 (let* ((line (lisp:second wb::*linemap*))
        (num (wb::svline-num line))
        (seg (first (wb::svline-segments line))))
   (and
    line
    (= num 61)
    (eq (wb::svsegment-start seg) 61)
    (eq (wb::svsegment-end seg) 120)
    (or (eq (wb::svsegment-color seg) :red)
        (eq (wb::svsegment-color seg) :magenta))
    (eq (car (wb::svline-links line)) #$Mark.marka1)
    (eq (car (wb::svline-genes line)) #$Mark.marka1)
    ))
 t
 )

(defssvtest=
 ss3
 (let* ((line (lisp:third wb::*linemap*))
        (num (wb::svline-num line))
        (segs (wb::svline-segments line))
        (seg1 (lisp:first segs))
        (seg2 (lisp:second segs)))
   (and
    line
    (= num 121)
    (and 
     (= (wb::svsegment-start seg1) 121) 
     (= (wb::svsegment-end seg1) 139)
     (eq (wb::svsegment-color seg1) :black)
     (null (wb::svsegment-genes seg1)))
    (and
     (= (wb::svsegment-start seg2) 140)
     (= (wb::svsegment-end seg2) 180)
     (or (eq (wb::svsegment-color seg2) :red)
         (eq (wb::svsegment-color seg2) :magenta))
     (eq (car (wb::svsegment-genes seg2)) #$Mark.marka2))
    (eq (car (wb::svline-links line)) #$Mark.marka2)
    ))
 t
 )

(defssvtest=
 ss4
 (let* ((line (lisp:fourth wb::*linemap*))
        (num (wb::svline-num line))
        (segs (wb::svline-segments line))
        (seg1 (lisp:first segs))
        (seg2 (lisp:second segs))
        (seg3 (lisp:third segs))
        (seg4 (lisp:fourth segs))
        (seg5 (lisp:fifth segs))
        )
   (and
    line
    (= num 181)
    (and 
     (= (wb::svsegment-start seg1) 181)
     (= (wb::svsegment-end seg1) 210)
     (or (eq (wb::svsegment-color seg1) :magenta)
         (eq (wb::svsegment-color seg1) :red))
     (eq (car (wb::svsegment-genes seg1)) #$Mark.marka2))
    (and 
     (= (wb::svsegment-start seg2) 211)
     (= (wb::svsegment-end seg2) 221)
     (eq (wb::svsegment-color seg2) :black)
     (null (wb::svsegment-genes seg2)))
    (and
     (= (wb::svsegment-start seg3) 222)
     (= (wb::svsegment-end seg3) 228)
     (or (eq (wb::svsegment-color seg3) :magenta)
         (eq (wb::svsegment-color seg3) :red))
     (eq (car (wb::svsegment-genes seg3)) #$Mark.marka3))
    (and
     (= (wb::svsegment-start seg4) 229)
     (= (wb::svsegment-end seg4) 239)
     (eq (wb::svsegment-color seg4) :orange)
     (and (eq (lisp:first (wb::svsegment-genes seg4)) #$Mark.marka3)
          (eq (lisp:second (wb::svsegment-genes seg4)) #$Mark.marka4)))
    (and 
     (= (wb::svsegment-start seg5) 240)
     (= (wb::svsegment-end seg5) 240)
     (or (eq (wb::svsegment-color seg5) :red)
         (eq (wb::svsegment-color seg5) :magenta))
     (eq (car (wb::svsegment-genes seg5)) #$Mark.marka4))
    ))
 t
 )
        

(defssvtest=
 ss5
 (let* ((line (lisp:fifth wb::*linemap*))
        (num (wb::svline-num line))
        (segs (wb::svline-segments line))
        (seg1 (lisp:first segs)))
   (and 
    line
    (= num 241)
    (= (wb::svsegment-start seg1) 241)
    (= (wb::svsegment-end seg1) 300)
    (eq (wb::svsegment-color seg1) :black)
    (null (wb::svsegment-genes seg1))
    ))
 t
 )

(defssvtest=
 ss6
 (let* ((line (lisp:sixth wb::*linemap*))
        (num (wb::svline-num line))
        (segs (wb::svline-segments line))
        (seg1 (lisp:first segs))
        (seg2 (lisp:second segs))
        (seg3 (lisp:third segs)))
   (and 
    line
    (= num 301)
    (and 
     (= (wb::svsegment-start seg1) 301)
     (= (wb::svsegment-end seg1) 329)
     (or (eq (wb::svsegment-color seg1) :blue)
         (eq (wb::svsegment-color seg1) :green))
     (eq (car (wb::svsegment-genes seg1)) #$Mark.marka5))
    (and 
     (= (wb::svsegment-start seg2) 330)
     (= (wb::svsegment-end seg2) 350)
     (eq (wb::svsegment-color seg2) :violet)
     (and (eq (lisp:first (wb::svsegment-genes seg2)) #$Mark.marka5)
          (eq (lisp:second (wb::svsegment-genes seg2)) #$Mark.marka6)))
    (and 
     (= (wb::svsegment-start seg3) 351)
     (= (wb::svsegment-end seg3) 360)
     (or (eq (wb::svsegment-color seg3) :blue)
         (eq (wb::svsegment-color seg3) :green))
     (eq (lisp:first (wb::svsegment-genes seg3)) #$Mark.marka6))
    ))
 t
 )

(defssvtest=
 ss7
 (let* ((line (lisp:seventh wb::*linemap*))
        (num (wb::svline-num line))
        (segs (wb::svline-segments line))
        (seg1 (lisp:first segs)))
   (and 
    line
    (= num 361)
    (= (wb::svsegment-start seg1) 361)
    (= (wb::svsegment-end seg1) 420)
    (eq (wb::svsegment-color seg1) :black)
    (null (wb::svsegment-genes seg1))
    ))
 t
 )

(defssvtest=
 ss8
 (let* ((line (lisp:eighth wb::*linemap*))
        (num (wb::svline-num line))
        (segs (wb::svline-segments line))
        (seg1 (lisp:first segs))
        (seg2 (lisp:second segs))
        (seg3 (lisp:third segs)))
   (and
    line
    (= num 421)
    (and 
     (= (wb::svsegment-start seg1) 421)
     (= (wb::svsegment-end seg1) 434)
     (or (eq (wb::svsegment-color seg1) :red)
         (eq (wb::svsegment-color seg1) :magenta))
     (eq (lisp:first (wb::svsegment-genes seg1)) #$Mark.marka7))
    (and 
     (= (wb::svsegment-start seg2) 435)
     (= (wb::svsegment-end seg2) 440)
     (eq (wb::svsegment-color seg2) :purple)
     (and (eq (lisp:first (wb::svsegment-genes seg2)) #$Mark.marka7)
          (eq (lisp:second (wb::svsegment-genes seg2)) #$Mark.marka8)))
    (and 
     (= (wb::svsegment-start seg3) 441)
     (= (wb::svsegment-end seg3) 480)
     (or (eq (wb::svsegment-color seg3) :blue)
         (eq (wb::svsegment-color seg3) :green))
     (eq (lisp:first (wb::svsegment-genes seg3)) #$Mark.marka8))
    ))
 t
 )

(defssvtest=
 ss9
 (let* ((line (lisp:ninth wb::*linemap*))
        (num (wb::svline-num line))
        (segs (wb::svline-segments line))
        (seg1 (lisp:first segs)))
   (and
    line
    (= num 481)
    (and 
     (= (wb::svsegment-start seg1) 481)
     (= (wb::svsegment-end seg1) 540)
     (or (eq (wb::svsegment-color seg1) :blue)
         (eq (wb::svsegment-color seg1) :green))
     (eq (lisp:first (wb::svsegment-genes seg1)) #$Mark.marka8))
    ))
 t
 )

(defssvtest=
 ss10
 (let* ((line (lisp:tenth wb::*linemap*))
        (num (wb::svline-num line))
        (segs (wb::svline-segments line))
        (seg1 (lisp:first segs)))
   (and
    line
    (= num 541)
    (and 
     (= (wb::svsegment-start seg1) 541)
     (= (wb::svsegment-end seg1) 600)
     (or (eq (wb::svsegment-color seg1) :blue)
         (eq (wb::svsegment-color seg1) :green))
     (eq (lisp:first (wb::svsegment-genes seg1)) #$Mark.marka8))
    ))
 t
 )

(defssvtest=
 ss11
 (let* ((line (lisp:elt wb::*linemap* 10))
        (num (wb::svline-num line))
        (segs (wb::svline-segments line))
        (seg1 (lisp:first segs))
        (seg2 (lisp:second segs))
        (seg3 (lisp:third segs)))
   (and
    line
    (= num 601)
    (and 
     (= (wb::svsegment-start seg1) 601)
     (= (wb::svsegment-end seg1) 620)
     (or (eq (wb::svsegment-color seg1) :green)
         (eq (wb::svsegment-color seg1) :blue))
     (eq (lisp:first (wb::svsegment-genes seg1)) #$Mark.marka9))
    (and 
     (= (wb::svsegment-start seg2) 621)
     (= (wb::svsegment-end seg2) 629)
     (eq (wb::svsegment-color seg2) :black)
     (null (wb::svsegment-genes seg2)))
    (and 
     (= (wb::svsegment-start seg3) 630)
     (= (wb::svsegment-end seg3) 660)
     (or (eq (wb::svsegment-color seg3) :red)
         (eq (wb::svsegment-color seg3) :magenta))
     (eq (lisp:first (wb::svsegment-genes seg3)) #$Mark.marka10))
    ))
 t
 )
     
(defssvtest=
 ss12
 (let* ((line (lisp:elt wb::*linemap* 11))
        (num (wb::svline-num line))
        (segs (wb::svline-segments line))
        (seg1 (lisp:first segs))
        (seg2 (lisp:second segs)))
   (and 
    line 
    (= num 661)
    (and 
     (= (wb::svsegment-start seg1) 661)
     (= (wb::svsegment-end seg1) 680)
     (or (eq (wb::svsegment-color seg1) :blue)
         (eq (wb::svsegment-color seg1) :green))
     (eq (lisp:first (wb::svsegment-genes seg1)) #$Mark.marka11))
    (and 
     (= (wb::svsegment-start seg2) 681)
     (= (wb::svsegment-end seg2) 720)
     (or (eq (wb::svsegment-color seg2) :blue)
         (eq (wb::svsegment-color seg2) :green))
     (eq (lisp:first (wb::svsegment-genes seg2)) #$Mark.marka12))
    ))
 t
 )

(defssvtest=
 ss13
 (let* ((line (lisp:elt wb::*linemap* 12))
        (num (wb::svline-num line))
        (segs (wb::svline-segments line))
        (seg1 (lisp:first segs)))
   (and
    line
    (= num 721)
    (= (wb::svsegment-start seg1) 721)
    (= (wb::svsegment-end seg1) 780)
    (eq (wb::svsegment-color seg1) :black)
    (null (wb::svsegment-genes seg1))
    ))
 t
 )

(defssvtest=
 update1
 (progn
   (wb::test-sseqview 
    '(("org" . "Mark") ("contig" . "Mark.Nextcontig") ("goto" . "") 
      ("submit" . "Go") ("search" . "") ("PKG" . "DOCUSER0"))
    :docuser0
    )
   t)
 t)

(defssvtest=
 ss14
 (let* ((line (lisp:first wb::*linemap*))
        (num (wb::svline-num line))
        (segs (wb::svline-segments line))
        (seg1 (lisp:first segs))
        (seg2 (lisp:second segs)))
   (and
    line 
    (= num 1)
    (and
     (= (wb::svsegment-start seg1) 1)
     (= (wb::svsegment-end seg1) 40)
     (eq (wb::svsegment-color seg1) :purple)
     (and (eq (lisp:first (wb::svsegment-genes seg1)) #$Mark.markb1)
          (eq (lisp:second (wb::svsegment-genes seg1)) #$Mark.markb2))
     )
    (and
     (= (wb::svsegment-start seg2) 41)
     (= (wb::svsegment-end seg2) 60)
     (or (eq (wb::svsegment-color seg2) :red)
         (eq (wb::svsegment-color seg2) :magenta))
     (eq (lisp:first (wb::svsegment-genes seg2)) #$Mark.markb1)
     )))
 t
 )

(defssvtest=
 ss15
 (let* ((line (lisp:seventh wb::*linemap*))
        (num (wb::svline-num line))
        (segs (wb::svline-segments line))
        (seg1 (lisp:first segs))
        (seg2 (lisp:second segs)))
   (and
    line 
    (= num 361)
    (and
     (= (wb::svsegment-start seg1) 361)
     (= (wb::svsegment-end seg1) 379)
     (or (eq (wb::svsegment-color seg1) :red)
         (eq (wb::svsegment-color seg1) :magenta))
     (eq (lisp:first (wb::svsegment-genes seg1)) #$Mark.markb1))
    (and
     (= (wb::svsegment-start seg2) 380)
     (= (wb::svsegment-end seg2) 420)
     (eq (wb::svsegment-color seg2) :purple)
     (and (eq (lisp:first (wb::svsegment-genes seg2)) #$Mark.markb1)
          (eq (lisp:second (wb::svsegment-genes seg2)) #$Mark.markb2)))
    ))
 t
 )
     
(defssvtest=
 update2
 (progn
   (wb::test-sseqview 
    '(("org" . "Mark") ("contig" . "Mark.longcontig") ("goto" . "") 
      ("submit" . "Go") ("search" . "") ("PKG" . "DOCUSER0"))
    :docuser0
    )
   t)
 t)

(defssvtest=
 ss16
 (let* ((f (lisp:first wb::*linemap*))
        (fseg (lisp:first (wb::svline-segments f)))
        (fstart (wb::svsegment-start fseg))
        (l (utils::lastelem wb::*linemap*))
        (lseg (lisp:first (wb::svline-segments l)))
        (lend (wb::svsegment-end lseg)))
   (and (= fstart 1) (= lend 6000))
   )
 t
 )

(defssvtest=
 update3
 (progn
   (wb::test-sseqview 
    '(("org" . "Mark") ("contig" . "Mark.longcontig") ("goto" . "") 
      ("submit" . "Go") ("search" . "") ("PKG" . "DOCUSER0") ("next" . "y"))
    :docuser0
    )
   t)
 t)

(defssvtest=
 ss17
 (let* ((f (lisp:first wb::*linemap*))
        (fseg (lisp:first (wb::svline-segments f)))
        (fstart (wb::svsegment-start fseg))
        (l (utils::lastelem wb::*linemap*))
        (lseg (lisp:first (wb::svline-segments l)))
        (lend (wb::svsegment-end lseg)))
   (and (= fstart 5881) (= lend 11880))
   )
 t)

(defssvtest=
 update4
 (progn
   (wb::test-sseqview 
    '(("org" . "Mark") ("contig" . "Mark.longcontig") ("goto" . "") 
      ("submit" . "Go") ("search" . "") ("PKG" . "DOCUSER0") ("start" . "y"))
    :docuser0
    )
   t)
 t)

(defssvtest=
 ss18
 (let* ((f (lisp:first wb::*linemap*))
        (fseg (lisp:first (wb::svline-segments f)))
        (fstart (wb::svsegment-start fseg))
        (l (utils::lastelem wb::*linemap*))
        (lseg (lisp:first (wb::svline-segments l)))
        (lend (wb::svsegment-end lseg)))
   (and (= fstart 1) (= lend 6000))
   )
 t)

(defssvtest=
 update5
 (progn
   (wb::test-sseqview 
    '(("org" . "Mark") ("contig" . "Mark.longcontig") ("goto" . "") 
      ("submit" . "Go") ("search" . "") ("PKG" . "DOCUSER0") ("end" . "y"))
    :docuser0
    )
   t)
 t)

(defssvtest=
 ss19
 (let* ((f (lisp:first wb::*linemap*))
        (fseg (lisp:first (wb::svline-segments f)))
        (fstart (wb::svsegment-start fseg))
        (l (utils::lastelem wb::*linemap*))
        (lseg (lisp:first (wb::svline-segments l)))
        (lend (wb::svsegment-end lseg)))
   (and (= fstart 12121) (= lend 18120))
   )
 t)

(defssvtest=
 update6
 (progn
   (wb::test-sseqview 
    '(("org" . "Mark") ("contig" . "Mark.longcontig") ("goto" . "") 
      ("submit" . "Go") ("search" . "") ("PKG" . "DOCUSER0") ("start" . "y"))
    :docuser0
    )
   t)
 t)

(defssvtest=
 ss20
 (let* ((f (lisp:first wb::*linemap*))
        (fseg (lisp:first (wb::svline-segments f)))
        (fstart (wb::svsegment-start fseg))
        (l (utils::lastelem wb::*linemap*))
        (lseg (lisp:first (wb::svline-segments l)))
        (lend (wb::svsegment-end lseg)))
   (and (= fstart 1) (= lend 6000))
   )
 t)

(defssvtest=
 update7
 (progn
   ;; go to start
   (wb::test-sseqview 
    '(("org" . "Mark") ("contig" . "Mark.longcontig") ("goto" . "") 
      ("submit" . "Go") ("search" . "") ("PKG" . "DOCUSER0") ("start" . "y"))
    :docuser0
    )
   ;; click next once
   (wb::test-sseqview 
    '(("org" . "Mark") ("contig" . "Mark.longcontig") ("goto" . "") 
      ("submit" . "Go") ("search" . "") ("PKG" . "DOCUSER0") ("next" . "y"))
    :docuser0
    )
   ;; click next again
   (wb::test-sseqview 
    '(("org" . "Mark") ("contig" . "Mark.longcontig") ("goto" . "") 
      ("submit" . "Go") ("search" . "") ("PKG" . "DOCUSER0") ("next" . "y"))
    :docuser0
    )
   t)
 t)

(defssvtest=
 ss21
 (let* ((f (lisp:first wb::*linemap*))
        (fseg (lisp:first (wb::svline-segments f)))
        (fstart (wb::svsegment-start fseg))
        (l (utils::lastelem wb::*linemap*))
        (lseg (lisp:first (wb::svline-segments l)))
        (lend (wb::svsegment-end lseg)))
   (and (= fstart 11761) (= lend 17760))
   )
 t)

(defssvtest=
 update8
 (progn
   ;; now click next again to wrap the boundary
   (wb::test-sseqview 
    '(("org" . "Mark") ("contig" . "Mark.longcontig") ("goto" . "") 
      ("submit" . "Go") ("search" . "") ("PKG" . "DOCUSER0") ("next" . "y"))
    :docuser0
    )
   t)
 t)

(defssvtest=
 ss22
 (let* ((f (lisp:first wb::*linemap*))
        (fseg (lisp:first (wb::svline-segments f)))
        (fstart (wb::svsegment-start fseg))
        (l (utils::lastelem wb::*linemap*))
        (lseg (lisp:first (wb::svline-segments l)))
        (lend (wb::svsegment-end lseg)))
   (and (= fstart 17641) (= lend 5520))
   )
 t)

;; go to 2000
(defssvtest=
 update9
 (progn
   (wb::test-sseqview 
    '(("org" . "Mark") ("contig" . "Mark.longcontig") ("goto" . "2000") 
      ("submit" . "Go") ("search" . "") ("PKG" . "DOCUSER0"))
    :docuser0
    )
   t)
 t)

(defssvtest=
 ss23
 (let* ((f (lisp:first wb::*linemap*))
        (fseg (lisp:first (wb::svline-segments f)))
        (fstart (wb::svsegment-start fseg))
        (l (utils::lastelem wb::*linemap*))
        (lseg (lisp:first (wb::svline-segments l)))
        (lend (wb::svsegment-end lseg)))
   (and (= fstart 2000) (= lend 7999) )
   )
 t)

;; testing search
(defssvtest=
 update10
 (progn
   (wb::test-sseqview 
    '(("org" . "Mark") ("contig" . "Mark.longcontig") ("goto" . "") 
      ("submit" . "Go") ("search" . "ATGTATCGT") ("PKG" . "DOCUSER0"))
    :docuser0
    )
   t)
 t)
    
(defssvtest=
 ss24
 (let* ((f (lisp:first wb::*linemap*))
        (seg1 (lisp:first (wb::svline-segments f)))
        (seg2 (lisp:second (wb::svline-segments f)))
        (seg1start (wb::svsegment-start seg1))
        (seg1end (wb::svsegment-end seg1))
        (seg2start (wb::svsegment-start seg2))
        (seg2end (wb::svsegment-end seg2))
        (seg1pat (wb::svsegment-pattern-match? seg1))
        (seg2pat (wb::svsegment-pattern-match? seg2)))
   (and 
    (= seg1start 1)
    (= seg1end 9)
    (= seg2start 10)
    (= seg2end 60)
    seg1pat
    (not seg2pat)
    ))
 t
 )
                   





     
    
 
    

    
    
     
     


  
             
            


    
    
    

   
