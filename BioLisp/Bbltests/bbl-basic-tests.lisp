;; -*- Package: bbl-test-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bbl-test-user) 

;;; +=========================================================================+
;;; | copyright (c) 2005 jp massar, jeff elhai, mark slupesky, peter seibel   |
;;; |                                                                         |
;;; | permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "software"), to deal in the software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the software, and to      |
;;; | permit persons to whom the software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | the above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the software.                  |
;;; |                                                                         |
;;; | the software is provided "as is", without warranty of any kind,         |
;;; | express or implied, including but not limited to the warranties of      |
;;; | merchantability, fitness for a particular purpose and noninfringement.  |
;;; | in no event shall the authors or copyright holders be liable for any    |
;;; | claim, damages or other liability, whether in an action of contract,    |
;;; | tort or otherwise, arising from, out of or in connection with the       |
;;; | software or the use or other dealings in the software.                  |
;;; +=========================================================================+

;;; author: jp massar, mark slupesky.  

;; IF-TRUE

;; test an easy then form
(defbbltest iftrue-1 
            (if-true t then 5 else 6)
            5)

;; test an easy ELSE form
(defbbltest iftrue-2
            (if-true nil then 5 else 6)
            6)

;; multiple args in THEN clause, return last
(defbbltest iftrue-3
            (if-true 5 then 5 6 else 6 12 7)
            6)

;; multiple args in ELSE clause, return last
(defbbltest iftrue-4
             (if-true nil then 5 6 else 6 7)
            7)

;; no else clause (default nil)
(defbbltest iftrue-5
             (if-true nil then 5 6)
            nil)

#|    THEN now required
;; no then clause (default nil)
(defbbltest iftrue-6            
             (if-true t else 8)
            nil)
|#

;; empty then clause
(defbbltest iftrue-7            
             (if-true t then else 5)
            nil)

;; empty else clause
(defbbltest iftrue-8            
            (if-true nil then 6 else)
            nil)

#| THEN now required
;; can handle no 'then'
;; the converse is semantically blocked
(defbbltest iftrue-10            
             (if-true t 5 6 else 4)
            6)
|#


;; IF-TRUE

;; test an easy then form
(defbbltest iffalse-1 
            (if-false t then 5 else 6)
            6)

;; test an easy ELSE form
(defbbltest iffalse-2
            (if-false nil then 5 else 6)
            5)

;; multiple args in THEN clause, return last
(defbbltest iffalse-3
            (if-false nil then 5 6 else 6 12 7)
            6)

;; multiple args in ELSE clause, return last
(defbbltest iffalse-4
            (if-false t then 5 6 else 6 7)
            7)

;; no else clause (default nil)
(defbbltest iffalse-5
             (if-false t then 5 6)
             nil)

#| THEN now required
;; no then clause (default nil)
(defbbltest iffalse-6            
             (if-false nil else 8)
            nil)
|#

;; empty then clause
(defbbltest iffalse-7            
             (if-false nil then else 5)
            nil)

;; empty else clause
(defbbltest iffalse-8            
            (if-false t then 6 else)
            nil)

#| THEN now required
;; can handle no 'then'
;; the converse is semantically blocked
(defbbltest iffalse-10            
            (if-false nil 5 6 else 4)
            6)
|#

;; LAST-N

;; simple case
(defbbltest=
 lastn-1 
  (last-n 7 (utils::iota 12))
 '(5 6 7 8 9 10 11))

;; passing 0 returns nil
(defbbltest
 lastn-2 
  (last-n 0 (utils::iota 12))
 nil)

;; first-arg > (length second-arg), return second-arg
(defbbltest=
 lastn-3 
  (last-n 20 (utils::iota 12))
 (utils::iota 12))


;; MAX-OF
;; max-of and min-of virtually identical 

;; simple case
;; why did i do the funcall? xxx
(defbbltest=
 max1 
  (let ((a (list 2 4 8 16 9 7)))
    (list 
     (cl:apply 'max-of a)
     (cl:apply 'min-of a)
     (max-of a)))
 '(16 2 16))

;; max-of must be passed a list
(defbblerrortest=
 e-max-1 
  (max-of)
 error) 

;; ASSIGN

;; one var, one val
(defbbltest assign-1              
             (let ((x 5))
               (bbi::assign x 6)
               x)
            6)

;; one var whose val is a list
(defbbltest= assign-2              
              (let ((x 5))
                (bbi::assign x '(9 12))
                x)
             '(9 12))

;; multiple vars to multiple vals
(defbbltest= 
 assign-3 
  (let ((x 1) (y 2) (z 3))
    (bbi::assign (x y z) '(5 6 7))
    (list z y x))
 '(7 6 5))

;; multiple vars, one val
(defbbltest= assign-4             
              (let ((a 5) (b 6) (c nil) (d nil))
                (bbi::assign (d c) = b)
                (list a b c d))
             '(5 6 6 6)) 

;; more complex of above
(defbbltest= assign-5              
              (let ((a 52) (b '(19 20 21 22 23 24)))
                (bbi::assign (a (ref b 3)) = 77)
                (cons a b))
             '(77 19 20 77 22 23 24))

;; assign in a degenerate list xxx
(defbbltest assign-6             
             (let ((x 5))
               (bbi::assign (x) 6)
               x)
            6)

;; multiple assignment
(defbbltest= assign-7              
              (let ((x 5) (y 6))
                (bbi::assign x = 1 y = 2)
                (list x y))
             '(1 2))

;; dependent assignment
(defbbltest= assign-8a              
              (let ((x 12) (y 17))
                (bbi::assign x = y y = 8)
                (list x y))
             '(17 8))

;; assignment to multiple variables given multiple return values
(defbbltest= assign-8b              
              (let (a b c)
                (bbi::assign (a b c) (values 1 2 3))
                (list a b c))
             '(1 2 3))

;; assignment to multiple variables given a single return value which is a list
(defbbltest= assign-9              
              (let (a b c)
                (bbi::assign (a b c) (list 1 2 3))
                (list a b c))
             '(1 2 3))

;; assignment to multiple variables given not enough multiple return values
(defbbltest= assign-10              
              (let (a b c)
                (bbi::assign (a b c) (values 1 2))
                (list a b c))
             '(1 2 nil))

;; assignment to multiple variables given a single list return value which 
;; doesn't have enough elements
(defbbltest= assign-10a              
              (let (a b c)
                (bbi::assign (a b c) (list 1 2))
                (list a b c))
             '(1 2 nil))

;; assignment to multiple variables given surplus multiple return values
(defbbltest= assign-11              
              (let (a b c)
                (bbi::assign (a b c) (values 1 2 3 4))
                (list a b c))
             '(1 2 3))

;; check can't assign to a constant variable
(defbblerrortest=
 e-assign1
 (lisp:eval 
  '(bbi::assign pi 200))
 error)


;; can't assign to a random constant
(defbblerrortest= 
 e-assign2
 (lisp:eval
  '(bbi::assign 5 6))
 error)

;; INCREMENT

;; basic increment
(defbbltest increment-1             
             (let ((x 88))
               (bbi::increment x)
               x)
            89)

;; increment with by keyword
(defbbltest increment-2             
             (let ((x 88) (h 100))
               (bbi::decrement x by h)
               x)
            -12)

;; trying things together
(defbbltest= increment-3              
              (mapcar (lambda (x) 
                        (if (oddp x) (bbi::decrement x by 2) (bbi::increment x by 2)))
                      (utils:iota 10))
             '(2 -1 4 1 6 3 8 5 10 7))
 
;; increment of garray with values
(defbbltest increment-4            
             (let ((g (utils:make-garray '(3 3) :initial-element 9)))
               (bbi::decrement (ref g 1 1) by 14))
            -5)
    
;; increment of non-existent garray
(defbbltest increment-5       
            (progn
             (lisp:makunbound '*g*)
             (bbi::decrement (ref *g* 1 1) by 14))
            -14)

;; increment of multiple variables with side-effecting 'by' form
(defbbltest= increment-6              
              (let ((x 88) (h 100) (z 1))
                (bbi::decrement (x h) by (incf z))
                (list x h z))
             '(86 98 2))  ;; added z

;; increment multiple places and test that NIL values get treated as 0
(defbbltest= increment-7              
              (let ((x (list 1 2 3)) 
                    (y (vector 1 2 3))
                    (z #$test.fred))
                (setf (ref #$test.fred #$test.wilma) nil)
                (bbi::decrement ((ref x 2) (ref y 3) (ref z #$test.wilma)))
                (list (ref x 2) (ref y 3) (ref z #$test.wilma)))
             '(1 2 -1))

;; put a bunch of stuff together, use a test frame
(defbbltest bbl-forms             
             (let ((a (list 2 4 6 8))
                   (b (vector 'alpha 'beta 'gamma))
                   (c (utils:make-garray '(3 3) :initial-element 12))
                   (d #$test.leonardo))
               (bbi::decrement ((ref a 2) (ref a 3)))
               (bbi::assign ((ref a 4) (ref a 1)) 162)
               (bbi::increment (ref a 4) by 100)
               (bbi::assign (ref b 3) 6)
               (bbi::assign (ref c 1 2) 54)
               (setf (ref d #$test.sword) nil)
               (bbi::increment (ref d #$test.sword) by 12)
               (and 
                (equal a '(162 3 5 262))
                (= (ref b 3) 6)
                (= (ref c 1 2) 54)
                (= (ref #$test.leonardo #$test.sword) 12)))
            t)     
   
  
;; SEQUENCE-OF

;; invert both inverts and complements
;; from-end affects the start while to-end affects the end 
;; as far as we can determine, from and from-end are mutually exclusive
;; as are to, to-end, and length 
;; if mutually exclusive flags are provided, no warning or error is signalled
;; from takes priority over from-end while to takes priority over to-end
;; which takes priority over length

;; simple string
(defbbltest= 
 seqof-1            
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz")
 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

;; from keyword
(defbbltest=
 seqof-2            
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" from 14)
 "NOPQRSTUVWXYZ")

;; to keyword
(defbbltest=
 seqof-3  
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" to 8)
 "ABCDEFGH")

;; ok from and to
(defbbltest=
 seqof-4 
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" :from-start 2 to-start 12)
 "BCDEFGHIJKL")

;; still returns length 1
(defbbltest=
 seqof-5 
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" from 7 to 7)
 "G")

;; from > (length string)
(defbblerrortest=
 e-seqof-1            
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" :from 50)
 error)

;; nonsense to keyword   
(defbblerrortest=
 e-seqof-2            
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" from 1 to -20)
 error)

;; test truncate keyword
(defbbltest=
 seqof-6 
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" from -10 truncate)
 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

;; to = 0
;; jeff e needs to tell us that this it not an error
(defbblerrortest=
 e-seqof-3            
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" :to 0)
 error)


;; reversed from to
(defbblerrortest=
 e-seqof-4
 (lisp:eval
  '(bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" from 20 to 10))
 error)

 
;; ok length
(defbbltest=
 seqof-6 
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" length 25)
 "ABCDEFGHIJKLMNOPQRSTUVWXY")

;; ok from length
(defbbltest=
 seqof-7 
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" from 6 length 8) "FGHIJKLM")

;; length-keyword > (length seq)
(defbbltest=
 seqof-7x
 (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" from 6 length 88)
 "FGHIJKLMNOPQRSTUVWXYZ")

;; from length and truncate, contrast with above
(defbbltest=
 seqof-7a 
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" from 6 length 88 truncate)
 "FGHIJKLMNOPQRSTUVWXYZ")

;; bad length
(defbblerrortest=
 e-seqof-5            
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" from 6 length 0)
 error)
;; should we warn that using TO makes any value passed to LENGTH meaningless
 
;; ok invert
(defbbltest=
 seqof-9 
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" invert)
 "ZRXWBUASYQPONKLMJIDCFEHGVT")
 
;; from invert
(defbbltest=
 seqof-10 
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" from 18 invert)
 "ZRXWBUASY")

;; to inver
(defbbltest=
 seqof-11 
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" to 6 invert)
 "FEHGVT")

;; from to invert
(defbbltest=
 seqof-12 
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" from 14 to 19 invert)
 "SYQPON")

;; try a fixnum instead of an integer
(defbbltest=
 seqof-13 
  (bbi::sequence-of "abcdefghijklmnopqrstuvwxyz" from 14.0 length 2 invert)
 "ON")

(defbbltest=
 seqof-14
 (bbi::sequence-of "a b 1 2 c d" from 1 to 4)
 "ABCD"
 )

(defbbltest=
 seqof-15
 (bbi::sequence-of "a b 1 2 c d" from 2 to 3)
 "BC"
 )

(defbbltest= 
  seqof-16
  (bbi::sequence-of "")
  ""
  )


;;; DEFINE

;; just one value with a token
(defbbltest
 define-1 
  (let ((test-u 19))
    #+:lispworks (lisp:declare (lisp:special test-u))
    (define test-u as 3)
    test-u)
 3)

;; multiple values, display-off
(defbbltest=
 define-2 
  (let ((test-a 12) (test-b 13) (test-c 14))
    (list 
     (define (test-a test-b test-c) (values 4 5 6) display-off)
     test-a test-b test-c))
 '("Results suppressed" 4 5 6))

;; multiple and dependant defines involving ref 
(defbbltest=
 define-3 
  (let ((test-a '(1 2 3 4 5)) 
        (test-b '(10 10 10))
        (test-c 22))
    (define ((ref test-a 4) (ref test-b 1) test-c) 
            (values (ref test-b 2) test-c 7))
    (list test-a test-b test-c))
 '((1 2 3 10 5) (22 10 10) 7))

;; undefined sequence 
(defbbltest
 define-4  
 (progn
   (define (ref nothing 1 2 3) 962)
   (utils::gref nothing 1 2 3))
 962)
 
  
;; a constant
(defbblerrortest=
 e-define-1
 (lisp:eval
  '(define pi 200))
 error)

;; another constant 
(defbblerrortest=
 e-define-2
 (lisp:eval
  '(bbi::assign 5 6))
 error)

;; were implicitly creating a garray by calling ref with an uninterned symbol and
;; 2 nums xxx
(defbbltest
 define-5
  (let ((x 2))
    (define (ref nothing2 (incf x) x) = 10)
    (ref nothing2 3 3))
 10)

(defbbltest=
    define-6
  (let ((test-v 19))
    #+:lispworks (lisp:declare (lisp:special test-v))
    (progn
      (define test-v 
        (bbi::make-labeled-sequence :label "a" :sequence (list 1 2 3)))
      (list
       (bbi::labeled-sequence-label test-v)
       (bbi::labeled-sequence-sequence test-v)
       )))
  '("a" (1 2 3))
  )

;; NEW-TABLE

;; easy
(defbbltest
 newtable-1 
  (let ((table (new-table '(2 2 2) initialize-to 45)))
    (every
     (lambda (x) (= x 46))
     (utils::gmap '1+ table :flatten? t)))
 t)

;; make sure its adjustable
(defbbltest
 newtable-2 
  (let ((table (new-table '(2 2 2) init 5)))
    (setf (utils::gref table 4 4 4) 22)
    (utils::gref table 4 4 4))
 22)

;; not-adjustable
(defbblerrortest=
 e-newtable-1 
  (let ((table (new-table '(2 2 2) init 3 not-adjustable)))
    (setf (utils::gref table 4 4 4) 22)
    (utils::gref table 4 4 4))
 error)

;; TABLE-FORMAT (same as new-table)

;; easy
(defbbltest
 table-format-1 
  (let ((table (table-format '(2 2 2) initialize-to 45)))
    (every
     (lambda (x) (= x 46))
     (utils::gmap '1+ table :flatten? t)))
 t)

;; make sure its adjustable
(defbbltest
 table-format-2 
  (let ((table (table-format '(2 2 2) init 5)))
    (setf (utils::gref table 4 4 4) 22)
    (utils::gref table 4 4 4))
 22)

;; not-adjustable
(defbblerrortest=
 e-table-format-1 
  (let ((table (table-format '(2 2 2) init 3 not-adjustable)))
    (setf (utils::gref table 4 4 4) 22)
    (utils::gref table 4 4 4))
 error)

;; SPLIT

;; assuming case-sensitive throughout

;; at = nil, b-w = nil
(defbbltest=
 split1
  (split "Bi0 bi  ke")
 '("B" "i" "0" " " "b" "i" " " " " "k" "e"))

;; at = nil, b-w = t
(defbbltest=
 split2  
  (split "If you order within the next 10 minutes..." between-words)
 '("If" "you" "order" "within" "the" "next" "10" "minutes..."))


;; at = integer, b-w = nil
(defbbltest=
 split3 
  (split "If you order within the next 10 minutes..." at 3)
 '("If " "you order within the next 10 minutes..."))

;; at = integer, b-w = t
(defbbltest=
 split4 
  (split "If you order within the next 10 minutes..." every 3 between-words)
 '("If" "you" "ord" "er" "wit" "hin" "the" "nex" "t" "10" "min" "ute" "s.." "."))
  
;; at = string " ", b-w = nil
(defbbltest=
 split5
  (split "the fox eats the cheese at the market" every " ")
 '("the" "fox" "eats" "the" "cheese" "at" "the" "market"))

;; at = string " ", b-w = t
(defbbltest=
 split6  
  (split "the fox eats the cheese at the market" every " " between-words)
 '("the" "fox" "eats" "the" "cheese" "at" "the" "market"))

;; at = string, b-w = nil
(defbbltest=
 split7
  (split "the fox eats the cheese at the market" at "the")
 '(" fox eats the cheese at the market"))

(defbbltest=
 split7a
  (split "the fox eats the cheese at the market" every "the")
  '(" fox eats " " cheese at " " market"))

;; at = string, b-w = t
(defbblerrortest=
 e-split1  
  (split "the fox eats the cheese at the market" at "the" between-words)
 error)


;; from doc: 'multiple contiguous delimiters are compressed to one'
;; ... 'the' 3 spaces 'cheese' 3 spaces 'at' 'the' ...
(defbbltest=
 split8
  (split "the fox eats the   cheese   at the market" every " ")
 '("the" "fox" "eats" "the" "cheese" "at" "the" "market"))
;; same string, not sure why they come up null strings and not single space strings xxx
(defbbltest=
    split9
  (split "the      fox eats the   cheese   at the market" at " " no-compress)
  '("the" "     fox eats the   cheese   at the market"))

(defbbltest=
    split9a
  (split "the      fox eats the   cheese   at the market" at " " no-compress)
  '("the" "     fox eats the   cheese   at the market"))

;; split and rebuild
(defbbltest
 split10 
  (let* ((string (make-string 50 :initial-element #\a))
         (broken (split string))
         (repair (cl-user::apply 'utils:s+ broken)))
    (and (every (lambda (x) (cl:char= x #\a)) repair)
         (= 50 (length repair))
         ))
 t)
     

;; at keyword
(defbbltest=
 split11 
  (let* ((string (make-string 480 :initial-element #\a))
         (broken (split string every 50))
         (repair (cl-user::apply 'utils:s+ broken)))
    (and 
     (= 480 (length repair))
     (= (length broken) (ceiling (/ 480 50)))
     (lisp:loop for s in broken collect
       (every (lambda (x) (cl:char= x #\a)) s))))
 '(t t t t t t t t t t))

;; STRING-arg can be mapped
(defbbltest=
 split12 
  (let* ((a (make-string 3 :initial-element #\a))
         (b (make-string 3 :initial-element #\b))
         (broken (split (list a b) every 1)))
    broken)
 '(("a" "a" "a") ("b" "b" "b")))

;; edge condition 
(defbbltest=
 split13 
  (split "")
 nil)


;; FROM

;; try with pos/neg and integer/fixnum
(defbbltest==
 from-1 
  (append 
   (from -3.0 0)
   (from 0 0)
   (from 0 to 4.0)
   (from 100 103)
   (from -5 2)
   (from 7 5))
 '(-3 -2 -1 0 0 0 1 2 3 4 100 101 102 103 -5 -4 -3 -2 -1 0 1 2 7 6 5))

(defbbltest=
 from-2 
  (append
   (from 1.1 5.1)
   (from 6.23 10.86))
 '(1.1 2.1 3.1 4.1 5.1 6.23 7.23 8.23 9.23 10.23))

;; by keyword that leaves a remainder
(defbbltest=
 from-3 
  (from 16 81 by 9)
 '(16 25 34 43 52 61 70 79))

;; checking using keywords with a colon, and stepping by both pos and neg
(defbbltest
 from-4 
  (let ((x (from 16 81 by 9))
        (y (from 16 81 :by -9)))
    (and (equal x y) 
         (equal x '(16 25 34 43 52 61 70 79))))
 t)

;; no strings
(defbblerrortest=
 e-from-1 
  (from "b" 5)
 error)


;; SAME
;; num checks
(defbbltest 
 same-1 
  (let ((a 1) (b 2.0))
    (incf a) 
    (when (same a b)
      (incf b))
    (and (same a 2) (same b 3.0)))
 t)

;; eql frames
(defbbltest 
 same-2 
  (let ((d (frames::frame-fnamed "test.donatello" t)))
    (same #$test.donatello d))
 t)

;; lists
(defbbltest 
 same-3  
  (same (list 0 1 2 3) (utils::iota 3))
 nil)

;; case-insensitive strings
(defbbltest 
 same-4  
  (same "abcde" "ABCDE" case-insensitive)
 t)

;; case-sensitive strings, NIL
(defbbltest
 same-5 
  (same "abcde" "AbCdE" case-sensitive)
 nil)

;; case-sensitive strings, T
(defbbltest
 same-6
   (same "ABCde" "ABCde" case-sensitive)
 t)

;; when comparing symbols vs strings, test is always case-insensitive
;; regardless of flag
(defbbltest
 same-7  
  (and 
   (same 'UhhKL as "UHHKL" case-sensitive)
   (same 'UhhKL "UhhKL" case-sensitive))
 t)

;; all symbols get interned uppercase
(defbbltest
 same-8  
  (same 'UhhKL 'uHHkl)
 t)

;; things which obviously aren't same
(defbbltest= 
 same-10  
  (let ((r (frames::frame-fnamed "test.raphael" t))
        (m (frames::frame-fnamed "test.michaelangelo" t)))
    (list 
     (same r m)
     (same r 16)
     (same m "abc")))
 '(nil nil nil))


;; characters, maybe not important for BBL
(defbbltest=
 same-11 
  (list 
   (same #\k #\K) 
   (same #\k #\K case-sensitive)
   (same #\k #\k))
 '(t nil t))

;; more 
;; 2 and complex 2 are same
(defbbltest=
 same-13 
  (list 
   (same #$test.jeffs #$test.jeffe)
   (same 2 8)
   (same 2 #C(2 0)))
 '(nil nil t))

;; some different formats for things which should be SAME 
(defbbltest
 same-14 
  (same `(1 2 "a" 19 ,(list 1 8)) 
        '(1 2 "A" 19 (1 8)))
 t)

;; some different formats for things which should be SAME 
(defbbltest
 same-15 
  (same '(7 4 2 #$test.alpha)
        `(7 4 2 ,(frames::frame-fnamed "test.alpha" t)))
 t)

;; incorrect lists
(defbbltest
 same-16 
  (same (utils::iota 9) (utils::iota 8))
 nil)

;; test mapping
(defbbltest
 same-17 
  (same #("a" #\u (U I 99))
        (lisp:vector "a" #\u (list 'u 'i 99)))
 t)

;; subtle thing which isn't same, 99/98
(defbbltest
 same-18  
  (same #("a" #\u (U I 99))
        (lisp:vector "a" #\u (list 'u 'i 98)))
 nil)

;; subtle thing which isn't same, no 99
(defbbltest
 same-19 
  (same #("a" #\u (U I 99))
        (lisp:vector "a" #\u (list 'u 'i)))
 nil)



;; STRING-OF
 
;; abd9 gets interned/uppercased, then becomes a string
;; frame's fname
(defbbltest=
 string-of-1  
  (string-of 
   (list "a" "ABC" 'abd9 #$test.camel 33 '(1 2 (3 4)) #(9)))
 '("a" "ABC" "ABD9" "test.camel" "33" "(1 2 (3 4))" "#(9)"))

;; DESCRIPTION-OF

;; using temp frames with desc-of

#-(or :weblistener-aframes :sframes)
(defbbltest=
 description-1 
 (progn
   ;; create a fake gene frame
   (frames::frame-fnamed "fakegene" t)
   ;; make it a gene
   (setf (ref #$fakegene #$organism-entity-type) #$gene)
   ;; give it a description
   (setf (ref #$fakegene #$cog-description) 
         "If you think this is a real gene, you are wrong.  Don't err again.")
   (description-of #$fakegene length 7 +label))
 (prog1
     (list #$fakegene "If you ")
   ;; just to be safe
   (frames::unintern-frame #$fakegene)))

#||

;;; Doesn't work because temporary frame cannot be a gene frame 

#+:sframes
(defbbltest=
    description-1 
  (let ((tf (frames::make-temp-frame)))
    ;; make it a gene
    (setf (ref tf #$organism-entity-type) #$gene)
    ;; give it a description
    (setf (ref tf #$cog-description) 
          "If you think this is a real gene, you are wrong.  Don't err again.")
    (let ((desc (description-of tf length 7 +label)))
      (and (frames::isframe? (first desc))
           (string-equal (second desc) "If you "))))
  t
  )

||#

#+:weblistener-aframes
(defbbltest=
 description-1 
  ;; create a fake gene frame.  Avoid BBL processing problem with '.'
  (let* ((gene-type (read-from-string "frames::bio.gene"))
         (fakegene 
          (frames::make-frame-instance gene-type "test.not-a-gene" t)))
    ;; give it a description
    (setf (ref fakegene #$cog-description) 
          "If you think this is a real gene, you're wrong. Don't err again.")
    (description-of fakegene length 7 +label)
    )
 (let* ((fakegene (frames::frame-fnamed "test.not-a-gene" nil)))
   (prog1
       (list fakegene "If you ")
     ;; just to be safe
     (frames::unintern-frame fakegene)
     )))


#-:weblistener-aframes
(defbbltest
    description-2 
  ;; don't give our fake gene a description, still make sure it's a gene
  (let ((f (frames::frame-fnamed "unrealgene" t #+:sframes 'bio::gene)))
    (setf (ref f #$organism-entity-type) #$gene)
    (prog1
        (description-of f)
      ;; just to be safe
      (frames::unintern-frame f)))
  nil)

#+:weblistener-aframes
(defbbltest
 description-3
  ;; don't give our fake gene a description, still make sure it's a gene
  (let* ((gene-type (read-from-string "frames::bio.gene"))
         (fakegene
          (frames::make-frame-instance gene-type "test.not-a-gene1" t))
    (prog1
        (description-of fakegene)
      ;; just to be safe
      (frames::unintern-frame fakegene)
      )))
 nil)




;; ENTER

;;;;; Deleted from tests because it screws up how ENTER determines
;;;;; whether a package has already been created

#|
;; this loads the alien-world.lisp file which creates the :alien-world 
;; package etc. 
(defbbltest=
    enter1             
  (let* ((*package* (find-package :bbl-test-user))
         (wb::*username* *package*))
    (if (not (cl-user::probe-file 
              (cl-user::translate-simple-lp 
               "biol:bike;Tutorials;alien-world.lisp")))
        "" 
      (lisp:eval '(enter alien-world)))
    (prog1
        (not (null (member (find-package 'alien-world)
                           (cl-user::package-use-list *package*))))
      (unuse-package (find-package 'alien-world) *package*)))
  (if (not (cl-user::probe-file 
            (cl-user::translate-simple-lp 
             "biol:bike;Tutorials;alien-world.lisp")))
      "" t))
|#
   
;; ==========================================================================================
;; FIT

;; string > num
(defbbltest=
 fit1
  (fit "abcdef" 3)
 "abc")

;; string < num defaults to adding spaces
(defbbltest=
 fit2
  (fit "abcdef" 10)
 "abcdef    ")

;; with keyword can add other things
(defbbltest=
 fit3
  (fit "abcdef" 9 with "9")
 "abcdef999")

;; center keyword
(defbbltest=
 fit4
  (fit "abcdef" 10 center)
 "  abcdef  ")
      
;; center it, and fill the spaces with something
;; the remainder goes at the front
(defbbltest=
 fit5
  (fit 'abcdef 13 center with "b")
 "bbbbABCDEFbbb")

;; flush- flags
(defbbltest=
 fit6  
  (fit 
   (fit "abcdef" 11 flush-right)
   12 flush-left)
 "     abcdef ")

;; contrast to 1, which prints the original string without if-too-big
(defbbltest=
 fit7
  (fit "abcdef" 3 if-too-big "4")
 "444")

;; test a map
(defbbltest=
 fit8 
  (fit 
   (list "abcdef    " "ghijklmnopqrst" "")
   10)
 '("abcdef    " "ghijklmnop" "          "))
   

;; contradictory flags
(defbblerrortest=
 e-fit1
  (fit "abcdef" 12 center flush-right)
 error)

;; can't WITH a character
(defbblerrortest=
 e-fit2
  (fit "abcdef" 19 with #\a)
 error)

;; nor a string longer than 1
(defbblerrortest=
 e-fit3
  (fit "abcdef" 19 with "bcg")
 error)

 
;; ==========================================================================================
;; COMBINATIONS-OF (formerly ALL-COMBINATIONS-OF)

;; simple combinations of list elems
(defbbltest=
 ac1 
  (combinations-of '(a b c) CHOOSING 2)
 '((A B) (A C) (B C))
  )

;; simple combinations of a string, case-sensitivity
(defbbltest=
 ac2 
  (combinations-of "bAC"  CHOOSING 2)
 '("bA" "bC" "AC")
  )

;; second arg (normally a num) can be mapped
(defbbltest=
 ac3 
  (combinations-of "bAC"  CHOOSING 2 AS-LIST)
 '(("b" "A") ("b" "C") ("A" "C"))
  )

;; second arg (normally a num) can be mapped
(defbbltest=
    ac4 
  (combinations-of "abc" CHOOSING '(0 1 2 3))
 '(NIL ("a" "b" "c") ("ab" "ac" "bc") ("abc"))
  )

;; COMBINATION-COUNTS-OF 
(defbbltest=
    acc1 
  (combination-counts-of 5 CHOOSING '(0 1 2 3 4 5))
 '(1 5 10 10 5 1)
  )

;; PERMUTATIONS-OF 

(defbbltest=
 ap1 
  (permutations-of '(a b c))
 '((A B C) (A C B) (B A C) (B C A) (C A B) (C B A))
  )

(defbbltest=
 ap2 
  (permutations-of '(a b c) CHOOSING 2)
 '((A B) (A C) (B A) (B C) (C A) (C B))
  )

(defbbltest=
 ap2 
  (permutations-of '(a b c) CHOOSING 2 WITH-REPETITION)
 '((A A) (A B) (A C) (B A) (B B) (B C) (C A) (C B) (C C))
  )


;; simple combinations of a string, case-sensitivity
(defbbltest=
 ap3
  (permutations-of "bAC"  CHOOSING 2)
 '("bA" "bC" "Ab" "AC" "Cb" "CA") 
  )

;; all-protein-sequences, alphabet is too big so just make sure its all there
;; by passing 1 as second arg
(defbbltest=
    ac5 
  (lisp:sort (all-protein-sequences 1) 'string<)
  (lisp:sort 
   '("A" "C" "D" "E" "F" "G" "H" "I" "K" "L" "M" "N" "P" "Q" "R" "S" "T"
         "V" "W" "Y") 'string<)
  )
 
;; ==========================================================================================
;; CHOOSE-FROM

;; simple sanity check -- the thing we chose from the list is from the list
(defbbltest
 choose1 
  (let ((seq (list 5 6 7 8)))
    (not (null (member (choose-from seq) '(5 6 7 8)))))
 t)

;; try it on a vector
(defbbltest
 choose2 
  (let* ((seq (list 5 6 7 8))
         (v (coerce seq 'vector)))
    (not (null (member (choose-from v) seq))))
 t)    

;; and finally on a string, which returns a char
(defbbltest
    choose3 
  (let ((seq "abc"))
    (not (null (member (choose-from seq) (list "a" "b" "c") :test 'equal))))
  t)
 
;; ==========================================================================================
;; LENGTH-OF / LENGTHS-OF

;; list, but a sublist just counts as 1
(defbbltest 
 basic-length1 
  (length-of
   (cons '(1 2 3) (utils::iota 10)))
 11)

;; string
(defbbltest 
 basic-length2 
  (length-of
   "abcdefghijklmnopqrstuvwxyz")
 26)

;; vector
(defbbltest
 basic-length3 
  (length-of
   (vector 1 2 #\a 99))
 4)

;; nil
(defbbltest
 basic-length4 
  (length-of
   nil)
 0)

;; test EACH keyword
(defbbltest=
 basic-length5 
  (length-of each '((1 2) (3 4 5) (6 7 8 9)))
 '(2 3 4))

;;; LENGTHS-OF

;; list of strings 
(defbbltest=
 basic-lengths1 
  (lengths-of 
   (list
    "abc" "abcdef" ""))
 '(3 6 0))

;; list of lists
;; the 4 expressions of a list: quoted, not, a function result, or a predef. result
(defbbltest
 basic-lengths2 
  (let ((x nil))
    (setq x
          (list (list 'a) (list 'b 'c) (list 'd 'e 'f)))
    (lisp:flet ((foo () `(,(list 'a) ,(list 2 3) ,(list #\k #\j #\m))))
      (let* ((a '((a) (b c) (d e f)))
             (b  (list '(a) '(b c) '(d e f)))
             (c (foo))
             (d x)
             (e (list a b c d)))
        (every (lambda (x) (equal (lengths-of x) '(1 2 3))) e))))
 t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; IS-NONNEGATIVE?

;; test a constant, 0, and easy
(defbbltest 
 nonneg-1  
  (and 
   (is-nonnegative? pi)
   (is-nonnegative? 0)
   (is-nonnegative? 12))
 t)

;; on a neg
(defbbltest 
 nonneg-2 
  (is-nonnegative? -3)
 nil)

;;; IS-SIMPLE-LIST?

;; yes it is
(defbbltest
 simplist-1  
  (is-simple-list? (utils::iota 4))
 t)

;; no it isn't
(defbbltest
 simplist-2 
  (is-simple-list? (list (utils::iota 2) (utils::iota 4)))
 nil)
;; yes again, an edge case maybe
(defbbltest
 simplist-3 
  (is-simple-list? nil)
 t)

;;; ADD
(defbbltest=
 add1
  (add '(1 2 3) and (add 4 to '(2 3 4)))
 '(7 9 11))

;;; SUBTRACT
(defbbltest=
 subtract1
  (subtract 5 from (subtract 20 by 2))
 13)

;;; MULTIPLY
(defbbltest=
 multiply1
  (multiply '(1 2 3) and (multiply 4 by '(2 3 4)))
 '(8 24 48))

;;; DIVIDE
(defbbltest=
 divide1
  (divide 2 into (divide 100 by '(25 10)))
 '(2 5))
 
;; divide by 0
(defbblerrortest=
 divide2
  (divide 2 into (divide 100 by '(0 10)))
 error)

;;; NEGATIVE / NEGATE
(defbbltest=
 negate1  
  (negate '(4 0 -4))
 '(-4 0 4))

;; SUM-OF
;; function's &rest
(defbbltest 
 sum1 
  (sum-of 3 4 5)
 12)

;; or we can pass it as a list already
(defbbltest
 sum2  
  (sum-of (utils::iota 4))
 6)

;;; PRODUCT-OF
;; function's &rest
(defbbltest 
 product1 
  (product-of 3 4 5)
 60)

;; or we pass a list
(defbbltest
 product2  
  (product-of (utils::iota 4))
 0)

;;; IS-LIST?
;; nil isn't a list
(defbbltest
 islist-1 
  (is-list? nil)
 nil)

;; but NIL-OK makes it one
(defbbltest
 islist-2 
  (is-list? nil nil-ok)
 t)

;; simple case
(defbbltest
 islist-3
   (is-list? '(1 2 3))
 t)

;;; IS-STRING?
;; yes
(defbbltest
 isstring-1 
  (is-string? "")
 t)

;; no
(defbbltest
 isstring-1 
  (is-string? 5)
 nil)

;;; SWAP
(defbbltest=
 swap1  
  (let ((a 3) (b 4))
    (swap a b)
    (list a b))
 '(4 3))

;;; MEAN
(defbbltest=
 mean1 
  (mean (utils::iota 4))
 1.5)

(defbbltest
 mean1a 
  (mean '())
 nil)

;; all-true
(defbbltest
 alltrue1 
  (let ((s1 (list #$foo "1" '(1)))
        (s2 (list #$foo "1" nil)))
    (unless (all-true s2)
      (all-true s1)))
 t)

;; none-true
(defbbltest 
 nonetrue1 
  (let ((s1 (list #$foo "1" '(1)))
        (s2 (list nil nil)))
    (unless (none-true s1)
      (none-true s2)))
 t)

;; all-false
(defbbltest 
 allfalse1 
  (let ((s1 (list #$foo "1" '(1)))
        (s2 (list nil nil)))
    (unless (all-false s1)
      (all-false s2)))
 t)

;; any-true
(defbbltest
 anytrue1 
  (let ((s1 (list #$foo "1" nil))
        (s2 (list nil nil)))
    (unless (any-true s2)
      (any-true s1)))
 t)

;; any-false
(defbbltest 
 anyfalse1 
  (let ((s1 (list #$foo "1" '(1)))
        (s2 (list #$foo "1" nil)))
    (unless (any-false s1)
      (any-false s2)))
 t)
    
;; inversion-of
;; testing strings and the function's aliases
(defbbltest= 
 inversion-of-1 
  (let ((seq "AbcGgtTu"))
    (list
     (inversion-of seq)
     (opposite-strand-of seq)
     (opposite-strands-of seq)))
 '("uAacCgbT" "uAacCgbT" "uAacCgbT"))

;; needs AT LEAST a nested list
(defbblerrortest=
 translist1  
  (transpose-list nil)
 error)

;; length of the longest list becomes default length of answer, 
;; fill in NIL for blanks
(defbbltest=
 translist2 
  (transpose-list '((a b c) (d e f) (1 2) (3 4 5 6))) '((a d 1 3) (b e 2 4) (c f nil 5) (nil nil nil 6)))
 
;; log2, log10, ^, ! all return nums
;; the logs can be mapped, so can accept NIL, but return it as well
(defbbltest
 log1 
  (unless (or (log2 nil) (log10 nil))
    (every 'numberp (list (log2 12) (log10 88)
                          (^ 2 5) (! 4))))
 t)

;; make sure all the elements are still there after a shuffle, and nothing else
(defbbltest
 shuffle1 
  (unless (shuffle nil)
    (let* ((seq '(12 "a" nil 'foo))
           (shufseq (shuffle seq)))
      (and (= (length seq) (length shufseq))
           (every 'listp 
                  `(,(member "a" shufseq) 
                    ,(member 12 shufseq) 
                    ,(member nil shufseq)
                    ,(member 'foo shufseq))))))
 t)


(defbbltest=
 count0a 
  (count-of "b" in-each '("BBbb" "ccc" "b") case-sensitive)
 '(2 0 1))

(defbbltest=
    count0b  
    (counts-of (list "b" "c") in-each-of '("BBbb" "bccc" "b") case-sensitive)
  '((2 0) (1 3) (1 0)))

(defbbltest=
    count0c  
    (counts-of "bc" in-each-of '("BBbb" "bccc" "b") case-sensitive)
  '(0 1 0))

(defbbltest
 alldnaseq-1   
    (let ((x (utils::purge-duplicates
              (all-dna-sequences of-length 3) :test 'string-equal))
          (d "ACGT"))
      (and (= (length x) 64)
           (every 
            (lambda (s) 
              (and (stringp s) 
                   (= (length s) 3)
                   (lisp::Find (lisp:elt s 0) d)
                   (lisp::Find (lisp:elt s 1) d)
                   (lisp::Find (lisp:elt s 2) d)))
            x)
           ))
 t)

(defbbltest
 allprotseq-1  
    (let ((x (utils::purge-duplicates (all-protein-sequences of-length 3)))
          (achars "ARNDCEQGHILKMFPSTWYV"))
      (and 
       (= (length x) 8000)
       (every
        (lambda (s) 
          (and (stringp s)
               (= (length s) 3)
               (lisp::Find (lisp:elt s 0) achars)
               (lisp::Find (lisp:elt s 1) achars)
               (lisp::Find (lisp:elt s 2) achars)))
     x)))
 t)

(defbbltest 
 posint1  
  (is-positive-integer? 23)
 t)
(defbbltest 
 posint2 
   (is-positive-integer? .2)
 nil)
(defbbltest
 posint3
 (is-positive-integer? (sqrt 25))
 t)
#+wait
(defbbltest
 nonneg1
 (is-nonnegative-integer? (sqrt 25))
 t)

(defbbltest 
 posnum1  
  (is-positive-number? 5)
 t)
(defbbltest 
 posnum2 
  (is-positive-number? -3)
 nil)

(defbbltest
 isint1
 (is-integer? 12)
 t)

(defbbltest
 isint2
 (is-integer? 1.0)
 t)

(defbbltest
 isint3
 (is-integer? 0)
 t)

(defbbltest
 isint4
 (is-integer? #C(1.0 3.0))
 nil)

(defbbltest
 isint5
 (is-integer? 1.0d0)
 t)

(defbbltest
 isint6
 (is-integer? 1.2)
 nil)

(defbbltest
 iseven1
 (is-even? 2)
 t)

(defbbltest
 iseven2
 (is-even? 0)
 t)

(defbbltest
 iseven3
 (is-even? 1.2)
 nil)

(defbbltest
 iseven4
 (is-even? 2.0d0)
 t)

(defbbltest
 iseven5
 (is-even? 2.8)
 nil)

(defbbltest
 iseven6
 (is-even? #C(2.0 1.0))
 nil)

(defbbltest
 iseven7
 (is-even? 3)
 nil)


(defbbltest
 isodd1
 (is-odd? 3)
 t)

(defbbltest
 isodd2
 (is-odd? 0)
 nil)

(defbbltest
 isodd3
 (is-odd? 1.2)
 nil)

(defbbltest
 isodd4
 (is-odd? 3.0d0)
 t)

(defbbltest
 isodd5
 (is-odd? 3.8)
 nil)

(defbbltest
 isodd6
 (is-odd? #C(1.0 1.0))
 nil)

(defbbltest
 isodd7
 (is-odd? 8)
 nil)

(defbbltest 
 ie1 
  (handler-case (internal-error 'oopsy)
    (error 
     ()
     t))
 t)

(defbbltest    
 bblversion1 
  (stringp (bbl-version))
 t)

(defbbltest 
 anynil1 
  (any-nil '(2 3 nil))
 t)

(defbbltest 
 istable-1 
  (is-table? (utils::make-garray '(1 1)))
 t)

(defbbltest
 isnumber-1 
  (is-number 3)
 t)

(defbbltest
 isnumber-2
 (or (is-number? t) (is-number? "h"))
 nil)
 
            

(defbbltest
 isframe-1  
  (is-frame? #$foo)
 t)

(defbbltest
    isframe-2  
    (is-frame? 90)
  nil)

(defbbltest 
 stddev1 
  (numberp (standard-deviation '(3 5 8)))
 t)

(defbbltest 
 stddev2 
  (zerop (standard-deviation '(5 5 5 5 5 5 5)))
 t)

(defbbltest= 
 poss1  
  (positions-of "ab" "xxabxxABA")
 '(3 7))

(defbbltest= 
 poss2 
  (positions-of "Ab" "xxabxxAbAb" case-sensitive)
 '(7 9))

(defbbltest=
 poss3 
  (positions-of 8 '(8 8 8 8))
 '(1 2 3 4))


(defbbltest=
alphaof1   
 (alphabet-of nil)
  "")

(defbbltest=
alphaof2 
(alphabet-of "aaaa")
"a")

(defbbltest=
alphaof3
(let* ((s "ab2bb22a")
       (alphabet (alphabet-of s))
       (allow (list #\a #\2 #\b)))
(when (= (length alphabet) (length allow))
  (lisp:loop for x across alphabet 
    while (member x allow)
  finally (return 12))))
12)


;;; Apply-function is actually MAPCAR, not APPLY.
;;; It has some fancy syntax having to do with '@' as well.

(defbbltest= appfun1 
  (apply-function (+ x 1) replacing x with '(1 2 3))
  '(2 3 4))

(defbbltest= appfun1a
  (apply-function (+ x 1) replacing (x) with '(1 2 3))
  '(2 3 4))

(defbbltest= appfun2
  (apply-function (+ x y) replacing (x y) with '(1 2 3) '(4 5 6))
  '(5 7 9)
  )

(defbbltest= appfun3
  (apply-function (+ x y z q) replacing (x y z q) 
    with 
    '(1 2 3) '(4 5 6) '(7 8 9) '(3 4 5)
    )
  '(15 19 23)
  )

(defbbltest= appfun4
  (let ((x 10) (y 12))
    (lisp:declare (ignorable x))
    (apply-function (+ x y) replacing (x) with '(1 2 3)))
  '(13 14 15)
  )

;; 5 and 6 are new tests for revised functionality that 
;; coerces each argument to a list.  
(defbbltest= appfun5
  (apply-function (+ x 12) replacing (x) with 10)
  '(22))

(defbbltest= appfun6
  (apply-function (+ x y) replacing (x y) with 10 20)
  '(30))

(defbbltest= repeat-function1
  (let ((x 0))
    (repeat-function (setq x (+ 1 x)) 10 times)
    x)
  10
  )

(defbbltest= repeat-function2 
  (progn
    (setq cl-user::*repeat-function-value* 0)
    (repeat-function cl-user::repeat-function-aux 10 times)
    cl-user::*repeat-function-value*)
  10
  )
  
;; strings taken from doc
(defbbltest= trans1
  (translation-of 
    "ATGCGTCGGGTCCCCzGTCGCTCGGGAAATGATAGTACGATAGATGATAGATCCCGCTCGATCGATCGCAAACT"
    nowarnings)
  "MRRVP-RSGNDSTIDDRSRSIDRK")

(defbbltest= trans2
  (translation-of 
    "ATGCGTCGGGTCCCCzGTCGCTCGGGAAATGATAGTACGATAGATGATAGATCCCGCTCGATCGATCGCAAACT"
    nowarnings if-bad-codon "$")
  "MRRVP$RSGNDSTIDDRSRSIDRK")

(defbbltest= trans2a
  (translation-of "taatagtgatgg")
  "***W")

;; test that extra characters at end don't matter 
(defbbltest= trans3
  (translation-of "aaaaa")
  "K")

;; should be the same for first, second, ..., tenth
(defbbltest
 third1
 (third '(5 6 7 8 9))
 7)

(defbblerrortest=
 e-third1
 (third '(5 6) strict)
 error)

(defbbltest
 third1x
 (third '(5 6))
 nil)

(defbbltest=
 third2
 (third '(5 6) nonstrict)
 nil)

(defbbltest= 
 third3
 (third in-each (list '(1 2 3 4 5) "abcde"))
 '(3 "c")
 )

(defbbltest= last0a (last '(1 2 3)) 3)
(defbbltest= last0b (last "abcd") "d")

(defbbltest= 
 first1
 (first '(1 2 3))
 1
 )

(defbbltest=
 first2
 (first "abc")
 "a"
 )

(defbbltest=
 first3
 (first 3 '(1 2 3 4 5))
 '(1 2 3))

(defbbltest=
 first4
 (first 4 "abcdef")
 "abcd"
 )

(defbbltest=
 first5
 (first 4 in '(1 2 3 4))
 '(1 2 3 4)
 )

(defbbltest=
 first6
 (first 4 in "abcdef")
 "abcd"
 )

(defbbltest=
 first7
 (first 3 in-each '((1 2 3 4 5) (2 3 4 5 6)))
 '((1 2 3) (2 3 4)))

(defbbltest=
 first8
 (first 3 in-each '("abcdefg" "1234567"))
 '("abc" "123")
 )

(defbblerrortest=
 e-first1
 (first 3 in '(1 2) strict)
 error
 )

(defbblerrortest=
 e-first2
 (first 3 in-each '("a" "b") strict)
 error
 )


 
(defbbltest= 
 last1
 (last '(1 2 3))
 3
 )

(defbbltest=
 last2
 (last "abc")
 "c"
 )

(defbbltest=
 last3
 (last in-each '((1 2 3 4 5) (2 3 4 5 6)))
 '(5 6))

(defbbltest=
 last4
 (last in-each '("abcdefg" "1234567"))
 '("g" "7")
 )

(defbbltest=
 last5
 (last '() nonstrict)
 nil)

(defbbltest=
 last6
 (last 3 in '(1 2 3 4 5 6))
 '(4 5 6))

(defbbltest=
    last7
 (last 3 in-each '((a b c d) (1 2 3 4 5 6)))
 '((b c d) (4 5 6)))

(defbbltest=
    last8 
  (last 2 in "abcdef")
  "ef")

(defbbltest=
    last9
  (last 2 in-each '("123456" "abcd"))
  '("56" "cd")
  )
  
       



(defbblerrortest=
 e-last1
 (last '() strict)
 error
 )

(defbblerrortest=
 e-last2
 (last in-each '(() ()) strict)
 error
 )


        
       
        
(defbbltest=
 randomnum1
 (numberp (random-number))
 t)

(defbbltest=
 randomint1
 (integerp (random-integer))
 t)

(defbbltest=
 randomnum2
 (let ((ran (random-number from 1 to 5)))
   (and (numberp ran)
        (>= ran 1)
        (<= ran 5)))
 t)

(defbbltest=
 randomint2
 (let ((ran (random-integer from 1 to 5)))
   (and (member ran '(1 2 3 4 5))
        (integerp ran)))
 t)

(defbblerrortest=
 e-both1
 (lisp:eval
  '(both t and t t))
 error)

(defbblerrortest=
 e-both2
 (lisp:eval '(both))
 error)

(defbbltest=
 both1
 (both t and t and t and t)
 t)


(defbblerrortest=
 e-either1
 (lisp:eval '(either t or t t))
 error)

(defbblerrortest=
 e-either2
 (lisp:eval '(either))
 error)

(defbbltest=
    either1
  (either nil or nil or nil or t)
  t)
 
(defbbltest= greater1 (greater-than 9 7) t)
(defbbltest= greater2 (greater-than 7 "abc") nil)
(defbbltest= greater3 (greater-than "abc" "abb") t)
(defbbltest= greater4 (greater-than "abc" "abC" case-sensitive) t)
(defbbltest= less1 (less-than -2 .11) t)
(defbbltest= less2 (less-than 7 "abc") t)
(defbbltest= less3 (less-than "abb" "abc") t)
(defbbltest= less4 (less-than "abC" "abc" case-sensitive) t)

(defbbltest= simplist1 
  (simplify-list '(a b c d e))
'(a b c d e))

(defbbltest= simplist2
  (simplify-list '(a (b) ((c (d e)))))
'(a b c d e))

(defbbltest= simplist3
  (simplify-list '(a nil c nil) no-nils)
  '(a c))

(defbbltest= simplist4
  (simplify-list '(2 2 3 "a" 3 3 4 4 4 4) no-duplicates)
'(2 "a" 3 4))

(defbbltest= translit1
             (transliterate "abcdef" "d" "i")
             "abcief")

(defbbltest= translit2
             (transliterate "aabbccd" from "abc" to "xyx")
             "xxyyxxd")

(defbbltest= translit3
             (transliterate "abcdef" from "z" "9")
             "abcdef")

(defbbltest= translit4
             (transliterate "abcdef" from "a" "93")
             "9bcdef")

(defbblerrortest=
 e-translit1
 (transliterate "abcdef" from "ab" "9")
 error)

(defbbltest=
 insert1
 (insert into "abcdef" "2" before "c")
 "ab2cdef"
 )

(defbbltest=
 insert2
 (insert into "abcdef" "2" after "c")
 "abc2def"
 )

(defbbltest=
 insert3
 (insert into '(1 2 3 4) 78 before 4)
 '(1 2 3 78 4))

(defbbltest=
 insert4
 (insert into '(1 2 3 4) 78 after-end)
 '(1 2 3 4 78))

(defbbltest=
 insert5
 (insert into '(2 3 4 5) each '("foo" 158) before 2)
 '((2 "foo" 3 4 5) (2 158 3 4 5)) 
 )

(defbbltest=
 insert6
 (insert into '(1 "a" "A") 99 after "A" case-sensitive)
 '(1 "a" "A" 99))

(defbblerrortest=
 einsert1
 (insert into '(1 2 3 4) 5 after 0 strict)
 error)

(defbbltest=
 insert7
 (let* ((x '(1 2 3 4))
        (y "abcd")
        (z (insert into-copy-of-each (list x y) "x" after-end)))
   (list x y z))
 '((1 2 3 4) "abcd" ((1 2 3 4 "x") "abcdx"))
 )
   
 

#||

(defbbltest= insert1
             (insert "a" into "bcdefg" before 3)
             "bcadefg")

(defbbltest= insert2
             (insert "ab" into "xxxxx" after 5)
             "xxxxxab")

(defbbltest= insert3
             (insert "ab" into "12345678" replacing 2 to 5)
             "1ab678")

(defbbltest= insert4
             (insert "ab" into "xxxxx" replacing 3 to 99 relaxed)
             "xxab")

(defbbltest= insert5
             (insert -1 into '(1 2 3 4 5 6 7 8 9 0) before 3)
             '(1 2 -1 3 4 5 6 7 8 9 0))

(defbbltest= insert6
             (insert -1 into '(1 2 3 4 5 6 7 8 9 0) replacing 5 to 10)
             '(1 2 3 4 -1))

(defbbltest= insert7
             (insert -1 into '(1 2 3 4 5 6 7 8 9 0) repeating-from 5 to 9)
             '(1 2 3 4 -1 -1 -1 -1 -1 0))

(defbbltest= insert8
  (insert "abcd" into "1234567890" replacing 1 to 2) 
  "abcd34567890")

(defbbltest= insert9
  (insert "ab" into "1234567890" replacing 4 to 9)
  "123ab0")

(defbbltest=
 insert10
 (insert "789" into "123456" after-end)
 "123456789"
 )

(defbbltest=
 insert11
 (let ((x "abc"))
   (insert "123" into x before 1)
   x)
 "123abc"
 )

(defbbltest=
 insert12
 (let ((x "abc"))
   (list (insert "123" into-copy-of x before 1) x))
 '("123abc" "abc")
 )

(defbbltest=
 insert13
 (let ((x "123") 
       (y "456"))
   (insert "abc" into-each (list x y) before 1) 
   (list x y))
 '("abc123" "abc456")
 )


(defbbltest=
 insert14
 (let ((x "123") 
       (y "456"))
   (list (insert "abc" into-copy-of-each (list x y) before 1) x y))
 '(("abc123" "abc456") "123" "456")
 )

(defbblerrortest=
 einsert1
 (insert "x" into "abc" after 5)
 error
 )

(defbbltest=
 insert15
 (insert "x" into "abc" relaxed after 5)
 "abcx"
 )

(defbblerrortest=
 einsert2
 (insert "x" into "abc" relaxed before 0)
 error
 )

(defbblerrortest=
 einsert3
 (insert "x" into "abc" relaxed before -2)
 error
 )

(defbbltest=
 insert16
 (insert "x" into "abc" after *end*)
 "abcx"
 )

||#

(defbbltest= repeat1
  (repeat "abcd" times 4)
  "abcdabcdabcdabcd")

(defbbltest= 
 repeat2
 (repeat "" times 9) 
 "")

(defbbltest=
 repeat3
 (repeat "a" times 0)
 "")

(defbbltest=
 repeat4
 (repeat '("ghi" "" "7") times 3)
 '("ghi" "" "7" "ghi" "" "7" "ghi" "" "7"))

(defbbltest=
    repeat4a
  (repeat '("ghi" "" "7") times 3 as-unit)
  '(("ghi" "" "7") ("ghi" "" "7") ("ghi" "" "7")))

(defbbltest=
    repeat4b
  (repeat each '("ghi" "" "7") times 3)
  '("ghighighi" "" "777"))

(defbbltest= 
    repeat5
  (repeat "abc" until-length 10)
  "abcabcabca")

(defbbltest= 
    repeat6
  (repeat "abcde" until-length 2)
  "ab")

(defbbltest=
    addset1 
  (let ((set (add-set '(1 (2) ((3))) to '(((3)) 1 (8)))))
    (and 
     (member 1 set :test 'equal) (member '(2) set :test 'equal)
     (member '((3)) set :test 'equal) (member '(8) set :test 'equal)
     t))
  t)

;; This wouldn't work if this file were compiled because btu::testfunc
;; would not get recreated 
(defbbltest= 
    myfunc1
  (progn
    (bbi::define-function 
        btu::testfunc 
      required a
      keyword b = 9
      body (+ a b))
    (let ((wb::*username* :bbl-test-user))
      (prog1
          (not (null (member 'testfunc (my-functions))))
        (unintern 'testfunc :btu)
        )))
  t)  

(defbbltest= 
    myvar1
  (let ()
    (lisp:declare (lisp:special btu::testvar))
    (bbi::define btu::testvar 400)
    (let ((wb::*username* :bbl-test-user))
      (prog1
          (not (null (member 'btu::testvar (my-variables -display))))
        (unintern 'testvar :btu)
        )))
  t)

(defbbltest=
 median1
 (median '(1 2 3 4 5 6))
 3.5)

(defbbltest=
 median2
 (median '(10 109 -5 1 8 18 2))
 8)

(defbbltest=
 median3
 (median '(7 10 109 -5 1 8 18 2))
 7.5)

(defbbltest= median4
  (median '(108))
  108)

(defbblerrortest= 
 e-median1
 (median '((1 2 3) 2))
 error)

(defbblerrortest= 
 e-median2
 (median '())
 error)
                  
                  
(defbbltest=
 pm1
 (bbi::mp-output-string
  '(()
    ((start) 
     (repeat indefinitely (not (pattern "#")))
     (end)
     )))
 "<START>[~\\d]*<END>"
 )

(defbbltest=
 pm2
 (bbi::mp-output-string
  '(()
    ((pattern "###-")
     (capture (pattern "##"))
     (pattern "-####")
     )))
  "\\d\\d\\d-<<\\d\\d>>-\\d\\d\\d\\d"
  )

(defbbltest=
 pm3
 (bbi::mp-output-string
  '((("x" "123"))
    ((pattern "0x"))))
 "0123"
 )

(defbbltest=
 simplist1
 (simplify-list '(1 2 (3 4) 5))
 '(1 2 3 4 5))

(defbbltest=
 simplist2
 (simplify-list '(1 2 3 4 5))
 '(1 2 3 4 5))

(defbbltest=
 simplist4
 (simplify-list '(1 () 2 3 4 5))
 '(1 nil 2 3 4 5))

(defbbltest=
 simplist4
 (simplify-list '(1 () 2 3 4 5) no-nils)
 '(1 2 3 4 5))

(defbbltest=
 simplist5
 (simplify-list '(1 (1 1)) no-duplicates)
 '(1))

(defbbltest=
 simplist6
 (simplify-list '(1 (2 2 nil) nil) no-nils no-duplicates)
 '(1 2))

(defbbltest=
 numlist1
 (number-list -3 0)
 '(-3 -2 -1 0))

(defbbltest=
 numlist2
 (number-list 0 2)
 '(0 1 2))

(defbbltest=
 numlist3
 (number-list 4 9 limit 2)
 '(4 5))

(defbbltest=
 numlist4
 (number-list 4 9 by 2)
 '(4 6 8))

(defbbltest=
 numlist5
 (number-list 1 10 by 2 limit 3)
 '(1 3 5))

(defbbltest=
 numlist6
 (number-list 5 1)
 '(5 4 3 2 1))

(defbbltest=
 numlist6
 (number-list 10 1 limit 3 by 2)
 '(10 8 6))
              



(defbbltest=
 negof1
 (lisp:mapcar 'negation-of '(-3 0 3))
 '(3 0 -3))
 
(defbbltest=
 difof1
 (difference-of 8 4)
 4)

(defbbltest=
 difof2
 (difference-of '(2 3 4) '(1 2 3))
 '(1 1 1))

(defbbltest=
 quotof1
 (quotient-of 8 4)
 2)

(defbbltest=
 quotof2
 (quotient-of '(2 4 6) '(2 2 2))
 '(1 2 3))

;; list no keywords
(defbbltest=
 make1
 (make list)
 '(nil))

;; table no keywords
(defbbltest=
 make2
 (let ((x (make table)))
   (and (typep x 'table)
        (null (utils::gref x 1))))
 t)

;; using a garray
(defbbltest=
 make3
 (let ((x (make garray)))
   (and (typep x 'table)
        (null (utils::gref x 1))))
 t)

;; string no keywords
(defbbltest=
 make4
 (make string)
 "a"
 )

(defbbltest=
 make5
 (make list how-big 3)
 '(nil nil nil)
 )

(defbbltest=
 make6
 (make list initial-element 4)
 '(4)
 )

(defbbltest=
 make7
 (make list how-big 2 initial-element "h")
 '("h" "h")
 )

(defbbltest=
 make8
 (make list how-big '(2 4 1) initial-element 1)
 '((1 1) (1 1 1 1) (1))
 )

(defbbltest=
 make8
 (let ((x (make table initial-element 3)))
   (and (typep x 'table)
        (= (utils::gref x 1) 3)))
 t)

(defbbltest= 
 make9
 (let ((x (make table how-big '(2 2))))
   (and (typep x 'table)
        (null (utils::gref x 2 2))
        (null (utils::gref x 1 1))
        (null (utils::gref x 2 1))
        (null (utils::gref x 1 2))))
 t)

(defbbltest 
 make10
 (let ((x (make garray initial-element 7 how-big '(3 3))))
   (and (typep x 'table)
        (every 
         (lambda (y) (= y 7))
         (list 
          (utils::gref x 1 1)
          (utils::gref x 0 0)
          (utils::gref x 1 0)
          (utils::gref x 0 1)))))
 t)

(defbbltest=
 make11
 (make string initial-element #\y)
 "y"
 )

(defbbltest=
 make12
 (make string how-big 6)
 "aaaaaa"
 )

(defbbltest=
 make13 
 (make string initial-element #\j how-big 4)
 "jjjj"
 )

(defbbltest=
 make14
 (make string initial-element "k" how-big 5)
 "kkkkk"
 )
   
(defbbltest= 
    interleave1
  (interleave '(1 2 3) '(4 5 6))
  '((1 4) (2 5) (3 6)))

(defbbltest= 
    interleave2
  (interleave '(1 2 3) '(4 5 6) '(7 8) :truncate)
  '((1 4 7) (2 5 8)))

(defbbltest= 
    interleave3
  (interleave '(1 (2) 3) '(4 5 (6)) '(7 8) :truncate :simplify)
  '((1 4 7) (2 5 8)))

(defbbltest= 
    interleave4
  (interleave nil nil)
  nil)

(defbbltest= 
    interleave5
  (interleave '(1 2) nil :truncate)
  nil)


(defbbltest=
 aacounts3
 (amino-acid-counts-of "yWAcKhpQdefVTSrgLiNM")
 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))

(defbbltest=
    aacounts4
  (amino-acid-counts-of (list "AcKhpQdefV" "TSrgLiNM"))
  '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0)
  )

(defbblerrortest=
 e-aacounts1
 (amino-acid-counts-of "ay2yyz") ; now uses extended aa's so z is OK
 error
 )

(defbbltest=
    aacounts5
  (let ((seq (bbi::make-labeled-sequence 
              :label "myseq" :sequence "AceF")))
    (amino-acid-counts-of seq))
  '(1 1 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  

(defbbltest=
 aafreqs3
 (let ((seq (amino-acid-frequencies-of "yWAcKhpQdefVTSrgLiNM")))
   (every (lambda (x) (= x (/ 1 20))) seq))
 t)

(defbbltest=
    aafreqs4
  (let* ((seq (amino-acid-frequencies-of "ywwyyw"))
         (sseq (lisp:subseq seq 0 18)))
    (and (= (lisp:nth 18 seq) (/ 1 2))
         (= (lisp:nth 19 seq) (/ 1 2))
         (every 'zerop sseq)))
  t)

(defbbltest=
 instring1
 (inside-string "abcdefg")
 "abcdefg"
 )

(defbbltest=
 instring2
 (inside-string "abcdefg" from 1 to 6)
 "abcdef"
 )

(defbbltest=
 instring3
 (inside-string "abcdefg" from 1 to 7)
 "abcdefg"
 )

(defbbltest=
 instring4
 (inside-string "abcdefg" from 2 to 5 reversed)
 "edcb"
 )

(defbbltest=
 instring5
 (let ((s (inside-string "abcdefg" from 2 to 6 randomized)))
   (and 
    (= 1 (lisp:count #\b s))
    (= 1 (lisp:count #\c s))
    (= 1 (lisp:count #\d s))
    (= 1 (lisp:count #\e s))
    (= 1 (lisp:count #\f s))
    ))
 t
 )

(defbbltest=
 instring6
 (let ((s (inside-string "abcdefg" from 2 to 6 reversed randomized)))
   (and 
    (= 1 (lisp:count #\b s))
    (= 1 (lisp:count #\c s))
    (= 1 (lisp:count #\d s))
    (= 1 (lisp:count #\e s))
    (= 1 (lisp:count #\f s))
    ))
 t
 )

(defbbltest=
 instring7
 (inside-string "abcdefh" by 3)
 "adh"
 )

(defbbltest=
 instring8
 (inside-string "abcdefh" by 3 reversed)
 "hda"
 )

(defbbltest=
 instring9
 (let ((s (inside-string "abcdefh" by 3 randomized)))
   (and 
    (= 1 (lisp:count #\a s))
    (= 1 (lisp:count #\h s))
    (= 1 (lisp:count #\d s))
    ))
 t
 )

(defbbltest=
 instring10
 (let ((s (inside-string "aBcdefgHij" from 2 to 9 by 2 randomized)))
   (and 
    (= 1 (lisp:count #\B s))
    (= 1 (lisp:count #\d s))
    (= 1 (lisp:count #\f s))
    (= 1 (lisp:count #\H s))
    ))
 t
 )

(defbbltest=
 instring11
 (inside-string "abcdefghij" from 2 to 10 by 2)
 "bdfhj"
 )

(defbbltest=
 instring12
 (inside-string "alongSTRing" from 2 length 5)
 "longS"
 )

(defbbltest=
 instring13
 (inside-string "alongSTRing" to 8 length 5)
 "ngSTR"
 )

(defbbltest=
 instring14
 (inside-string "alongstring" from 2 length 6)
 "longst"
 )

(defbbltest=
 instring15
 (inside-string "alongstring" from 2 to 2)
 "l"
 )

(defbbltest=
 instring16
 (inside-string "alonGString" item 5)
 "G"
 )

(defbbltest=
 instring17
 (inside-string "alonGStringG" from 4 by 4)
 "nrG"
 )

(defbblerrortest=
 e-instring1
 (inside-string "alongstring" length 5)
 error
 )

(defbblerrortest=
 e-instring2
 (inside-string "alongstring" from 2 to 10 length 8)
 error
 )

(defbblerrortest=
 e-instring3
 (inside-string "alonGString" from 3 to 6 item 3)
 error
 )
 
(defbbltest=
 insidelist1
 (inside-list '(1 2 3 4 5 6 7 8) from 2 to 6)
 '(2 3 4 5 6))

(defbbltest=
 insidelist2
 (let ((x (inside-list '(1 2 3 4) from 2 to 3 randomized)))
   (not (null (and (member 2 x) (member 3 x)))))
 t)

(defbbltest=
 insidelist3
 (inside-list '(1 2 3 4 5 6 7 8 9 10) from 2 to 9 by 3)
 '(2 5 8))

(defbblerrortest=
 insidelist4
 (inside-list '(1 2 3 4 5 6) from 2 to 6 length 1)
 error)

(defbbltest=
 insidelist5
 (inside-list '(1 2 3 4 5 6) reversed)
 '(6 5 4 3 2 1))

(defbbltest=
 insidelist6
 (inside-list '(a b c d e f g h) item '(2 3 4))
 '(b c d))
 
(defbbltest=
 intersect1
 (let ((x (intersection-of '(1 2 3 4 5) '(3 4 5 6))))
   (not (null (and (member 3 x) (member 4 x) (member 5 x)))))
 t)

(defbbltest=
 intersect2
 (intersection-of '(1 2 3) '(4 5 6))
 nil
 )

(defbbltest=
 join1
 (join bbi::*amino-acids* by "|")
 "A|C|D|E|F|G|H|I|K|L|M|N|P|Q|R|S|T|V|W|Y"  
 )

(defbbltest=
 join2
 (join '(1 2 (3 4) 5) as-list)
 '(1 2 3 4 5))

(defbbltest=
 join3
 (join '(1 2 (3 4) 5) as-string)
 "12345")

(defbbltest=
    join4
  (let ((a (bbi::make-labeled-sequence :label "a" :sequence "abcdef"))
        (b (bbi::make-labeled-sequence :label "b" :sequence "ghijkl")))
    (join a b by "*"))
  "abcdef*ghijkl")

(defbbltest=
 list1
 (list 5)
 '(5)
 )

(defbbltest= 
 calc1
 (calc "1+4+5")
 10)

(defbbltest=
 calc2
 (calc '(1+4+5))
 10
 )

(defbbltest=
 calc3
 (let ((x 5)) (calc "x"))
 5
 )

(defbbltest=
 calc4
 (let ((x- 6)) (calc "x-" :only-with-whitespace))
 6
 )

(defbbltest=
 calc5
 (let ((x 5) (y 10) (x-y 15)) (progn x-y (calc "x-y")))
 -5
 )

(defbbltest=
 calc6
 (let ((x 5) (y 10) (x-y 15)) (progn x y (calc "x-y" :only-with-whitespace)))
 15
 )

(defbbltest=
 calc7
 (let ((x 5) (y 10) (x-y 15)) (progn x y (calc "x-y" :unary-and-whitespace)))
 15
 )

(defbbltest=
 calc8
 (let ((x 5) (y 10) (x-y 15)) (progn x-y (calc "x+-y" :unary-and-whitespace)))
 -5
 )

(defbbltest=
 calc9
 (let ((x 5) (y -1) (-y 6)) (= -y (calc "x-y")))
 t
 )

(defbbltest=
 calc10
 (let ((x 5) (-y 8)) (calc "x+-y" :only-with-whitespace))
 13
 )

(defbbltest=
 calc11
 (let ((x 5) (-y -3)) (progn x -y (calc "x+-y" :only-with-whitespace)))
 2
 )

(defbbltest=
 calc12
 (let ((foo 1) (bar 2) (foo-bar 3)) (progn foo-bar (calc "foo-bar+1")))
 0
 )

(defbbltest=
 calc13
 (let ((foo 1) (bar 2) (foo-bar 3)) 
   (progn foo bar (calc "foo-bar+1" :unary-and-whitespace)))
 4
 )

(defbbltest=
 calc14
 (let ((foo 1) (bar 2) (foo-bar 3)) 
   (progn foo bar (calc "foo\\-bar+1")))
 4
 )

(defbbltest=
 calc15
 (let ((x 5) (y- 8)) (calc "x+y-" :only-with-whitespace))
 13
 )

(defbbltest=
 calc16
 (let ((x 5) (y- 8)) (calc "x+y-" :unary-and-whitespace))
 13
 )

(defbbltest=
 calc17
 (let ((a 2)) (calc (-a+ (a^^3))))
 6
 )

(defbbltest=
 calc18
 (let ((a 5)) (calc (sqrt (a) +a/5)))
 3.236068
 )

(defbbltest=
 calc19
 (let ((x 6)) (calc 1-x-10))
 -15
 )

(defbblerrortest=
 e-calc1
 (lisp:eval (lisp:macroexpand-1 '(let ((x- 6)) (progn x- (bbi::calc "x-")))))
 error
 )

(defbblerrortest=
 e-calc2
 (lisp:eval 
  (lisp:macroexpand-1
   '(let ((x 5) (y 6) (-y 8)) (progn x y -y (bbi::calc "x-y" :unary-and-whitespace)))
   ))
 error
 )

(defbblerrortest=
 e-calc3
 (lisp:eval 
  (lisp:macroexpand-1 
   '(let ((x 5) (y 8)) (progn x y (bbi::calc "4+x-y" :only-with-whitespace)))
   ))
 error
 )

(defbblerrortest=
 e-calc4
 (lisp:eval 
  (lisp:macroexpand-1
   '(let ((x 5) (y- 8)) (progn x y (bbi::calc "x+y-" :always)))
   ))
 error
 )

(defbbltest= sortbbl1
  (bbl::sort '((1 2 3 1) (4 3 2 4) (2 4 3 1)))
  '((1 2 3 1) (2 4 3 1) (4 3 2 4)))

(defbbltest= sortbbl2
  (bbl::sort '((1 2 3 1) (4 3 2 4) (2 4 3 1)) descending)
  '((4 3 2 4) (2 4 3 1) (1 2 3 1)))

(defbbltest= sortbbl3
  (bbl::sort '((1 2 3 1) (4 3 2 4) (2 4 3 1)) by-position 3)
  '((4 3 2 4) (1 2 3 1) (2 4 3 1)))

(defbbltest= sortbbl4
  (bbl::sort '((1 2 3 1) (4 3 2 4) (2 4 3 1)) by-position 2 descending)
  '((2 4 3 1) (4 3 2 4) (1 2 3 1)))

(defbbltest= sortbbl5
  (bbl::sort '((1 2 3 1) (4 3 2 4) (2 4 3 1)) by-position 3 by-position 1)
  '((4 3 2 4) (1 2 3 1) (2 4 3 1)))

(defbbltest= sortbbl6
  (bbl::sort '((1 2 3 1) (4 3 2 4) (2 4 3 1))
            by-position 3 :then-sort-ascending-by 1)
  '((4 3 2 4) (1 2 3 1) (2 4 3 1)))

(defbbltest= sortbbl7
  (bbl::sort '((1 2 3 1) (4 3 2 4) (2 4 3 1))
                 by-position 3 descending by-position 1 ascending)
  '((1 2 3 1) (2 4 3 1) (4 3 2 4)))

(defbbltest= sortbbl8
  (bbl::sort '((1 2 3 1) (4 3 2 4) (2 4 3 1))
            by-position 4 descending
            by-position 3 ascending
            by-position 1)
  '((4 3 2 4) (1 2 3 1) (2 4 3 1)))

(defbbltest= sortbbl9
  (bbl::sort '((3 "A")(2 "b") (1 "C")) BY-POSITION 2 CASE-SENSITIVE)
  '((3 "A") (1 "C") (2 "b")))

(defbbltest= sortbbl10
  (bbl::sort 
   '(("A" "b" "c" "a") ("d" "c" "b" "d") ("a" "d" "c" "a")) descending)
  '(("d" "c" "b" "d") ("a" "d" "c" "a") ("A" "b" "c" "a")))

(defbbltest= sortbbl11
  (bbl::sort 
   '(("A" "b" "c" "a") ("d" "c" "b" "d") ("a" "d" "c" "a")) 
   position 1 descending case-insensitive :then-sort-descending-by 2)
  '(("d" "c" "b" "d") ("a" "d" "c" "a") ("A" "b" "c" "a")))


#+error-at-compilation-time
(defbblerrortest= esortbbl1
  (bbl::sort '((1 2 3 1) (4 3 2 4) (2 4 3 1)) descending by-position 2)
  error
  )
  
(defbbltest= 
    align1
  (ecase (user::os?)
    (:unix (alignment-of '("ACTACCTACTA" "ACACTACTAC" "ACACACATCT")))
    (:windows nil))
  (ecase (user::os?)
    (:unix
     '(("Seq 2:ACACT" "--ACACTACTAC") ("Seq 3:ACACA" "ACACACATCT--")
       ("Seq 1:ACTAC" "ACTACCTACTA-") ("consensus" "     *  **  ")))
    (:windows nil)
    ))
 

#||
m atg
a gcc
r aga
k aaa 
g ggt ggg
h cat cac
i att ata

sequence ghimarkghi
"ggtcatattatggccagaaaagggcacata"
||#

;; test relative values
(defbbltest=
 cfo1
 (let* ((seq "ggtcatattatggccagaaaagggcacata")
        (freqcomp 
         (codon-frequencies-of seq)))
   (lisp:flet ((comp (p) (mapcar 'float (ref freqcomp p))))
     (let ((g-comp (comp 6))
           (h-comp (comp 7))
           (i-comp (comp 8))
           (m-comp (comp 11))
           (a-comp (comp 1))
           (r-comp (comp 15))
           (k-comp (comp 9)))
       (and
        (equalp g-comp '(0.5 0.0 0.0 0.5))
        (equalp h-comp '(0.5 0.5))
        (equalp i-comp '(0.5 0.0 0.5))
        (equalp m-comp '(1.0))
        (equalp a-comp '(0.0 1.0 0.0 0.0))
        (equalp r-comp '(0.0 0.0 0.0 0.0 1.0 0.0))
        (equalp k-comp '(1.0 0.0))
        ))))
 t
 )

;; test absolute values
(defbbltest=
 cfo2
 (let* ((seq "ggtcatattatggccagaaaagggcacata")
        (freqcomp 
         (codon-frequencies-of seq absolute)))
   (lisp:flet ((comp (p) (mapcar 'float (ref freqcomp p))))
     (let ((g-comp (comp 6))
           (h-comp (comp 7))
           (i-comp (comp 8))
           (m-comp (comp 11))
           (a-comp (comp 1))
           (r-comp (comp 15))
           (k-comp (comp 9)))
       (and
        (equalp g-comp '(0.1 0.0 0.0 0.1))
        (equalp h-comp '(0.1 0.1))
        (equalp i-comp '(0.1 0.0 0.1))
        (equalp m-comp '(0.1))
        (equalp a-comp '(0.0 0.1 0.0 0.0))
        (equalp r-comp '(0.0 0.0 0.0 0.0 0.1 0.0))
        (equalp k-comp '(0.1 0.0))
        ))))
 t
 )

;; testing labeled
(defbbltest=
 cfo3
 (let* ((seq "ggtcat")
        (compfreq (codon-frequencies-of seq labeled)))
   (equalp seq (first compfreq)))
 t
 )

;; testing label-frequencies
(defbbltest=
    cfo4
 (let* ((seq "ggtcatattatggccagaaaagggcacata")
        (freqcomp 
         (codon-frequencies-of seq label-frequencies)))
   (lisp:flet ((comp (p) 
                 (mapcar 
                  (lambda (x) (list (first x) (float (second x))))
                  (ref freqcomp p)
                  )))
     (let ((g-comp (comp 6))
           (h-comp (comp 7))
           (i-comp (comp 8))
           (m-comp (comp 11))
           (a-comp (comp 1))
           (r-comp (comp 15))
           (k-comp (comp 9)))
       (and
        (equalp g-comp '(("GGT" 0.5) ("GGC" 0.0) ("GGA" 0.0) ("GGG" 0.5)))
        (equalp h-comp '(("CAT" 0.5) ("CAC" 0.5)))
        (equalp i-comp '(("ATT" 0.5) ("ATC" 0.0) ("ATA" 0.5)))
        (equalp m-comp '(("ATG" 1.0)))
        (equalp a-comp '(("GCT" 0.0) ("GCC" 1.0) ("GCA" 0.0) ("GCG" 0.0)))
        (equalp 
         r-comp
         '(("CGT" 0.0) ("CGC" 0.0) ("CGA" 0.0)
           ("CGG" 0.0) ("AGA" 1.0) ("AGG" 0.0)))
        (equalp k-comp '(("AAA" 1.0) ("AAG" 0.0)))
        ))))
 t
 )

;; testing label-frequencies and absolute
(defbbltest=
 cfo5
 (let* ((seq "ggtcatattatggccagaaaagggcacata")
        (freqcomp 
         (codon-frequencies-of seq label-frequencies absolute)))
   (lisp:flet ((comp (p) 
                 (mapcar 
                  (lambda (x) (list (first x) (float (second x))))
                  (ref freqcomp p)
                  )))
     (let ((g-comp (comp 6))
           (h-comp (comp 7))
           (i-comp (comp 8))
           (m-comp (comp 11))
           (a-comp (comp 1))
           (r-comp (comp 15))
           (k-comp (comp 9)))
       (and
        (equalp g-comp '(("GGT" 0.1) ("GGC" 0.0) ("GGA" 0.0) ("GGG" 0.1)))
        (equalp h-comp '(("CAT" 0.1) ("CAC" 0.1)))
        (equalp i-comp '(("ATT" 0.1) ("ATC" 0.0) ("ATA" 0.1)))
        (equalp m-comp '(("ATG" 0.1)))
        (equalp a-comp '(("GCT" 0.0) ("GCC" 0.1) ("GCA" 0.0) ("GCG" 0.0)))
        (equalp 
         r-comp
         '(("CGT" 0.0) ("CGC" 0.0) ("CGA" 0.0) 
           ("CGG" 0.0) ("AGA" 0.1) ("AGG" 0.0)))
        (equalp k-comp '(("AAA" 0.1) ("AAG" 0.0)))
        ))))
 t
 )

;; testing labeled and absolute
(defbbltest=
 cfo6
 (let* ((seq "ggtcatattatggccagaaaagggcacata")
        (freqcomp 
         (codon-frequencies-of seq labeled absolute))
        (label (first freqcomp))
        (fc (second freqcomp)))
   (lisp:flet ((comp (p) (ref fc p)))
     (let ((g-comp (comp 6))
           (h-comp (comp 7))
           (i-comp (comp 8))
           (m-comp (comp 11))
           (a-comp (comp 1))
           (r-comp (comp 15))
           (k-comp (comp 9)))
       (and
        (equalp label seq)
        (equalp g-comp '(0.1 0.0 0.0 0.1))
        (equalp h-comp '(0.1 0.1))
        (equalp i-comp '(0.1 0.0 0.1))
        (equalp m-comp '(0.1))
        (equalp a-comp '(0.0 0.1 0.0 0.0))
        (equalp r-comp '(0.0 0.0 0.0 0.0 0.1 0.0))
        (equalp k-comp '(0.1 0.0))
        ))))
 t)



(defbbltest=
 pm1
 (bbi::matches-of-pattern  "abc" in "dabcd")
 '((2 4 "abc"))
 )

(defbbltest=
 pm2
 (bbi::matches-of-pattern  "a##" in "da23bcd")
 '((2 4 "a23"))
 )

(defbbltest=
 pm3
 (bbi::matches-of-pattern  "#{2,3}" in "abc123abc456abc")
 '((4 6 "123") (10 12 "456"))
 )

(defbbltest=
 pm4
 (bbi::matches-of-pattern  "#{2,3}" in "1abc123abc456abc")
 '((5 7 "123") (11 13 "456"))
 )

(defbbltest=
 pm5
 (bbi::matches-of-pattern  "#{2,}" in "a1b23c45612345")
 '((4 5 "23") (7 14 "45612345"))
 )

(defbbltest=
 pm6
 (bbi::matches-of-pattern  "#*" in "abc123abc456abc")
 '((4 5 "12") (6 7 "3a") (10 11 "45") (12 13 "6a"))
 )

(defbbltest=
 pm7
 (bbi::matches-of-pattern  "#{2}" in "12345a1b23c456")
 '((1 2 "12") (3 4 "34") (9 10 "23") (12 13 "45"))
 )

(defbbltest=
 pm8
 (bbi::matches-of-pattern  "#{5}" in "a1b23c45612345")
 '((7 11 "45612"))
 )

(defbbltest=
 pm9
 (bbi::matches-of-pattern  "#|ab" in "ab3")
 '((1 2 "ab") (3 3 "3"))
 )

(defbbltest=
 pm10
 (bbi::matches-of-pattern  "#|ab" in "3ab")
 '((1 1 "3") (2 3 "ab"))
 )

(defbbltest=
 pm11
 (bbi::matches-of-pattern  "<3>" in "abcde")
 nil
 )

(defbbltest=
    pm12
  (bbi::matches-of-pattern  "ab*" in "ababababa")
  '((1 3 "aba") (5 7 "aba"))
  )

(defbbltest=
    pm13
  (bbi::matches-of-pattern "a(b(c))d" in "aabcdd")
  '(((2 5 "abcd") (3 4 "bc") (4 4 "c")))
  )

(defbbltest=
    pm14
  (bbi::matches-of-pattern "$=$=$" "<A=b=5>")
  '((2 6 "A=b=5"))
  )
    

(defbbltest=
    pm15
  (let ((pattern "($$$####) ...(#...) ...(#...) ...([BF])")
        (line "all0002      981    1718   B"))
    (bbi::matches-of-pattern pattern in line))
  '(((1 28 "all0002      981    1718   B") (1 7 "all0002") (14 16 "981")
     (21 24 "1718") (28 28 "B")))
  )

(defbbltest=
 pm16
 (bbi::matches-of-pattern "12[abc]34" in "123412d3412c34")
 '((10 14 "12c34")))

(defbbltest=
 pm17
 (bbi::matches-of-pattern "12[~abc]34" in "123412d3412c34")
 '((5 9 "12d34")))

(defbbltest=
 pm18
 (bbi::matches-of-pattern "ab[1-3]cd" "abcdab5cdab1cd")
 '((10 14 "ab1cd")))

(defbbltest=
 pm19
 (bbi::matches-of-pattern "ab[~1-3]cd" "abcdab5cdab1cd")
 '((5 9 "ab5cd")))

(defbbltest=
 pm20 
 (bbi::matches-of-pattern "ab~#c" "ab4cabcc")
 '((5 8 "abcc")))

(defbbltest=
 pm21
 (bbi::matches-of-pattern "1$2" "1<2192")
 '((4 6 "192")))

(defbbltest=
 pm22
 (bbi::matches-of-pattern "1~$2" "1921<2")
 '((4 6 "1<2")))

(defbbltest=
 pm23
 (bbi::matches-of-pattern "^" "	3 4")
 '((1 1 "	") (3 3 " ")))

(defbbltest=
 pm24
 (bbi::matches-of-pattern "~^" "	3 4")
 '((2 2 "3") (4 4 "4")))

(defbbltest=
 pm25
 (bbi::matches-of-pattern "ab?c" "ac")
 '((1 2 "ac" "F")))

(defbbltest=
 pm26
 (bbi::matches-of-pattern "ab...c" "abbbbc")
 '((1 6 "abbbbc")))

(defbbltest=
 pm27
 (bbi::matches-of-pattern "ab?...c" "acabcabbbc")
'((1 2 "ac") (3 5 "abc") (6 10 "abbbc")))

(defbbltest=
 pm28
 (bbi::matches-of-pattern "`*" "a*c")
 '((2 2 "*")))

(defbbltest=
 pm29
 (bbi::matches-of-pattern "`^" "a^c")
'((2 2 "^")))

;; Assumes a DNA strand and tests both forward and backwards.
;; In this case only the backwards (and complementary) sequence matches.
(defbbltest=
    pm30
  (bbi::matches-of-pattern "acct" "atgaggtgga")
  '((7 2 "acct" "B")))

;; Find all characters up to the first '>' 
(defbbltest=
    pm31
  (bbi::matches-of-pattern "<*..>" "<U>sentence</U>")
  '((1 3 "<U>") (12 15 "</U>")))

;; Find all characters up to the last '>'
(defbbltest=
    pm32
  (bbi::matches-of-pattern "<*...>" "<U>sentence</U>")
  '((1 15 "<U>sentence</U>")))
 





    


#||

Tests to be written:

genes-described-by 

matches-any-term
matches-all-terms

organism/s-in-group...how to broad test

information-about-genes...only from vpl

||#
   
 
   
   
 
             
