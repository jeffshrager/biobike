;;; -*- Package: bbl-test-user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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


(defreplacetest= 
 replace1
 (replace :into "abcabc" :replacing-every "a" :with "d")
 "dbcdbc")

(defreplacetest= 
 replace1l
 (replace :into '(1 2 1 2) :replacing-every 1 :with 9)
 '(9 2 9 2))

(defreplacetest= 
 replace1a
 (replace "abcabc" :replacing-every "a" :with "d")
 "dbcdbc")

(defreplacetest= 
 replace1al
 (replace '(1 2 1 2) :replacing-every 1 :with 9)
 '(9 2 9 2))

(defreplacetest= 
 replace1b
 (replace "abcabc" :replacing-every "a" :with "d" :strict)
 "dbcdbc")

(defreplacetest= 
 replace1bl
 (replace '(1 2 1 2) :replacing-every 1 :with 2 :strict)
 '(2 2 2 2))

(defreplacetest= 
 replace1c
 (replace "abcabc" :with "d" :replacing-every "a" :strict)
 "dbcdbc")

(defreplacetest= 
 replace1cl
 (replace '(1 2 3 1) :with 99 :replacing-every 1 :strict)
 '(99 2 3 99))

(defreplacetest= 
 replace2
 (replace into-each '("abcabc" "aaa") replacing-every "a" with "d")
 '("dbcdbc" "ddd"))

(defreplacetest= 
 replace2l
 (replace :into-each '((1 2 3) (1 8 1)) :replacing-every 1 :with 400)
 '((400 2 3) (400 8 400)))

(defreplacetest= 
 replace3
 (replace :into-each '("abcabc" "defdef") :replacing-each '("a" "d")
                       :with-each '("x" "y"))
 '("xbcxbc" "yefyef"))

(defreplacetest= 
 replace3l
 (replace :into-each '((1 2 1 2) (3 4 3 4)) replacing-each '(1 4) 
          :with-each '("x" "y"))
 '(("x" 2 "x" 2) (3 "y" 3 "y")))

(defreplacetest= 
 replace4
 (replace :into "rstrst" :replacing-every "s"
          :from 3 :with "a")
 "rstrat")

(defreplacetest= 
 replace4l
 (replace :into '(1 2 3 1 2 3) :replacing-every 2  :with "a"
          :from 3)
 '(1 2 3 1 "a" 3))

(defreplacetest= 
 replace5
 (replace :into "rstrst" :replacing-every "s"
                   :to 3 :with "a")
 "ratrst")

(defreplacetest= 
 replace5l
 (replace :into '(1 2 3 1 2 3) :replacing-every 2
         :to 3 :with "a")
 '(1 "a" 3 1 2 3))


;; changed FROM from 2 to NIL just for a minute
(defreplacetest= 
 replace7
 (replace :into-each '("kangaroo" "aardvark")
                   :replacing-each '("o" "a") :with "x")
 '("kxngxrxx" "xxrdvxrk"))

(defreplacetest= 
 replace7l
 (replace :into-each '((1 2 3 1 2 3) (4 5 6 4 5 6))
                   :replacing-each '(2 5) :with "x")
 '((1 "x" 3 1 "x" 3) (4 "x" 6 4 "x" 6)))

(defreplacetest= 
 replace8
 (replace :into-copy-of-each '("blueberry" "blackberry")
                   :replacing-every "berry" :with "yum")
 '("blueyum" "blackyum"))

(defreplacetest= 
 replace8l
 (replace :into-copy-of-each '((1 2 3 1 2 3) (2 4 5))
                   :replacing-every 2 :with "h")
 '((1 "h" 3 1 "h" 3) ("h" 4 5)))

(defreplacetest= 
 replace9
 (replace :into-copy-of "abcdef"
          :replacing-each '("a" "d") :with "xy")
 "xybcxyef")

(defreplacetest= 
 replace9l
 (replace :into-copy-of '(1 2 3 1 2 3)
                   :replacing-each '(1 3) :with "xy")
 '("xy" 2 "xy" "xy" 2 "xy"))

(defreplacetest= 
 replace10
 (replace :into "abcdef"
          :replacing-each '("a" "d") :with "xy")
 "xybcxyef")

(defreplacetest= 
 replace10l
 (replace :into '("a" "b" "c" "d")
          :replacing-each '("a" "d") :with "xy")
 '("xy" "b" "c" "xy"))

(defreplacetest= 
 replace11
 (replace :into "common lisp"
                        :replacing-first "m" :with "g")
 "cogmon lisp")

(defreplacetest= 
 replace11l
 (replace :into '(9 10 11 11 11)
          :replacing-first 11 :with "g")
 '(9 10 "g" 11 11))

(defreplacetest= 
 replace12
 (replace :into "common lisp"
          :replacing-first "m" :from 4 :with "g")
 "comgon lisp")

(defreplacetest= 
 replace12l
 (replace :into '(9 10 11 11 11)
          :replacing-first 11 :from 4 :with "g")
 '(9 10 11 "g" 11))

(defreplacetest= 
 replace13
 (replace :into "common lisp"
          :replacing-first "m" :from 3 :with "g")
 "cogmon lisp")

(defreplacetest= 
 replace13l
 (replace :into '(9 10 11 11 11)
          :replacing-first 11 :from 3 :with "g")
 '(9 10 "g" 11 11))

;; check whether target is string (so character) or list (so string)
(defreplacetest= 
 replace14
 (replace :into '(1 2 3 4 5)
          :replacing-first 1 :with "g")
 '("g" 2 3 4 5))

(defreplacetest= 
 replace15
 (replace :into '(1 2 3 1 5)
          :replacing-every 1 :with 45)
 '(45 2 3 45 5))

(defreplacetest= 
 replace16
 (replace :into '(1 2 3 1 2 3)
          :replacing-each '(1 2 3) :from 2 :to 4
          :with-each  '("one" "two" "three"))
 '(1 "two" "three" "one" 2 3)
 )
 
(defreplacetest= 
 replace17
 (replace :into-each '("abcabc" "aaa") :replacing-every "a"
          :from 1 :to 2 :with "d")
 '("dbcabc" "dda"))

(defreplacetest= 
 replace17l
 (replace :into-each '((1 2 3 4) (1 1 1)) :replacing-every 1
          :from 1 :to 2 :with "d")
 '(("d" 2 3 4) ("d" "d" 1)))


(defreplacetest= 
 replace18
 (replace
  :into "xyzzy" :from 1 :to 3 :with "q")
 "qqqzy"
 )

(defreplacetest= 
 replace18l
 (replace
  :into '(1 2 3 4) :from 1 :to 3 :with "q")
 '("q" "q" "q" 4)
 )

(defreplacetest= 
 replace19
 (replace
  :into "xyzzy" :from 1 :to 3 :with "qz")
 "qzzy"
 )

(defreplacetest= 
 replace19l
 (replace
  :into '(1 2 3 4) :from 1 :to 3 :with "qz")
 '("qz" "qz" "qz" 4)
 )

(defreplacetest= 
 replace19a
 (replace
  :into "xyzzy" :from 1 :to 1 :with "qz")
 "qzyzzy"
 )

(defreplacetest= 
 replace19al
 (replace
  :into '(a b c d):from 1 :to 1 :with "qz")
 '("qz" b c d))

(defreplacetest= 
 replace19b
 (replace
  :into "xyzzy" :from 3 :to 3 :with "qz")
 "xyqzzy"
 )

(defreplacetest= 
 replace19bl
 (replace
  :into '(1 2 3 4) :from 3 :to 3 :with "qz")
 '(1 2 "qz" 4))

(defreplacetest= 
 replace19c
 (replace
  :into "xyzzy":from 5 :to 5 :with "qz")
 "xyzzqz"
 )

(defreplacetest= 
 replace19cl
 (replace
  :into '(1 2 3 4 5) :from 5 :to 5 :with "qz")
 '(1 2 3 4 "qz")
 )

(defreplacetest= 
 replace19d
 (replace
  :into "xyzzy" :from 5 :to 5 :with #\a)
 "xyzza"
 )

(defreplacetest= 
 replace19dl
 (replace
  :into '(1 2 3 4 5 6) :from 5 :to 5 :with #\a)
 '(1 2 3 4 #\a 6)
 )

(defreplacetest= 
 replace19e
 (replace
  :into "xyzzy" :from 5 :to 5 :with 'a)
 "xyzzA"
 )

(defreplacetest= 
 replace19el
 (replace
  :into '(1 2 3 4 5) :from 5 :to 5 :with 'a)
 '(1 2 3 4 a)
 )

(defreplacetest= 
 replace19f
 (replace
  :into "abcabc" :replacing-first "b" :with 1)
 "a1cabc"
 )

(defreplacetest= 
 replace19fl
 (replace
  :into '("a" "b" "c") :replacing-first "b" :with 1)
 '("a" 1 "c")
 )

(defreplacetest= 
 replace20
 (replace
  :into "xyzzy" :From 4 :to 6 :with "q")
 "xyzqq"
 )

(defreplacetest= 
 replace20l
 (replace
  :into '(1 2 3 4 5) :from 4 :to 6 :with "q")
 '(1 2 3 "q" "q")
 )

(defreplacetest=
 replace21
 (replace :into "xyzzy" :replacing-every "x" :from 2 :to 4 
          :with "q")
 "xyzzy")

(defreplacetest=
 replace21l
 (replace :into '("x" 2 3 4 "x") :replacing-every "x" :from 2 :to 4 
          :with "q")
 '("x" 2 3 4 "x"))

(defreplacetest=
 replace22
 (replace :into "xyzzy" :replacing-first "x" :from 0 :to 2 
          :with "q")
 "qyzzy")

(defreplacetest=
 replace22l
 (replace :into '("x" "x" 3 4 5) :replacing-first "x" :From 0 :to 2 
          :with "q")
 '("q" "x" 3 4 5))

(defreplacetest= 
 replace23
 (replace :into "1abc2abc3abc4" :replacing-every "abc" 
          :with "z")
 "1z2z3z4")

;; test for prevention of infinite recursion
(defreplacetest= 
 replace23a
 (replace :into "1abc2abc3abc4" :replacing-every "abc" 
          :with "abc")
 "1abc2abc3abc4")

(defreplacetest= 
 replace23b
 (replace :into "1abc2abc3abc4" :replacing-every "b" 
          :with "b")
 "1abc2abc3abc4")

(defreplacetest= 
 replace23l
 (replace :into '((1 2) 1 (1 2)) :replacing-every '(1 2)
          :with "z")
 '("z" 1 "z"))

(defreplacetest= 
 replace24
 (replace :into "1abc2abc3abc4" :replacing-every "abc" 
          :with "defg")
 "1defg2defg3defg4")

(defreplacetest= 
 replace24l
 (replace :into '((1 2) 1 (1 2)) :replacing-every '(1 2) 
          :with '(1 2 3))
 '((1 2 3) 1 (1 2 3)))
  
(defreplacetest= 
 replace25
 (replace :into "xyzzy" :replacing-every "a" 
          :with-each '("9" "8"))
 '("xyzzy" "xyzzy"))

(defreplacetest= 
 replace25a
 (replace :into '(1 2 3) :replacing-every 4
           :with-each '("9" "8"))
 '((1 2 3) (1 2 3))
 )

(defreplacetest= 
 replace25l
 (replace :into '(1 2 3) :replacing-every 3
          :with-each '("9" "8"))
 '((1 2 "9") (1 2 "8")))
                        
(defreplacetest=
 replace26
 (replace :into "abcabc" :replacing-every "c"
          :from 4 :to 8 :with "z" :relaxed)
 "abcabz")

(defreplacetest=
 replace26l
 (replace :into '(1 2 3 1 2 3) :replacing-every 2
          :from 4 :to 8 :with 44 :relaxed)
 '(1 2 3 1 44 3))

(defreplacetest=
 replace27
 (replace :into "" :replacing-every "c"
          :from 4 :to 8 :with "z" :relaxed)
 "")

(defreplacetest=
 replace27l
 (replace :into '() :replacing-every "c"
          :from 4 :to 8 :with "z" :relaxed)
 nil)

(defreplacetest=
 replace29
 (replace :into "abc" :replacing-first "c" :with "z"
          :from -2 :to 5 :relaxed)
 "abz")

(defreplacetest=
 replace29l
 (replace :into '(1 2 3) :replacing-first 3
          :from -2 :to 5 :with 900 :relaxed)
 '(1 2 900))

(defreplacetest=
    replace31
  (replace :into "abc" :replacing-first "c"
           :from 1 :to bbi::*end* :with "z" :relaxed)
  "abz")

(defreplacetest=
 replace31l
 (replace :into '(1 2 3 5) :replacing-first 3
          :from 1 :to bbi::*end* :with 6 :relaxed)
 '(1 2 6 5))

(defreplacetest=
 replace32
 (replace :into "abc" :replacing-first "c"
          :from 1 :to bbi::*end* :with "z" :strict)
 "abz")

(defreplacetest=
 replace32l
 (replace :into '(7 8 9) :replacing-first 8
                        :from 1 :to bbi::*end* :with "z" :strict)
 '(7 "z" 9))

(defreplacetest=
    replace34
  (replace :into "abcabca" :replacing-every "a"
            :with "zq" :strict)
  "zqbczqbczq")

(defreplacetest=
 replace34l
 (replace :into '(1 2 3 1 2 3) :replacing-every 2
          :with '(8 8) :strict)
 '(1 (8 8) 3 1 (8 8) 3))

(defreplacetest=
 replace35
 (replace :into "xyzzy" :replacing-every "" :with "a")
 "xyzzy")

(defreplacetest=
 replace35l
 (replace :into '(1 2 3) :replacing-every nil :with 400)
 '(1 2 3))

(defreplacetest=
 replace35la
 (replace :into '(1 nil 2 3) :replacing-every nil :with 400)
 '(1 400 2 3))

(defreplacetest=
 replace36
 (replace :into "xyzzy" :replacing-first "" :with "a")
 "xyzzy")

(defreplacetest=
 replace36l
 (bbl::replace :into '(1 2 3 2 3) :replacing-first nil :with 8)
 '(1 2 3 2 3))

(defreplacetest=
 replace36la
 (bbl::replace :into '(1 2 nil 3 nil 2 3) :replacing-first nil :with 8)
 '(1 2 8 3 nil 2 3))

(defreplacetest=
 replace37
 (replace :into "xyzzy" :replacing-every "z" :with "")
 "xyy")

(defreplacetest=
 replace37l
 (replace :into '(1 2 3) :replacing-every 2 :with nil)
 '(1 nil 3))

(defreplacetest=
 replace38
 (bbl::replace :into "xyzzy" :replacing-first "z" :with "")
 "xyzy")

(defreplacetest=
 replace38l
 (bbl::replace :into '(1 2 3 1 2) :replacing-first 1 :with nil)
 '(nil 2 3 1 2))

(defreplacetest=
 replace39
 (bbl::replace :into "xyqzy" :replacing-first "z" :with "a" :from 1 :to 3)
 "xyqzy")

(defreplacetest=
 replace39l
 (bbl::replace :into '(1 2 3 4 5) :replacing-first 4 :with "a" :from 1 :to 3)
 '(1 2 3 4 5))

(defreplacetest=
 replace39a
 (bbl::replace :into "xyqzy" :replacing-first "q" :with "a" :from 1 :to 3)
 "xyazy")

(defreplacetest=
 replace39al
 (bbl::replace :into '(1 2 3 4 5) :replacing-first 3 :with "a" :from 1 :to 3)
 '(1 2 "a" 4 5))

(defreplacetest=
 replace40
 (bbl::replace 
  :into "abcdef" :replacing-every "CD" :with "xxxxx")
 "abxxxxxef")

(defreplacetest=
 replace40a
 (bbl::replace 
  :into "abcdef" :replacing-every "CD" :with "xxxxx" case-sensitive)
 "abcdef")

(defreplacetest=
 replace40b
 (bbl::replace 
  into "abCDef" :replacing-every "cd" with "xxxxx")
 "abxxxxxef")

(defreplacetest=
 replace40c
 (bbl::replace 
  :into "abCDef" :replacing-every "cd" :with "xxxxx" case-sensitive)
 "abCDef")

(defreplacetest=
 replace41
 (bbl::replace 
  :into '(1 2 "ab" 3) :replacing-each '(2 "AB") :with-each '(5 "cd"))
 '(1 5 "cd" 3))

(defreplacetest=
 replace41a
 (bbl::replace 
  :into '(1 2 "ab" 3) :replacing-each '(2 "AB") 
  with-each '(5 "cd") case-sensitive)
 '(1 5 "ab" 3))

(defreplacetest=
 replace42
 (bbl::replace
  :into "abcabcabc" :replacing-first "cab" :with "xxx"
  :from 5 :to 9)
 "abcabxxxc")

(defreplacetest=
 replace43
 (bbl::replace
  :into "aaBbcc" replacing-every "B" :with-each '("x" "y"))
 '("aaxxcc" "aayycc"))

(defreplacetest=
 replace43a
 (bbl::replace
  :into "aaBbcc" :replacing-every "B" :with-each '("x" "y") case-sensitive)
 '("aaxbcc" "aaybcc"))

(defreplacetest=
 replace43l
 (bbl::replace
  :into '(1 "a" "A") :replacing-every "A" :with-each '("x" "y"))
 '((1 "x" "x") (1 "y" "y")))

(defreplacetest=
 replace43la
 (bbl::replace
  :into '(1 "A" "a") :replacing-every "A" :with-each '("x" "y") case-sensitive)
 '((1 "x" "a") (1 "y" "a")))

(defreplacetest=
 replace44
 (bbl::replace 
  :into "AabB" :replacing-first "B" :with "q")
 "AaqB")

(defreplacetest=
 replace44a
 (bbl::replace 
  into "AabB" replacing-first "B" with "q" case-sensitive)
 "Aabq")

(defreplacetest=
 replace44l
 (bbl::replace
  :into '("b" "B") :replacing-first "B" :with "q")
 '("q" "B"))

(defreplacetest=
 replace44la
 (bbl::replace
  :into '("b" "B") :replacing-first "B" :with "q" case-sensitive)
 '("b" "q"))

(defreplacetest=
 replace45zero
 (bbl::replace
  :into "bB" :replacing-first "B" :with "y")
 "yB")

(defreplacetest=
 replace45azero
 (bbl::replace
  :into "bB" :replacing-first "B" :with "y" case-sensitive)
 "by")

(defreplacetest=
    replace45z
  (bbl::replace 
   :into "xyzzy" :replacing-every "Z" :with-each '("a" "b"))
  '("xyaay" "xybby"))

(defreplacetest=
    replace45za
  (bbl::replace 
   :into "xyzzy" :replacing-every "Z" :with-each '("a" "b"))
  '("xyaay" "xybby"))

(defreplacetest=
 replace45
 (bbl::replace
  :into "bB" :replacing-first "B" :with-each '("y" "z"))
 '("yB" "zB"))

(defreplacetest=
 replace45a
 (bbl::replace
  :into "bB" :replacing-first "B" :with-each '("y" "z") case-sensitive)
 '("by" "bz"))

(defreplacetest=
 replace45l
 (bbl::replace
  :into '("b" "B") :replacing-first "B" :with-each '("y" "z"))
 '(("y" "B") ("z" "B")))

(defreplacetest=
 replace45la
 (bbl::replace
  :into '("b" "B") :replacing-first "B" :with-each '("y" "z") case-sensitive)
 '(("b" "y") ("b" "z")))

(defreplacetest=
 replace46
 (bbl::replace
  :into "abcABC" :replacing-each '("A" "B") :with "h")
 "hhchhC")

(defreplacetest=
 replace46a
 (bbl::replace
  :into "abcABC" :replacing-each '("A" "B") :with "h" case-sensitive)
 "abchhC")

(defreplacetest=
 replace46l
 (bbl::replace
  :into '("a" "A" "b" "B") :replacing-each '("A" "B") :with "u")
 '("u" "u" "u" "u"))

(defreplacetest=
 replace46l
 (bbl::replace
  :into '("a" "A" "b" "B") :replacing-each '("A" "B") :with "u" case-sensitive)
 '("a" "u" "b" "u"))

(defreplacetest=
 replace47
 (bbl::replace
  :into "aaAAAa" :replacing-every "Aa" :with "9999")
 "999999999999")

(defreplacetest=
 replace47a
 (bbl::replace
  :into "aaAAAa" :replacing-every "Aa" :with "9999" case-sensitive)
 "aaAA9999")

(defreplacetest=
 replace48
 (bbl::replace
  :into "aaAAAa" :replacing-first "Aa" :with "9999")
 "9999AAAa")

(defreplacetest=
 replace48a
 (bbl::replace
  :into "aaAAAa" :replacing-first "Aa" :with "9999" case-sensitive)
 "aaAA9999")

(defreplacetest=
    replace50
  (let* ((x "xyzzy")
        (y (bbl::replace :into x :replacing-first "z" :with "a")))
    (eq x y))
  t
  )

(defreplacetest=
    replace50l
  (let* ((x '(1 2 3 4))
         (y (bbl::replace :into x :replacing-first 1 :with 9)))
    (eq x y))
  t
  )

(defreplacetest=
    replace50a1
  (let* ((x "xyzzy")
        (y (bbl::replace :into x :replacing-first "zz" :with "a")))
    (eq x y))
  nil
  )

(defreplacetest=
    replace50a2
  (let* ((x "xyzzy")
        (y (bbl::replace :into x :replacing-first "z" :with "aa")))
    (eq x y))
  nil
  )

(defreplacetest=
    replace50a2l
  (let* ((x '(1 2 3 4))
         (y (bbl::replace :into x :replacing-first 2 :with '(3 4))))
    (eq x y))
  t
  )

(defreplacetest=
    replace50a
  (let* ((x "xyzzy"))
    (bbl::replace :into x :replacing-first "z" :with-each '("b" "a"))
    (string= x "xyzzy"))
  t
  )

(defreplacetest=
    replace50al
  (let* ((x '(1 2 3 4)))
    (bbl::replace :into x :replacing-first 3 :with-each '(9 8))
    (equal x '(1 2 3 4)))
  t
  )

(defreplacetest=
    replace50b
  (let* ((x "xyzzy")
         (y (bbl::replace :into x :replacing-every "z" :with "a")))
    (eq x y))
  t
  )

(defreplacetest=
    replace50bl
  (let* ((x '(1 2 3 4))
         (y (bbl::replace :into x :replacing-every 4 :with 9)))
    (eq x y))
  t
  )

(defreplacetest=
    replace50b1
  (let* ((x "xyzzy")
        (y (bbl::replace :into x :replacing-every "zz" :with "a")))
    (eq x y))
  nil
  )

(defreplacetest=
    replace50b2
  (let* ((x "xyzzy")
        (y (bbl::replace :into x :replacing-every "z" :with "aa")))
    (eq x y))
  nil
  )

(defreplacetest=
    replace50b2l
  (let* ((x '(1 2 3 4))
         (y (bbl::replace :into x :replacing-every 2 :with '(1 2))))
    (eq x y))
  t
  )

(defreplacetest=
    replace50c
  (let* ((x "xyzzy")
        (y (bbl::replace :into-copy-of x :replacing-first "z" :with "a")))
    (eq x y))
  nil
  )

(defreplacetest=
    replace50cl
  (let* ((x '(1 2 3 4))
         (y (bbl::replace :into-copy-of x :replacing-first 3 :with 7)))
    (eq x y))
  nil
  )

(defreplacetest=
    replace50d
  (let* ((x "xyzzy"))
    (bbl::replace :into x :replacing-every "z" :with-each '("b" "a"))
    (string= x "xyzzy"))
  t
  )

(defreplacetest=
    replace50dl
  (let* ((x '(1 2 3 4)))
    (bbl::replace :into x :replacing-every 2 :with-each '(7 9))
    (equal x '(1 2 3 4)))
  t
  )

(defreplacetest=
    replace50e
  (let* ((x "xyzzy")
        (y (bbl::replace :into x :replacing-each '("x" "y") :with "a")))
    (eq x y))
  t
  )

(defreplacetest=
    replace50el
  (let* ((x '(1 2 3 4))
         (y (bbl::replace :into x :replacing-each '(2 3) :with 7)))
    (eq x y))
  t
  )

(defreplacetest=
    replace50f
  (let* ((x "xyzzy")
        (y (bbl::replace 
            :into x :replacing-each '("x" "y") :with-each '("a" "b"))))
    (eq x y))
  t
  )

(defreplacetest=
    replace50fl
  (let* ((x '(1 2 3 4))
         (y (bbl::replace 
             :into x :replacing-each '(1 2) :with-each '(9 8))))
    (eq x y))
  t
  )

(defreplacetest=
 replace51
 (replace :into "1234" :at 3 :with "x")
 "12x4")

(defreplacetest=
 replacea51a
 (replace :into "1234" :from 3 :with "x")
 "12xx")

(defreplacetest=
 replace51b
 (replace :into "1234" :to 2 :with "x")
 "xx34")

(defreplacetest=
 replace52
 (bbl::replace :into '(1 2 3 4) :at 3 :with "x")
 '(1 2 "x" 4))

(defreplacetest=
    replace53
  (bbl::replace "email" replacing-first "-" with "")
  "email")

(defreplacetest=
    replace53l
  (bbl::replace '(1 2 3 4) replacing-first 5 with nil)
  '(1 2 3 4))

(defreplacetest=
    replace54
  (bbl::replace into "12346" replacing-every "6" with "56")
  "123456"
  )




(defreplaceerrortest=
 e-replace1
 (replace :into "abcabc" :replacing-every "c"
          :from 4 :to 8 :with "z" :strict)
 error)

(defreplaceerrortest=
 e-replace1l
 (replace :into '(1 2 3 1 2 3) :replacing-every 2
          :from 4 :to 8 :with "z" :strict)
 error)

(defreplaceerrortest=
 e-replace2
 (replace :into "abcabc" :replacing-every "c"
          :from -2 :to 8 :at 16 :with "z" :relaxed)
 error)

(defreplaceerrortest=
 e-replace2l
 (replace :into '(1 2 3 1 2 3) :replacing-every 2
          :from -2 :to 8 :at 16 :with "z" :relaxed)
 error)

(defreplaceerrortest=
 e-replace3
 (replace :into "abcabc" :replacing-every "c"
          :from -2 :to 8 :at 16 :with "z" :strict)
 error)

(defreplaceerrortest=
 e-replace3l
 (replace :into '(1 2 3 1 2 3) :replacing-every 2
          :from -2 :to 8 :at 16 :with "z" :strict)
 error)

(defreplaceerrortest=
 e-replace4
 (replace :into "abcabc" :replacing-every "c"
          :from 4 :to 1 :with "z" :strict)
 error)

(defreplaceerrortest=
 e-replace4l
 (replace :into '(1 2 3 1 2 3) :replacing-every 2
          :from 4 :to 1 :with "z" :strict)
 error)

(defreplaceerrortest=
 e-replace5
 (replace :into "abcabc" :replacing-every "c"
          :from 4 :to 1 :with "z" :relaxed)
 error)

(defreplaceerrortest=
 e-replace5l
 (replace :into '(1 2 3 1 2 3) :replacing-every 2
          :from 4 :to 1 :with "z" :relaxed)
 error)

(defreplaceerrortest=
 e-replace6
 (bbl::replace :into "xyqzy" :replacing-first "q" :with "a" :at 1)
 error)

(defreplaceerrortest=
 e-replace6l
 (bbl::replace :into '(1 2 3 4 5) :replacing-first 3 :with "a" :at 1)
 error)

(defreplaceerrortest=
 e-replace7
 (bbl::replace :into "xyqzy" :replacing-each "q" :with "a" :at 3)
 error)

(defreplaceerrortest=
 e-replace7l
 (bbl::replace :into '(1 2 3 4 5) :replacing-each 3 :with "a" :at 3)
 error)

(defreplaceerrortest=
 e-replace8
 (bbl::replace :into "xyqzy" :replacing-every "q" :with "a" :at 3)
 error)

(defreplaceerrortest=
 e-replace8l
 (bbl::replace :into '(1 2 3 4 5) :replacing-every 3 :with "a" :at 3)
 error)



(defreplacetest= 
 insert1
 (bbl::insert into "" "abc" before *end*)
 "abc")

(defreplacetest= 
 insert1l
 (bbl::insert into nil 3 before *end*)
 '(3))

(defreplacetest= 
 insert2
 (bbl::insert into "" "abc" after *end*)
 "abc")

(defreplacetest= 
 insert2l
 (bbl::insert into nil 3 after *end*)
 '(3))

(defreplacetest= 
 insert3
 (bbl::insert into "" "abc" before 1)
 "abc")

(defreplacetest= 
 insert3l
 (bbl::insert into nil "x" before 1)
 '("x"))

(defreplacetest=
 insert4
 (bbl::insert into "" "abc" after-end)
 "abc")

(defreplacetest=
 insert4l
 (bbl::insert into nil "x" after-end)
 '("x"))

(defreplacetest= 
 insert5
 (bbl::insert into "" "" after-end)
 "")

(defreplacetest= 
 insert5l
 (bbl::insert into nil nil after-end)
 '(()))

(defreplacetest=
 insert6
 (bbl::insert into "abc" "" before 2)
 "abc")

(defreplacetest=
 insert6l
 (bbl::insert into '(1 2 3) nil before 2)
 '(1 nil 2 3))

(defreplacetest= 
insert7
  (bbl::insert into "abc" "" after 2)
  "abc")

(defreplacetest= 
 insert7l
 (bbl::insert into '(1 2 3) nil after 2)
 '(1 2 nil 3))

(defreplacetest= 
 insert8
 (bbl::insert into "abc" "" before 1)
 "abc")

(defreplacetest= 
 insert8l
 (bbl::insert into '(1 2 3) nil before 1)
 '(nil 1 2 3))

(defreplacetest= 
 insert9
 (bbl::insert into "xyzzy" "abc" before 1)
 "abcxyzzy")

(defreplacetest= 
 insert9l
 (bbl::insert into '(1 2 3 4 5) 99 before 1)
 '(99 1 2 3 4 5))

(defreplacetest=
 insert10
 (bbl::insert into "xyzzy" "abc" before 2)
 "xabcyzzy")

(defreplacetest=
 insert10l
 (bbl::insert into '(1 2 3 4 5) 3 before 2)
 '(1 3 2 3 4 5))

(defreplacetest= 
 insert11
 (bbl::insert into "xyzzy" "abc" before *end*)
 "xyzzabcy")

(defreplacetest= 
 insert11l
 (bbl::insert into '(9 8 7) 55 before *end*)
 '(9 8 55 7))

(defreplacetest=
 insert12l
 (bbl::insert into '(12 13 14) 5 after *end*)
 '(12 13 14 5))

(defreplacetest= 
 insert13
 (bbl::insert into "xyzzy" "abc" after-end)
 "xyzzyabc")

(defreplacetest= 
 insert13l
 (bbl::insert into '(1 2 3 4) 123 after-end)
 '(1 2 3 4 123))

(defreplacetest= 
 insert14
 (bbl::insert into "xyzzy" "a" after-end)
 "xyzzya")

(defreplacetest= 
 insert14l
 (bbl::insert into '(1 2 3 4) "a" after-end)
 '(1 2 3 4 "a"))

(defreplacetest=
 insert15
 (bbl::insert into "xyzzy" "a" before 3)
 "xyazzy")

(defreplacetest=
 insert15l
 (bbl::insert into '(1 2 3 4) 99 before 3)
 '(1 2 99 3 4))

(defreplacetest=
 insert16
 (bbl::insert into "xyzzy" "a" after 3)
 "xyzazy")

(defreplacetest=
 insert16l
 (bbl::insert into '(1 2 3 4) "a" after 3)
 '(1 2 3 "a" 4))

(defreplacetest=
 insert17
 (bbl::insert into "xyzzy" #\a after 3)
 "xyzazy")

(defreplacetest=
 insert17l
 (bbl::insert into '(1 2 3 4) #\a after 3)
 '(1 2 3 #\a 4))

(defreplacetest=
 insert18
 (bbl::insert into "xyzzy" "a" before "x")
 "axyzzy")

(defreplacetest=
 insert18l
 (bbl::insert into '("a" "b" "c") 99 before "b")
 '("a" 99 "b" "c"))

(defreplacetest=
 insert18a
 (bbl::insert into "xyzzy" "ab" before "x")
 "abxyzzy")

(defreplacetest=
 insert18al
 (bbl::insert into '("a" "b" "c") "ab" before "c")
 '("a" "b" "ab" "c"))

(defreplacetest=
 insert19
 (bbl::insert into "xyzzy" "a" after "z")
 "xyzazy")

(defreplacetest=
 insert19l
 (bbl::insert into '("a" "b" "c") 23 after "a")
 '("a" 23 "b" "c"))

(defreplacetest=
 insert20
 (bbl::insert into "xyzzy" "auuu" after "Z")
 "xyzauuuzy")

(defreplacetest=
 insert20l
 (bbl::insert into '("a" "b" "c") '(2 3) after "b")
 '("a" "b" (2 3) "c"))


(defreplacetest=
 insert21
 (bbl::insert into "xyzZy" "auuu" after "Z" case-sensitive)
 "xyzZauuuy")

(defreplacetest=
 insert21l
 (bbl::insert into '(1 "a" "A") 99 after "A" case-sensitive)
 '(1 "a" "A" 99))

(defreplacetest=
 insert21a
 (bbl::insert into "xyzZy" "auuu" after #\Z case-sensitive)
 "xyzZauuuy")

(defreplacetest=
 insert22
 (bbl::insert into "xyzzy" "auuu" after "Z" case-sensitive)
 "xyzzy")

(defreplacetest=
 insert22l
 (bbl::insert into '(1 "a" "B") 12 after "A" case-sensitive)
 '(1 "a" "B"))

(defreplacetest= 
 insert23
 (bbl::insert into "xyzZy" "auuu" after "zZ" case-sensitive)
 "xyzZauuuy")

(defreplacetest= 
 insert23l
 (bbl::insert into '("a" "A" "B") 12 after "a" case-sensitive)
 '("a" 12 "A" "B"))

(defreplacetest=
 insert24
 (bbl::insert into "xyzZy" "auuu" before "xy" case-sensitive)
 "auuuxyzZy")

(defreplacetest=
 insert24l
 (bbl::insert into '(1 "a" "X" "x") 99 before "x")
 '(1 "a" 99 "X" "x"))

(defreplacetest=
 insert25
 (let* ((x "xyzzy")
        (y (bbl::insert into x "foo" after 1)))
   (eq x y))
 t
 )

(defreplacetest=
 insert25l
 (let* ((x '(1 2 3))
        (y (bbl::insert into x "foo" after 1)))
   (eq x y))
 t
 )

(defreplacetest=
 insert25a
 (let* ((x "xyzzy")
        (y (bbl::insert into-copy-of x "foo" after 1)))
   (eq x y))
 nil
 )

(defreplacetest=
 insert25al
 (let* ((x '(1 2 3))
        (y (bbl::insert into-copy-of x "foo" after 1)))
   (eq x y))
 nil
 )

(defreplacetest=
 insert25b
 (let* ((x "xyzzy")
        (y (bbl::insert into x "foo" before 1)))
   (eq x y))
 t
 )

(defreplacetest=
 insert25bl
 (let* ((x '(1 2 3 4 5))
        (y (bbl::insert into x "foo" before 1)))
   (eq x y))
 t
 )

(defreplacetest=
 insert25c
 (let* ((x "xyzzy")
        (y (bbl::insert into-copy-of x "foo" before 1)))
   (eq x y))
 nil
 )

(defreplacetest=
 insert25cl
 (let* ((x '(1 2 3 "x"))
        (y (bbl::insert into-copy-of x "foo" before 1)))
   (eq x y))
 nil
 )

(defreplacetest=
 insert26
 (bbl::insert into-each '("abcdef" "hihihi") "foo" after 2)
 '("abfoocdef" "hifoohihi"))

(defreplacetest=
 insert26l
 (bbl::insert into-each '((7 8 9) (3 4 5)) 55 after 2)
 '((7 8 55 9) (3 4 55 5)))

(defreplacetest=
 insert26a
 (bbl::insert into-each '("abcdef" "hihihi") each '("foo" "bar") after 2)
 '("abfoocdef" "hibarhihi"))

(defreplacetest=
 insert26al
 (bbl::insert into-each '((7 8 9) (3 4 5)) each '(17 19) after 2)
 '((7 8 17 9) (3 4 19 5)))

(defreplacetest=
 insert26b
 (bbl::insert into "abcdef" each '("foo" "bar") after 2)
 '("abfoocdef" "abbarcdef"))

(defreplacetest=
 insert26bl
 (bbl::insert into '(2 3 4 5) each '("foo" 158) before 2)
 '((2 "foo" 3 4 5) (2 158 3 4 5)))

(defreplacetest=
 insert26c
 (let* ((x "abcdef")
        (y (bbl::insert into x each '("foo" "bar") after 2)))
   (and (string-equal (first y) "abfoocdef")
        (not (string-equal x "abfoocdef"))
        (not (eq (first y) x))))
 t
 )

(defreplacetest=
 insert26cl
 (let* ((x '(2 3 4 5))
        (y (bbl::insert into x each '(11 12) after 2)))
   (and (equal (first y) '(2 3 11 4 5))
        (not (equal x '(2 3 11 4 5)))
        (not (eq (first y) x))))
 t
 )

(defreplacetest=
 insert26d
 (let* ((x "abcdef")
        (y "hihihi")
        (z (bbl::insert into-each (list x y) each '("foo" "bar") after 2)))
   (and (null (eq x (first z))) (null (eq y (second z)))
        (equal z '("abfoocdef" "hibarhihi"))))
 t)

(defreplacetest=
 insert26dl
 (let* ((x '(1 2 3))
        (y '(4 5 6))
        (z (bbl::insert into-each (list x y) each '(10 11) after 2)))
   (and (null (eq x (first z))) (null (eq y (second z)))
        (equal z '((1 2 10 3) (4 5 11 6)))))
 t)

(defreplacetest=
 insert26e
 (let* ((x "abcdef")
        (y "hihihi")
        (z 
         (bbl::insert
          into-copy-of-each (list x y) each '("foo" "bar") after 2)))
   (and (string-equal (first z) "abfoocdef")
        (not (eq x (first z)))
        (string-equal (second z) "hibarhihi")
        (not (eq y (second z)))))
 t)

(defreplacetest=
 insert26el
 (let* ((x '(1 2 3))
        (y '(4 5 6))
        (z 
         (bbl::insert
          into-copy-of-each (list x y) each '(10 11) after 2)))
   (and (equal (first z) '(1 2 10 3))
        (not (eq x (first z)))
        (equal (second z) '(4 5 11 6))
        (not (eq y (second z)))))
 t)

(defreplacetest=
 insert27
 (bbl::insert into "xyzzy" "foo" before "qq")
 "xyzzy")

(defreplacetest=
 insert27l
 (bbl::insert into '(7 6 5 4) "foo" before '(7 6))
 '(7 6 5 4))

(defreplacetest=
    insert28
  (bbl::insert into "xyzzy" "foo" after "q")
  "xyzzy")

(defreplacetest=
 insert28l
 (bbl::insert into '(7 6 5 4) 99 after "j")
 '(7 6 5 4))

(defreplaceerrortest=
 e-insert1
 (bbl::insert into "xyzzy" "1" after 9 strict)
 error)

(defreplaceerrortest=
 e-insert1l
 (bbl::insert into '(4 3 2 1) "1" after 9 strict)
 error)

(defreplaceerrortest=
 e-insert2
 (bbl::insert into "xyzzy" "1" before 0 strict)
 error)

(defreplaceerrortest=
 e-insert2l
 (bbl::insert into '(3 5 7) "1" before 0 strict)
 error)

