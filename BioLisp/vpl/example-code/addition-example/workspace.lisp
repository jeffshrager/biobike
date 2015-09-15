
(IN-PACKAGE :NVPL)

(PROGN (SETQ *VPL-WORKSPACE-HISTORY-INDEX* 0)
       (SETQ *VPL-EXECUTION-HISTORY-INDEX* 3))

(SETF (UVS-WS *VPL-STATE*) 896)

(SETF (UVS-RS *VPL-STATE*) 903)

(SETF (UVS-WSH *VPL-STATE*)
      '(896 8029 7609 7189 6776 6356 5978 5600 5229 4851))

(PROGN (DEFPARAMETER C-ARRAY-97105 (MAKE-ARRAY (LIST 89)))
       (SETF (AREF C-ARRAY-97105 0)
             (LIST :SPROGN (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 1) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 2) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 3) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 4) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 5) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 6)
             (LIST :SPROGN (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 7) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 8)
             (LIST :SPROGN (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 9)
             (LIST :SPROGN (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 10)
             (LIST (LIST :FLAG :DISPLAY-OFF) (LIST :FLAG :LABELED)))
       (SETF (AREF C-ARRAY-97105 11)
             (LIST (LIST :FLAG :DISPLAY-OFF) (LIST :FLAG :LABELED)))
       (SETF (AREF C-ARRAY-97105 12)
             (LIST (LIST :FLAG :DISPLAY-OFF) (LIST :FLAG :LABELED)))
       (SETF (AREF C-ARRAY-97105 13)
             (LIST :SPROGN (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 14) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 15) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 16)
             (LIST :SPROGN (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 17)
             (LIST :SPROGN (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 18) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 19)
             (LIST :SPROGN (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 20)
             (LIST :SPROGN (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 21) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 22) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 23)
             (LIST :SPROGN (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 24)
             (LIST :SPROGN (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 25) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 26)
             (LIST (LIST :FLAG :DISPLAY-OFF) (LIST :FLAG :LABELED)))
       (SETF (AREF C-ARRAY-97105 27)
             (LIST (LIST :FLAG :DISPLAY-OFF) (LIST :FLAG :LABELED)))
       (SETF (AREF C-ARRAY-97105 28)
             (LIST (LIST :FLAG :DISPLAY-OFF) (LIST :FLAG :LABELED)))
       (SETF (AREF C-ARRAY-97105 29) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 30)
             (LIST :SPROGN (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 31)
             (LIST :SPROGN (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 32)
             (LIST (LIST :FLAG :DISPLAY-OFF) (LIST :FLAG :LABELED)))
       (SETF (AREF C-ARRAY-97105 33) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 34) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 35) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 36) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 37) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 38) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 39) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 40)
             (LIST :SPROGN (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 41) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 42)
             (LIST :SPROGN (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 43)
             (LIST :SPROGN (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 44) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 45)
             (LIST (LIST :FLAG :DISPLAY-OFF) (LIST :FLAG :LABELED)))
       (SETF (AREF C-ARRAY-97105 46) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 47) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 48) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 49) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 50) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 51) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 52) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 53) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 54) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 55) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 56) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 57) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 58) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 59)
             (LIST :SPROGN (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 60) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 61)
             (LIST :SPROGN (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 62)
             (LIST :SPROGN (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 63) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 64) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 65) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 66)
             (LIST :SPROGN (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 67)
             (LIST (LIST :FLAG :DISPLAY-OFF) (LIST :FLAG :LABELED)))
       (SETF (AREF C-ARRAY-97105 68) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 69) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 70) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 71) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 72) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 73) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 74)
             (LIST :SPROGN (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 75) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 76) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 77) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 78) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 79)
             (LIST (LIST :FLAG :DISPLAY-OFF) (LIST :FLAG :LABELED)))
       (SETF (AREF C-ARRAY-97105 80) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 81) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 82) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 83) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 84) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 85)
             (LIST :SPROGN (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST))))
       (SETF (AREF C-ARRAY-97105 86) (LIST :FORM "number" 'NUMBER))
       (SETF (AREF C-ARRAY-97105 87) (LIST (LIST :FLAG :DISPLAY-OFF)))
       (SETF (AREF C-ARRAY-97105 88)
             (LIST :SPROGN (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                   (LIST :FORM "number" (LIST 'OR 'NUMBER 'LIST)))))

(DEFPARAMETER S-LIST-97106
    (LIST
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 0)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(7525 7462 (7532)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(5915 5908 (5922 5929)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7042 7035 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(5432 5411 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 1)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(6566 6538 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(4956 4942 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(7532 7525 (7539 7546)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(5922 5915 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL
                         (LIST :GC-MARK NIL :TEMPLATE NIL)
                         '(896 NIL (910 623 2737)) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:SUM-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SUM-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(7049 7035 (7056 7063 7112))
                         '(:LABEL "sum-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 2)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(5439 5411 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 3)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(6573 6489 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(4963 4942 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(7539 7532 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CONTENTS "" :HOLE-OPEN
                               NIL :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(5929 5915 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'TOPLEVEL-RS-SNIPPET NIL
                         (LIST :GC-MARK NIL :TEMPLATE NIL)
                         '(903 NIL (651 602 588)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SUM-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "SUM-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7056 7049 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 4)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(5446 5362 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET (AREF C-ARRAY-97105 5)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(6580 6447 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(4970 4942 (4977 4984))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:PRODUCT-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "PRODUCT-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(7546 7532 (7553 7560 7567))
                         '(:LABEL "product-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 6)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(5936 5873 (5943)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(910 896 (917 1463 931 1694 980))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:PRODUCT-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "PRODUCT-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(7063 7049 (7070 7077 7084))
                         '(:LABEL "product-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET (AREF C-ARRAY-97105 7)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(5453 5320 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var" T)
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST
                                  (LIST :FLAG :DISPLAY-OFF)
                                  (LIST :FLAG :LABELED)))))
                         '(6587 6356 (6594 6601 6608 6615 6720))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(4977 4970 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "PRODUCT-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "PRODUCT-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7553 7546 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(5943 5936 (5950 5957)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(917 910 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "PRODUCT-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "PRODUCT-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7070 7063 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var" T)
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST
                                  (LIST :FLAG :DISPLAY-OFF)
                                  (LIST :FLAG :LABELED)))))
                         '(5460 5229 (5467 5474 5481 5488 5593))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(6594 6587 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '+
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "+"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(4984 4970 (4991 4998 5033 5068))
                         '(:LABEL "+" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CONTENTS NIL :HOLE-OPEN T
                               :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(7560 7546 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(5950 5943 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(7077 7063 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(5467 5460 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :SYMBOL-NODE-TYPE :PLACE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(6601 6587 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "+"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(4991 4984 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 8)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(7567 7546 (7574)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CONTENTS "" :HOLE-OPEN
                               NIL :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(5957 5943 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(931 910 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 9)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(7084 7063 (7091)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :SYMBOL-NODE-TYPE :PLACE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(5474 5460 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(6608 6587 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(4998 4984 (5005 5012 5019 5026))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(2758 2737 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(7574 7567 (7581 7588)) 'NIL)
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 10)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0 1)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF :LABELED) :KEYWORDS
                               NIL :ORDERED-NAMES
                               (LIST :DISPLAY-OFF :LABELED)
                               :SUBTEMPLATES
                               (LIST
                                (LIST :FLAG :DISPLAY-OFF)
                                (LIST :FLAG :LABELED))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST
                                 (LIST :FLAG :DISPLAY-OFF)
                                 (LIST :FLAG :LABELED))))
                         '(5964 5831 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(7091 7084 (7098 7105)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(5481 5460 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(6615 6587 (6622 6629))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5005 4998 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(7581 7574 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(5971 5180 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 4
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 4))
                         '(945 623 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(7098 7091 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(5488 5460 (5495 5502))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6622 6615 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(5012 4998 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 11)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0 1)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF :LABELED) :KEYWORDS
                               NIL :ORDERED-NAMES
                               (LIST :DISPLAY-OFF :LABELED)
                               :SUBTEMPLATES
                               (LIST
                                (LIST :FLAG :DISPLAY-OFF)
                                (LIST :FLAG :LABELED))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST
                                 (LIST :FLAG :DISPLAY-OFF)
                                 (LIST :FLAG :LABELED))))
                         '(2772 2737 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CONTENTS "" :HOLE-OPEN
                               NIL :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(7588 7574 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(7105 7091 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5495 5488 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:SUM-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SUM-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(6629 6615 (6636 6643 6692))
                         '(:LABEL "sum-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(5019 4998 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 12)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0 1)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF :LABELED) :KEYWORDS
                               NIL :ORDERED-NAMES
                               (LIST :DISPLAY-OFF :LABELED)
                               :SUBTEMPLATES
                               (LIST
                                (LIST :FLAG :DISPLAY-OFF)
                                (LIST :FLAG :LABELED))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST
                                 (LIST :FLAG :DISPLAY-OFF)
                                 (LIST :FLAG :LABELED))))
                         '(7595 7420 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 13)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(7112 7049 (7119)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:SUM-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SUM-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(5502 5488 (5509 5516 5565))
                         '(:LABEL "sum-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SUM-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "SUM-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6636 6629 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 14)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(5026 4998 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(7602 6727 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(7119 7112 (7126 7133)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SUM-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "SUM-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5509 5502 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:PRODUCT-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "PRODUCT-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(6643 6629 (6650 6657 6664))
                         '(:LABEL "product-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(5033 4984 (5040 5047 5054 5061))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(7126 7119 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:PRODUCT-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "PRODUCT-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(5516 5502 (5523 5530 5537))
                         '(:LABEL "product-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "PRODUCT-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "PRODUCT-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6650 6643 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5040 5033 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:SUM-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SUM-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(4473 3878 (4480 5180 4494))
                         '(:LABEL "sum-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 15)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(980 910 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:PRODUCT-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "PRODUCT-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(7133 7119 (7140 7147 7154))
                         '(:LABEL "product-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "PRODUCT-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "PRODUCT-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5523 5516 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(6657 6643 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(5047 5033 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL
                         (LIST :GC-MARK NIL :TEMPLATE NIL)
                         '(8029 NIL (8036 8078 8120 8260)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SUM-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "SUM-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(4480 4473 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "PRODUCT-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "PRODUCT-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7140 7133 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CONTENTS "" :HOLE-OPEN
                               NIL :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(5530 5516 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 16)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(6664 6643 (6671)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(5054 5033 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(8036 8029 (8043 8050 8057 8064 8071))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CONTENTS "" :HOLE-OPEN
                               NIL :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(7147 7133 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 17)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(5537 5516 (5544)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(6671 6664 (6678 6685)) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 18)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(5061 5033 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(8043 8036 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 19)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(4494 4473 (4501)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 20)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(7154 7133 (7161)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(5544 5537 (5551 5558)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(6678 6671 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 21)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(5068 4984 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(8050 8036 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(7161 7154 (7168 7175)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(5551 5544 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(6685 6671 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 22)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(5075 4942 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(8057 8036 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(7168 7161 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CONTENTS "" :HOLE-OPEN
                               NIL :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(5558 5544 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 23)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(6692 6629 (6699)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var" T)
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST
                                  (LIST :FLAG :DISPLAY-OFF)
                                  (LIST :FLAG :LABELED)))))
                         '(5082 4851 (5089 5096 5103 5110 5173))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 3
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 3))
                         '(8064 8036 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CONTENTS "" :HOLE-OPEN
                               NIL :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(7175 7161 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 24)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(5565 5502 (5572)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(6699 6692 (6706 6713)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(5089 5082 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 25)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(8071 8036 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 26)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0 1)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF :LABELED) :KEYWORDS
                               NIL :ORDERED-NAMES
                               (LIST :DISPLAY-OFF :LABELED)
                               :SUBTEMPLATES
                               (LIST
                                (LIST :FLAG :DISPLAY-OFF)
                                (LIST :FLAG :LABELED))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST
                                 (LIST :FLAG :DISPLAY-OFF)
                                 (LIST :FLAG :LABELED))))
                         '(7182 7007 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(5572 5565 (5579 5586)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(6706 6699 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :SYMBOL-NODE-TYPE :PLACE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(5096 5082 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(8078 8029 (8085 8092 8099 8106 8113))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(5579 5572 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CONTENTS "" :HOLE-OPEN T
                               :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(6713 6699 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(5103 5082 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(8085 8078 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(4501 4494 (4508 6727)) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CONTENTS "" :HOLE-OPEN
                               NIL :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(5586 5572 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 27)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0 1)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF :LABELED) :KEYWORDS
                               NIL :ORDERED-NAMES
                               (LIST :DISPLAY-OFF :LABELED)
                               :SUBTEMPLATES
                               (LIST
                                (LIST :FLAG :DISPLAY-OFF)
                                (LIST :FLAG :LABELED))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST
                                 (LIST :FLAG :DISPLAY-OFF)
                                 (LIST :FLAG :LABELED))))
                         '(6720 6587 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(5110 5082 (5117 5124))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(8092 8078 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(4508 4501 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 28)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0 1)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF :LABELED) :KEYWORDS
                               NIL :ORDERED-NAMES
                               (LIST :DISPLAY-OFF :LABELED)
                               :SUBTEMPLATES
                               (LIST
                                (LIST :FLAG :DISPLAY-OFF)
                                (LIST :FLAG :LABELED))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST
                                 (LIST :FLAG :DISPLAY-OFF)
                                 (LIST :FLAG :LABELED))))
                         '(5593 5460 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:PRODUCT-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "PRODUCT-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(6727 4501 (6734 7602 6748))
                         '(:LABEL "product-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5117 5110 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(8099 8078 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "PRODUCT-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "PRODUCT-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6734 6727 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:SUM-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SUM-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(5124 5110 (5131 5138 5145))
                         '(:LABEL "sum-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 4
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 4))
                         '(8106 8078 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SUM-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "SUM-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5131 5124 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 29)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(8113 8078 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 30)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(6748 6727 (6755)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CONTENTS NIL :HOLE-OPEN T
                               :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(5138 5124 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(8120 8029 (8127 8134 8141 8148 8253))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(6755 6748 (6762 8022)) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 31)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(5145 5124 (5152)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(8127 8120 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(6762 6755 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(5152 5145 (5159 5166)) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(8134 8120 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(5159 5152 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(8141 8120 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CONTENTS "" :HOLE-OPEN
                               NIL :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(5166 5152 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(8148 8120 (8155 8162))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 32)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0 1)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF :LABELED) :KEYWORDS
                               NIL :ORDERED-NAMES
                               (LIST :DISPLAY-OFF :LABELED)
                               :SUBTEMPLATES
                               (LIST
                                (LIST :FLAG :DISPLAY-OFF)
                                (LIST :FLAG :LABELED))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST
                                 (LIST :FLAG :DISPLAY-OFF)
                                 (LIST :FLAG :LABELED))))
                         '(5173 5082 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(8155 8148 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:PRODUCT-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "PRODUCT-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(5180 4473 (5187 5971 5201))
                         '(:LABEL "product-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '+
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "+"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(8162 8148 (8169 8176 8211 8246))
                         '(:LABEL "+" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "PRODUCT-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "PRODUCT-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5187 5180 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "+"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(8169 8162 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(8176 8162 (8183 8190 8197 8204))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 30)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(5201 5180 (5208)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(8183 8176 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(5208 5201 (5215 6349)) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(8190 8176 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(5215 5208 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(8197 8176 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 33)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(8204 8176 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(8211 8162 (8218 8225 8232 8239))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(8218 8211 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(8225 8211 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(8232 8211 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 34)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(8239 8211 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL
                         (LIST :GC-MARK NIL :TEMPLATE NIL)
                         '(5978 NIL (5985 6027 6069 6209)) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 35)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(8246 8162 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(5985 5978 (5992 5999 6006 6013 6020))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 36)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(8253 8120 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(5992 5985 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var" T)
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST
                                  (LIST :FLAG :DISPLAY-OFF)
                                  (LIST :FLAG :LABELED)))))
                         '(8260 8029 (8267 8274 8281 8288 8435))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL
                         (LIST :GC-MARK NIL :TEMPLATE NIL)
                         '(7609 NIL (7616 7658 7700 7840)) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(5999 5985 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(8267 8260 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(6006 5985 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(7616 7609 (7623 7630 7637 7644 7651))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :SYMBOL-NODE-TYPE :PLACE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(8274 8260 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 3
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 3))
                         '(6013 5985 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(7623 7616 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(8281 8260 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 37)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(6020 5985 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(7630 7616 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(8288 8260 (8295 8302))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(6027 5978 (6034 6041 6048 6055 6062))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(7637 7616 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(8295 8288 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(6034 6027 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 3
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 3))
                         '(7644 7616 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:SUM-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SUM-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(8302 8288 (8309 8316 8365))
                         '(:LABEL "sum-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(6041 6027 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 38)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(7651 7616 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SUM-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "SUM-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(8309 8302 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(6048 6027 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(7658 7609 (7665 7672 7679 7686 7693))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:PRODUCT-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "PRODUCT-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(8316 8302 (8323 8330 8337))
                         '(:LABEL "product-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 4
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 4))
                         '(6055 6027 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(7665 7658 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "PRODUCT-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "PRODUCT-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(8323 8316 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 39)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(6062 6027 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(7672 7658 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(8330 8316 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(6069 5978 (6076 6083 6090 6097 6202))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(7679 7658 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 40)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(8337 8316 (8344)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(6076 6069 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(3878 2737 (3885 4473))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 4
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 4))
                         '(7686 7658 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(8344 8337 (8351 8358)) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(6083 6069 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(3885 3878 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 41)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(7693 7658 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(8351 8344 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(6090 6069 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(7700 7609 (7707 7714 7721 7728 7833))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(8358 8344 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(6097 6069 (6104 6111))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(7707 7700 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 42)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(8365 8302 (8372)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6104 6097 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(7714 7700 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(8372 8365 (8379 8386)) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '+
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "+"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(6111 6097 (6118 6125 6160 6195))
                         '(:LABEL "+" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(7721 7700 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(8379 8372 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "+"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6118 6111 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(7728 7700 (7735 7742))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:PRODUCT-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "PRODUCT-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(8386 8372 (8393 8400 8407))
                         '(:LABEL "product-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(6125 6111 (6132 6139 6146 6153))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7735 7728 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "PRODUCT-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "PRODUCT-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(8393 8386 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6132 6125 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '+
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "+"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(7742 7728 (7749 7756 7791 7826))
                         '(:LABEL "+" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(8400 8386 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(6139 6125 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "+"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7749 7742 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'TOPLEVEL-OUTPUT-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :INPUT-FORM
                               (LIST 'BBI:DEFINE '$$::X '= 3)
                               :ERROR-CONDITION NIL :VALUE-STRINGS
                               (LIST "3") :NVALUES 1 :VALUES (LIST 3)
                               :PRINTOUT "" :RESULT-TYPE :SUCCESS
                               :OUTPUT-INDEX 1 :TEMPLATE NIL)
                         '(588 903 (595)) '(:LABEL "1> "))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 43)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(8407 8386 (8414)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(6146 6125 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(7756 7742 (7763 7770 7777 7784))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL
                         (LIST :GC-MARK NIL :TEMPLATE NIL)
                         '(7189 NIL (7196 7238 7280 7420)) 'NIL)
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(8414 8407 (8421 8428)) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 44)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(6153 6125 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7763 7756 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(7196 7189 (7203 7210 7217 7224 7231))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(8421 8414 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(6160 6111 (6167 6174 6181 6188))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(7770 7756 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(7203 7196 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :BACKGROUND-COLOR :ELHAI-ORANGE
                               :SYMBOL-NODE-TYPE :VALUE :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::Y))
                         '(8428 8414 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6167 6160 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(7777 7756 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL
                         (LIST :GC-MARK NIL :TEMPLATE NIL)
                         '(5600 NIL (5607 5649 5691 5831)) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(7210 7196 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 45)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0 1)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF :LABELED) :KEYWORDS
                               NIL :ORDERED-NAMES
                               (LIST :DISPLAY-OFF :LABELED)
                               :SUBTEMPLATES
                               (LIST
                                (LIST :FLAG :DISPLAY-OFF)
                                (LIST :FLAG :LABELED))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST
                                 (LIST :FLAG :DISPLAY-OFF)
                                 (LIST :FLAG :LABELED))))
                         '(8435 8260 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(6174 6160 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 46)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(7784 7756 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(5607 5600 (5614 5621 5628 5635 5642))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(7217 7196 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(6181 6160 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(7791 7742 (7798 7805 7812 7819))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(5614 5607 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 3
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 3))
                         '(7224 7196 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 47)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(6188 6160 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7798 7791 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(5621 5607 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 48)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(7231 7196 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'OUTPUT-VALUE-SNIPPET "3"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE NIL)
                         '(595 588 NIL)
                         '(:RETURN-TYPE (SIMPLE-ARRAY CHARACTER (1))))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 49)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(6195 6111 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(7805 7791 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(5628 5607 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(7238 7189 (7245 7252 7259 7266 7273))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'TOPLEVEL-OUTPUT-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :INPUT-FORM
                               (LIST 'BBI:DEFINE '$$::Y '= 4)
                               :ERROR-CONDITION NIL :VALUE-STRINGS
                               (LIST "4") :NVALUES 1 :VALUES (LIST 4)
                               :PRINTOUT "" :RESULT-TYPE :SUCCESS
                               :OUTPUT-INDEX 2 :TEMPLATE NIL)
                         '(602 903 (609)) '(:LABEL "2> "))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 50)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(6202 6069 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(7812 7791 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 3
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 3))
                         '(5635 5607 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(7245 7238 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'OUTPUT-VALUE-SNIPPET "4"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE NIL)
                         '(609 602 NIL)
                         '(:RETURN-TYPE (SIMPLE-ARRAY CHARACTER (1))))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var" T)
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST
                                  (LIST :FLAG :DISPLAY-OFF)
                                  (LIST :FLAG :LABELED)))))
                         '(6209 5978 (6216 6223 6230 6237 6342))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 51)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(7819 7791 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 52)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(5642 5607 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(7252 7238 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(6216 6209 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 53)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(7826 7742 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(5649 5600 (5656 5663 5670 5677 5684))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(7259 7238 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(623 896 (630 672 637 945 644))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL
                         (LIST :GC-MARK NIL :TEMPLATE NIL)
                         '(6776 NIL (6783 6825 6867 7007)) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :SYMBOL-NODE-TYPE :PLACE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(6223 6209 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 54)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(7833 7700 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(5656 5649 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 4
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 4))
                         '(7266 7238 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(630 623 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(6783 6776 (6790 6797 6804 6811 6818))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(6230 6209 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var" T)
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST
                                  (LIST :FLAG :DISPLAY-OFF)
                                  (LIST :FLAG :LABELED)))))
                         '(7840 7609 (7847 7854 7861 7868 8015))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(5663 5649 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 55)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(7273 7238 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(637 623 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(6790 6783 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(6237 6209 (6244 6251))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(7847 7840 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(5670 5649 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(7280 7189 (7287 7294 7301 7308 7413))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 56)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(644 623 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(6797 6783 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6244 6237 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :SYMBOL-NODE-TYPE :PLACE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(7854 7840 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(1463 910 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 4
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 4))
                         '(5677 5649 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(7287 7280 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'TOPLEVEL-OUTPUT-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :INPUT-FORM
                               (LIST
                                'BBI:DEFINE
                                '$$::HYP
                                '=
                                (LIST
                                 'SQRT
                                 (LIST
                                  '+
                                  (LIST '* '$$::X '$$::X)
                                  (LIST '* '$$::Y '$$::Y))))
                               :ERROR-CONDITION NIL :VALUE-STRINGS
                               (LIST "5.0") :NVALUES 1 :VALUES
                               (LIST 5.0) :PRINTOUT "" :RESULT-TYPE
                               :SUCCESS :OUTPUT-INDEX 3 :TEMPLATE NIL)
                         '(651 903 (658)) '(:LABEL "3> "))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(6804 6783 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:SUM-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SUM-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(6251 6237 (6258 6265 6314))
                         '(:LABEL "sum-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(7861 7840 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 57)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(5684 5649 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(7294 7280 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'OUTPUT-VALUE-SNIPPET "5.0"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE NIL)
                         '(658 651 NIL)
                         '(:RETURN-TYPE (SIMPLE-ARRAY CHARACTER (3))))
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 3
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 3))
                         '(6811 6783 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SUM-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "SUM-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6258 6251 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(7868 7840 (7875 7882))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(5691 5600 (5698 5705 5712 5719 5824))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(7301 7280 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 58)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(6818 6783 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:PRODUCT-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "PRODUCT-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(6265 6251 (6272 6279 6286))
                         '(:LABEL "product-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7875 7868 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(5698 5691 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(7308 7280 (7315 7322))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(672 623 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(6825 6776 (6832 6839 6846 6853 6860))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "PRODUCT-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "PRODUCT-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6272 6265 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:SUM-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SUM-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(7882 7868 (7889 7896 7945))
                         '(:LABEL "sum-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(5705 5691 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7315 7308 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(6832 6825 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL
                         (LIST :GC-MARK NIL :TEMPLATE NIL)
                         '(6356 NIL (6363 6405 6447 6587)) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(6279 6265 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SUM-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "SUM-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7889 7882 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(5712 5691 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '+
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "+"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(7322 7308 (7329 7336 7371 7406))
                         '(:LABEL "+" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL
                         (LIST :GC-MARK NIL :TEMPLATE NIL)
                         '(5229 NIL (5236 5278 5320 5460)) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(6839 6825 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(6363 6356 (6370 6377 6384 6391 6398))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 59)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(6286 6265 (6293)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:PRODUCT-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "PRODUCT-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(7896 7882 (7903 7910 7917))
                         '(:LABEL "product-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(5719 5691 (5726 5733))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "+"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7329 7322 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(5236 5229 (5243 5250 5257 5264 5271))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(6846 6825 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(6370 6363 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(6293 6286 (6300 6307)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "PRODUCT-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "PRODUCT-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7903 7896 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5726 5719 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(7336 7322 (7343 7350 7357 7364))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(5243 5236 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 4
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 4))
                         '(6853 6825 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(6377 6363 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(6300 6293 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(7910 7896 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '+
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "+"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(5733 5719 (5740 5747 5782 5817))
                         '(:LABEL "+" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7343 7336 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(5250 5236 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 60)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(6860 6825 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(6384 6363 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CONTENTS "" :HOLE-OPEN T
                               :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(6307 6293 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 61)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(7917 7896 (7924)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "+"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5740 5733 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(7350 7336 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(5257 5236 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(6867 6776 (6874 6881 6888 6895 7000))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 3
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 3))
                         '(6391 6363 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 62)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(6314 6251 (6321)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(7924 7917 (7931 7938)) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(5747 5733 (5754 5761 5768 5775))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(7357 7336 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 3
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 3))
                         '(5264 5236 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(6874 6867 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 63)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(6398 6363 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(6321 6314 (6328 6335)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(7931 7924 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5754 5747 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 64)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(7364 7336 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 65)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(5271 5236 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(6881 6867 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(6405 6356 (6412 6419 6426 6433 6440))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(6328 6321 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(7938 7924 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(5761 5747 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(7371 7322 (7378 7385 7392 7399))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(5278 5229 (5285 5292 5299 5306 5313))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(6888 6867 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(6412 6405 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CONTENTS "" :HOLE-OPEN
                               NIL :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(6335 6321 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 66)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(7945 7882 (7952)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(5768 5747 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7378 7371 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(5285 5278 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :SYMBOL-NODE-TYPE :PLACE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(3325 2737 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(6895 6867 (6902 6909))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(6419 6405 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 67)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0 1)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF :LABELED) :KEYWORDS
                               NIL :ORDERED-NAMES
                               (LIST :DISPLAY-OFF :LABELED)
                               :SUBTEMPLATES
                               (LIST
                                (LIST :FLAG :DISPLAY-OFF)
                                (LIST :FLAG :LABELED))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST
                                 (LIST :FLAG :DISPLAY-OFF)
                                 (LIST :FLAG :LABELED))))
                         '(6342 6209 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(7952 7945 (7959 7966)) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 68)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(5775 5747 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(7385 7371 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(5292 5278 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6902 6895 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(6426 6405 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(6349 5208 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(7959 7952 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(5782 5733 (5789 5796 5803 5810))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(7392 7371 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(5299 5278 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '+
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "+"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(6909 6895 (6916 6923 6958 6993))
                         '(:LABEL "+" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 4
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 4))
                         '(6433 6405 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:PRODUCT-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "PRODUCT-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(7966 7952 (7973 7980 7987))
                         '(:LABEL "product-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5789 5782 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 69)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(7399 7371 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "+"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6916 6909 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 4
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 4))
                         '(5306 5278 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 70)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(6440 6405 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "PRODUCT-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "PRODUCT-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7973 7966 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(5796 5782 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 71)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(7406 7322 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(6923 6909 (6930 6937 6944 6951))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 72)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(5313 5278 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(6447 6356 (6454 6461 6468 6475 6580))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(7980 7966 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(5803 5782 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 73)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(7413 7280 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6930 6923 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(5320 5229 (5327 5334 5341 5348 5453))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(6454 6447 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 74)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(7987 7966 (7994)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 75)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(5810 5782 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var" T)
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST
                                  (LIST :FLAG :DISPLAY-OFF)
                                  (LIST :FLAG :LABELED)))))
                         '(7420 7189 (7427 7434 7441 7448 7595))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(6937 6923 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(5327 5320 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(6461 6447 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL
                         (LIST :GC-MARK NIL :TEMPLATE NIL)
                         '(4851 NIL (4858 4900 4942 5082)) 'NIL)
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(7994 7987 (8001 8008)) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 76)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(5817 5733 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(7427 7420 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(6944 6923 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(5334 5320 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(6468 6447 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(4858 4851 (4865 4872 4879 4886 4893))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(8001 7994 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 77)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(5824 5691 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :SYMBOL-NODE-TYPE :PLACE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(7434 7420 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 78)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(6951 6923 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(5341 5320 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(6475 6447 (6482 6489))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(4865 4858 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CONTENTS "" :HOLE-OPEN T
                               :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(8008 7994 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var" T)
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST
                                  (LIST :FLAG :DISPLAY-OFF)
                                  (LIST :FLAG :LABELED)))))
                         '(5831 5600 (5838 5845 5852 5859 5964))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(7441 7420 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(6958 6909 (6965 6972 6979 6986))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(5348 5320 (5355 5362))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6482 6475 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(4872 4858 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 79)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0 1)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF :LABELED) :KEYWORDS
                               NIL :ORDERED-NAMES
                               (LIST :DISPLAY-OFF :LABELED)
                               :SUBTEMPLATES
                               (LIST
                                (LIST :FLAG :DISPLAY-OFF)
                                (LIST :FLAG :LABELED))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST
                                 (LIST :FLAG :DISPLAY-OFF)
                                 (LIST :FLAG :LABELED))))
                         '(8015 7840 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(7448 7420 (7455 7462))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(5838 5831 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6965 6958 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5355 5348 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '+
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "+"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(6489 6475 (6496 6503 6538 6573))
                         '(:LABEL "+" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(4879 4858 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :BACKGROUND-COLOR :ELHAI-ORANGE
                               :SYMBOL-NODE-TYPE :VALUE :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::Y))
                         '(8022 6755 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :SYMBOL-NODE-TYPE :PLACE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(5845 5831 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7455 7448 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(6972 6958 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '+
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "+"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(5362 5348 (5369 5376 5411 5446))
                         '(:LABEL "+" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "+"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6496 6489 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 3
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 3))
                         '(4886 4858 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(5852 5831 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:SUM-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SUM-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(7462 7448 (7469 7476 7525))
                         '(:LABEL "sum-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(6979 6958 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "+"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "+"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5369 5362 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(6503 6489 (6510 6517 6524 6531))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 80)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(4893 4858 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(5859 5831 (5866 5873))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SUM-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "SUM-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7469 7462 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 81)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(6986 6958 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(5376 5362 (5383 5390 5397 5404))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6510 6503 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(4900 4851 (4907 4914 4921 4928 4935))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SQRT"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "SQRT"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5866 5859 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:PRODUCT-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "PRODUCT-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(7476 7462 (7483 7490 7497))
                         '(:LABEL "product-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 82)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(6993 6909 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5383 5376 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(6517 6503 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(4907 4900 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "PRODUCT-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "PRODUCT-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(7483 7476 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:SUM-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SUM-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "+" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(5873 5859 (5880 5887 5936))
                         '(:LABEL "sum-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 83)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(7000 6867 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(5390 5376 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(6524 6503 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(4914 4900 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(7490 7476 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "SUM-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "SUM-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5880 5873 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var" T)
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST
                                  (LIST :FLAG :DISPLAY-OFF)
                                  (LIST :FLAG :LABELED)))))
                         '(7007 6776 (7014 7021 7028 7035 7182))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(5397 5376 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 84)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(6531 6503 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(4921 4900 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 85)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(7497 7476 (7504)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:PRODUCT-OF
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "PRODUCT-OF"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST
                                  :SPROGN
                                  (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                  (LIST
                                   :FORM
                                   "number"
                                   (LIST 'OR 'NUMBER 'LIST)))
                                 :SPLICE)))
                         '(5887 5873 (5894 5901 5908))
                         '(:LABEL "product-of" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(7014 7007 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 86)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST :FORM "number" 'NUMBER)
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(5404 5376 NIL) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(6538 6489 (6545 6552 6559 6566))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 4
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 4))
                         '(4928 4900 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                (LIST
                                 :FORM
                                 "number"
                                 (LIST 'OR 'NUMBER 'LIST))
                                :SPLICE)
                               :SPLICE T)
                         '(7504 7497 (7511 7518)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "PRODUCT-OF"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "PRODUCT-OF"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5894 5887 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::HYP
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :SYMBOL-NODE-TYPE :PLACE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::HYP))
                         '(7021 7007 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET '*
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "*"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST :FORM "number" 'NUMBER)
                                (LIST
                                 :&REST
                                 "numbers"
                                 (LIST :FORM "number" 'NUMBER))))
                         '(5411 5362 (5418 5425 5432 5439))
                         '(:LABEL "*" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(6545 6538 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'KEYS-AND-FLAGS-SNIPPET
                         (AREF C-ARRAY-97105 87)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FLAG-INDICES (LIST 0)
                               :KEY-INDICES NIL :FLAGS
                               (LIST :DISPLAY-OFF) :KEYWORDS NIL
                               :ORDERED-NAMES (LIST :DISPLAY-OFF)
                               :SUBTEMPLATES
                               (LIST (LIST :FLAG :DISPLAY-OFF))
                               :KEYS-PRESENT NIL :FLAGS-PRESENT NIL
                               :TEMPLATE
                               (LIST
                                :KEYS-AND-FLAGS
                                (LIST (LIST :FLAG :DISPLAY-OFF))))
                         '(4935 4900 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var" T)
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST
                                  (LIST :FLAG :DISPLAY-OFF)
                                  (LIST :FLAG :LABELED)))))
                         '(2737 896 (2744 3325 2758 3878 2772))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(7511 7504 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CONTENTS NIL :HOLE-OPEN T
                               :HOLE-TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST))
                               :TEMPLATE
                               (LIST
                                :FORM
                                "number"
                                (LIST 'OR 'NUMBER 'LIST)))
                         '(5901 5887 NIL)
                         '(:LABEL "number" :RETURN-TYPE
                           (OR NUMBER LIST)))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "="
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "="
                                :CLASS
                                :DEFINE-FUNCTION-TOKEN)
                               :CLASS :DEFINE-FUNCTION-TOKEN)
                         '(7028 7007 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "*"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :LITERAL
                                "*"
                                :CLASS
                                :FUNCTION-NAME)
                               :CLASS :FUNCTION-NAME)
                         '(5418 5411 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(6552 6538 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBI:DEFINE
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :CALL-TYPE :MACRO
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "DEFINE"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST :PLACE "var")
                                (LIST
                                 :LITERAL
                                 "="
                                 :CLASS
                                 :DEFINE-FUNCTION-TOKEN)
                                (LIST :FORM "value" T)
                                (LIST
                                 :KEYS-AND-FLAGS
                                 (LIST (LIST :FLAG :DISPLAY-OFF)))))
                         '(4942 4851 (4949 4956 4963 4970 5075))
                         '(:LABEL "define" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 3
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :FORMAT "~S" :TEMPLATE
                               (LIST :CONSTANT 3))
                         '(1694 910 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(2744 2737 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::X
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :SYMBOL-NODE-TYPE :VALUE
                               :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::X))
                         '(7518 7504 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-97105 88)
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :TEMPLATE
                               (LIST
                                :AGG
                                "numbers"
                                (LIST
                                 :SPROGN
                                 (LIST :LITERAL "*" :CLASS :LOOP-TAG)
                                 (LIST
                                  :FORM
                                  "number"
                                  (LIST 'OR 'NUMBER 'LIST)))
                                :SPLICE
                                :DISPLAY-ONE-HOLE
                                T
                                :SPLICE)
                               :SPLICE T :DISPLAY-ONE-HOLE T :SPLICE T)
                         '(5908 5887 (5915)) '(:LABEL "numbers"))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'SQRT
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :CALL-TYPE :FUNCTION
                               :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "SQRT"
                                 :CLASS
                                 :FUNCTION-NAME)
                                (LIST :FORM "number" 'NUMBER)))
                         '(7035 7007 (7042 7049))
                         '(:LABEL "sqrt" :RETURN-TYPE NUMBER))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(5425 5411 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::Y
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-ORANGE :FORMAT "~S" :TEMPLATE
                               (LIST :SYMBOL '$$::Y))
                         '(6559 6538 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "DEFINE"
                         (LIST :GC-MARK NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :LITERAL
                                "DEFINE"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(4949 4942 NIL) 'NIL)))

(N-RESET-WORKSPACE-POINTERS S-LIST-97106)

(PROGN (UNINTERN 'S-LIST-97106) (UNINTERN 'C-ARRAY-97105))
