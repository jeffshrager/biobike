
(IN-PACKAGE :NVPL)

(PROGN (SETQ *VPL-WORKSPACE-HISTORY-INDEX* 0)
       (SETQ *VPL-EXECUTION-HISTORY-INDEX* 2))

(SETF (UVS-WS *VPL-STATE*) 20423)

(SETF (UVS-RS *VPL-STATE*) 20428)

(SETF (UVS-WSH *VPL-STATE*)
      '(20423 22228 22133 22033 21638 21543 21448 21368 21088 21003))

(PROGN (DEFPARAMETER C-ARRAY-93741 (MAKE-ARRAY (LIST 60)))
       (SETF (AREF C-ARRAY-93741 0)
             (LIST (LIST "by"
                         (LIST :SPROGN
                               (LIST :LITERAL "by" :CLASS :LOOP-TAG)
                               (LIST :FORM "step" 'INTEGER)))))
       (SETF (AREF C-ARRAY-93741 1)
             (LIST :SPROGN (LIST :LITERAL "init" :CLASS :LOOP-TAG)
                   (LIST :ARG "var")
                   (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                   (LIST :FORM "value" T) :DELETE-BOX :MAIN-MENU :HTYPE
                   :LOOP-INIT-CLAUSE))
       (SETF (AREF C-ARRAY-93741 2)
             (LIST (LIST "FOR variable IN collection"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST
                                :FORM
                                "collection"
                                (LIST
                                 'OR
                                 'SEQUENCE
                                 'HASH-TABLE
                                 'UTILS::GARRAY
                                 'FRAMES:%FRAME))
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) IN list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) = value-list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial values" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next values" 'LIST)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 TO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 BELOW n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "below" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 (without limit)"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "step function"
                                    'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 DOWNTO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :LITERAL
                                "downto"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "WHILE (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "while" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "UNTIL (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "until" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = list-part"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "on" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = value"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial value" T)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next value" T)))))
                               :DELETE-BOX :NEWLINE))))
       (SETF (AREF C-ARRAY-93741 3)
             (LIST (LIST "by"
                         (LIST :SPROGN
                               (LIST :LITERAL "by" :CLASS :LOOP-TAG)
                               (LIST :FORM "step" 'INTEGER)))))
       (SETF (AREF C-ARRAY-93741 4)
             (LIST (LIST "do"
                         (LIST :SPROGN
                               (LIST :LITERAL "do" :CLASS :LOOP-TAG)
                               (LIST
                                :ONE-ESPLICED
                                "forms"
                                (LIST :FORM "form" T))
                               :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 5)
             (LIST (LIST "append"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... append"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "collect"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... collect"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "min"
                         (LIST :SPROGN
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... min"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "max"
                         (LIST :SPROGN
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... max"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "count"
                         (LIST :SPROGN
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... count"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 6)
             (LIST (LIST "finally"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "finally"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "action" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 7)
             (LIST :SPROGN (LIST :LITERAL "init" :CLASS :LOOP-TAG)
                   (LIST :ARG "var")
                   (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                   (LIST :FORM "value" T) :DELETE-BOX :MAIN-MENU :HTYPE
                   :LOOP-INIT-CLAUSE))
       (SETF (AREF C-ARRAY-93741 8)
             (LIST (LIST "FOR variable IN collection"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST
                                :FORM
                                "collection"
                                (LIST
                                 'OR
                                 'SEQUENCE
                                 'HASH-TABLE
                                 'UTILS::GARRAY
                                 'FRAMES:%FRAME))
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) IN list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) = value-list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial values" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next values" 'LIST)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 TO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 BELOW n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "below" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 (without limit)"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "step function"
                                    'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 DOWNTO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :LITERAL
                                "downto"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "WHILE (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "while" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "UNTIL (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "until" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = list-part"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "on" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = value"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial value" T)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next value" T)))))
                               :DELETE-BOX :NEWLINE))))
       (SETF (AREF C-ARRAY-93741 9)
             (LIST (LIST "by"
                         (LIST :SPROGN
                               (LIST :LITERAL "by" :CLASS :LOOP-TAG)
                               (LIST :FORM "step" 'INTEGER)))))
       (SETF (AREF C-ARRAY-93741 10)
             (LIST (LIST "do"
                         (LIST :SPROGN
                               (LIST :LITERAL "do" :CLASS :LOOP-TAG)
                               (LIST
                                :ONE-ESPLICED
                                "forms"
                                (LIST :FORM "form" T))
                               :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 11)
             (LIST (LIST "append"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... append"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "collect"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... collect"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "min"
                         (LIST :SPROGN
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... min"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "max"
                         (LIST :SPROGN
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... max"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "count"
                         (LIST :SPROGN
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... count"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 12)
             (LIST (LIST "finally"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "finally"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "action" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 13)
             (LIST :SPROGN (LIST :LITERAL "init" :CLASS :LOOP-TAG)
                   (LIST :ARG "var")
                   (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                   (LIST :FORM "value" T) :DELETE-BOX :MAIN-MENU :HTYPE
                   :LOOP-INIT-CLAUSE))
       (SETF (AREF C-ARRAY-93741 14)
             (LIST (LIST "FOR variable IN collection"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST
                                :FORM
                                "collection"
                                (LIST
                                 'OR
                                 'SEQUENCE
                                 'HASH-TABLE
                                 'UTILS::GARRAY
                                 'FRAMES:%FRAME))
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) IN list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) = value-list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial values" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next values" 'LIST)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 TO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 BELOW n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "below" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 (without limit)"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "step function"
                                    'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 DOWNTO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :LITERAL
                                "downto"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "WHILE (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "while" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "UNTIL (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "until" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = list-part"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "on" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = value"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial value" T)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next value" T)))))
                               :DELETE-BOX :NEWLINE))))
       (SETF (AREF C-ARRAY-93741 15)
             (LIST (LIST "by"
                         (LIST :SPROGN
                               (LIST :LITERAL "by" :CLASS :LOOP-TAG)
                               (LIST :FORM "step" 'INTEGER)))))
       (SETF (AREF C-ARRAY-93741 16)
             (LIST (LIST "do"
                         (LIST :SPROGN
                               (LIST :LITERAL "do" :CLASS :LOOP-TAG)
                               (LIST
                                :ONE-ESPLICED
                                "forms"
                                (LIST :FORM "form" T))
                               :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 17)
             (LIST (LIST "append"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... append"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "collect"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... collect"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "min"
                         (LIST :SPROGN
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... min"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "max"
                         (LIST :SPROGN
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... max"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "count"
                         (LIST :SPROGN
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... count"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 18)
             (LIST (LIST "finally"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "finally"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "action" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 19)
             (LIST :SPROGN (LIST :LITERAL "init" :CLASS :LOOP-TAG)
                   (LIST :ARG "var")
                   (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                   (LIST :FORM "value" T) :DELETE-BOX :MAIN-MENU :HTYPE
                   :LOOP-INIT-CLAUSE))
       (SETF (AREF C-ARRAY-93741 20)
             (LIST (LIST "FOR variable IN collection"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST
                                :FORM
                                "collection"
                                (LIST
                                 'OR
                                 'SEQUENCE
                                 'HASH-TABLE
                                 'UTILS::GARRAY
                                 'FRAMES:%FRAME))
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) IN list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) = value-list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial values" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next values" 'LIST)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 TO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 BELOW n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "below" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 (without limit)"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "step function"
                                    'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 DOWNTO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :LITERAL
                                "downto"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "WHILE (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "while" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "UNTIL (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "until" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = list-part"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "on" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = value"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial value" T)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next value" T)))))
                               :DELETE-BOX :NEWLINE))))
       (SETF (AREF C-ARRAY-93741 21)
             (LIST (LIST "by"
                         (LIST :SPROGN
                               (LIST :LITERAL "by" :CLASS :LOOP-TAG)
                               (LIST :FORM "step" 'INTEGER)))))
       (SETF (AREF C-ARRAY-93741 22)
             (LIST (LIST "do"
                         (LIST :SPROGN
                               (LIST :LITERAL "do" :CLASS :LOOP-TAG)
                               (LIST
                                :ONE-ESPLICED
                                "forms"
                                (LIST :FORM "form" T))
                               :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 23)
             (LIST (LIST "append"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... append"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "collect"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... collect"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "min"
                         (LIST :SPROGN
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... min"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "max"
                         (LIST :SPROGN
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... max"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "count"
                         (LIST :SPROGN
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... count"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 24)
             (LIST (LIST "finally"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "finally"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "action" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 25)
             (LIST :SPROGN (LIST :LITERAL "init" :CLASS :LOOP-TAG)
                   (LIST :ARG "var")
                   (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                   (LIST :FORM "value" T) :DELETE-BOX :MAIN-MENU :HTYPE
                   :LOOP-INIT-CLAUSE))
       (SETF (AREF C-ARRAY-93741 26)
             (LIST (LIST "FOR variable IN collection"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST
                                :FORM
                                "collection"
                                (LIST
                                 'OR
                                 'SEQUENCE
                                 'HASH-TABLE
                                 'UTILS::GARRAY
                                 'FRAMES:%FRAME))
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) IN list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) = value-list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial values" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next values" 'LIST)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 TO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 BELOW n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "below" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 (without limit)"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "step function"
                                    'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 DOWNTO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :LITERAL
                                "downto"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "WHILE (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "while" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "UNTIL (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "until" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = list-part"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "on" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = value"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial value" T)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next value" T)))))
                               :DELETE-BOX :NEWLINE))))
       (SETF (AREF C-ARRAY-93741 27)
             (LIST (LIST "by"
                         (LIST :SPROGN
                               (LIST :LITERAL "by" :CLASS :LOOP-TAG)
                               (LIST :FORM "step" 'INTEGER)))))
       (SETF (AREF C-ARRAY-93741 28)
             (LIST (LIST "do"
                         (LIST :SPROGN
                               (LIST :LITERAL "do" :CLASS :LOOP-TAG)
                               (LIST
                                :ONE-ESPLICED
                                "forms"
                                (LIST :FORM "form" T))
                               :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 29)
             (LIST (LIST "append"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... append"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "collect"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... collect"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "min"
                         (LIST :SPROGN
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... min"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "max"
                         (LIST :SPROGN
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... max"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "count"
                         (LIST :SPROGN
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... count"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 30)
             (LIST (LIST "finally"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "finally"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "action" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 31)
             (LIST :SPROGN (LIST :LITERAL "init" :CLASS :LOOP-TAG)
                   (LIST :ARG "var")
                   (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                   (LIST :FORM "value" T) :DELETE-BOX :MAIN-MENU :HTYPE
                   :LOOP-INIT-CLAUSE))
       (SETF (AREF C-ARRAY-93741 32)
             (LIST (LIST "FOR variable IN collection"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST
                                :FORM
                                "collection"
                                (LIST
                                 'OR
                                 'SEQUENCE
                                 'HASH-TABLE
                                 'UTILS::GARRAY
                                 'FRAMES:%FRAME))
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) IN list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) = value-list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial values" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next values" 'LIST)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 TO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 BELOW n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "below" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 (without limit)"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "step function"
                                    'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 DOWNTO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :LITERAL
                                "downto"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "WHILE (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "while" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "UNTIL (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "until" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = list-part"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "on" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = value"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial value" T)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next value" T)))))
                               :DELETE-BOX :NEWLINE))))
       (SETF (AREF C-ARRAY-93741 33)
             (LIST (LIST "by"
                         (LIST :SPROGN
                               (LIST :LITERAL "by" :CLASS :LOOP-TAG)
                               (LIST :FORM "step" 'INTEGER)))))
       (SETF (AREF C-ARRAY-93741 34)
             (LIST (LIST "do"
                         (LIST :SPROGN
                               (LIST :LITERAL "do" :CLASS :LOOP-TAG)
                               (LIST
                                :ONE-ESPLICED
                                "forms"
                                (LIST :FORM "form" T))
                               :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 35)
             (LIST (LIST "append"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... append"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "collect"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... collect"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "min"
                         (LIST :SPROGN
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... min"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "max"
                         (LIST :SPROGN
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... max"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "count"
                         (LIST :SPROGN
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... count"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 36)
             (LIST (LIST "finally"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "finally"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "action" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 37)
             (LIST :SPROGN (LIST :LITERAL "init" :CLASS :LOOP-TAG)
                   (LIST :ARG "var")
                   (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                   (LIST :FORM "value" T) :DELETE-BOX :MAIN-MENU :HTYPE
                   :LOOP-INIT-CLAUSE))
       (SETF (AREF C-ARRAY-93741 38)
             (LIST (LIST "FOR variable IN collection"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST
                                :FORM
                                "collection"
                                (LIST
                                 'OR
                                 'SEQUENCE
                                 'HASH-TABLE
                                 'UTILS::GARRAY
                                 'FRAMES:%FRAME))
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) IN list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) = value-list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial values" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next values" 'LIST)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 TO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 BELOW n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "below" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 (without limit)"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "step function"
                                    'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 DOWNTO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :LITERAL
                                "downto"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "WHILE (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "while" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "UNTIL (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "until" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = list-part"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "on" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = value"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial value" T)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next value" T)))))
                               :DELETE-BOX :NEWLINE))))
       (SETF (AREF C-ARRAY-93741 39)
             (LIST (LIST "by"
                         (LIST :SPROGN
                               (LIST :LITERAL "by" :CLASS :LOOP-TAG)
                               (LIST :FORM "step" 'INTEGER)))))
       (SETF (AREF C-ARRAY-93741 40)
             (LIST (LIST "do"
                         (LIST :SPROGN
                               (LIST :LITERAL "do" :CLASS :LOOP-TAG)
                               (LIST
                                :ONE-ESPLICED
                                "forms"
                                (LIST :FORM "form" T))
                               :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 41)
             (LIST (LIST "append"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... append"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "collect"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... collect"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "min"
                         (LIST :SPROGN
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... min"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "max"
                         (LIST :SPROGN
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... max"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "count"
                         (LIST :SPROGN
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... count"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 42)
             (LIST (LIST "finally"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "finally"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "action" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 43)
             (LIST :SPROGN (LIST :LITERAL "init" :CLASS :LOOP-TAG)
                   (LIST :ARG "var")
                   (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                   (LIST :FORM "value" T) :DELETE-BOX :MAIN-MENU :HTYPE
                   :LOOP-INIT-CLAUSE))
       (SETF (AREF C-ARRAY-93741 44)
             (LIST (LIST "FOR variable IN collection"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST
                                :FORM
                                "collection"
                                (LIST
                                 'OR
                                 'SEQUENCE
                                 'HASH-TABLE
                                 'UTILS::GARRAY
                                 'FRAMES:%FRAME))
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) IN list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) = value-list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial values" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next values" 'LIST)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 TO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 BELOW n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "below" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 (without limit)"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "step function"
                                    'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 DOWNTO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :LITERAL
                                "downto"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "WHILE (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "while" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "UNTIL (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "until" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = list-part"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "on" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = value"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial value" T)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next value" T)))))
                               :DELETE-BOX :NEWLINE))))
       (SETF (AREF C-ARRAY-93741 45)
             (LIST :SPROGN (LIST :LITERAL "init" :CLASS :LOOP-TAG)
                   (LIST :ARG "var")
                   (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                   (LIST :FORM "value" T) :DELETE-BOX :MAIN-MENU :HTYPE
                   :LOOP-INIT-CLAUSE))
       (SETF (AREF C-ARRAY-93741 46)
             (LIST (LIST "FOR variable IN collection"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST
                                :FORM
                                "collection"
                                (LIST
                                 'OR
                                 'SEQUENCE
                                 'HASH-TABLE
                                 'UTILS::GARRAY
                                 'FRAMES:%FRAME))
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) IN list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) = value-list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial values" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next values" 'LIST)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 TO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 BELOW n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "below" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 (without limit)"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "step function"
                                    'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 DOWNTO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :LITERAL
                                "downto"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "WHILE (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "while" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "UNTIL (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "until" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = list-part"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "on" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = value"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial value" T)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next value" T)))))
                               :DELETE-BOX :NEWLINE))))
       (SETF (AREF C-ARRAY-93741 47)
             (LIST (LIST "do"
                         (LIST :SPROGN
                               (LIST :LITERAL "do" :CLASS :LOOP-TAG)
                               (LIST
                                :ONE-ESPLICED
                                "forms"
                                (LIST :FORM "form" T))
                               :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 48)
             (LIST (LIST "append"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... append"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "collect"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... collect"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "min"
                         (LIST :SPROGN
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... min"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "max"
                         (LIST :SPROGN
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... max"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "count"
                         (LIST :SPROGN
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... count"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 49)
             (LIST (LIST "finally"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "finally"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "action" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 50)
             (LIST (LIST "by"
                         (LIST :SPROGN
                               (LIST :LITERAL "by" :CLASS :LOOP-TAG)
                               (LIST :FORM "step" 'INTEGER)))))
       (SETF (AREF C-ARRAY-93741 51)
             (LIST (LIST "do"
                         (LIST :SPROGN
                               (LIST :LITERAL "do" :CLASS :LOOP-TAG)
                               (LIST
                                :ONE-ESPLICED
                                "forms"
                                (LIST :FORM "form" T))
                               :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 52)
             (LIST (LIST "append"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... append"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "collect"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... collect"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "min"
                         (LIST :SPROGN
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... min"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "max"
                         (LIST :SPROGN
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... max"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "count"
                         (LIST :SPROGN
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... count"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 53)
             (LIST (LIST "finally"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "finally"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "action" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 54)
             (LIST :SPROGN (LIST :LITERAL "init" :CLASS :LOOP-TAG)
                   (LIST :ARG "var")
                   (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                   (LIST :FORM "value" T) :DELETE-BOX :MAIN-MENU :HTYPE
                   :LOOP-INIT-CLAUSE))
       (SETF (AREF C-ARRAY-93741 55)
             (LIST (LIST "FOR variable IN collection"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST
                                :FORM
                                "collection"
                                (LIST
                                 'OR
                                 'SEQUENCE
                                 'HASH-TABLE
                                 'UTILS::GARRAY
                                 'FRAMES:%FRAME))
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) IN list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "in" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR (var1 var2...) = value-list"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ONE "vars" (LIST :ARG "var"))
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial values" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next values" 'LIST)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 TO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 BELOW n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST :LITERAL "below" :CLASS :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 (without limit)"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "step function"
                                    'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "FOR variable FROM n1 DOWNTO n2"
                         (LIST :SPROGN
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               (LIST :FORM "first value" 'INTEGER)
                               (LIST
                                :LITERAL
                                "downto"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "last value" 'INTEGER)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER)))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "WHILE (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "while" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "UNTIL (condition)"
                         (LIST :SPROGN
                               (LIST :LITERAL "until" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = list-part"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "on" :CLASS :LOOP-TAG)
                               (LIST :FORM "list" 'LIST)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "step function")))))
                               :DELETE-BOX :NEWLINE))
                   (LIST "AS variable = value"
                         (LIST :SPROGN
                               (LIST :LITERAL "as" :CLASS :LOOP-TAG)
                               (LIST :ARG "var")
                               (LIST :LITERAL "=" :CLASS :EQUAL-SIGN)
                               (LIST :FORM "initial value" T)
                               (LIST
                                :OPTIONAL-LABELED-ARG
                                "then"
                                (LIST
                                 (LIST
                                  "then"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "then"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "next value" T)))))
                               :DELETE-BOX :NEWLINE))))
       (SETF (AREF C-ARRAY-93741 56)
             (LIST (LIST "by"
                         (LIST :SPROGN
                               (LIST :LITERAL "by" :CLASS :LOOP-TAG)
                               (LIST :FORM "step" 'INTEGER)))))
       (SETF (AREF C-ARRAY-93741 57)
             (LIST (LIST "do"
                         (LIST :SPROGN
                               (LIST :LITERAL "do" :CLASS :LOOP-TAG)
                               (LIST
                                :ONE-ESPLICED
                                "forms"
                                (LIST :FORM "form" T))
                               :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 58)
             (LIST (LIST "append"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... append"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "append"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "collect"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... collect"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "min"
                         (LIST :SPROGN
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... min"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "min" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "max"
                         (LIST :SPROGN
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... max"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "max" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... sum"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "sum" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "count"
                         (LIST :SPROGN
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))
                   (LIST "when ... count"
                         (LIST :SPROGN
                               (LIST :LITERAL "when" :CLASS :LOOP-TAG)
                               (LIST :FORM "condition" 'LISP-BOOLEAN)
                               (LIST :LITERAL "count" :CLASS :LOOP-TAG)
                               (LIST :FORM "value" T) :DELETE-BOX))))
       (SETF (AREF C-ARRAY-93741 59)
             (LIST (LIST "finally"
                         (LIST :SPROGN
                               (LIST
                                :LITERAL
                                "finally"
                                :CLASS
                                :LOOP-TAG)
                               (LIST :FORM "action" T) :DELETE-BOX)))))

(DEFPARAMETER S-LIST-93742
    (LIST
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :COLLAPSED? NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                                (LIST :ARG "var")
                                (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                                (LIST :FORM "first value" 'INTEGER)
                                (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                                (LIST :FORM "last value" 'INTEGER)
                                (LIST
                                 :OPTIONAL-LABELED-ARG
                                 "by"
                                 (LIST
                                  (LIST
                                   "by"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "by"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "step" 'INTEGER)))))
                                :DELETE-BOX
                                :NEWLINE
                                :SPLICE)
                               :DELETE-BOX T :NEWLINE T :SPLICE T)
                         '(20798 20453
                           (20803 20998 20813 21083 20823 22323 20833))
                         'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "for"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(20803 20798 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "from"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(20813 20798 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "to"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(20823 20798 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET (AREF C-ARRAY-93741 0)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER))))
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-ARG
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :CHOICE-TYPE :OPTIONAL-LABELED-ARG
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(20833 20798 NIL) '(:LABEL "by"))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL (LIST :TEMPLATE NIL)
                         '(20423 NIL (20438)) 'NIL)
     (N-RECREATE-SNIPPET 'TOPLEVEL-RS-SNIPPET NIL (LIST :TEMPLATE NIL)
                         '(20428 NIL (22518)) 'NIL)
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL (LIST :TEMPLATE NIL)
                         '(22033 NIL (22038)) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBL:LOOP
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :HIDDEN-NODES (LIST 1 5) :CALL-TYPE
                               :MACRO :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "LOOP"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST
                                 :AGG
                                 "initializations"
                                 (LIST
                                  :SPROGN
                                  (LIST
                                   :LITERAL
                                   "init"
                                   :CLASS
                                   :LOOP-TAG)
                                  (LIST :ARG "var")
                                  (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                  (LIST :FORM "value" T)
                                  :DELETE-BOX
                                  :MAIN-MENU
                                  :HTYPE
                                  :LOOP-INIT-CLAUSE)
                                 :SPLICE
                                 :REVEAL-LABEL
                                 "inits"
                                 :HTYPE
                                 :LOOP-INITIALIZATIONS
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "INITIALIZATION SECTION: Variables preset before loop begins"
                                 :OPTIONS-LABEL
                                 "inits")
                                (LIST
                                 :LOOP-ITERATOR
                                 "controls"
                                 (LIST
                                  (LIST
                                   "FOR variable IN collection"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "collection"
                                     (LIST
                                      'OR
                                      'SEQUENCE
                                      'HASH-TABLE
                                      'UTILS::GARRAY
                                      'FRAMES:%FRAME))
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) IN list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) = value-list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial values" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "next values"
                                         'LIST)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 TO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "to"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 BELOW n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "below"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 (without limit)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step function"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 DOWNTO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "downto"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "WHILE (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "while"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "UNTIL (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "until"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = list-part"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "on"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = value"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial value" T)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :FORM "next value" T)))))
                                    :DELETE-BOX
                                    :NEWLINE)))
                                 :HTYPE
                                 :LOOP-ITERATOR
                                 :NEWLINE
                                 :DESCRIPTION
                                 "CONTROLS: Determine how the loop executes and terminates")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "action"
                                 (LIST
                                  (LIST
                                   "do"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "do"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE-ESPLICED
                                     "forms"
                                     (LIST :FORM "form" T))
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "action clause"
                                 :HTYPE
                                 :LOOP-ACTION
                                 :NEWLINE
                                 :DESCRIPTION
                                 "BODY: Forms to be iterated")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "results"
                                 (LIST
                                  (LIST
                                   "append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "aggregation clause"
                                 :HTYPE
                                 :LOOP-AGGREGATE
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "RESULT SECTION: Determines result returned by the loop")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "finally"
                                 (LIST
                                  (LIST
                                   "finally"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "finally"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "action" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "final action"
                                 :HTYPE
                                 :LOOP-FINALLY
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "FINAL ACTION: Performed after last iteration")))
                         '(22038 22033
                           (22043 22048 22053 22098 22103 22123))
                         '(:LABEL "loop" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "LOOP"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "LOOP"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(22043 22038 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-93741 1)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :AGG
                                "initializations"
                                (LIST
                                 :SPROGN
                                 (LIST
                                  :LITERAL
                                  "init"
                                  :CLASS
                                  :LOOP-TAG)
                                 (LIST :ARG "var")
                                 (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                 (LIST :FORM "value" T)
                                 :DELETE-BOX
                                 :MAIN-MENU
                                 :HTYPE
                                 :LOOP-INIT-CLAUSE)
                                :SPLICE
                                :REVEAL-LABEL
                                "inits"
                                :HTYPE
                                :LOOP-INITIALIZATIONS
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "INITIALIZATION SECTION: Variables preset before loop begins"
                                :OPTIONS-LABEL
                                "inits")
                               :SPLICE T :REVEAL-LABEL "inits" :HTYPE
                               :LOOP-INITIALIZATIONS :HIDDEN-NODE T
                               :NEWLINE T :DESCRIPTION
                               "INITIALIZATION SECTION: Variables preset before loop begins"
                               :OPTIONS-LABEL "inits")
                         '(22048 22038 NIL)
                         '(:LABEL "initializations"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET (AREF C-ARRAY-93741 2)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 NIL)
                                (LIST 3 (LIST 0))
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "controls"
                                (LIST
                                 (LIST
                                  "FOR variable IN collection"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "collection"
                                    (LIST
                                     'OR
                                     'SEQUENCE
                                     'HASH-TABLE
                                     'UTILS::GARRAY
                                     'FRAMES:%FRAME))
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) IN list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) = value-list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial values" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "next values"
                                        'LIST)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 TO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "to"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 BELOW n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "below"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 (without limit)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "step function"
                                        'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 DOWNTO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "downto"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "WHILE (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "while"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "UNTIL (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "until"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = list-part"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "on"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = value"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial value" T)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "next value" T)))))
                                   :DELETE-BOX
                                   :NEWLINE)))
                                :HTYPE
                                :LOOP-ITERATOR
                                :NEWLINE
                                :DESCRIPTION
                                "CONTROLS: Determine how the loop executes and terminates"
                                :CHOICE-TYPE
                                :LOOP-ITERATOR
                                :INSTANTIATED
                                :REQUIRED
                                :MULTIPLE
                                :REPEATABLE
                                :SPLICE)
                               :HTYPE :LOOP-ITERATOR :NEWLINE T
                               :DESCRIPTION
                               "CONTROLS: Determine how the loop executes and terminates"
                               :CHOICE-TYPE :LOOP-ITERATOR
                               :INSTANTIATED T :REQUIRED T :MULTIPLE T
                               :REPEATABLE T :SPLICE T)
                         '(22053 22038 (22058)) '(:LABEL "controls"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                                (LIST :ARG "var")
                                (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                                (LIST :FORM "first value" 'INTEGER)
                                (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                                (LIST :FORM "last value" 'INTEGER)
                                (LIST
                                 :OPTIONAL-LABELED-ARG
                                 "by"
                                 (LIST
                                  (LIST
                                   "by"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "by"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "step" 'INTEGER)))))
                                :DELETE-BOX
                                :NEWLINE
                                :SPLICE)
                               :DELETE-BOX T :NEWLINE T :SPLICE T)
                         '(22058 22053
                           (22063 22068 22073 22078 22083 22088 22093))
                         'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "for"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(22063 22058 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::J
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SYMBOL-NODE-TYPE :ARG :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::J))
                         '(22068 22058 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "from"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(22073 22058 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 1
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW :FORMAT
                               "~S" :TEMPLATE (LIST :CONSTANT 1))
                         '(22078 22058 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "to"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(22083 22058 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 10
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW :FORMAT
                               "~S" :TEMPLATE (LIST :CONSTANT 10))
                         '(22088 22058 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET (AREF C-ARRAY-93741 3)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER))))
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-ARG
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :CHOICE-TYPE :OPTIONAL-LABELED-ARG
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(22093 22058 NIL) '(:LABEL "by"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET (AREF C-ARRAY-93741 4)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "action"
                                (LIST
                                 (LIST
                                  "do"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "do"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :ONE-ESPLICED
                                    "forms"
                                    (LIST :FORM "form" T))
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "action clause"
                                :HTYPE
                                :LOOP-ACTION
                                :NEWLINE
                                :DESCRIPTION
                                "BODY: Forms to be iterated"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "action clause" :HTYPE
                               :LOOP-ACTION :NEWLINE T :DESCRIPTION
                               "BODY: Forms to be iterated"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(22098 22038 NIL) '(:LABEL "action"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET (AREF C-ARRAY-93741 5)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 (LIST 0))
                                (LIST 3 NIL)
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL)
                                (LIST 11 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "results"
                                (LIST
                                 (LIST
                                  "append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "aggregation clause"
                                :HTYPE
                                :LOOP-AGGREGATE
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "RESULT SECTION: Determines result returned by the loop"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "aggregation clause"
                               :HTYPE :LOOP-AGGREGATE :HIDDEN-NODE NIL
                               :NEWLINE T :DESCRIPTION
                               "RESULT SECTION: Determines result returned by the loop"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(22103 22038 (22108)) '(:LABEL "results"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST
                                 :LITERAL
                                 "collect"
                                 :CLASS
                                 :LOOP-TAG)
                                (LIST :FORM "value" T)
                                :DELETE-BOX
                                :SPLICE)
                               :DELETE-BOX T :SPLICE T)
                         '(22108 22103 (22113 22118)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "collect"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(22113 22108 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::J
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SYMBOL-NODE-TYPE :VALUE :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::J))
                         '(22118 22108 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET (AREF C-ARRAY-93741 6)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "finally"
                                (LIST
                                 (LIST
                                  "finally"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "finally"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "action" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "final action"
                                :HTYPE
                                :LOOP-FINALLY
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "FINAL ACTION: Performed after last iteration"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "final action" :HTYPE
                               :LOOP-FINALLY :HIDDEN-NODE T :NEWLINE T
                               :DESCRIPTION
                               "FINAL ACTION: Performed after last iteration"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(22123 22038 NIL) '(:LABEL "finally"))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL (LIST :TEMPLATE NIL)
                         '(22133 NIL (22138)) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBL:LOOP
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :HIDDEN-NODES (LIST 1 5) :CALL-TYPE
                               :MACRO :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "LOOP"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST
                                 :AGG
                                 "initializations"
                                 (LIST
                                  :SPROGN
                                  (LIST
                                   :LITERAL
                                   "init"
                                   :CLASS
                                   :LOOP-TAG)
                                  (LIST :ARG "var")
                                  (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                  (LIST :FORM "value" T)
                                  :DELETE-BOX
                                  :MAIN-MENU
                                  :HTYPE
                                  :LOOP-INIT-CLAUSE)
                                 :SPLICE
                                 :REVEAL-LABEL
                                 "inits"
                                 :HTYPE
                                 :LOOP-INITIALIZATIONS
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "INITIALIZATION SECTION: Variables preset before loop begins"
                                 :OPTIONS-LABEL
                                 "inits")
                                (LIST
                                 :LOOP-ITERATOR
                                 "controls"
                                 (LIST
                                  (LIST
                                   "FOR variable IN collection"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "collection"
                                     (LIST
                                      'OR
                                      'SEQUENCE
                                      'HASH-TABLE
                                      'UTILS::GARRAY
                                      'FRAMES:%FRAME))
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) IN list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) = value-list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial values" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "next values"
                                         'LIST)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 TO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "to"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 BELOW n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "below"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 (without limit)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step function"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 DOWNTO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "downto"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "WHILE (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "while"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "UNTIL (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "until"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = list-part"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "on"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = value"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial value" T)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :FORM "next value" T)))))
                                    :DELETE-BOX
                                    :NEWLINE)))
                                 :HTYPE
                                 :LOOP-ITERATOR
                                 :NEWLINE
                                 :DESCRIPTION
                                 "CONTROLS: Determine how the loop executes and terminates")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "action"
                                 (LIST
                                  (LIST
                                   "do"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "do"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE-ESPLICED
                                     "forms"
                                     (LIST :FORM "form" T))
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "action clause"
                                 :HTYPE
                                 :LOOP-ACTION
                                 :NEWLINE
                                 :DESCRIPTION
                                 "BODY: Forms to be iterated")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "results"
                                 (LIST
                                  (LIST
                                   "append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "aggregation clause"
                                 :HTYPE
                                 :LOOP-AGGREGATE
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "RESULT SECTION: Determines result returned by the loop")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "finally"
                                 (LIST
                                  (LIST
                                   "finally"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "finally"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "action" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "final action"
                                 :HTYPE
                                 :LOOP-FINALLY
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "FINAL ACTION: Performed after last iteration")))
                         '(22138 22133
                           (22143 22148 22153 22198 22203 22223))
                         '(:LABEL "loop" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "LOOP"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "LOOP"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(22143 22138 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-93741 7)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :AGG
                                "initializations"
                                (LIST
                                 :SPROGN
                                 (LIST
                                  :LITERAL
                                  "init"
                                  :CLASS
                                  :LOOP-TAG)
                                 (LIST :ARG "var")
                                 (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                 (LIST :FORM "value" T)
                                 :DELETE-BOX
                                 :MAIN-MENU
                                 :HTYPE
                                 :LOOP-INIT-CLAUSE)
                                :SPLICE
                                :REVEAL-LABEL
                                "inits"
                                :HTYPE
                                :LOOP-INITIALIZATIONS
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "INITIALIZATION SECTION: Variables preset before loop begins"
                                :OPTIONS-LABEL
                                "inits")
                               :SPLICE T :REVEAL-LABEL "inits" :HTYPE
                               :LOOP-INITIALIZATIONS :HIDDEN-NODE T
                               :NEWLINE T :DESCRIPTION
                               "INITIALIZATION SECTION: Variables preset before loop begins"
                               :OPTIONS-LABEL "inits")
                         '(22148 22138 NIL)
                         '(:LABEL "initializations"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET (AREF C-ARRAY-93741 8)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 NIL)
                                (LIST 3 (LIST 0))
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "controls"
                                (LIST
                                 (LIST
                                  "FOR variable IN collection"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "collection"
                                    (LIST
                                     'OR
                                     'SEQUENCE
                                     'HASH-TABLE
                                     'UTILS::GARRAY
                                     'FRAMES:%FRAME))
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) IN list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) = value-list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial values" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "next values"
                                        'LIST)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 TO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "to"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 BELOW n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "below"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 (without limit)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "step function"
                                        'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 DOWNTO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "downto"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "WHILE (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "while"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "UNTIL (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "until"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = list-part"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "on"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = value"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial value" T)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "next value" T)))))
                                   :DELETE-BOX
                                   :NEWLINE)))
                                :HTYPE
                                :LOOP-ITERATOR
                                :NEWLINE
                                :DESCRIPTION
                                "CONTROLS: Determine how the loop executes and terminates"
                                :CHOICE-TYPE
                                :LOOP-ITERATOR
                                :INSTANTIATED
                                :REQUIRED
                                :MULTIPLE
                                :REPEATABLE
                                :SPLICE)
                               :HTYPE :LOOP-ITERATOR :NEWLINE T
                               :DESCRIPTION
                               "CONTROLS: Determine how the loop executes and terminates"
                               :CHOICE-TYPE :LOOP-ITERATOR
                               :INSTANTIATED T :REQUIRED T :MULTIPLE T
                               :REPEATABLE T :SPLICE T)
                         '(22153 22138 (22158)) '(:LABEL "controls"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :COLLAPSED? NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                                (LIST :ARG "var")
                                (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                                (LIST :FORM "first value" 'INTEGER)
                                (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                                (LIST :FORM "last value" 'INTEGER)
                                (LIST
                                 :OPTIONAL-LABELED-ARG
                                 "by"
                                 (LIST
                                  (LIST
                                   "by"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "by"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "step" 'INTEGER)))))
                                :DELETE-BOX
                                :NEWLINE
                                :SPLICE)
                               :DELETE-BOX T :NEWLINE T :SPLICE T)
                         '(22158 22153
                           (22163 22168 22173 22178 22183 22188 22193))
                         'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "for"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(22163 22158 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::J
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SYMBOL-NODE-TYPE :ARG :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::J))
                         '(22168 22158 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "from"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(22173 22158 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 1
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW :FORMAT
                               "~S" :TEMPLATE (LIST :CONSTANT 1))
                         '(22178 22158 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "to"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(22183 22158 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :CONTENTS "" :HOLE-OPEN NIL
                               :HOLE-TEMPLATE
                               (LIST :FORM "last value" 'INTEGER)
                               :TEMPLATE
                               (LIST :FORM "last value" 'INTEGER))
                         '(22188 22158 NIL)
                         '(:LABEL "last value" :RETURN-TYPE INTEGER))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET (AREF C-ARRAY-93741 9)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER))))
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-ARG
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :CHOICE-TYPE :OPTIONAL-LABELED-ARG
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(22193 22158 NIL) '(:LABEL "by"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 10)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "action"
                                (LIST
                                 (LIST
                                  "do"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "do"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :ONE-ESPLICED
                                    "forms"
                                    (LIST :FORM "form" T))
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "action clause"
                                :HTYPE
                                :LOOP-ACTION
                                :NEWLINE
                                :DESCRIPTION
                                "BODY: Forms to be iterated"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "action clause" :HTYPE
                               :LOOP-ACTION :NEWLINE T :DESCRIPTION
                               "BODY: Forms to be iterated"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(22198 22138 NIL) '(:LABEL "action"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 11)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 (LIST 0))
                                (LIST 3 NIL)
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL)
                                (LIST 11 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "results"
                                (LIST
                                 (LIST
                                  "append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "aggregation clause"
                                :HTYPE
                                :LOOP-AGGREGATE
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "RESULT SECTION: Determines result returned by the loop"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "aggregation clause"
                               :HTYPE :LOOP-AGGREGATE :HIDDEN-NODE NIL
                               :NEWLINE T :DESCRIPTION
                               "RESULT SECTION: Determines result returned by the loop"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(22203 22138 (22208)) '(:LABEL "results"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST
                                 :LITERAL
                                 "collect"
                                 :CLASS
                                 :LOOP-TAG)
                                (LIST :FORM "value" T)
                                :DELETE-BOX
                                :SPLICE)
                               :DELETE-BOX T :SPLICE T)
                         '(22208 22203 (22213 22218)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "collect"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(22213 22208 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::J
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SYMBOL-NODE-TYPE :VALUE :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::J))
                         '(22218 22208 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 12)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "finally"
                                (LIST
                                 (LIST
                                  "finally"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "finally"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "action" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "final action"
                                :HTYPE
                                :LOOP-FINALLY
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "FINAL ACTION: Performed after last iteration"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "final action" :HTYPE
                               :LOOP-FINALLY :HIDDEN-NODE T :NEWLINE T
                               :DESCRIPTION
                               "FINAL ACTION: Performed after last iteration"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(22223 22138 NIL) '(:LABEL "finally"))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::J
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SYMBOL-NODE-TYPE :ARG :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::J))
                         '(20998 20798 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL (LIST :TEMPLATE NIL)
                         '(21368 NIL (21373)) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBL:LOOP
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :HIDDEN-NODES (LIST 1 4 5) :CALL-TYPE
                               :MACRO :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "LOOP"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST
                                 :AGG
                                 "initializations"
                                 (LIST
                                  :SPROGN
                                  (LIST
                                   :LITERAL
                                   "init"
                                   :CLASS
                                   :LOOP-TAG)
                                  (LIST :ARG "var")
                                  (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                  (LIST :FORM "value" T)
                                  :DELETE-BOX
                                  :MAIN-MENU
                                  :HTYPE
                                  :LOOP-INIT-CLAUSE)
                                 :SPLICE
                                 :REVEAL-LABEL
                                 "inits"
                                 :HTYPE
                                 :LOOP-INITIALIZATIONS
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "INITIALIZATION SECTION: Variables preset before loop begins"
                                 :OPTIONS-LABEL
                                 "inits")
                                (LIST
                                 :LOOP-ITERATOR
                                 "controls"
                                 (LIST
                                  (LIST
                                   "FOR variable IN collection"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "collection"
                                     (LIST
                                      'OR
                                      'SEQUENCE
                                      'HASH-TABLE
                                      'UTILS::GARRAY
                                      'FRAMES:%FRAME))
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) IN list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) = value-list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial values" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "next values"
                                         'LIST)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 TO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "to"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 BELOW n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "below"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 (without limit)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step function"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 DOWNTO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "downto"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "WHILE (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "while"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "UNTIL (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "until"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = list-part"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "on"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = value"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial value" T)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :FORM "next value" T)))))
                                    :DELETE-BOX
                                    :NEWLINE)))
                                 :HTYPE
                                 :LOOP-ITERATOR
                                 :NEWLINE
                                 :DESCRIPTION
                                 "CONTROLS: Determine how the loop executes and terminates")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "action"
                                 (LIST
                                  (LIST
                                   "do"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "do"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE-ESPLICED
                                     "forms"
                                     (LIST :FORM "form" T))
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "action clause"
                                 :HTYPE
                                 :LOOP-ACTION
                                 :NEWLINE
                                 :DESCRIPTION
                                 "BODY: Forms to be iterated")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "results"
                                 (LIST
                                  (LIST
                                   "append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "aggregation clause"
                                 :HTYPE
                                 :LOOP-AGGREGATE
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "RESULT SECTION: Determines result returned by the loop")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "finally"
                                 (LIST
                                  (LIST
                                   "finally"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "finally"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "action" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "final action"
                                 :HTYPE
                                 :LOOP-FINALLY
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "FINAL ACTION: Performed after last iteration")))
                         '(21373 21368
                           (21378 21383 21388 21433 21438 21443))
                         '(:LABEL "loop" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "LOOP"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "LOOP"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(21378 21373 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-93741 13)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :AGG
                                "initializations"
                                (LIST
                                 :SPROGN
                                 (LIST
                                  :LITERAL
                                  "init"
                                  :CLASS
                                  :LOOP-TAG)
                                 (LIST :ARG "var")
                                 (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                 (LIST :FORM "value" T)
                                 :DELETE-BOX
                                 :MAIN-MENU
                                 :HTYPE
                                 :LOOP-INIT-CLAUSE)
                                :SPLICE
                                :REVEAL-LABEL
                                "inits"
                                :HTYPE
                                :LOOP-INITIALIZATIONS
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "INITIALIZATION SECTION: Variables preset before loop begins"
                                :OPTIONS-LABEL
                                "inits")
                               :SPLICE T :REVEAL-LABEL "inits" :HTYPE
                               :LOOP-INITIALIZATIONS :HIDDEN-NODE T
                               :NEWLINE T :DESCRIPTION
                               "INITIALIZATION SECTION: Variables preset before loop begins"
                               :OPTIONS-LABEL "inits")
                         '(21383 21373 NIL)
                         '(:LABEL "initializations"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 14)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 NIL)
                                (LIST 3 (LIST 0))
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "controls"
                                (LIST
                                 (LIST
                                  "FOR variable IN collection"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "collection"
                                    (LIST
                                     'OR
                                     'SEQUENCE
                                     'HASH-TABLE
                                     'UTILS::GARRAY
                                     'FRAMES:%FRAME))
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) IN list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) = value-list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial values" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "next values"
                                        'LIST)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 TO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "to"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 BELOW n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "below"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 (without limit)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "step function"
                                        'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 DOWNTO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "downto"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "WHILE (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "while"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "UNTIL (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "until"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = list-part"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "on"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = value"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial value" T)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "next value" T)))))
                                   :DELETE-BOX
                                   :NEWLINE)))
                                :HTYPE
                                :LOOP-ITERATOR
                                :NEWLINE
                                :DESCRIPTION
                                "CONTROLS: Determine how the loop executes and terminates"
                                :CHOICE-TYPE
                                :LOOP-ITERATOR
                                :INSTANTIATED
                                :REQUIRED
                                :MULTIPLE
                                :REPEATABLE
                                :SPLICE)
                               :HTYPE :LOOP-ITERATOR :NEWLINE T
                               :DESCRIPTION
                               "CONTROLS: Determine how the loop executes and terminates"
                               :CHOICE-TYPE :LOOP-ITERATOR
                               :INSTANTIATED T :REQUIRED T :MULTIPLE T
                               :REPEATABLE T :SPLICE T)
                         '(21388 21373 (21393)) '(:LABEL "controls"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                                (LIST :ARG "var")
                                (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                                (LIST :FORM "first value" 'INTEGER)
                                (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                                (LIST :FORM "last value" 'INTEGER)
                                (LIST
                                 :OPTIONAL-LABELED-ARG
                                 "by"
                                 (LIST
                                  (LIST
                                   "by"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "by"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "step" 'INTEGER)))))
                                :DELETE-BOX
                                :NEWLINE
                                :SPLICE)
                               :DELETE-BOX T :NEWLINE T :SPLICE T)
                         '(21393 21388
                           (21398 21403 21408 21413 21418 21423 21428))
                         'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "for"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21398 21393 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::J
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SYMBOL-NODE-TYPE :ARG :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::J))
                         '(21403 21393 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "from"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21408 21393 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 1
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW :FORMAT
                               "~S" :TEMPLATE (LIST :CONSTANT 1))
                         '(21413 21393 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "to"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21418 21393 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 10
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW :FORMAT
                               "~S" :TEMPLATE (LIST :CONSTANT 10))
                         '(21423 21393 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 15)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER))))
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-ARG
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :CHOICE-TYPE :OPTIONAL-LABELED-ARG
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21428 21393 NIL) '(:LABEL "by"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 16)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "action"
                                (LIST
                                 (LIST
                                  "do"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "do"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :ONE-ESPLICED
                                    "forms"
                                    (LIST :FORM "form" T))
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "action clause"
                                :HTYPE
                                :LOOP-ACTION
                                :NEWLINE
                                :DESCRIPTION
                                "BODY: Forms to be iterated"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "action clause" :HTYPE
                               :LOOP-ACTION :NEWLINE T :DESCRIPTION
                               "BODY: Forms to be iterated"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21433 21373 NIL) '(:LABEL "action"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 17)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 NIL)
                                (LIST 3 NIL)
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL)
                                (LIST 11 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "results"
                                (LIST
                                 (LIST
                                  "append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "aggregation clause"
                                :HTYPE
                                :LOOP-AGGREGATE
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "RESULT SECTION: Determines result returned by the loop"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "aggregation clause"
                               :HTYPE :LOOP-AGGREGATE :HIDDEN-NODE T
                               :NEWLINE T :DESCRIPTION
                               "RESULT SECTION: Determines result returned by the loop"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21438 21373 NIL) '(:LABEL "results"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 18)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "finally"
                                (LIST
                                 (LIST
                                  "finally"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "finally"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "action" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "final action"
                                :HTYPE
                                :LOOP-FINALLY
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "FINAL ACTION: Performed after last iteration"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "final action" :HTYPE
                               :LOOP-FINALLY :HIDDEN-NODE T :NEWLINE T
                               :DESCRIPTION
                               "FINAL ACTION: Performed after last iteration"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21443 21373 NIL) '(:LABEL "finally"))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL (LIST :TEMPLATE NIL)
                         '(21003 NIL (21008)) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBL:LOOP
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :HIDDEN-NODES (LIST 1 4 5) :CALL-TYPE
                               :MACRO :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "LOOP"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST
                                 :AGG
                                 "initializations"
                                 (LIST
                                  :SPROGN
                                  (LIST
                                   :LITERAL
                                   "init"
                                   :CLASS
                                   :LOOP-TAG)
                                  (LIST :ARG "var")
                                  (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                  (LIST :FORM "value" T)
                                  :DELETE-BOX
                                  :MAIN-MENU
                                  :HTYPE
                                  :LOOP-INIT-CLAUSE)
                                 :SPLICE
                                 :REVEAL-LABEL
                                 "inits"
                                 :HTYPE
                                 :LOOP-INITIALIZATIONS
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "INITIALIZATION SECTION: Variables preset before loop begins"
                                 :OPTIONS-LABEL
                                 "inits")
                                (LIST
                                 :LOOP-ITERATOR
                                 "controls"
                                 (LIST
                                  (LIST
                                   "FOR variable IN collection"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "collection"
                                     (LIST
                                      'OR
                                      'SEQUENCE
                                      'HASH-TABLE
                                      'UTILS::GARRAY
                                      'FRAMES:%FRAME))
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) IN list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) = value-list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial values" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "next values"
                                         'LIST)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 TO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "to"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 BELOW n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "below"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 (without limit)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step function"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 DOWNTO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "downto"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "WHILE (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "while"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "UNTIL (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "until"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = list-part"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "on"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = value"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial value" T)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :FORM "next value" T)))))
                                    :DELETE-BOX
                                    :NEWLINE)))
                                 :HTYPE
                                 :LOOP-ITERATOR
                                 :NEWLINE
                                 :DESCRIPTION
                                 "CONTROLS: Determine how the loop executes and terminates")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "action"
                                 (LIST
                                  (LIST
                                   "do"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "do"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE-ESPLICED
                                     "forms"
                                     (LIST :FORM "form" T))
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "action clause"
                                 :HTYPE
                                 :LOOP-ACTION
                                 :NEWLINE
                                 :DESCRIPTION
                                 "BODY: Forms to be iterated")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "results"
                                 (LIST
                                  (LIST
                                   "append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "aggregation clause"
                                 :HTYPE
                                 :LOOP-AGGREGATE
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "RESULT SECTION: Determines result returned by the loop")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "finally"
                                 (LIST
                                  (LIST
                                   "finally"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "finally"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "action" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "final action"
                                 :HTYPE
                                 :LOOP-FINALLY
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "FINAL ACTION: Performed after last iteration")))
                         '(21008 21003
                           (21013 21018 21023 21068 21073 21078))
                         '(:LABEL "loop" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "LOOP"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "LOOP"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(21013 21008 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-93741 19)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :AGG
                                "initializations"
                                (LIST
                                 :SPROGN
                                 (LIST
                                  :LITERAL
                                  "init"
                                  :CLASS
                                  :LOOP-TAG)
                                 (LIST :ARG "var")
                                 (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                 (LIST :FORM "value" T)
                                 :DELETE-BOX
                                 :MAIN-MENU
                                 :HTYPE
                                 :LOOP-INIT-CLAUSE)
                                :SPLICE
                                :REVEAL-LABEL
                                "inits"
                                :HTYPE
                                :LOOP-INITIALIZATIONS
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "INITIALIZATION SECTION: Variables preset before loop begins"
                                :OPTIONS-LABEL
                                "inits")
                               :SPLICE T :REVEAL-LABEL "inits" :HTYPE
                               :LOOP-INITIALIZATIONS :HIDDEN-NODE T
                               :NEWLINE T :DESCRIPTION
                               "INITIALIZATION SECTION: Variables preset before loop begins"
                               :OPTIONS-LABEL "inits")
                         '(21018 21008 NIL)
                         '(:LABEL "initializations"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 20)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 NIL)
                                (LIST 3 (LIST 0))
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "controls"
                                (LIST
                                 (LIST
                                  "FOR variable IN collection"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "collection"
                                    (LIST
                                     'OR
                                     'SEQUENCE
                                     'HASH-TABLE
                                     'UTILS::GARRAY
                                     'FRAMES:%FRAME))
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) IN list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) = value-list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial values" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "next values"
                                        'LIST)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 TO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "to"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 BELOW n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "below"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 (without limit)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "step function"
                                        'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 DOWNTO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "downto"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "WHILE (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "while"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "UNTIL (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "until"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = list-part"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "on"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = value"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial value" T)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "next value" T)))))
                                   :DELETE-BOX
                                   :NEWLINE)))
                                :HTYPE
                                :LOOP-ITERATOR
                                :NEWLINE
                                :DESCRIPTION
                                "CONTROLS: Determine how the loop executes and terminates"
                                :CHOICE-TYPE
                                :LOOP-ITERATOR
                                :INSTANTIATED
                                :REQUIRED
                                :MULTIPLE
                                :REPEATABLE
                                :SPLICE)
                               :HTYPE :LOOP-ITERATOR :NEWLINE T
                               :DESCRIPTION
                               "CONTROLS: Determine how the loop executes and terminates"
                               :CHOICE-TYPE :LOOP-ITERATOR
                               :INSTANTIATED T :REQUIRED T :MULTIPLE T
                               :REPEATABLE T :SPLICE T)
                         '(21023 21008 (21028)) '(:LABEL "controls"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                                (LIST :ARG "var")
                                (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                                (LIST :FORM "first value" 'INTEGER)
                                (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                                (LIST :FORM "last value" 'INTEGER)
                                (LIST
                                 :OPTIONAL-LABELED-ARG
                                 "by"
                                 (LIST
                                  (LIST
                                   "by"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "by"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "step" 'INTEGER)))))
                                :DELETE-BOX
                                :NEWLINE
                                :SPLICE)
                               :DELETE-BOX T :NEWLINE T :SPLICE T)
                         '(21028 21023
                           (21033 21038 21043 21048 21053 21058 21063))
                         'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "for"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21033 21028 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::J
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SYMBOL-NODE-TYPE :ARG :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::J))
                         '(21038 21028 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "from"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21043 21028 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :CONTENTS "" :HOLE-OPEN T :HOLE-TEMPLATE
                               (LIST :FORM "first value" 'INTEGER)
                               :TEMPLATE
                               (LIST :FORM "first value" 'INTEGER))
                         '(21048 21028 NIL)
                         '(:LABEL "first value" :RETURN-TYPE INTEGER))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "to"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21053 21028 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :CONTENTS "" :HOLE-OPEN NIL
                               :HOLE-TEMPLATE
                               (LIST :FORM "last value" 'INTEGER)
                               :TEMPLATE
                               (LIST :FORM "last value" 'INTEGER))
                         '(21058 21028 NIL)
                         '(:LABEL "last value" :RETURN-TYPE INTEGER))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 21)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER))))
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-ARG
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :CHOICE-TYPE :OPTIONAL-LABELED-ARG
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21063 21028 NIL) '(:LABEL "by"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 22)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "action"
                                (LIST
                                 (LIST
                                  "do"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "do"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :ONE-ESPLICED
                                    "forms"
                                    (LIST :FORM "form" T))
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "action clause"
                                :HTYPE
                                :LOOP-ACTION
                                :NEWLINE
                                :DESCRIPTION
                                "BODY: Forms to be iterated"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "action clause" :HTYPE
                               :LOOP-ACTION :NEWLINE T :DESCRIPTION
                               "BODY: Forms to be iterated"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21068 21008 NIL) '(:LABEL "action"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 23)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 NIL)
                                (LIST 3 NIL)
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL)
                                (LIST 11 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "results"
                                (LIST
                                 (LIST
                                  "append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "aggregation clause"
                                :HTYPE
                                :LOOP-AGGREGATE
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "RESULT SECTION: Determines result returned by the loop"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "aggregation clause"
                               :HTYPE :LOOP-AGGREGATE :HIDDEN-NODE T
                               :NEWLINE T :DESCRIPTION
                               "RESULT SECTION: Determines result returned by the loop"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21073 21008 NIL) '(:LABEL "results"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 24)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "finally"
                                (LIST
                                 (LIST
                                  "finally"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "finally"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "action" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "final action"
                                :HTYPE
                                :LOOP-FINALLY
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "FINAL ACTION: Performed after last iteration"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "final action" :HTYPE
                               :LOOP-FINALLY :HIDDEN-NODE T :NEWLINE T
                               :DESCRIPTION
                               "FINAL ACTION: Performed after last iteration"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21078 21008 NIL) '(:LABEL "finally"))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL (LIST :TEMPLATE NIL)
                         '(22228 NIL (22233)) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 1
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW :FORMAT
                               "~S" :TEMPLATE (LIST :CONSTANT 1))
                         '(21083 20798 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBL:LOOP
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :HIDDEN-NODES (LIST 1 5) :CALL-TYPE
                               :MACRO :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "LOOP"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST
                                 :AGG
                                 "initializations"
                                 (LIST
                                  :SPROGN
                                  (LIST
                                   :LITERAL
                                   "init"
                                   :CLASS
                                   :LOOP-TAG)
                                  (LIST :ARG "var")
                                  (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                  (LIST :FORM "value" T)
                                  :DELETE-BOX
                                  :MAIN-MENU
                                  :HTYPE
                                  :LOOP-INIT-CLAUSE)
                                 :SPLICE
                                 :REVEAL-LABEL
                                 "inits"
                                 :HTYPE
                                 :LOOP-INITIALIZATIONS
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "INITIALIZATION SECTION: Variables preset before loop begins"
                                 :OPTIONS-LABEL
                                 "inits")
                                (LIST
                                 :LOOP-ITERATOR
                                 "controls"
                                 (LIST
                                  (LIST
                                   "FOR variable IN collection"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "collection"
                                     (LIST
                                      'OR
                                      'SEQUENCE
                                      'HASH-TABLE
                                      'UTILS::GARRAY
                                      'FRAMES:%FRAME))
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) IN list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) = value-list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial values" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "next values"
                                         'LIST)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 TO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "to"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 BELOW n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "below"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 (without limit)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step function"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 DOWNTO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "downto"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "WHILE (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "while"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "UNTIL (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "until"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = list-part"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "on"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = value"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial value" T)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :FORM "next value" T)))))
                                    :DELETE-BOX
                                    :NEWLINE)))
                                 :HTYPE
                                 :LOOP-ITERATOR
                                 :NEWLINE
                                 :DESCRIPTION
                                 "CONTROLS: Determine how the loop executes and terminates")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "action"
                                 (LIST
                                  (LIST
                                   "do"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "do"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE-ESPLICED
                                     "forms"
                                     (LIST :FORM "form" T))
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "action clause"
                                 :HTYPE
                                 :LOOP-ACTION
                                 :NEWLINE
                                 :DESCRIPTION
                                 "BODY: Forms to be iterated")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "results"
                                 (LIST
                                  (LIST
                                   "append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "aggregation clause"
                                 :HTYPE
                                 :LOOP-AGGREGATE
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "RESULT SECTION: Determines result returned by the loop")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "finally"
                                 (LIST
                                  (LIST
                                   "finally"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "finally"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "action" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "final action"
                                 :HTYPE
                                 :LOOP-FINALLY
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "FINAL ACTION: Performed after last iteration")))
                         '(22233 22228
                           (22238 22243 22248 22293 22298 22318))
                         '(:LABEL "loop" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-93741 25)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :AGG
                                "initializations"
                                (LIST
                                 :SPROGN
                                 (LIST
                                  :LITERAL
                                  "init"
                                  :CLASS
                                  :LOOP-TAG)
                                 (LIST :ARG "var")
                                 (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                 (LIST :FORM "value" T)
                                 :DELETE-BOX
                                 :MAIN-MENU
                                 :HTYPE
                                 :LOOP-INIT-CLAUSE)
                                :SPLICE
                                :REVEAL-LABEL
                                "inits"
                                :HTYPE
                                :LOOP-INITIALIZATIONS
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "INITIALIZATION SECTION: Variables preset before loop begins"
                                :OPTIONS-LABEL
                                "inits")
                               :SPLICE T :REVEAL-LABEL "inits" :HTYPE
                               :LOOP-INITIALIZATIONS :HIDDEN-NODE T
                               :NEWLINE T :DESCRIPTION
                               "INITIALIZATION SECTION: Variables preset before loop begins"
                               :OPTIONS-LABEL "inits")
                         '(22243 22233 NIL)
                         '(:LABEL "initializations"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 26)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 NIL)
                                (LIST 3 (LIST 0))
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "controls"
                                (LIST
                                 (LIST
                                  "FOR variable IN collection"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "collection"
                                    (LIST
                                     'OR
                                     'SEQUENCE
                                     'HASH-TABLE
                                     'UTILS::GARRAY
                                     'FRAMES:%FRAME))
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) IN list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) = value-list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial values" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "next values"
                                        'LIST)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 TO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "to"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 BELOW n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "below"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 (without limit)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "step function"
                                        'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 DOWNTO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "downto"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "WHILE (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "while"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "UNTIL (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "until"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = list-part"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "on"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = value"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial value" T)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "next value" T)))))
                                   :DELETE-BOX
                                   :NEWLINE)))
                                :HTYPE
                                :LOOP-ITERATOR
                                :NEWLINE
                                :DESCRIPTION
                                "CONTROLS: Determine how the loop executes and terminates"
                                :CHOICE-TYPE
                                :LOOP-ITERATOR
                                :INSTANTIATED
                                :REQUIRED
                                :MULTIPLE
                                :REPEATABLE
                                :SPLICE)
                               :HTYPE :LOOP-ITERATOR :NEWLINE T
                               :DESCRIPTION
                               "CONTROLS: Determine how the loop executes and terminates"
                               :CHOICE-TYPE :LOOP-ITERATOR
                               :INSTANTIATED T :REQUIRED T :MULTIPLE T
                               :REPEATABLE T :SPLICE T)
                         '(22248 22233 (22253)) '(:LABEL "controls"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :COLLAPSED? NIL :BACKGROUND-COLOR
                               :ELHAI-YELLOW :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                                (LIST :ARG "var")
                                (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                                (LIST :FORM "first value" 'INTEGER)
                                (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                                (LIST :FORM "last value" 'INTEGER)
                                (LIST
                                 :OPTIONAL-LABELED-ARG
                                 "by"
                                 (LIST
                                  (LIST
                                   "by"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "by"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "step" 'INTEGER)))))
                                :DELETE-BOX
                                :NEWLINE
                                :SPLICE)
                               :DELETE-BOX T :NEWLINE T :SPLICE T)
                         '(22253 22248
                           (22258 22263 22268 22273 22278 22283 22288))
                         'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "for"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(22258 22253 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::J
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SYMBOL-NODE-TYPE :ARG :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::J))
                         '(22263 22253 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "from"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(22268 22253 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 1
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW :FORMAT
                               "~S" :TEMPLATE (LIST :CONSTANT 1))
                         '(22273 22253 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "to"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(22278 22253 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :CONTENTS NIL :HOLE-OPEN T
                               :HOLE-TEMPLATE
                               (LIST :FORM "last value" 'INTEGER)
                               :TEMPLATE
                               (LIST :FORM "last value" 'INTEGER))
                         '(22283 22253 NIL)
                         '(:LABEL "last value" :RETURN-TYPE INTEGER))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 27)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER))))
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-ARG
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :CHOICE-TYPE :OPTIONAL-LABELED-ARG
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(22288 22253 NIL) '(:LABEL "by"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 28)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "action"
                                (LIST
                                 (LIST
                                  "do"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "do"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :ONE-ESPLICED
                                    "forms"
                                    (LIST :FORM "form" T))
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "action clause"
                                :HTYPE
                                :LOOP-ACTION
                                :NEWLINE
                                :DESCRIPTION
                                "BODY: Forms to be iterated"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "action clause" :HTYPE
                               :LOOP-ACTION :NEWLINE T :DESCRIPTION
                               "BODY: Forms to be iterated"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(22293 22233 NIL) '(:LABEL "action"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 29)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 (LIST 0))
                                (LIST 3 NIL)
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL)
                                (LIST 11 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "results"
                                (LIST
                                 (LIST
                                  "append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "aggregation clause"
                                :HTYPE
                                :LOOP-AGGREGATE
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "RESULT SECTION: Determines result returned by the loop"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "aggregation clause"
                               :HTYPE :LOOP-AGGREGATE :HIDDEN-NODE NIL
                               :NEWLINE T :DESCRIPTION
                               "RESULT SECTION: Determines result returned by the loop"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(22298 22233 (22303)) '(:LABEL "results"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST
                                 :LITERAL
                                 "collect"
                                 :CLASS
                                 :LOOP-TAG)
                                (LIST :FORM "value" T)
                                :DELETE-BOX
                                :SPLICE)
                               :DELETE-BOX T :SPLICE T)
                         '(22303 22298 (22308 22313)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "collect"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(22308 22303 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::J
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SYMBOL-NODE-TYPE :VALUE :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::J))
                         '(22313 22303 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 30)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "finally"
                                (LIST
                                 (LIST
                                  "finally"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "finally"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "action" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "final action"
                                :HTYPE
                                :LOOP-FINALLY
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "FINAL ACTION: Performed after last iteration"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "final action" :HTYPE
                               :LOOP-FINALLY :HIDDEN-NODE T :NEWLINE T
                               :DESCRIPTION
                               "FINAL ACTION: Performed after last iteration"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(22318 22233 NIL) '(:LABEL "finally"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "LOOP"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "LOOP"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(22238 22233 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 11
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW :FORMAT
                               "~S" :TEMPLATE (LIST :CONSTANT 11))
                         '(22323 20798 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL (LIST :TEMPLATE NIL)
                         '(21448 NIL (21453)) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBL:LOOP
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :HIDDEN-NODES (LIST 1 5) :CALL-TYPE
                               :MACRO :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "LOOP"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST
                                 :AGG
                                 "initializations"
                                 (LIST
                                  :SPROGN
                                  (LIST
                                   :LITERAL
                                   "init"
                                   :CLASS
                                   :LOOP-TAG)
                                  (LIST :ARG "var")
                                  (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                  (LIST :FORM "value" T)
                                  :DELETE-BOX
                                  :MAIN-MENU
                                  :HTYPE
                                  :LOOP-INIT-CLAUSE)
                                 :SPLICE
                                 :REVEAL-LABEL
                                 "inits"
                                 :HTYPE
                                 :LOOP-INITIALIZATIONS
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "INITIALIZATION SECTION: Variables preset before loop begins"
                                 :OPTIONS-LABEL
                                 "inits")
                                (LIST
                                 :LOOP-ITERATOR
                                 "controls"
                                 (LIST
                                  (LIST
                                   "FOR variable IN collection"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "collection"
                                     (LIST
                                      'OR
                                      'SEQUENCE
                                      'HASH-TABLE
                                      'UTILS::GARRAY
                                      'FRAMES:%FRAME))
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) IN list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) = value-list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial values" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "next values"
                                         'LIST)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 TO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "to"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 BELOW n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "below"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 (without limit)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step function"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 DOWNTO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "downto"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "WHILE (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "while"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "UNTIL (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "until"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = list-part"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "on"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = value"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial value" T)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :FORM "next value" T)))))
                                    :DELETE-BOX
                                    :NEWLINE)))
                                 :HTYPE
                                 :LOOP-ITERATOR
                                 :NEWLINE
                                 :DESCRIPTION
                                 "CONTROLS: Determine how the loop executes and terminates")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "action"
                                 (LIST
                                  (LIST
                                   "do"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "do"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE-ESPLICED
                                     "forms"
                                     (LIST :FORM "form" T))
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "action clause"
                                 :HTYPE
                                 :LOOP-ACTION
                                 :NEWLINE
                                 :DESCRIPTION
                                 "BODY: Forms to be iterated")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "results"
                                 (LIST
                                  (LIST
                                   "append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "aggregation clause"
                                 :HTYPE
                                 :LOOP-AGGREGATE
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "RESULT SECTION: Determines result returned by the loop")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "finally"
                                 (LIST
                                  (LIST
                                   "finally"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "finally"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "action" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "final action"
                                 :HTYPE
                                 :LOOP-FINALLY
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "FINAL ACTION: Performed after last iteration")))
                         '(21453 21448
                           (21458 21463 21468 21513 21518 21523))
                         '(:LABEL "loop" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "LOOP"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "LOOP"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(21458 21453 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-93741 31)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :AGG
                                "initializations"
                                (LIST
                                 :SPROGN
                                 (LIST
                                  :LITERAL
                                  "init"
                                  :CLASS
                                  :LOOP-TAG)
                                 (LIST :ARG "var")
                                 (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                 (LIST :FORM "value" T)
                                 :DELETE-BOX
                                 :MAIN-MENU
                                 :HTYPE
                                 :LOOP-INIT-CLAUSE)
                                :SPLICE
                                :REVEAL-LABEL
                                "inits"
                                :HTYPE
                                :LOOP-INITIALIZATIONS
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "INITIALIZATION SECTION: Variables preset before loop begins"
                                :OPTIONS-LABEL
                                "inits")
                               :SPLICE T :REVEAL-LABEL "inits" :HTYPE
                               :LOOP-INITIALIZATIONS :HIDDEN-NODE T
                               :NEWLINE T :DESCRIPTION
                               "INITIALIZATION SECTION: Variables preset before loop begins"
                               :OPTIONS-LABEL "inits")
                         '(21463 21453 NIL)
                         '(:LABEL "initializations"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 32)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 NIL)
                                (LIST 3 (LIST 0))
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "controls"
                                (LIST
                                 (LIST
                                  "FOR variable IN collection"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "collection"
                                    (LIST
                                     'OR
                                     'SEQUENCE
                                     'HASH-TABLE
                                     'UTILS::GARRAY
                                     'FRAMES:%FRAME))
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) IN list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) = value-list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial values" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "next values"
                                        'LIST)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 TO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "to"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 BELOW n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "below"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 (without limit)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "step function"
                                        'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 DOWNTO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "downto"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "WHILE (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "while"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "UNTIL (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "until"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = list-part"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "on"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = value"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial value" T)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "next value" T)))))
                                   :DELETE-BOX
                                   :NEWLINE)))
                                :HTYPE
                                :LOOP-ITERATOR
                                :NEWLINE
                                :DESCRIPTION
                                "CONTROLS: Determine how the loop executes and terminates"
                                :CHOICE-TYPE
                                :LOOP-ITERATOR
                                :INSTANTIATED
                                :REQUIRED
                                :MULTIPLE
                                :REPEATABLE
                                :SPLICE)
                               :HTYPE :LOOP-ITERATOR :NEWLINE T
                               :DESCRIPTION
                               "CONTROLS: Determine how the loop executes and terminates"
                               :CHOICE-TYPE :LOOP-ITERATOR
                               :INSTANTIATED T :REQUIRED T :MULTIPLE T
                               :REPEATABLE T :SPLICE T)
                         '(21468 21453 (21473)) '(:LABEL "controls"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                                (LIST :ARG "var")
                                (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                                (LIST :FORM "first value" 'INTEGER)
                                (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                                (LIST :FORM "last value" 'INTEGER)
                                (LIST
                                 :OPTIONAL-LABELED-ARG
                                 "by"
                                 (LIST
                                  (LIST
                                   "by"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "by"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "step" 'INTEGER)))))
                                :DELETE-BOX
                                :NEWLINE
                                :SPLICE)
                               :DELETE-BOX T :NEWLINE T :SPLICE T)
                         '(21473 21468
                           (21478 21483 21488 21493 21498 21503 21508))
                         'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "for"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21478 21473 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::J
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SYMBOL-NODE-TYPE :ARG :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::J))
                         '(21483 21473 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "from"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21488 21473 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 1
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW :FORMAT
                               "~S" :TEMPLATE (LIST :CONSTANT 1))
                         '(21493 21473 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "to"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21498 21473 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 10
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW :FORMAT
                               "~S" :TEMPLATE (LIST :CONSTANT 10))
                         '(21503 21473 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 33)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER))))
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-ARG
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :CHOICE-TYPE :OPTIONAL-LABELED-ARG
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21508 21473 NIL) '(:LABEL "by"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 34)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "action"
                                (LIST
                                 (LIST
                                  "do"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "do"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :ONE-ESPLICED
                                    "forms"
                                    (LIST :FORM "form" T))
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "action clause"
                                :HTYPE
                                :LOOP-ACTION
                                :NEWLINE
                                :DESCRIPTION
                                "BODY: Forms to be iterated"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "action clause" :HTYPE
                               :LOOP-ACTION :NEWLINE T :DESCRIPTION
                               "BODY: Forms to be iterated"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21513 21453 NIL) '(:LABEL "action"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 35)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 NIL)
                                (LIST 3 NIL)
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL)
                                (LIST 11 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "results"
                                (LIST
                                 (LIST
                                  "append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "aggregation clause"
                                :HTYPE
                                :LOOP-AGGREGATE
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "RESULT SECTION: Determines result returned by the loop"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "aggregation clause"
                               :HTYPE :LOOP-AGGREGATE :HIDDEN-NODE NIL
                               :NEWLINE T :DESCRIPTION
                               "RESULT SECTION: Determines result returned by the loop"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21518 21453 NIL) '(:LABEL "results"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 36)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "finally"
                                (LIST
                                 (LIST
                                  "finally"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "finally"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "action" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "final action"
                                :HTYPE
                                :LOOP-FINALLY
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "FINAL ACTION: Performed after last iteration"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "final action" :HTYPE
                               :LOOP-FINALLY :HIDDEN-NODE T :NEWLINE T
                               :DESCRIPTION
                               "FINAL ACTION: Performed after last iteration"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21523 21453 NIL) '(:LABEL "finally"))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL (LIST :TEMPLATE NIL)
                         '(21088 NIL (21093)) 'NIL)
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST
                                 :LITERAL
                                 "collect"
                                 :CLASS
                                 :LOOP-TAG)
                                (LIST :FORM "value" T)
                                :DELETE-BOX
                                :SPLICE)
                               :DELETE-BOX T :SPLICE T)
                         '(21528 20463 (21533 21733)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "collect"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21533 21528 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBL:LOOP
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :HIDDEN-NODES (LIST 1 4 5) :CALL-TYPE
                               :MACRO :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "LOOP"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST
                                 :AGG
                                 "initializations"
                                 (LIST
                                  :SPROGN
                                  (LIST
                                   :LITERAL
                                   "init"
                                   :CLASS
                                   :LOOP-TAG)
                                  (LIST :ARG "var")
                                  (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                  (LIST :FORM "value" T)
                                  :DELETE-BOX
                                  :MAIN-MENU
                                  :HTYPE
                                  :LOOP-INIT-CLAUSE)
                                 :SPLICE
                                 :REVEAL-LABEL
                                 "inits"
                                 :HTYPE
                                 :LOOP-INITIALIZATIONS
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "INITIALIZATION SECTION: Variables preset before loop begins"
                                 :OPTIONS-LABEL
                                 "inits")
                                (LIST
                                 :LOOP-ITERATOR
                                 "controls"
                                 (LIST
                                  (LIST
                                   "FOR variable IN collection"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "collection"
                                     (LIST
                                      'OR
                                      'SEQUENCE
                                      'HASH-TABLE
                                      'UTILS::GARRAY
                                      'FRAMES:%FRAME))
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) IN list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) = value-list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial values" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "next values"
                                         'LIST)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 TO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "to"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 BELOW n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "below"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 (without limit)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step function"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 DOWNTO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "downto"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "WHILE (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "while"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "UNTIL (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "until"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = list-part"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "on"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = value"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial value" T)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :FORM "next value" T)))))
                                    :DELETE-BOX
                                    :NEWLINE)))
                                 :HTYPE
                                 :LOOP-ITERATOR
                                 :NEWLINE
                                 :DESCRIPTION
                                 "CONTROLS: Determine how the loop executes and terminates")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "action"
                                 (LIST
                                  (LIST
                                   "do"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "do"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE-ESPLICED
                                     "forms"
                                     (LIST :FORM "form" T))
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "action clause"
                                 :HTYPE
                                 :LOOP-ACTION
                                 :NEWLINE
                                 :DESCRIPTION
                                 "BODY: Forms to be iterated")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "results"
                                 (LIST
                                  (LIST
                                   "append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "aggregation clause"
                                 :HTYPE
                                 :LOOP-AGGREGATE
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "RESULT SECTION: Determines result returned by the loop")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "finally"
                                 (LIST
                                  (LIST
                                   "finally"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "finally"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "action" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "final action"
                                 :HTYPE
                                 :LOOP-FINALLY
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "FINAL ACTION: Performed after last iteration")))
                         '(21093 21088
                           (21098 21103 21108 21153 21158 21163))
                         '(:LABEL "loop" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "LOOP"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "LOOP"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(21098 21093 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-93741 37)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :AGG
                                "initializations"
                                (LIST
                                 :SPROGN
                                 (LIST
                                  :LITERAL
                                  "init"
                                  :CLASS
                                  :LOOP-TAG)
                                 (LIST :ARG "var")
                                 (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                 (LIST :FORM "value" T)
                                 :DELETE-BOX
                                 :MAIN-MENU
                                 :HTYPE
                                 :LOOP-INIT-CLAUSE)
                                :SPLICE
                                :REVEAL-LABEL
                                "inits"
                                :HTYPE
                                :LOOP-INITIALIZATIONS
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "INITIALIZATION SECTION: Variables preset before loop begins"
                                :OPTIONS-LABEL
                                "inits")
                               :SPLICE T :REVEAL-LABEL "inits" :HTYPE
                               :LOOP-INITIALIZATIONS :HIDDEN-NODE T
                               :NEWLINE T :DESCRIPTION
                               "INITIALIZATION SECTION: Variables preset before loop begins"
                               :OPTIONS-LABEL "inits")
                         '(21103 21093 NIL)
                         '(:LABEL "initializations"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 38)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 NIL)
                                (LIST 3 (LIST 0))
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "controls"
                                (LIST
                                 (LIST
                                  "FOR variable IN collection"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "collection"
                                    (LIST
                                     'OR
                                     'SEQUENCE
                                     'HASH-TABLE
                                     'UTILS::GARRAY
                                     'FRAMES:%FRAME))
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) IN list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) = value-list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial values" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "next values"
                                        'LIST)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 TO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "to"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 BELOW n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "below"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 (without limit)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "step function"
                                        'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 DOWNTO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "downto"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "WHILE (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "while"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "UNTIL (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "until"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = list-part"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "on"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = value"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial value" T)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "next value" T)))))
                                   :DELETE-BOX
                                   :NEWLINE)))
                                :HTYPE
                                :LOOP-ITERATOR
                                :NEWLINE
                                :DESCRIPTION
                                "CONTROLS: Determine how the loop executes and terminates"
                                :CHOICE-TYPE
                                :LOOP-ITERATOR
                                :INSTANTIATED
                                :REQUIRED
                                :MULTIPLE
                                :REPEATABLE
                                :SPLICE)
                               :HTYPE :LOOP-ITERATOR :NEWLINE T
                               :DESCRIPTION
                               "CONTROLS: Determine how the loop executes and terminates"
                               :CHOICE-TYPE :LOOP-ITERATOR
                               :INSTANTIATED T :REQUIRED T :MULTIPLE T
                               :REPEATABLE T :SPLICE T)
                         '(21108 21093 (21113)) '(:LABEL "controls"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                                (LIST :ARG "var")
                                (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                                (LIST :FORM "first value" 'INTEGER)
                                (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                                (LIST :FORM "last value" 'INTEGER)
                                (LIST
                                 :OPTIONAL-LABELED-ARG
                                 "by"
                                 (LIST
                                  (LIST
                                   "by"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "by"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "step" 'INTEGER)))))
                                :DELETE-BOX
                                :NEWLINE
                                :SPLICE)
                               :DELETE-BOX T :NEWLINE T :SPLICE T)
                         '(21113 21108
                           (21118 21123 21128 21133 21138 21143 21148))
                         'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "for"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21118 21113 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::J
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SYMBOL-NODE-TYPE :ARG :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::J))
                         '(21123 21113 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "from"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21128 21113 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 1
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW :FORMAT
                               "~S" :TEMPLATE (LIST :CONSTANT 1))
                         '(21133 21113 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "to"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21138 21113 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :CONTENTS "" :HOLE-OPEN T :HOLE-TEMPLATE
                               (LIST :FORM "last value" 'INTEGER)
                               :TEMPLATE
                               (LIST :FORM "last value" 'INTEGER))
                         '(21143 21113 NIL)
                         '(:LABEL "last value" :RETURN-TYPE INTEGER))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 39)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER))))
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-ARG
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :CHOICE-TYPE :OPTIONAL-LABELED-ARG
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21148 21113 NIL) '(:LABEL "by"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 40)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "action"
                                (LIST
                                 (LIST
                                  "do"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "do"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :ONE-ESPLICED
                                    "forms"
                                    (LIST :FORM "form" T))
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "action clause"
                                :HTYPE
                                :LOOP-ACTION
                                :NEWLINE
                                :DESCRIPTION
                                "BODY: Forms to be iterated"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "action clause" :HTYPE
                               :LOOP-ACTION :NEWLINE T :DESCRIPTION
                               "BODY: Forms to be iterated"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21153 21093 NIL) '(:LABEL "action"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 41)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 NIL)
                                (LIST 3 NIL)
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL)
                                (LIST 11 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "results"
                                (LIST
                                 (LIST
                                  "append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "aggregation clause"
                                :HTYPE
                                :LOOP-AGGREGATE
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "RESULT SECTION: Determines result returned by the loop"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "aggregation clause"
                               :HTYPE :LOOP-AGGREGATE :HIDDEN-NODE T
                               :NEWLINE T :DESCRIPTION
                               "RESULT SECTION: Determines result returned by the loop"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21158 21093 NIL) '(:LABEL "results"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 42)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "finally"
                                (LIST
                                 (LIST
                                  "finally"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "finally"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "action" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "final action"
                                :HTYPE
                                :LOOP-FINALLY
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "FINAL ACTION: Performed after last iteration"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "final action" :HTYPE
                               :LOOP-FINALLY :HIDDEN-NODE T :NEWLINE T
                               :DESCRIPTION
                               "FINAL ACTION: Performed after last iteration"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21163 21093 NIL) '(:LABEL "finally"))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL (LIST :TEMPLATE NIL)
                         '(21543 NIL (21548)) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBL:LOOP
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :HIDDEN-NODES (LIST 1 5) :CALL-TYPE
                               :MACRO :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "LOOP"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST
                                 :AGG
                                 "initializations"
                                 (LIST
                                  :SPROGN
                                  (LIST
                                   :LITERAL
                                   "init"
                                   :CLASS
                                   :LOOP-TAG)
                                  (LIST :ARG "var")
                                  (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                  (LIST :FORM "value" T)
                                  :DELETE-BOX
                                  :MAIN-MENU
                                  :HTYPE
                                  :LOOP-INIT-CLAUSE)
                                 :SPLICE
                                 :REVEAL-LABEL
                                 "inits"
                                 :HTYPE
                                 :LOOP-INITIALIZATIONS
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "INITIALIZATION SECTION: Variables preset before loop begins"
                                 :OPTIONS-LABEL
                                 "inits")
                                (LIST
                                 :LOOP-ITERATOR
                                 "controls"
                                 (LIST
                                  (LIST
                                   "FOR variable IN collection"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "collection"
                                     (LIST
                                      'OR
                                      'SEQUENCE
                                      'HASH-TABLE
                                      'UTILS::GARRAY
                                      'FRAMES:%FRAME))
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) IN list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) = value-list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial values" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "next values"
                                         'LIST)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 TO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "to"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 BELOW n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "below"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 (without limit)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step function"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 DOWNTO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "downto"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "WHILE (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "while"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "UNTIL (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "until"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = list-part"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "on"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = value"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial value" T)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :FORM "next value" T)))))
                                    :DELETE-BOX
                                    :NEWLINE)))
                                 :HTYPE
                                 :LOOP-ITERATOR
                                 :NEWLINE
                                 :DESCRIPTION
                                 "CONTROLS: Determine how the loop executes and terminates")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "action"
                                 (LIST
                                  (LIST
                                   "do"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "do"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE-ESPLICED
                                     "forms"
                                     (LIST :FORM "form" T))
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "action clause"
                                 :HTYPE
                                 :LOOP-ACTION
                                 :NEWLINE
                                 :DESCRIPTION
                                 "BODY: Forms to be iterated")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "results"
                                 (LIST
                                  (LIST
                                   "append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "aggregation clause"
                                 :HTYPE
                                 :LOOP-AGGREGATE
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "RESULT SECTION: Determines result returned by the loop")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "finally"
                                 (LIST
                                  (LIST
                                   "finally"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "finally"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "action" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "final action"
                                 :HTYPE
                                 :LOOP-FINALLY
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "FINAL ACTION: Performed after last iteration")))
                         '(21548 21543
                           (21553 21558 21563 21608 21613 21633))
                         '(:LABEL "loop" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "LOOP"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "LOOP"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(21553 21548 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-93741 43)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :AGG
                                "initializations"
                                (LIST
                                 :SPROGN
                                 (LIST
                                  :LITERAL
                                  "init"
                                  :CLASS
                                  :LOOP-TAG)
                                 (LIST :ARG "var")
                                 (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                 (LIST :FORM "value" T)
                                 :DELETE-BOX
                                 :MAIN-MENU
                                 :HTYPE
                                 :LOOP-INIT-CLAUSE)
                                :SPLICE
                                :REVEAL-LABEL
                                "inits"
                                :HTYPE
                                :LOOP-INITIALIZATIONS
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "INITIALIZATION SECTION: Variables preset before loop begins"
                                :OPTIONS-LABEL
                                "inits")
                               :SPLICE T :REVEAL-LABEL "inits" :HTYPE
                               :LOOP-INITIALIZATIONS :HIDDEN-NODE T
                               :NEWLINE T :DESCRIPTION
                               "INITIALIZATION SECTION: Variables preset before loop begins"
                               :OPTIONS-LABEL "inits")
                         '(21558 21548 NIL)
                         '(:LABEL "initializations"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 44)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 NIL)
                                (LIST 3 (LIST 0))
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "controls"
                                (LIST
                                 (LIST
                                  "FOR variable IN collection"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "collection"
                                    (LIST
                                     'OR
                                     'SEQUENCE
                                     'HASH-TABLE
                                     'UTILS::GARRAY
                                     'FRAMES:%FRAME))
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) IN list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) = value-list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial values" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "next values"
                                        'LIST)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 TO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "to"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 BELOW n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "below"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 (without limit)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "step function"
                                        'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 DOWNTO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "downto"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "WHILE (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "while"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "UNTIL (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "until"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = list-part"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "on"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = value"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial value" T)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "next value" T)))))
                                   :DELETE-BOX
                                   :NEWLINE)))
                                :HTYPE
                                :LOOP-ITERATOR
                                :NEWLINE
                                :DESCRIPTION
                                "CONTROLS: Determine how the loop executes and terminates"
                                :CHOICE-TYPE
                                :LOOP-ITERATOR
                                :INSTANTIATED
                                :REQUIRED
                                :MULTIPLE
                                :REPEATABLE
                                :SPLICE)
                               :HTYPE :LOOP-ITERATOR :NEWLINE T
                               :DESCRIPTION
                               "CONTROLS: Determine how the loop executes and terminates"
                               :CHOICE-TYPE :LOOP-ITERATOR
                               :INSTANTIATED T :REQUIRED T :MULTIPLE T
                               :REPEATABLE T :SPLICE T)
                         '(21563 21548 (21568)) '(:LABEL "controls"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                                (LIST :ARG "var")
                                (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                                (LIST :FORM "first value" 'INTEGER)
                                (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                                (LIST :FORM "last value" 'INTEGER)
                                (LIST
                                 :OPTIONAL-LABELED-ARG
                                 "by"
                                 (LIST
                                  (LIST
                                   "by"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "by"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "step" 'INTEGER)))))
                                :DELETE-BOX
                                :NEWLINE
                                :SPLICE)
                               :DELETE-BOX T :NEWLINE T :SPLICE T)
                         '(21568 21563
                           (21573 21578 21583 21588 21593 21598 21603))
                         'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "for"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21573 21568 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::J
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SYMBOL-NODE-TYPE :ARG :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::J))
                         '(21578 21568 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBL:LOOP
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :HIDDEN-NODES (LIST 1 5) :CALL-TYPE
                               :MACRO :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "LOOP"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST
                                 :AGG
                                 "initializations"
                                 (LIST
                                  :SPROGN
                                  (LIST
                                   :LITERAL
                                   "init"
                                   :CLASS
                                   :LOOP-TAG)
                                  (LIST :ARG "var")
                                  (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                  (LIST :FORM "value" T)
                                  :DELETE-BOX
                                  :MAIN-MENU
                                  :HTYPE
                                  :LOOP-INIT-CLAUSE)
                                 :SPLICE
                                 :REVEAL-LABEL
                                 "inits"
                                 :HTYPE
                                 :LOOP-INITIALIZATIONS
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "INITIALIZATION SECTION: Variables preset before loop begins"
                                 :OPTIONS-LABEL
                                 "inits")
                                (LIST
                                 :LOOP-ITERATOR
                                 "controls"
                                 (LIST
                                  (LIST
                                   "FOR variable IN collection"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "collection"
                                     (LIST
                                      'OR
                                      'SEQUENCE
                                      'HASH-TABLE
                                      'UTILS::GARRAY
                                      'FRAMES:%FRAME))
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) IN list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) = value-list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial values" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "next values"
                                         'LIST)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 TO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "to"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 BELOW n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "below"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 (without limit)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step function"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 DOWNTO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "downto"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "WHILE (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "while"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "UNTIL (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "until"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = list-part"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "on"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = value"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial value" T)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :FORM "next value" T)))))
                                    :DELETE-BOX
                                    :NEWLINE)))
                                 :HTYPE
                                 :LOOP-ITERATOR
                                 :NEWLINE
                                 :DESCRIPTION
                                 "CONTROLS: Determine how the loop executes and terminates")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "action"
                                 (LIST
                                  (LIST
                                   "do"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "do"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE-ESPLICED
                                     "forms"
                                     (LIST :FORM "form" T))
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "action clause"
                                 :HTYPE
                                 :LOOP-ACTION
                                 :NEWLINE
                                 :DESCRIPTION
                                 "BODY: Forms to be iterated")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "results"
                                 (LIST
                                  (LIST
                                   "append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "aggregation clause"
                                 :HTYPE
                                 :LOOP-AGGREGATE
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "RESULT SECTION: Determines result returned by the loop")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "finally"
                                 (LIST
                                  (LIST
                                   "finally"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "finally"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "action" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "final action"
                                 :HTYPE
                                 :LOOP-FINALLY
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "FINAL ACTION: Performed after last iteration")))
                         '(20438 20423
                           (20443 20448 20453 20458 20463 20468))
                         '(:LABEL "loop" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "LOOP"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "LOOP"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(20443 20438 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-93741 45)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :AGG
                                "initializations"
                                (LIST
                                 :SPROGN
                                 (LIST
                                  :LITERAL
                                  "init"
                                  :CLASS
                                  :LOOP-TAG)
                                 (LIST :ARG "var")
                                 (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                 (LIST :FORM "value" T)
                                 :DELETE-BOX
                                 :MAIN-MENU
                                 :HTYPE
                                 :LOOP-INIT-CLAUSE)
                                :SPLICE
                                :REVEAL-LABEL
                                "inits"
                                :HTYPE
                                :LOOP-INITIALIZATIONS
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "INITIALIZATION SECTION: Variables preset before loop begins"
                                :OPTIONS-LABEL
                                "inits")
                               :SPLICE T :REVEAL-LABEL "inits" :HTYPE
                               :LOOP-INITIALIZATIONS :HIDDEN-NODE T
                               :NEWLINE T :DESCRIPTION
                               "INITIALIZATION SECTION: Variables preset before loop begins"
                               :OPTIONS-LABEL "inits")
                         '(20448 20438 NIL)
                         '(:LABEL "initializations"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 46)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 NIL)
                                (LIST 3 (LIST 0))
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "controls"
                                (LIST
                                 (LIST
                                  "FOR variable IN collection"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "collection"
                                    (LIST
                                     'OR
                                     'SEQUENCE
                                     'HASH-TABLE
                                     'UTILS::GARRAY
                                     'FRAMES:%FRAME))
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) IN list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) = value-list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial values" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "next values"
                                        'LIST)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 TO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "to"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 BELOW n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "below"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 (without limit)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "step function"
                                        'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 DOWNTO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "downto"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "WHILE (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "while"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "UNTIL (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "until"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = list-part"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "on"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = value"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial value" T)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "next value" T)))))
                                   :DELETE-BOX
                                   :NEWLINE)))
                                :HTYPE
                                :LOOP-ITERATOR
                                :NEWLINE
                                :DESCRIPTION
                                "CONTROLS: Determine how the loop executes and terminates"
                                :CHOICE-TYPE
                                :LOOP-ITERATOR
                                :INSTANTIATED
                                :REQUIRED
                                :MULTIPLE
                                :REPEATABLE
                                :SPLICE)
                               :HTYPE :LOOP-ITERATOR :NEWLINE T
                               :DESCRIPTION
                               "CONTROLS: Determine how the loop executes and terminates"
                               :CHOICE-TYPE :LOOP-ITERATOR
                               :INSTANTIATED T :REQUIRED T :MULTIPLE T
                               :REPEATABLE T :SPLICE T)
                         '(20453 20438 (20798)) '(:LABEL "controls"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 47)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "action"
                                (LIST
                                 (LIST
                                  "do"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "do"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :ONE-ESPLICED
                                    "forms"
                                    (LIST :FORM "form" T))
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "action clause"
                                :HTYPE
                                :LOOP-ACTION
                                :NEWLINE
                                :DESCRIPTION
                                "BODY: Forms to be iterated"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "action clause" :HTYPE
                               :LOOP-ACTION :NEWLINE T :DESCRIPTION
                               "BODY: Forms to be iterated"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(20458 20438 NIL) '(:LABEL "action"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 48)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 (LIST 0))
                                (LIST 3 NIL)
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL)
                                (LIST 11 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "results"
                                (LIST
                                 (LIST
                                  "append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "aggregation clause"
                                :HTYPE
                                :LOOP-AGGREGATE
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "RESULT SECTION: Determines result returned by the loop"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "aggregation clause"
                               :HTYPE :LOOP-AGGREGATE :HIDDEN-NODE NIL
                               :NEWLINE T :DESCRIPTION
                               "RESULT SECTION: Determines result returned by the loop"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(20463 20438 (21528)) '(:LABEL "results"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 49)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "finally"
                                (LIST
                                 (LIST
                                  "finally"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "finally"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "action" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "final action"
                                :HTYPE
                                :LOOP-FINALLY
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "FINAL ACTION: Performed after last iteration"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "final action" :HTYPE
                               :LOOP-FINALLY :HIDDEN-NODE T :NEWLINE T
                               :DESCRIPTION
                               "FINAL ACTION: Performed after last iteration"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(20468 20438 NIL) '(:LABEL "finally"))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "from"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21583 21568 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 1
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW :FORMAT
                               "~S" :TEMPLATE (LIST :CONSTANT 1))
                         '(21588 21568 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "to"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21593 21568 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 10
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW :FORMAT
                               "~S" :TEMPLATE (LIST :CONSTANT 10))
                         '(21598 21568 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 50)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER))))
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-ARG
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :CHOICE-TYPE :OPTIONAL-LABELED-ARG
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21603 21568 NIL) '(:LABEL "by"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 51)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "action"
                                (LIST
                                 (LIST
                                  "do"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "do"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :ONE-ESPLICED
                                    "forms"
                                    (LIST :FORM "form" T))
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "action clause"
                                :HTYPE
                                :LOOP-ACTION
                                :NEWLINE
                                :DESCRIPTION
                                "BODY: Forms to be iterated"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "action clause" :HTYPE
                               :LOOP-ACTION :NEWLINE T :DESCRIPTION
                               "BODY: Forms to be iterated"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21608 21548 NIL) '(:LABEL "action"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 52)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 (LIST 0))
                                (LIST 3 NIL)
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL)
                                (LIST 11 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "results"
                                (LIST
                                 (LIST
                                  "append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "aggregation clause"
                                :HTYPE
                                :LOOP-AGGREGATE
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "RESULT SECTION: Determines result returned by the loop"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "aggregation clause"
                               :HTYPE :LOOP-AGGREGATE :HIDDEN-NODE NIL
                               :NEWLINE T :DESCRIPTION
                               "RESULT SECTION: Determines result returned by the loop"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21613 21548 (21618)) '(:LABEL "results"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST
                                 :LITERAL
                                 "collect"
                                 :CLASS
                                 :LOOP-TAG)
                                (LIST :FORM "value" T)
                                :DELETE-BOX
                                :SPLICE)
                               :DELETE-BOX T :SPLICE T)
                         '(21618 21613 (21623 21628)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "collect"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21623 21618 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :CONTENTS "" :HOLE-OPEN NIL
                               :HOLE-TEMPLATE (LIST :FORM "value" T)
                               :TEMPLATE (LIST :FORM "value" T))
                         '(21628 21618 NIL)
                         '(:LABEL "value" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 53)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "finally"
                                (LIST
                                 (LIST
                                  "finally"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "finally"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "action" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "final action"
                                :HTYPE
                                :LOOP-FINALLY
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "FINAL ACTION: Performed after last iteration"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "final action" :HTYPE
                               :LOOP-FINALLY :HIDDEN-NODE T :NEWLINE T
                               :DESCRIPTION
                               "FINAL ACTION: Performed after last iteration"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21633 21548 NIL) '(:LABEL "finally"))
     (N-RECREATE-SNIPPET 'TOPLEVEL-OUTPUT-SNIPPET NIL
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :INPUT-FORM
                               (LIST
                                'BBL:LOOP
                                'FOR
                                '$$::J
                                'FROM
                                1
                                'TO
                                11
                                'COLLECT
                                '$$::J)
                               :ERROR-CONDITION NIL :VALUE-STRINGS
                               (LIST "(1 2 3 4 5 6 7 8 9 10 11)")
                               :NVALUES 1 :VALUES
                               (LIST (LIST 1 2 3 4 5 6 7 8 9 10 11))
                               :PRINTOUT "" :RESULT-TYPE :SUCCESS
                               :OUTPUT-INDEX 2 :TEMPLATE NIL)
                         '(22518 20428 (22523)) '(:LABEL "2> "))
     (N-RECREATE-SNIPPET 'OUTPUT-VALUE-SNIPPET
                         "(1 2 3 4 5 6 7 8 9 10 11)"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE NIL)
                         '(22523 22518 NIL)
                         '(:RETURN-TYPE (SIMPLE-ARRAY CHARACTER (25))))
     (N-RECREATE-SNIPPET 'TOPLEVEL-WS-SNIPPET NIL (LIST :TEMPLATE NIL)
                         '(21638 NIL (21643)) 'NIL)
     (N-RECREATE-SNIPPET 'CALL-SNIPPET 'BBL:LOOP
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :HIDDEN-NODES (LIST 1 5) :CALL-TYPE
                               :MACRO :TEMPLATE
                               (LIST
                                (LIST
                                 :LITERAL
                                 "LOOP"
                                 :CLASS
                                 :MACRO-NAME)
                                (LIST
                                 :AGG
                                 "initializations"
                                 (LIST
                                  :SPROGN
                                  (LIST
                                   :LITERAL
                                   "init"
                                   :CLASS
                                   :LOOP-TAG)
                                  (LIST :ARG "var")
                                  (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                  (LIST :FORM "value" T)
                                  :DELETE-BOX
                                  :MAIN-MENU
                                  :HTYPE
                                  :LOOP-INIT-CLAUSE)
                                 :SPLICE
                                 :REVEAL-LABEL
                                 "inits"
                                 :HTYPE
                                 :LOOP-INITIALIZATIONS
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "INITIALIZATION SECTION: Variables preset before loop begins"
                                 :OPTIONS-LABEL
                                 "inits")
                                (LIST
                                 :LOOP-ITERATOR
                                 "controls"
                                 (LIST
                                  (LIST
                                   "FOR variable IN collection"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "collection"
                                     (LIST
                                      'OR
                                      'SEQUENCE
                                      'HASH-TABLE
                                      'UTILS::GARRAY
                                      'FRAMES:%FRAME))
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) IN list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "in"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR (var1 var2...) = value-list"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE
                                     "vars"
                                     (LIST :ARG "var"))
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial values" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "next values"
                                         'LIST)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 TO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "to"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 BELOW n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "below"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 (without limit)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step function"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "FOR variable FROM n1 DOWNTO n2"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "for"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "from"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "first value" 'INTEGER)
                                    (LIST
                                     :LITERAL
                                     "downto"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "last value" 'INTEGER)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST
                                         :FORM
                                         "step"
                                         'INTEGER)))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "WHILE (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "while"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "UNTIL (condition)"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "until"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = list-part"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "on"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "list" 'LIST)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "by"
                                     (LIST
                                      (LIST
                                       "by"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "by"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :ARG "step function")))))
                                    :DELETE-BOX
                                    :NEWLINE))
                                  (LIST
                                   "AS variable = value"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "as"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :ARG "var")
                                    (LIST
                                     :LITERAL
                                     "="
                                     :CLASS
                                     :EQUAL-SIGN)
                                    (LIST :FORM "initial value" T)
                                    (LIST
                                     :OPTIONAL-LABELED-ARG
                                     "then"
                                     (LIST
                                      (LIST
                                       "then"
                                       (LIST
                                        :SPROGN
                                        (LIST
                                         :LITERAL
                                         "then"
                                         :CLASS
                                         :LOOP-TAG)
                                        (LIST :FORM "next value" T)))))
                                    :DELETE-BOX
                                    :NEWLINE)))
                                 :HTYPE
                                 :LOOP-ITERATOR
                                 :NEWLINE
                                 :DESCRIPTION
                                 "CONTROLS: Determine how the loop executes and terminates")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "action"
                                 (LIST
                                  (LIST
                                   "do"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "do"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :ONE-ESPLICED
                                     "forms"
                                     (LIST :FORM "form" T))
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "action clause"
                                 :HTYPE
                                 :LOOP-ACTION
                                 :NEWLINE
                                 :DESCRIPTION
                                 "BODY: Forms to be iterated")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "results"
                                 (LIST
                                  (LIST
                                   "append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... append"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "append"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... collect"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "collect"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... min"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "min"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... max"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "max"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... sum"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "sum"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX))
                                  (LIST
                                   "when ... count"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "when"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST
                                     :FORM
                                     "condition"
                                     'LISP-BOOLEAN)
                                    (LIST
                                     :LITERAL
                                     "count"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "value" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "aggregation clause"
                                 :HTYPE
                                 :LOOP-AGGREGATE
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "RESULT SECTION: Determines result returned by the loop")
                                (LIST
                                 :OPTIONAL-LABELED-CLAUSE
                                 "finally"
                                 (LIST
                                  (LIST
                                   "finally"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "finally"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "action" T)
                                    :DELETE-BOX)))
                                 :REVEAL-LABEL
                                 "final action"
                                 :HTYPE
                                 :LOOP-FINALLY
                                 :HIDDEN-NODE
                                 :NEWLINE
                                 :DESCRIPTION
                                 "FINAL ACTION: Performed after last iteration")))
                         '(21643 21638
                           (21648 21653 21658 21703 21708 21728))
                         '(:LABEL "loop" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "LOOP"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "LOOP"
                                :CLASS
                                :MACRO-NAME)
                               :CLASS :MACRO-NAME)
                         '(21648 21643 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'AGGREGATE-SNIPPET (AREF C-ARRAY-93741 54)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :AGG
                                "initializations"
                                (LIST
                                 :SPROGN
                                 (LIST
                                  :LITERAL
                                  "init"
                                  :CLASS
                                  :LOOP-TAG)
                                 (LIST :ARG "var")
                                 (LIST :LITERAL "=" :CLASS :LOOP-TAG)
                                 (LIST :FORM "value" T)
                                 :DELETE-BOX
                                 :MAIN-MENU
                                 :HTYPE
                                 :LOOP-INIT-CLAUSE)
                                :SPLICE
                                :REVEAL-LABEL
                                "inits"
                                :HTYPE
                                :LOOP-INITIALIZATIONS
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "INITIALIZATION SECTION: Variables preset before loop begins"
                                :OPTIONS-LABEL
                                "inits")
                               :SPLICE T :REVEAL-LABEL "inits" :HTYPE
                               :LOOP-INITIALIZATIONS :HIDDEN-NODE T
                               :NEWLINE T :DESCRIPTION
                               "INITIALIZATION SECTION: Variables preset before loop begins"
                               :OPTIONS-LABEL "inits")
                         '(21653 21643 NIL)
                         '(:LABEL "initializations"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 55)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 NIL)
                                (LIST 3 (LIST 0))
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "controls"
                                (LIST
                                 (LIST
                                  "FOR variable IN collection"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "collection"
                                    (LIST
                                     'OR
                                     'SEQUENCE
                                     'HASH-TABLE
                                     'UTILS::GARRAY
                                     'FRAMES:%FRAME))
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) IN list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "in"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR (var1 var2...) = value-list"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ONE "vars" (LIST :ARG "var"))
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial values" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "next values"
                                        'LIST)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 TO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "to"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 BELOW n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "below"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 (without limit)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST
                                        :FORM
                                        "step function"
                                        'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "FOR variable FROM n1 DOWNTO n2"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "for"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "from"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "first value" 'INTEGER)
                                   (LIST
                                    :LITERAL
                                    "downto"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "last value" 'INTEGER)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "step" 'INTEGER)))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "WHILE (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "while"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "UNTIL (condition)"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "until"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = list-part"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "on"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "list" 'LIST)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "by"
                                    (LIST
                                     (LIST
                                      "by"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "by"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :ARG "step function")))))
                                   :DELETE-BOX
                                   :NEWLINE))
                                 (LIST
                                  "AS variable = value"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "as"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :ARG "var")
                                   (LIST
                                    :LITERAL
                                    "="
                                    :CLASS
                                    :EQUAL-SIGN)
                                   (LIST :FORM "initial value" T)
                                   (LIST
                                    :OPTIONAL-LABELED-ARG
                                    "then"
                                    (LIST
                                     (LIST
                                      "then"
                                      (LIST
                                       :SPROGN
                                       (LIST
                                        :LITERAL
                                        "then"
                                        :CLASS
                                        :LOOP-TAG)
                                       (LIST :FORM "next value" T)))))
                                   :DELETE-BOX
                                   :NEWLINE)))
                                :HTYPE
                                :LOOP-ITERATOR
                                :NEWLINE
                                :DESCRIPTION
                                "CONTROLS: Determine how the loop executes and terminates"
                                :CHOICE-TYPE
                                :LOOP-ITERATOR
                                :INSTANTIATED
                                :REQUIRED
                                :MULTIPLE
                                :REPEATABLE
                                :SPLICE)
                               :HTYPE :LOOP-ITERATOR :NEWLINE T
                               :DESCRIPTION
                               "CONTROLS: Determine how the loop executes and terminates"
                               :CHOICE-TYPE :LOOP-ITERATOR
                               :INSTANTIATED T :REQUIRED T :MULTIPLE T
                               :REPEATABLE T :SPLICE T)
                         '(21658 21643 (21663)) '(:LABEL "controls"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                                (LIST :ARG "var")
                                (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                                (LIST :FORM "first value" 'INTEGER)
                                (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                                (LIST :FORM "last value" 'INTEGER)
                                (LIST
                                 :OPTIONAL-LABELED-ARG
                                 "by"
                                 (LIST
                                  (LIST
                                   "by"
                                   (LIST
                                    :SPROGN
                                    (LIST
                                     :LITERAL
                                     "by"
                                     :CLASS
                                     :LOOP-TAG)
                                    (LIST :FORM "step" 'INTEGER)))))
                                :DELETE-BOX
                                :NEWLINE
                                :SPLICE)
                               :DELETE-BOX T :NEWLINE T :SPLICE T)
                         '(21663 21658
                           (21668 21673 21678 21683 21688 21693 21698))
                         'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "for"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "for" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21668 21663 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::J
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SYMBOL-NODE-TYPE :ARG :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::J))
                         '(21673 21663 NIL) '(:RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "from"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "from" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21678 21663 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 1
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW :FORMAT
                               "~S" :TEMPLATE (LIST :CONSTANT 1))
                         '(21683 21663 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "to"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST :LITERAL "to" :CLASS :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21688 21663 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'CONSTANT-SNIPPET 10
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW :FORMAT
                               "~S" :TEMPLATE (LIST :CONSTANT 10))
                         '(21693 21663 NIL) '(:RETURN-TYPE FIXNUM))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 56)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "by"
                                (LIST
                                 (LIST
                                  "by"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "by"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "step" 'INTEGER))))
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-ARG
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :CHOICE-TYPE :OPTIONAL-LABELED-ARG
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21698 21663 NIL) '(:LABEL "by"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 57)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "action"
                                (LIST
                                 (LIST
                                  "do"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "do"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :ONE-ESPLICED
                                    "forms"
                                    (LIST :FORM "form" T))
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "action clause"
                                :HTYPE
                                :LOOP-ACTION
                                :NEWLINE
                                :DESCRIPTION
                                "BODY: Forms to be iterated"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "action clause" :HTYPE
                               :LOOP-ACTION :NEWLINE T :DESCRIPTION
                               "BODY: Forms to be iterated"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21703 21643 NIL) '(:LABEL "action"))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 58)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES
                               (LIST
                                (LIST 0 NIL)
                                (LIST 1 NIL)
                                (LIST 2 (LIST 0))
                                (LIST 3 NIL)
                                (LIST 4 NIL)
                                (LIST 5 NIL)
                                (LIST 6 NIL)
                                (LIST 7 NIL)
                                (LIST 8 NIL)
                                (LIST 9 NIL)
                                (LIST 10 NIL)
                                (LIST 11 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "results"
                                (LIST
                                 (LIST
                                  "append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... append"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "append"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... collect"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "collect"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... min"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "min"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... max"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "max"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... sum"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "sum"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX))
                                 (LIST
                                  "when ... count"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "when"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST
                                    :FORM
                                    "condition"
                                    'LISP-BOOLEAN)
                                   (LIST
                                    :LITERAL
                                    "count"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "value" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "aggregation clause"
                                :HTYPE
                                :LOOP-AGGREGATE
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "RESULT SECTION: Determines result returned by the loop"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "aggregation clause"
                               :HTYPE :LOOP-AGGREGATE :HIDDEN-NODE NIL
                               :NEWLINE T :DESCRIPTION
                               "RESULT SECTION: Determines result returned by the loop"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21708 21643 (21713)) '(:LABEL "results"))
     (N-RECREATE-SNIPPET 'PROGN-SNIPPET NIL
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :PROGN
                                (LIST
                                 :LITERAL
                                 "collect"
                                 :CLASS
                                 :LOOP-TAG)
                                (LIST :FORM "value" T)
                                :DELETE-BOX
                                :SPLICE)
                               :DELETE-BOX T :SPLICE T)
                         '(21713 21708 (21718 21723)) 'NIL)
     (N-RECREATE-SNIPPET 'LITERAL-SNIPPET "collect"
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :TEMPLATE
                               (LIST
                                :LITERAL
                                "collect"
                                :CLASS
                                :LOOP-TAG)
                               :CLASS :LOOP-TAG)
                         '(21718 21713 NIL) 'NIL)
     (N-RECREATE-SNIPPET 'VALUE-FORM-SNIPPET :|hole|
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :CONTENTS NIL :HOLE-OPEN T
                               :HOLE-TEMPLATE (LIST :FORM "value" T)
                               :TEMPLATE (LIST :FORM "value" T))
                         '(21723 21713 NIL)
                         '(:LABEL "value" :RETURN-TYPE T))
     (N-RECREATE-SNIPPET 'UNIFORM-CHOICE-SNIPPET
                         (AREF C-ARRAY-93741 59)
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SELECTED-CHOICES (LIST (LIST 0 NIL))
                               :TEMPLATE
                               (LIST
                                :CHOICE
                                "finally"
                                (LIST
                                 (LIST
                                  "finally"
                                  (LIST
                                   :SPROGN
                                   (LIST
                                    :LITERAL
                                    "finally"
                                    :CLASS
                                    :LOOP-TAG)
                                   (LIST :FORM "action" T)
                                   :DELETE-BOX)))
                                :REVEAL-LABEL
                                "final action"
                                :HTYPE
                                :LOOP-FINALLY
                                :HIDDEN-NODE
                                :NEWLINE
                                :DESCRIPTION
                                "FINAL ACTION: Performed after last iteration"
                                :CHOICE-TYPE
                                :OPTIONAL-LABELED-CLAUSE
                                :INSTANTIATED
                                :OPTIONAL
                                :SINGLE
                                :NON-REPEATABLE
                                :SPLICE)
                               :REVEAL-LABEL "final action" :HTYPE
                               :LOOP-FINALLY :HIDDEN-NODE T :NEWLINE T
                               :DESCRIPTION
                               "FINAL ACTION: Performed after last iteration"
                               :CHOICE-TYPE :OPTIONAL-LABELED-CLAUSE
                               :INSTANTIATED T :OPTIONAL T :SINGLE T
                               :NON-REPEATABLE T :SPLICE T)
                         '(21728 21643 NIL) '(:LABEL "finally"))
     (N-RECREATE-SNIPPET 'SYMBOL-SNIPPET '$$::J
                         (LIST :BACKGROUND-COLOR :ELHAI-YELLOW
                               :SYMBOL-NODE-TYPE :VALUE :FORMAT "~S"
                               :TEMPLATE (LIST :SYMBOL '$$::J))
                         '(21733 21528 NIL) '(:RETURN-TYPE T))))

(N-RESET-WORKSPACE-POINTERS S-LIST-93742)

(PROGN (UNINTERN 'S-LIST-93742) (UNINTERN 'C-ARRAY-93741))
