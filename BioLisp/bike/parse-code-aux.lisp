;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE :bbi)

(eval-when (:compile-toplevel :load-toplevel :execute)
(DEFUN Char2code (char-or-string)
  (COND
    ((CHARACTERP char-or-string) 
         (CHAR-CODE char-or-string))
    ((NOT (STRINGP char-or-string))
        (ERROR "Non-string or character input to Char2code"))
    ((= (LENGTH char-or-string) 1)
        (CHAR-CODE (CHAR char-or-string 0)))
    (T (ERROR "Multicharacter input to Char2code")))
)

(DEFUN Code2Char (n)
  (COND
     ((NOT (INTEGERP n))
      (ERROR "Input to Code2String must be integer, not ~A" n))
     ((> n 255) (ERROR "Input ('~A') to Code2String too big" n))
     ((< n 0) (ERROR "Input ('~A') to Code2String too small" n))
     (T (STRING (CODE-CHAR n))))
)

)  ; ********** EVAL-WHEN


