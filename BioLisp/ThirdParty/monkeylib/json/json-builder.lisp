(in-package :com.gigamonkeys.json.parser)

;; Parser for JSON syntax (<http://www.json.org/>)

(defvar *empty-object* (make-symbol "EMPTY-OBJECT"))

(defchartype string-char '(not (member #\\ #\")))

(defchartype digit1-9 
  '(member #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defchartype digit
  '(or (eql #\0) digit1-9))

(defchartype hex
  '(or digit (member #\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F)))

(defprod ws ()
  (* (/ #\Space #\Tab #\Newline)))

(defun save-in-hash (key-value-pair hash)
  (setf (gethash (car key-value-pair) hash) (cdr key-value-pair)))

(defun save-key-value-pair (key-value-pair vector)
  (vector-push-extend (car key-value-pair) vector)
  (vector-push-extend (cdr key-value-pair) vector))

;;; Main productions

(defprod object ()
  ((^ "{" (make-array 5 :adjustable t :fill-pointer 0))
   ws 
   (? (@ key-value-pair (save-key-value-pair key-value-pair object)))
   (* ws "," ws (@ key-value-pair (save-key-value-pair key-value-pair object)))
   ws (^ "}" (or (coerce object 'list) *empty-object*))))

(defprod key-value-pair () 
  (^ (string ws ":" ws value) (cons string value)))

(defprod array () 
  ((^ "[" (make-array 5 :adjustable t :fill-pointer 0))
   ws
   (? (@ value (vector-push-extend value array)))
   (* ws "," ws (@ value (vector-push-extend value array)))
   ws "]"))

(defprod value ()
  (/ (^ string)
     (^ number)
     (^ object)
     (^ array)
     (^ "true" :true)
     (^ "false" :false)
     (^ "null" :null)))

(defprod xvalue ()
  (^ array (coerce array 'list)))


(defprod string ()
  ;; In JSON syntax, unlike full Javascript, only double-quoted strings are allowed.
  ((^ "\"" (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))
   (* (@ char-or-escape (vector-push-extend char-or-escape string)))
   "\""))

(defprod char-or-escape ()
  (^ (/ escape string-char)))

(defprod escape ()
  ("\\" 
   (/ (^ "\"" #\")
      (^ "\\" #\\)
      (^ "/" #\/)
      (^ "b" #\Backspace)
      (^ "f" #\Page)
      (^ "n" #\Newline)
      (^ "r" #\Return)
      (^ "t" #\Tab)
      ("u" (^ hex4 (code-char (parse-integer hex4 :radix 16)))))))

(defprod hex4 () (hex hex hex hex))

(defprod number ()
  (^ number-syntax (let ((*read-default-float-format* 'double-float)) (read-from-string number-syntax))))
  
(defprod number-syntax ()
  (int (? (/ (frac (? exp)) exp))))

(defprod int ()
  ((? "-") (/ (digit1-9 (* digit)) "0")))

(defprod frac () ("." (* digit)))

(defprod exp () (e (* digit)))

(defprod e () ((/ "e" "E") (? (/ "-" "+"))))

(defparser json-parser (^ value))

(defun parse-json (text)
  "Parse json text into Lisp objects. Hash tables are used to
represent Javascript objects and vectors to represent arrays."
  (fix-empty-object (nth-value 1 (json-parser text))))

(defun fix-empty-object (json)
  (cond
    ((eql json *empty-object*) ())
    ((consp json) (mapcar #'fix-empty-object json))
    ((stringp json) json)
    ((vectorp json) (map 'vector #'fix-empty-object json))
    (t json)))

(defmacro tjp (production input)
  `((lambda (x)
      (parselet ((foo (^ ,production)))
	(foo x))) ,input))
