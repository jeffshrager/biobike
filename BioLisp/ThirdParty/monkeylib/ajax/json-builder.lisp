(in-package :com.gigamonkeys.ajax.json-parser)

;; Parser for JSON syntax (<http://www.json.org/>)

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

;;; Main productions

(defprod object ()
  ((^ "{" (make-hash-table))
   ws 
   (? (@ key-value-pair (save-in-hash key-value-pair object)))
   (* ws "," ws (@ key-value-pair (save-in-hash key-value-pair object)))
   ws "}"))

(defprod key-value-pair () 
  (^ (string ws ":" ws value) (cons (intern (string-upcase string) :keyword) value)))

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

(defprod string ()
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
  (^ number-syntax (read-from-string number-syntax)))
  
(defprod number-syntax ()
  (int (? (/ (frac (? exp)) exp))))

(defprod int ()
  ((? "-") (/ (digit1-9 (* digit)) "0")))

(defprod frac () ("." (* digit)))

(defprod exp () (e (* digit)))

(defprod e () ((/ "e" "E") (? (/ "-" "+"))))

(defparser parse-json (^ value))

(defmacro tjp (production input)
  `((lambda (x)
      (parselet ((foo (^ ,production)))
	(foo x))) ,input))
