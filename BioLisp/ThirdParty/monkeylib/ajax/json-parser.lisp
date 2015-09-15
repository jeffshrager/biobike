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

;;; Main productions

(defprod object ()
  ("{" ws (? key-value-pair) (* ws "," ws key-value-pair) ws "}"))

(defprod key-value-pair () (string ws ":" ws value))

(defprod array () 
  ("[" ws (? value) (* ws "," ws value) ws "]"))

(defprod value ()
  (/ string number object array "true" "false" "null"))

(defprod string ()
  ("\"" (* (/ escape string-char)) "\""))

(defprod escape ()
  ("\\" (/ "\"" "\\" "/" "b" "f" "n" "r" "t" ("u" hex hex hex hex))))

(defprod number ()
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
     