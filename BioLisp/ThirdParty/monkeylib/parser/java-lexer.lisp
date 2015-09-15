;; java-lexer.lisp -- PARSER based lexer for Java source code.
;;
;; Copyright (c) 2002 Peter Seibel
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA
;;

(in-package :com.gigamonkeys.java-lexer)

;; 3.3 Unicode Escapes

(defun make-buffer (len)
  (make-array len :element-type 'base-char :fill-pointer t))

(defun unicode-translation (filename)
  (with-open-file (in filename)
    (unicode-escapes in (file-length in))))

(defun unicode-escapes (in output-length)
  (let ((state 'normal)
        (buffer (make-buffer output-length))
        (slash-count 0)
        (hex-buffer (make-string 4))
        (idx 0)
        (hex-count 0))
    (labels ((save-char (c) (setf (aref buffer idx) c) (incf idx))
             (emit-slashes (count)
               (dotimes (i count) (save-char #\\))
               (setq slash-count 0))
             (boom (char state)
               (error "Unexpected char ~A in state ~A at idx ~A"
                      char state idx)))
      (loop for char = (read-char in nil nil)
            while char do
            (case state
              (normal
               (case char
                 (#\\ (incf slash-count) (setq state 'odd-slashes))
                 (t (save-char char))))
          
              (odd-slashes
               (case char
                 (#\\ (incf slash-count)
                      (setq state 'even-slashes))
                 (#\u (emit-slashes (1- slash-count))
                      (setq state 'unicode-markers))
                 (t (emit-slashes slash-count)
                    (save-char char)
                    (setq state 'normal))))
          
              (even-slashes
               (case char
                 (#\\
                  (incf slash-count)
                  (setq state 'odd-slashes))
                 (t
                  (emit-slashes slash-count)
                  (save-char char)
                  (setq state 'normal))))
          
              (unicode-markers
               (case char
                 ((#\u))
                 ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                       #\a #\b #\c #\d #\e #\f
                       #\A #\B #\C #\D #\E #\F)
                  (setf (aref hex-buffer hex-count) char)
                  (incf hex-count)
                  (setq state 'hex))
                 (t (boom char 'unicode-markers))))
              (hex
               (case char
                 ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                       #\a #\b #\c #\d #\e #\f
                       #\A #\B #\C #\D #\E #\F)
                  (setf (aref hex-buffer hex-count) char)
                  (when (= (incf hex-count) 4)
                    (save-char
                     (code-char (parse-integer hex-buffer :radix 16)))
                    (setq hex-count 0)
                    (setq state 'normal)))
                 (t (boom char 'hex))))))
      (case state
        ((odd-slashes even-slashes)
         (emit-slashes slash-count)))
      (setf (fill-pointer buffer) idx)
      buffer)))


;; 3.4 Line terminators

(defprod line-terminator () (/ #\newline (#\return (? #\newline))))

(defchartype input-character
  '(and character (not (member #\newline #\return))))

;; 3.5 Input Elements and Tokens

(defprod input () ((* input-element) (? #\Sub)))

(defprod input-element () (/ white-space comment token))

(defprod token () (/ identifier java-keyword literal separator operator))

;; 3.6 White space

(defprod white-space () (/ #\space #\tab #\page line-terminator))

;; 3.7 Comments

;; Note: the JLS, 2nd. edition incorrectly disallows javadoc-style
;; comments. We correctly allow them.

(defprod comment () (/ traditional-comment end-of-line-comment))

(defprod traditional-comment ()
  ("/*" (* (& (/ input-character line-terminator) (~ "*/"))) "*/"))

(defprod end-of-line-comment ()
  ("//" (* input-character) line-terminator))

;; 3.8 Identifiers

(defprod identifier ()
  (& (java-letter (* java-letter-or-digit))
     (! (/ java-keyword boolean-literal null-literal))))

;; not quite correct unless alpha-char-p and digit-char-p do exactly
;; what we want.

(defchartype java-letter '(or (satisfies alpha-char-p) (member #\_ #\$)))
(defchartype java-digit '(satisfies digit-char-p))
(defchartype java-letter-or-digit '(or java-letter java-digit))

;; 3.9 Keywords
(defprod java-keyword ()
  (/
   "abstract" "boolean" "break" "byte" "case" "catch" "char" "class" "const"
   "continue" "default" ("do" (? "uble")) "else" "extends" ("final" (? "ly"))
   "float" "for" "goto" "if" "implements" "import" "instanceof"
   ("int" (? "erface")) "long" "native" "new" "package" "private" "protected"
   "public" "return" "short" "static" "strictfp" "super" "switch"
   "synchronized" "this" ("throw" (? "s")) "transient" "try" "void"
   "volatile" "while"))

;; 3.10 Literals

(defprod literal ()
  (/ floating-point-literal integer-literal boolean-literal character-literal
     string-literal null-literal))

;; 3.10.1 Integer Literals
(defprod integer-literal ()
  ;; order matters here as a single decimal-integer-literal can
  ;; match a leading 0.
  (/ hex-integer-literal octal-integer-literal decimal-integer-literal))

(defchartype integer-type-suffix '(member #\l #\L))

(defprod decimal-integer-literal () (decimal-numeral (? integer-type-suffix)))
(defprod decimal-numeral () (/ #\0 (non-zero-digit (* digit))))
(defchartype non-zero-digit '(member #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(defchartype digit '(or (eql #\0) non-zero-digit))

(defprod hex-integer-literal () (hex-numeral (? integer-type-suffix)))
(defprod hex-numeral () (#\0 (/ #\x #\X) (+ hex-digit)))
(defchartype hex-digit
  '(or digit (member #\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F)))

(defprod octal-integer-literal () (octal-numeral (? integer-type-suffix)))
(defprod octal-numeral () (#\0 (+ octal-digit)))
(defchartype octal-digit '(member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))

;; 3.10.2 Floating-Point Literals

(defprod floating-point-literal ()
  (/
   ((+ digit)
    (/
     (#\. (* digit) (? exponent-part) (? float-type-suffix))
     (exponent-part (? float-type-suffix))
     float-type-suffix))
   (#\. (+ digit) (? exponent-part) (? float-type-suffix))))

(defprod exponent-part () (exponent-indicator signed-integer))
(defchartype exponent-indicator '(member #\e #\E))
(defprod signed-integer () ((? sign) (+ digit)))
(defchartype sign '(member #\+ #\-))
(defchartype float-type-suffix '(member #\f #\F #\d #\D))
    

;; 3.10.3 Boolean Literals
(defprod boolean-literal () (/ "true" "false"))

;; 3.10.4 Character Literals
(defprod character-literal () (#\' (/ single-character escape-sequence) #\'))
(defchartype single-character '(and input-character (not (member #\' #\\))))

;; 3.10.5 String Literals
(defprod string-literal ()
  (#\" (* (/ (& input-character (~ #\") (~ #\\)) escape-sequence)) #\"))

;; 3.10.6 Espace Sequences for Character and String Literals
(defprod escape-sequence ()
  (#\\
   (/ #\b #\t #\n #\f #\r #\" #\' #\\
      (zero-to-three octal-digit octal-digit)
      (octal-digit octal-digit) octal-digit)))

(defchartype zero-to-three '(member #\0 #\1 #\2 #\3))

;; 3.10.7 The Null Literal

(defprod null-literal () "null")

;; 3.11 Separators

(defchartype separator '(member #\( #\) #\{ #\} #\[ #\] #\; #\, #\.))

;; 3.12 Operators

(defprod operator ()
  (/
   ":"
   "?"
   "~"
   ("!" (? "="))
   ("%" (? "="))
   ("&" (? (/ "=" "&")))
   ("*" (? "="))
   ("+" (? (/ "=" "+")))
   ("-" (? (/ "=" "-")))
   ("/" (? "="))
   ("<" (? "<") (? "="))
   ("=" (? "="))
   (">" (? ">" (? ">")) (? "="))
   ("^" (? "="))
   ("|" (? (/ "=" "|")))))


(deflexer java-lexer (input)
  ((:tokens identifier java-keyword literal separator operator)))

