;; java-lexer-test.lisp -- unit test for java-lexer.lisp.
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

(defpackage "JAVA-LEXER-TEST"
  (:use "COMMON-LISP" "TEST" "PARSER"))

(in-package "JAVA-LEXER-TEST")

(defvar *cr* (string #\return))
(defvar *lf* (string #\newline))
(defvar *crlf* (concatenate 'string *cr* *lf*))
(defvar *space* (string #\space))
(defvar *tab* (string #\tab))
(defvar *form-feed* (string #\page))

(defun s (&rest args) (apply #'concatenate 'string args))

(defvar *line-terminators* (list *cr* *lf* *crlf*))

(defvar *identifiers* '("java" "lang" "String" "a" "ab" "a1" "foo"))

(defvar *white-space* (list *space* *tab* *form-feed*))

(defvar *comments* 
  (list "/**/"
        "/* text */"
        "/** text */"
        (s "/*" *lf* "foo" *lf* "  */")
        (s "// foo bar baz" *lf*)
        (s "// foo bar baz" *cr*)
        (s "// foo bar baz" *crlf*)))

(defvar *keywords*
  '("abstract" "boolean" "break" "byte" "case" "catch" "char" "class"
    "const" "continue" "default" "do" "double" "else" "extends" "final"
    "finally" "float" "for" "goto" "if" "implements" "import"
    "instanceof" "int" "interface" "long" "native" "new" "package"
    "private" "protected" "public" "return" "short" "static"
    "strictfp" "super" "switch" "synchronized" "this" "throw" "throws"
    "transient" "try" "void" "volatile" "while"))

(defvar *decimal-integer-literals* '("0" "1996" "2"))

(defvar *hex-integer-literals*  '("0x0" "0X0" "0x00FF00FF" "0xDadaCafe"))

(defvar *octal-integer-literals* '("00" "0372"))

(defvar *integer-literals*
  (append *decimal-integer-literals*
          *hex-integer-literals*
          *octal-integer-literals*))

(defvar *floating-point-literals*
  '("1e1f" "2.f" ".3f" "0f" "3.14f" "6.022137e+23f" "1e1" "2." ".3" "0.0"
    "3.14" "1e-9d" "1e137"))

(defvar *boolean-literals* '("true" "false"))

(defvar *character-literals*
  '("'a'" "'%'" "'\\t'" "'\\\\'" "'\\''" "'\\1'" "'\\12'" "'\\123'" "'\\177'"))

(defvar *string-literals*
  '("\"\"" "\"\\\"\"" "\"This is a string\""))

(defvar *escapce-sequences*
  '("\\b" "\\t" "\\n" "\\f" "\\r" "\\\"" "\\'" "\\\\" "\\1" "\\4" "\\12"
    "\\42" "\\123"))

(defvar *null-literals* '("null"))

(defvar *separators*
  '("(" ")" "{" "}" "[" "]" ";" "," "."))

(defvar *operators*
  '("=" "==" "+" "+=" ">" "<=" "-" "-=" "<" ">=" "*" "*=" "!" "!=" "/"
    "/=" "~" "&&" "&" "&=" "?" "||" "|" "|=" ":" "++" "^" "^=" "--" "%"
    "%=" "<<" "<<=" ">>" ">>=" ">>>" ">>>="))

(defvar *literals*
  (append *integer-literals*
          *floating-point-literals*
          *boolean-literals*
          *character-literals*
          *string-literals*
          *null-literals*))
  
(defvar *tokens*
  (append *identifiers*
          *keywords*
          *literals*
          *separators*
          *operators*))

(deftest 3.3-unicode-escapes ()
  (flet ((test (s expected)
           (with-input-from-string (in s)
             (equalp expected (java-lexer::unicode-escapes in (length s)))))
         (s (&rest args)
           (apply #'concatenate 'string (mapcar #'string args))))
    (check
     ;; n.b. since we have to escape \ in string literals, remember
     ;; that every \\ in a string below represents a single slash.
     (test "foo" "foo")
     (test "foo\\" "foo\\")
     (test "foo\\u000dbar" (s "foo" (code-char #x00d) "bar"))
     (test "foo\\uu000dbar" (s "foo" (code-char #x00d) "bar"))
     (test "foo\\\\u000dbar" "foo\\\\u000dbar")
     (test "foo\\\\\\u000dbar" (s "foo\\\\" (code-char #x00d) "bar"))
     (test "foo\\\\\\uu000dbar" (s "foo\\\\" (code-char #x00d) "bar"))
     (test "foo\\\\\\uuu000dbar" (s "foo\\\\" (code-char #x00d) "bar"))
     )))

(deftest 3.4-line-terminators ()
  (parselet ((parser java-lexer::line-terminator))
    (dolist (l *line-terminators*) (check (parser l)))
    (check
     (not (parser (concatenate 'string *lf* *cr*))))))

(deftest 3.5-input-elements-and-tokens ()
  (parselet ((token-parser java-lexer::token)
             (input-element-parser java-lexer::input-element)
             (input-parser java-lexer::input))
    (dolist (tok *tokens*)
      (check
       (token-parser tok)
       (input-element-parser tok)
       (input-parser tok)))
    (dolist (thing (append *white-space* *comments*))
      (check
       (not (token-parser thing))
       (input-element-parser thing)
       (input-parser thing)))
    (check
     (input-parser "")
     (input-parser (string #\Sub))
     (input-parser "package java.lang; import foo.bar.baz.*; class X {}")
     (input-parser (s "package java.lang; import foo.bar.baz.*; class X {}" (string #\Sub))))))

(deftest 3.6-white-space ()
  (parselet ((white-space-parser java-lexer::white-space))
    (dolist (ws *white-space*) (check (white-space-parser ws)))
    (dolist (lt *line-terminators*) (check (white-space-parser lt)))))

(deftest 3.7-comments ()
  (parselet ((comment-parser java-lexer::comment))
    (dolist (c *comments*) (check (comment-parser c)))))

(deftest 3.8-identifiers ()
  (parselet ((parser java-lexer::identifier))
    (dolist (i *identifiers*) (check (parser i)))
    (dolist (k *keywords*) (check (not (parser k))))
    (dolist (k *boolean-literals*) (check (not (parser k))))
    (dolist (k *null-literals*) (check (not (parser k))))
    ;; check a legal identifier that starts with a keyword.
    (check (parser "finalize"))))

(deftest 3.9-keywords ()
  (parselet((keyword-parser java-lexer::java-keyword))
    (dolist (k *keywords*) (check (keyword-parser k)))
    (dolist (i *identifiers*) (check (not (keyword-parser i))))))

(deftest 3.10.1-integer-literals ()
  (parselet ((integer java-lexer::integer-literal)
             (decimal java-lexer::decimal-integer-literal)
             (hex     java-lexer::hex-integer-literal)
             (octal   java-lexer::octal-integer-literal))
    (dolist (i *decimal-integer-literals*)
      (check
       (integer i)
       (decimal i)
       (not (hex i))
       (not (octal i))))
    (dolist (i *hex-integer-literals*)
      (check
       (integer i)
       (hex i)
       (not (decimal i))
       (not (octal i))))
    (dolist (i *octal-integer-literals*)
      (check
       (integer i)
       (octal i)
       (not (decimal i))
       (not (hex i))))))

(deftest 3.10.2-floating-point-literals ()
  (parselet ((floating-point-parser java-lexer::floating-point-literal))
    (dolist (f *floating-point-literals*) (check (floating-point-parser f)))))

(deftest 3.10.3-boolean-literals ()
  (parselet ((boolean-parser java-lexer::boolean-literal))
    (dolist (b *boolean-literals*) (check (boolean-parser b)))))


(deftest 3.10.4-character-literals ()
  (parselet ((character-parser java-lexer::character-literal))
    (dolist (c *character-literals*) (check (character-parser c)))))

(deftest 3.10.5-string-literals ()
  (parselet ((string-parser java-lexer::string-literal))
    (dolist (s *string-literals*) (check (string-parser s)))))

(deftest 3.10.6-escape-sequences ()
  (parselet ((escape-sequence-parser java-lexer::escape-sequence))
    (dolist (e *escapce-sequences*) (check (escape-sequence-parser e)))
    (check (not (escape-sequence-parser "\\423")))))

(deftest 3.10.7-the-null-literal ()
  (parselet ((null-parser java-lexer::null-literal))
    (dolist (n *null-literals*) (check (null-parser n)))
    (check (not (null-parser "NULL")))))

(deftest 3.11-separators ()
  (parselet ((separator-parser java-lexer::separator))
    (dolist (sep *separators*) (check (separator-parser sep)))))

(deftest 3.12-operators ()
  (parselet ((operator-parser java-lexer::operator))
    (dolist (op *operators*) (check (operator-parser op)))))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(defun tests (&optional reload)
  (if reload
      (utils::load-and-test "java-lexer")
      (test::test-package "JAVA-LEXER-TEST")))

(defun old-lex-file (filename)
  (parselet ((parser java-lexer::input))
    (with-open-file (in filename)
      (let ((str (make-string (file-length in))))
        (read-sequence str in)
        (parser str)))))

(defun lex-file (filename)
  (parselet ((parser (^ java-lexer::input)))
    (parser (java-lexer::unicode-translation filename))))

(defun tokenize-file (filename)
  (with-open-file (in filename)
    (let ((str (make-string (file-length in))))
      (read-sequence str in)
      (java-lexer::java-lexer str))))

(defun slurp-file (filename)
  (with-open-file (in filename)
    (let ((str (make-string (file-length in))))
      (read-sequence str in)
      str)))

(defun lex-source-tree ()
  (let ((bad nil))
    (dolist (file (directory (parse-namestring "~/jdk/src/**/*.java")))
      (format t "Lexing file ~A~&" file)
      (unless (lex-file file) (push file bad)))
    bad))



