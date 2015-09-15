;; parser-test.lisp -- unit test for PARSER parser generator.
;;
;; Copyright (c) 2003 Peter Seibel
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

(defpackage "PARSER-TEST"
  (:use "COMMON-LISP" "TEST"))

(in-package "PARSER-TEST")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;;
;; Make hitting the end of the string cause backtracking. For instance
;; (defparser foo () (/ "a" "ab")) should parse "ab" rather than
;; matching "a" and then failing because we're not at the end of the
;; string. Or not. Maybe it's up to you to write your grammars the
;; right way.
;;
;; Do some left factoring (or whatever it is) of or expressions. E.g.
;; (/ "foo" "foom" "bar") currently needs to be written as (/ ("foo"
;; (? "m")) "bar") since otherwise, when looking at "foom" it will
;; match "foo". (For a real-life example see the production for java
;; keywords.)
;;
;; Error detection: at parser definition time, it's an error for there
;; to be a symbol in the grammar that isn't a production.

;; char types
(parser::defchartype foochar '(member #\f #\o))
(parser::defchartype barchar '(member #\b #\a #\r))

;; productions
(parser::defprod bar () (foo))
(parser::defprod foo () "foo")
(parser::defprod baz () (bar foo))

;; simple parsers
;;(parser::defparser empty-parser)
(parser::defparser char-parser (#\a))
(parser::defparser string-parser ("foo"))
(parser::defparser char-type-parser foochar)
(parser::defparser prod-parser foo)
(parser::defparser two-level-prod-parser bar)
(parser::defparser two-char-parser (#\a #\b))

;; semi-complex parsers
(parser::defparser star-parser     (* #\a))
(parser::defparser one-star-parser (+ #\a))
(parser::defparser optional-parser (? #\a))
(parser::defparser or-parser       (/ #\a #\b))
(parser::defparser and-parser      (& #\a "a"))
(parser::defparser not-parser      (~ #\a))
;;(parser::defparser lisp-escape-parser (!(zerop 0)))
;;(parser::defparser lisp-escape-parser-2 #\a !(zerop 0))
;;(parser::defparser lisp-escape-parser-3 #\a !(zerop 1))
(parser::defparser sequence-parser (#\a #\b #\c))
(parser::defparser tricky-1 ((/ #\a #\b) (& #\c (~ (#\c #\d))) #\= #\d))
(parser::defparser tricky-2 (* (& #\a (~ "ab"))))
(parser::defparser tricky-3 ((* (& #\a (~ "ab"))) (/ "ab" "abc") "c"))
(parser::defparser tricky-4 (& "ab" (! "a")))
(parser::defparser sequence-rollback (/ (#\a #\b #\c) (#\a #\b) (#\a)))

;; arithmetic parser

(progn
  (parser::defchartype digit     '(satisfies digit-char-p))
  (parser::defchartype term-op   '(member #\+ #\-))
  (parser::defchartype factor-op '(member #\/ #\*))
  
  (parser::defprod number ()      (+ digit))
  (parser::defprod expression ()  (term (* term-op term)))
  (parser::defprod term ()        (factor (* factor-op factor)))
  (parser::defprod factor ()      (/ number (#\( expression #\))))
  
  (parser::defparser arithmetic expression))

(deftest local-parser ()
  (parser::parselet ((parser (number)))
    (check
    (parser "1")
    (parser "20"))))

(deftest simple-parsers ()

  (check
   ;;(empty-parser "")
   ;;(not (empty-parser "a"))

   (char-parser "a")
   (not (char-parser ""))
   (not (char-parser "b"))
   (not (char-parser "aa"))

   (string-parser "foo")
   (not (string-parser ""))
   (not (string-parser "afoo"))
   (not (string-parser "fooa"))
   (not (string-parser "fo"))

   (char-type-parser "f")
   (char-type-parser "o")
   (not (char-type-parser ""))
   (not (char-type-parser "ff"))
   (not (char-type-parser "a"))

   (prod-parser "foo")
   (not (prod-parser ""))
   (not (prod-parser "afoo"))
   (not (prod-parser "fooa"))
   (not (prod-parser "fo"))

   (two-level-prod-parser "foo")
   (not (two-level-prod-parser ""))
   (not (two-level-prod-parser "afoo"))
   (not (two-level-prod-parser "fooa"))
   (not (two-level-prod-parser "fo"))

   (two-char-parser "ab")
   (not (two-char-parser ""))
   (not (two-char-parser "abc"))
   (not (two-char-parser "ba"))))

(deftest semi-complex-parsers ()
  (check
   (star-parser "")
   (star-parser "a")
   (star-parser "aa")
   (star-parser "aaa")
   (not (star-parser "b"))
   (not (star-parser "ab"))
   (not (star-parser "ba"))

   (one-star-parser "a")
   (one-star-parser "aa")
   (one-star-parser "aaa")
   (not (one-star-parser ""))
   (not (one-star-parser "b"))
   (not (one-star-parser "ab"))
   (not (one-star-parser "ba"))

   (optional-parser "")
   (optional-parser "a")
   (not (optional-parser "aa"))
   (not (optional-parser "b"))
   (not (optional-parser "ab"))
   (not (optional-parser "ba"))

   (or-parser "a")
   (or-parser "b")
   (not (or-parser ""))
   (not (or-parser "c"))
   (not (or-parser "ab"))
   
   (and-parser "a")
   (not (and-parser ""))
   (not (and-parser "b"))
   (not (and-parser "ab"))

   (not-parser "")
   (not (not-parser "a"))

;;   (lisp-escape-parser "")
;;   (not (lisp-escape-parser "a"))

;;   (lisp-escape-parser-2 "a")
;;   (not (lisp-escape-parser-2 ""))

;;   (not (lisp-escape-parser-3 "a"))
;;   (not (lisp-escape-parser-2 ""))
   
   (sequence-parser "abc")
   (not (sequence-parser ""))
   (not (sequence-parser "ab"))
   (not (sequence-parser "abx"))

   (tricky-1 "ac=d")
   (tricky-1 "bc=d")

   (tricky-2 "")
   (tricky-2 "a")
   (tricky-2 "aa")
   (tricky-2 "aaa")

   (tricky-3 "abc")
   (tricky-3 "aabc")
   (tricky-3 "aaabc")
   (tricky-3 "aaaabc")


   (sequence-rollback "abc")
   (sequence-rollback "ab")
   (sequence-rollback "a")

   ))

(deftest arithmetic-parser ()
  (check
   (arithmetic "1")
   (arithmetic "10")
   (arithmetic "(10)")
   (arithmetic "1+2")
   (arithmetic "1*2")
   (arithmetic "1*2+3")
   (arithmetic "(3+4)*5")
   (arithmetic "(3+4)/5")
   (not (arithmetic ""))
   (not (arithmetic "1x"))))



(deftest longest-match ()
  (check (tricky-4 "ab")))


(parser::defprod aaa () "aaa")
(parser::defprod aaa-up () (^ "aaa" (string-upcase parser::last-match)))
(parser::defprod bbb () "bbb")
(parser::defprod aaa-bbb () (^ (aaa bbb) (list aaa bbb)))
(parser::defprod aaa-or-bbb () (/ aaa bbb))

(parser::defparser p (^ (#\a #\b)))


(deftest accumulation ()
  (macrolet
      ((test (grammar string expected)
         `(parser::parselet ((parser ,grammar))
           (multiple-value-bind (ok value) (parser ,string)
             (check ok (equal value ,expected))))))
    
    (test (^ #\a) "a" "a")
    (test (^ "a") "a" "a")
    (test (^ aaa) "aaa" "aaa")
    (test (^ (? #\a)) "a" "a")
    (test (^ (? #\a)) "" nil)
    (test (^ (/ "aaa" "bbb")) "aaa" "aaa")
    (test (^ (& "aaa" (~ "aaab"))) "aaa" "aaa")
    (test (^ (~ "aaa")) "" nil)
    (test (^ aaa-bbb) "aaabbb" '("aaa" "bbb"))
    (test (^ "aaa" (string-upcase parser::last-match)) "aaa" "AAA")
    (test (^ aaa (string-upcase aaa)) "aaa" "AAA")
    (test (^ aaa-up) "aaa" "AAA")
    (test (^ bbb (string-upcase parser::last-match)) "bbb" "BBB")

    ;;; Tests that depend on old auto-gathering behaviour
    ;;(test (^ (#\a #\b)) "ab" '("a" "b"))
    ;;(test (^ ("aaa" "bbb")) "aaabbb" '("aaa" "bbb"))
    ;;(test (^ (aaa "bbb")) "aaabbb" '("aaa" "bbb"))
    ;;(test (^ (* #\a)) "aaa" '("a" "a" "a"))
    ;;(test (^ (* #\a)) "" nil)
    ;;(test (^ (+ #\a)) "aaa" '("a" "a" "a"))
    ;;(test (^ (* aaa-bbb)) "aaabbbaaabbb" '(("aaa" "bbb") ("aaa" "bbb")))
    ;;(test (^ (* aaa bbb)) "aaabbbaaabbb" '("aaa" "bbb" "aaa""bbb"))
    ;;(test (^ (* (aaa bbb))) "aaabbbaaabbb" '(("aaa" "bbb") ("aaa""bbb")))
    ;;(test (^ (* ((aaa bbb)))) "aaabbbaaabbb" '((("aaa" "bbb")) (("aaa""bbb"))))
    ;;(test (^ (+ aaa-bbb)) "aaabbbaaabbb" '(("aaa" "bbb") ("aaa" "bbb")))
    ;;(test (^ (+ aaa bbb)) "aaabbbaaabbb" '("aaa" "bbb" "aaa""bbb"))
    ;;(test (^ (+ (aaa bbb))) "aaabbbaaabbb" '(("aaa" "bbb") ("aaa""bbb")))
    ;;(test (^ (+ ((aaa bbb)))) "aaabbbaaabbb" '((("aaa" "bbb")) (("aaa""bbb"))))

    ;;(test (^ (* (/ aaa bbb) aaa)) "aaaaaabbbaaabbbaaa" '("aaa" "aaa" "bbb" "aaa" "bbb" "aaa"))

    ))

(deftest token-parser ()
  (macrolet 
      ((test (grammar input expected)
         `(parser::parselet ((parser ,grammar :type parser::token-parser))
           (multiple-value-bind (ok value) (parser ,input)
             (check ok (equal value ,expected))))))

    (let ((t1-tok (parser::make-token :type 't1 :value "boo"))
          (a-tok (parser::make-token :type nil :value #\a))
          (foo-tok (parser::make-token :type 'keyword :value "foo")))

      (test (% t1) (vector t1-tok) nil)
      (test (^ (% t1)) (vector t1-tok) t1-tok)
      (test (^ (% t1) (parser::token-value parser::last-match))
            (vector t1-tok) "boo")
      (test (^ (% t1) (parser::token-value t1))
            (vector t1-tok) "boo")
      
      (test "a" (vector a-tok) nil)
      (test "foo" (vector foo-tok) nil)

      (test ("a" "foo" (% t1)) (vector a-tok foo-tok t1-tok) nil)
      )))


(parser::deflexer lex-1 (aaa bbb "foo")
  ((:tokens aaa bbb "foo")))

(parser::deflexer lex-2 (/ aaa bbb "foo" foochar)
  ((:tokens aaa bbb "foo" foochar)))

(parser::deflexer lex-3 (* (/ aaa bbb "foo"))
  ((:tokens aaa bbb "foo")))

(parser::deflexer lex-4 (* (/ aaa bbb "foo"))
  ((:tokens aaa "foo")))

(parser::deflexer lex-5 (* (/ aaa-or-bbb "foo"))
  ((:tokens aaa "foo")))

(parser::deflexer lex-6 (foochar)
  ((:tokens foochar)))

(deftest lexer-test ()

  (let ((a-tok (parser::make-token :type 'aaa :value "aaa"))
        (b-tok (parser::make-token :type 'bbb :value "bbb"))
        (foo-tok (parser::make-token :type
                                     'parser::string-literal :value "foo"))
        (foochar-tok (parser::make-token :type
                                         'foochar :value #\f)))

    (multiple-value-bind (parsed value) (lex-1 "aaabbbfoo")
      (check
       parsed
       (equalp (vector a-tok b-tok foo-tok) value)))
  
    (multiple-value-bind (parsed value) (lex-2 "aaa")
      (check
       parsed
       (equalp (vector a-tok) value)))

    (multiple-value-bind (parsed value) (lex-2 "bbb")
      (check
       parsed
       (equalp (vector b-tok) value)))

    (multiple-value-bind (parsed value) (lex-2 "foo")
      (check
       parsed
       (equalp (vector foo-tok) value)))

    (multiple-value-bind (parsed value) (lex-3 "aaabbbfoo")
      (check
       parsed
       (equalp (vector a-tok b-tok foo-tok) value)))

    (multiple-value-bind (parsed value) (lex-4 "aaabbbfoo")
      (check
       parsed
       (equalp (vector a-tok foo-tok) value)))

    (multiple-value-bind (parsed value) (lex-5 "aaabbbfoo")
      (check
       parsed
       (equalp (vector a-tok foo-tok) value)))

    (multiple-value-bind (parsed value) (lex-6 "f")
      (check
       parsed
       (equalp (vector foochar-tok) value)))))

   
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities.

(defun tests (&optional reload)
  (if reload
      (utils::load-and-test "parser")
      (test::test-package "PARSER-TEST")))

