;;; -*- Package: utils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :utils)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

;;; Author:  JP Massar.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 
   '(
     help:document-module 
        help:document-function)
   :utils
   ))

(help:def-module "LIST-UTILS"
  (:summary "Functions dealing with lists")
  (:text "These functions work on Lisp lists.")
  (:author "JP Massar")
  (:see-also (module sequence-utils))
  (:keywords :utilities :lists)
  (:display-modes :biolisp)
  #.`(:functions ,@*utility-list-user-symbols*))

(document-function iota 
  (:summary "Creates a list of sequential integers starting at 0 up to N-1.")
  (:returns "Consecutive numbers starting from 0" :type list)
  (:examples
   "(iota 3) -> (0 1 2)"
   "(iota 0) -> NIL"
   "(iota -5) -> NIL"
   "(mapcar 'list '(a b c) (iota 3)) -> ((a 0) (b 1) (c 2))"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Returns a list of consecutive integers starting with 0 and"
       "ending with one less than the argument.  If the argument is"
       "an integer which is not positive, NIL is returned.  If the argument"
       "is anything but an integer, the result is undefined.")))
  (:parameters
   (n :docstring "An integer." :value-type integer))
  (:see-also ilist first-n)
  )

(document-function ilist 
  (:summary "Creates an arithmetic sequence")
  (:returns "Integers in an arithmetic progression" :type list)
  (:examples
   "(ilist 10 13) --> (10 11 12)"
   "(ilist 5 20 3) --> (5 8 11 14 17)"
   "(ilist 5 -5 2) --> (5 3 1 -1 -3)"
   "(ilist 4 0) --> (4 3 2 1)"
   "(ilist 4 4) --> NIL"
   )
  (:text 
   (:p 
    #.(one-string-sp
       "Returns a list of integers, by default (min min+1 ... LIMIT-1)"
       "If LIMIT < MIN (min, min-1 ... LIMIT+1)."
       "If STEP is provided it is the interval between elements instead of 1."
       "The sign of STEP is ignored, its absolute value is used. "
       "If MIN, LIMIT and STEP are not integers the result is undefined.")))
  (:parameters
   (min :docstring "The start of the sequence returned"
        :value-type integer)
   (limit :docstring "The upper/lower exclusive bound of the sequence returned."
          :value-type integer)
   (step :docstring 
         #.(one-string-sp
            "The interval between successive elements of the sequence."
            "If the value is negative, its absolute value is used.")
         :value-type integer
         :parameter-type &optional
         :default-value 1))
  (:see-also nlist iota first-n)
  )

(document-function nlist
  (:summary "Creates an arithmetic sequence")
  (:returns "Numbers in an arithmetic progression" :type list)
  (:examples
   "(nlist 10 13) --> (10 11 12)"
   "(nlist 5 20 3) --> (5 8 11 14 17)"
   "(nlist 5.5 -5.5 2) --> (5.5 3.5 1.5 -0.5 -2.5 -4.5)"
   "(nlist 1/3 8/3) --> (1/3 4/3 7/3)"
   "(nlist 1/3 8/3 1/3) --> (1/3 2/3 1 4/3 5/3 2 7/3)"
   "(nlist 4.0 4.0) --> NIL"
   )
  (:text 
   (:p 
    #.(one-string-sp
       "Returns a list of numbers, by default (min min+1 ... LIMIT-1)"
       "If LIMIT < MIN (min, min-1 ... LIMIT+1)."
       "If STEP is provided it is the interval between elements instead of 1."
       "The sign of STEP is ignored, its absolute value is used. ")))
  (:parameters
   (min :docstring "The start of the sequence returned"
        :value-type real)
   (limit :docstring "The upper/lower exclusive bound of the sequence returned."
          :value-type real)
   (step :docstring 
         #.(one-string-sp
            "The interval between successive elements of the sequence."
            "If the value is negative, its absolute value is used.")
         :value-type real
         :parameter-type &optional
         :default-value 1))
  (:see-also ilist iota first-n)
  )

(document-function first-n 
  (:summary "Creates a new list of the first N elements of the input list.")
  (:returns "The initial N elements of the input" :type list)
  (:examples
   "(first-n 2 '(a b c d)) --> (a b)"
   "(first-n 10 '(a b c)) --> (a b c)"
   "(first-n 0 '(a b c)) --> NIL"
   "(first-n -3 '(a b c)) --> NIL"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Efficient way to extract the first N elements of a list."
       "If N is >= the number of elements in LIST, then a copy"
       "of LIST is returned.  If N is a non-positive integer, NIL is returned."
       "If N is not an integer or LIST is not a list, the result is undefined."
       )))

  (:parameters
   (n :docstring 
      "An integer specifying the number of initial elements to return. " 
      :value-type integer)
   (list :docstring "The list whose initial elements are returned." 
         :value-type list))
  (:see-also cl:subseq cl:nth cl:nthcdr cl:butlast cl:nbutlast)
  )


(document-function exactly-one-of? 
  (:summary "Tests whether exactly one of a number of values is non-nil.")
  (:returns "T or NIL" :type t)
  (:examples
   "(exactly-one-of? t nil t) --> NIL"
   "(exactly-one-of? 5 nil nil nil) --> T"
   "(exactly-one-of?) --> NIL"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Takes as input an arbitrary number of arguments and returns"
       "T if exactly one of those arguments is non-nil, otherwise"
       "it returns NIL (if no arguments are provided, NIL is returned).")))
  (:parameters
   (args :docstring "Any number of arbitrary objects" 
         :parameter-type &rest
         :value-type t))
  (:see-also cl:some cl:every cl:notany cl:notevery cl:and cl:or)
  )


(document-function insert-into-ordered-list 
  (:summary 
   #.(one-string-sp
      "Destructively modifies its LIST argument by inserting ELEM"
      "between two of its elements; LIST should be ordered by TEST."))
  (:returns 
   #.(one-string-sp 
      "The LIST argument, with a new element inserted, or the CONS of"
      "the new element onto the LIST argument (when the new element's proper"
      "location in the ordering is before the existing first element)")
   :type list :display-type nil)
  (:examples
   "(insert-into-ordered-list 3 '< (list 1 2 6)) --> (1 2 3 6)"
   "(insert-into-ordered-list 3 '> (list 7 5)) --> (7 5 3)"
   "(insert-into-ordered-list \"aa\" 'string< (list \"ab\" \"zq\")) ->
       (\"aa\" \"ab\" \"zq\")"
   "(insert-into-ordered-list 3 '> nil) --> (3)"
   "(insert-into-ordered-list '(3 4) '< '((2 1) (0 8))
         :key (lambda (x) (reduce '+ x))) --> ((2 1) (3 4) (0 8))"
   )
  (:text
   (:p 
    #.(one-string-sp
       "ELEM is placed immediately before the first element, A, of LIST,"
       "such that calling TEST on ELEM and A returns NIL. If no element"
       "of LIST satisfies this condition ELEM is appended to the end"
       "of LIST. LIST is destructively modified unless ELEM is placed"
       "before the first element (or LIST is actually NIL)."))
   (:p 
    #.(one-string-sp
       "If LIST is not already ordered with respect to TEST the results"
       "are undefined."))
   (:p
    #.(one-string-sp
       "Note: This function uses an O(n) algorithm; it should not"
       "necessarily be used to repetitively insert into a very long list."
       "Consider using CONS/APPEND and SORT to insert multiple elements.")))
  (:parameters
   (elem :docstring "The object to be inserted into LIST." :value-type t)
   (test :docstring "A function of two arguments, returning T or NIL"
         :value-type function)
   (list :docstring "The list being inserted into." :value-type list)
   (key :docstring 
        #.(one-string-sp
           "A function applied to ELEM and the elements of LIST which returns"
           "the actual values which TEST is called with. Since the default"
           "value of KEY is the identity function by default ELEM and the"
           "elements of LIST are used in the calls made to TEST.")
        :value-type function :parameter-type &key :default-value identity))
  (:see-also cl:sort binsearch cl:merge cl:cons cl:append cl:nconc)
  )

(document-function ensure-list 
  (:summary "Returns its argument as a list." )
  (:returns 
   #.(one-string-sp 
      "The object itself if it is a list, or a list whose sole element"
      "is the object") :type list :display-type nil)
  (:examples
   "(ensure-list 5) --> (5)"
   "(ensure-list '(1 2)) --> (1 2)"
   "(ensure-list nil) --> NIL"
   "(ensure-list #(1 2 3)) --> (#(1 2 3))"
   )
  (:text
   (:p 
    #.(one-string-sp
       "If the argument thing is a list already, it is simply returned"
       "otherwise (list thing) is returned.  This is often used when a"
       "function argument can either be an atom or a singleton list and you"
       "want to ensure that it's a list (i.e, canonicalize the object as a"
       "list) before proceeding further.")))
  (:parameters
   (thing :docstring "The object to be listified" 
          :value-type t))
  (:see-also cl:coerce cl:list cl:cons cl:atom)
  )

(document-function flatten
  (:summary "Removes all nested structure from a list.")
  (:returns "The non-nil atoms in the input" :type list)
  (:examples
   "(flatten '(((a) (((b)))))) => (a b)"
   "(flatten '(nil 3)) --> (3)"
   "(flatten '((a b) (c nil))) --> (a b c)"
   "(flatten '((a . b) . c) --> (a b c)"
   "(flatten 3.41) -> (3.41)"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Extracts all the non-nil atoms from its input and returns"
       "them as a list.  The ordering of the returned list is as if"
       "a depth-first descent were done into the input list structure."
       "If the input X is not a cons or NIL, (LIST X) is returned."
       "If the argument thing is a list already, it is simply returned."
       "Flatten does not destructively modify its input list."
       )))
  (:parameters (obj :docstring "The object to be flattened." :value-type t))
  (:see-also cl:copy-tree cl:tree-equal one-string cl:nconc cl:append)
  )

(document-function delete-last-element
  (:summary "Deletes last element of a list")
  (:returns 
   "The destructively modified list, or NIL" :type list :display-type nil)
  (:examples
   "(lastelem '(8 3 2)) --> 2"
   "(lastelem \"xyzzy\") --> #\\y"
   "(lastelem #(foo bar baz)) --> baz"
   "(lastelem nil) --> nil"
   "(lastelem '(nil) --> nil"
   "(lastelem #()) --> nil"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Returns the last element of a sequence."
       "If the sequence is zero-length (either (), NIL, "", or #() )"
       "then NIL is returned."
       "There is no way to distinguish between NIL being returned because"
       "the sequence is empty and NIL being returned because NIL is in fact"
       "the last element."
       "If the input is not a sequence an error is signalled."
       )))
  (:parameters 
   (seq :docstring "The sequence from which the last element is taken"
        :value-type sequence))
  (:see-also cl:last cl:butlast cl:nbutlast cl:aref cl:length)
  )

(document-function mapcarnn
  (:summary "Maps F over ARGS returning only non-nil results as a list")
  (:returns "The non-nil results of the mapping" :type list)
  (:examples
   "(mapcarnn 'evenp '(1 2 3 4)) --> (t t)"
   "(mapcarnn 'identity '(a b nil c (nil))) --> (a b c (nil))"
   "(mapcarnn (lambda (x y) (and (plusp (+ x y)) (list x y))) '(-1 1) '(0 2))
       --> ((1 2))"
   "(mapcarnn 'oddp '(2 4 6)) --> nil"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Like MAPCAR, except that any null results are filtered out."
       "Equivalent to (remove-if-not 'identity (mapcar ...)) but less"
       "verbose and more efficient."
       )))
  (:parameters 
   (f :docstring "The function which is mapped over the list arguments."
        :value-type function-designator)
   (args :parameter-type &rest :value-type list
         :docstring "The list arguments which are to be mapped over."))
  (:see-also 
   cl:mapcar cl:mapcan cl:map cl:loop cl:remove-if cl:remove-if-not
  ))

(document-function all-unordered-pairs
  (:summary "Computes all pairs between the elements of the input list.")
  (:returns "The element pairs, each a 2-element list," :type list)
  (:examples
   "(all-unordered-pairs '(a b c)) --> ((a b) (a c) (b c))"
   "(all-unordered-pairs '(a)) --> nil"
   "(all-unordered-pairs '(a (b c) d)) --> ((a (b c)) (a d) ((b c) d))"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Computes a list of all pairs of elements in SET, such that if A and B"
       "are elements of SET and A occurs before B, then the pair (A B) is"
       "included in the returned list while (B A) is not."
       "If SET is NIL or contains a single element, NIL is returned."
       "If SET is not a list the result is undefined."
       "If SET does not contain all unique elements (the assumption)"
       "the result may contain duplicates (some with the order of the"
       "elements reversed).")
    ))
  (:parameters 
   (set :value-type list
        :docstring "The list of elements from which the pairs are computed."))
  (:see-also 
   all-ordered-pairs cl:union cl:intersection cl:set-difference
   all-contiguous-sublists all-strings-of-length
   ))

(document-function all-ordered-pairs
  (:summary 
   #.(one-string-sp
      "Computes all pairs between the elements of the input list,"
      "such that is A and B are elements, both (A B) and (B A) are returned."))
  (:returns "The element pairs, each a 2-element list," :type list)
  (:examples
   "(all-ordered-pairs '(a b c)) --> ((a b) (a c) (b c) (b a) (c a) (c b))"
   "(all-ordered-pairs '(a)) --> nil"
   "(all-ordered-pairs '(a d)) --> ((a d) (d a))"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Computes a list of all pairs of elements in SET, such that if A and B"
       "are elements of SET then both (A B) and (B A) are included in the"
       "returned list.  If X is an element of the input, (X X) is NOT"
       "included in the returned list."))
   (:p
    #.(one-string-sp
       "In effect, the crossproduct of the input list is computed except"
       "for the main diagonal."))
   (:p
    #.(one-string-sp
       "If SET does not contain all unique elements (the implicit assumption)"
       "the result may contain duplicates and pairs with identical elements."))
    )
  (:parameters 
   (set :value-type list
        :docstring "The list of elements from which the pairs are computed."))
  (:see-also 
   all-unordered-pairs cl:union cl:intersection cl:set-difference
   all-contiguous-sublists all-strings-of-length
   ))


(document-function separate-into-lists
  (:summary "Divides a list into mutually exclusive sublists")
  (:returns "Multiple values, each value being a sublist or the input."
   :type t)
  (:examples
   "(separate-into-lists (iota 6) 'evenp) --> (0 2 4) (1 3 5)"
   "(separate-into-lists '(1 0 -1) 'minusp 'plusp) --> (-1) (1) (0)"
   "(separate-into-lists (iota 6) 'plusp 'oddp) --> (1 2 3 4 5) NIL (0)"
   #.(one-string-sp
      "(separate-into-lists" 
      "'(a ab bc) (lambda (x) (< (length (string x)) 3))) -->"
      "(a ab bc) NIL")
   "(separate-into-lists nil 'evenp 'oddp 'plusp) --> NIL NIL NIL NIL"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Divides a list into mutually exclusive sublists. The number"
       "of such sublists, N, is one more than the number of predicates"
       "provided.  N values are returned."))
   (:p
    #.(one-string-sp
       "The first value returned is a sublist of the input consisting"
       "of those elements of the input that satisfy the first predicate."
       "The second returned is that sublist of the input consisting"
       "of those elements that satisfy the second predicate,"
       "but not the first (or the remaining elements, if there is no"
       "second predicate).  And so forth through all N-1 predicates."
       "If no predicates are provided a copy of the input is returned."
       ))
   (:p
    #.(one-string-sp
       "The ordering of the elements from the input is preserved within"
       "each returned sublist.")))
  (:parameters 
   (list 
    :docstring "The list whose elements the predicates will be applied to"
    :value-type list)
   (predicates
    :docstring 
    #.(one-string-sp
       "A set of boolean functions, applied to the elements of the input"
       "list in order.")))
  (:see-also 
   cl:remove-if cl:remove-if-not cl:mapcan cl:sort cl:stable-sort string-split)
  )


(document-function length-circular-or-dotted?
  (:summary "Determines the properties of its input list.")
  (:returns 
   #.(one-string-sp
      "Two values.  The first is either the length of the list or NIL."
      "The second is one of :proper, :circular or :dotted.")
   :type (values t keyword) :display-type nil)
  (:examples
   "(length-circular-or-dotted? '(1 2 3)) --> 3 :PROPER"
   "(length-circular-or-dotted? '(1 2 . 3)) --> 3 :DOTTED"
   "(length-circular-or-dotted? nil) --> 0 :proper)"
   #.(one-string-sp
      "(length-circular-or-dotted? (let ((x (list 1 2 3))) (setf (cddr x) x)))"
      " --> nil :CIRCULAR"))
  (:text
   (:p 
    #.(one-string-sp
       "Determines whether a list is a proper list, ends with a dotted pair,"
       "or is circular (points back at some part of itself)."
       "Two values are returned."
       "The first is NIL if the list is circular, otherwise the length of"
       "the list (counting the last cons cell of a list ending in a dotted"
       "pair as two elements)."
       "The second value is one of :proper, :circular or :dotted."
       "If the input is not a list the result is undefined."))
   (:p 
    #.(one-string-sp
       "This function is useful when one has to deal with data coming from"
       "an unknown source without erroring out, such as displaying arbitrary"
       "list structure in the frame browser."))
   (:p 
    #.(one-string-sp
       "Note: This function might be more concisely named 'list-properties'.")))
  (:parameters 
   (list :docstring "The input list whose properties are to be determined."
        :value-type list))
  (:see-also cl:list-length cl:*print-circle* cl:cons)
  )


(document-function all-contiguous-sublists
  (:summary 
   "Creates Contiguous sublists of a specified length from the input list")
  (:returns 
   #.(one-string-sp "A set of sublists of the input list") :type list)
  (:examples
   "(all-contiguous-sublists '(1 2 3 4) 2) --> ((1 2) (2 3) (3 4))"
   "(all-contiguous-sublists '(1 2 3 4 5) 3) --> ((1 2 3) (2 3 4) (3 4 5))"
   "(all-contiguous-sublists '(1 2 3 4) 1 2) --> ((1) (3))"
   "(all-contiguous-sublists '(1 2 3 4 5 6) 2 3) --> ((1 2) (4 5))"
   "(all-contiguous-sublists '(1 2 3) 4) --> NIL")
  (:text
   (:p 
    #.(one-string-sp
       "Each sublist created is a subsequence of LIST"
       "consisting of SUBLIST-SIZE contiguous elements of LIST."
       "INTERVAL specifies how many elements in LIST to skip before creating"
       "the next sublist. NIL is returned if SUBLIST-SIZE is larger than"
       "the length of LIST. The sublists are returned in the order they"
       "appear in the original list.")))
  (:parameters 
   (list 
    :docstring "The input list from which contiguous subsequence is created"
    :value-type list)
   (sublist-size
    :docstring 
    "A positive integer. The size of each returned contiguous sublist."
    :value-type (integer 1 *))
   (interval
    :docstring 
    #.(one-string-sp
       "A positive integer."
       "How many elements to skip from the last element of the current"
       "subsequence to the beginning of the next one. An INTERVAL of 1"
       "means that no elements are skipped.")
    :value-type (integer 1 *)
    :parameter-type &optional
    ))
  (:see-also all-unordered-pairs all-ordered-pairs cl:subseq)
  )

(document-function maptree
  (:summary 
   "Maps a function over the leaves of one or more identically-structured lists"
   )
  (:returns 
   #.(one-string-sp 
      "A list of the same structure as the input lists,"
      "each leaf being the result of applying the input function to the"
      "leaves of each input list.")
   :type list :display-type nil)
  (:examples
   "(maptree '+ '(1 2) '(3 4)) --> (4 6) ;; here equivalent to mapcar"
   "(maptree '+ '((1) (2 (3 4))) '((2) (3 (4 5)))) --> ((3) (5 (7 9)))"
   "(maptree 'list '((1) (2) (3) (4))) --> (((1) ((2)) ((3)) ((4))))"
   "(maptree '+ '(1 2) '(1 2 3)) --> error")
  (:text
   (:p 
    #.(one-string-sp
       "Each input list argument is descended in depth-first fashion"
       "until a leaf (an atom) from each list is encountered (if one or more"
       "input lists does not have a leaf where others do, an error is"
       "signalled).  The input function is then called with each of the leaves"
       "so found as an argument.  The result is a list of the same structure"
       "as the input lists, with the function results at each leaf position."))
   (:p 
    #.(one-string-sp
       "Note: Unlike MAPCAR and friends, the iteration does not terminate"
       "if one list termiantes and the others do not; an error is signalled."
       "All the lists must be identical in structure.")))
  (:parameters 
   (f 
    :docstring
    #.(one-string-sp
     "The input function.  It must accept as many arguments as there are"
     "input lists.")
    :value-type function-designator)
   (arg
    :docstring 
    "The first input list (at least one input list must be provided.)"
    :value-type list)
   (args
    :docstring 
    #.(one-string-sp
       "The remaining input lists.  There must be as many input lists as"
       "allowed arguments to the input function.")
    :value-type list
    :parameter-type &rest
    ))
  (:see-also subst mapcar mapcan maplist map mapl mapcon
   copy-tree tree-equal equivalent-tree-structure?)
  )

(document-function equivalent-tree-structure?
  (:summary 
   "Decides whether two or more trees (lists) have the same list structure.")
  (:returns "T or NIL" :type boolean)
  (:examples
   "(equivalent-tree-structure? '(a b (c d)) '(d e (f g))) --> T"
   #.(one-string-sp
      "(equivalent-tree-structure? '((a1 a2) b (c . d)) '((b2) e (f . g)))"
      " --> NIL")
   "(equivalent-tree-structure? '(a) nil) --> nil"
   "(equivalent-tree-structure? '(a b)) --> T")
  (:text
   (:p 
    #.(one-string-sp
       "Determines if all the input lists (trees) have identical"
       "list structure.  That is, where one tree has a leaf, the other"
       "trees also have leaves.  (The car of a (sub)tree is a leaf if"
       "it contains an atom, and the cdr of a (sub)tree is a leaf"
       "if it contains a non-NIL atom.)  If there are no input lists or"
       "a single input list, T is always returned.")))
  (:parameters 
   (trees
    :docstring
    "The input trees to be tested."
    :parameter-type &rest
    :value-type list))
  (:see-also tree-equal equal equalp)
  )

(document-function remove-and-push
  (:summary 
   #.(one-string-sp  
      "Add an element to the beginning of a list, ensuring that said element"
      "does not appear anywhere else in the list."))
  (:returns 
   "A copy of LIST, with ITEM prepended"
   :type list :display-type nil)
  (:examples
   "(remove-and-push 3 '(1 2 3)) --> (3 1 2)"
   "(remove-and-push 4 '(1 2 3)) --> (4 1 2 3)"
   "(remove-and-push 5 nil) --> (5)"
   "(remove-and-push 6 '(6 7 8)) --> (6 7 8)"
   "(remove-and-push 1.0 '(1 1.0 1.0d0 2) :test '=) --> (1.0 2)")
  (:syntax (remove-and-push item list &key test))
  (:text 
   (:p
    #.(one-string-sp
       "Add an element to the beginning of a list, ensuring that said element"
       "does not appear anywhere else in the list.  This can be thought of as"
       "bringing an element to the front of a list (if it is already present)."
       "The value of the TEST keyword is passed to LISP:REMOVE, which is"
       "used to remove ITEM from LIST.")))
  (:parameters 
   (item
    :docstring 
    "An arbitrary object to be removed from and then pushed onto LIST."
    :value-type t)
   (list
    :docstring
    #.(one-string-sp
       "First, ITEM is removed from LIST (which may cause it to be copied"
       "if ITEM is in fact present in LIST)."
       "Then ITEM is pushed onto the (possibly) copied LIST and the augmented list is"
       "returned.")
    :value-type list)
   (test 
    :docstring
    "A standard Lisp TEST argument which is passed to LISP:REMOVE."
    :value-type function-designator
    :parameter-type &key
    :default-value eql))
  (:see-also delete-and-push lisp:remove lisp:delete lisp:push lisp:pushnew 
   )
  )

(document-function delete-and-push
  (:summary 
   #.(one-string-sp  
      "Add an element to the beginning of a list, ensuring that said element"
      "does not appear anywhere else in the list.  (A macro)."))
  (:returns 
   "PLACE, destructively modified with ITEM prepended"
   :type list)
  (:examples
   "(let ((x (list 1 2 3))) (delete-and-push 3 x)) --> (3 1 2) ;; x became (3 1 2)"
   "(let ((x (list 1 2 3))) (delete-and-push 4 x)) --> (4 1 2 3) ;; x became (4 1 2 3)"
   "(let ((x nil)) (delete-and-push 5 x)) --> (5) ;; x became (5)"
   #.(one-string-sp
      "(let ((x (list (list 1 2) 3))) (delete-and-push 2 (first x)))"
      "--> (2 1) ;; x became ((2 1) 3)")
   #.(one-string-sp
      "(let ((x (vector 3 (list 1 2)))) (delete-and-push 5 (aref x 1)))"
      "--> (5 1 2) ;; x became #(3 (5 1 2))"))
  (:syntax (delete-and-push item list &key test))
  (:text 
   (:p
    #.(one-string-sp
       "Add an element to the beginning of a list, ensuring that said element"
       "does not appear anywhere else in the list.  This can be thought of as"
       "bringing an element to the front of a list (if it is already present)."
       "The value of the TEST keyword is passed to LISP:DELETE, which is"
       "used to delete ITEM from PLACE."))
   (:p
    #.(one-string-sp
       "PLACE must be a form that is valid with respect to SETF."
       "The value in PLACE is destructively modified by this operation."))
   (:p
    #.(one-string-sp
       "Unlike SETF, which takes care to evaluate the subforms of its place" 
       "argument exactly once, DELETE-AND-PUSH causes the subforms of PLACE"
       "to be evaluated twice;"
       "once when the deletion is performed, and once when the PUSH is done.")))
  (:parameters 
   (item
    :docstring 
    "An arbitrary object to be deleted from and then pushed onto PLACE."
    :value-type t)
   (place
    :docstring
    #.(one-string-sp
       "First, ITEM is deleted from the list stored in PLACE."
       "Then ITEM is pushed onto PLACE and the augmented list is returned.")
    :value-type list)
   (test 
    :docstring
    "A standard Lisp TEST argument which is passed to LISP:DELETE."
    :value-type function-designator
    :parameter-type &key
    :default-value eql))
  (:see-also remove-and-push lisp:remove lisp:delete lisp:push lisp:pushnew 
   )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(document-module sequence-utils
  "Functions dealing with sequences"
  (:keywords :utilities :sequences)
  (:display-modes :biolisp)
  #.`(:functions ,@*utility-sequence-user-symbols*))


(document-function lastelem
  (:summary "Procures the last element of a sequence (a list or a vector)")
  (:returns "The last element, or NIL if the sequence is empty." 
   :type t)
  (:examples
   "(lastelem '(8 3 2)) --> 2"
   "(lastelem \"xyzzy\") --> #\\y"
   "(lastelem #(foo bar baz)) --> baz"
   "(lastelem nil) --> nil"
   "(lastelem '(nil) --> nil"
   "(lastelem #()) --> nil"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Returns the last element of a sequence."
       "If the sequence is zero-length (either (), NIL, \"\", or #() )"
       "then NIL is returned."
       "There is no way to distinguish between NIL being returned because"
       "the sequence is empty and NIL being returned because NIL is in fact"
       "the last element."
       "If the input is not a sequence an error is signalled."
       )))
  (:parameters 
   (seq :docstring "The sequence from which the last element is taken"
        :value-type sequence))
  (:see-also cl:last cl:butlast cl:nbutlast cl:aref cl:length)
  )

(document-function remove-all-of
  (:summary 
   "Removes every occurrence of a set of objects from a sequence")
  (:returns 
   "A new sequence, a copy of the input sequence with certain items removed."
   :type sequence :display-type nil)
  (:examples
   "(remove-all-of '(1 2 3 4 3 2 2) '(2 4)) --> '(1 3 3)"
   "(remove-all-of \"xyzzyXYZZY\" \"xq\" :test 'char-equal) -> \"yzzyYZZY\""
   "(remove-all-of '((1 2) (1 3) (2 3)) '(1) :key 'first) --> ((2 3))"
   "(remove-all-of #() #(1 2 3)) --> #()"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Constructs a new sequence, removing elements from the input"
       "SEQUENCE if they are elements of ELEMENTS (the elements of"
       "SEQUENCE are first processed via the KEY before being compared"
       "to the elements of ELEMENTS using TEST)."
       "If either argument is not a sequence an error is signalled."
       ))
   (:p 
    #.(one-string-sp
       "Note: If the length of ELEMENTS is too large, the algorithm this"
       "function uses may not be efficient, as it will take time proportional"
       "to the product of the length of ELEMENTS and SEQUENCE. In this"
       "case it might be wise to implement an algorithm storing the "
       "elements of ELEMENTS in a hash table, which would then take time"
       "proportional to the length of SEQUENCE.")))
  (:parameters 
   (sequence :docstring "The sequence from which elements are removed."
             :value-type sequence)
   (elements
    :docstring 
    "A set of elements, all of which are to be removed from the input sequence"
    :value-type sequence)
   (test
    :docstring 
    #.(one-string-sp    
       "A standard Lisp TEST argument, used to determine whether an element"
       "of SEQUENCE is equivalent to an element of ELEMENTS and should"
       "therefore be removed.")
    :value-type function
    :default-value eql)
   (key
    :docstring
    #.(one-string-sp  
       "A standard Lisp KEY argument, applied to each element of SEQUENCE"
       "before it is compared to the elements of ELEMENTS.  KEY is *** NOT ***"
       "applied to the elements of ELEMENTS, only to those of SEQUENCE.")
    :value-type function-designator
    :parameter-type &key
    :default-value identity))
  (:see-also 
   cl:remove cl:remove-if cl:delete cl:intersection cl:set-difference
   cl:mapcan purge-duplicates
   ))


(document-function positions
  (:summary "Determines every position an object occurs in a sequence")
  (:returns "The positions of a given object in a sequence" 
   :type (list-of integers))
  (:examples
   "(positions 3 '(1 2 3 4 5 4 3 21)) --> (2 6)"
   "(positions #\c \"This is a string with no 'z', or is it?\") --> nil"
   "(positions 0 #((1 1) (2 0) (3 0) (4 1)) :key 'second :start 1) --> (1 2)"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Returns a list of all positions in SEQUENCE where ITEM was detected"
       "(subject to the usual POSITION argument semantics and restrictions"
       "for TEST, KEY, START and END.  (The POSITION arguments TEST-NOT and"
       "FROM-END are not allowed).")
       ))
  (:parameters 
   (item :docstring "The item to be located in the input sequence"
         :value-type t)
   (sequence :docstring "The sequence in which the item is to be search for"
             :value-type sequence)
   (test :docstring "A standard Lisp sequence function TEST argument"
         :value-type function-designator :parameter-type &key
         :default-value eql)
   (key :docstring "A standard Lisp sequence function KEY argument"
        :value-type function-designator :parameter-type &key
        :default-value identity)
   (start :docstring "A standard Lisp sequence function START argument"
          :value-type integer :parameter-type &key :default-value 0)
   (end :docstring "A standard Lisp sequence function END argument"
          :value-type (or integer nil) :parameter-type &key
          :default-value nil))
  (:see-also cl:position cl:find positions-if binsearch ppcre:all-matches)
  )


(document-function positions-if
  (:summary 
   #.(one-string-sp
      "Determines the position of every element in a sequence satisfying"
      "a condition")
  (:returns "The positions of those elements satisfying the condition")
   :type (list-of integers))
  (:examples
   "(positions-if 'evenp '(1 2 3 4 5 4 3 21)) --> (1 3 5)"
   "(positions-if 'upper-case-p \"This is a string\") --> (0)"
   "(positions-if 'zerop #((1 1) (2 0) (3 0) (4 1)) :key 'second :start 1)"
   "--> (1 2)"
   "(positions-if (lambda (x) (eq x \"a\")) '(\"b\" \"a\")) --> NIL"
   )
  (:text
   (:p
    #.(one-string-sp
       "Returns a list of all positions of all the elements in SEQUENCE"
       "which satisfy PREDICATE (subject to the usual POSITION-IF argument"
       "semantics and restrictions for KEY, START and END.  (The POSITION-IF"
       "arguments TEST-NOT and FROM-END are not allowed).")
       ))
  (:parameters 
   (predicate 
    :docstring 
    #.(one-string-sp
       "A function of one argument returning a boolean, applied to each"
       "element of SEQUENCE.")
    :value-type function-designator)
    (sequence :docstring "The sequence whose elements are tested."
             :value-type sequence)
   (key :docstring "A standard Lisp sequence function KEY argument"
        :value-type function-designator :parameter-type &key
        :default-value identity)
   (start :docstring "A standard Lisp sequence function START argument"
          :value-type integer :parameter-type &key :default-value 0)
   (end :docstring "A standard Lisp sequence function END argument"
          :value-type (or integer nil) :parameter-type &key
          :default-value nil))
  (:see-also 
   cl:position-if cl:find-if position cl:mapcan remove-all-of cl:search)
  )


(document-function c+
  (:summary "Concatenates sequences together")
  (:returns 
   "all the elements of the input sequences in order"
   :type sequence)
  (:examples
   "(c+ '(1 2) '(3 4) #(5 6)) --> (1 2 3 4 5 6)"
   "(c+ \"abc\" #(1 2 3)) --> #(#\a #\b #\c 1 2 3)"
   "(c+ nil nil #(a b c)) --> (a b c)"
   )
  (:text
   (:p
    #.(one-string-sp
       "Concatenates all the SEQUENCES together into a single sequence."
       "If there are no sequences, NIL is returned."
       "If all the sequences are strings, a SIMPLE-STRING is returned."
       "If all the sequences are vectors, a SIMPLE-VECTOR is returned."
       "If any sequence is a list (including NIL), a list is returned."
       "If any argument is not a sequence the result is undefined."
       ))
   (:p
    #.(one-string-sp
       "Note: This is similar to the Lisp function CONCATENATE, except"
       "that the type of sequence returned is determined automatically"
       "and it is much less verbose."))
   (:p
    #.(one-string-sp
       "Note: It is more efficient to use ONE-STRING to concatenate"
       "known strings together.")))
  (:parameters 
   (sequences 
    :docstring "The zero or more sequences to be concatenated."
    :value-type sequence :parameter-type &rest))
  (:see-also 
   cl:concatenate cl:append cl:nconc s+ one-string cl:cons string-join)
  )

(document-function initial-subsequence-of?
  (:summary "Tests whether one sequence occurs at the beginning of another.")
  (:returns "T if the sequences initially match, or NIL otherwise." 
   :type boolean)
  (:examples
   "(initial-subsequence-of? '(1 2 3 4) '(1 2)) --> T"
   "(initial-subsequence-of? \"abcdefg\" \"cde\") --> NIL"
   "(initial-subsequence-of? #(1 2 3 4) nil) --> T"
   "(initial-subsequence-of? #(2 3 4) #(2 3 4 5)) --> NIL"
   #.(one-string-sp
      "(initial-subsequence-of? \"abcde\" \"AbCd\" :element-test 'char-equal)"
      "--> T"))
  (:text
   (:p
    #.(one-string-sp
       "Determines whether INITIAL-SEQUENCE is equivalent to the first"
       "elements of SEQUENCE, as determined by ELEMENT-TEST."
       "Note: This function is more efficient than using SEARCH, since"
       "only the first few positions of SEQUENCE are looked at for a match."
       "Note: This function is particularly efficient if both sequences"
       "are either simple strings or both are lists.")))
  (:parameters 
   (sequence
    :docstring "The sequence which is searched for the initial match."
    :value-type sequence)
   (initial-sequence
    :docstring "The sequence which is used to do the initial matching."
    :value-type sequence)
   (element-test
    :docstring 
    #.(one-string-sp
       "A predicate; A function designator of two arguments which must return"
       "NIL if two elements, one from each sequence, are not equivalent.")))
  (:see-also 
   cl:search cl:subseq cl:find cl:position
   ))

(document-function ordered?
  (:summary "Determines if the elements of a sequence are ordered.")
  (:returns "T or NIL" :type boolean)
  (:examples
   "(ordered? '(1 2 3)) --> T"
   "(ordered? '(1 2 1)) --> NIL"
   "(ordered? '(1 2 2)) --> NIL"
   "(ordered? '(1 2 2) :predicate '<=) --> T"
   "(ordered? #(1.0 2.0 20.0) :seq-type 'float) --> T"
   "(ordered? #()) --> T)")
  (:text
   (:p
    #.(one-string-sp
       "Determines if the elements of SEQ are ordered with respect"
       "to PREDICATE (which defaults to '<).  The function is specifically"
       "optimized for the case where the elements of SEQ are fixnums and"
       "the predicate is '< or '>."
       ))
   (:p
    #.(one-string-sp
       "If the elements of SEQ are not all fixnums then SEQ-TYPE must be"
       "provided and not be :fixnum.  The results are undefined if any"
       "elements are not fixnums and SEQ-TYPE is :fixnum, the default."))
   (:p
    #.(one-string-sp
       "PREDICATE must be a function which accepts two arguments and"
       "returns NIL or non-NIL.")))       
  (:parameters 
   (seq
    :docstring "The sequence which is tested."
    :value-type (or list vector))
   (predicate
    :docstring "The function used to compare elements."
    :value-type (or symbol function)
    :parameter-type &key 
    :default-value '<)
   (seq-type
    :docstring "Specifies the type of elements in SEQ."
    :value-type symbol
    :parameter-type &key 
    :default-value :fixnum))
  (:see-also 
   cl:sort cl:stable-sort cl:merge binsearch cl:reduce))

(document-function select-sequence-elements
  (:summary "Selects elements from a sequence using a Boolean vector.")
  (:returns 
   "A sequence of similar type to the input sequence." 
   :type sequence :display-type nil)
  (:examples
   ((select-sequence-elements '(1 2 3 4) '(t t nil t)) (1 2 4))
   ((select-sequence-elements '(1 2 3 4) #(t nil nil nil)) (1))
   ((select-sequence-elements #(1 2 3 4) '(nil nil)) #())
   ((select-sequence-elements "abcd" '(x nil y nil))
    (:string "ac") "(non-nil is treated as T)")
   ((select-sequence-elements "abcd" nil) (:string ""))
   )
  (:text
   (:p
    #.(one-string-sp
       "Select the elements of SEQUENCE whose corresponding elements"
       "(by position) in BOOLEAN-SEQUENCE are non-NIL."
       "A list, string or vector is returned depending on SEQUENCE's type."
       "A null list, string or vector is returned if no elements are selected."
       "A new sequence is always returned; the operation is not destructive."
       "If the Boolean sequence is shorter than the input sequence it is as if"
       "the boolean sequence was extended with NIL as necessary.  If the"
       "input sequence is shorter than the boolean sequence the remaining"
       "elements of the boolean sequence are ignored."
       ))
   )       
  (:parameters 
   (sequence
    :docstring "The sequence from which elements will be selected."
    :value-type sequence)
   (boolean-sequence
    :docstring 
    #.(one-string-sp
       "A sequence which determines which corresponding elements of"
       "SEQUENCE will be selected.")
    :value-type sequence
    ))
  (:see-also cl:remove cl:remove-if cl:remove-if-not cl:delete cl:mapcan)
  )

(document-function frequency-count 
  (:summary "Determines how many of each element are in a set of sequences.")
  (:returns "A list of counts, or an association list of elements and counts"
   :type list :display-type nil)
  (:examples
   ((frequency-count "acgt" "actgcggtacggtagat") 
    ((#\g 6) (#\a 4) (#\t 4) (#\c 3)))
   ((frequency-count "acgt" "acgttguuutaclsllxt")
    ((#\t 4) (#\a 2) (#\c 2) (#\g 2)))
   ((frequency-count "acgt" "acgttguuutaclsllxt" :if-not-in-alphabet :error)
    :nil "Error condition")
   ((frequency-count t "abcdefghij") 
    ((#\a 1) (#\b 1) (#\c 1) (#\d 1) (#\e 1) (#\f 1) (#\g 1)))
   ((frequency-count t '("abcd" "efgh" "abc" "lll")) 
    ((#\l 3) (#\a 2) (#\b 2) (#\c 2) (#\h 1) (#\d 1) (#\e 1) (#\f 1) (#\g 1)))
   ((frequency-count "acgt" "actgcggtacggtagat" 
                     :return-as :element-position-list)
    (4 3 6 4))
   ((frequency-count '(1 2) '(1 2 3 2 1)) :nil 
    "Error condition (Must enclose a single list inside a list, see next).")
   ((frequency-count '(1 2) '((1 2 3 2 1))) ((1 2) (2 2)))
   ((frequency-count "acgt" "ACTGCGGTACGGTAGAT") 
    ((#\a 0) (#\c 0) (#\g 0) (#\t 0)))
   ((frequency-count "acgt" "ACTGCGGTACGGTAGAT" :test 'char-equal)
    ((#\g 6) (#\a 4) (#\t 4) (#\c 3)))
   ((frequency-count t '(((a 2) (a 3) (b 2) (c 1) (c 25))) :key 'first)
    ((C 2) (A 2) (B 1)))
   ((frequency-count 
     "abcdefghijklmnopqrstuvwxyz"
     "Replace this text will all English prose to get true letter distribution"
     :test 'char-equal)
    ((#\t 10) (#\e 9) (#\l 7) (#\i 6) (#\r 5) (#\s 4) (#\o 3) (#\a 2) (#\g 2)
     (#\h 2) (#\n 2) (#\p 2) (#\u 2) (#\b 1) (#\c 1) (#\d 1) (#\w 1) (#\x 1)
     (#\f 0) (#\j 0) (#\k 0) (#\m 0) (#\q 0) (#\v 0) (#\y 0) (#\z 0))
   ))
  (:text
   (:p
    #.(one-string-sp
       "Counts the number of times elements appear in a set of sequences."
       "If the ELEMENTS-TO-COUNT argument is T or :ALL, then all elements in"
       "the sequence set are counted, otherwise just the elements in"
       "ELEMENTS-TO-COUNT are totalled."
       ))
   (:p
    #.(one-string-sp
       "SEQUENCES must be a list of sequences (except that a single string"
       "can be provided).  If a single list of elements is to be counted,"
       "it MUST be the single element of a surrounding list (see examples)."
       ))
   (:p
    #.(one-string-sp
       "The code is highly optimized for the case where ELEMENTS-TO-COUNT"
       "is a simple string and each sequence in the sequence set"
       "is also a simple string."
       ))
   (:p
    #.(one-string-sp
       "If :RETURN-AS is :SORTED-LIST (the default), the counted elements"
       "are returned as an association list, with an element being the first"
       "item of each sublist, and the count being the second item.  The"
       "items are returned sorted by count.  If :RETURN-AS is"
       ":ELEMENT-POSITION-LIST then only the counts are returned (the user"
       "must provide ELEMENTS-TO-COUNT in this case), and the counts are"
       "returned in the same order as the order of ELEMENTS-TO-COUNT."
       ))
   (:p
    #.(one-string-sp
       "TEST determines what kind of equality predicate is used."
       "If TEST is a predicate that can be used with a hash table, a"
       "linear time hash algorithm is used to do the count; otherwise"
       "a less efficient algorithm is used."
       ))
   (:p
    #.(one-string-sp
       "IF-NOT-IN-ALPHABET determines what happens if one of the elements"
       "of the sequences is not in ELEMENTS-TO-COUNT.  If it is NIL"
       "(the default), the element is ignored; otherwise an error is signalled."
       ))
   (:p
    #.(one-string-sp
       "KEY functions as a standard Common Lisp key argument."
       ))
   ) 
  (:parameters 
   (elements-to-count
    :docstring 
    #.(one-string-sp
       "The elements whose frequencies are to be totalled, or instructions"
       "to total all the elements in the sequences.")
    :value-type t)
   (sequences
    :docstring 
    "The sequences whose elements' frequencies are to be totalled."
    :value-type (or list string)
    )
   (test 
    :docstring 
    "A standard Common Lisp test argument."
    :value-type (or symbol function)
    :parameter-type &key 
    :default-value eql)
   (return-as
    :docstring 
    "Determines how the results are to be returned."
    :value-type keyword
    :parameter-type &key 
    :default-value :sorted-list)
   (key
    :docstring 
    "A standard Common Lisp key argument."
    :value-type (or symbol function)
    :parameter-type &key 
    :default-value identity)
   (if-not-in-alphabet
    :docstring 
    #.(one-string-sp
       "Determines what happens if ELEMENTS-TO-COUNT is provided and an element"
       "in the sequence set does not belong to that alphabet.")
    :value-type symbol
    :parameter-type &key 
    :default-value nil)
   )
  (:see-also lisp:count freqsort lisp:sort character-counts)
  )

(document-function elements-in
  (:summary 
   "Determines which elements of the 1st sequence are present in the 2nd.")
  (:returns "A list of elements present in both sequences."
   :type list :display-type nil)
  (:examples
   ((elements-in "this is a test" "abcde") (#\e #\a))
   ((elements-in "this is a test" "stuvw" :all-matches? t)
    (#\t #\s #\t #\s #\s #\t))
   ((elements-in "This Is A Test" "aeiou" :test 'char-equal)
    (#\e #\A #\I #\i))
   ((elements-in "This Is A Test" "aeiou" :test 'char-equal 
                 :sequence-element-test 'char-equal)
    (#\e #\A #\i) "#\i and #\I are CHAR-EQUAL, so only one is returned.")
   ((elements-in #(1 2 3 4 5 6 7 8 9) '(1.0 2.0 4.0 9.0) :test '=) (9 4 2 1)
    "Uses slow algorithm because '=' cannot be used as a hash comparitor.")
   ((elements-in #(1 2 3 4 5 6 7 8 9) '(1.0 2.0 4.0 9.0) :test 'equalp) 
    (9 4 2 1) 
    "Uses faster algorithm because 'equalp' is a valid hash comparitor."))
  (:text
   (:p
    #.(one-string-sp
       "Returns a list of elements found in the first sequence and also"
       "found in the second.  (Like LISP:UNION, but this function allows"
       "any kind of sequence, not just lists, and does not insist on the"
       "elements of the sequences being unique.)"))
   (:p
    #.(one-string-sp
       "TEST and KEY behave like standard Common Lisp TEST and KEY arguments."
       "ALL-MATCHES? determines whether equal elements in the first sequence"
       "are included individually or just one of them is included in the"
       "returned list.  SEQUENCE-ELEMENT-TEST is used to determine whether"
       "two elements of the first sequence are, in fact, equal."))
   (:p
    #.(one-string-sp
       "Note: This function is particularly fast (O(n)) when the TEST and"
       "SEQUENCE-ELEMENT-TEST predicates are usable as :test arguments"
       "to MAKE-HASH-TABLE."  
       ))
   )
  (:parameters 
   (sequence
    :docstring 
    "The sequence whose elements are returned if they are found in ALPHABET."
    :value-type sequence)
   (alphabet
    :docstring 
    "A sequence whose elements are compared to the elements of SEQUENCE."
    :value-type sequence
    )
   (test
    :docstring 
    "A standard Common Lisp test argument."
    :value-type (or symbol function)
    :parameter-type &key 
    :default-value eql)
   (key
    :docstring 
    "A standard Common Lisp key argument."
    :value-type (or symbol function)
    :parameter-type &key 
    :default-value identity)
   (sequence-element-test
    :docstring 
    "A predicate used to decide if two elements from SEQUENCE are equal."
    :value-type (or symbol function)
    :parameter-type &key 
    :default-value 'eql)
   (all-matches?
    :docstring 
    "Determines whether equal elements of SEQUENCE are all returned."
    :value-type boolean
    :parameter-type &key 
    :default-value nil)
   )
  (:see-also elements-not-in lisp:union lisp:intersection lisp:set-difference
   purge-duplicates find-duplicates set-equal))

(document-function elements-not-in
  (:summary 
   "Determines which elements of the 1st sequence are not present in the 2nd.")
  (:returns "A list of elements from the 1st sequence not present in the 2nd."
   :type list :display-type nil)
  (:examples
   ((elements-not-in "this is a test" "abcde") 
    (#\Space #\s #\i #\h #\t))
   ((elements-not-in "this is a test" "stuvw" :all-non-matches? t)
    (#\e #\Space #\a #\Space #\i #\Space #\i #\h))
   ((elements-not-in "This Is A Test" "aeiou" :test 'char-equal)
    (#\t #\Space #\s #\h #\T))
   ((elements-not-in "This Is A Test" "aeiou" :test 'char-equal 
                 :sequence-element-test 'char-equal)
    (#\Space #\s #\h #\T)
    "#\T and #\t are CHAR-EQUAL, so only one is returned.")
   ((elements-not-in #(1 2 3 4 5 6 7 8 9) '(1.0 2.0 4.0 9.0) :test '=)
    (8 7 6 5 3)
    "Uses slow algorithm because '=' cannot be used as a hash comparitor.")
   ((elements-not-in #(1 2 3 4 5 6 7 8 9) '(1.0 2.0 4.0 9.0) :test 'equalp) 
    (8 7 6 5 3)
    "Uses faster algorithm because 'equalp' is a valid hash comparitor."))
  (:text
   (:p
    #.(one-string-sp
       "Returns a list of elements found in the first sequence and not found"
       "in the second.  (Like LISP:SET-DIFFERENCE, but this function allows"
       "any kind of sequence, not just lists, and does not insist on the"
       "elements of the sequences being unique.)"))
   (:p
    #.(one-string-sp
       "TEST and KEY behave like standard Common Lisp TEST and KEY arguments."
       "ALL-NON-MATCHES? determines whether equal elements in the 1st sequence"
       "are included individually or just one of them is included in the"
       "returned list.  SEQUENCE-ELEMENT-TEST is used to determine whether"
       "two elements of the first sequence are, in fact, equal."))
   (:p
    #.(one-string-sp
       "Note: This function is particularly fast (O(n)) when the TEST and"
       "SEQUENCE-ELEMENT-TEST predicates are usable as :test arguments"
       "to MAKE-HASH-TABLE."  
       ))
   )
  (:parameters 
   (sequence
    :docstring 
    "The sequence whose elements are returned if they aren't found in ALPHABET."
    :value-type sequence)
   (alphabet
    :docstring 
    "A sequence whose elements are compared to the elements of SEQUENCE."
    :value-type sequence
    )
   (test
    :docstring 
    "A standard Common Lisp test argument."
    :value-type (or symbol function)
    :parameter-type &key 
    :default-value eql)
   (key
    :docstring 
    "A standard Common Lisp key argument."
    :value-type (or symbol function)
    :parameter-type &key 
    :default-value identity)
   (sequence-element-test
    :docstring 
    "A predicate used to decide if two elements from SEQUENCE are equal."
    :value-type (or symbol function)
    :parameter-type &key 
    :default-value 'eql)
   (all-non-matches?
    :docstring 
    "Determines whether equal elements of SEQUENCE are all returned."
    :value-type boolean
    :parameter-type &key 
    :default-value nil)
   )
  (:see-also elements-in lisp:union lisp:intersection lisp:set-difference
   purge-duplicates find-duplicates set-equal))
 
(one-string-nl
  
     )

(document-function first-difference
  (:summary "Finds the position that two sequences first differ in.")
  (:returns "A position (an integer) or NIL (if no difference)" 
   :type (or integer null) :display-type nil)
  (:examples
   ((first-difference '(3 4 5) '(3 4 6 7)) 2)
   ((first-difference "abcd" "ABCD" :test 'char-equal) nil)
   ((first-difference "abcd" '(#\a #\b #\c #\e)) 3)
   ((first-difference "xyz" "xyz" :if-same? :the-same) :the-same)
   ((first-difference "" "xyz" :if-null? 102) 102)
   )
  (:text
   (:p
    #.(one-string-sp
       "Determines whether and where two sequences differ, as defined by TEST."
       ))
   (:p
    #.(one-string-sp
       "If the two sequences are identical, the value of IF-SAME? is returned."
       "A second value is also returned:  :null if both sequences are null, or"
       ":both otherwise."
       ))
   (:p
    #.(one-string-sp
       "If one of the sequences is the null sequence, the value of IF-NULL? is"
       "returned. A second value, :first or :second, indicates which sequence"
       "is the null sequence."
       ))
   (:p
    #.(one-string-sp
       "If one of the sequences, X, is the same as the initial subsequence"
       "of the other sequence, Y, then the length of X is returned,"
       "along with a second value indicating whether the first sequence is"
       "longer (:first) or the second is longer (:second)."
       ))
   (:p
    #.(one-string-sp
       "Finally, in the standard case where the elements at position I in both"
       "sequences differ, I is returned, along with a second value :both."
       )))
  (:parameters 
   (seq1
    :docstring "The first sequence"
    :value-type sequence)
   (seq2
    :docstring "The second sequence"
    :value-type sequence)
   (test
    :docstring 
    "A standard lisp TEST argument; applied to corresponding elements."
    :value-type function
    :parameter-type &key 
    :default-value 'eql)
   (if-same?
    :docstring "If the two sequences are equivalent, this value is returned."
    :value-type t
    :parameter-type &key 
    :default-value nil)
   (if-null?  
    :docstring "If one of the sequences is null, this value is returned."
    :value-type t
    :parameter-type &key 
    :default-value -1)
   )
  (:see-also cl:position cl:find positions positions-if)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(document-module string-utils
  "Functions dealing with strings"
  (:keywords :utilities :strings)
  (:display-modes :biolisp)
  #.`(:functions ,@*utility-string-user-symbols*))

(document-function formatt
  (:summary "Writes formatted output to standard output (a macro)")
  (:returns "NIL" :type boolean)
  (:examples
   "(formatt \"A random number: ~D~%\" (random 10))"
   "macroexpands to (format t \"A random number: ~D~%\" (random 10))")
  (:text
   (:p
    #.(one-string-sp
       "A handy abbreviation instead of writing (format t ...)"
       "mostly because the form indents better if it spans multiple lines."))
   (:p
    #.(one-string-sp
       "Note: This is a macro, so it cannot be FUNCALL'ed or APPLY'ed.")))
  (:parameters 
   (format-string
    :docstring "A standard Lisp FORMAT format string" :value-type string)
   (format-args
    :docstring "Zero or more standard Lisp FORMAT arguments"
    :value-type (list-of t) :parameter-type &rest))
  (:see-also 
   cl:format cl:print cl:prin1 cl:write formatn cformatt
   cl:with-open-file cl:with-output-to-string cl:formatter 
   cl:*standard-output* cl:*error-output*
  ))

(document-function formatn
  (:summary "Creates formatted output as a string (a macro)")
  (:returns "The formatted output" :type string)
  (:examples
   "(formatn \"A random number: ~D\" (random 10))"
   "macroexpands to (format nil \"A random number: ~D\" (random 10))"
   "and evaluates to, e.g., \"A random number: 5\"")
  (:text
   (:p
    #.(one-string-sp
       "A handy abbreviation instead of writing (format nil ...)"
       "mostly because the form indents better if it spans multiple lines."))
   (:p
    #.(one-string-sp
       "Note: This is a macro, so it cannot be FUNCALL'ed or APPLY'ed.")))
  (:parameters 
   (format-string
    :docstring "A standard Lisp FORMAT format string" :value-type string)
   (format-args
    :docstring "Zero or more standard Lisp FORMAT arguments"
    :value-type (list-of t) :parameter-type &rest))
  (:see-also 
   cl:format cl:print cl:prin1 cl:write formatt cformatt
   cl:with-open-file cl:with-output-to-string cl:formatter 
   cl:*standard-output* cl:*error-output*
  ))

(document-function cformatt
  (:summary "Writes commented-out formatted output to standard output")
  (:returns "NIL" :type boolean)
  (:examples
    #.(one-string-sp   
       "(cformatt \Foo ~D\" 5) causes the characters ';; Foo 5' and a newline"
       "to be output to standard output.  NIL is returned."))
  (:text
   (:p
    #.(one-string-sp
       "A handy way to output a line of commentary."
       "The arguments are handled to (FORMAT NIL ...), then two semicolons"
       "and a space are prepended, and a newline is postpended to the result"
       "of the call to FORMAT.")))
  (:parameters 
   (format-string
    :docstring "A standard Lisp FORMAT format string" :value-type string)
   (format-args
    :docstring "Zero or more standard Lisp FORMAT arguments"
    :value-type (list-of t) :parameter-type &rest))
  (:see-also 
   cl:format cl:print cl:prin1 cl:write formatn formatt cl:terpri
   surround limited-string maybe-clip-string 
   cl:with-open-file cl:with-output-to-string cl:formatter 
   cl:*standard-output* cl:*error-output*
  ))


(document-function string-join
  (:summary 
   #.(one-string-sp   
      "Concatenates a set of strings together with a separator inserted"
      "between each of them."))
  (:returns "The concatenation" :type string)
  (:examples
   "(string-join '(\"abc\" \"def\")) --> \"abc def\""
   "(string-join '(\"abc\" \"def\" \"ghi\") \"\") --> \"abcdefghi\""
   "(string-join '(\"abc\" \"def\" \"efg\") \"++\") --> \"abc++def++ghi\""
   "(string-join '(\"abc\") \"any\") --> \"abc\""
   "(string-join '(\"\" \"abc\" \"\") '$) --> \"$abc$\""
   )
  (:text
   (:p
    #.(one-string-sp
       "Creates a single string by concentating together a list of strings"
       "while inserting a separator string between each string in the list."
       "The separator is neither prepended before the first string nor"
       "postpended after the last string."))
   (:p
    #.(one-string-sp
       "The separator can be any Lisp object will be converted to a Lisp"
       "string using the Lisp function STRING."))
   (:p
    #.(one-string-sp
       "Note: When used with a #\Tab separator, this is useful for creating"
       "tab-delimited data (i.e., Excel spreadsheet format).  When used with"
       "a #\Newline separator, this is useful for creating a string which"
       "when output to a file looks like a standard text file.")))
  (:parameters 
   (string-list
    :docstring "A set of strings to be concatenated together" 
    :value-type (list-of string))
   (sep
    :docstring 
    #.(one-string-sp
       "A string designator, converted to a string and inserted between"
       "each string in STRING-LIST.")
    :value-type (or string character symbol) 
    :default-value #\Space
    :parameter-type &optional))
  (:see-also 
   cl:concatenate s+ c+ surround string-split simple-string-split)
  )

(document-function surround
  (:summary 
   #.(one-string-sp   
      "Prepends a prefix and postpends a suffix to a string"))
  (:returns "The concatenation of the prefix, the string, and the suffix" 
   :type string)
  (:examples
   "(surround \"A quote\" #\') --> \"'A quote'\""
   "(surround \"world\" \"Hello, \" '!) --> \"Hello, world!\""
   "(surround \"\" \"foo\" \"\") --> \"foo\""
   )
  (:text
   (:p
    #.(one-string-sp
       "Creates a single string by concentating together a base string"
       "with a prefix and a suffix.  STRING must be a string; PREFIX and"
       "SUFFIX must be convertible to strings via the STRING function."
       "If SUFFIX is not provided it defaults to PREFIX (making it easier"
       "to surround a string with something like \"***\", say)."
       "If SUFFIX or PREFIX are NIL it is as if they were \"\"."
       )))
  (:parameters 
   (string
    :docstring "The string which is to be prefixed and suffixed."
    :value-type string)
   (prefix
    :docstring "The prefix to be prepended to STRING."
    :value-type (or string character symbol))
   (suffix
    :docstring "The suffix to be postpended to STRING."
    :value-type (or string character symbol)
    :default-value prefix
    :parameter-type &optional
    ))
  (:see-also 
   cl:concatenate s+ c+ string-join string-split simple-string-split)
  )

(document-function ntranslate-string
  (:summary 
   #.(one-string-sp   
      "Changes certain characters in a string based on a mapping from"
      "old characters to new characters."))
  (:returns "The STRING input, suitably modified." 
   :type string :display-type nil)
  (:examples
   "(ntranslate-string \"frob\" \"abcdefghij\" \"ABCDEFGHIJ\") --> \"FroB\")"
   "(ntranslate-string \"a_c_like_name\" \"_\" \"-\") --> \"a-c-like-name\""
   #.(one-string-sp   
      "(ntranslate-string \"<foo {bar} [baz]>\" \"<>{}[]\" \"()()()\") -->"
      "\"(foo (bar) (baz))\""))
  (:text
   (:p
    #.(one-string-sp
       "Each character of STRING that is found in FROM is changed to"
       "the corresponding (by position) character in TO; characters of"
       "STRING which are not found in FROM are not changed."
       "This function is destructive -- a copy of STRING is <<NOT>> made,"
       "modified or returned -- the original input STRING is modified and"
       "returned.  If the length of TO is not at least equal to the length"
       "of FROM the results are undefined; if TO contains non-characters"
       "the results are undefined."))
   (:p 
    #.(one-string-sp   
       "Note: This function is particularly efficient if all its arguments"
       "are simple strings.")))
  (:parameters 
   (string
    :docstring "The string whose characters are to be translated."
    :value-type string)
   (from
    :docstring "The universe of characters to be translated."
    :value-type sequence)
   (to
    :docstring "The alphabet to which characters are translated to."
    :value-type sequence)
   )
  (:see-also cl:substitute cl:replace translate-string cl:map))

(document-function translate-string
  (:summary 
   #.(one-string-sp   
      "Changes certain characters of a string based on a mapping from"
      "old characters to new characters."))
  (:returns "A copy of the STRING input, suitably modified." 
   :type string :display-type nil)
  (:examples
   "(translate-string \"frob\" \"abcdefghij\" \"ABCDEFGHIJ\") --> \"FroB\")"
   "(translate-string \"a_c_like_name\" \"_\" \"-\") --> \"a-c-like-name\""
   #.(one-string-sp   
      "(translate-string \"<foo {bar} [baz]>\" \"<>{}[]\" \"()()()\") -->"
      "\"(foo (bar) (baz))\""))
  (:text
   (:p
    #.(one-string-sp
       "Each character of STRING that is found in FROM is changed to"
       "the corresponding (by position) character in TO; characters of"
       "STRING which are not found in FROM are not changed."
       "STRING is first copied, then returned; the original STRING is not"
       "modified. If the length of TO is not at least equal to the length"
       "of FROM the results are undefined; if TO contains non-characters"
       "the results are undefined."))
    (:p
    #.(one-string-sp     
       "Note: This function is particularly efficient if all its arguments"
       "are simple strings.")))
  (:parameters 
   (string
    :docstring "The string whose characters are to be translated."
    :value-type string)
   (from
    :docstring "The universe of characters to be translated."
    :value-type sequence)
   (suffix
    :docstring "The alphabet to which characters are translated to."
    :value-type sequence)
   )
  (:see-also cl:substitute cl:replace ntranslate-string cl:map))


(document-function limited-string
  (:summary 
   "Creates an initial substring of the input, postpended with '...'")
  (:returns 
    #.(one-string-sp
       "If the input is a string, an initial substring of the input"
       "postpended with '...', or the input string itself,"
       "depending on LIMIT. If the input is not a string it is returned"
       "unmodified.")
    :type t)
  (:examples
   "(limited-string \"abcdefghij\" 5) --> \"abcde...\""
   "(limited-string \"abcdefghij\" 20) --> \"abcdefghij\""
   "(limited-string \"abcdefghij\" 9) --> \"abcdefghi...\""
   "(limited-string \"a\" 0) --> \"...\""
   "(limited-string 'frob 1) --> frob")
  (:text
   (:p
    #.(one-string-sp
       "If VALUE is not a string, it is returned."
       "If VALUE is a string, if LIMIT is greater than or equal to"
       "the length of the string, VALUE itself is returned unmodified."
       "Otherwise an initial substring of VALUE of length LIMIT is"
       "constructed, '...' is postpended, and the resulting new"
       "string is returned.  The result is not defined if LIMIT is not"
       "a non-negative integer."))
   (:p
    #.(one-string-sp
       "Note: In certain cases the returned string will be longer than"
       "the original input string, as is shown in the examples."))
   (:p
    #.(one-string-sp
       "Note: This is useful for creating formatted output to a window"
       "that will (probably) not run off the right edge of the window"
       "(assuming some reasonable window width).")))
  (:parameters 
   (value
    :docstring "The string to potentially be truncated, or a random object."
    :value-type t)
   (limit
    :docstring 
    "The maximum number of initial characters to be used from VALUE."
    :value-type (integer 0 *)
    :default-value 100
    :parameter-type &optional))
  (:see-also cl:subseq limited-form-string maybe-clip-string))

(document-function limited-form-string
  (:summary 
   #.(one-string-sp
      "Creates an initial substring of a string representation of the input,"
      "postpended with '...'"))
  (:returns 
   #.(one-string-sp
      "An initial substring of a representation of the input as a string,"
      "postpended with '...', or the string representation itself,"
      "depending on LIMIT.")
   :type string :display-type nil)
  (:examples
   "(limited-form-string '(1 2 3 4) 5) --> \"(1 2 ...\""
   "(limited-form-string '(1 2 3 4) 20) --> \"(1 2 3 4)\""
   "(limited-form-string :a-long-keyword 10) --> \"A-LONG-KEY...\""
   #.(one-string-sp
      "(limited-form-string :a-long-keyword 10 :format-mode \"~S\") -->"
      "\":A-LONG-KE...\""))
  (:text
   (:p
    #.(one-string-sp
       "First, converts FORM to a string using FORMAT with the format"
       "control string FORMAT-MODE.  Then the utility function LIMITED-STRING"
       "is called with the string representation as its VALUE argument and"
       "LIMIT as its limit argument, possibly creating an initial substring"
       "with '...' postpended."))
   (:p
    #.(one-string-sp
       "Note: In certain cases the returned string will be longer than"
       "the original string representation, because of the postpending."))
   (:p
    #.(one-string-sp
       "Note: This is useful for creating formatted output to a window"
       "that will (probably) not run off the right edge of the window"
       "(assuming some reasonable window width)."))
   (:p
    #.(one-string-sp
       "Note: To ensure that no newlines are placed into the resulting string"
       "by FORMAT, make sure the PRINT-PRETTY keyword argument value is NIL."
       )))
  (:parameters 
   (form
    :docstring "The form whose string representation is to be limited."
    :value-type t)
   (limit
    :docstring 
    #.(one-string-sp
       "The maximum number of initial characters to be used from the string"
       "representation of FORM.")
    :value-type (integer 0 *))
   (format-mode
    :docstring 
    #.(one-string-sp
       "A formatting directive as a string. The default is \"~A\";"
       "The most sensible alternative would be \"~S\", although others such"
       "as \"~D\" could conceivably make sense.")
    :default-value "~A"
    :value-type string
    :parameter-type &key
    )
   (print-pretty 
    :docstring
    #.(one-string-sp
       "Used to either enable or disable the pretty printer mechanism"
       "when the internal call to FORMAT is executed.")
    :default-value *print-pretty*
    :value-type boolean
    :parameter-type &key))
  (:see-also cl:subseq limited-string maybe-clip-string))


(document-function all-strings-of-length
  (:summary 
   #.(one-string-sp
      "Creates all possible strings of a given length from an alphabet"))
  (:returns 
   #.(one-string-sp
      "All possible strings (alphabetized, unless otherwise directed) of length LENGTH from an alphabet of characters")
   :type list)
  (:examples
   "(all-strings-of-length \"AB\" 2) --> (\"AA\" \"AB\" \"BA\" \"BB\")"
   "(all-strings-of-length \"abcdef\" 0) --> (\"\")"
   "(all-strings-of-length \"A\" 4) --> (\"AAAA\")"
   "(all-strings-of-length '(#\x) 4) --> (\"xxxx\")"
   )
  (:text
   (:p
    #.(one-string-sp
       "Creates all strings of length LENGTH whose characters are"
       "(non-uniquely) drawn from the sequence of characters in ALPHABET."))
   (:p
    #.(one-string-sp
       "By default, the list of strings is presented in alphabetical"
       "order. If AS-ODOMETER is given a non-NIL value, then the list"
       "of strings is presented according to the order of the letters"
       "in the alphabet. All strings that begin with the first letter"
       "of the alphabet are presented then all strings that begin with"
       "the second letter of the alphabet, and so forth."))
   (:p
    #.(one-string-sp
       "Note: There are (expt (length ALPHABET) LENGTH) such strings."
       "A long alphabet and/or a LENGTH of any size will result in"
       "a combinatorial explosion, which could exceed the memory capacity"
       "of the executing machine.")))
  (:parameters 
   (alphabet
    :docstring "A sequence containing the characters to be made into strings."
    :value-type sequence)
   (length
    :docstring "The number of characters each returned string contains.")
   (as-odometer
    :docstring "When not NIL, specifies that the list is to be presented in odomoter order, according to the order of characters in the alphabet string"))
  (:see-also all-ordered-pairs all-unordered-pairs all-contiguous-sublists)
  )


(document-function character-counts
  (:summary 
   #.(one-string-sp
      "Computes number of occurrences of each character in the input strings"))
  (:returns 
   #.(one-string-sp
      "A sorted list of sublists, each sublist consisting of a character"
      "and its number of occurrences in the input strings.")
   :type list)
  (:examples
   "(character-counts \"aaabb\") --> ((#\a 3) (#\b 2))"
   #.(one-string-sp
      "(character-counts \"dddeef\" :sort-by :frequency) -->"
      "((#\d 3) (#\e 2) (#\f 1))")
   "(character-counts '(\"ab\" \"bc\")) --> ((#\a 1) (#\b 2) (#\c 1))"
   "(character-counts \"\") --> NIL"
   )
  (:text
   (:p
    #.(one-string-sp
       "The number of occurrences of each character that occurs in the"
       "input strings is computed and associated with that character."))
   (:p
    #.(one-string-sp
       "If SORT-BY is not provided, the returned sublists are sorted by"
       "the character code (CHAR-CODE) of the occurring characters."
       "If SORT-BY is provided and is anything other than :CHAR-CODE"
       "(a mnemonic choice would be :FREQUENCY), then the returned sublists"
       "are sorted according to the number of occurrences of the characters,"
       "most often occurring first."))
   (:p
    #.(one-string-sp
       "Note:  In essense this provides a histogram or frequency count"
       "for text.")))
  (:parameters 
   (s
    :docstring "A single string or a list of strings."
    :value-type (or string list))
   (sort-by
    :docstring 
    #.(one-string-sp
       "Either :CHAR-CODE, denoting alphabetic sorting of the results,"
       "or anything else, denoting frequency sorting.")))
  (:see-also cl:count cl:count-if cl:count-if-not freqsort frequency-count)
  )


(document-function s+
  (:summary 
   #.(one-string-sp
      "Converts all its arguments into strings and concatenates them together"))
  (:returns 
   #.(one-string-sp
      "The concatenation of the string representation of all its arguments")
   :type string)
  (:examples
   "(s+ 1 #\a 'foo) --> \"1aFOO\""
   "(s+ \"frob \" #C(1.0 1.0)) --> \"frob #C(1.0 1.0)\""
   "(s+ '+ (float 1/3) '?) --> \"+0.3333333333333333?\""
   "(s+) --> \"\""
   )
  (:text
   (:p
    #.(one-string-sp
       "First, each argument is converted to a string representation"
       "if possible, using the generic function ANYTHING-STRINGLIKE-TO-STRING."
       "(This function converts characters, symbols, numbers and pathnames to"
       "their standard string representation. For any other type of Lisp"
       "object an error is generated -- a user may define additional methods"
       "for this function which will allow S+ to convert other types of"
       "arguments.)"))
   (:p
    #.(one-string-sp
       "Once a string representation for each argument is had, all the"
       "strings are concatenated together and a single string returned."))
   (:p
    #.(one-string-sp
       "Note: S+ is a function, so it may be used in situations where"
       "ONE-STRING could not be."))
   (:p
    #.(one-string-sp
       "Note: The Weblistener software provides a method for"
       "ANYTHING-STRINGLIKE-TO-STRING to convert frames into their names"
       "via FNAME.")))
  (:parameters 
   (args
    :docstring "Any number of objects to be stringified."
    :value-type list
    :parameter-type &rest))
  (:see-also cl:concatenate cl:append one-string)
  )

(document-function one-string
  (:summary 
   #.(one-string-sp
      "Macro: Concatenates its arguments (which must be strings) together."))
  (:returns 
   #.(one-string-sp
      "The concatenation of all its arguments")
   :type string)
  (:examples
   "(one-string \"abc\" \"def\") --> \"abcdef\""
   "(one-string (string 'abc) \"def\") --> \"ABCdef\""
   "(one-string) --> \"\""
   "(one-string (string 'a) (string #\b) \"c\") --> \"Abc\""
   )
  (:text
   (:p
    #.(one-string-sp
       "The effect is to concatenate all its arguments (which must be strings)"
       "together.  Since ONE-STRING is a macro, it looks at its arguments at"
       "macroexpansion time and if adjacent arguments are both constant"
       "strings, it concatenates them together during macroexpansion rather"
       "than at execution time."))
   (:p
    #.(one-string-sp
       "Note: This macro is normally used to create a string that would"
       "otherwise span multiple lines textually by breaking up the string"
       "into substrings that fit on individual lines; it is also sometimes"
       "used as an abbreviation for (concatenate 'string ...)"
       "(But see s+ for a more concise form.)")))
  (:parameters 
   (string-designators
    :docstring "Any number of strings to be concatenated."
    :value-type (list-of string)
    :parameter-type &rest))
  (:see-also cl:concatenate cl:append one-string-nl 
   one-string-nli one-string-sp s+)
  )

(document-function one-string-nl
  (:summary 
   #.(one-string-sp
      "Macro: Concatenates its arguments (which must be strings) together"
      "and puts a #\Newline character after each argument string (including"
      "the last)."))
  (:returns 
   #.(one-string-sp
      "The concatenation of all its arguments, with newlines")
   :type string)
  (:text
   (:p
    #.(one-string-sp
       "The effect is to concatenate all its arguments (which must be strings)"
       "together, as with ONE-STRING; the difference is that a #\Newline"
       "character is effectively added to the end of each argument string"
       "before they are all concatenated together.  (If called with no"
       "arguments the null string is returned.)"))
   (:p
    #.(one-string-sp
       "Note: This macro is normally used to create a string that would"
       "otherwise span multiple lines textually by breaking up the string"
       "into substrings that fit on individual lines; in particular it"
       "is used for documentation strings so that when they are displayed"
       "back, the documentation string is already formatted so that it"
       "does not run off the right edge of a display.")))
  (:parameters 
   (string-designators
    :docstring "Any number of strings to be concatenated."
    :value-type (list-of string)
    :parameter-type &rest))
  (:see-also cl:concatenate cl:append one-string 
   one-string-nli one-string-sp s+)
  )

(document-function one-string-nli
  (:summary 
   #.(one-string-sp
      "Macro: Concatenates its arguments (which must be strings) together"
      "and puts a #\Newline character after each argument string except"
      "the last."))
  (:returns 
   #.(one-string-sp
      "The concatenation of all its arguments, with newlines")
   :type string)
  (:text
   (:p
    #.(one-string-sp
       "The effect is to concatenate all its arguments (which must be strings)"
       "together, as with ONE-STRING; the difference is that a #\Newline"
       "character is effectively added to the end of each argument string"
       "except the last before they are all concatenated together."
       "(If called with no arguments the null string is returned"))
   (:p
    #.(one-string-sp
       "Note: This macro is normally used to create a string that would"
       "otherwise span multiple lines textually by breaking up the string"
       "into substrings that fit on individual lines; in particular it"
       "is used for certain DOCUMENT-FUNCTION strings.")))   
  (:parameters 
   (string-designators
    :docstring "Any number of strings to be concatenated."
    :value-type (list-of string)
    :parameter-type &rest))
  (:see-also cl:concatenate cl:append one-string one-string-nl one-string-sp s+)
  )

(document-function one-string-sp
  (:summary 
   #.(one-string-sp
      "Macro: Concatenates its arguments (which must be strings) together"
      "and puts a #\Space character between each argument string (but not"
      "before the first or after the last)."))
  (:returns 
   #.(one-string-sp
      "The concatenation of all its arguments, with spaces intertwined.")
   :type string)
  (:examples
   "(one-string-sp \"abc\" \"def\") --> \"abc def\""
   "(one-string-sp (string 'abc) \"def\") --> \"ABC def\""
   "(one-string-sp) --> \"\""
   "(one-string-sp (string 'a) (string #\b) \"c\") --> \"A b c"
   )
  (:text
   (:p
    #.(one-string-sp
       "The effect is to concatenate all its arguments (which must be strings)"
       "together, as with ONE-STRING; the difference is that a #\Space"
       "character is effectively added to the end of each argument string"
       "except the last before they are all concatenated together."))
   (:p
    #.(one-string-sp
       "Note: This macro is normally used to create a string that would"
       "otherwise span multiple lines textually by breaking up the string"
       "into substrings that fit on individual lines; in particular it"
       "is used for creating documentation text spanning multiple lines"
       "for DOCUMENT-FUNCTION forms (which are rendered using HTML so that"
       "inserting newlines as with ONE-STRING-NL would be fruitless.)")))
  (:parameters 
   (string-designators
    :docstring "Any number of strings to be concatenated."
    :value-type (list-of string)
    :parameter-type &rest))
  (:see-also cl:concatenate cl:append one-string 
   one-string-nl one-string-nli s+)
  )

(document-function string-split
  (:summary "Chops a string up into substrings")
  (:returns 
    #.(one-string-sp   
       "A set of non-overlapping substrings of the input string, possibly"
       "sharing data space with the input string")
   :type (list-of strings))
  (:examples
   "(string-split \"This is a string\") --> (\"This\" \"is\" \"a\" \"string\")"
   "(string-split \"foo;bar;;baz\" #\\;) --> (\"foo\" \"bar\" \"\" \"baz\")"
   "(string-split \"Foobarbaz\" #\\*) --> (\"Foobarbaz\")"
   "(string-split \"FooFarbaF\" #\\F) --> (\"\" \"oo\" \"arba\" \"\")"
   )
  (:text
   (:p
    #.(one-string-sp
       "The input string is divided up into substrings occurring between"
       "DELIMITER characters (such that DELIMITER characters are *NOT*"
       "included in the substrings). If a DELIMITER character occurs as"
       "the first (or last) element of the input string a null substring"
       "is created as the first (or last) substring returned.  If"
       "successive DELIMITER characters occur null substrings are also created"
       "in the appropriate places in the returned substring list."))
   (:p
    #.(one-string-sp
       "METHOD determines whether the substrings returned share data space"
       "with the input string (that is, if you modify one of the returned"
       "substrings, whether the original string is modified as well."))
   (:p
    #.(one-string-sp
       "If METHOD is :NEW-STRINGS, the returned substrings do not share data"
       "space with the original string; the returned substrings are"
       "created as SIMPLE-STRINGs."))
   (:p
    #.(one-string-sp
       "If METHOD is :IN-PLACE, the returned substrings share data"
       "space with the original string; the returned substrings are"
       "created as displaced arrays (see MAKE-ARRAY), and are not"
       "SIMPLE-STRINGs."))
   (:p
    #.(one-string-sp
       "Note: This function is useful in many capacities; some of the"
       "most common are 1) dividing a string into 'words'; 2) dividing"
       "a file into lines (using #\Newline as the DELIMITER), 3) dividing"
       "a line into 'cells', as in an EXCEL-format file -- using #\Tab to"
       "separate the 'cells' from each other."))
   (:p
    #.(one-string-sp
       "Note: The :IN-PLACE method is time and space efficient when dividing"
       "a very long string into a relatively small number of substrings"
       "(such as dividing a string a million characters long into 20 pieces)."
       "The :NEW-STRINGS method (the default) is more space efficient when the"
       "input string is relatively short and/or the size of each substring"
       "is generally small (such as dividing up a 1,000 character file into"
       "its english words).")))
  (:parameters 
   (string
    :docstring "The input to be split into substrings"
    :value-type string)
   (delimiter
    :docstring "A character which delimits which substrings are created."
    :value-type character
    :parameter-type &optional
    :default-value #\Space)
   (method
    :docstring "One of :NEW-STRINGS (the default) or :IN-PLACE"
    :value-type keyword
    :parameter-type &optional
    :default-value :NEW-STRINGS))
  (:see-also cl:subseq separate-into-lists)
  )

(document-function s+join
  (:summary 
    #.(one-string-sp   
       "Converts objects to strings and puts them together with"
       "a separator in between each."))
  (:returns 
    #.(one-string-sp   
       "A single string being the concatenation of the"
       "string representation of each input and the separator.")
   :type string :display-type nil)
  (:examples
   "(s+join #\Space 1 'goo #\;) --> \"1 GOO ;\""
   "(s+join \" --> \" 1 2 3 \"infinity\") --> \"1 --> 2 --> 3 --> infinity\""
   )
  (:text
   (:p
    #.(one-string-sp
       "This function combines S+ and STRING-JOIN. Each &rest argument"
       "is converted to a string using ANYTHING-STRINGLIKE-TO-STRING"
       "(as does S+), and then the strings are combined, along with JOIN"
       "as the separator, using STRING-JOIN.")))
  (:parameters 
   (join
    :docstring 
    #.(one-string-sp
       "Something that can be converted to a string, representing"
       "the SEPARATOR argument to STRING-JOIN.")
    :value-type t)
   (args
    :docstring "A set of objects to be converted to string representation"
    :value-type t
    :parameter-type &rest))
  (:see-also string-join s+ cl:concatenate string-split)
  )


(document-function whitespacep
  (:summary 
   #.(one-string-nl
      "Returns T if input is a whitespace character or input is a string"
      "consisting of all whitespace characters."))
  (:returns "T or NIL" :type boolean)
  (:examples
   "(whitespacep #\\space) --> T"
   "(whitespacep \"  \") --> T"
   "(whitespacep #\\x) --> NIL"
   "(whitespacep (s+ #\\tab #\\newline #\\null)) --> T")
  (:text 
   (:p
    #.(one-string-nl
       "Whitespace characters as defined for this function are:"
       "#\\Space #\\Tab #\\Return #\\Newline #\\Page #\\Null #\\Linefeed."
       "If the input is one of these characters or a string consisting"
       "solely of these characters, T is returned, otherwise NIL.")))
  (:parameters 
   (x
    :docstring 
    "The input character or string."
    :value-type (or character string)))
  (:see-also lisp:digit-char-p lisp:alpha-char-p lisp:alphanumericp 
   lisp:every lisp:char-code lisp:code-char remove-all-whitespace)
  )

(document-function remove-all-whitespace
  (:summary "Remove all whitespace (as defined by WHITESPACEP) from STRING.")
  (:returns "A copy of STRING with all whitespace removed." 
   :type string :display-type nil)
  (:examples
   "(remove-all-whitespace \"   \") --> \"\""
   "(remove-all-whitespace \"This is a test.\") --> \"Thisisatest.\""
   "(remove-all-whitespace (s+ #\\newline #\\a #\\tab \" foo \")) --> \"afoo\""
   )
  (:text 
   (:p
    #.(one-string-nl
       "A copy of the input string with all whitespace characters removed"
       "(as defined by WHITESPACEP) is returned.")))
  (:parameters 
   (string :docstring "The input string which is copied."
           :value-type string))
  (:see-also lisp:remove lisp:remove-if lisp:remove-if-not lisp:delete 
   whitespacep lisp:replace remove-all-of))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(document-module macro-utils
  "Macros to make programming a bit easier."
  (:keywords :utilities :macros)
  (:display-modes :biolisp)
  #.`(:functions ,@*utility-macro-user-symbols*))


(document-function vif
  (:summary 
    #.(one-string-sp   
       "Like LISP:IF, except that it binds a user-provided symbol to the"
       "result of the conditional form, and this value is then available"
       "within the scope of the THEN and ELSE clauses. (a macro)")) 
  (:returns 
    #.(one-string-sp   
       "What LISP:IF returns -- either the result of evaluating the THEN"
       "clause or the result of the evaluating the ELSE clause.")
   :type t)
  (:examples
   "(vif (x (member 2 '(1 2 3))) x 0) --> (2 3)"
   "(vif (y (position #\\y \"xyzzy\")) (1+ y) -1) --> 2"
   "(vif (z (every 'evenp '(1 2 3))) 'shazam!) --> NIL"
   )
  (:syntax (vif (var test) then &optional (else nil)))
  (:parameters 
   (var 
    :docstring 
    "A symbol, bound to the value of TEST while THEN/ELSE are executed."
    :value-type symbol)
   (test 
    :docstring
    "Any form, usually some kind of predicate.")
   (then
    :docstring "Any form, executed only if TEST returns non-NIL."
    :value-type t
    )
   (else 
    :docstring "Any form, executed only if TEST returns NIL."
    :value-type t
    :parameter-type &optional
    :default-value nil))
  (:see-also lisp:if vwhen vwhile lisp:cond lisp:case lisp:when)
  )

(document-function vwhen 
  (:summary 
   #.(one-string-sp   
      "Like LISP:WHEN, except that it binds a user-provided symbol to the"
      "result of the conditional form, and this value is then available"
      "within the scope of the body.  (a macro)"))
  (:returns 
   #.(one-string-sp   
      "What LISP:WHEN returns --  the result of evaluating the BODY"
      "clauses as an implicit PROGN (i.e., the result of the last body clause)."
      )
   :type t)
  (:examples
   "(vwhen (x (member 2 '(1 2 3))) (print 'hello) (cdr x)) --> (3)"
   "(vwhen (y (position #\\y \"xyzzy\")) (1+ y)) --> 2"
   "(vwhen (z (every 'evenp '(1 2 3))) 'shazam!) --> NIL"
   )
  (:syntax (vwhen (var test) &body body))
  (:parameters 
   (var 
    :docstring 
    "A symbol, bound to the value of TEST while BODY forms are executed."
    :value-type symbol)
   (test 
    :docstring
    "Any form, usually some kind of predicate."
    :value-type t)
   (body
    :docstring 
    #.(one-string-sp 
       "Any number of forms, all of which are executed with the value of"
       "the last form being returned.")
    :value-type t
    :parameter-type &rest
    ))
  (:see-also lisp:if vif vwhile lisp:cond lisp:case lisp:when)
  )

(document-function vwhile 
  (:summary 
   #.(one-string-sp  
      "Continues to execute a set of forms (a body) until a given"
      "condition (evaluated each time before the set of forms is"
      "executed again) is no longer true.  (a macro)"))

  (:returns 
   #.(one-string-sp   
      "When EXPR evaluates to NIL, the END-FORMS, if any, are evaluted"
      "and the result of the last form is returned (or NIL if there are no"
      "END-FORMS.")
   :type t)
  (:examples
   #.(one-string-sp
      "(let ((x '(1 2 3 4)) (c 0)) (vwhile (y (member 2 x) c) (pop x)"
      "(incf c))) --> 2")
   #.(one-string-sp
      "(vwhile (state (everything-ok?) state) (print state) (sleep 3600))"
      "--> <<Hopefully will never return!  Otherwise returns bad state.  >>")
   )
  (:syntax (vwhile (var expr &rest end-forms) &body body))
  (:text 
   (:p
    #.(one-string-sp
       "Executes BODY as long as EXPR is non-nil, lexically binding the"
       "the symbol VAR to the value of EXPR on each iteration step."
       "END-FORMS are executed when EXPR returns NIL and VWHILE returns the"
       "value of evaluating the last END-FORM (or NIL if there are"
       "no end-forms."))
   (:p
    #.(one-string-sp
       "Note: If EXPR never returns NIL then this loop will continue forever!"
       )))
  (:parameters 
   (var 
    :docstring 
    "A symbol, bound to the value of EXPR while BODY forms are executed."
    :value-type symbol)
   (expr
    :docstring
    "Any form, usually some kind of predicate."
    :value-type t)
   (end-forms
    :docstring 
    #.(one-string-sp 
       "Any number of forms, all of which are executed when the loop"
       "terminates, the result of the last form being returned.")
    :value-type t
    :parameter-type &rest
    )
   (body 
    :docstring 
    #.(one-string-sp 
       "Any number of forms, all of which are executed each time through"
       "the loop.")))
  (:see-also lisp:do lisp:do* lisp:loop "bioutils:xloop" vif vwhen)
  )

(document-function vcond
  (:summary 
   #.(one-string-sp   
      "Like LISP:COND, except that it allows the user to bind"
      "a variable to the result of the conditional test for each clause."
      ))
  (:returns
   #.(one-string-sp   
      "What LISP:COND returns, the result of executing the subsequent"
      "forms of the first conditional that is non-nil."
      )
   :type t)
  (:examples
   ((vcond 
      ((x (position #\q "abc")) x)
      ((y (position #\b "abc")) y)
      (t -1)) 
    1)
   )    
  (:syntax (vcond
             ((symbol1 cond1) &body body1)
             ((symbol2 cond2) &body body2)
             &rest more-vcond-clauses))
  (:text
   (:p
    #.(one-string-sp
       "Binds a variable to the result of each conditional for"
       "the scope of the clauses executed if that conditional is true."
       "Instead of a simple conditional as the first element of a COND"
       "clause, VCOND takes a 2-element form, (symbol conditional)."
       "If the conditional is a symbol rather than a 2-element list,"
       "it is treated as a COND clause, no binding variable is assumed"
       "(this allows the user to use T, as in the above example)."))
   )

  (:parameters 
   (symboln 
    :docstring 
    "Symboln is bound to the result of condn while bodyn executes."
    :value-type symbol)
   (condn
    :docstring
    "A standard conditional clause as in LISP:COND."
    :value-type t)
   (bodyn
    :docstring "A standard implicit PROGN, as in LISP:COND."
    :value-type t
    )
   )
  (:see-also vif lisp:if vwhen vwhile lisp:cond lisp:case lisp:when)
  )
  
(document-function assocadr 
  (:summary 
   #.(one-string-sp  
      "Shorthand for (cadr (assoc key list)).  (a macro)"))
  (:returns 
   #.(one-string-sp   
      "The element associated with KEY, or NIL if KEY is not"
      "found.  (There's no way to distinguish between the cases"
      "of KEY not being found and a KEY having an associated value of NIL.)")
   :type t)
  (:examples
   "(assocadr 'foo '((bar 3) (foo 4))) --> 4"
   "(assocadr 23 '((1 foo) (2 bar))) --> NIL"
   "(assocadr 'foo '(((bar foo) 3) ((foo bar) 4)) :key 'second) --> 3"
   )
  (:syntax (assocadr key alist &rest assoc-keywords))
  (:text 
   (:p
    #.(one-string-sp
       "A macro used as a shorthand for (cadr (assoc key list))."
       "Small amounts of data are often stored in what are called"
       "association lists, which are often of the form:"
       "((key1 datum1) (key2 datum2) ...)."
       "The lisp function ASSOC returns the entire (key datum) pair,"
       "while often what you want is simply the datum.")))
  (:parameters 
   (key 
    :docstring 
    "Any lisp object, to be searched for in ALIST."
    :value-type t)
   (alist
    :docstring
    "A list which should be of the form ((k1 v1) (k2 v2) ...)"
    :value-type list)
   (assoc-keywords
    :docstring 
    #.(one-string-sp 
       "Keywords and keyword values which are passed en masse to ASSOC.")
    :value-type list
    :parameter-type &rest
    ))
  (:see-also lisp:assoc assocdr lisp:find lisp:position lisp:get
   lisp:getf lisp:gethash)
  )

(document-function assocdr
  (:summary 
   #.(one-string-sp  
      "Shorthand for (cdr (assoc key list)).  (a macro)"))
  (:returns 
   #.(one-string-sp   
      "The element associated with KEY, or NIL if KEY is not"
      "found.  (There's no way to distinguish between the cases"
      "of KEY not being found and a KEY having an associated value of NIL.)")
   :type t)
  (:examples
   "(assocdr 'foo '((bar . 3) (foo . 4))) --> 4"
   "(assocdr 23 '((1 . foo) (2 . bar))) --> NIL"
   "(assocdr 'foo '(((bar foo) . 3) ((foo bar) . 4)) :key 'second) --> 3"
   )
  (:syntax (assocdr key alist &rest assoc-keywords))
  (:text 
   (:p
    #.(one-string-sp
       "A macro used as a shorthand for (cdr (assoc key list))."
       "Small amounts of data are often stored in what are called"
       "association lists, which are often of the form:"
       "((key1 . datum1) (key2 . datum2) ...)."
       "The lisp function ASSOC returns the entire (key . datum) pair,"
       "while often what you want is simply the datum."
       ))
   (:p 
    #.(one-string-sp
       "Note: If the association list looks like:"
       "((foo 1 2 3) (bar 4 5 6))"
       "then to get the data associated with FOO -- the list (1 2 3) --"
       "use ASSOCDR; ASSOCADR would only get you '1'.")))
  (:parameters 
   (key 
    :docstring 
    "Any lisp object, to be searched for in ALIST."
    :value-type t)
   (alist
    :docstring
    "A list which should be of the form ((k1 . v1) (k2 . v2) ...)"
    :value-type list)
   (assoc-keywords
    :docstring 
    #.(one-string-sp 
       "Keywords and keyword values which are passed en masse to ASSOC.")
    :value-type list
    :parameter-type &rest
    ))
  (:see-also lisp:assoc assocadr lisp:find lisp:position lisp:get
   lisp:getf lisp:gethash)
  )


(document-function mvsetf
  (:summary 
   #.(one-string-sp  
      "A way to setf multiple places using multiple values.  (a macro)"))
  (:returns 
   #.(one-string-sp   
      "The value stored into the last place; usually the last value returned by"
      "VALUE-EXPR (unless it returns more or less values than there are places"
      "to store into)")
   :type t)
  (:examples
   "(mvsetf (x y) (values 3 4)) --> 4 ;; now x = 3 and y = 4"
   #.(one-string-sp
      "(let ((s \"ab\")) (mvsetf ((aref s 0) (aref s 1)) (values #\\x #\\y)))"
      "  --> #\\y ;; s became the string \"xy\"")
   "(mvsetf (x y z) (values 1 2)) --> NIL ;; now x = 1, y = 2, z = nil."
   "(mvsetf (x y) (values 1 2 3)) --> 2 ;; now x = 1, y = 2, 3 is not used."
   #.(one-string-sp
      "(let ((v #(1 2 3)) (list '(1 2 3)))"
      "  (mvsetf ((aref v 1) z (third list)) (values 10 11 12)))"
      " --> 12 ;; now v = #(1 10 3), z = 11, list = (1 2 12)")
   #.(one-string-sp
      "(let ((c 0) (v #(1 2 3 4 5)))"
      "  (mvsetf ((aref v (incf c)) (aref v (incf c)))" 
      "     (values (incf c) (incf c))))"
      " --> 2 ;; now v = #(1 2 3 1 2) -- not #(1 3 4 4 5)"))
  (:syntax (mvsetf setf-exprs value-expr))
  (:text 
   (:p
    #.(one-string-sp
       "Common lisp provides MULTIPLE-VALUE-SETQ, which allows"
       "one to store the result of multiple values into multiple variables."
       "It does not, however, provide an analogous way to store multiple"
       "values into multiple arbitrary places.  MVSETF provides this"
       "functionality."  
       ))
   (:p
    #.(one-string-sp
       "MVSETF evaluates VALUE-EXPR, which may return multiple values."
       "The jth value returned is stored into the jth place (the jth"
       "form of SETF-EXPRS). If there are more values than places,"
       "the excess values are ignored.  If there are more places than"
       "values the value NIL is stored into each excess place, and"
       "NIL is returned."
       ))
   (:p 
    #.(one-string-sp
       "MVSETF has one slight quirk: it does not evaluate its forms"
       "from left to right -- it first evaluates VALUE-EXPR, and"
       "then evaluates any forms within the SETF-EXPRS that need evaluation."
       "(A standard SETF form always arranges for its subexpressions to be"
       "evaluated left to right.)")))
  (:parameters 
   (setf-exprs 
    :docstring 
    "An unevaluated list of valid places for SETF."
    :value-type list)
   (value-expr
    :docstring
    "A form which may return multiple values."
    :value-type t)
   )
  (:see-also lisp:multiple-value-setq lisp:setf lisp:multiple-value-bind
   lisp:values lisp:multiple-value-prog1 lisp:setq lisp:defsetf)
  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(document-module hash-utils
  #.(one-string-sp
     "Functions dealing with hash tables and functions which use hash tables"
     "to perform certain set-like operations.")
  (:keywords :utilities :hash-tables :binary-search :set-operations)
  (:display-modes :biolisp)
  #.`(:functions ,@*utility-hash-user-symbols*))


(document-function hash-table-contents 
  (:summary "Creates a list of the key/value pairs in a hash table")
  (:returns "A list of key / value pairs" :type list :display-type nil)
  (:examples
   #.(one-string-sp
      "(let ((h (utils:create-hash-table '((a 1) (b 2) (c 3)))))"
      "  (hash-table-contents h))"
      " --> ((b 2) (c 3) (a 1)) or ((a 1) (b 2) (c 3)) etc"))
  (:text
   (:p 
    #.(one-string-sp
       "Returns a list of key / value pairs from the input hash table."
       "The list is in the form ((key1 value1) (key2 value2) ...)."
       "The order of the key / value pairs is not defined; any ordering"
       "is possible.  If the hash table is empty, NIL is returned."
       )))
  (:parameters
   (ht :docstring "A hash table." :value-type hash-table))
  (:see-also hash-table-keys hash-table-values 
   lisp:maphash lmaphash lmaphashnn lisp:gethash
  ))

(document-function hash-table-keys
  (:summary "Creates a list of all the keys in a hash table")
  (:returns "The keys of the input hash table" :type list)
  (:examples
   #.(one-string-sp
      "(let ((h (utils:create-hash-table '((a 1) (b 2) (c 3)))))"
      "   (hash-table-keys h))"
      " --> (a b c) or (c b a) or (b a c) etc."))
  (:text
   (:p 
    #.(one-string-sp
       "Returns a list of the keys from the input hash table."
       "The order of the keys is not defined; any ordering"
       "is possible.  If the hash table is empty, NIL is returned."
       )))
  (:parameters
   (ht :docstring "A hash table." :value-type hash-table))
  (:see-also hash-table-contents hash-table-values 
   lisp:maphash lmaphash lmaphashnn lisp:gethash
  ))

(document-function hash-table-values
  (:summary 
   "Creates a list of all the values associates with the keys in a hash table")
  (:returns
   "The values associated with the keys of the input hash table"
   :type list)
  (:examples
   #.(one-string-sp
      "(let ((h (utils:create-hash-table '((a 1) (b 2) (c 3)))))"
      "   (hash-table-values h))"
      " --> (1 2 3) or (3 2 1) or (2 3 1) etc."))
  (:text
   (:p 
    #.(one-string-sp
       "Returns a list of the values associated with the keys"
       "from the input hash table. The order of the keys is not defined;"
       "any ordering is possible. If the hash table is empty, NIL is returned."
       )))
  (:parameters
   (ht :docstring "A hash table." :value-type hash-table))
  (:see-also hash-table-contents hash-table-keys 
   lisp:maphash lmaphash lmaphashnn lisp:gethash
  ))

(document-function create-hash-table
  (:summary
   "Creates a hash table and initializes its contents.")
  (:returns 
   "The hash table so created" :type hash-table :display-type nil)
  (:examples
   #.(one-string-sp
      "(create-hash-table '((a 1) (b 2) (c 10))) --> ;; a hash table"
      "whose keys are A,B and C with associated values 1,2 and 3, respectively."
      "The hash table uses the EQL test (the default).")
   #.(one-string-sp
      "(create-hash-table '((\"abc\" . 1) (\"def\" . 2)) :test 'equal) -->"
      ";; a hash table whose keys are \"abc\" and \"def\" with associated"
      "values 1 and 2 respectively. The hash table used the EQUAL test"
      "(which works case-sensitively on strings).")
   #.(one-string-sp
      "(create-hash-table '(a b c) :default-value 23) --> ;; a hash table"
      "whose keys are A,B and C each with the associated value 23."))
  (:text
   (:p 
    #.(one-string-sp
       "A new hash table is created (using LISP:MAKE-HASH-TABLE), and"
       "it is populated with key/value pairs taken from INITIAL-CONTENTS."))
   (:p
    #.(one-string-sp
       "The INITIAL-CONTENTS list may contain three different types of"
       "data representing key/value pairs: 1) a list of the form (key value),"
       "2) a dotted pair of the form (key . value), 3) a non-list taken to"
       "be both key and value (unless DEFAULT-VALUE is specified, in which"
       "case the value is determined by the value of DEFAULT-VALUE)."))
   (:p
    #.(one-string-sp
       "CREATE-HASH-TABLE accepts all keywords, and any that are not"
       "specific to it --anything but DEFAULT-VALUE and MODE -- are"
       "passed through to MAKE-HASH-TABLE (see the Lisp Hyperspec for"
       "a specification of all the keywords that MAKE-HASH-TABLE will"
       "accept)."))
   (:p
    #.(one-string-sp
       "The DEFAULT-VALUE keyword determines how to compute a value for"
       "a singleton key (see above).  If DEFAULT-VALUE is a function object,"
       "it must accept a single argument, and it is called with the"
       "singleton key value to compute a value (the default for DEFAULT-VALUE"
       "is #'IDENTITY, thus each singleton key will by default be associated"
       "with itself). Otherwise, it's value is used directly (In this way all"
       "singleton keys can be initialized to the same identical value).")))
  (:parameters
   (initial-contents 
    :docstring 
    #.(one-string-sp
       "A list whose elements define key/value pairs to initialize the created"
       "hash table with.")
      :value-type list)
   (default-value 
    :docstring 
    #.(one-string-sp
       "A function or value used to compute the value to be associated with"
       "singleton keys.")
    :value-type t
    :parameter-type &key)
   (other-keys
    :docstring
    "keywords and their values passed to MAKE-HASH-TABLE."
    :value-type t
    :parameter-type &allow-other-keys))
  (:see-also lisp:make-hash-table hash-table-contents 
   hash-table-keys hash-table-values
   lisp:maphash maphash lmaphashnn 
   lisp:gethash lisp:with-hash-table-iterator
   "bioutils:xloop"
   ))


(document-function lmaphash
  (:summary
   #.(one-string-sp
      "Maps a function over the keys and values of a hash table"))
  (:returns 
   "The results of calling the input function on the hash table entries"
   :type list)
  (:examples
   #.(one-string-sp
      "(let ((h (utils:create-hash-table '((1 1) (2 5) (3 0)))))"
      "   (lmaphash (lambda (k v) (+ k v)) h)"
      " --> (2 7 3) or (7 3 2) or (3 7 2) etc.")
   #.(one-string-sp
      "(let ((h (utils:create-hash-table '((1 1) (2 5) (3 0)))))"
      "   (lmaphash (lambda (k v) (declare (ignore k)) (- v)) h)"
      " --> (-1 -5 0) or (-5 0 -1) or (-5 -1 0) etc."))
  (:text
   (:p 
    #.(one-string-sp
       "The function F, which must be a function which will accept"
       "two arguments, is called once on each (key value) pair in"
       "input the hash table HASH-TABLE, and the results collected"
       "and returned in a list."))
   (:p
    #.(one-string-sp
       "Note: The lisp function MAPHASH does not return a value, it"
       "executes its input function for side effect only."))
   (:p
    #.(one-string-sp
       "Note: The bioutils XLOOP facility (in BioBike named LOOP)"
       "iterates over the key/value pairs in a syntactically straightforward"
       "manner: (xloop for (key value) in my-hash-table ...). LISP:LOOP"
       "can also iterate over hash tables but using much more obscure syntax."
       )))
  (:parameters
   (f :docstring "A function or function designator of two arguments"
      :value-type function-designator)
   (ht :docstring "A hash table." :value-type hash-table))
  (:see-also hash-table-contents hash-table-keys hash-table-values
   lisp:maphash maphash lmaphashnn 
   lisp:gethash lisp:with-hash-table-iterator
   "bioutils:xloop"
   ))

(document-function lmaphashnn
  (:summary
   #.(one-string-sp
      "Maps a function over the keys and values of a hash table, returning"
      "non-NIL results."))
  (:returns 
   "The non-NIL results of calling the input function on the hash table entries"
   :type list)
  (:examples
   #.(one-string-sp
      "(let ((h (utils:create-hash-table '((1 1) (2 5) (3 0)))))"
      "   (lmaphashnn (lambda (k v) (when (evenp k) (+ k v))) h))"
      " --> (7)")
   #.(one-string-sp
      "(let ((h (utils:create-hash-table '((1 1) (2 5) (3 0)))))"
      "   (lmaphashnn"
      "     (lambda (k v) (declare (ignore k)) (when (minusp v) v)) h))"
      " --> nil"))
  (:text
   (:p 
    #.(one-string-sp
       "The function F, which must be a function which will accept"
       "two arguments, is called once on each (key value) pair in"
       "input the hash table HASH-TABLE, and the non-NIL results collected"
       "and returned in a list."))
   (:p
    #.(one-string-sp
       "Note: The lisp function MAPHASH does not return a value, it"
       "executes its input function for side effect only."))
   (:p
    #.(one-string-sp
       "Note: The bioutils XLOOP facility (in BioBike named LOOP)"
       "iterates over the key/value pairs in a syntactically straightforward"
       "manner: (xloop for (key value) in my-hash-table ...). LISP:LOOP"
       "can also iterate over hash tables but using much more obscure syntax."
       )))
  (:parameters
   (f :docstring "A function or function designator of two arguments"
      :value-type (or function symbol))
   (ht :docstring "A hash table." :value-type hash-table))
  (:see-also hash-table-contents hash-table-keys hash-table-values
   lisp:maphash maphash lmaphash
   lisp:gethash lisp:with-hash-table-iterator
   "bioutils:xloop"
   ))


(document-function showhash
  (:summary
   "Displays the key/value pairs of a hash table.")
  (:returns "NIL" :type boolean)
  (:examples
   #.(one-string-sp
      "(let ((h (utils:create-hash-table '((1 1) (2 5) (3 0)))))"
      "  (showhash h))"
      " --> NIL, printing out '1 1' '2 5' and '3 0' on separate lines")
   #.(one-string-sp
      "(let ((h (utils:create-hash-table '((1 1) (2 5) (3 0)))))"
      "  (showhash h 2))"
      " --> NIL, printing out '1 1' and '2 5' on separate lines"))
  (:text
   (:p 
    #.(one-string-sp
       "SHOWHASH is executed for side effect -- it always returns NIL."
       "It prints out a header describing the input hash table, and"
       "then its key/value pairs each on a separate line.  The second"
       "optional argument determines how many key/value pairs to display."
       "(If this value exceeds the number of entries in the hash table,"
       "all the entries are displayed.")))
  (:parameters
   (hash-table :docstring "A hash table." :value-type hash-table)
   (n :docstring "The number of key/value pairs to print out." 
      :value-type integer 
      :parameter-type &optional
      :default-value 10))
  (:see-also hash-table-contents hash-table-keys hash-table-values
   ))

(document-function check-for-duplicates
  (:summary "Returns a list of duplicates found in LIST.")
  (:returns "Duplicates found in LIST" :type list)
  (:examples
   "(check-for-duplicates '(1 1 2 3 1 3)) --> (3 1 1)"
   "(check-for-duplicates '((1 2) (2 3) (1 5)) :key 'first) --> (1)"
   "(check-for-duplicates '(\"ab\" \"cd\" \"AB\") :test 'string-equal) -->"
   "   (\"ab\")")
  (:text
   (:p 
    #.(one-string-sp
       "Searches a list for duplicate elements and returns the keys"
       "associated with those elements.  If an element is in fact duplicated"
       "N times, then N-1 keys will be included in the returned list."
       "The keys are computed using the KEY argument, which should be"
       "a function designator.  If not provided, IDENTITY is used,"
       "meaning that the elements themselves in the list are the keys."
       "The TEST argument behaves as a normal Common Lisp :TEST argument."))
   (:p
    #.(one-string-sp
       "This function uses one of two algorithms to determine duplicates,"
       "depending on HASH-THRESHOLD and the length of the input list."
       "If HASH-THRESHOLD is less than the length of the input list,"
       "then a doubly-nested loop algorithm is used; otherwise"
       "a hash lookup algorithm is used.  Since the hash lookup algorithm"
       "takes time proportional to the length of the list, while the"
       "doubly-nested algorithm takes time proportional to the square of the"
       "length of the list, the hash algorithm is significantly faster for"
       "long lists.  The arbitrary default for HASH-THRESHOLD is 100.")))
  (:parameters
   (list :docstring "The input to be searched for duplicates"
         :value-type list)
   (key 
    :docstring
    #.(one-string-sp
       "A function used to extract a key value from each element of"
       "the input list.  These key values are used for both comparison"
       "and as elements of the return list.")
    :value-type (or function symbol)
    :parameter-type &key
    :default-value identity)
   (test :docstring "A standard Common Lisp :test argument."
         :value-type (or function symbol)
         :parameter-type &key 
         :default-value eql)
   (hash-threshold 
    :docstring
    #.(one-string-sp
       "An integer, compared to the length of the input list, to determine"
       "whether to use a hash lookup algorithm or not.")
    :value-type integer
    :parameter-type &key
    :default-value 100))
  (:see-also lisp:remove-duplicates purge-duplicates find-duplicates 
   set-equal lisp:union lisp:intersection lisp:set-difference)
  )

(document-function purge-duplicates
  (:summary "Returns a list of the non-duplicated elements of LIST.")
  (:returns "Non-duplicates found in LIST"
   :type list)
  (:examples
   "(purge-duplicates '(1 1 2 3 1 3)) --> (1 2 3)"
   "(purge-duplicates '((1 2) (2 3) (1 5)) :key 'first) --> ((1 2) (2 3))"
   "(purge-duplicates '(\"ab\" \"cd\" \"AB\") :test 'string-equal) --> "
   "   (\"ab\" \"cd\")"
   "(purge-duplicates (iota 5)) --> (0 1 2 3 4)")
  (:text
   (:p 
    #.(one-string-sp
       "Removes the duplicate elements from the input list.  The order of"
       "the elements remaining from the input list is not guaranteed."
       "The KEY and TEST arguments behave as normal Common Lisp :key and :test"
       "arguments."))
   (:p
    #.(one-string-sp
       "This function uses one of two algorithms to remove duplicates,"
       "depending on HASH-THRESHOLD and the length of the input list."
       "If HASH-THRESHOLD is less than the length of the input list,"
       "then a doubly-nested loop algorithm is used; otherwise"
       "a hash lookup algorithm is used.  Since the hash lookup algorithm"
       "takes time proportional to the length of the list, while the"
       "doubly-nested algorithm takes time proportional to the square of the"
       "length of the list, the hash algorithm is significantly faster for"
       "long lists.  The arbitrary default for HASH-THRESHOLD is 50"))
   (:p
    #.(one-string-sp
       "Note: This function is semantically equivalent to"
       "LISP:REMOVE-DUPLICATES.  However, in some implementations"
       "(notably ACL) the duplicate removal algorithm always takes time"
       "proportional to the square of the length of the list.  Therefore"
       "LISP:REMOVE-DUPLICATES is rendered useless for very long lists.")))
  (:parameters
   (list :docstring "The input to be stripped of duplicates"
         :value-type list)
   (key 
    :docstring
    "A standard Common Lisp :key argument."
    :value-type (or function symbol)
    :parameter-type &key
    :default-value identity)
   (test :docstring "A standard Common Lisp :test argument."
         :value-type (or function symbol)
         :parameter-type &key 
         :default-value eql)
   (hash-threshold 
    :docstring
    #.(one-string-sp
       "An integer, compared to the length of the input list, to determine"
       "whether to use a hash lookup algorithm or not.")
    :value-type integer
    :parameter-type &key
    :default-value 50))
  (:see-also lisp:remove-duplicates check-for-duplicates find-duplicates 
   set-equal lisp:union lisp:intersection lisp:set-difference)
  )

(document-function find-duplicates
  (:summary
   "Returns a list of duplicates found in LIST, in the original list order.")
  (:returns 
   "Duplicates found in LIST"
   :type list)
  (:examples
   "(find-duplicates '(\"a\" \"A\" \"B\" \"b\" \"c\") :test 'string-equal)"
   "  --> (\"A\" \"b\") ;; not (\"a\" \"b\") or (\"a\" \"B\")."
   "(find-duplicates '(1 2 1 3 1 2) :return-exactly-one-duplicate? t) --> (1 2)"
   "(find-duplicates '(1 2 1 3 1 2)) --> (1 1 2)"
   "(find-duplicates (iota 5)) --> NIL")
  (:text
   (:p 
    #.(one-string-sp
       "Searches a list for duplicate elements and returns the duplicated"
       "elements.  If an element is in fact duplicated N times, then N-1"
       "of those elements will be in the returned list if the value of the"
       "keyword RETURN-EXACTLY-ONE-DUPLICATE? is NIL, or once if said keyword"
       "value is T.  The duplicate elements are returned in the same order"
       "that they appear in the input list."))
   (:p 
    #.(one-string-sp
       "The KEY and TEST arguments operate as standard Common Lisp"
       ":KEY and :TEST arguments."))
   (:p
    #.(one-string-sp
       "This function uses one of two algorithms to determine duplicates,"
       "depending on HASH-THRESHOLD and the length of the input list."
       "If HASH-THRESHOLD is less than the length of the input list,"
       "then a doubly-nested loop algorithm is used; otherwise"
       "a hash lookup algorithm is used.  Since the hash lookup algorithm"
       "takes time proportional to the length of the list, while the"
       "doubly-nested algorithm takes time proportional to the square of the"
       "length of the list, the hash algorithm is significantly faster for"
       "long lists.  The arbitrary default for HASH-THRESHOLD is 50.")))
  (:parameters
   (list :docstring "The input to be searched for duplicates"
         :value-type list)
   (key 
    :docstring "A standard Common Lisp :key argument."
    :value-type (or function symbol)
    :parameter-type &key
    :default-value identity)
   (test :docstring "A standard Common Lisp :test argument."
         :value-type (or function symbol)
         :parameter-type &key 
         :default-value eql)
   (hash-threshold 
    :docstring
    #.(one-string-sp
       "An integer, compared to the length of the input list, to determine"
       "whether to use a hash lookup algorithm or not.")
    :value-type integer
    :parameter-type &key
    :default-value 50))
  (:see-also lisp:remove-duplicates purge-duplicates check-for-duplicates
   set-equal lisp:union lisp:intersection lisp:set-difference)
  )


(document-function set-equal
  (:summary "Determines whether two sequences contain the same elements.")
  (:returns 
   #.(one-string-sp
      "True if the two sequences contain the same elements without respect"
      "to their order, false otherwise")
   :type boolean)
  (:examples
   "(set-equal '(1 2 3) '(2 3 1)) --> t"
   "(set-equal #(a b c d) '(d b a c)) --> t"
   "(set-equal '(1 2 3) '(1 2 3 3)) --> nil"
   "(set-equal \"abcdefg\" \"AbCdEfG\" :test 'char-equal) --> t"
   "(set-equal nil \"\") --> t"
   #.(one-string-sp
      "(set-equal (list (list 1 2) (list 4 5)) (list (list 1 2) (list 4 5)))"
      "--> nil ;; to be T, the :test argument would need to be 'equal"
      ))
  (:text
   (:p 
    #.(one-string-sp
       "Determines if two sequences treated as sets contain the same"
       "elements, regardless of the order of the elements in the sequences."
       "In order to be SET-EQUAL, the two sequences must be the same length,"
       "and each element in the second sequence must be the same as some"
       "element in the first sequence according to TEST."
       "If some of the elements in either sequence are duplicates within"
       "said sequence the results are undefined -- SET-EQUAL is only"
       "guaranteed to be correct if the sequences are indeed each sets"
       "(that is, there are no duplicates with respect to TEST).")) 
   (:p 
    #.(one-string-sp
       "TEST argument operates as a standard Common Lisp"
       ":TEST argument."))
   (:p
    #.(one-string-sp
       "This function uses one of two algorithms to determine set equality,"
       "depending on HASH-THRESHOLD and the lengths of the input sequences."
       "If HASH-THRESHOLD is less than the product of the lengths of the"
       "input sequences, then a doubly-nested loop algorithm is used; otherwise"
       "a hash lookup algorithm is used.  Since the hash lookup algorithm"
       "takes time proportional to the sum of the lengths of the sequences,"
       "while the doubly-nested algorithm takes time proportional to the"
       "product of the lengths of the sequences, the hash algorithm is"
       "significantly faster for long sequences.  The arbitrary default"
       "for HASH-THRESHOLD is 256.")))
  (:parameters
   (seq1 :docstring "An input sequence to be compared with the other one."
         :value-type sequence)
   (seq2 :docstring "An input sequence to be compared with the other one."
         :value-type sequence)
   (test :docstring "A standard Common Lisp :test argument."
         :value-type (or function symbol)
         :parameter-type &key 
         :default-value eql)
   (hash-threshold 
    :docstring
    #.(one-string-sp
       "An integer, compared to the product of the lengths of the input"
       "sequences, to determine whether to use a hash lookup algorithm or not.")
    :value-type integer
    :parameter-type &key
    :default-value 256))
  (:see-also lisp:union lisp:intersection lisp:set-difference
   lisp:equal lisp:equalp lisp:every)
  )

(document-function binsearch
  (:summary
   "Locates an item in an ordered vector using a binary search algorithm.")
  (:returns 
   #.(one-string-sp
      "The index of ITEM found in VECTOR, or NIL if ITEM cannot be found.")
   :type (or integer null) :display-type nil)
  (:examples
   ((binsearch 10 #(1 2 8 10 15 16 20)) 3)
   ((binsearch 10 #(1 2 8 11 15 16 20)) NIL)
   ((binsearch 10 #(22 20 16 15 10 8 2) :test '>) 4)
   ((binsearch 10 #(1 2 8 11 15 16 20) :if-not-found :range)
    (:values nil 2 3) 
    "(Returns 3 values)")
   )
  (:text
   (:p 
    #.(one-string-sp
       "Determines the location of ITEM assuming that VECTOR is ordered"
       "(by default using '<, but specified by the keyword TEST)."
       "If the item is not found, NIL is returned, unless the keyword"
       "IF-NOT-FOUND is specified and has the value :RANGE, in which case"
       "NIL is returned as the first value while the indices of the"
       "elemnents in VECTOR immediately before and immediately after"
       "where ITEM would have been found are returned as the second and"
       "third values."))
   (:p 
    #.(one-string-sp
       "This function uses a binary search algorithm, which takes time"
       "proportional to the log (base 2) of the length of VECTOR."
       ))
   (:p
    #.(one-string-sp
       "This function takes KEY, TEST, START, and END keywords with the usual"
       "Common Lisp semantics."))
   (:p
    #.(one-string-sp
       "BINSEARCH also takes a keyword TEST-TYPE, which can have the values"
       ":TWO-WAY or :THREE-WAY.  :TWO-WAY is the default and specifies that"
       "the TEST function returns either non-NIL or NIL; :THREE-WAY"
       "specifies that they function returns either -1, 0, or 1, depending on"
       "whether the two values being compared are less than, equal to, or"
       "greater than, respectively, to each other."))
   (:p
    #.(one-string-sp
       "Note: Each contiguous sequence in the Biobike system has its"
       "genes sorted by position and stored in order in the slot"
       "#$genes-sorted-by-position.  BINSEARCH is used by various Biobike"
       "utilities on this data (e.g., GENE-UPSTREAM-FROM)."))
   )
  (:parameters
   (item :docstring "The thing to be searched for in VECTOR."
         :value-type t)
   (vector :docstring "An ordered vector, through which ITEM is searched for." 
           :value-type vector)
   (test :docstring 
         #.(one-string-sp
            "A standard Common Lisp :test argument, if TEST-TYPE is :TWO-WAY;"
            "otherwise if TEST-TYPE is :THREE-WAY, a function of two arguments"
            "which returns -1, 0, or 1.")
         :value-type (or function symbol)
         :parameter-type &key 
         :default-value <)
   (test-type :docstring
              #.(one-string-sp
                 "Either :TWO-WAY, if TEST returns T or NIL, or :THREE-WAY,"
                 "if TEST returns -1, 0, or 1.")
              :value-type keyword
              :parameter-type &key 
              :default-value :two-way)
   (key 
    :docstring "A standard Common Lisp key argument."
    :value-type (or function symbol)
    :parameter-type &key
    :default-value identity)
   (start
    :docstring "A standard Common Lisp start argument."
    :value-type integer
    :parameter-type &key 
    :default-value 0)
   (start
    :docstring "A standard Common Lisp end argument."
    :value-type integer
    :parameter-type &key 
    :default-value nil)
   (if-not-found
    :docstring 
    #.(one-string-sp
       "Determines what is returned if ITEM is not found in VECTOR."
       "If anything other than :range or :start-end, NIL is returned."
       "If IF-NOT-FOUND is the value :RANGE or :START-END then the 2nd and 3rd"
       "values returned if the ITEM is not found are the indices of the"
       "elements between which ITEM would go were it inserted into VECTOR."
       "If ITEM would go before the first element then the values NIL NIL 0"
       "are returned, while if ITEM would go after the last element then the"
       "values NIL <LASTINDEX> NIL are returned.")
    :value-type t
    :parameter-type &key 
    :default-value nil))
  (:see-also lisp:gethash lisp:position lisp:find lisp:search)
  )

(document-function group-by-type
  (:summary 
   "Creates a histogram-like structure for the elements of the input list.")
  (:returns 
   "A list of lists, each sublist of the form (count key elements)"
   :type list :display-type nil)
  (:examples
   "(group-by-type #(a b c b b a)) --> ((3 b (b b b)) (2 a (a a)) (1 c (c)))"
   #.(one-string-sp
      "(group-by-type '((a 1) (a 2) (b 6)) :key 'first)"
      "--> ((2 a ((a 2) (a 1))) (1 b ((b 6))))")
   )
  (:text
   (:p 
    #.(one-string-sp
       "GROUP-BY-TYPE separates a sequence into equivalence classes and"
       "keeps a count of the number of elements in each equivalence class"
       "and all the elements in each equivalence class.  The equivalence"
       "classes are determined by TEST and KEY.  The result is the set"
       "of equivalence classes, in the form of a list of lists, each sublist"
       "being an equivalence class of the form (count key elements)."))
   (:p 
    #.(one-string-sp
       "The TEST argument must be suitable to be passed as the :test"
       "argument to MAKE-HASH-TABLE, because the function uses a hash table"
       "lookup to calculate the equivalence classes.  (The functions"
       "STRING= and STRING-EQUAL are also acceptable; they are"
       "converted to EQUAL and EQUALP, respectively.)"))
   (:p 
    #.(one-string-sp
       "By default the resulting sublists are sorted by count."
       "If the keyword SUBLISTS? is NIL, then the elements of the equivalence"
       "classes are not returned, only the keys (this makes GROUP-BY-TYPE"
       "almost equivalent to FREQSORT).")))
  (:parameters
   (sequence :docstring
             "The input sequence to be separated into equivalence classes."
             :value-type sequence)
  
   (key
    :docstring "A standard Common Lisp key argument."
    :value-type (or function symbol)
    :parameter-type &key 
    :default-value identity)
   (test :docstring
         "A standard Common Lisp :test argument (but see description)."
         :value-type (or function symbol)
         :parameter-type &key 
         :default-value equal)
   (sort? :docstring "Whether the result is sorted by count or not."
          :value-type boolean
          :parameter-type &key 
          :default-value t)
   (sublists? :docstring 
              "Whether the elements of the equivalence classes are returned."
              :value-type boolean
              :parameter-type &key 
              :default-value t))
  (:see-also freqsort lisp:sort lisp:count separate-into-lists
   character-counts)
  )


(document-function freqsort
  (:summary 
   "Creates a histogram-like structure for the elements of the input list.")
  (:returns 
   "A list of lists, each sublist of the form (count element)"
   :type list :display-type nil)
  (:examples
   "(freqsort '(a b c b b a)) --> ((3 b) (2 a) (1 c))"
   #.(one-string-sp
      "(freqsort '(a b c b b a) :sort-direction :ascending)"
      "   --> ((1 c) (2 a) (3 b))")
   "(freqsort '(\"ab\" \"AB\" \"ab\")) --> ((2 \"ab\") (1 \"AB\"))"
   "(freqsort '(\"ab\" \"AB\" \"ab\") :test 'string-equal) --> ((3 \"ab\"))"
   )
  (:text
   (:p 
    #.(one-string-sp
       "FREQSORT accepts a list with possibly duplicated elements,"
       "and creates a histogram of the number of times each element"
       "occurs in the input list.  The histogram is of the form of"
       "a list of lists, each sublist of the form (count element)"
       "The elements in the returned list are sorted by the count,"
       "by default in descending order."))
   (:p 
    #.(one-string-sp
       "As illustrated in the examples, if two or more objects are not"
       "identical but satisfy TEST, only the first object is present in the"
       "histogram."))
   (:p 
    #.(one-string-sp
       "The TEST argument must be suitable to be passed as the :test"
       "argument to MAKE-HASH-TABLE, because the function uses a hash table"
       "lookup to calculate the histogram.  (The functions STRING= and"
       "STRING-EQUAL are also acceptable; they are converted to EQUAL and"
       "EQUALP, respectively.)")))
  (:parameters
   (list :docstring "The input list which is histogrammed."
         :value-type list)
   (test :docstring
         "A standard Common Lisp :test argument (but see description)."
         :value-type (or function symbol)
         :parameter-type &key 
         :default-value equal)
   (sort-direction
    :docstring "Either :ascending or :descending"
    :value-type keyword
    :parameter-type &key 
    :default-value :descending)
   )
  (:see-also group-by-type lisp:sort lisp:count separate-into-lists
   character-counts frequency-count)
  )

   

(document-module filepath-utils
  "Functions dealing with files and directories"
  (:keywords :files :directories)
  (:display-modes :biolisp)
  #.`(:functions ,@*utility-filepath-user-symbols*))


(document-function c/l
  (:summary 
   "Loads a file and, if appropriate, compiles the file before loading it.")
  (:returns 
   #.(one-string-sp
      "What LISP:LOAD returns, if the file exists (implementation dependent);"
      "otherwise an error is signalled.")
   :type t)
  (:examples
   #.(one-string-sp
      "(c/l \"foo.lisp\") --> ;; compile/loads the foo.lisp file"
      "in the user's home directory.  A binary file, foo.fasl, may"
      "be created.")
   #.(one-string-sp
      "(c/l \"/home/BioDemo/users/Fred/bar.lisp\") --> ;; compile/loads"
      "the bar.lisp file in the user Fred's directory (assuming users' home"
      "directories are stored in /home/BioDemo/users/).")
   )
  (:text
   (:p 
    #.(one-string-sp
       "If the file named does not exist, an error is signaled."
       "If an equivalent binary file (a file with the same name"
       "but with a binary extension such as .fasl or .fsl) exists"
       "and was last written after the input file was last written, the binary"
       "is loaded using LISP:LOAD; otherwise the source file is compiled into"
       "a binary file using LISP:COMPILE-FILE and then the binary is loaded."
       ))
   (:p 
    #.(one-string-sp
       "The RECOMPILE? optional argument can be used to force recompilation"
       "(with a value T).  If the argument is NIL or not provided at all,"
       "then recompilation depends on the above algorithm."
       ))
   (:p
    #.(one-string-sp
       "Note: the source file itself is never loaded using C/L.  If"
       "compilation is attempted and fails the C/L command will fail."))
   (:p
    #.(one-string-sp
       "Note: If the source pathname has no extension ('foo' vs 'foo.lisp')"
       "a source file with a .lisp extension is searched for; therefore it"
       "is not possible to use C/L on source files without some kind of"
       "extension.")))
  (:parameters
   (file :docstring "The input file which is compile/loaded."
         :value-type (or string pathname))
   (recompile?
    :docstring "Whether to force recompilation."
    :value-type boolean
    :parameter-type &optional 
    :default-value nil)
   )
  (:see-also lisp:load lisp:compile-file lisp:compile "biobike:bbload")
  )

(document-function make-new-temp-file-path
  (:summary 
   "Creates a full filename, generally unique, to be used as a temporary file")
  (:returns 
   #.(one-string-sp
      "Two values: a full pathname to a file in DIRECTORY, and whether that"
      "file exists or not")
   :type (values pathname boolean) :display-type nil)
  (:examples
   "(make-new-temp-file-path \"C:/foo/\") --> #P\"C:/foo/T13-temp.temp\" NIL"
   #.(one-string-sp
      "(make-new-temp-file-path \"C:/foo/\" :name :fred) -->"
      "#P\"C:/foo/T11-FRED.temp\" NIL")
   #.(one-string-sp
      "(make-new-temp-file-path \"C:/foo/\" :type \"txt\" :prefix :x) -->"
      "#P\"C:/foo/X12-temp.txt\" NIL")
   )
  (:text
   (:p 
    #.(one-string-sp
       "Returns a full pathname to a file in DIRECTORY whose"
       "FILE-NAMESTRING is of the form <prefix><unique-id>-<name>.<type>."))
   (:p
    "If PREFIX is NIL then no <prefix>, <unique-id> or '-' is used."
    (:br)
    "If PREFIX is a symbol its symbol-name is used as <prefix>."
    (:br)
    "If PREFIX is a string it itself is used as <prefix>."
    (:br)
    "if NAME is NIL then no <name> or '-' is used."
    (:br)
    "If NAME is a symbol its symbol-name is used, else it must be a string."
    (:br)
    "If TYPE is NIL then no <type> or '.' is used."
    (:br)
    "If TYPE is a symbol its symbol-name is used, else it must be a string."
    (:br)
    "<unique-id> is unique with respect to the current process,"
    "therefore it is possible that a file of the name generated might exist,"
    "having been created by some other program or a differenct instance of"
    "whatever program is using this function."
    (:br)
    "In this case IF-EXISTS is examined -- it can be one of: "
    (:br)
    "  -- NIL (return the path anyway silently)"
    (:br)
    "  -- :WARN (return the path anyway after calling WARN to note duplicate)"
    (:br)
    "  -- :ERROR (signal an error, do not return)"
    (:br)
    "  -- :AGAIN (the default, call this function again with the same"
    "     arguments , thereby generating a new <unique-id>, which eventually"
    "     must result in a non-existing filename being generated.)")
   (:p
    #.(one-string-sp
       "The second value returned is whether the file designated by the"
       "returned pathname currently exists.")))
  (:parameters
   (directory 
    :docstring "A directory pathname designator"
    :value-type (or pathname string))
   (prefix
    :docstring "A string designator prepended to the name of the output file."
    :value-type string-designator
    :parameter-type &key
    :default-value "T")
   (name
    :docstring 
    "A string designator used as the name component of the output file"
    :value-type string-designator
    :parameter-type &key
    :default-value "temp")
   (type
    :docstring 
    "A string designator used as the type component of the output file"
    :value-type string-designator
    :parameter-type &key
    :default-value "temp")
   (if-exists
    :docstring 
    "Determines behavior if file exists; see description for details."
    :value-type keyword
    :parameter-type &key
    :default-value :again)
   )
  (:see-also lisp:make-pathname with-temp-file-in with-temp-directory
   lisp:gensym lisp:gentemp)
  )

(document-function with-temp-file-in
  (:summary 
   #.(one-string-sp
      "Executes a body after creating a temporary file, and then makes sure"
      "the temporary file is deleted.  (A macro)"))
  (:returns "What the body of the form returns" :type t)
  (:examples
   #.(one-string-sp
      "(with-temp-file-in (f \"C:/lispcode/\") body-forms) --> ;; executes"
      "BODY-FORMS with the variable F bound to a pathname representing"
      "a temporary file in the C:/lispcode/ directory.")
   #.(one-string-sp
      "(with-temp-file-in (f \"C:/lispcode/\" :delete? nil) body-forms) -->"
      ";; executes BODY-FORMS with the variable F bound to a pathname"
      "representing a file in the C:/lispcode/ directory; the file, if"
      "in fact created, is not deleted upon exit of WITH-TEMP-FILE-IN."))
  (:text
   (:p 
    #.(one-string-sp
       "Creates a pathname representing a temporary file in DIRECTORY "
       "using (MAKE-NEW-TEMP-FILE-PATH ...), binds that pathname to VAR,"
       "and insures that if the execution of BODY has in fact"
       "created a file named by said pathname that DELETE-FILE"
       "is called on the pathname if DELETE? is non-NIL."))
   (:p
    #.(one-string-sp
       "If DELETE-FILE is called, it might fail.  The action taken"
       "if so depends on the keyword variable ON-DELETE-ERROR.  By default,"
       "a warning is issued (value :warn), while if the value is :error"
       "an error is signaled."))
   (:p
    #.(one-string-sp
       "Note: many of the keyword arguments get passed through to"
       "MAKE-NEW-TEMP-FILE-PATH.  These arguments determine the exact"
       "name of the temporary file."))
   (:p
    #.(one-string-sp
       "Note: No file is actually created by WITH-TEMP-FILE-IN; only a"
       "filename is generated.  It is up to the user to actually create"
       "the file in the body of the macro.  If no file is created,"
       "the code will not attempt to delete it.")))
  (:parameters
   (var 
    :docstring "A variable bound to the pathname created."
    :value-type symbol)
   (directory
    :docstring 
    #.(one-string-sp
       "The directory used as the directory component of the pathname"
       "created (recall that no file is actually created).")
    :value-type (or pathname string))
   (delete? 
    :docstring 
    #.(one-string-sp
       "Determines whether any attempt is made to delete the file denoted by"
       "the pathname created.")
    :value-type boolean
    :parameter-type &key
    :default-value t)
   (on-delete-error
    :docstring 
    "Determines what happens if LISP:DELETE-FILE fails; either :warn or :error"
    :value-type keyword 
    :parameter-type &key
    :default-value :warn)
   (prefix  
    :docstring "A string designator prepended to the name of the pathname."
    :value-type string-designator
    :parameter-type &key
    :default-value nil)
   (name
    :docstring 
    "A string designator used as the name component of the pathname"
    :value-type string-designator
    :parameter-type &key
    :default-value nil)
   (type
    :docstring 
    "A string designator used as the type component of the pathname"
    :value-type string-designator
    :parameter-type &key
    :default-value nil)
   (if-exists
    :docstring 
    "Determines behavior if file exists; see MAKE-NEW-TEMP-FILE-PATH."
    :value-type keyword
    :parameter-type &key
    :default-value nil)
   )
  (:see-also lisp:with-open-file lisp:directory with-temp-directory 
   lisp:make-pathname)
  )

(document-function with-temp-directory
  (:summary 
   #.(one-string-sp
      "Executes a body after creating a temporary directory, and then"
      "makes sure the temporary directory and all its contents is deleted."
      "(A macro)"))
  (:returns "What the body of the form returns" :type t)
  (:examples
   #.(one-string-sp
      "(with-temp-directory (f \"/home/massar/\") body-forms) --> ;; executes"
      "BODY-FORMS with the variable F bound to a pathname representing"
      "a temporary subdirectory in the /home/massar/ directory.")
   #.(one-string-sp
      "(with-temp-directory (f \"/home/massar/\" :delete? nil) body-forms) -->"
      ";; executes BODY-FORMS with the variable F bound to a pathname"
      "representing a subdirectory of /home/massar/; the directory"
      "is not deleted upon exit of WITH-TEMP-DIRECTORY."))
  (:text
   (:p 
    #.(one-string-sp
       "Creates a directory in which to create temporary files."
       "You provide the parent directory (where this one will be created)"
       "and the new directory is bound to VAR as a pathname, which can be"
       "passed on to, for example, WITH-TEMP-FILE-IN, to create temp files."
       "Most key args are as in WITH-TEMP-FILE-IN.  If DELETE? is non-NIL,"
       "before WITH-TEMP-DIRECTORY returns, it attempts to insure that"
       "the created subdirectory and any files within it are deleted"))
   (:p
    #.(one-string-sp
       "If an attempt is made to delete the created directory, the attempt"
       "may fail.  The action taken if so depends on the keyword variable"
       "ON-DELETE-ERROR.  By default, a warning is issued (value :warn),"
       "while if the value is :error an error is signaled."))
   (:p
    #.(one-string-sp
       "Note: many of the keyword arguments get passed through to"
       "MAKE-NEW-TEMP-FILE-PATH.  These arguments determine the exact"
       "name of the temporary directory."))
   (:p
    #.(one-string-sp
       "Note: The deletion of the temporary directory only happens on"
       "a Unix-based operating system; it doesn't work on Windows.")))
       
  (:parameters
   (var 
    :docstring "A variable bound to the pathname created."
    :value-type symbol)
   (parent-directory
    :docstring 
    #.(one-string-sp
       "The directory pathname used as the root directory under which a"
       "subdirectory is created.")
    :value-type (or pathname string))
   (delete? 
    :docstring 
    #.(one-string-sp
       "Determines whether any attempt is made to delete the subdirectory"
       "created.")
    :value-type boolean
    :parameter-type &key
    :default-value t)
   (on-delete-error
    :docstring 
    #.(one-string-sp
       "Determines what happens if the attempt to delete the subirectory"
       "fails; either :warn or :error")
    :value-type keyword 
    :parameter-type &key
    :default-value :warn)
   (prefix  
    :docstring "A string designator prepended to the name of the subdirectory."
    :value-type string-designator
    :parameter-type &key
    :default-value nil)
   (name
    :docstring 
    "A string designator used as the name of the subdirectory."
    :value-type string-designator
    :parameter-type &key
    :default-value nil)
   (if-exists
    :docstring 
    "Determines behavior if subdirectory exists; see MAKE-NEW-TEMP-FILE-PATH."
    :value-type keyword
    :parameter-type &key
    :default-value nil)
   )
  (:see-also lisp:with-open-file lisp:directory with-temp-file-in 
   lisp:make-pathname lisp:ensure-directories-exist)
  )

(document-function file-to-string
  (:summary "Reads a file and creates a single string out of its characters.")
  (:returns "The characters of the file" :type string)
  (:examples
   #.(one-string-sp
      "(file-to-string \"/home/BioLingua/massar/foo.txt\") --> ;; a string"
      "containing the characters of foo.txt")
   "(file-to-string \"averylargefile.txt\" :max 1000) --> NIL")
  (:text
   (:p 
    #.(one-string-sp
       "Returns a string containing all the characters in FILE with line"
       "terminators converted to Newlines.  If the string would exceed MAX"
       "characters (default a million) a warning is issued and NIL is returned."
       )))
  (:parameters
   (file :docstring "The file whose characters are read." 
         :value-type (or string pathname))
   (max :docstring "The maximum file size allowed." 
        :value-type integer
        :parameter-type &key
        :default-value 1000000))
  (:see-also lisp:with-open-file lisp:read-sequence 
   file-to-string-list list-file-contents)
  )

(document-function file-to-string-list
  (:summary "Reads a file and creates a list of lines in that file.")
  (:returns "Each line of the file as a string," :type list)
  (:examples
   #.(one-string-sp
      "(file-to-string-list \"foo.txt\") -->"
      "  (\"This is the first line of foo.txt\""
      "   \"This is the second line of foo.txt\")"
      "  ;; (Assuming foo.txt has exactly two lines...)"))
  (:text
   (:p 
    #.(one-string-sp
       "Returns a list of strings, each string representing a line from FILE."
       "(Newline characters and EOF in the file determine where each line ends)"
       )))
  (:parameters
   (file :docstring "The file whose lines are read into strings"
         :value-type (or string pathname))
   )
  (:see-also lisp:with-open-file lisp:read-sequence 
   file-to-string list-file-contents)
  )

(document-function list-file-contents
  (:summary "Reads a file and prints it out.")
  (:returns nil :type t)
  (:examples
   #.(one-string-sp
      "(list-file-contents \"foo.txt\" :line-limit 10) --> ;; prints out"
      "up to the first 10 lines of foo.txt")
   #.(one-string-sp
      "(list-file-contents \"foo.txt\") --> ;; prints out"
      "up to the first 100 (the default) lines of foo.txt")
   #.(one-string-sp
      "(list-file-contents \"foo.txt\" :line-limit nil) --> ;; prints out"
      "all the lines of foo.txt"))
  (:text
   (:p 
    #.(one-string-sp
       "Prints out the first N lines of a file, depending on the keyword"
       "argument LINE-LIMIT.  By default, up to 100 lines of the file"
       "are shown (if the file has less than 100 lines, all the lines"
       "are printed).  If LINE-LIMIT is NIL, all the lines of the file"
       "are printed regardless of how many.  If LINE-LIMIT is a specific"
       "value then up to that many lines are shown."))
   (:p 
    #.(one-string-sp
       "Note: When used from the weblistener, the weblistener has its own"
       "limit on how many lines it will print out before bailing; the"
       "function SET-OUTPUT-LIMITS allows you to specify such a limit."
       "The actual number of lines printed will be the minimum of"
       "the value shown by SHOW-OUTPUT-LIMITS and LINE-LIMIT."
       )))
  (:parameters
   (file :docstring "The file whose lines are displayed."
         :value-type (or string pathname))
   )
  (:see-also lisp:with-open-file file-to-string file-to-string-list 
   "wb:set-output-limits" "wb:show-output-limits" lisp:*print-lines*)
  )

(document-function file-operate
  (:summary 
   #.(one-string-sp
      "Calls a function on each file in a directory and possibly"
      "its subdirectories"))
  (:returns 
   #.(one-string-sp
      "A list of lists consisting of the filepath and the result of"
      "the function call on that filepath.")
   :type list :display-type nil)
  (:examples
   #.(one-string-sp
      "(file-operate \"/home/massar/\"" 
      "(lambda (x) (equalp (pathname-type x) \"txt\")) 'null) -->"
      "((#P\"/home/massar/foo.txt\" nil) (#P\"/home/massar/bar.txt\" nil))"
      ";; (returns all the files of type .txt in the directory)"))
  (:text
   (:p
    #.(one-string-sp
       "Find all the file pathnames in DIRECTORIES that satisfy PREDICATE and"
       "call OPERATION on that file pathname."))
   (:p
    #.(one-string-sp
       "If ROOT is non-nil it is assumed to be a full pathname specifying a"
       "directory, and DIRECTORIES are assumed to be relative pathnames;"
       "ROOT is then merged with each of DIRECTORIES using MERGE-PATHNAMES"
       "to create a full directory pathname."))
   (:p
    #.(one-string-sp
       "If RECURSIVE? is non-NIL any subdirectories found in DIRECTORIES"
       "are searched recursively for files satisfying PREDICATE, etc."))
   (:p
    #.(one-string-sp
       "FILE-OPERATE returns a list of two element lists: the first element of"
       "the list being a file satisfying PREDICATE (and therefore operated on),"
       "and the second being the result of OPERATION on the file. The files"
       "are returned in the order they are found (the search being depth-first,"
       "if it is specified recursive, with files in a directory operated on"
       "before the subdirectories are search."))
   )
  (:parameters
   (directories 
    :docstring
    #.(one-string-sp
       "The directories (either relative or absolute) that will be"
       "searched to find files satisfying PREDICATE.")
    :value-type list)
   (predicate
    :docstring
    #.(one-string-sp
       "A function of one argument applied to the filepath of each file found"
       "which returns NIL if the file is to be ignored, or non-NIL otherwise.")
    :value-type (or function symbol))
   (operation
    :docstring
    #.(one-string-sp
       "A function of one argument applied to each filepath that satisfies"
       "PREDICATE.  The value of this function application is returned"
       "as the second element of the component list whose first element is"
       "the filepath.")
    :value-type (or function symbol))
   (root
    :docstring
    #.(one-string-sp
       "Either NIL or an absolute pathname or string denoting a directory."
       "If NIL, the DIRECTORIES are taken to be absolute pathnames; if not,"
       "then each of the DIRECTORIES is merged with ROOT to form an absolute"
       "directory which is searched.")
    :value-type (or null pathname string)
    :parameter-type &key 
    :default-value nil)
   (recursive?
    :docstring
    #.(one-string-sp
       "If NIL, the search does not extend to subdirectories of the"
       "directories specifically listed; otherwise all subdirectories"
       "(recursively) are searched in depth-first order as well as the"
       "specified directories.")
    :value-type boolean
    :parameter-type &key 
    :default-value nil))
  (:see-also lisp:ensure-directories-exist lisp:directory lisp:merge-pathnames 
   maptree lisp:make-pathname) 
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(document-module symbol-utils
  #.(one-string-sp
     "Functions dealing with symbols.")
  (:keywords :utilities :symbols :packages)
  (:display-modes :biolisp)
  #.`(:functions ,@*utility-symbols-user-symbols*))


(document-function keywordize
  (:summary "Creates a keyword symbol from a string or symbol.")
  (:returns "A keyword" :type symbol :display-type nil)
  (:examples
   "(keywordize 'foo) --> :FOO"
   "(keywordize :fred) --> :FRED"
   "(keywordize \"abc\") --> :ABC"
   "(keywordize 'cl-user::frob) --> :FROB"
   "(keywordize 'xyz :lower) --> :|xyz|"
   "(keywordize \"zYx\" :preserve) --> :|zYx|"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Returns a keyword symbol whose characters are the characters"
       "of the first input argument, potentially modified by the CASE"
       "argument.  If CASE is :upper, the result is a normal keyword symbol."
       "If CASE is :lower, the result is a keyword symbol with lowercase"
       "characters (which will be printed using vertical bar escapes)."
       "If CASE is :preserve, the alphabetic characters will remain in"
       "whatever case they are."
       ))
   (:p 
    #.(one-string-sp
       "Note: in standard Common Lisp, symbols, while typed in in lowercase"
       "by convention, actually consist of uppercase characters."
       ))
   (:p 
    #.(one-string-sp
       "Note: This function is useful to canonicalize symbols, especially"
       "when a user may type a symbol into the reader in an unknown package."
       )))
  (:parameters
   (symbol :docstring "The symbol or string to be converted to a keyword"
           :value-type (or symbol string))
   (case :docstring 
     "How to treat the characters of SYMBOL: :upper, :lower, :preserve"
     :value-type keyword))
  (:see-also lisp:intern lisp:read-from-string lisp:find-symbol lisp:apropos
   symbol=)
  )
  

(document-function symbol=
  (:summary "Test whether two symbols have the same name")
  (:returns "T or NIL" :type boolean)
  (:examples
   "(symbol= 'lisp:car :car) --> T"
   "(symbol= 'lisp:car :|car|) --> nil"
   "(symbol= 'fred 'Fred) --> T"
   "(symbol= 'cl-user::symbol= 'utils::symbol=) --> T"
   "(symbol= 'fred 'freddy) --> NIL"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Returns T if both symbols contain the same characters."
       "Case is respected: two symbols, one with uppercase letters and one"
       "with lowercase letters are not symbol=."
       ))
   (:p 
    #.(one-string-sp
       "Note: This function is useful when, as with the Common Lisp LOOP"
       "macro, a user can type a 'keyword' in an arbitrary package."
       )))
  (:parameters
   (s1 :docstring "The first symbol"
       :value-type symbol) 
   (s2 :docstring "The second symbol"
       :value-type symbol))
  (:see-also keywordize lisp:equal lisp:equalp lisp:string-equal lisp:string=)
  )
   
(document-function package-external-symbols
  (:summary "Creates a list of the external symbols of a package.")
  (:returns "The external symbols of a package" :type list)
  (:examples
   "(package-external-symbols :cl) --> ;; all 978 common lisp exported symbols"
   )
  (:text
   (:p
    "Creates and returns a list of the external symbols of PACKAGE.")
   )
  (:parameters
   (package :docstring "The package in which external symbols are found."
            :value-type (or package symbol string)) 
   )
  (:see-also lisp:do-external-symbols lisp:export)
  )
   
(document-function package-and-symbol-string 
  (:summary "Creates a string with both package prefix and symbol name.")
  (:returns "The package prefix, colon(s), and the symbol name" :type string)
  (:examples
   "(package-and-symbol-string 'car) --> \"COMMON-LISP::CAR\")"
   "(package-and-symbol-string :fred) --> \":FRED\")"
   "(package-and-symbol-string (gensym)) --> ;; error"
   )
  (:text
   (:p
    #.(one-string-sp
    "Returns a string with the symbol's package prefix and the symbol's name."
    "If the symbol is a keyword symbol, the string consists of a single"
    "leading colon followed by the symbol name.  Otherwise the string"
    "consists of the package name followed by two colons, followed by the"
    "symbol name.  It is an error to call this function on an"
    "uninterned symbol.")))
  (:parameters
   (symbol :docstring "The symbol to be turned into a package/name string"
           :value-type symbol) 
   )
  (:see-also lisp:*package* lisp:format formatn lisp:symbol-package 
   lisp:symbol-name)
  )
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(document-module math-utils
  #.(one-string-sp
     "Utility functions dealing with math and statistics")
  (:keywords :utilities :math :mathematics :stats :statistics)
  (:display-modes :biolisp)
  #.`(:functions ,@*utility-math-user-symbols*))

(document-function fixnump 
  (:summary "Determines whether its argument is a small integer.")
  (:returns "T or NIL" :type boolean)
  (:examples
   "(fixnump 0) --> T"
   "(fixnump most-positive-fixnum) --> T"
   "(fixnump (1+ most-positive-fixnum)) --> NIL"
   "(fixnump most-negative-fixnum) --> T"
   "(fixnump (1- most-negative-fixnum)) --> NIL"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Returns T if the integer argument is of Common Lisp type fixnum."
       "(Lisp divides the integers into two sets -- fixnums and bignums."
       "Fixnums are generally those integers which can be manipulated"
       "by the underlying computer in a single machine instruction, and"
       "are therefore significantly faster to use.)"
       )))
  (:parameters
   (n :docstring "An integer." :value-type integer))
  (:see-also lisp:integerp lisp:typep lisp:type-of)
  )

(document-function mean
  (:summary "Determines the arithmetic mean of a sequence of numbers.")
  (:returns "The arithmetic mean" :type (or float complex))
  (:examples
   "(mean '(1 2 3)) --> 2.0"
   "(mean #(1 2 3)) --> 2.0"
   "(mean '(1 #c(1.0 1.0))) --> #C(1.0 0.5)"
   )
  (:text
   (:p 
    #.(one-string-sp
       "Returns the arithmetic mean of a sequence of numbers as a float"
       "(unless one or more of the inputs is a complex number, in which case"
       "the result is also complex.)  If the sequence is 0-length a"
       "division-by-zero error will ensue.")
    ))
  (:parameters
   (seq :docstring "A sequence of numbers." :value-type sequence))
  (:see-also lisp:reduce)
  )

(document-function factorial
  (:summary "Calculates factorial efficiently.")
  (:returns "x!" :type integer)
  (:examples
   ((factorial 5) 120) 
   ((factorial 0) 1)
   ((factorial 40) 815915283247897734345611269596115894272000000000)
   )
  (:text
   (:p 
    #.(one-string-sp
       "Calculates the factorial function for non-negative integers."
       "Uses an algorithm posted by Richard Fateman to comp.lang.lisp"
       "on April 8, 2006, which should be much faster than the standard"
       "recursive algorithm."
       )
    ))
  (:parameters
   (n :docstring "A non-negative integer" :value-type integer))
  (:see-also combinations-of lisp:*)
  )

(document-function combinations-of
  (:summary "Calculates (N choose K), or N!/(K!(N-K)!)")
  (:returns "The answer" :type integer)
  (:examples
   ((combinations-of 6 2) 15)
   ((combinations-of 200 10) 22451004309013280)
   ((combinations-of 1000000 1) 1000000)
   )
  (:text
   (:p 
    #.(one-string-sp
       "Calculates N choose K efficiently (without generating the factorials)."
       "(ie, the number of ways K elements can be chosen from a set of N"
       "elements without duplication.)"
       "N and K must be positive integers, N >= K."
       )
    ))
  (:parameters
   (n :docstring "The number of elements to be chosen from" :value-type integer)
   (k :docstring "The number of elements to choose" :value-type integer))
  (:see-also factorial)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(document-module misc-utils
  #.(one-string-sp
     "Miscellaneous utility functions that don't fit any other category.")
  (:keywords :utilities :miscellaneous)
  (:display-modes :biolisp)
  #.`(:functions ,@*utility-misc-user-symbols*))

(document-function make-timestamp-string
  (:summary "Returns a timestamp in one of a number of formats.")
  (:returns "A timestamp "  :type string)
  (:examples
   "(make-timestamp-string) --> \"03/11/06 16:11:41\" ;; the current time"
   #.(one-string-sp
      "(make-timestamp-string :universal-time 0) --> \"12/31/99 16:00:00\""
      ";; Pacific time, 8 hours ahead of Greenwich time" )
   #.(one-string-sp
      "(make-timestamp-string :mode :DDMMYYHHMMSS) --> \"11/03/06 16:17:29\""
      ";; The current date and time in European format.")
   )
  (:text
   (:p 
    "Return a timestamp based on MODE which looks like: " (:br)
    ":MMDDYYHHMMSS \"mm/dd/yy hh:mm:ss\"" (:br)
    ":YYMMDDHHMMSS \"yy/mm/dd hh:mm:ss\"" (:br)
    ":DDMMYYHHMMSS \"dd/mm/yy hh:mm:ss\"" (:br)
    ":MMDDYYHHMM \"mm/dd/yy hh:mm\"" (:br)
    ":HHMMSS \"hh:mm:ss\"" (:br)
    ":STDFULL \"yyyymmddhhmmss\"" (:br)
    ":STDYMD \"yyyymmdd\"" (:br)
    #.(one-string-sp
       "By default the current time is used.  If the keyword :universal-time"
       "is provided, its value is used instead."))
   (:p
    "Note: This function is useful for creating log file entries."))
  (:parameters
   (universal-time
    :docstring "The time to be converted."
    :value-type integer
    :parameter-type &key
    :default-value (get-universal-time)
    )
   (mode
    :docstring "The format the timestring is returned as."
    :value-type keyword
    :parameter-type &key
    :default-value :mmddyyhhmmss
    ))
  (:see-also lisp:get-universal-time lisp:decode-universal-time 
   lisp:encode-universal-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com.gigamonkeys.foo.html:define-html-macro foo-indent (n &rest stuff)
  `(:progn ,@(loop repeat n collect `(:character "nbsp")) ,@stuff))

;;; example document-function form with various kinds of examples

(document-function example-function
  (:summary "An example of different ways to use :examples.")
  (:returns nil :type boolean)

  (:examples

   ;; EXAMPLE 1

   ;; This is a standard example string.  Since it's a string no special
   ;; parsing or handling is done.  The output is as you see it without the
   ;; double quotes.  (Any characters special to HTML will have been
   ;; escaped.)

   "(bar 1) --> 2 ;; adds 1"

   ;; EXAMPLE 2
   
   ;; This is an example of :foo processing, using (:br) to produce
   ;; a newline, and a FOO macro to do indenting (definition of this
   ;; foo macro is above).

   ((:foo "(multiple-value-bind (x y) '(3 4) " (:br) 
     (foo-indent 2 "(+ x y 1))"))
    8 "An example of MULTIPLE-VALUE-BIND")
   
   ;; It produces something like:
   ;;
   ;; (multiple-value-bind (x y)
   ;;   (+ x y 1))
   ;; 
   ;; --> 8 ;; An example of MULTIPLE-VALUE-BIND

   ;; EXAMPLE 3
   
   ;; This is an example of processing the FORM as a single-string, and
   ;; multiple result values.  

   (#.(one-string-nli 
       "(let ((x 3)"
       "      (y 4))"
       "  (values (+ x y) (- x y))))")
    (:values 7 -1)
    "The sum and difference")

   ;; It produces something like:
   ;;
   ;;  (let ((x 3)
   ;;        (y 4))
   ;;    (values (+ x y) (- x y)))
   ;;  --> 7, -1 ;; The sum and the difference

   ;; EXAMPLE 4

   ;; This illustrates using a lisp expression as FORM, which gets processed
   ;; by the pretty-printer.  The comment gets printed out on a separate
   ;; line because the return value and the comment won't fit on a single line.
   ;; The return value gets printed out on a separate line because the FORM
   ;; is multi-lined when it comes out of the pretty-printer.  
   
   ((defun a-long-function-name (argument1 argument2 argument3)
      (* argument1 argument2 argument3))
    A-LONG-FUNCTION-NAME
    "This illustrates how to define a function with three arguments.")
   
   ;; It produces something like:
   ;; 
   ;; (defun a-long-function-name (argument1 argument2 argument3)
   ;;   (* argument1 argument2 argument3))
   ;; --> A-LONG-FUNCTION-NAME
   ;; This illustrates how to define a function with three arguments.

   ;; EXAMPLE 5

   ;; This illustrates using :string to insure that the result string is
   ;; shown quoted.  It also illustrates no comment field and that the
   ;; formatter will print the form and the results on a single line if they
   ;; will fit.  (One can always type "\"abbc\"" instead, but that becomes
   ;; quite hard to read.)

   ((one-string "ab" "bc") (:string "abbc"))

   ;; It produces something like: 
   ;; 
   ;; (one-string "ab" "bc") --> "abbc"

   ;; EXAMPLE 6
   
   ;; This illustrates the use of :nil to indicate that no 
   ;; result need be shown; all that is important is the side effect, 
   ;; which should be stated in the comment.  (Note that just NIL instead
   ;; of :NIL will be interpreted as the form returning NIL)

   ("(blow-up-world)" :nil "Causes world to end.")

   ;; It produces something like:
   ;;
   ;; (blow-up-world)  ;; Causes world to end.

   )

  (:text "This is some text.")
  (:parameters)
  )