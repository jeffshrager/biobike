;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: utils; -*-

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

;;; Author:  JP Massar.  Foo.  

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *utility-sequence-user-symbols*
    '(
      lastelem
      remove-all-of
      positions
      positions-if
      c+
      initial-subsequence-of?
      ordered? 
      select-sequence-elements
      frequency-count
      elements-in
      elements-not-in
      first-difference
      multiple-key-sort
      initial-subseq-or-all
      ))

  (defparameter *utility-sequence-api-symbols*
    (append *utility-sequence-user-symbols*
            '(
              collect-and-process-subsequences
              )))

  (export *utility-sequence-api-symbols* (find-package :utils)))

(defun lastelem (seq)
  "The last element of either a list or a vector."
  #.(optimization-declaration)
  (cond 
   ((listp seq) (first (last seq)))
   ((vectorp seq) 
    (let ((len (length seq)))
      (declare (fixnum len))
      (if (zerop len) nil (aref seq (the fixnum (1- len))))
      ))
   (t (error "Non-sequence ~A to LASTELEM" seq))
   ))



(defun remove-all-of (sequence elements &key (test 'eql) (key 'identity))
  #.(one-string-nl
     "Remove all occurrences of the items in ELEMENTS from SEQUENCE."
     "Returns a new sequence.  SEQUENCE itself is unchanged.")
  (remove-if 
   (lambda (x) (member (funcall key x) elements :test test))
   sequence
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun collect-and-process-subsequences
       (sequence begin-predicate end-predicate process-function
                 &key 
                 (exclusive? t)
                 (predicate-mode :unary))
  #.(one-string-nl
     "Find all start and end markers, i.e. the elements of SEQUENCE that"
     "satisfy BEGIN-PREDICATE and/or END-PREDICATE, respectively."
     "Then collect all the elements between each such set of begin and/or"
     "end markers."
     ""
     "If END-PREDICATE is NIL, the subsequences are collected between the"
     "start markers, ignoring any elements of SEQUENCE before the first start"
     "marker.  The last subsequence extends from the last start marker through"
     "the end of SEQUENCE."
     "If START-PREDICATE is NIL, the subsequences are collected between the"
     "end markers, ignoring any elements of SEQUENCe after the last end marker."
     "The first subsequence extends from the beginning of SEQUENCE through the"
     "first end marker."
     "If both START-PREDICATE and END-PREDICATE are non-nil, the subsequences"
     "are collected between the paired start and end markers.  The element"
     "determining each end marker is included or not in the subsequence"
     "depending on whether EXCLUSIVE? is non-NIL.  Elements before the first"
     "start marker, after the last end marker, and between and end marker and"
     "the next start marker are not collected.  If there is no end marker"
     "after a start marker the last subsequence is collected through the end"
     "of SEQUENCE.  Once a start marker is found, further start markers are"
     "ignored until an end marker is found.  Once an end marker is found,"
     "further end markers are ignored until another start marker is found."
     ""
     "Finally, PROCESS-FUNCTION is called on each collected subsequence and"
     "a list of the results is returned.  PROCESS-FUNCTION may be NIL, in"
     "which case it is equivalent to it having the value 'IDENTITY."
     ""
     "Note that EXCLUSIVE? is ignored unless both START-PREDICATE and"
     "END-PREDICATE are present."
     ""
     "If PREDICATE-MODE is :UNARY, the predicates accept a single argument,"
     "an element of the sequence."
     "If PREDICATE-MODE is :BINARY, the predicates accept two arguments,"
     "the SEQUENCE itself and the index of an element."
     "If PREDICATE-MODE is :TRINARY, the predicates accept three arguments,"
     "the SEQUENCE itself, an element, and the element's index."
     ""
     "Example:"
     "(collect-and-process-subsequences "
     "  \" turn this string   into  tokens \""
     "  (lambda (seq i) (and (not (eql (aref seq i) #\Space))"
     "                       (or (zerop i) (eql (aref seq (1- i)) #\Space))))"
     "  (lambda (seq i) (and (eql (aref seq i) #\Space)"
     "                       (not (zerop i))"
     "                       (not (eql (aref seq (1- i)) #\Space))))"
     "  'string-capitalize :mode :unary)"
     "(\"Turn\" \"This\" \"String\" \"Into\" \"Tokens\")")
  (unless (member predicate-mode '(:unary :binary :trinary))
    (error "Invalid predicate mode: ~A" predicate-mode))
  (cond
   ((listp sequence)
    (collect-and-process-sublists
     sequence begin-predicate end-predicate process-function 
     exclusive? predicate-mode))
   ((vectorp sequence)
    (collect-and-process-subvectors
     sequence begin-predicate end-predicate process-function 
     exclusive? predicate-mode))
   (t (error "Not a sequence: ~A" sequence))
   ))


(defun collect-and-process-sublists 
       (list begin-predicate end-predicate 
             process-function exclusive? predicate-mode)

  (labels ((collect-until (list end-cons-cell)
             (loop for sublist on list until (eq sublist end-cons-cell)
                   collect (first sublist)))
           (collect-through (list end-cons-cell)
             (if (null end-cons-cell)
                 (collect-until list nil)
               (let ((done? nil))
                 (loop for sublist on list as elem = (first sublist)
                       until done?
                       collect 
                       (prog1 elem (setq done? (eq sublist end-cons-cell)))
                       )))))

    (let* ((start-markers nil) (end-markers nil) (toggle :start))

      ;; Locate the start and/or end markers.
      (loop for rest on list for j fixnum from 0 do
            (flet ((fpredicate (p)
                     (ecase predicate-mode
                       (:unary (funcall p (first rest)))
                       (:binary (funcall p list j))
                       (:trinary (funcall p list (first rest) j))
                       )))
              (cond
               ((and begin-predicate end-predicate)
                (ecase toggle
                  (:start 
                   (when (fpredicate begin-predicate)
                     (push rest start-markers) (setq toggle :end)))
                  (:end
                   (when (fpredicate end-predicate)
                     (push rest end-markers) (setq toggle :start)))))
               (begin-predicate
                (when (fpredicate begin-predicate) (push rest start-markers)))
               (end-predicate
                (when (fpredicate end-predicate) (push rest end-markers))
                ))))
      (when start-markers (setq start-markers (nreverse start-markers)))
      (when end-markers (setq end-markers (nreverse end-markers)))

      ;; Collect the sublists as determined by the start and/or end markers.
      (let ((sublists
             (cond
              ((and begin-predicate end-predicate)
               (loop for sm on start-markers
                     ;; might not be a last end-marker, but we don't
                     ;; want iteration to terminate before last start-marker.
                     for em = end-markers then (cdr em)
                     as start-sublist = (first sm)
                     as end-sublist = (first em)
                     collect 
                     (if exclusive?
                         (collect-until start-sublist end-sublist)
                       (collect-through start-sublist end-sublist)
                       )))
              (begin-predicate
               (loop for markers on start-markers
                     as start-sublist = (first markers)
                     as end-sublist = (second markers)
                     collect (collect-until start-sublist end-sublist)
                     ))
              (end-predicate
               (loop for markers on (cons list end-markers)
                     for j fixnum from 0
                     as start-sublist = (first markers)
                     as end-sublist = (second markers)
                     when end-sublist 
                     collect 
                     (if (zerop j)
                         (collect-through start-sublist end-sublist)
                       (collect-through (cdr start-sublist) end-sublist)
                       ))))))

        ;; Call the process function on the collected sublists.
        (cond
         ((or (null process-function) 
              (eq process-function 'identity)
              (eq process-function #'identity))
          sublists)
         (t (mapcar process-function sublists))
         ))

      )))

(defun collect-and-process-subvectors
       (sequence begin-predicate end-predicate 
                 process-function exclusive? predicate-mode)
  (let* ((start-markers nil) (end-markers nil) (toggle :start))
    (loop for j fixnum from 0 below (length sequence) do
          (flet ((fpredicate (p)
                   (ecase predicate-mode
                     (:unary (funcall p (aref sequence j)))
                     (:binary (funcall p sequence j))
                     (:trinary (funcall p sequence (aref sequence j) j))
                     )))
            (cond
             ((and begin-predicate end-predicate)
              (ecase toggle
                (:start
                 (when (fpredicate begin-predicate)
                   (push j start-markers) (setq toggle :end)))
                (:end 
                 (when (fpredicate end-predicate)
                   (push j end-markers) (setq toggle :start)))))
             (begin-predicate
              (when (fpredicate begin-predicate) 
                (push j start-markers)))
             (end-predicate
              (when (fpredicate end-predicate)
                (push j end-markers)))
             (t (error "Must specifier BEGIN-PREDICATE or END-PREDICATE!"))
             )))
    (setq start-markers (nreverse start-markers))
    (setq end-markers (nreverse end-markers))
    ;; (print (list 'start start-markers 'end end-markers))
    (let ((subsequences
           (cond
            ((and begin-predicate end-predicate)
             (loop for start in start-markers 
                   ;; May not be an end marker for last start marker
                   ;; Don't want iteration to terminate prematurely
                   for end-list = end-markers then (cdr end-list)
                   as end = (first end-list)
                   collect
                   (if exclusive?
                       (subseq sequence start end)
                     (subseq sequence start (or (null end) (1+ end)))
                     )))
            (begin-predicate
             (loop for markers on start-markers
                   as start-index = (first markers)
                   as end-index = (second markers)
                   collect (subseq sequence start-index end-index)
                   ))
            (end-predicate
             (loop for markers on (cons -1 end-markers)
                   as previous-index = (first markers)
                   as start-index = (the fixnum (1+ previous-index))
                   as last-index = (second markers)
                   as end-index = (and (integerp last-index) (1+ last-index))
                   collect (subseq sequence start-index end-index)
                   )))))
      (cond
       ((or (null process-function) 
            (eq process-function 'identity)
            (eq process-function #'identity))
        subsequences)
       (t (mapcar process-function subsequences))
       ))))


(defun positions (item sequence &key (test 'eql) (start 0) (end nil) key)
  #.(one-string-nl
     "Returns a list of all positions in SEQUENCE where ITEM was detected"
     "(subject to the usual POSITION argument semantics and restrictions"
     "for TEST, KEY, START and END.  The POSITION arguments TEST-NOT and"
     "FROM-END are not allowed.")
  (loop with current-start = start
        for pos = (position item sequence 
                            :start current-start
                            :end end
                            :test test 
                            :key key
                            )
        until (null pos)
        do (setq current-start (1+ pos))
        collect pos
        ))

(defun positions-if (predicate sequence &key (start 0) (end nil) key)
  #.(one-string-nl
     "Returns a list of all positions in SEQUENCE where PREDICATE is satisfied"
     "(subject to the usual POSITION-IF argument semantics and restrictions for"
     "KEY, START and END.  The POSITION-IF argument FROM-END is not allowed.")
  (loop with current-start = start
        for pos = (position-if predicate sequence 
                               :start current-start
                               :end end
                               :key key
                               )
        until (null pos)
        do (setq current-start (1+ pos))
        collect pos
        ))

(defun c+ (&rest sequences)
  #.(one-string-nl
     "Concatenates all the SEQUENCES together into a single sequence."
     "If there are no sequences, NIL is returned."
     "If all the sequences are strings, a SIMPLE-STRING is returned."
     "If all the sequences are vectors, a SIMPLE-VECTOR is returned."
     "If any sequence is a list (including NIL), a list is returned."
     "Example:  (c+ \"abc\" #(1 2 3)) -> #(#\a #\b #\c 1 2 3)"
     "Example:  (c+ #(1 2 3) '(4 5 6)) -> '(1 2 3 4 5 6)")
  (cond
   ((null sequences) nil)
   ((every 'stringp sequences)
    (apply 'concatenate 'simple-string sequences))
   ((every 'vectorp sequences)
    (apply 'concatenate 'simple-vector sequences))
   (t
    (apply 'concatenate 'list sequences)
    )))


(defun initial-subsequence-of? 
       (sequence initial-sequence &key (element-test 'eql))
  #.(one-string-nl
     "Returns T if the elements of INITIAL-SEQUENCE begin SEQUENCE"
     "using ELEMENT-TEST as the comparison predicate."
     "This function is optimized for SIMPLE-STRING's and LISTS, and in"
     "any case should not create garbage.")
  (flet ((general-initial-subsequence-of? (s is)
           (block exit
             (map
              nil
              (lambda (x y) 
                (unless (funcall element-test x y) (return-from exit nil)))
              s is)
             t)))
    (and (>= (length sequence) (length initial-sequence))
         (cond
          ((and (simple-string-p sequence) (simple-string-p initial-sequence))
           (let ((s sequence) (is initial-sequence))
             (declare (simple-string s is))
             (cond
              ((or (eq element-test 'eql) (eq element-test #'eql)
                   (eq element-test 'char=) (eq element-test #'char=)
                   (eq element-test 'equal) (eq element-test #'equal))
               (loop for schar across s
                     for ischar across is do
                     (unless (char= schar ischar) (return nil))
                     finally (return t)
                     ))
              ((or (eq element-test 'char-equal) (eq element-test #'char-equal)
                   (eq element-test 'equalp) (eq element-test #'equalp))
               (loop for schar across s
                     for ischar across is do
                     (unless (char-equal schar ischar) (return nil))
                     finally (return t)
                     ))
              (t (general-initial-subsequence-of? sequence initial-sequence))
              )))
          ((and (listp sequence) (listp initial-sequence))
           (loop for s in sequence
                 for is in initial-sequence do
                 (unless (funcall element-test s is) (return nil))
                 finally (return t)
                 ))
          ((and (vectorp sequence) (vectorp initial-sequence))
           (loop for s across sequence
                 for is across initial-sequence do
                 (unless (funcall element-test s is) (return nil))
                 finally (return t)
                 ))
          (t (general-initial-subsequence-of? sequence initial-sequence))
          ))))

(defun ordered? (seq &key (predicate '<) (seq-type :fixnum))
  #.(one-string-nl
     "Determines if the elements of SEQ are ordered with respect to PREDICATE."
     "The algorithm is optimized for the cases where PREDICATE is either"
     "'<' or '>' and the elements of SEQ are declared to be fixnums"
     "(using the SEQ-TYPE keyword with a value of :fixnum)."
     "If SEQ is empty, T is returned."
     "If the sequence is not a sequence whose elements are all fixnums"
     "then :seq-type MUST be provided (a value of T is fine).")
  #.(optimization-declaration)
  (cond
   ((vectorp seq)  
    (let ((len (length seq)))
      (declare (fixnum len))
      (cond 
       ((zerop len) t)
       ((symbol= seq-type :fixnum) 
        (macrolet
            ((doit (predicate-form)
               `(loop with prev fixnum = (the fixnum (aref seq 0)) 
                      for j fixnum from 1 to (the fixnum (1- len)) 
                      as next fixnum = (the fixnum (aref seq j)) 
                      do
                      (if (not ,predicate-form) 
                          (return nil)
                        (setq prev next))
                      finally (return t) 
                      )))
          (cond 
           ((or (eq predicate '<) (eq predicate #'<)) 
            (doit (< prev next)))
           ((or (eq predicate '>) (eq predicate #'>))
            (doit (> prev next)))
           (t (doit (funcall predicate prev next)))
           )))
       (t
        (loop with prev = (aref seq 0)
              for j fixnum from 1 to (the fixnum (1- len)) 
              as next = (aref seq j)
              do
              (if (not (funcall predicate prev next))
                  (return nil)
                (setq prev next))
              finally (return t) 
              )))))
   ((listp seq) 
    (cond
     ((null seq) t)
     ((symbol= seq-type :fixnum)
      (macrolet 
          ((doit (predicate-form)
             `(loop with prev fixnum = (the fixnum (first seq))
                    for next fixnum in (rest seq)
                    do
                    (if (not ,predicate-form)
                        (return nil)
                      (setq prev next))
                    finally (return t)
                    )))
        (cond 
         ((or (eq predicate '<) (eq predicate #'<)) 
          (doit (< prev next)))
         ((or (eq predicate '>) (eq predicate #'>))
          (doit (> prev next)))
         (t (doit (funcall predicate prev next)))
         )))
     (t 
      (loop with prev  = (first seq)
            for next in (rest seq)
            do
            (if (not (funcall predicate prev next))
                (return nil)
              (setq prev next))
            finally (return t)
            ))))
   (t (error "Argument to ORDERED? is not a sequence."))
   ))
    
              
(defun select-sequence-elements (sequence boolean-sequence)
  #.(one-string-nl
     "Select the elements of SEQUENCE whose corresponding elements"
     "(by position) in BOOLEAN-SEQUENCE are non-NIL."
     "A list, string or vector is returned depending on SEQUENCE's type."
     "A null list, string or vector is returned if no elements are selected.")
  (flet ((how-many-selected? () (count-if 'identity boolean-sequence)))
    (cond
     ((and (listp sequence) (listp boolean-sequence))
      (loop for elem in sequence for b in boolean-sequence
            when b collect elem
            ))
     ((and (listp sequence) (vectorp boolean-sequence))
      (loop for elem in sequence for b across boolean-sequence
            when b collect elem
            ))
     (t 
      (macrolet 
          ((do-it (creator-function loop-in-or-across accessor)
             `(let ((x (,creator-function (how-many-selected?))))
                #.(optimization-declaration)
                (loop with j fixnum = 0
                      for elem across sequence
                      for b ,loop-in-or-across boolean-sequence
                      when b do
                      (setf (,accessor x j) elem)
                      (setq j (the fixnum (1+ j)))
                      finally (return x)
                      ))))
        (cond
         ((and (stringp sequence) (vectorp boolean-sequence))
          (do-it make-string across char))
         ((and (stringp sequence) (listp boolean-sequence))
          (do-it make-string in char))
         ((and (vectorp sequence) (vectorp boolean-sequence))
          (do-it make-array across aref))
         ((and (vectorp sequence) (listp boolean-sequence))
          (do-it make-array in aref))
         (t
          (let ((seq? (typep sequence 'sequence)))
            (error 
             (one-string-nl
              "The ~:R argument to SELECT-SEQUENCE-ELEMENTS,"
              "~S,"
              "is not a list, string or vector!" 
              "Both arguments must be sequences.")
             (if seq? 2 1) (if seq? boolean-sequence sequence)
             )))))))))


(defparameter *freqcount-hash-cutoff* 14)

(defun frequency-count 

       (elements-to-count 
        sequences
        &key
        (test 'eql)
        (return-as :sorted-list)
        (key 'identity)
        (if-not-in-alphabet nil))

  (when (stringp sequences) (setq sequences (list sequences)))
  (unless (every (lambda (x) (typep x 'sequence)) sequences)
    (error "Illegal SEQUENCES argument -- must be a list of SEQUENCES"))
  (when (eq key #'identity) (setq key 'identity))

  (let ((hash-test (canonicalize-hash-test test)))

    (cond

     ;; We don't have a known set of elements to count

     ((member elements-to-count '(t :all))
      (cond
       ((and hash-test (every 'simple-string-p sequences))
        (return-freqcount-hash
         nil
         (freqcount-all-simple-strings sequences hash-test key)
         :sorted-list
         ))
       (hash-test
        (return-freqcount-hash
         nil 
         (freqcount-all-via-hash sequences hash-test key)
         :sorted-list
         ))
       (t
        (return-freqcount-alist
         nil (freqcount-all-via-assoc sequences test key) test :sorted-list
         ))))

     ;; We do have a known set of elements to count

     ((typep elements-to-count 'sequence) 
      (cond
       ;; Can we use our super-optimize binning algorithm?
       ((and hash-test 
             (simple-string-p elements-to-count)
             (every 'simple-string-p sequences)
             (null if-not-in-alphabet)
             (eq key 'identity))
        (return-freqcount-alist
         elements-to-count
         (funcall
          (if (eq hash-test 'equalp)
              'freqcount-alphabet-simple-strings-case-insensitive 
            'freqcount-alphabet-simple-strings-case-sensitive)
          elements-to-count sequences)
         hash-test
         return-as
         ))
       ;; Can we use a linear-time hash algorithm?
       ((and hash-test (> (length elements-to-count) *freqcount-hash-cutoff*))
        (return-freqcount-hash
         elements-to-count
         (freqcount-alphabet-via-hash 
          elements-to-count sequences hash-test key if-not-in-alphabet)
         return-as
         ))
       (t
        (return-freqcount-alist
         elements-to-count
         (freqcount-alphabet-via-assoc 
          elements-to-count sequences test key if-not-in-alphabet)
         test
         return-as
         ))))

     (t (error "Illegal value for ELEMENTS-TO-COUNT: ~S" elements-to-count))

     )))

(defun return-freqcount-alist (alphabet alist test return-as)
  (ecase return-as 
    (:sorted-list (sort alist '> :key 'second))
    (:element-position-list 
     (map
      'list
      (lambda (z) (second (find z alist :test test :key 'first)))
      alphabet))))

(defun return-freqcount-hash (alphabet hash return-as)
  (ecase return-as
    (:sorted-list (sort (hash-table-contents hash) '> :key 'second))
    (:element-position-list
     (map 'list (lambda (z) (gethash z hash)) alphabet)
     )))

(defun freqcount-all-simple-strings (sequences test key)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((hash (make-hash-table :test test)))
    (loop for string in sequences do
          (locally 
            (declare (simple-string string))
            (if (eq key 'identity) 
                (loop for i fixnum from 0 below (length string) 
                      as ch = (schar string i)
                      do
                      (incf (the fixnum (gethash ch hash 0))))
              (loop for i fixnum from 0 below (length string) 
                      as ch = (funcall key (schar string i)) 
                      do
                      (incf (the fixnum (gethash ch hash 0))))))
          finally (return hash)
          )))
          

(defun freqcount-all-via-hash (sequences test key)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((hash (make-hash-table :test test)))
    (loop for seq in sequences do
          (if (eq key 'identity)
              (map nil (lambda (x) (incf (the fixnum (gethash x hash 0)))) seq)
            (map nil (lambda (x)
                       (incf (the fixnum (gethash (funcall key x) hash 0))))
                 seq))            
          finally (return hash)
          )))

(defun freqcount-all-via-assoc (sequences test key)
  (let ((alist nil))
    (loop for seq in sequences do 
          (loop for elem in seq 
                as target = (funcall key elem)
                as cell = (find target alist :test test :key 'second)
                do 
                (if cell 
                    (incf (the fixnum (second cell)))
                  (push (list target 0) alist)))
          finally (return alist)
          )))

(defun freqcount-alphabet-simple-strings-case-sensitive (alpha strings)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (simple-string alpha))  
  (let* ((max (reduce 'max alpha :key 'char-code :initial-value 0))
         (array (make-array (1+ max) :element-type 'fixnum :initial-element 0)))
    (declare (fixnum max))
    (declare ((simple-array fixnum (*)) array))
    (loop for string in strings do
          (locally
            (declare (simple-string string))    
            (loop for i fixnum from 0 below (length string)             
                  as index fixnum = (char-code (schar string i))
                  do
                  (when (<= index max) (incf (aref array index)))
                  )))
    (map 'list (lambda (ch) (list ch (aref array (char-code ch)))) alpha)
    ))

(defun freqcount-alphabet-simple-strings-case-insensitive (alpha strings)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (simple-string alpha))  
  (let* ((upper-alpha (string-upcase alpha))
         (max (reduce 'max upper-alpha :key 'char-code :initial-value 0))
         (array (make-array (1+ max) :element-type 'fixnum :initial-element 0)))
    (declare (fixnum max))
    (declare (simple-string upper-alpha))
    (declare ((simple-array fixnum (*)) array))
    (loop for string in strings do
          (locally
            (declare (simple-string string))    
            (loop for i fixnum from 0 below (length string)             
                  as index fixnum = (char-code (char-upcase (schar string i)))
                  do
                  (when (<= index max) (incf (aref array index)))
                  )))
    (map 
     'list
     (lambda (ch) (list ch (aref array (char-code (char-upcase ch)))))
     alpha
     )))

(defun freqcount-alphabet-via-hash 
       (alphabet sequences test key if-not-in-alphabet)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (simple-string alphabet))
  (let ((hash (make-hash-table :test test)))
    (map nil (lambda (x) (setf (gethash x hash) 0)) alphabet)
    (loop for seq in sequences do
          (if (eq key 'identity) 
              (map 
               nil
               (lambda (x) 
                 (let ((elem (gethash x hash)))
                   (if elem
                       (incf (the fixnum (gethash x hash)))
                     (when if-not-in-alphabet 
                       (oops-not-in-alphabet elem alphabet)
                       ))))
               seq)
            (map 
             nil
             (lambda (x) 
               (let* ((target (funcall key x))
                      (elem (gethash target hash)))
                 (if elem 
                     (incf (the fixnum (gethash target hash)))
                   (when if-not-in-alphabet 
                     (oops-not-in-alphabet elem alphabet)
                     ))))
             seq))               
          finally (return hash)
          )))

(defun freqcount-alphabet-via-assoc 
       (alphabet sequences test key if-not-in-alphabet)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (simple-string alphabet))
  (let ((alist (map 'list (lambda (x) (list x 0)) alphabet)))
    (loop for seq in sequences do
          (map 
           nil
           (lambda (elem) 
             (setq elem (funcall key elem))
             (let ((cell (find elem alist :test test :key 'first)))
               (if cell
                   (setf (the fixnum (second cell)) 
                         (the fixnum (1+ (the fixnum (second cell)))))
                 (when if-not-in-alphabet 
                   (oops-not-in-alphabet elem alphabet)
                   ))))
           seq)
          finally (return alist)
          )))

(defun oops-not-in-alphabet (elem alphabet)
  (error
   (one-string-nl
    "The element ~S was found in the sequences to be counted,"
    "but the alphabet you specified, ~S,"
    "does not contain that element!")
   elem alphabet))
           

#+test
(defun random-string-from-alphabet (alphabet n)
  (let ((string (make-string n)))
    (loop 
     with len = (length alphabet) 
     for j from 0 below n
     as i = (random len)
     do 
     (setf (aref string j) (aref alphabet i))
     finally (return string))))

#+test
(defun time-assoc-versus-hash (start stop string-size)
  (declare (fixnum start stop string-size))
  (let ((alphabet (map 'simple-string 'code-char (iota 256)))
        (result nil))
    (loop for j fixnum from start below stop
          as alpha = (subseq alphabet 0 start)
          as rstring = (random-string-from-alphabet alpha string-size)
          do 
          (locally 
            (declare (simple-string rstring))
            (declare (optimize (speed 3) (safety 0) (debug 0)))
            (time 
             (let ((alist (map 'list (lambda (ch) (cons ch 0)) alpha)))
               (loop for i fixnum from 0 below string-size 
                     as ch = (schar rstring i)
                     as cell = (loop for c in alist do 
                                     (when (eql ch (first c)) 
                                       (return c)))
                     do 
                     (when cell
                       (setf (the fixnum (cdr cell)) 
                             (the fixnum (1+ (the fixnum (cdr cell)))))))
               (setq result (sort alist '> :key 'cdr))
               ))
            (time 
             (let ((hash (make-hash-table :test 'eql)))
               (map nil (lambda (ch) (setf (gethash ch hash) 0)) alpha)
               (loop for i fixnum from 0 below string-size 
                     as ch = (schar rstring i)
                     as elem = (gethash ch hash)
                     do
                     (when elem 
                       (setf (gethash ch hash) 
                             (the fixnum (1+ (the fixnum elem))))))
               (setq
                result 
                (sort 
                 (hash-table-contents hash) 
                 '> :key 'second
                 ))))))
    result))


(defun elements-in 
       (sequence 
        alphabet
        &key
        (test 'eql)
        (key 'identity)
        (sequence-element-test 'eql set-provided?)
        (all-matches? nil)
        )
  (elements-in-or-not-in 
   sequence alphabet test key 
   sequence-element-test set-provided? all-matches? t nil))

(defun elements-not-in 
       (sequence
        alphabet
        &key
        (test 'eql)
        (key 'identity)
        (sequence-element-test 'eql set-provided?)
        (all-non-matches? nil)
        )
  (elements-in-or-not-in 
   sequence alphabet test key
   sequence-element-test set-provided? all-non-matches? nil t))


(defun elements-in-or-not-in 
       (sequence alphabet test key sequence-element-test set-provided?
                 return-all-matches? add-if-found? add-if-not-found?)
  (macrolet ((add? ()
               `(or (and found? add-if-found?)
                    (and (not found?) add-if-not-found?))))
    (let ((hash-test (canonicalize-hash-test test))
          (seq-hash-test (canonicalize-hash-test sequence-element-test))
          (results nil))
      (cond
       ;; The comparison function is not portably able to be
       ;; used in a hash table.  Use FIND
       ((null hash-test)
        (map
         nil
         (lambda (x) 
           (let ((found? (find (funcall key x) alphabet :test test)))
             (when (add?) 
               (if return-all-matches?
                   (push x results)
                 (pushnew x results :test sequence-element-test)
                 ))))
         sequence))
       ;; The comparison function is usable in a hash, 
       ;; and the sequence elements are also comparable via a hash-usable function
       ;; We can use an O(N) algorithm not involving PUSHNEW.
       ((or (and seq-hash-test set-provided?)
            (eq key 'identity) (eq key #'identity))
        (let ((alpha-hash (make-hash-table :test hash-test))
              (seq-hash (make-hash-table :test seq-hash-test)))
          (map nil (lambda (x) (setf (gethash x alpha-hash) t)) alphabet)
          (map
           nil
           (lambda (x)
             (let ((found? (gethash (funcall key x) alpha-hash)))
               (when (add?)
                 (when (or return-all-matches? (not (gethash x seq-hash)))
                   (push x results)
                   (setf (gethash x seq-hash) x)
                   ))))
           sequence)))
       (t
        (let ((alpha-hash (make-hash-table :test hash-test)))
          (map nil (lambda (x) (setf (gethash x alpha-hash) t)) alphabet)
          (map
           nil
           (lambda (x)
             (let ((found? (gethash (funcall key x) alpha-hash)))
               (when (add?)
                 (if return-all-matches?
                     (push x results)
                   (pushnew x results :test sequence-element-test)
                   ))))
           sequence))))
      results
      )))



(defun first-difference 
       (seq1 seq2 &key (test 'eql) (if-same? nil) (if-null? -1))
  #.(one-string-nl
     "Determines whether and where two sequences differ, as defined by TEST."
     ""
     "If the two sequences are identical, the value of IF-SAME? is returned."
     "A second value is also returned:  :null if both sequences are null, or"
     ":both otherwise."
     ""
     "If one of the sequences is the null sequence, the value of IF-NULL? is"
     "returned. A second value, :first or :second, indicates which sequence"
     "is the null sequence."
     ""
     "If one of the sequences, X, is the same as the initial subsequence of the"
     "other sequence, Y, then the length of X is returned, along with a second"
     "value indicating whether the first sequence is longer (:first) or the"
     "second is longer (:second)."
     ""
     "Finally, in the standard case where the elements at position I in both"
     "sequences differ, I is returned, along with a second value :both."
     )
  (block exit
    (let ((pos 0) (any-elements? nil))
      (map 
       nil
       (lambda (x y) 
         (if (null (funcall test x y)) 
             (return-from exit (values pos :both))
           (progn
             (setq any-elements? t)
             (incf pos)
             )))
       seq1 
       seq2
       )
      (if any-elements?
          ;; Then the two sequences are equivalent up to end of shorter one.
          (let ((len1 (length seq1)) (len2 (length seq2)))
            (cond 
             ((= len1 len2) (values if-same? :both))
             ((< len1 len2) (values len1 :second))
             ((> len1 len2) (values len2 :first))
             ))
        ;; Otherwise at least one of the sequences was a null sequence
        (flet ((null-sequence? (x) 
                 (if (listp x) (null x) (zerop (length x)))))
          (let ((seq1-null? (null-sequence? seq1))
                (seq2-null? (null-sequence? seq2)))
            (if (and seq1-null? seq2-null?) 
                (values if-same? :null)
              (values if-null? (if seq1-null? :first :second))
              )))))))
      
(defun multiple-key-sort (sequence predicates keys)
  #.(one-string-nl
     "Sorts a sequence using an ordered set of keys (fields)."
     "The primary key (field) is provided first in the KEYS list,"
     "the secondary key is provided second, etc."
     "If a single predicate is provided that predicated is used for"
     "all the keys, otherwise each predicate is used with its associated"
     "key.")
  (unless (listp keys)
    (error "Must provide a list of keys!"))
  (unless (listp predicates)
    (setq predicates (make-list (length keys) :initial-element predicates)))
  (unless (= (length keys) (length predicates))
    (error "There must be as many keys as predicates!"))
  (loop for p in (reverse predicates)
        for k in (reverse keys)
        do
        (setq sequence (stable-sort sequence p :key k))
        finally 
        (return sequence)
        ))
     
               
(defun initial-subseq-or-all (seq n &key (copy? nil))
  #.(one-string-nl
     "Returns the first N elements of a sequence or the entire sequence"
     "if the length of the sequence is less than N.  Makes sure a copy"
     "of the sequence is returned instead of the original sequence"
     "if COPY? is T."
     )
  (if (<= (length seq) n)
      (if copy? (copy-seq seq) seq)
    (subseq seq 0 n)
    ))
    