;; -*- Package: aframes; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :aframes)

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

(defmethod anything-stringlike-to-string ((x %aframe)) (fname x))


(defun canonicalize-frame-designator (f &optional (create? nil))
  #.(one-string-nl
     "Returns a frame created using F."
     "If F is already a frame it is returned."
     "If F is a string, a frame by that name is created."
     "If F is a symbol, a frame by the symbol's name is created.")
  (cond 
   ((isframe? f) f)
   ((stringp f) (frame-fnamed f create?))
   ((symbolp f) (frame-fnamed (symbol-name f) create?))
   (t (error "Don't know how to coerce ~A into a frame." f))
   ))

(defun create-valid-frame-name 
       (string 
        &key
        (prefix nil)
        (suffix nil)
        (case-action :none)
        (space-char-action :remove)
        (from-chars "()")
        (to-chars "[]")
        (verify? t)
        )
  #.(one-string-nl
     "Create a frame name from an arbitrary string STRING.  By default:"
     "  -- No case conversion is done."
     "  -- Spaces in the string are removed."
     "  -- left/right parentheses are converted to left/right brackets."
     "  -- the resulting string is scanned for illegal frame characters."
     "STRING itself is not modified, a freshly minted string is returned."
     "CASE-ACTION determines what Lisp 'case' function to call on STRING:"
     "  -- :UPPERCASE - STRING-UPCASE"
     "  -- :LOWERCASE - STRING-DOWNCASE"
     "  -- :CAPITALIZE - STRING-CAPITALIZE"
     "  -- :NONE (or NIL) - no case conversion."
     "SPACE-CHAR-ACTION determines what happens to space characters in STRING."
     "If the value is :REMOVE or :DELETE (the default) all spaces (but not"
     "other whitespace) are removed from STRING.  If the value is a character"
     "object, that character replaces all occurences of spaces in STRING."
     "Any other value is erroneous."
     "FROM-CHARS and TO-CHARS define a substitution mapping. STRING is scanned"
     "and any char in STRING which is found in FROM-CHARS is replaced by the"
     "corresponding (indexwise) char in TO-CHARS."
     "If PREFIX and/or SUFFIX are non-nil they are assumed to be strings and"
     "are concatenated to STRING before and/or after."
     "VERIFY determines whether the result string is finally scanned for"
     "characters that are not legal in frame names."
     )
  (let ((sstring (coerce (copy-seq string) 'simple-string)))
    (setq sstring
          (case case-action
            ((nil :none) sstring)
            (:uppercase (nstring-upcase sstring))
            (:lowercase (nstring-downcase sstring))
            (:capitalize (nstring-capitalize sstring))
            ))
    (when from-chars
      (unless to-chars (error "FROM-CHARS provided but not TO-CHARS!"))
      (unless (= (length to-chars) (length from-chars))
        (error "FROM-CHARS and TO-CHARS must be same length!"))
      (ntranslate-string sstring from-chars to-chars))
    (case space-char-action
      ((:remove :delete) (setq sstring (delete #\Space sstring)))
      (t
       (unless (characterp space-char-action)
         (error "SPACE-CHAR-ACTION neither a valid action nor a character!"))
       (setq sstring (nsubstitute space-char-action #\Space sstring))
       ))
    (setq sstring
          (cond
           ((and (null prefix) (null suffix)) sstring)
           ((and prefix suffix) (one-string prefix sstring suffix))
           (prefix (one-string prefix sstring))
           (suffix (one-string sstring suffix))
           ))
    (when verify?
      (let ((bad-char? nil))
        (loop for ch across sstring do
              (unless (valid-frame-char? ch)
                (cformatt "Ruh roh. Invalid character: ~S" ch)
                (setq bad-char? t)))
        (when bad-char? 
          (error "CONCOCT-VALID-FRAME-NAME: Illegal characters found!"))))
    sstring
    ))

(defun ->frames (object &optional (create nil))
  #.(one-string-nl
     "Translates all of the strings or symbols in any sort of complex list"
     "structure into the frames named by those strings. Returns new structure"
     "exactly like the given one, where the strings are translated into"
     "frames wherever possible.  If CREATE is true, strings which do not"
     "currently name frames have frames created with that name.")
   (cond ((null object) nil)
	 ((or (stringp object) 
	      (symbolp object))
	  (or (frame-fnamed object create) object))
	 ((listp object) (mapcar (lambda (o) (->frames o create)) object))
	 (t object)))

(defun frame->related-and-containing-frames 
       (frame relating-slot 
              &key (containing-slot #$sys.isa) (duplicates-ok? t))
  #.(one-string-nl
     "Retrieves the value of RELATING-SLOT from FRAME, which must be a list "
     "of frames.  Returns a list of these frames along with others that "
     "are elements of the CONTAINING-SLOT of each retrieved frame, applied "
     "recursively.  In other words, we calculate the transitive closure "
     "of the CONTAINING-SLOT relationship to all elements of the value of "
     "RELATING-SLOT.  Example:  #$A has a slot #$B containing (#$C #$D). "
     "#$C is a #$WIDGET, and #$WIDGET is a #$OBJECT.  #$D is a #$MACHINE. "
     "Then (frame->related-and-containing-frames #$A #$B) -> "
     "(#$C #$D #$WIDGET #$OBJECT #$MACHINE) "
     "(not necessarily in that order)." )
  (let ((result
         (loop for frame in (slotv frame relating-slot) nconc
               (compute-transitive-slot frame containing-slot))))
    (if duplicates-ok? result (remove-duplicates result))
    ))

(defun find-frames (substring &key (framespaces :default) (slot *fname-frame*))
  #.(one-string-nl
  "Return a list of all frames that contain SUBSTRING in their name"
  "(the default) or some other designated SLOT.")
  (search-frames substring framespaces :slots slot))



(defun google-frames (framespaces &rest strings)
  #.(one-string-nl
     "Given an arbitrary set of strings, find all the frames whose name"
     "contains all the strings.  Example:"
     "(google-frames :all \"go.\" \"sodium\" \"activity\")"
     "This is like FIND-FRAMES but takes an arbitrary number of strings,"
     "and only searches the #^FNAME slot."
     )
  (search-frames 
   "" framespaces :slots #$fname
   :slot-value-predicate 
   (lambda (s) 
     (loop for string in strings
           when (not (search string s :test 'char-equal))
           do (return nil)
           finally (return t)))))



(defun googleplex-frames (framespaces &rest strings)
  #.(one-string-nl
     "Find frames containing any of STRINGS in any string-valued slots.")
  (search-frames 
   "" framespaces 
   :slot-value-predicate 
   (lambda (s) 
     (and 
      ;; Need stringp test because this is being called on value of every slot
      (stringp s)
      (loop for string in strings
            when (search string s :test 'char-equal)
            do (return t)
            finally (return nil))))))

        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generalized search frames utility

(defun funcall-form (f &rest args)
  (cond 
   ((or (eq f 'identity) (eq f #'identity)) (first args))
   ((typep f 'function) `(funcall ,f ,@args))
   (t `(,f ,@args))))



(defun pattern-to-search-function (p search-function test)
  (labels ((ps (p)
             (cond 
              ((stringp p)
               (funcall-form 
                search-function p 's :test (if (functionp test) test `',test)))
              ((eq :not  (first p)) 
               `(not ,(ps (second p))))
              ((eq :and  (first p)) 
               `(and ,@(mapcar #'ps (rest p))))
              ((eq :or  (first p)) 
               `(or ,@(mapcar #'ps (rest p))))
              )))
    `(lambda (s) ,(ps p))
    ))


(defun canonicalize-search-pattern (p)
  (cond 
   ((stringp p) p)
   ((not (listp p)) (error "unrecognized search pattern: ~a" p))
   ((every 'stringp p) `(:and ,@p))
   ((not (symbolp (first p))) 
    (error "bad search pattern: ~a" p))
   ((eq :not (keywordize (first p))) 
    (unless (= 2 (length p)) 
      (error "invalid NOT pattern: ~a" p))
    `(:not ,(canonicalize-search-pattern (second p))))
   ((or (eq :and (keywordize (first p))) (eq :or (keywordize (first p))))
    `(,(keywordize (first p))
      ,@(mapcar 'canonicalize-search-pattern (rest p))))
   (t (error "unrecognized search pattern: ~a" p))
   ))

(defun canonicalize-search-slots (s)
  (cond
   ((eq s t) s)
   ((framep s) (list s))
   ((every 'framep s) s)
   (t (error "invalid list of search slots: ~a" s))
   ))

(defun frame-match-code  
       (frame-symbol slots slot-predicate slot-value-predicate 
                     slot-value-key match-function)
  `(block exit
     (let* ((slot-elements ',slots) 
            (slot-count (length slot-elements))
            (slot-matches 0))
       (declare (fixnum slot-count slot-matches))
       (declare (ignorable slot-count slot-matches))
       (for-each-frame-slot (slot value) ,frame-symbol
             ,@(when slots
                 `((when (= slot-matches slot-count) (return-from exit nil))))
             (vwhen (key (and ,(if slots `(and (member slot slot-elements)
                                               (incf slot-matches)) t)
                              ,(if slot-predicate 
                                   (funcall-form slot-predicate 'slot)
                                 t)
                              ,(if slot-value-predicate 
                                   (funcall-form slot-value-predicate 'value)
                                 t)
                              ,(if slot-value-key 
                                   (funcall-form slot-value-key 'value)
                                 'value
                                 )))
               (when ,(funcall-form match-function 'key)
                 (return-from exit t)) 
               )))))

(defvar *frames-variable*)

(defun search-frames 
       (search-pattern 
        framespaces
        &key 
        (frame-predicate 'identity)
        (slots nil)
        (slot-predicate 'identity)
        (slot-value-predicate 'stringp)
        (slot-value-key 'identity)
        (match-function 'search)
        (match-function-test-keyword-value 'string-equal)
        (pprint? nil) 
        (execute? t)
        )
  #. (one-string-nl 
      "Search for frames in all the framespaces specified by FRAMESPACES"
      "(if FRAMESPACES is :all then the entire frame world is searched)"
      "for SEARCH-PATTERN, which can either be a string or a"
      "boolean pattern e.g.," 
      "'(:and \"cat\" (:or \"dog\" (:not \"foo\")))"
      "By default, the search will be over all the slot values of"
      "each frame which are strings."
      "Which frames are searched can be controlled by FRAME-PREDICATE."
      "The slots in each frame which are searched can be controlled"
      "using either or both of SLOTS, which is a list of slot frames"
      "to be searched, or SLOT-PREDICATE, which is called on each"
      "slot frame."
      "The slots searched can be further controlled using SLOT-VALUE-PREDICATE"
      "which is called on the value of the slot."
      "Finally, SLOT-VALUE-KEY is called on the value of the slot and the"
      "result is passed to the matching function for a final comparison."
      "By default, the MATCH-FUNCTION used is the Common Lisp function"
      "SEARCH and it is called with :test being 'string-equal"
      "(That is, the string search is case insensitive.)"
      )
      
 (setq search-pattern (canonicalize-search-pattern search-pattern))
  (setq slots (canonicalize-search-slots slots))
  (let* ((inner-search-function 
          (pattern-to-search-function  
           search-pattern match-function match-function-test-keyword-value))
         (framespace-list 
          (cond 
           ((eq framespaces :all) (list nil)) 
           ((atom framespaces) (list framespaces))
           (t framespaces)))
         (search-function 
          `(lambda () 
             (let ((matching-frames nil))
               (dolist (fs ',framespace-list)  
                 (with-frames-iterated (f fs)
                   (when ,(funcall-form frame-predicate 'f)
                     (when 
                         ,(frame-match-code 
                           'f
                           slots 
                           slot-predicate
                           slot-value-predicate 
                           slot-value-key
                           inner-search-function 
                           )
                       (push f matching-frames)
                       ))))
               matching-frames
               ))))
    (when pprint? 
      (cformatt "Generated search function: ")
      (terpri)
      (pprint search-function)
      (terpri)
      )
    (when execute? (funcall (compile nil search-function)))
    ))

(defmacro with-temp-frames ((tf frame-names) &body body)
  (unless (symbolp tf) (error "Not a symbol: ~S" tf))
  (let ((fn-symbol (gensym "FRAME-NAME-")))
    `(let ((,fn-symbol ,frame-names))
       (setq ,fn-symbol
             (loop for f in ,fn-symbol collect 
                   (cond 
                    ((symbolp f) (symbol-name f))
                    ((stringp f) f)
                    ((isframe? f) (error "Cannot use existing frame! ~A" f))
                    (t (error "Unrecognizable frame designator: ~S" f)))))
       (loop for fname in ,fn-symbol do 
             (vwhen (f (frame-fnamed fname))
               (error "Cannot use existing frame as temporary frame! ~A" f)))
       (unwind-protect 
           (progn 
             (let ((,tf
                    (loop for fname in ,fn-symbol
                          collect (frame-fnamed fname t))))
               ,@body))
         (loop for frame-name in ,fn-symbol do 
               (unintern-frame (frame-fnamed frame-name)))
         ))))
  
       
(defparameter *loop-verbs* 
  '(:append :do :collect :sum :nconc))

(defun loop-verb? (x)
  (and (symbolp x) (member (keywordize x) *loop-verbs*)))


(defun parse-loop-body (loop-body)
  (let ((pos (position-if 'loop-verb? loop-body)))
    (cond
     ((null pos) (values nil nil loop-body))
     ((zerop pos) (values nil (first loop-body) (rest loop-body)))
     (t (values (subseq loop-body 0 pos) 
                (nth pos loop-body)
                (subseq loop-body (1+ pos))
                )))))


(defun create-frame-slot-binding (binding frame-symbol)
  (flet ((to-frame-symbol (x)
           (cond 
            ((symbolp x) x)
            ((isframe? x) 
             (intern (string-upcase (slotv x *fname-frame*)) *package*))
            (t (error "Cannot convert ~S to symbol."))))
         (to-symbol-frame (x)
           (cond
            ((isframe? x) x)
            ((symbolp x) (frame-fnamed (string x) t))
            (t (error "Cannot convert ~S to frame"))))
         (to-slot-accessor (slot-frame)
           `(slotv ,frame-symbol ,slot-frame)))
    (cond
     ((symbolp binding) 
      (list binding (to-slot-accessor (to-symbol-frame binding))))
     ((isframe? binding) 
      (list (to-frame-symbol binding) (to-slot-accessor binding)))
     ((listp binding) 
      (unless (= 2 (length binding)) 
        (error "Invalid binding form: ~S" binding))
      (list (to-frame-symbol (first binding)) 
            (to-slot-accessor (to-symbol-frame (second binding))))
      ))))


(defmacro frameloop ((frame-var frame-list) slot-bindings &body loop-body)
  #.(one-string-nl 
     "Executes LOOP-BODY with FRAME-VAR bound successively to the elements"
     "of FRAME-LIST. In addition, variables defined by SLOT-BINDINGS are bound"
     "to the values of slots in the frame currently bound to FRAME-VAR."
     "SLOT-BINDINGS is a list of elements which are either symbols, frames, or"
     "two element lists. If a slot binding is a symbol, then that symbol"
     "is bound to the value of a slot by the same name.  If a slot binding"
     "is a frame, then a symbol whose name is the same as the frame's #$fname"
     "is bound to the value of the slot named by that frame."
     "If slot binding is a two element list, then the first element designates"
     "the variable name, and the second element designates the slot frame."
     "LOOP-BODY may include LOOP directives. If no loop action (such as 'do'"
     "or 'nconc') is present in LOOP-BODY then 'do' is assumed."
     "Example: "
     "(frameloop (f '(#$dog #$cat)) (name (sound #$vocalization)) "
     "   collect (list name 'says sound))"
     )
  (multiple-value-bind (pre loop-verb body)
      (parse-loop-body loop-body)
    (let ((bindings 
           (mapcar 
            (lambda (sb) (create-frame-slot-binding sb frame-var))
            slot-bindings 
            )))
      `(loop for ,frame-var in ,frame-list 
             ,@(loop for (var value) in bindings nconc
                     `(as ,var = ,value))
             ,@pre
             ,(or loop-verb :do)
             ,@body
             ))))

(defmacro frame-loop (&rest args)
  `(frameloop ,@args))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-frame-hierarchy (relationship-frame arcs)
    (loop for (from to) in arcs do 
          (pushnew to (slotv from relationship-frame)))))


#|
(defparameter test-h (make-frame-hierarchy 
              #$isa 
              '((#$qi #$qh) 
                (#$qh #$qe)
                (#$qe #$qd)
                (#$qe #$qc)
                (#$qf #$qc)
                (#$qd #$qb)
                (#$qg #$qb)
                (#$qb #$qa)
                (#$qc #$qa)
                (#$qi #$qg)
                )))
|#


(defmacro with-temporary-slot ((frame slot &optional value) &body body)
  #.(one-string-nl 
     "Executes BODY with FRAME provided with a slot SLOT whose value is"
     "VALUE.  If SLOT already exists in FRAME an error is signalled.")
  (let ((frame-symbol (gensym "FRAME-"))
        (slot-symbol (gensym "SLOT-")))
    `(let ((,frame-symbol ,frame)
           (,slot-symbol ,slot))
       (when (frame-has-slot? ,frame-symbol ,slot-symbol)
         (error "Frame ~A has an existing slot ~A" ,frame-symbol ,slot-symbol))
       (setf (slotv ,frame-symbol ,slot-symbol) ,value)
       (unwind-protect 
           (progn ,@body)
         (delete-slot ,frame-symbol ,slot-symbol)
         ))))
           