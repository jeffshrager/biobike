;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2008 The BioBike Team                                |
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

;;; Authors:  Jeff Shrager & JP Massar

;;; Contains the main BioLingua primitives.  Every top-level primitive
;;; in here should have a doc string, and should be exported. 

;;; The exporting is done now in the library-api.lisp file.

#|
(defun index-a-db-file (dbfile key-line-predicate &key (key-make-fn #'identity))
  #.(one-string-nl 
     "Finds records in a file and returns an alist of (key . startpos) pairs."
     "The requires key-line-predicate tells us when we are at a key line."
     "The keyword argument :key-make-fn (default #'identity) turns each such key line into a key in the alist."
     "The function reads forward until it find a key before beginning to index the file."
     "(This often useful for scanning spreadsheets.)"
     ) 
  (with-open-file 
   (i dbfile)
   (let ((curkey))
     (loop for line = (read-line i nil nil)
	   with lastpos = 0
	   until (null line)
	   do (cond ((funcall key-line-predicate line)
		     (push (cons (funcall key-make-fn line) lastpos) result))
		    (t (setf lastpos (file-position i))))

   (loop with result = nil
	 with last-key = nil
	 with lastpos = 0
	 with curpos = 0
	 for line = (read-line i nil nil)
	 until (null line)
	 do
	 (cond ((funcall key-line-predicate line)
		(push (cons (funcall key-make-fn line) lastpos) result)
		(setf lastpos curpos))
	       (t (setf curpos (file-position i))))
	 finally (return result))))
|#

            
#||

We need a better mechanism to run shell commands, because the
way it is done now halts the entire system until the shell
command returns.  

What we will do is: 

Figure out the current timeout limit, and set a slightly smaller
timeout limit for the code that is to run the shell command.  
(If the timeout limit is too small, we'll just not run the command)

Start the shell command going, getting the process ID.  We
start the shell command preceding it with 'exec' so that we will get
the process ID of the command itself, not a shell that then starts 
the command.  (See excl:run-shell-command documentation) 

Establish an unwind-protect so that if the timeout occurs, 
we will execute a kill -9 shell command to kill the 'runaway'
process.  

Then we'll go into a loop where we sleep for one second
then call sys:reap-os-subprocess to see if the shell command
is finished.  If it is, we exit normally, otherwise, 
to our sleep loop.  

||#

#-allegro
(defun shell-command-with-timeout (&rest args) 
  (declare (ignore args))
  (error "Not implemented."))

#+allegro
(defun shell-command-with-timeout 
       
       (command-string 
        &key 
        (valid-return-codes (list 0))
        (action-on-invalid-return :print)
        (directory nil)
        (exec? t)
        (renice-at 3)
        &aux 
        (command (first (string-split command-string)))
        (user-limit wb::*execution-timelimit*)
        (sys-limit wb::*default-execution-timelimit*)
        (current-limit (or user-limit sys-limit)))
  #.(one-string-nl
     "Executes COMMAND-STRING as a shell command, aborting it"
     "if the process so created uses more than the user's current"
     "timelimit.  If such a timeout occurs :TIMEOUT is returned, otherwise"
     "the return code of the execution of the shell command is returned."
     ""
     "By default, if the return code is non-zero a warning to this effect"
     "is printed.  If instead ACTION-ON-INVALID-RETURN is :error, an error"
     "is signaled instead of printing a warning; if the value is NIL nothing"
     "is printed and no error is signaled.  The user may provide a list of"
     "valid return codes intsead of using the default of 0 via the"
     "VALID-RETURN-CODES keyword."
     ""
     "By default if the shell command takes more than 3 seconds of"
     "real time to execute, it is lowered in priority to the lowest possible"
     "system priority.  This can be adjusted via the RENICE-AT keyword,"
     "which accepts values in seconds.")

  (unless current-limit 
    (error "No execution timelimit defined!"))
  (unless (plusp renice-at)
    (error "RENICE-AT must be a positive integer!"))
  (unless (every 'integerp valid-return-codes) 
    (error "VALID-RETURN-CODES contains a non-integer!"))

  (let ((limit (- current-limit 2)))
    (unless (> limit 3) 
      (error
       (one-string-nl
        "You cannot run '~A' with a timelimit of ~S seconds."
        "Please set your timelimit to at least 5 seconds.")
       command current-limit))

    (let ((exec-command (if exec? (s+ "exec " command-string) command-string))
          (status :running))
      (multiple-value-bind (in out error pid)
          (excl:run-shell-command 
           exec-command 
           :input nil
           :output nil
           :error-output nil
           :separate-streams t
           :wait nil
           :directory directory
           )
        (declare (ignore in out error))
        (flet ((killit ()
                 (excl:run-shell-command (formatn "kill -9 ~D" pid) :wait t)
                 (multiple-value-bind (exit-status finished-pid signal)
                     (sys:reap-os-subprocess :pid pid :wait nil)
                   (declare (ignore finished-pid signal))
                   (unless (and (numberp exit-status) (zerop exit-status)) 
                     (cformatt 
                      "Process ~d did not die using kill -9 (status ~A)!!!" 
                      pid exit-status)
                     ))))
          (unwind-protect 
              (mp:with-timeout 
               (limit 
                (cformatt 
                 (one-string-nl
                  "Biotools program ~A timeout."
                  "Killing ~A program." 
                  "Use Tools -> Prefs to set *execution-timelimit* to a larger"
                  "value or contact the system administrators if it is already"
                  "at the maximum allowable value and you need more time.")
                 command command)
                (killit)
                (setq status :killed)
                :timeout 
                )
               (loop 
                for j from 1 do
                (multiple-value-bind (exit-status finished-pid signal)
                    (sys:reap-os-subprocess :pid pid :wait nil)
                  (declare (ignore signal finished-pid))
                  (if (null exit-status)
                      (progn
                        (sleep 1)
                        (when (>= j renice-at)
                          (excl:run-shell-command
                           (formatn "renice +20 ~D" pid))))
                    (flet ((oops (f) 
                             (apply 
                              f 
                              (one-string-nl
                               "When executing biotools program ~A,"
                               ";; the command ~s"
                               ";; returned the exit status ~d," 
                               ";; while the expected status was ~s")
                              command command-string
                              exit-status valid-return-codes
                              )))
                      (unless (member exit-status valid-return-codes)
                        (case action-on-invalid-return
                          (:print (oops 'cformatt))
                          (:error (oops 'error))
                          (nil nil)))
                      (setq status :finished)
                      (return exit-status)
                      )))))
            (case status 
              ((:killed :finished) nil)
              (:running 
               (cformatt "*** User timeout: Killing ~A program." command)
               (killit)
               ))))))))


(defmacro aloop ((array &rest dims) &body body)
  #.(one-string-nl
     "ALOOP loops over the multiple dimensions of an arbitrary array in the"
     "same order in which ARRAY-DIMENSIONS would return the size of the array."
     "You provide the variables that will be bound to the incrementing"
     "dimenions. These variables are used to build the loop across"
     "the array, so the number of these"
     "variables must match the number of dimensions of the array."
     "Example:"
     "(let ((my-string \"a string\"))"
     "  (aloop (my-string pos) (print (list pos (aref my-string pos)))))))"
     "=> (0 #\a) (1 #\Space) (2 #\s) (3 #\t) (4 #\r) (5 #\i) (6 #\n) (7 #\g)"
     ""
     "Example:"
     "(setq two-d-array (make-array '(3 5)))"
     "(aloop (two-d-array r c)"
     "     (setf (aref two-d-array r c)"
     "            (random 100)))"
     "two-d-array"
     "=> #2A((43 43 38 18 36) (44 81 12 92 17) (29 28 75 94 29))"
     ""
     "Note carefully the syntax: (aloop (array-variable dv1 dv2 ...) body...)"
     "where d1 dv2 etc are the variables that will be bound to the array"
     "indexes in array-dimensions order."
     )
  (let ((intarray (gensym "aloop")))
    (labels 
        ((aloop! (dims n)
           (cond ((null dims) (cons 'progn body))
                 (t `(loop for ,(car dims)
                           below (nth ,n (array-dimensions ,intarray))
                           do ,(aloop! (cdr dims) (1+ n)))))))
      `(let ((,intarray ,array))
         (when (not (= ,(length dims)
                       (length (array-dimensions ,intarray))))
           (error 
            (one-string-nl
             "ALoop expected an array of ~a dimensions,"
             "but got one, ~S, of ~a dimensions.")
            ,(length dims) (array-rank ,intarray)))
         ,(aloop! dims 0)))))
	  
;;; Tools for working with microarray data tables a little more
;;; conveniently than trying to use the table prims morass.

(defun select-microarray-table-data (table test)
  #.(one-string-nl
     "Given a table, loaded by read-microarray-table, and a test "
     "with two args: a gene frame and a data vector, run the test "
     "on each pair (gene frame and data vector), and collect those "
     "pairs that pass.  Example: (select-from-table h "
     "#'(lambda (gene data) (search \"1234\" (#^fname gene)))) "
     "finds the genes and table data friom table h where the gene "
     "names have 1234 in them.")
  (loop for gene across (car (table-data-cols table (list "geneID")))
      as data = (table-data-data-row table gene)
      when (funcall test gene data)
      collect (list gene data)))


(defun gene->all-related-go-frames-and-isas (gene &key (duplicates-ok? t))
  #.(one-string-nl
     "Assumes GENE has frames in its #$GO-FRAMES slot. "
     "Returns a list of all these frames and all frames for which there is "
     "an ISA relationship, direct or transitively, with one or more of the "
     "frames in the #$GO-FRAMES slot.")
  ;; shut compiler up for seed compilation
  (forward-funcall 
   'frame->related-and-containing-frames 
   gene #$go-frames :duplicates-ok? duplicates-ok?))


(defun is-gene-related-to-go-frame? (gene go-frame)
  #.(one-string-nl
     "Assumes GENE has frames in its #$GO-FRAMES slot. "
     "Returns T if GO-FRAME is in the list returned by "
     "(GENE->ALL-RELATED-GO-FRAMES-AND-ISAS gene), but does so more "
     "efficiently than directly calling that function.")
  (let ((related-go-frames (#^Go-frames gene)))
    (or (member go-frame related-go-frames)
        (some
         #'(lambda (related-frame)
             (member 
              go-frame
              ;; shut compiler up for seed compilation
              (forward-funcall 'compute-transitive-slot related-frame #$isa)))
         related-go-frames
         ))))

(defun genes->common-go (genes)
  #.(one-string-nl
     "Figures out the intersection of all the closures of all the go frames"
     "related to the listed genes.  That is, asks: 'What do these genes have"
     "in common (in terms of shared GO frames)? ")
  (loop with set = (GENE->ALL-RELATED-GO-FRAMES-AND-ISAS (car genes) :duplicates-ok? nil)
	as gene in (cdr genes)
	do (setq set (intersection set (GENE->ALL-RELATED-GO-FRAMES-AND-ISAS gene :duplicates-ok? nil)))
	finally (return set)))

;;; Count and report the number of GO categories represented in a set of genes.

;; No sense keeping the hash table around because when
;; GO-MEMBERSHIP-COUNT is called it was doing an CLRHASH anyway.  JP.
(defvar *temp-table* nil)

(defun go-membership-count (genes)
  #.(one-string-nl
     "Given a set of genes, count how many hits there are for "
     "each GO frame, for all the genes, all the way up the GO tree."
     "The result is a list (sorted for greatest first) of the categories "
     "and counts. Note that there is NO protection against double-counting "
     "for the same gene!"
     )
  (let ((*temp-table* (make-hash-table :test #'equal)))
    (loop for gene in genes 
	  when (framep gene)
	  do (mapcar #'score-go-frame-parents (#^go-frames gene)))
    (gather-go-frame-scores)))

(defun gather-go-frame-scores (&optional (limit 25))
 (sort 
  (loop for key being the hash-keys of *temp-table*
	using (hash-value value)
	when (>= value limit)
	collect (cons key value))
  #'(lambda (a b) (> (cdr a) (cdr b)))))

(defun score-go-frame-parents (frame)
  (cond ((null frame) nil)
	(t (let ((v (gethash frame *temp-table*)))
	          (cond (v (incf (gethash frame *temp-table*)))
			   (t (setf (gethash frame *temp-table*) 1))))
	      (mapcar #'score-go-frame-parents
		         (#^isa frame)))))

;;; Some pathway tools

;;; Experiments with Ocelot <-> GO

(defun render-pathway-in-organism-terms (pathway organism)
  #.(one-string-nl
     "Uses the GO->gene bindings, and the pathway->reaction->Go bindings, "
     "figure out the genes in a given organism that make up a pathway")
  ;; Note, reaction list weirdly can sometimes only be one element (non-listp!)
  (remove-duplicates 
   (flatten 
    (loop for reaction in (let ((reactions? (#^reaction-list pathway)))
			    (if (listp reactions?) reactions?
			      (list reactions?)))
	  collect (loop for go-frames in (#^go-frames reaction)
		        collect (loop for gene in (#^related-genes go-frames)
				      when (eq organism (#^organism gene))
				      collect gene))))))

;;; This could just use intersection, but this is faster and less consy.
(defun count-commonalities (l1 l2)
  #.(one-string-nl
     "Counts the number of elements that two lists have in common (by EQL)."
     "Uses O(N**2) algorithm so not a good choice for very long lists.")
  (loop for i in l1
	when (member i l2)
	sum 1))

;;; Beginnings of a hypothesis working directory toolset.

#|

;;; This is hardcoded and it's not clear that it is used at all.
;;; See if anyone screams that this is commented out.  Need to
;;; consider whether wb::*username* should become (wb::user-session-id)

(defun append-to-hypothesis-space (expr)
  (with-open-file 
      (o "/tmp/modelworld.lisp" 
         :direction :output :if-exists :append :if-does-not-exist :create)
    (print expr o)))

(defun assert-constraint (constraint)
  (add-hypo-expr 'constraint constraint))

(defun assert-qrelation (rel)
  (add-hypo-expr 'qrelation rel))

(defun add-hypo-expr (type expr)
  (append-to-hypothesis-space 
   `((,(get-universal-time)
     ,wb::*username*
     ,type)
     ,expr)))

(defun list-constraints ()
  (with-open-file (i "/tmp/modelworld.lisp")
    (loop for expr = (read i nil nil)
          until (null expr)
	  when (member 'constraint (car expr))
	  collect expr)))

|#

(defun goid-as-string->number+frame (s)
  #.(one-string-nl
     "Removes the 'GO:' part from a go ID given as a string, "
     "and gives you back both the number and to go frame as a pair.")
  (let ((gopos (search "GO:" s)))
    (if (and gopos (zerop gopos ))
	(let ((n (read-from-string (subseq s 3))))
          ;; shut compiler up for seed compilation
	  (cons n (forward-funcall 'goid->frame n)))
      (error 
       (one-string
        "You passed goid-as-string->number+frame something that didn't look "
        "like a GO ID: '~a'.  GO IDs have to start with 'GO:' !" s
        )))))

(defun summarize-alignment (strings)
  #.(one-string-nl
     "Given a single argument which is a list of strings, "
     "which may be of various lengths, "
     "this function produces a table of tables, "
     "where the outer table has the same number of "
     "inner tables as there are characters in the longest string, "
     "and where each inner table reports the number of occurrances "
     "of each character that appears in each string at the "
     "given position.  Use REPORT-ALIGNMENT-SUMMARY to print out the"
     "contents of these results.")
  (loop for position from 0 below (apply #'max (mapcar #'length strings))
	with outer-table = (make-hash-table :test #'equal)
	do (setf (gethash position outer-table)
		 (loop for string in strings
		       as string-length = (length string)
		       with inner-table = (make-hash-table :test #'equal)
		       as char = (and (< position string-length) 
				      (aref string position))
		       when char
		       do (cond ((gethash char inner-table)
				 (incf (gethash char inner-table)))
				(t (setf (gethash char inner-table) 1)))
		       finally (return inner-table)))
	finally (return outer-table)
	))

(defun report-alignment-summary (summary-table)
  #.(one-string-nl 
     "Displays the results of calling summarize-alignment "
     "on a list of strings.  Also, returns the summary as a list of lists.")
  (loop for position below (hash-table-count summary-table)
	as inner-table = (gethash position summary-table)
	do 
	(format t "~a: " position)
	collect
	(cons position 
	(loop for char being the hash-keys of inner-table
	      using (hash-value count)
	      do (format t "(~a ~a) " char count)
	      collect (cons char count)
	      finally (format t "~%")))))

;;; Interfacing to external code.

(defmacro with-entity-encoding 
          ((infn outfn 
                 &key (mapfn '(let ((counter 0))
                                (lambda (entity) 
                                  (declare (ignore entity))
                                  (incf counter)))))
           &body body)
  #.(one-string-nl
     "Provides to the body expressions two functions, an in-function and "
     "an out-function.  The in-function takes any object and gives it a "
     "unique numerical id (just a counter) and hands you back this id."
     "The outfunction takes those IDs and returns the object."
     "This is generally used to translate arbitrarily-named objects,"
     "such as gene frames, into something that can be passed out to"
     "external programs and then passed back in and dereferenced to the"
     "object.  The problem being that external programs generally have"
     "arbitrary constraints on what you can call something, which may"
     "not be the same as BioLingua's FNAME contraints."
     "Example: "
     "(with-entity-encoding (in out) (mapcar #'out (mapcar #'in '(a s d f))))"
     "This executes an identity.  If you provide a :MAPFN it must take"
     "one arg which is the entity, and return an ID of your own choosing."
     "The default :MAPFN is just to count up from 1."
     )
  (let ((table (gensym "table"))
        (inmapfn (gensym "mapfn")))
    `(let ((,table (make-hash-table :test #'equal))
           (,inmapfn (if (symbolp ,mapfn) ',mapfn ,mapfn)))
       (flet 
           ((,infn (entity)
              (let ((id (funcall ,inmapfn entity)))
                (setf (gethash id ,table) entity) 
                id))
            (,outfn (mycounter)
              (gethash mycounter ,table)))
         ,@body))))

(defun extract-sequence+ (entity &key 
                                 (error-if-not-sequential? nil)
                                 (verbose? t)
                                 )
  #.(one-string-nl
     "Like extract-sequence, this will get the sequence information, if"
     "possible, from any sequence-like object (e.g., genes, proteins, etc.)"
     "but can also accept a string, in which case you get the string back."
     "This is generally used around external calls to get the sequences"
     "to be passed, for example, to blast.  If :ERROR-IF-NOT-SEQUENTIAL?"
     "is T (default=nil) then you'll get an error if this isn't the sort"
     "of thing that has a sequence (or is a string), otherwise you'll get"
     "NIL for non-seq entities.  Thus, To execute over a set, and drop those"
     "that are not sequence-like simply mapcarnn this over the entities."
     )
  (cond ((stringp entity) entity)
        ((symbolp entity) (copy-seq (string entity)))
        ((null error-if-not-sequential?)
         (or 
          (ignore-errors (extract-sequence entity))
          (when verbose?
            (formatt 
             "Dropped: ~s because it had no sensible sequence.~%" entity)
            nil)))
        (t (extract-sequence entity))))

;;; FFF This should be changed to use with-temp-file-in (jp's version), and/or
;;; all calls ot this should be changed to use that function instead, and then
;;; this version deleted.

(defmacro with-temporary-file (arguments &body body)
  (destructuring-bind 
      (type prefix-symbol tfname-symbol tfpath-symbol . more-args) 
      arguments
    (declare (ignore more-args))
    `(let* ((,prefix-symbol 
             (formatn 
              "~a-~a-~a" 
              (string wb:*username*) (get-universal-time) (random 10000)))
	    (,tfname-symbol (format nil "~a.~a" ,prefix-symbol ,type))
	    (,tfpath-symbol 
             (merge-pathnames cl-user:*tmp-directory* ,tfname-symbol))
	    )
       (unwind-protect
           (progn ,@body)
         (when *delete-temp-files*
           (when (probe-file ,tfpath-symbol) (delete-file ,tfpath-symbol))
           )))))

(defmacro with-temporary-fasta-file
          ((prefix-symbol
            fa-filepath-symbol
            fa-filename-symbol
            master-list-symbol 
            &key (safe? t) (translate nil) (sequence-length-limit nil) 
            (user-label nil))
           objects
           &body body)
  #.(one-string-nl
     "This macro creates a temporary fasta file, writes the sequences from the"
     "given OBJECTS to it, and binds up a number of user-given symbols that"
     "permit the body expressions to conveniently get to the file."
     ""
     "ARGUMENTS is of the form:"
     "(prefix-symbol fa-file-name-symbol"
     "    fa-file-path-symbol master-list . more-args)"
     "  prefix-symbol - A symbol bound to the random filename's prefix."
     "                  The filename itself will be <prefix>.fa"
     "  fa-filename-symbol - A symbol bound to the full filename of the file."
     "  fa-filepath-symbol - A symbol bound to the filepath of the .fa file."
     "  master-list-symbol - A symbol bound to the master list (see below)."
     "  "
     "More-args can include:  "
     "  :safe? t/nil -- If T (the default), then non-sequence objects are"
     "   dropped, and a warning is printed for each." 
     "  "
     "  :translate '(\"fromstring\" \"tostring..\")"
     "   In this case, fromstring and tostring must be of equal length"
     "   and any character in the FROMSTRING is tranlated to the matching"
     "   one in the tostring.  For example:"
     "      :translate '(\"*\" \"Z\")"
     "   will change all the stops (*) to the character Z.  This argument is"
     "   used rarely, for example to change stop codons so that Clustal will"
     "   honor them."
     "  :sequence-length-limit value"
     "  if value is non-nil it should be a positive integer.  If any sequence"
     "  exceeds this limit, an error will be signalled."
     "  "
     "Objects in the OBJECTS list can be either a string (interpreted as a"
     "sequence), or anything that can be passed to (EXTRACT-SEQUENCE ...)"
     "to get a sequence, or either of those prefixed with a user-given"
     "label, as: ((\"my label\" object1) (\"another label\" \"GATACA...\")...)"
     "The master-list is a list of elements, one for each OBJECT.  Each"
     "element is of the form: (key-number user-given-label object sequence)"
     "If safe?=t only safe objects will make it to this list."
     "The key-number is a sequence number assigned to the object and this is"
     "the value used in the FASTA file to indicate this object/sequence."
     "The reason for this arcane binding is that many programs have weird"
     "limitations on the length of sequence names.  This normalizes the names"
     "to all be three-digit numbers, and then the user's parser must figure"
     "out which things came back in what order. The user-given-label is just"
     "the object itself if the user didn't give a label.")
  `(let* ((xlate ,translate) (u-label ,user-label)
          (,master-list-symbol
           (canonicalize-temporary-fasta-file-objects 
            ,objects ,safe? ,sequence-length-limit))
          )
     (with-temporary-file 
         ("fa" ,prefix-symbol ,fa-filename-symbol ,fa-filepath-symbol)
       (with-open-file 
           (o ,fa-filepath-symbol :direction :output :if-exists :supersede)
         (loop for (key-number user-given-label nil sequence) 
               in ,master-list-symbol
               do 
               (when xlate
                 (let ((from-map (first xlate))
                       (to-map (second xlate)))
                   (cond 
                    ((and (stringp from-map) (stringp to-map)
                          (= (length from-map) (length to-map)))
                     (loop for from-char across from-map
                           as to-char across to-map
                           do (setq sequence 
                                    (substitute to-char from-char sequence))))
                    (t 
                     (error 
                      (one-string
                       "The FROM (~s) and TO (~s) translation maps aren't "
                       "valid in with-temporary-fasta-file.  They must be "
                       " strings of the same length!")
                      from-map to-map)))))
              (if u-label
               (format o ">~a~%~a~%" user-given-label sequence)
               (format o ">~3,'0d~%~a~%" key-number sequence))
))
       ,@body)))

(defun canonicalize-temporary-fasta-file-objects
       (objects safe? sequence-length-limit &aux result-list)
  (setq 
   result-list         
   (loop for object-or-name/object in objects 
         with key-number = 0
         for element-number from 1
         as object = (if (listp object-or-name/object) 
                         (second object-or-name/object)
                       object-or-name/object)
         as user-given-label 
         =
         (if (listp object-or-name/object)
             (first object-or-name/object)
           (if (and (stringp object-or-name/object)
                    (> (length object-or-name/object) 30))
               (formatn "Element ~D: ~A" 
                        element-number 
                        (limited-form-string 
                         object-or-name/object 30 :format-mode "~S"
                         ))
             object-or-name/object
             ))
         as sequence = (if (stringp object) 
                           object
                         (if safe? 
                             (ignore-errors 
                               (extract-sequence object))
                           (extract-sequence (second object))))
         if sequence
         collect (list element-number 
                       (incf key-number)
                       user-given-label
                       object
                       sequence)
         else do 
         (formatt 
          #.(one-string
             "Dropped: element ~d (~s) because we don't know how to "
             "extract a sequence from it.~%"
             )
          element-number
          (if (listp object-or-name/object)
              user-given-label
            (limited-form-string object-or-name/object 30 :format-mode "~S")
            ))))
  (when sequence-length-limit 
    (loop for (i nil user-given-label nil sequence) in result-list do
          (when (> (length sequence) sequence-length-limit) 
            (error 
             (one-string-nl
              "The ~:R sequence or object passed to Clustal exceeds"
              "the default or specified limit on the length of a sequence,"
              "~D.  The label of the offending object passed in is:"
              "~S")
             i sequence-length-limit user-given-label 
             ))))
  (mapcar 'cdr result-list)
  )
              




;;; This is probably completely wrong because I don't actually
;;; understand how XML works and I'm trying to warp it into something
;;; sensible in terms of frames, but there are weird cases, like:
;;; ((|ArticleId| |IdType| "pii") "S0003497504005235") which have no
;;; simple interpretation in terms of frames.  (If the article id is
;;; s000..., where does the idtype go?  If you make an articleid frame
;;; with idtype pii, what the slot name for S000...?  On the
;;; otherhand, if you make an articleid slot with arg s000..., where
;;; does the idtype pii go?  See, I have no idea what I'm doing! --  JS)
;;; As an example, type calling (pubmed-query "shrager").  

(defun xml->temp-frame (xml)
  (xml->temp-frame2 (remove-crap-strings xml)))

(defun frame-from-symbol (symbol &optional (force? t))
  (frame-fnamed (symbol-name symbol) force?))

(defun xml->temp-frame2 (xml)
  (cond 
   ;; <fred> blah blah blah </fred>
   ((and (listp xml) (symbolp (car xml)))
    (let ((temp-frame (make-temp-frame (frame-from-symbol (car xml)))))
      (loop for slot-spec in (cdr xml)
            do 
            (cond 
             ;; blah = <pet> "dino" </pet>
             ((and (listp slot-spec) (symbolp (car slot-spec)) 
                   (stringp (second slot-spec)) (null (cddr slot-spec)))
              (setf (slotv temp-frame (frame-from-symbol (car slot-spec)))
                    (string-to-number-or-original-string (second slot-spec))))
             ;; blah = <pet> blah blah </pet>
             ((and (listp slot-spec) (symbolp (car slot-spec)))
              (push (xml->temp-frame2 slot-spec)
                    (slotv temp-frame (frame-from-symbol (car slot-spec)))))
             ;; blah = <pet species='dinosaur' imaginary="t"> blah blah </pet>
             ((listp (car slot-spec))
              (let ((sub-temp-frame (xml->temp-frame2 slot-spec)))
                (push 
                 sub-temp-frame
                 (slotv temp-frame (frame-from-symbol (caar slot-spec))))))
             (t 
              (cformatt 
               "Oops: Dropped ~s on the floor in XML conversion (1)!" 
               slot-spec))))
      temp-frame))
   ;; <fred wife="wilma"> blah blah blah </fred>
   ((and (listp xml) (listp (car xml)))
    (let* ((non-lists (remove-if #'listp (cdr xml)))
           (lists (remove-if-not #'listp (cdr xml)))
           (temp-frame (xml->temp-frame2 (cons (caar xml) lists))))
      ;; CAN'T USE FIXED NAMES THAT WILL CONFLICT WITH LIKELY INCOMING NAMES (such as "#$DATA") ... D-OH!!
      (setf (slotv temp-frame #$xml.data) non-lists)
      (setf (slotv temp-frame #$xml.properties)
            (loop for (a b . nil) on (cdar xml) by #'cddr collect 
                  (list (keywordize (string-upcase (symbol-name a))) b)))
      temp-frame))
   (t (cformatt "Oops: Dropped ~s on the floor in XML conversion (2)!" xml))
   ))

(defun remove-crap-strings (xml)
  (car (remove-crap-strings2 xml)))
(defun remove-crap-strings2 (xml)
  (cond 
   ((and (stringp xml) (every (lambda (ch) (member ch *whitespace*)) xml)) nil)
   ((listp xml) (list (mapcan #'remove-crap-strings2 xml)))
   (t (list xml))))
	
(defun string-to-number-or-original-string (string)
  #.(one-string-nl
     "If STRING can be read as a number using READ-FROM-STRING the number"
     "read is returned, otherwise STRING itself is returned.")
  (let ((result (ignore-errors (read-from-string string))))
    (if (numberp result) result string)))

(defun cross-subsets (list)
  #.(one-string-nl 
     "Forms the lower triangle of the cross-product of a list."
     "(cross-subsets '(a s d f)) => '((a s) (a d) (a f) (s d) (s f) (d f))")
  (all-unordered-pairs list))

;;;

;;; If the constraints is a string, it is assumed to be a file
;;; <name>.cons in the user's directory.  All that we
;;; need to do to do that is to run the program in the right place.
;;; %%% Stephen's code takes a prefix and automagically adds .cons for
;;; the input, which is weird, so there's a little weird name-play
;;; going on here so that the user can call their constraints anything
;;; they like.  If constraints is a list (or, more precisely, not a string)
;;; then we create the temp file directly by dumping them instead of 
;;; copying it out of the user's directory.

(defun consolve (constraints &key (trials 10) (iterations 10))
  #.(one-string-nl
     "Solve-constraints interprets constraints representing background "
     "knowledge and produces a graphical 'model' that summarizes those "
     "constraints."
     "Basic usage: (solve-constraints \"constraint-filename\"), or" 
     "(solve-constraints list-of-constraints)"
     "Optionally you can tell how many trials to run (default is 10), "
     "by adding a :trials keyword.   The resulting graph appears as a list"
     "in the form that SEEGRAPH requires. (See documentation on SEEGRAPH)."
     )
  (when (stringp constraints)
    (setq constraints (string-downcase constraints))
    (unless (probe-file constraints)
      (error "In CONSOLVE, the file \"~a\" does not exist." constraints)))
  (bio::with-temporary-file 
      ("cons" temp-prefix temp-filename temp-path)
    ;; Either get the constraint file, or create it.
    (cond ((stringp constraints)
           (protected-shell-command
            (format nil "cp ~a ~a" 
                    (merge-pathnames *DEFAULT-PATHNAME-DEFAULTS* constraints)
                    (merge-pathnames temp-path temp-filename))))
          ((listp constraints)
           (check-and-dump-constraints 
            constraints (merge-pathnames temp-path temp-filename)))
          (t (error 
              (one-string 
              "In CONSOLVE, the constraints argument must be either a "
              "filename (string) or a list of constraints."))))
    (let ((consolve-command 
           (formatn
            "~a ~a -t ~a -i ~a >& ~a" 
            (cl-user:translate-simple-lp "biotools:cdisc;solve") 
            temp-prefix trials iterations (format nil "~a.log" temp-prefix))))
      ;; Now we can do the real work:
      (case (protected-shell-command 
             consolve-command :directory cl-user::*tmp-directory*)
        (:timeout nil)
        (otherwise
         (with-open-file 
             (i (merge-pathnames
                 cl-user:*tmp-directory* (format nil "~a.cdl" temp-prefix)))
           (read i nil nil))
         )))))

(defun check-and-dump-constraints (constraints file)
  (flet ((badcons (cons) 
           (error 
            "In CHECK-AND-DUMP-CONSTRAINTS, ~s is not a valid constraint!" 
            cons)))
    (with-open-file (o file :direction :output :if-exists :supersede)
      (loop for cons in constraints
	    do 
	    (unless (listp cons) (badcons cons))
	    (case (keywordize (car cons))
	       ;; FFF Should do more checking!!
	      (:correlated 
               (format o "correlated ~a ~a ~a~%" 
                       (second cons) (third cons) (fourth cons)))
	      (t (badcons cons))
	      )))))

(defun consolve-avg (constraints &key (trials 10) (navg 10) (iterations 10))
  #.(one-string-nl
     "Uses (CONSOLVE ...) multiple times and returns the average graph."
     "This can be passed to SEEGRAPH to visualize it.  The keyword :NAVG"
     "(default: 10) tells us how many calls to consolve are averaged together.")
  (avgraphs 
   (loop for i below navg collect 
         (consolve constraints :trials trials :iterations iterations))))


(defun seegraph (graph 
		 &key 
		 (mods nil)
		 (xinches 12) (yinches 8)
		 (display-slot #$fname)
		 (font cl-user::*default-dotty-font*)
		 rank-sets
		 )
  #.(one-string-nl
     "Use AT&T's DOTTY to visualize a graph, represented as a list."
     "The graph must be a list of edges of the form: (from -> to1 to2 ...)"
     "where each of from and to(x) is either a symbol or frame"
     "(in which case the #$fname slot is used). "
     "If the -> is replaced by anything else, then that is displayed as"
     "the edge label. (I.e., using -> means that there is no edge label.)"
     "The keyword :rank-sets lets you control which nodes appear on the same"
     "level of the graph. Example :rank-sets '((a b) (c d)) will force nodes"
     "a and b to appear on the same level, and c and d to appear on the same level."
     "The keyword :mods is list of Dotty parameters for the digraph. These are passed"
     "uninterpreted to Dotty. You need to know how to work Dotty to use these!"
     )
  ;; Turns out that the names of graph nodes have to be cleaned up some
  ;; for dotty.
  (flet ((nameof (obj) 
           (substitute 
            #\_ #\-
            ;; this cleanup is unfortunately bcs it changes numbers,
            ;; like: 3.141 to: 3_141
            (substitute 
             #\_ #\. 
             (cond ((isframe? obj) (slotv obj display-slot))
                   ((symbolp obj) (string obj))
                   (t (format nil "~a" obj)))))))
    (with-temp-file-in (dot-path *tmp-directory* :type "dot" :delete? nil)
      (with-temp-file-in (jpg-path *webtmp-directory* :type "jpg" :delete? nil)
        (with-open-file (o dot-path :direction :output)
	  ;; Translate graph edges to ines as: OPN->OPN [label=0.58]
 	  (format o "digraph foo {~%")
	  (loop for (from arrow . to*) in graph 
		do (loop for to in to* 
			 do (format o "~s->~s" (nameof from) (nameof to))
			 (unless (and (symbolp arrow) 
				      (eq :-> (keywordize arrow)))
			   (format o " [label=~s]" (nameof arrow)))
			 (format o "~%")))
	  (loop for set in rank-sets
		do (format o "{rank = same; ")
		(loop for item in set
		      do (format o "~s; " item)))
	  (when mods 
	    (loop for mod in (if (listp mods) mods (list mods))
		  do (format o "~a~%" mod)))
          (format o "size=\"~a,~a\";}~%" xinches yinches))
        (let ((seegraph-command 
               (if font
                   (formatn "dot -Nfontname=~s -Efontname=~s -Tjpg -o ~a ~a" 
                            font font jpg-path dot-path)
                 (formatn "dot -Tjpg -o ~a ~a" jpg-path dot-path))))
	  (case (protected-shell-command seegraph-command)
            (:timeout nil)
            (otherwise
             (wb::make-jpg 
              :path (format nil "~a~a.~a" cl-user::*webtmp-url* 
                            (pathname-name jpg-path) (pathname-type jpg-path)))
             )))))))

(defun avgraphs (graphs)
  #.(one-string-nl
     "Given a list of graphs in cdisc standard output format, creates an"
     "average graph, that is, one where each edge is given in terms of"
     "the fraction of times that that edge appears in the given graphs."
     "See 'seegraph' for help on graph format."
     "Edges must be of -> type -- that is, numbers or other symbols are"
     "ignored on the edge labels.")
  (let ((edge-table (make-hash-table :test #'equal)))
    (loop for graph in graphs
	  do (loop for (from nil . to*) in graph
		   do (loop for to in to*
			    as edge = (cons from to)
			    as count = (gethash edge edge-table)
			    do (if count (incf (gethash edge edge-table))
				 (setf (gethash edge edge-table) 1.0)))))
    (loop with sum = (length graphs)
	  for (from . to) being the hash-keys of edge-table
	  using (hash-value count)
	  collect (list from (/ count sum) to))))

;;; Just a testing function for all of the above gunk.

(defun test-graph-hacks (&key (symbols '(a b c d e)) (ngraphs 10) (nedges 4))
  (let* ((graphs 
          (loop for n from 1 to ngraphs
                collect 
                (loop for e from 1 to nedges
                      collect 
                      (list (nth (random (length symbols)) symbols) 
                            '-> 
                            (nth (random (length symbols)) symbols)))))
         (av (avgraphs graphs))) 
    (print graphs)
    (print av)
    (seegraph av :resolution 100)))

;;;

(defun google-search (string* &key (hook-to-frames? nil))
  #.(one-string-nl
     "Given an arbitrary string or list of strings, return a set of frames"
     "that represent the result of passing these google via its API."
     "The key :hook-to-frames? (default nil)" 
     "asks the system to try to bind the results into the frame system,"
     "which can take a long time.")
  (block exit
    (with-temporary-file 
        ("cons" temp-prefix temp-filename temp-path)
      (let* ((googledir (cl-user::translate-simple-lp "biotools:googleapi;"))
             (google-command 
              (formatn
               (one-string
                "/usr/java/j2sdk1.4.2/bin/java "
                "-cp googleapi.jar com.google.soap.search.GoogleAPIDemo "
                "dW4nEFRQFHI4YYAPKc3ZHGMhicLSue0I search ~s > ~a")
               (cond ((listp string*)
                      (loop with result = ""
                            as word in string*
                            do (setq result (format nil "~a ~a" result word))
                            finally (return result)))
                     (t (format nil "~a" string*)))
               (merge-pathnames cl-user:*tmp-directory* temp-filename)
               )))
        (case (protected-shell-command google-command :directory googledir)
          (:timeout (return-from exit nil))
          (otherwise nil)))
      (with-open-file 
          (i (merge-pathnames cl-user:*tmp-directory* temp-filename))
        (loop for line = (read-line i nil nil)
              until (and count (zerop count))
              with count = nil
              with result = nil
              do 
              (cond ((and (null count)
                          (search "End   Index = " line))
                     (setq count (read-from-string (subseq line 13))))
                    ((string-equal "  [" line)
                     (push (parse-google-result i hook-to-frames?) result)
                     (decf count)))
              finally (return (reverse result))))
      )))

(defun parse-google-result (i &optional (hook-to-frames? nil))
  (loop 
   as line = (read-line i nil nil)
   with frame = (make-temp-frame #$google-result)
   with strings = nil
   until (char-equal #\] (aref line 2))
   as =pos = (position #\= line)
   do 
   (setf
    (slotv
     frame
     (frame-fnamed
      (read-from-string
       (substitute #\- #\space (subseq line 2 (- =pos 1)))) t))
    (let ((string (subseq line (+ 2 =pos))))
      (push string strings)
      string))
   finally 
   (progn (when hook-to-frames?
            (setf (slotv frame #$frame-bindings)
                  ;; shut compiler up on seed machines
                  (mapcar (symbol-function 'try-to-hook-text-to-frames) strings)
                  ))
     (return frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun external-executable-path
       (name 
        &key
        (absolute-path user:*biotools-directory*)
        (tools-directory name)
        (version-directory "current")
        (bin-directory "bin")
        &aux path
        )
  #.(one-string-nl
     "Returns the namestring of a pathname which consists, conceptually, of"
     "<absolute-path>/<tools-directory>/<version-directory>/<bin-directory>/<name>"
     "If TOOLS-DIRECTORY, VERSION-DIRECTORY or BIN-DIRECTORY are specified" 
     "explicitly as NIL, then they are omitted from the path."
     "If TOOLS-DIRECTORY is left unspecified the default is to use NAME."
     "By default, the path returned is (on nostoc): "
     "/usr/local/biotools/<name>/current/bin/<name>")
  (unless absolute-path 
    (error "You must provide an absolute path!"))
  (setq path absolute-path)
  (when tools-directory 
    (setq path (append-subdir path tools-directory)))
  (when version-directory 
    (unless tools-directory 
      (error "You specified a version directory without a tools directory."))
    (setq path (append-subdir path version-directory)))
  (when bin-directory 
    (setq path (append-subdir path bin-directory)))
  (namestring (merge-pathnames name path))
  )

(defmacro knowalk (firstframe path eltarg &body body)
  #.(one-string-nl 
     "Descends through a series of frames, from FIRSTFRAME, following"
     "the path defined by the list of slots in PATH (which should be the"
     "names of slots, NOT accessors.  That is, for example: #$SLOT).  At "
     "the end of the path, the ELTARG will be bound to each elements of"
     "the contents of the last given slot in the path, and the body will"
     "be executed with each successive binding of these elements."
     "Note that at each level, (ENSURE-LIST ...) is called on the "
     "contents of the slot so that singltons are treated as lists of one"
     "element.  Here's a example:"
     "  (let ((genes nil))"
     "    (knowalk reaction (#$OC.Enzymatic-Reaction #$OC.Enzyme #$oc.gene)"
     "      gene (push gene genes))"
     "    genes))"
     )
  (labels ((knowalk-expander 
	    (path)
	    (cond ((null path) `(progn ,@body))
		  (t `(loop for ,eltarg in (ensure-list (slotv  ,eltarg ,(car path)))
			    do ,(knowalk-expander (cdr path)))))))
    `(loop for ,eltarg in (ensure-list (slotv ,firstframe ,(pop path)))
	   do ,(knowalk-expander path))))


;;; Loading uses the file "loadable-tables.index" in the user's home directory.
;;; This is a file full of simple lists: (varname full-file-path) which you can 
;;; load either way and the var gets set. (FFF This should go into SIMPLE-TABLE.LISP)
;;; FFF Potential rationalization of all this: Any data, whether a table or not, 
;;; should be able to be shared and loaded by others (this makes less sense for
;;; up/downloading because it would have to be in serialized Lisp form)

(defun ensure-table (var-or-filename &key (force-reload? nil))
  #.(one-string-nl
     "(ensure-table 'varname) or (ensure-table \"filename\") loads data from"
     "either a shared or local simple table file,  such as those created by"
     "saving from Excel as tab-separated text."
     ""
     "There are two ways to get files set up to be loaded in this way:"
     ""
     "1. When you upload files, you can check a box indicating that the file should"
     "be made loadable by ensure-table, and you give the name of a symbol that will"
     "be set with the values of the result of a call to (simple-read-table ...)."
     "[See (help simple-read-table) for more info on the format.]"
     ""
     "2. Calling: (share-table 'varname \"filename\" data) will create a loadable table"
     "file in the (first) shared directory for this BioBike instance."
     "(Usually there's only one!)"
     ""
     "When ensure-table searches for the indicated table, it uses local copies before"
     "shared copies. That way you can override public data with local data."
     ""
     "The reason that this is called ENSURE-table is that it doesn't do the read"
     "unless the value of the target variable is nil, or has not been set at all."
     "You can force the read to take place by using the :FORCE-RELOAD? T keyword"
     "argument, as: (ensure-table var :force-reload? t)"
     ""
     "The file called LOADABLE-TABLES.INDEX in your home directory contains the"
     "index for ENSURE-TABLE. If you delete or otherwise modify this file, unpredictable"
     "behavior will result."
     ""
     "The special argument :LIST [as: (ensure-table :list) will do the obvious useful thing.")
  (let ((local-index-file (merge-pathnames 
			   (wb::visitor-directory *username*)
			   "loadable-tables.index"))
	(shared-index-file (merge-pathnames 
			    (first (wb::application-shared-files-directories t))
			    "loadable-tables.index"))
	)
    (case 
     var-or-filename
     (:list 
      (loop for index-file in (list local-index-file shared-index-file)
	    when (probe-file index-file)
	    collect (list index-file 
			  (with-open-file 
			   (i index-file)
			   (loop for entry  = (read i nil nil)
				 until (null entry)
				 collect entry
				 )))))
     (t 
      ;; Here we first try to look for a local, and then for a shared
      ;; database. Only fail hard if neither exists!
      (or
       ;; Look in the user's index file. This one just returns the
       ;; value or nil, so that we go on to the shared one if nothing's found!
       (when (probe-file local-index-file)
	 (with-open-file 
	  (i local-index-file)
	  (loop for entry  = (read i nil nil)
		until (null entry)
		do (unless (and (listp entry)
				(symbolp (first entry))
				(stringp (second entry))
				)
		     (error "An badly formatted index entry (~s) was read. Someone monkeyed with the table file!" entry))
		(when (or (and (symbolp var-or-filename)
			       (eq var-or-filename (first entry)))
			  (and (stringp var-or-filename)
			       (file/pathname-equalp var-or-filename
						     (second entry))))
		  (when (and (boundp (first entry))
			     (symbol-value (first entry))
			     (null force-reload?))
		    (format t "~a already has a value and so ENSURE-TABLE did not reload the table in ~a" (first entry) (second entry)))
		  (format t "Loading ~s into ~a~%" (second entry) (first entry))
		  (set (first entry)
		       (simple-read-table (second entry)))
		  (return (symbol-value (first entry)))
		  ))))
       ;; Now look in the shared directory -- here the format is a
       ;; little different (although we could probably do some code
       ;; sharing with the above).
       (when (probe-file shared-index-file)
	 (with-open-file 
	  (i shared-index-file)
	  (loop for entry  = (read i nil nil)
		until (null entry)
		do (unless (and (listp entry)
				(symbolp (first entry))
				(stringp (second entry))
				)
		     (error "An badly formatted index entry (~s) was read. Someone monkeyed with the table file!" entry))
		(when (or (and (symbolp var-or-filename)
			       (eq var-or-filename (first entry)))
			  (and (stringp var-or-filename)
			       (file/pathname-equalp var-or-filename
						     (second entry))))
		  (when (and (boundp (first entry))
			     (symbol-value (first entry))
			     (null force-reload?))
		    (format t "~a already has a value and so ENSURE-TABLE did not reload the table in ~a" (first entry) (second entry)))
		  (format t "Loading ~s into ~a~%" (second entry) (first entry))
		  (set (first entry)
		       (simple-read-table (second entry)))
		  (return (symbol-value (first entry)))
		  )
		)))
       ;; This will only happen if both of the above returned NIL.
       (error "No entry was found for ~a" var-or-filename)
       )
      ))))

(defun file/pathname-equalp (target filepath/name)
  "string-equal(p) where foo.bar matches with baz\frob\foo.bar too!"
  (or (string-equal target filepath/name)
      (let* ((shlashified-name (substitute #\/ #\\ filepath/name))
	     (last-/-pos (position #\/ (reverse shlashified-name))))
	(when last-/-pos
	  (string-equal target 
			(subseq filepath/name (- (length filepath/name)
						 last-/-pos)))))))

(defun share-table (varname filename data)
  #.(one-string-nl
     "(share-table 'varname \"filename\" data-as-a-list) creates a public (shared)" 
     "data file containing the indicated data table. This can be reloaded by any user"
     "via (ensure-table 'varname) or (ensure-table \"filename\")."
     "If the filename already exists, you won't be able to replace it,"
     "and only the system administrator can remove shared tables (at the moment!)"
     "See (help ensure-table) for more info."
     )
  (unless (and (stringp filename)
	       (symbolp varname)
	       (listp data))
    (error "Invalid arguments to SHARE-TABLE: Must be (share-table 'varname \"shared-filename\" table-data-as-list)"))
  (let ((datafile (merge-pathnames (first (wb::application-shared-files-directories t)) filename)))
    (simple-write-table datafile data)
    (with-open-file 
     (o (merge-pathnames (first (wb::application-shared-files-directories t))
			 "loadable-tables.index")
	:direction :output
	:if-exists :append
	:if-does-not-exist :create)
     (print (list varname 
		  ;; There must be a better way to do this:
		  (format nil "~a" datafile)
		  wb::*username*
		  ) o))
    ))

