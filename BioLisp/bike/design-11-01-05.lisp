
#|

Biobike language design document.  11-01-05.

Toplevel topics: 

  -- Syntax 

  -- Frame conversion

  -- Evaluation algorithm

  -- File loader

  -- Define-function

  -- Rewrite of biolite functionality 

  -- Help 

  -- Loop 

  -- Changes to common lisp function semantics

  -- Things no longer thought useful, or practicable given resources,
     or otherwise (?)

General discussion: By creating syntax and semantics which are
different than common lisp, we make it difficult for this language
to be used in a standard lisp environment -- a biobike REPL would
have to be provided if someone wanted to just load up a lisp and try
to run biobike code.  (The weblistener will be such a REPL, but it is
very specialized and is not designed to execute code in a standard
lisp environment, so one would have to be specially written if this
functionality were desired. It is not known to be desired)


 =============================================================

I. Syntax 

Three syntactic deviations from common lisp are proposed:

1) []  

[] notation is transformed via a post-read transform into standard lisp 
notation, namely calls to REF.  

Examples: 

(+ a[2] 3) --> (+ (ref a 2) 3)

(setf h[:foo] :bar) --> (setf (ref h :foo) :bar)

where REF and (SETF REF) are generic functions that work on lists,
strings, vectors, arrays, frames, and hash tables. (Other methods on
other types of objects can be defined)
 
2) {}

{1 2 3} --> (list 1 2 3) 

3) Infix designation 

We may specify some designators (such as #{ and #} ) to denote the
start and end of an infix expression.

Whether or not infix is a good idea and then whether or not we will
implement it is not yet clear.  Further, exactly what the designators
are and what kind of expressions would be allowed is not decided.



II. Frame conversion

There will exist a frame environment which is a mapping from symbol
names to frames.  (The current system can be thought of as having a
null frame environment.)

Conceptually, any symbol that is not lexically bound and is found in
non-function position in a biobike form will be looked up in this
frame environment mapping, and if found replaced by the frame it maps
to.

Furthermore, any symbol with a '.' in its name is considered to be a
frame reference (unless it is being bound in which case it is an
error).


A.  General Implementation

There are two practical ways this might be implemented: 

  -- The Shrager algorithm: For any form, find all the symbols in that
form and see which ones are in the mapping and which contain '.'s.
Then wrap the form with a LET which binds these symbols to the frames
they map to.

Examples:

(slotv all1234 .from) --> 

(let ((all1234 #$npun.all1234) (.from #$from))
  (slotv all1234 .from))

;;;;;

(let ((all1234 5)) (+ all1234 10)) --> 

(let ((all1234 #$npun.all1234)) (let ((all1234 5)) (+ all1234 10)))

Notice that since in the original form all1234 is lexically bound
the outer binding caused by the Shrager algorithm has no effect.  

;;;;;

(defun foo () all1234[.direction]) -->

(let ((all1234 #$npun.all1234) (.direction #$direction))
  (defun foo () (ref all1234 .direction)))

Notice that this creates a closure.

Discussion of Shrager algorithm:

The algorithm will not bind symbols 'generated' by macros, because no
macro-expansion is done on the form or its subforms.

In some cases the algorithm binds unnecessarily, such as when a
symbol is used in function position or is lexically bound.  Using 

  (declare (ignorable ...))

will prevent any warnings from being issued by the lisp compiler in
these cases.

  -- Code-walk algorithm: A code walk of the form, keeping track of
variables that are lexically bound and doing macroexpansion 'all the
way down'.  Instead of creating an outer LET form around every form, a
substitute form is generated which contains the actual frames the
symbols map to.

Examples:

(slotv all1234 .from) --> (slotv #$npun.all1234 #$from)

;;;;;

(let ((all1234 5)) (+ all1234 10)) --> 	(let ((all1234 5)) (+ all1234 10))

;;;;;

(defun foo () all1234[.direction]) --> 

(defun foo (ref #npun.all1234 #$direction))

Discussion of Code-Walk algorithm: 

To do this a code walker would have to be written that keeps track
of lexical variable scope.  


B. Biobike implementation

The frame environment can contain any mapping it wishes, but the
proposed environment specific to Biobike will contain only aliases for
protein and gene frames and possibly those symbols which are now
nicknames for organisms.  In particular, common slot frames such as
FROM, TO, and DIRECTION are NOT in the mapping.  This means that the
biobike user must use #$ or . notation to refer to these slots/frames.

(If a biobike user wishes to refer to these slots directly by name,
using #$ or '.' notation, or they will have to do the equivalent of

(setq from #$from)

either at toplevel or in their initialization file.  Of course, doing
the latter they would then have to be careful not to reset the value
of their 'from' variable or create a lexical binding around a
reference to 'from' which was meant to be a frame reference.

(Of course the mappings in this environment can be changed if
necessary; the basic glitch is that is a name like FROM is added to
the mapping and the user tries to do (setq from 5) then he's totally
screwed).

C. Local frames (unresolved)

In previous designs we had the concept of local frames: e.g., if a
user typed #$my-frame he would actually create a frame
#$jelhai.my-frame (assuming he was user jelhai).  This prevents two
users from smashing each other's frames if they happen to both use a
common name like #$Dog.

We could keep this concept, which would involve some changes to the
frame creation algorithms in the frame system, or we could discard the
notion, keeping the existing paradigm of global frames without
prefixes.

This interacts with the frame conversion implementation in that we
might want local frame names treated as symbols to become part of the
user's frame mapping environment.

Another issue is how such frames get created -- do they get created
using #$ notation or only by some kind of function.  Examples
illustrating the possible different types of behavior:

1.

> #$my-frame 
#$jelhai.my-frame 

> (slotv my-frame .fname) 
"jelhai.my-frame"

vs. 

2.

> #$my-frame
#$my-frame  ;;; or an error, if we don't allow users to create global frames
> (new-frame "my-frame")  
#$jelhai.my-frame

> (slotv my-frame .fname) 
"jelhai.my-frame" 

vs.

3.

> #$my-frame 
#$my-frame 

> (slotv my-frame .fname) 
ERROR: my-frame is an unbound symbol

vs. 

4.

> (new-frame "my-frame")  
#$my-frame

> (slotv my-frame .fname) 
ERROR: my-frame is an unbound symbol

In the first case, a local frame gets created when the user types it
using #$ notation (assuming it hasn't already been created), and once
created it becomes part of the 'local' frame environment. 

The second case is similar to the first, except the intent is that a
local frame can only be created using NEW-FRAME.

In the third case, using #$ notation creates a global frame, not a
local frame, which does not become part of the 'local' frame
environment or mapping.

In the fourth case, even using NEW-FRAME does not create a local frame
(the idea here is that if you want a 'local' frame you'll use your own
prefix).

Note that if users can use #$ notation instead of NEW-FRAME to create
frames then the frame is created at read time instead of execution
time.  If local frames are allowed and become part of the frame
mapping then this distinction may prove troublesome at times in
determining the meaning of code:

(let ((x #$my-frame)) 
  (slotv my-frame .fname))  

vs.

(let ((x (new-frame "my-frame"))) 
  (slotv my-frame .fname))

The first case works because MY-FRAME has been created at read time
and put into the frame mapping before the form is otherwise processed.  

In the second case, the frame is not in the map at the time 
the form is processed.  



III. Evaluation

Under this proposal Biobike does not have its own complete evaluator.
The Biobike algorithm for processing a form is simple
  -- do some kind of code and/or syntax tree walk to convert
[] notation into REF's and SETF REF's, and convert frame
references into frames, returning a new form F'.
  -- Call the standard Weblistener evaluation algorithm
on F' (which involves compilation and then execution of F')

In terms of the Weblistener itself, which receives a string from
the user, not a form, an additional thing must be done:

  -- Process the string in its entirety:  If a string such as

"a[2]" 

is entered and READ were called (as is done by the Weblistener now)
then READ would return simply 'a' and not all four tokens 'a', '[',
'2', and ']'.  Therefore the string -> form algorithm must be
augmented to deal with cases like this.

Consequences: 

  -- This means that no function defined with DEFINE-FUNCTION (see
below) can be funcalled or applied, since these are really macros.
(If a real biobike compiler and evaluation algorithm existed, a way
could probably be found to allow this).

Note: The user can always wrap a LAMBDA just as any Lisp user
wraps a lambda around a macro he needs to 'funcall'.

  -- Unless a code walking option is used, there is no way to
implement an understandable stack trace mechanism as is currently now
being done with XLOOP; we would have to rely on lisp's stack tracing
mechanism which is to some extent unacceptable because it is
unintelligible to mere mortals.

Note: It is possible to implement a very stripped-down code walker to
do this one task -- i.e, inserting stack trace information where
appropriate.  Such a code walker does not necessarily have to keep
track of lexical scope nor even macroexpand as long as it understood
how to walk all of Lisp's special forms and macros.

Note: Another possibility is to try to capture, parse, and make
sense out of the existing Allegro stack trace information.


IV. File loader.  

Since biobike code is not lisp code, the standard LOAD and
COMPILE-FILE functions cannot be used to deal with files containing
such code.

Furthermore, it is probably necessary for a user in the biobike
environment to be able to load files of standard lisp code, and it
would be nice if this were transparent.  Unfortunately, this seems to
be impossible.

There are a couple of ways of dealing with this: 

1.  Use a different file type for biobike code files, such as .blisp
or .bl or .bike.

2. Use some kind of indicator within the source file to indicate
whether it is biobike code or standard lisp code.  Jeff S's suggestion
is to use the presence of a mode line at the top of a file to indicate
that it is standard common lisp code; if no mode line exists, then 
in the biobike environment the file would be treated as biobike code.  

The biobike environment needs to have its own LOAD function because it
must process forms in a file according to the biobike processing
algorithms (frame names and bracket notation) before calling EVAL on
the forms.  Furthermore biobike should have no concept of compilation
or .fasl files because biobike's LOAD function should do all the
necessary processing.

However, in order to do the necessary processing the biobike LOAD
function must do two passes over biobike source code files.  This is
because functions defined with DEFINE-FUNCTION can call each other,
and as discussed above, these functions are actually macros.
Therefore, the biobike loader needs to find all the DEFINE-FUNCTION
forms in a file and create 'stubs' for the definitions so that the
bodies of all the DEFINE-FUNCTION forms can be compiled in the second
pass.  


V. Define-function

DEFINE-FUNCTION is discussed in detail in new-evaluation.html.  Here
are the main proposed features, taken from that document:

    * Handling of keyword arguments including allowing use of bare
symbols and synonyms.
    * "Flag" arguments--keyword arguments that take now subsequent
argument but are simply provided or not (i.e. their value is always T
or NIL.)
    * Runtime checking of argument types and generation of meaningful
error messages.
    * Runtime coercion of arguments by type.
    * Automatically generating documentation of allowed argument types.
    * Providing infrastructure for better error messages.

Other features that have been discussed but did not make it into this
list are:

    * Function Nicknames (aliases)
    * Syntactic sugar (prepositions which have no semantic consequences)
    * Implicit mapping (e.g, if a function is defined to take a gene,
and it is given a list of genes, map the function over the list)


VI. Rewrite of biolite functionality.  

Once the new language is in place (including DEFINE-FUNCTION) the
existing biolite functionality will need to be rewritten so that it
will be executable within the new language.

Note: There are some 10,000 lines of code in the two main biolite
source code files.



VII. Help

A new help facility and various add-ons (like live-tutorials) have been
suggested and are currently being implemented or under discussion.  



VIII. Loop

A new restricted loop facility has been proposed and implemented.  

A higher-level macro which turns Elhai-syntax loop (see DEFINE-LOOP
in wish-list.txt) has not been written.



IX. Changes to common lisp function semantics

At least three sets of changes have been proposed:  

1) STRING-ELEMENTS: making elements of strings behave like strings of
length 1 instead of characters.

2) 1-BASED: making all sequence operators that take indices be 1-based
instead of 0-based

3) IMPLICIT MAPPING: making functions which work on atoms exclusively
also work on sequences using an implicit MAP.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

The motivation for STRING-ELEMENTS is Jeff Elhai's suggestion that 

(xloop for j in "abc" collect j) 

should produce 

("a" "b" "c")

instead of 

(#\a #\b #\c)

Presumably this would imply that 

(ref "abc" 1) --> "a" instead of #\a

and that, for consistency, something like 

(map nil (lambda (x) (print x)) "abc") 

produce 

"a" 
"b"
"c"

instead of 

#\a
#\b
#\c

also, for consistency, the semantics of various search functions would
need to change:

(find "a" "abc") --> "a" instead of producing NIL

(count "x" "xxx") --> 3 instead of NIL 

To be completely consistent AREF, SVREF, CHAR and SCHAR and their SETF
partners would have to be modified when dealing with strings along
with REF.

The way to do these semantic changes is of course to shadow common
lisp function in biobike and redefine an extended or changed version
of the function.  This generally involves another level of function
call but can also result in the common lisp compiler being unable to
optimize certain expressions based on its knowledge of the semantics
of various common lisp functions.  

Another approach to this is to ignore common lisp functions that
return elements of strings; leaving it to the unsuspecting biobike
user to figure out that if and when he uses any common lisp functionality
it may or may not conform to biobike semantics.  (This comment applies
as well to the next section).   

The consequence of doing this is signficant: algorithms which loop
over strings in their inner loops could suffer degradations of 5-7
times, and garbage collections will be triggered much more often.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


The motivation for 1-BASED is that Jeff Elhai believes that NPBs
will find 1-based indexing much more intuitive and in fact would
possibly find 0-based indexing bad enough that they would be tempted
to give up attempts to use the system.  

To implement 1-based indexing consistently would mean changing every
common lisp function that deals with sequences and their indices.
These would include

AREF, NTH, ELT, CHAR, SCHAR, ROW-MAJOR-AREF, BIT, and SVREF and their
associated SETF functions.  

Also the return value of things like 

POSITION, COUNT, SEARCH, and their -IF and -IF-NOT cousins would
need to be adjusted.

Any function that takes START and/or END index arguments would
also have to be fixed:  SUBSEQ, REMOVE, DELETE, FIND, etc.

Also there is the issue of PPCRE which returns matched indices 0-based.  

Also every biolisp utility function which takes or returns indices
would have to be shadowed and redefined.

The redefinitions are generally easy: take the index argument,
subtract 1 from it, and call the corresponding lisp function.  Of
course, this again introduces another level of function call and in
many cases prevents optimization by the lisp compiler.  

Another approach is as above:  Leave the unsuspecting BioBike user
to their fate when attempting to use Common Lisp functions.

Another approach would be to use compiler macros instead of shadowing
functions.  This might be a significant amount of work but would
result in much faster code as the Lisp compiler would be able to
optimize it.

;;;;;;;;;;;;;;;;;;;;;;;

The motivation for IMPLICIT-MAPPING is the idea that there is no
reason why a function which works solely on atomic objects when given
a sequence could not map over those objects. 
Examples: 

(abs #(1 -1 -2)) --> #(1 1 2) 

(+ '(1 2 3) '(4 5 6)) --> (5 7 9)

This idea could also be extended to operations on n-dimensional arrays
and to operations involving scalars and sequences  
Example:

(+ 1 '(1 2 3)) --> (2 3 4)

Again, the consequence of doing this kind of thing is additional
overhead for the simple cases and the inability of the lisp compiler
to optimize things like arithmetic since it no longer understands the
arithmetic operators (the user would have to use things like lisp:+ to
get the compiler to do any optimization).

There are many lisp functions that could be 'vectorized'; many of which
are probably completely useless to biologists such as SINH.  


X.  Things no longer thought useful, or practicable given resources,
    or otherwise:

  -- Ranges.  

  -- Slices.  

  -- Non-line-noise way to expression regular expressions.



=============================================================================

Status: 

Syntax:

[] is implemented; processing needs to be integrated into weblistener.
The generic functions REF and (SETF REF) have a prototype implemetation.  

{} is not implemented yet but relatively trivial 

Infix: We have one variant of an infix->prefix translator which works; 
but we have not defined what we want or whether we want it.  It might
be necessary to modify the translator to do what we want it to, which
might involve a fair amount of work understanding it.  


Frame conversion:

No work done on this yet. 


Evaluation algorithm: 

As noted above, [] processing is implemented.  No work on the proposed
frame conversion algorithm / code analyzer has been done.


File loader:

No work done on this yet.   


Define-function:

A preliminary macro has been implemented.  However, the full spec for
DEFINE-FUNCTION does not yet exist.  DEFINE-FUNCTION is also tied up
with HELP and various subcapacities such as automatic type conversion
utilities.  In other words, this is a significant amount of effort.  


Rewrite of biolite functionality:

No work done on this yet.  


Help:

Live-tutorial infrastructure created.  DWIM facility for misspellings
implemented.  Help facility itself needs significant overhaul;
miscellaneous other things: see help-ideas.txt 


Loop:

Implemented.  Needs to be integrated into biobike language and/or
biolisp.  Various optimizations and improvements are possible.
DEFINE-LOOP uber-macro not implemented.


Changes to common lisp function semantics:

A small amount of code to illustrate vectorization has been done.  

Otherwise nothing has been implemented.  


============================================================================

Specific tasks with (very crude) time estimates:

Create Weblistener BioBike REPL hooks:

  2 days.

Define {} syntax an hook in new readtable with {} and []

  1/2 day

Infix hacking:

  2 days (?)

Creation of biobike frame environment algorithm:

  1 day

Modification of frame creation algorithms to create
local frames (both current and new frame systems):

  1 day

Shrager analysis algorithm:

  1 day

Full lexical-scoping / macroexpanding code-walker:

  5 days

Simple code walker for stack trace info:

  2 days

Stack trace mechanism and error reporting with stack traces:

  3 days

Miscellaneous problems with frame resolution algorithms:

  2 days

Biobike file loader:

  4 days

Issues having to do with printing biobike forms in style
they were read in ([]'s)

  3 days

Specification of full DEFINE-FUNCTION capabilities:

  2 days

Implementation of full DEFINE-FUNCTION capabilities:

  10 days

Rewrite of biolite functionality:

  30 days

Infrastructure for new HELP system:

  10 days

Creating new HELP doc strings, web pages, etc

  45 days

XLOOP optimizations:

  2 days

DEFINE-LOOP

  1 day

XLOOP documentation

  1 day

XLOOP help page, live tutorial

  3 days

Changes Common Lisp semantics:

  Unknown, range from a couple of days to a month.

Miscellaneous unexpected problems:

 5 days

-------------------------------------------------------

Number of days for basic functionality:  50 x 8 = 400 hrs.

Number of days for changes to Common Lisp Semantics:  unknown, 3 - 30

Number of days for biolite rewrite 30 x 8 = 240 hrs.

Number of days for documentation effort: 50 x 8 = 400 hrs.

|#
