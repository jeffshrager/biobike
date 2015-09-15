
;;; For more details on some of these ideas refer to the lang-spec.txt
;;; document in this directory.


("Ideas that may make it easier for beginners to write code:"

 ("Array indexing using [] notation. (syntax / compiler analysis)" 
  (Peter
   --
   "The effect on the language ecosystem is just bad.")
  (JP
   +
   "No one has demonstrated to me Peter's claim, and/or that the way"
   "I propose implementing this (using the reader to isolate '[' and ']'"
   "then doing a post-reader transform to AREF notation) is flawed."
   "Given this, if people are 'born' liking [] notation then let's give"
   "it to them..."
   )
  )

 ("Extend [] notation to any sequence, not just garrays" 
  (Peter
   -
   "If we add this syntax, okay, but we shouldn't add it.")
  (JP
   0
   "Seems like an obvious extension, and is very concise. x[3] where X is"
   "a list makes sense; x["foo"] where X is a hash table makes sense;"
   "x[1 2] where X is a list of lists or an array or a sequence of sequences"
   "makes sense."
   )
  )

 ("array indexing using arrays themselves as function objects (array i j) (semantics / compiler analysis)" 
  (Peter
   ++
   "Okay, so this was my idea so of course I like it.")
  (JP
   (+ 
    "I think this is cool but without Lisp1 semantics the potential for"
    "ambiguity is too great.  (let ((list (foo))) (list 3 4))"
    "What if (FOO) returns a two-dimensional array??"
    )
  )

 ("AREF allowed to take list of indices (syntax, semantics)" 
  (Peter
   --
   "I don't get what this buys.")
  (JP
   0
   "It buys not having to use APPLY when working with a list of indices."
   )
  )


 ("Removing the need for QUOTE where it is not necessary (syntax / compiler analysis)" 
  (Peter
   -
   "I think this is probably a bad idea that will lead to more
     confusion rather than less. It may also become less of an
     issue if we deemphasize list manipulation.")
  (JP
   +
   "Can anyone really make an argument that '(1 2 3) is really necessary"
   "vs (1 2 3) ??  Why <isn't> the evaluation rule 'If the first element"
   "of a list is a non-symbol / non-lambda, the list is treated as data. ?"
   "Extending the semantics of evaluation further is more questionable"
   "but these two relaxations of Lisp's rules seem totally obvious."
   )
  )

 ("Providing aliases (synonyms) for functions and keywords. (syntax / compiler analysis)" 
  (Peter
   -
   "I suspect this will be more confusing than helpful--it
    increases the amount of vocabulary that folks have to learn
    *and* they have to learn that some things that look different
    are really the same.")
  )

 ("Providing a more english-like syntax for function calls. (syntax / compiler analysis)" 
  (Peter
   +
   "I'm okay with this one though seeing the troubles students
    have with syntax I think they might be better off having to
    learn a single consistent rule rather than a bunch of
    syntactic details associated with each function.")
  (JP
   -
   "I think this is mainly for readability. As Peter says, I suspect this"
   "will actually inhibit learnability and writability. The idea of"
   "matching arguments to types (so that one needn't remember the order of"
   "arguments to NTH, say) makes this potentially less important."
   "I think this would require a lot of thought and actual 'experimental'"
   "results from users to really be able to determine whether it was good"
   "bad or indifferent."
   )
  )

 ("Not having to use #$ notation to refer to frames. (syntax / compiler analysis)" 
  (Peter
   ++
   "Sounds good to me. I think we should provide macros with
    meaningful names for translating from symbols to various
    kinds of objects (which may be frames underneath),
    e.g. (organism s6734)")
  (JP
   +
   "I worry about the blurring in users' minds between frames and symbols."
   "But maybe this is a silly worry."
   )
  )

 ("Removing the necessity to use ':' to denote keywords. (syntax / compiler analysis)" 
  (Peter
   0
   "Whatever. Tied up with the english-like syntax for functions.")
  (JP
   0
   "There are two almost distinct uses for keywords in Lisp: As essentially"
   "syntactic markers in function calls and as symbolic values (enumerations"
   "in C). As syntactic markers the ':' can be removed, although I believe"
   "the work involved in doing this 'right' turns out to be huge.  As"
   "symbolic values the ':' cannot reasonably removed."
   )
  )

 ("1-based arrays instead of 0-based arrays. (semantics)" 
  (Peter
   0
   "I can go either way on this though I suspect Jeff S's point
   about the need to index into upstream genes is important; if
   the beginning of a gene is at index 1 then where is the
   previous base-pair? 0 or -1?")
  (JP
   --
   "I think this is perhaps the worst idea from the perspective of"
   "efficiency and easy-of-fit back into Lisp, and it implies a large"
   "implementation effort. I suspect that if people are just told that,"
   "like almost every other programming language, counting starts at 0,"
   "they'll simply accept it."
   )
  )

 ("Handling multiple values more straightforwardly (syntax)" 
  (Peter
   ++ 
   "Okay.")
  (JP
   +
   "If it can be done cleanly."
   )
  )

 ("Equivalence of SETQ and LET for local variables (semantics / compiler analysis)" 
  (Peter
   --
   "This takes us way to far away from Lisp. If you want Python,
    you know where to get it. (I.e. incorporating this feature
    into a Lispy language would require rethinking way too much
    stuff. That said, the idea about outlawing the reuse of the
    same name for different lexical bindings in nested scopes is
    probably a good one--Java has that rule and it seems to work
    out well since it's almost always a mistake.")
  (JP
   0
   "I'm not convinced that 'incpororating this feature ... would require"
   "rethinking way too much stuff.' What if the compiler can do this"
   "semi-trivially with no nasty implications? I like Peter's suggestion"
   "of no nesting of lexical bindings."
   )
  )

 ("IF/THEN/ELSE syntactic sugar (syntax)" 
  (Peter
   + 
   "Fine. IF* rides again.")
  (JP
   ++
   "This seems to make a lot of sense for a NPB.")
  )

 ("Making the syntactic form of LOOP similar to the syntactic
   form of DEFINE-FUNCTION, using keywords to denote various
   'parts' of the LOOP (initialization, repeat code, body code,
   etc)" 
  (Peter
   -
   "Hmmm. I'm not sure I like either the DEFINE-LOOP or the"
   "DEFINE-FUNCTION syntax. But I don't have a good reason except"
   "for my sense of esthetics.")
  (JP
   0
   "I think the term 'DEFINE-LOOP' is a misnomer. Other than that,"
   "structuring the thing similarly to a DEFINE-FUNCTION may not be"
   "unreasonable."
   )
  )

 )

("Ideas that remove certain 'gotchas' and restrictions from Common Lisp"

 ("functions which can naturally map over sequences in an
      unambiguous way should do so. (semantics)" 
  (Peter
   -
   "This gives me the heebie jeebies but I'm not sure exactly why.")
  (JP
   "That's because you never used APL. Why shouldn't ABS work on a"
   "vector of numbers?"
   )
  )
 
 ("LOOP should be able to generically loop over sequences and any object
     which has an iterator method. (semantics)" 
  (Peter
   ++
   "Yes. As should mapping functions.")
  (JP
   ++
   "Good idea about mapping functions.")
  )

 ("More consistent LOOP syntax; removal of WITH vs AS confusion (syntax, semantics)" 
  (Peter
   ++
   "Sounds good. I'd just get rid of AS and keep WITH and FOR.")
  (JP
   +
   "As this seems to be a big problem, I'm for trying to create a more"
   "reasonable LOOP. I think it will be a lot of work."
   )
  )

 ("if a function accepts a sequence naturally and is given an atom,
     the atom should be converted to a singleton sequence unless there
     is some kind of semantic problem in doing so.
     (semantics)" 
  (Peter
   -
   "I'm not sure what this buys us. Need to see some examples
     of where it comes up.")
  (JP
   +
   "(union 'a '(b c d)) --> (a b c d)"
   "There are lots of places in our code we have to (or want to)"
   "call our little utility ENSURE-LIST for just this kind of thing."
   )
  (Peter
   "I guess I'm not sure the BPs are going to have a lot of need"
   "to do this. Of coures you know how I feel about BPs doing
   lots of list manipulation in the first place."))

 ("if a function accepts a character and is given a length-one string,
    it should use the single character of the string. (semantics)" 
  (Peter
   0
   "Actually I think if we want to simplify things we should
    just get rid of the character data type--just have strings,
    some of which are one character long.")
  (JP
   0
   "I like Peter's suggestion; again the problem is connecting to Lisp."
   "So, IIUC, in Peter's world, the reader would translate #\Space into"
   "a string with one character, a space. So the big problem is efficiency."
   "If every AREF into a string produces another string, the inner, inner"
   "loop of many functions will slow precipitously."
   )
  (Peter
   "(deftype bike-string () '(or string character))"
   "(typep #\Space 'bike-string) ==> T"
   "(typep " " 'bike-string) ==> T"
   "(defgeneric bike-print-object (obj stream))"
   "(defmethod bike-print-object ((obj t) stream) (cl:print-object obj stream))"
   "(defmethod bike-print-object ((obj character) stream) (cl:print-object (string obj) stream))"
   )
 )

("Ideas that help users debug:"

 ("Clear and obvious stack trace explanations (compiler output / internals)" 
  (Peter
   ++
   "Yes. In the Bike evaluator (interpreter or compiler) we
     should store a stack of the actual code forms so we can show
     a stack trace with the code as the user typed it.")
  (JP
   ++
   "This would be like being against motherhood and apple pie."
  )

 ("SWIPM.  Try to figure out what user is trying to do, especially 
      when a function or variable does not exist. (tools)" 
  (Peter
   ++
   "If this means something like the edit-distance restarts, I like it.")
  (JP
   ++
   "Yes, an even more sophisticated something like that."
   )
  )

 ("Catching and clearly explaining LOOP problems, especially
 failure to include action verbs (DO, COLLECT, SUM,
 etc). (compiler analysis)" 
  (Peter
   ++
   "Sure. Can be part of our reimplementation of LOOP")
  (JP
   +
   "Agreed. Just that, again, LOOP will be a lot fo work."
   )
  )

 ("error reports provide hyperlinks to possible help topics, doc pages, etc
     when language is used in a web-enabled capacity (e.g., Weblistener) (tools)" 
  (Peter
   ++
   "Okay.")
  (JP
   +
   "This will require lots of grad-student help creating the actual pages"
   "and links, and constant maintainance."
   "Providing the actual tools to do this should not be that hard."
   )
  )

 ("Analysis of previous errors and on-going analysis of errors to provide
     better error messages and help as time goes by. (tools, system staff)" 
  (Peter
   ++
   "Probably the key here is to build some infrastructure here
     that captures all errors with appropriate context and
     categorizes them in some useful ways so that folks can
     easily look at a bunch of related errors via the web interface.")
  (JP
   +
   "The downside is it's not clear anyone will ever look at this"
   "if we build it.")
  )

 ("Two modes:  error checking (default) or no error checking (optimization),
     controllable by user.  (Common Lisp has at least 16 possible modes,
     depending on the declared values of SPEED and SAFETY)" 
  (Peter
   ++
   "Sounds good.")
  )

 ("Better HELP facility (google-ize the doc strings and the documentation?)
     Glossary of biological terms. Remove matching scores; use word homology but not as main criterion
     for matching.  Incorporate wordnet synonyms ? (tools)" 
  (Peter
   ++
   "Okay. I think the first step is to provide a way to add
     structured documentation to functions, macros, etc.")
  )

 )

("Ideas for data structures"

 ("Generalized arrays (garrays) (semantics, libraries, compiler analysis)" 
  (Peter
   ++
   "Seems like a good idea. I might take this a step farther and just
     make everything a frame (or my suggested prototype-based objects.)")
  (JP
   0
   "This is all (this and the next three items) very vague now."
   "My concern is that it become an infinite time and idea sink."
   )
  )

 ("slices of garrays (syntax, semantics, compiler analysis?)" 
  (Peter
   0
   "I'm not convinced about the real need for this one
    yet. However I can see more specific data structures that
    would be amenable to slicing.")
  )

 ("Generalized sequences (a la Python?) (everything)" 
  (Peter
   ++
   "Yes. I think this will be a big thing. Especially if we
move toward a more functional (higher-order functions) style,
then the sequence functions should all work with some generic
protocol rather than being limited to just lists and vectors.")
  )

 ("Specific biologically important data structures such as BLAST-RESULT, FASTA-DATA (tools, libraries)" 
  (Peter
   ++
   "Intuitively this seems like goodness to me. I haven't
     looked yet at what those tools actually return to have any
     sense of what a good representation would look like.")
  )

 ("No manipulation of lists per se within BioPike proper (?)" 
  (Peter
   ++
   "You know I like this since I keep harping on it.")
  (JP
   --
   "I think this is a pipe dream. Lists are just too useful and handy."
   "If they're not available, people will invent them horribly."
   )
  )

 ("Creation of a model of a biologist's world (cf GMOD)" 
  (Peter
   ++
   "I'm not suggesting we go nuts with this right away. However
     to the extent that the biologists are continually
     hand-rolling 'objects' out of nested lists or tables or
     whatever, we should try and help them find domain-meaningful
     abstractions and fit them into the language/libraries.")
  )
 (JP
  +
  "Seems reasonable. Again, the concern is of an endless sink."
  )
 )

("Other ideas"

 ("compilation by default and always (compilation issues)" 
  (Peter
   ++
   "Using JIT technology, yes. So this could just as well be
     'interpretation by default and always.' ;-)")
  )

 ("compiler understands lexical environment (compilation issues)" 
  (Peter
   ++
   "Sure. Though if the primary motivation for this is to
     massively overload the evaluation rule for bare symbols I'm
     not sure *that's* a good idea.")
  )

 ("compiler understands BIKE environment (semantics, compilation issues)" 
  (Peter
   ++
   "Fine. Though again I'm less interested in using this
     understanding to affect how symbols are evaluated. (Mostly
     because that also requires the programmer to understand the
     entire BIKE environment in order to understand what a program means.")
  )

 ("just-in-time compiler (compilation issues)" 
  (Peter
   ++ 
   "Yup, nothing
 but goodness there. Regarding some of the commentary in
 lang-spec.txt: I think the JIT should be quite aggressive in
 checking code and should keep a list of 'inconsistencies' but
 not bug the user too much. For example, if I define 
 (defun foo () (bar))
 it'll immediately check that BAR is defined. If
 not, it'll add an entry that says 'FOO calls undefined function
 BAR' to it's collection of inconsistencies and link from the FOO
 function object to that entry and also from the symbol BAR. Then
 when I define a function BAR, that entry is found (via the BAR
 symbol) and removed from the global collection of
 inconsistencies and from FOO's list. If I try to invoke FOO
 before all of it's inconsistencies have been removed the runtime
 will signal an error that tells me about the
 inconsistencies (possibly providing a restart to allow me to
 proceed anyway in case the inconsistency is in a code path that
 won't actually be taken.) There should also be a way to get a
 list of all the inconsistencies in the system or all the
 inconsistencies related to some particular definition in order
 to fix them. For example, if I change the arity of a function,
 the system should add an inconsistency for every place that
 calls the redefined function and I should be able to get that
 list of functions and easily edit the code in order to fix the
 call sites. If we really wanted to go to town, we could provide
 some mechanism for automatically fixing certain inconsistencies;
 renaming would be one such mechanism--if I rename BAR to BAZ I
 might have an option to automatically change the name at every
 call site without having to edit them individually.)")
  )

 ("decent web-based editor (tools)" 
  (Peter
   ++ 
   "Sounds good. Pretty sure this is a whole lot of work.")
  (JP
   +
   "Yes, lots of work. Ajax technology might make it possible; I don't"
   "whether it would be user-responsive enough.")
  )

 ("annotation-specific functions and tools (tools, libraries)" 
  (Peter
   0 
   "Not sure what this means.")
  (JP
   0
   "This is a catch-all for the interaction to the SEED part of the project."
   "Probably requires its own design/discussion document. No ratings or"
   "opinions make sense at this time."
   )
  )

 ("'Live tutorial' introductions that 'jump-start' the non-programmer 
     biologist (tools, system staff)" 
  (Peter
   0 
   "Okay.")
  )

 ("Better way to describe patterns (regular expressions)" 
  (Peter
   +
   "I think the idea we discussed the other day of creating
     'composable' regexps might have some legs. We should think
     about that more.")
  (JP
   +
   "I've given Mark the task of seeing whether we can make use of the"
   "code in AIMA (he's currently taking Berkeley's Intro to AI course)"
   "on natural language parsing to come up with something that we could"
   "use to translate quasi-english into regular expressions."
   "Apparently there is sort of crude form of composability in existing"
   "Perl regular expressions using named patterns or something."
   )
  )

 )

