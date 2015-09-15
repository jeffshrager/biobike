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

                MAKING BIOLINGUA WORK WITH LISPWORKS


NOTES FOR LISPWORKS 4.4 (Beta currently)

We'll assume, for the sake of having a particular name, that you've
installed the Biolisp files in the

  C:/Lispcode/Biolisp/

directory.  If you've installed it somewhere else, then just substitute
your directory appropriately in what follows.

Also, we'll assume that if you've installed Portable Allegroserve, it's
installed in

  C:/Lispcode/portableaserve/

and if you've installed CLOCC, it's installed in

  C:/Lispcode/CLOCC/

Again, use appropriate substitutions.

Lispworks does not come with primitives to access the World Wide Web,
nor with primitives to read and write XML.  Most importantly, it does
not come with AllegroServe, which is necessary to use the Weblistener.

It is conceivable that you could use some of the BioLingua software
without using the Weblistener, but we will leave that, as they say,
as an exercise to the reader.


                     OBTAINING WWW and AllegroServe


The Portable Allegroserve package is one way to obtain this functionality.
Here are step by step instructions for obtaining it, installing it and
making it work smoothly with the BioLingua code.

STEP 1.  Download the package.

  Start here: 

  http://sourceforge.net/projects/portableaserve/

  or here is a page with a link to the actual tar-gzipped file:

  http://sourceforge.net/project/showfiles.php?group_id=32760&release_id=92707


STEP 2.  Unzip and untar the downloaded file.  For instance, to put
the package into the

  C:/Lispcode/portableaserve directory

  -- create that directory
  -- move the downloaded file (call it xyz.tar.gz) into C:/Lispcode/
  -- gunzip it  (gunzip xyz.tar.gz)
  -- untar it   (tar xvf xyz.tar)
  
There is one change you need to make.  In the file

.../acl-compat/packages.lisp

Change the DEFPACKAGE at the top to begin like this:

(defpackage :acl-compat.excl
  (:use #:common-lisp
        #+cmu #:ext
        #+clisp #:ext
        #+sbcl #:sb-ext #+sbcl #:sb-gray
        #+cormanlisp :excl
        #+mcl :ccl
        )
  #+:lispworks4.4 (:nicknames :excl)

The last line is the one you need to add.

(This makes it so we can use the syntax EXCL: and EXCL:: in the BioLingua
code without regard to whether we are running on Allegro or Lispworks)


STEP 3.

In the file .../BioLisp/webload.lisp look at the code conditionalized on
Lispworks4.4.  

You need to modify the value of *portableaserve-path* appropriately.
Do so and save the file out.

STEP 4.  Find your LispWorks executable.  On my Windows machine, this is in

C:/Program Files/Xanalys/LispWorks4.4/lispworks-4400.exe

Start it up.

STEP 5.  

In the Lisp Listener, type

(load "C:/Lispcode/Biolisp/blload.lisp")

This should compile and load all the software, including
PortableAserve (which includes CL-PPCRE).

Step 6.

You should be able to start the Weblistener by executing

(wb:start-weblistener)


Step 7.  If you wish, you could dump an executable
(before you start a weblistener, presumably).  Then you would
not have to load the BioLingua software each time.  See below
in the corresponding section for 4.2 for clues as to how to
dump an executable.


			OBTAINING XML FUNCTIONALITY


<<< *** THIS IS NOT IMPLEMENTED FOR Lispworks 4.4 *** >>>
<<< *** The BioLingua software does not use this functionality currently *** >>>


The CLOCC package is one way to obtain XML parsing functionality.
But to integrate with Biolisp involves a little more than just getting
the package and loading it into Lispworks.  

We don't want to load in all the CLOCC code, nor even all of the
CLLIB subsystem of CLOCC.  What we'd like to do is just load in enough
stuff so that the XML parsing code works.

Here's what you have to do:

STEP 1:  Obtain the CLOCC code.  Here's the SourceForge URL:

  http://clocc.sourceforge.net/

and this is a link to a page with a link to the latest gzipped tar 
distribution file:

  http://clocc.sourceforge.net/snapshot/

Unzip and untar the files (see above), and put them in a directory

  C:/Lispcode/clocc/


STEP 2:  In the Biolisp toplevel directory, there is a subdirectory called
 
  miscellaneous/

This contains three files that you need:

  load-xml.lisp
  load-generic.lisp
  xml-to-lisp.lisp

Copy these files to the toplevel directory where you installed CLOCC.


STEP 3:  Edit the file lispworks-interface.lisp in the toplevel Biolisp
directory.  (If you've previously commented out the part of this file 
dealing with XML and CLOCC, then uncomment it).

Change the definition of the parameter *clocc-path* to point to
the toplevel directory where you installed CLOCC.


STEP 4:  Try it out.  Start up Lispworks and load Biolisp:

  (load "C:/Lispcode/Biolisp/bllload")

Lots of CLOCC files should get loaded or compiled and loaded.  What
should be happening is that the lispworks-interface.lisp file should
load the load-xml.lisp file, which then loads the load-generic.lisp file
and then the CLOCC file xml.lisp, which causes all the files it
depends on to load.  Finally, the xml-to-lisp.lisp file is loaded.
(Phew!).

(The xml-to-lisp.lisp file is a glue file that we wrote, that converts
between the CLOCC representation of XML and the representation of XML
that is used by the Allegro XML parser.)

STEP 5: If you wish, you can go through a similar procedure as above
to create a new Lispworks executable that contains the XML
functionality from CLOCC (and Portable Allegroserve).  The details of
this are left as an exercise.  It is not so important to do this as
the required CLOCC files load fairly quickly.

At some point you could even create an executable containing the 
entire Biolisp system.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


NOTES FOR LISPWORKS 4.2


NOTE:  THIS MAY BE SOMEWHAT OUT OF DATE!  NEVERTHELESS, IT SHOULD
PROVIDE CLUES AS TO WHAT YOU NEED TO DO.


We'll assume, for the sake of having a particular name, that you've
installed the Biolisp files in the

  C:/Lispcode/Biolisp/

directory.  If you've installed it somewhere else, then just substitute
your directory appropriately in what follows.

Also, we'll assume that if you've installed Portable Allegroserve, it's
installed in

  C:/Lispcode/portableaserve/

and if you've installed CLOCC, it's installed in

  C:/Lispcode/CLOCC/

Again, use appropriate substitutions.

Lispworks does not come with primitives to access the World Wide Web,
nor with primitives to read and write XML.

Much of the Biolisp code can be run without either set of primitives,
but the first two tutorials require WWW and WWW/XML primitives,
respectively.

If you don't want to bother with the WWW and/or XML at the moment, you
need to comment out the code in Biolisp that uses these primitives.
Here's how:

STEP 1:  Look at the lispworks-interface.lisp file in the toplevel
biolisp directory.  Comment the whole file out, or the parts of it
pertaining to WWW or CLOCC and XML, as appropriate.

STEP 2:  Look at the bioload.lisp file in the toplevel biolisp directory.
Comment out these lines:

 	"TutorialCode;biolisp1"
 	"TutorialCode;biolisp2"

(The first uses WWW, the second WWW and XML)

That should do it.


                     OBTAINING WWW FUNCTIONALITY


The Portable Allegroserve package is one way to obtain WWW functionality.
Here are step by step instructions for obtaining it, installing it and
making it work smoothly with the Biolisp code.

STEP 1.  Download the package.

  Start here: 

  http://sourceforge.net/projects/portableaserve/

  or here is a page with a link to the actual tar-gzipped file:

  http://sourceforge.net/project/showfiles.php?group_id=32760&release_id=92707


STEP 2.  Unzip and untar the downloaded file.  For instance, to put
the package into the

  C:/Lispcode/portableaserve directory

  -- create that directory
  -- move the downloaded file (call it xyz.tar.gz) into C:/Lispcode/
  -- gunzip it  (gunzip xyz.tar.gz)
  -- untar it   (tar xvf xyz.tar)
  
Voila!


STEP 3.  Create the file build-lw-with-paserve.lisp with the following
content, changing the pathnames as appropriate, and save the file into
the C:/Lispcode/ directory.

NOTE:  THIS WON'T WORK IF YOU ARE USING THE FREE VERSION OF LISPWORKS,
SINCE IT DOES NOT ALLOW YOU TO CREATE AN AUGMENTED EXECUTABLE FILE.
YOU'LL JUST HAVE TO LIVE WITH COMPILING AND LOADING ALL OF PORTABLEASERVE
EACH TIME.

---------------------

;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: CL-USER; Base: 10 -*-
;;;
;;; Builds a LispWorks image of paserve.

(in-package :cl-user)

;;; ***> Change this directory as appropriate!!!  <***
(load "C:/Lispcode/portableaserve/install.lisp")
(save-image "C:/Lispcode/lw-with-paserve.exe")
(quit)

---------------------

This file will be used as a script which will cause Lispworks to
create a new Lisp executable with the Portable Allegroserve code
already loaded into it.


STEP 4.  Find your LispWorks executable.  On my Windows machine, this is in

C:/Program Files/Xanalys/LispWorks/

and is called

LispWorks-4200.exe


STEP 5.  Run this executable from a command line (a DOS prompt, or perhaps
a cygwin shell, on Windows):

Lispworks-4200.exe -init c:/lispcode/build-lw-with-paserve.lisp


STEP 6.  That's it!  If the above finished successfully you should
have an executable named lw-with-paserve.exe in the C:/Lispcode/
directory, and if you start it LispWorks should come up just as it
always did.  To make sure the Portable Allegroserve code was loaded
into this executable, type

(find-package :net.aserve.client)

once it starts up.  You should get back a package.  (If you get back
NIL, something didn't work!).

Now you can rename the executable to whatever you like, move it
wherever you want and/or create a Shortcut and/or create a desktop Icon
for your new, shiny program.

STEP 7.  You can now start up this new executable and load in Biolisp, e.g.,

(load "C:/Lispcode/biolisp/load")

(except that you still have to have commented out the code that loads
CLOCC in the lispworks-interface.lisp file, unless you do the next
section).

NOTE: Why do we go through all the trouble of creating a new Lispworks
executable instead of just loading the Portable Allegroserve files
into Lispworks when we load the Biolisp code?  Good question.  The
answer is bizarre.  It turns out that it is currently impossible to
simply load the Portable Allegroserve system (in the sense of just
loading the binary, or fasl, files).  One must FIRST compile AND THEN
load each of the files.  Attempting to just load the fasl files using
the standard defsystem results in a runtime error.  Therefore, instead
of setting things up so that when you load Biolisp, it compiles all
the Portable Allegroserve files each and every time, we have you build
a new image.



			OBTAINING XML FUNCTIONALITY


The CLOCC package is one way to obtain XML parsing functionality.
But to integrate with Biolisp involves a little more than just getting
the package and loading it into Lispworks.  

We don't want to load in all the CLOCC code, nor even all of the
CLLIB subsystem of CLOCC.  What we'd like to do is just load in enough
stuff so that the XML parsing code works.

Here's what you have to do:

STEP 1:  Obtain the CLOCC code.  Here's the SourceForge URL:

  http://clocc.sourceforge.net/

and this is a link to a page with a link to the latest gzipped tar 
distribution file:

  http://clocc.sourceforge.net/snapshot/

Unzip and untar the files (see above), and put them in a directory

  C:/Lispcode/clocc/


STEP 2:  In the Biolisp toplevel directory, there is a subdirectory called
 
  miscellaneous/

This contains three files that you need:

  load-xml.lisp
  load-generic.lisp
  xml-to-lisp.lisp

Copy these files to the toplevel directory where you installed CLOCC.


STEP 3:  Edit the file lispworks-interface.lisp in the toplevel Biolisp
directory.  (If you've previously commented out the part of this file 
dealing with XML and CLOCC, then uncomment it).

Change the definition of the parameter *clocc-path* to point to
the toplevel directory where you installed CLOCC.


STEP 4:  Try it out.  Start up Lispworks and load Biolisp:

  (load "C:/Lispcode/Biolisp/bllload")

Lots of CLOCC files should get loaded or compiled and loaded.  What
should be happening is that the lispworks-interface.lisp file should
load the load-xml.lisp file, which then loads the load-generic.lisp file
and then the CLOCC file xml.lisp, which causes all the files it
depends on to load.  Finally, the xml-to-lisp.lisp file is loaded.
(Phew!).

(The xml-to-lisp.lisp file is a glue file that we wrote, that converts
between the CLOCC representation of XML and the representation of XML
that is used by the Allegro XML parser.)

STEP 5: If you wish, you can go through a similar procedure as above
to create a new Lispworks executable that contains the XML
functionality from CLOCC (and Portable Allegroserve).  The details of
this are left as an exercise.  It is not so important to do this as
the required CLOCC files load fairly quickly.

At some point you could even create an executable containing the 
entire Biolisp system.

