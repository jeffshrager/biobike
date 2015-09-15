;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)

;;; +=========================================================================+
;;; | Copyright (c) 2006 JP Massar, John Myers                                |
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

;; Author: JP Massar.

#||

VPL menus are of two distinct types: menus that are used in the palette, 
known as palette menus, and menus that are used in the workspace within 
snippets, known as snippet menus.

As things developed, these two menu types are significantly different
and in VPL 2.0 they will remain so, at least for now.  

I. Palette menus.

A. Syntax.

Palette menus consist of (ID MENU-TITLE SUBMENUS MENU-ENTRIES).

Submenus are of the form (SUBMENU-TITLE SUBMENU-SUBMENUS MENU-ENTRIES).

Menu entries are of the form (ENTRY-TEXT ENTRY-OPERATOR-OR-ID).

Palette menus are therefore recursive, except that the toplevel menu has
an ID whereas the submenus do not.

B. Overview.

Palette menus are currently cached.  Each menu (or in the case of
module menus, the set of module menus) is stored in a variable.  When
the first user creates the first VPL session, all the palette menus
are instantiated and stored in these variables.  The menus are not
recreated each time a new session is initiated, unless the variable
*FORCE-VPL-SYSTEM-INIT* is non-nil.  Generally this variable is only
set non-nil when debugging the system.

Palette menus come in two flavors, those which represent BBL/Lisp
functions (and the templates defined for them), and standard menus
with operators associated with each menu selection.  The former are
known as MODULE PALETTE MENUS, while the latter are known as STANDARD
PALETTE MENUS.  

In module palette menus, the ENTRY-OPERATOR-OR-ID is a template ID
(currently an integer) returned by the function NEW-UNIQUE-ID.  When
the module menu entry is selected, the template ID is returned to the
server, where it is recognized as such and decoded into the
appropriate template.

In standard palette menus, the ENTRY-OPERATOR-OR-ID is defined by the
menu creator as a symbol, one of the set of symbols defined by
DEFUN-OPCODE.  Before a standard menu is sent to the server, it must
be compiled.  Compilation of a standard palette menu consists of
translating the symbols into opcodes.  (In fact, this may be overkill;
the client is capable of receiving the symbol name instead of an
opcode, and sending back the same symbol name instead of the opcode.
The only real advantage is that opcodes are smaller (in ASCII or XML
representation)).

In a sense, module palette menus are precompiled, because template IDs
are integers, just as are opcodes, so that no further processing is
necessary.

C. Standard palette menu definition.

Standard palette menus are defined in the file palette-menus.lisp.
These palette menus are created by the routine CREATE-TOPLEVEL-MENU,
which compiles the menu at load time using the routine
COMPILE-TOPLEVEL-MENU.  *** These routines should renamed
CREATE-PALETTE-MENU and COMPILE-PALETTE-MENU.

Palette menus are sent to the client when the user starts up a VPL
session (or does a browser refresh).  The routine
INITIALIZE-PALETTE-MENUS initializes each palette menu in turn by
calling INITIALIZE-A-PALETTE-MENU.  The routine
INITIALIZE-A-PALETTE-MENU takes a compiled palette menu and sends it
to the client.  This routine takes an additional argument, a color,
which is used as the background for the menu title box shown in the
palette.  The colors associated with menus are simply defconstants
defined in defs.lisp.  Currently the valid colors are "red", "green",
"blue", and "black".

*** It would make some sense to associate the color with the menu
directly.

The display of a particular palette menu can be modified using the
REPLACE-A-PALETTE-MENU command.  The server takes the ID from the menu
to be modified, conses up a new menu with the same ID and one or more
menu entries changed, and then calls this function, which sends a
command to the client to change the display.  Modified menus are
currently used as the user defines and/or deletes new functions and
variables.

D. Module palette menu definition.

Module palette menus are defined in the file module-menus.lisp.  The
routine GENERATE-VPL-MODULE-TREE generates a list of module menus, one
for each toplevel BBL module.  The routine
INITIALIZE-FUNCTION-PALETTE-MENUS calls GENERATE-VPL-MODULE-TREE, and
then calls INITIALIZE-A-PALETTE-MENU on each module menu in the
returned list. INITIALIZE-FUNCTION-PALETTE-MENUS is called by
INITIALIZE-PALETTE-MENUS.

The creation of module palette menus is quite complicated; the code is
extremely obscure.  Basically it takes the functions and submodules of
each BBL module, finds the associated template IDs for the functions
and creates menu entries for each, and recurses on each submodule.
(Submodules of a module correspond to submenus of a module menu.)  A
final subroutine creates a module menu consisting of all the BBL
functions found in modules, listed in alphabetical order and submenued
so that at most a certain number (currently 20) of functions appear in
a submenu.


II. Snippet menus.

A. Overview.

Snippet menus are created by the code that takes snippets and turns
them into boxes.  Snippet menus are components of boxes.

Snippet menus come in two flavors, MAIN-MENUS and OPTIONS-MENUS.
Although the client does not enforce such a restriction, no box in
version 2.0 will have both a main menu and an options menu.  One or the
other will contain all the menu options associated with a box.

Main menus are associated with the arrow icon in the upper left of a
visible box.  The icon associated with an options menu appears
wherever in the conaining box the options menu definition is placed,
although by convention the options menu definition (and hence the
icon) is always placed at the extreme right of the box.  The options
icon is currently a small green box with a white arrow inside and a
label below.  *** We want the ability to be able to specify the icon
instead of having it hardcoded into the client.

The syntax for a snippet menu is:

(ID TYPE TITLE ENTRY1 ENTRY2...)

Where TYPE is either :jbml-main-box-menu or :jbml-options-menu, 
and ENTRY is (ENTRY-TEXT ENTRY-OPERATOR).

The ENTRY-OPERATOR is a symbol defined using DEFUN-OPCODE.

Currently, a snippet menu in the above form needs to be processed 
before being sent to the client.  This processing has two parts:

  -- Compilation, which transforms the operators into opcodes, just as
above with palette menu entries.  Compilation is currently performed
by compiling a set of menu items; there is no routine to compile the
entire menu object.  The routine that compiles menu items is called
COMPILE-MENU-ITEMS (and is the same routine as that used for palette
menu items).

  -- Conversion, which translates the compiled menu entries into a
form the client expects: (TEXT OPCODE) -> (opcode :jbml-menu-entry
text).  This conversion is done by the routine MAKE-JBML-MENU-ENTRIES.
Again, this is done to a set of menu items, not to the menu object
itself.

*** Currently these two processes are independent but they should be
coalesced into a single process known as snippet-menu-compilation.

*** Currently doing the compilation/conversion on options menus uses
different code than that for main menus, and this should be merged.

Note that the final form of the menu entries for snippet menus is
different than that for palette menus.  Final palette menu entries are
of the form (TEXT OPCODE) while final snippet menu entries are of the
form (OPCODE :JBML-MENU-ENTRY TEXT).  *** This really should be made
more consistent!

B. Generating menu options.

Currently the mechanism to generate menu items for main menus is
significantly different than that to generate menu items for option
menus.  

To generate main menu options the routine CREATE-MAIN-MENU-FOR-SNIPPET
is called.  To generate options menu items specialized routines for 
each type of options menu are called.  (E.g., CREATE-COMPILED-&REST-MENU)

Main menu options for a given box/snippet are generated by iterating
over the list of all possible main menu options and for each one
asking if the option is appropriate for the snippet and the snippet's
context (e.g, whether it is at toplevel).

In version 2.0 we want to merge the generation of menu options for 
main menu items and options menu items.  

To do this we define a set of possible menu option types: 

  -- Universal (e.g., Help).  
     All snippet menus have these options available.

  -- Contextually universal (e.g., Collapse).
     All snippet menus may have this option, depending on context
     (e.g., unless the box is already collapsed).

  -- Toplevel.
     Items that appear only and exclusively on toplevel nodes.
     (Are there any?)

  -- Node required (e.g., 'Add another' for an :agg snippet). 
     Options particular to a node, and always present.  

  -- Node optional (e.g., 'Add left').
     Options particular to a node, but dependent on context.
 
  -- Node set (e.g., Keywords and flags). 
     Options that are data-dependent (e.g., the keywords and flags for a 
     particular :keywords-and-flags node are function-specific).  
     Node sets are found basically on all the :choice snippets.

To create a menu for a box/snippet, we therefore call the routines to
create a menu item set for each of the node types above, then append
them all together, then create the menu itself, and finally compile
the menu.  This algorithm should apply to all snippet menus.

||#



;;; General menu item construction tools 

(defun menu-item (name operator) (list name operator))

(defun create-menu-items (menu-items) menu-items)

(defun compile-menu-items (menu-items)
  (loop for (entry operator) in menu-items 
        collect
        (menu-item
         entry
         (if (integerp operator) operator (operator->opcode operator))
         )))

#||

Data Input:  (ID# 
  
              MyMenuTitle 

              ((MySubmenuTitle1 
                third-level-menus-if-any-otherwise-nil 
                ((MenuEntry1-1 ID1) 
                 (MenuEntry1-2 ID2)
                 ...
                 ))
               (MySubMenuTitle2
                nil 
                ((entry2-1 ID3) 
                 (entry2-2 ID4)
                 ...
                 )))

              ((MainEntry1 ID5)   
               (MainEntry2 ID6)
               ...
               ))

||#

