;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl) 

;;; +=========================================================================+
;;; | Copyright (c) 2005 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

;;; Author: JP Massar. 


#||

The VPL server model for the workspace area is composed of 
snippets.  Snippets have: 

  -- a unique ID, used to identify them across the client/server interface
  -- a parent
  -- a value
  -- children, a list of snippets 
  -- a property list 

Some snippets have a return-type, specifying the lisp type the expression
returns (not all snippets are expressions, and hence only some snippets
have return types).  These snippets inherit from RETURN-TYPE-SNIPPET. 

Most snippets have a label as well, which is used in the visual display.
These snippets inherit from LABELED-SNIPPET.

Templates are used to define the snippets that compose a given LISP or
BBL construct.  Templates are composed of subtemplates.  A subtemplate
generally specifies a child of the snippet created for a LISP or BBL
construct.

Examples of things defined with templates are:

  -- a particular Common Lisp special form (e.g., IF), 
  -- a particular BBL define-function or macro (e.g., LENGTH-OF), 
  -- a Common Lisp function (e.g., ABS)

Templates have the following structure:

  TEMPLATE ::= 
   (IDENTIFIER TTYPE RETURN-TYPE
     <zero or more subtemplates> <various keys and flags>)
 
Valid TTYPES in templates are:
  :macro, :function, :variable, or :define-function

Certain kinds of snippets are created from the subtemplates of a
template when that template is instantiated.  (There are other snippet
types which are only created by other VPL code and never when
templates are instantiated).

There are multiple types of snippets created from templates and subtemplates:

  -- Leaves (symbols, literals, constants)
  -- Forms (things the user fills in)
  -- Calls (macros, functions, define-functions)
  -- Aggregates (zero or more things defined by a single subtemplate)
  -- Choices (which come in many flavors, see below)
  -- Keys-and-flags (a special kind of choice)
  -- Progn (a list of subtemplates)

The other kinds of snippets, not created from templates are:

  -- toplevel snippets (used to denote the root of the workspace and 
     results areas)
  -- toplevel output snippets (first level nodes in the results area)
  -- output value snippets (representing a single result value)
  -- keyword snippets 
  -- flag snippets

THE FORM OF ALL SUBTEMPLATES IS: 

  SUBTEMPLATE::= <subtemplate-type> <one or more required args>
       &rest <keys and flags>

Each snippet type knows how many required args are required, and which
keys and flags are valid.

When a subtemplate is instantiated into a snippet, the subtemplate is
parsed, separating the keys and flags from the main arguments.  The
keys and flags are massaged into a property list and stored on the
property list of the newly created snippet.  Other properties may also
be stored on the property list based on the values of the required
arguments.

The subtemplate type may correspond directly to a snippet type, or it
may be the equivalent of a macro which expands into another
subtemplate type with appropriate arguments, keys, and flags.  The
macro DEF-SUBTEMPLATE-ALIAS defines these pseudo-macro expansions.


1.  Leaves.  

These are the terminal snippets.  They have no children.  
All they have is a value.  For symbols the value is the symbol
itself; for literals the value is the string representing the literal; 
and for constants, the value is the constant itself.

Literals are specified in subtemplates by (:literal "<literal-string>").
Constants are specified in subtemplates by (:constant <constant>).
Symbols are not specified in subtemplates but are specified dynamically
using a subtemplate of the form (:symbol <symbol>).  

Leaves have a method, snippet-label, which creates a string to be used 
by the visual display (or subsequently modified and displayed).

Literals have a property called :class, which is used to obtain 
further specification of how the literal should appear when displayed 
(such as color, bold or not, italics or not).

Constants have a slot, snippet-return-type, which is filled with the
lisp type of the constant and used by parents to enforce type
constraints.

2. Forms.

There are two kinds of forms, those which can accept any expression
subject to type constraint, :form, and those which can only accept a
place, :arg (such as would be found in an argument list).

By default, :arg accepts only symbols.  But if it is given the flag
:place, it is allowed to accept a valid lisp or bbl place (e.g., x[j]).

:form snippets are specified in subtemplates by 
(:form "<label>" <return-type>).

:arg snippets are specified in subtemplates by (:arg "<label>").

:form has a slot, snippet-return-type, which is used to restrict the
values that may be entered within.

Both :form and :arg have a slot, snippet-label, which is used in the
display and to hint at what the form hole should be filled in with.

3. Calls 

Calls are basically all the same even though we have three distinct
types.  Calls consist of a literal, being the name of the
function/macro being invoked, followed by the arguments (in the case
of a function), or the forms (in the case of a macro).  The
arguments/forms are described by subtemplates.

Calls are generally defined by TEMPLATEs.  Each
function/macro/define-function in the BBL language has its own
template.  

Call nodes, forms, and some leaves are represented in the visual
display by boxes.  Each such box has a small stylish arrow in the
upper left.  When the mouse is moved over this arrow, menu options
appear (such as 'Help', 'Execute').  Each box also has in the upper
right a small clear/delete X button.  The semantics of clicking this
button varies depending on context and the type of node.

4. Aggregates

Aggregates are repeated forms or subtemplates.  They represent things
like the &rest argument to +, the body of a PROGN or WHEN, and a
simple argument list (with only required arguments).

Aggregates have a label, followed by a subtemplate, followed by
optional flags.  The subtemplate is the pattern used by the aggregate
node to spawn a new child.  

Aggregates have a flag, :one-form-required.  If specified, the node
must be filled in, whereas if not specified, the node can be left
empty.  Aggregates have a related flag, :display-one-hole.  If
specified and the node is empty, the visual display shows a single
subtemplate pattern anyway, presupposing that the user will almost
certainly want to fill in at least one form.

Aggregates also have a flag :splice.  This determines whether
the set of nodes created by the aggregate is to be returned as a list
into the code that is eventually created, or spliced in, effectively
using ',@' vs. ','.  As an example

(+ 1 2 3) 

vs. 

(defun foo (x y z) ...)

The arguments to + are treated as an aggregate node and given the
:splice flag.  The argument list for a defun is also an aggregate node
but does not have the :splice flag.  Thus in the created code the
forms for defun's argument list are turned into a list of values,
whereas the forms for +'s aggregate node are spliced.

Aggregate nodes are represented in the visual display using 'arrow
option' notation.  An icon in the form of an arrow is used in place
of a box.  When the mouse is moved over this icon, the menu choices 
appear, including choices such as 'add another', 'add two more'.  
The icon always appears to the RIGHT of all its children.

5. Choices

Choices are nodes that represent something that is either optional
or give the user in effect a menu of options.

Choices come in two major flavors: standard choices and keys-and-flags.

Keys-and-flags nodes consist basically of a list of keyword and flag
specifiers, in the form (:keyword <name> ...) and (:flag <name>).

Standard choices consist of a label, followed by a set of choices,
followed by optional flags.  The format of the set of choices is
dependent on exactly what kind of choice node is being specified.  The
general form of each choice is ("menu-entry-text" <subtemplate>), 
although in certain cases this may be simplified.

The semantics of standard choices is controlled by eight flags which
come in pairs: 

  -- :instantiated vs. :literal 
  -- :single vs. :multiple
  -- :required vs. :optional
  -- :repeatable vs. :non-repeatable

If :instantiated is specified, the choices provided must be a
subtemplate pattern, which gets instantiated when the user selects it.
If :literal is specified, the choices provided must be strings or
symbols which are turned into literals (the latter is used for BBL
tokens).

If :single is specified, the user is only allowed to select one 
of the set of choices.  If :multiple is specified, the user is allowed
to select any or all of the choices, one by one.

If :required is specified, the user must select one of the choices,
otherwise his code will not execute.  If :optional is specified, the
user is free to leave the choice unspecified; the code will use a
default or as in the case of a lisp &optional argument, leave 
the node out entirely.  

If :repeatable is specified, the user is allowed to select an option
he has already selected (this is used for the controls section of a
LOOP, where, for instance, more than one 'For var in list' control can
be specified.  If :non-repeatable is specified, the user is not allowed
to select an option he has already selected. 

Choice nodes are also represented in the visual display using 'arrow option'
notation.  When the mouse is moved over the icon representing this node, 
various standard menu items along with all the currently legal choices 
from the choice node are shown to the user.  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

||#