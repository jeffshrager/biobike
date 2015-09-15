/*

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

*/

parenstack = new Array (30);
var totallinecount, cursorlineindex;


/* 
   CHECKPARENS
   Old checkparens function.  Used for IE since other stuff
   only works for Modzilla-style browsers.
*/

function checkparens(event, field, form) {
  var stackposition = 0;
  for (i = 0; i < 30; i++) { Array[i]=0; }
  currenttext = field.value;
  textlen = currenttext.length;
  for (i = 0; i <= currenttext.length; i++) {
    if (currenttext.substring(i,i+1) == '(') {
      parenstack[stackposition] = i;
      stackposition++;
    }
    if ((currenttext.substring(i,i+1) == ')') &&
        (stackposition > 0)) {
       parenstack[stackposition] = 0;
       stackposition--;
    }
  }
  foo = parenstack[stackposition];
  form.closer.value = currenttext.substring(foo,foo+15);
}


/*
  IMBALANCE
   Returns the number of excess left parens over right parens.
   Returns a positive number of more left parens,
   returns 0 if they balance,
   and a negative number if more right parens.
   Does not attempt to deal with Lisp syntax such as strings
   as character notation, e.g., #\(
   it just counts straight characters.

*/

function imbalance(s,start,stop) {
  var lcount = 0; var rcount = 0;
  for (j = start; j < stop; j++) {
    if (s.charAt(j) == '(') { lcount++; }
    if (s.charAt(j) == ')') { rcount++; }
  }
  return(lcount-rcount);
}


/*
   ALLBLANKS
   Returns 1 if every character in the range START (inclusive)
   to STOP (exclusive) is a space or a tab.  Otherwise returns 0.
*/

function allblanks(s,start,stop) {
  var ch;
  for (j = start; j < stop; j++) {
    ch = s.charAt(j);
    if ((ch != ' ') && (ch != '\t')) { return(0); }
  }
  return(1);
}
 

/*
   PREVNEWLINEPOS
   ENDPOS must be the index of a newline character.
   Find the index of the immediately previous newline character.
   If there is no such character, -1 is returned.
*/

function prevnewlinepos(s,endpos) {
  for (j = endpos-1; j >= 0; j--) {
    if (s.charAt(j) == '\n') return(j);
  }
  return(-1);
}


/*
   PREVLINE
   Returns a 'line' of characters prior to ENDPOS which has at
   least one non-blank character in it.  If no such line exists
   the null string is returned.  The string returned contains
   no newline characters.
   ENDPOS should be the index of a newline character.
*/

function prevline(s,endpos) {
  var pnp, ab;
  if (endpos < 1) return("");
  while (true) {
    pnp = prevnewlinepos(s,endpos);
    if (-1 == pnp) { return(s.substring(0,endpos)); }
    ab = allblanks(s,pnp+1,endpos);
    if (ab == 0) { return(s.substring(pnp+1,endpos)); }
    endpos = pnp;
  }
}


/*
   BLANKS
   Creates a string of N blanks and returns that string.
   Presumably there is a better way of doing this...
*/

function blanks(n) { 
  var s = ""; for (j=0; j<n; j++) s+=' '; return(s);
}

/*
   FIRSTNONBLANKPOS
   Returns the position of the first non-blank character in S.
   Tabs are considered non-blank characters here.
   If no such character exists returns -1.
*/

function firstnonblankpos(s) {
  for (j = 0; j < s.length; j++) {
    if (s.charAt(j) != ' ') return(j);
    }
  return(-1);
}


/*
  PARENMATCHING
*/

function parenmatching (form, curtext, cursorpos) {
  var stackposition = 0;
  for (i = 0; i < 30; i++) {
    Array[i]=0;
  }
  for (i = 0; i < cursorpos; i++) {
   if (curtext.charAt(i) == '(') {
     parenstack[stackposition] = i;
     stackposition++;
   }
   if ((curtext.charAt(i) == ')') && (stackposition > 0)) {
      parenstack[stackposition] = 0;
      stackposition--;
   }
  }
  var matchpos = parenstack[stackposition];
  form.closer.value = 
    curtext.substring(matchpos,Math.min(cursorpos,matchpos+15));
}


/*
   WHICHLINE
   This setq's two global variables, TOTALLINECOUNT (the total number of
   newlines in the CURTEXT), and CURSORLINEINDEX, the line index of the
   line the character at CURPOS can be found at.
*/

function whichline (curtext,curpos) {
  totallinecount = 0; cursorlineindex = 0;
  for (j = 0; j < curtext.length; j++) {
    if (j == curpos) cursorlineindex = totallinecount;
    if (curtext.charAt(j) == '\n') totallinecount++;
  }
}

/*
  INDENTING
*/

function indenting (field,curtext,textlen,cursorpos) {
  var pv = prevline(curtext,cursorpos-1);
  var imb = imbalance(pv,0,pv.length);
  var nbpos = firstnonblankpos(pv);
  if (nbpos == -1) nbpos = 0;
  var bl = blanks(nbpos + (imb*2));
  var pre = curtext.substring(0,cursorpos);
  var post = curtext.substring(cursorpos,textlen);
  var newval = pre + bl + post;
  field.value = newval;
  var newcurpos = pre.length + bl.length;
  field.setSelectionRange(newcurpos,newcurpos);
  whichline(curtext,cursorpos);
  if (totallinecount < field.rows) return;
  var halfrows = Math.floor(field.rows / 2);
  if (totallinecount == 0) totallinecount == 1;
  var percentdown = (cursorlineindex - halfrows) / totallinecount;
  if (percentdown < 0.0) percentdown = 0.0;
  var pixelsdown = Math.floor(field.scrollHeight * percentdown);
  field.scrollTop = pixelsdown;
}


/*
   PREVIOUSFUNCTIONNAME
   Finds the first previous occurrence of a left paren by going backwards,
   then goes forward until it finds a space, then extracts the stuff in
   between and returns that as a string.  If it cannot find a left paren
   before it finds a newline going backwards, or it cannot find a space
   before it hits the end of the string, the null string is returned.
*/

function previousfunctionname(s,curpos) {
  var ch; var lparenpos = -1;
  for (j = curpos; j >= 0; j--) {
    ch = s.charAt(j);
    if (ch == '\n') return("");
    if (ch == '(') { lparenpos = j; break; }
  }
  if (lparenpos == -1) return("");
  for (j = lparenpos; j < s.length; j++) {
  ch = s.charAt(j);
  if (ch == ' ') return(s.substring(lparenpos+1,j));
  }
  return("");
}


/*
  FINDARGLIST
*/

function findarglist (form,curtext,cursorpos) {
  if (!arglistaainitialized) initarglistaa();
  var textlen = curtext.length;
  var fname = previousfunctionname(curtext,cursorpos-1);
  if (fname.length == 0) return;
  fname = fname.toLowerCase();
  var result = arglistaa[fname];
  if (typeof(result) == "undefined") { form.closer.value = '?'; }
  else { form.closer.value = result; }
}


/*
  MINIEDITORKEYUP
  The test for selectionStart determines whether we have an IE Browser.
  If so, we punt, defaulting to the old paren checking code.
*/

function minieditorkeyup(event, field, form) {
  var ch = String.fromCharCode(event.which);
  var curtext = field.value;
  var textlen = curtext.length;
  var cursorpos = field.selectionStart;
  if (!field.selectionStart) return(checkparens(event,field,form));
  prevchar = curtext.charAt(cursorpos-1);
  // when the text area with a scroll bar first comes up, 
  // the cursorpos is equal to textlen.  It remains that way 
  // until some event happens within the text of the textarea.  
  // but if the first event is in the scroll area, the code
  // was forcing an indent, causing the scroll bar to go back 
  // to the very top.  By immediately returning here, we prevent
  // this behavior at the slight cost of not doing paren matching
  // if the very last character is a right paren (but it's usually
  // a newline)
  if (cursorpos == textlen) {return(0);} // { curchar = '\0';}
  else { curchar = curtext.charAt(cursorpos); }
  if (prevchar == ')') {
    // alert('matching: ' + prevchar + ' ' + cursorpos);
    parenmatching (form, curtext, cursorpos);
  }
  else if ((ch != '\b') && (prevchar=='\n') && (cursorpos>=2)) {
    indenting(field,curtext,textlen,cursorpos);
  }
  else if (ch == ' ') {
    findarglist(form,curtext,cursorpos-1);
  }
}

