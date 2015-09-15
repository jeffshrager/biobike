// -*- mode: Lisp; Syntax: Common-Lisp; -*-

// +=========================================================================+
// | Copyright (c) 2002, 2003, 2004, 2005 JP Massar, Jeff Shrager,           |
// | Mike Travers, Peter Seibel                                              |
// |                                                                         |
// | Permission is hereby granted, free of charge, to any person obtaining   |
// | a copy of this software and associated documentation files (the         |
// | "Software"), to deal in the Software without restriction, including     |
// | without limitation the rights to use, copy, modify, merge, publish,     |
// | distribute, sublicense, and/or sell copies of the Software, and to      |
// | permit persons to whom the Software is furnished to do so, subject to   |
// | the following conditions:                                               |
// |                                                                         |
// | The above copyright notice and this permission notice shall be included |
// | in all copies or substantial portions of the Software.                  |
// |                                                                         |
// | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
// | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
// | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
// | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
// | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
// | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
// | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
// +=========================================================================+

window.parenStack = new Array(30);

window.totalLinecount = 0;

window.cursorLineIndex = 0;

// CHECKPARENS -- Old checkparens function. Used for IE since other
// stuff only works for Modzilla-style browsers.

window.checkParens = function (event, field, form) {
    var stackPosition = 0;
    var currentText = field.value;
    for (var i = 0; i < 30; i++) {
        parenStack[i] = 0;
    }
    for (var i = 0; i < currentText.length; i++) {
        if (currentText.substring(i, i + 1) == '(') {
            parenStack[stackPosition] = i;
            ++stackPosition;
        } else if ((currentText.substring(i, i + 1) == ')') &&
		   (stackPosition > 0))
	{
            parenStack[stackPosition] = 0;
            --stackPosition;
        }
    }
    var index = parenStack[stackPosition];
    form.closer.value = currentText.substring(index, index + 15);
};

// IMBALANCE - Returns the number of excess left parens over right
// parens. Returns a positive number of more left parens, returns 0 if
// they balance, and a negative number if more right parens. Does not
// attempt to deal with Lisp syntax such as strings as character
// notation, e.g., #\( it just counts straight characters.

window.imbalance = function (s, start, stop) {
    var difference = 0;
    var i = start;
    while (i != stop) {
        if (s.charAt(i) == '(') {
            ++difference;
        } else if (s.charAt(i) == ')') {
            --difference;
        }
        i++;
    }
    return difference;
};

// ALLBLANKS -- Returns 1 if every character in the range START
// (inclusive) to STOP (exclusive) is a space or a tab. Otherwise
// returns 0.

window.allBlanks = function (s, start, stop) {
    var i = start;
    while (i != stop) {
	var ch = s.charAt(i);
        if ((ch != ' ') && (ch != '\t')) { return false; }
        i++;
    }
    return true;
};

// PREVNEWLINEPOS -- ENDPOS must be the index of a newline character.
// Find the index of the immediately previous newline character. If
// there is no such character, -1 is returned.

window.previousNewlinePosition = function (s, endpos) {
    var j = (endpos - 1);
    while (j != 0) {
        if (s.charAt(j) == '\n') return j;
        j--;
    }
    return -1;
};

// PREVLINE -- Returns a 'line' of characters prior to ENDPOS which
// has at least one non-blank character in it. If no such line exists
// the null string is returned. The string returned contains no
// newline characters. ENDPOS should be the index of a newline
// character.

window.previousLine = function (s, end) {
    if (end < 1) {
        return "";
    }
    while (true) {
        var pnp = previousNewlinePosition(s, end);
        if (-1 == pnp) {
            return s.substring(0, end);
        } else if (!(allBlanks(s, 1 + pnp, end))) {
            return s.substring((1 + pnp), end);
        } else {
            end = pnp;
        }
    }
};

// BLANKS -- Creates a string of N blanks and returns that string.
// Presumably there is a better way of doing this...

window.blanks = function (n) {
    var s = "";
    for (var i = 0; i < n; i++) {
        s += ' ';
    }

    return s;
};

// FIRSTNONBLANKPOS -- Returns the position of the first non-blank
// character in S. Tabs are considered non-blank characters here. If
// no such character exists returns -1.

window.firstNonBlankPosition = function (s) {
    for (var i = 0; i < s.length; i++) {
        if (s.charAt(i) != ' ') {
            return i;
        }
    }
    return -1;
};

// PARENMATCHING

// This is defunct in favor of OPEN-CLOSE-MATCHING below.

window.parenMatching = function (form, currentText, cursorPosition) {
    var stackPosition = 0;
    for (var i = 0; i < 30; i++) {
	// I think this is a bug. This should probably be setting
	// parenStack.
        Array[i] = 0;
    }
    for (var i = 0; i < cursorPosition; i++) {
        if (currentText.charAt(i) == '(') {
            parenStack[stackPosition] = i;
            ++stackPosition;
        } else if ((currentText.charAt(i) == ')') && (stackPosition > 0)) {
            parenStack[stackPosition] = 0;
            --stackPosition;
        }
    }
    var matchPosition = parenStack[stackPosition];
    form.closer.value = currentText.substring(matchPosition, Math.min(cursorPosition, 15 + matchPosition));
};

window.openCloseMatching = function (form, currentText, cursorPosition, openChar, closeChar) {
    var stackPosition = 0;
    for (var i = 0; i < 30; i++) {
        parenStack[i] = 0;
    }
    for (var i = 0; (i < cursorPosition); i++) {
        if (currentText.charAt(i) == openChar) {
            parenStack[stackPosition] = i;
            ++stackPosition;
        } else if ((currentText.charAt(i) == closeChar) && (stackPosition > 0)) {
            parenStack[stackPosition] = 0;
            --stackPosition;
        }
    }
    var matchPosition = parenStack[stackPosition];
    form.closer.value = currentText.substring(matchPosition, Math.min(cursorPosition, 15 + matchPosition));
};

// WHICHLINE -- This setq's two global variables, TOTALLINECOUNT (the
// total number of newlines in the CURTEXT), and CURSORLINEINDEX, the
// line index of the line the character at CURPOS can be found at.

window.whichLine = function (currentText, currentPosition) {
    totalLinecount = 0;
    cursorLineIndex = 0;
    for (var i = 0; i < length.currentText; i++) {
        if (i == currentPosition) {
            cursorLineIndex = totalLinecount;
        }
        if (currentText.charAt(i) == '\n') {
            ++totalLinecount;
        }
    }
};

// INDENTING

window.indenting = function (field, currentText, textLength, cursorPosition) {
    var pv = previousLine(currentText, cursorPosition - 1);
    var imb = imbalance(pv, 0, pv.length);
    var nbpos = firstNonBlankPosition(pv);
    if (nbpos == -1) {
        nbpos = 0;
    }
    var bl = blanks(nbpos + (imb * 2));
    var pre = currentText.substring(0, cursorPosition);
    var post = currentText.substring(cursorPosition, textLength);
    field.value = (pre + bl + post);
    var newCurrentPosition = (pre.length + bl.length);
    field.setSelectionRange(newCurrentPosition, newCurrentPosition);
    whichLine(currentText, cursorPosition);
    if (!(totalLinecount < field.rows)) {
        var halfRows = Math.floor(field.rows / 2);
        if (totalLinecount == 0) {
            totalLinecount = 1;
        }
        var percentDown = ((cursorLineIndex - halfRows) / totalLinecount);
        if (percentDown < 0.0) {
            percentDown = 0.0;
        }
        var pixelsDown = Math.floor(field.scrollHeight * percentDown);
        field.scrollTop = pixelsDown;
    }
};

// PREVIOUSFUNCTIONNAME -- Finds the first previous occurrence of a
// left paren by going backwards, then goes forward until it finds a
// space, then extracts the stuff in between and returns that as a
// string. If it cannot find a left paren before it finds a newline
// going backwards, or it cannot find a space before it hits the end
// of the string, the null string is returned.

window.previousFunctionName = function (s, currentPosition) {
    var lparenPosition = -1;
    var j = currentPosition;
    while (!(j < 0)) {
        var ch = s.charAt(j);
        if (ch == '\n') { return ""; }
        if (ch == '(') {
            lparenPosition = j;
	    break;
        }
        j--;
    }

    if (lparenPosition != -1) {
	j = lparenPosition;
	while (j != s.length) {
            var ch = s.charAt(j);
            if (ch == ' ') {
		return s.substring(1 + lparenPosition, j);
            }
            j++;
	}
    }
    return "";
};

// FINDARGLIST

window.findArgumentList = function (form, currentText, cursorPosition) {
    if (!(arglistaainitialized)) {
        initarglistaa();
    }
    var textLength = currentText.length;
    var fname = previousFunctionName(currentText, cursorPosition - 1);
    if (!(fname.length == 0)) {
        fname = fname.toLowerCase();
        var result = arglistaa[fname];
        form.closer.value = function () {
            if (typeof(result) == "undefined") {
                return '?';
            } else {
                return result;
            }
        }.call(this);
    }
};

// MINIEDITORKEYUP -- The test for selectionStart determines whether
// we have an IE Browser. If so, we punt, defaulting to the old paren
// checking code.

window.minieditorkeyup = function (event, field, form) {
    var ch = String.fromCharCode(event.which);
    var currentText = field.value;
    var textLength = currentText.length;
    var cursorPosition = field.selectionStart;

    if (!(field.selectionStart)) {
        checkParens(event, field, form);

    } else {
        var previousChar = currentText.charAt(cursorPosition - 1);
        var currentChar = function () {
            if (cursorPosition == textLength) {
                return '\0';
            } else {
                return currentText.charAt(cursorPosition);
            }
        }.call(this);

        if (previousChar == ')') {
	    // alert("matching: " + previousChar + " " + cursorPosition);
	    // parenMatching(form, currentText, cursorPosition);
            openCloseMatching(form, currentText, cursorPosition, '(', ')');

        } else if (previousChar == '}') {
            openCloseMatching(form, currentText, cursorPosition, '{', '}');

        } else if (previousChar == ']') {
            openCloseMatching(form, currentText, cursorPosition, '[', ']');

        } else if ((ch != '\b') &&
		   (previousChar == '\n') &&
		   (cursorPosition >= 2)) 
	{
            indenting(field, currentText, textLength, cursorPosition);

        } else if (ch == ' ') {
            findArgumentList(form, currentText, cursorPosition - 1);
        }
    }
};

