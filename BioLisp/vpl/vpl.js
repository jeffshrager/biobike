/*
 *  All log entries for this file moved to vplls-comments.txt
 *  Please add further log entries there
 *
 *  PIPE INTERFACE
 *  Incoming:   *LISP* function (defun vpl-on-message (message))  handles dispatching.
 *                      Note:  spelled vplOnMessage in the JS file here.  Oops, deprecated and now errors only.
 *                    **Actual dispatch is now here in JS function:  vplJsonOnMessage().  **
 *  Other end of pipe:  vpl.lisp, (defun vpl (channel message))
 *
 *  Outgoing:   sendJson (payload, statusMessage)   is the door into the basement glue.
 *
 *  SYSTEM SETUP
 *  function setup, which calls message "hello".
 *  - Menu gets displayed upon receipt of an event coded "makemenu".
 *
/BioLisp/ThirdParty/monkeylib/ajax  /queue.js, .ajax.js.
window.queue is created in ajax, it's a very simple hand-rolled Queue.

CATALOG OF SEND COMMANDS  Oct '12
hello			kicks off everything, request for fill-in provisioning
browser-page-closed	the entire page got closed.  Or moved away from.
global-keypress
palettemenutooltips
search-box-request
palettemenumouseclick
click-workspace
click-results
clear-workspace
clear-results
left-undo
right-redo
boxmenumouseclick
update-open-box-contents
input-text          when Open Hole receives a CR.
input-text-tab
input-multiline-text
input-multiline-text-tab
box-out   "perhaps ignored on server side?"
box-click
box-clear-hole-click
box-double-click
icon-click
box-delete-click
box-clear-click
box-clear-delete-click
dnd-box
dnd-box-after
dnd-box-before
palettemenuitemhelp
boxmenumultiselect
user-popup-input
palettemenumouseclick
 */

window.dbg = (function (text) {
  return (function (debugging) {
    if (debugging) {
      return debugging.innerHTML += ("<p><font color='#00aa00'> " + text + "</font>");
    } else {
      return undefined;
    }
  }).call(this, document.getElementById("debugging"));
});

function DUMP(obj)
{//	var n="";
 //	for(var name in obj) n += name + "\n";
	var n = dumptxt(obj);
	alert( n );
}
function dumptxt(arr,level) {
	var dumped_text = "";
	if(!level) level = 0;
	if(level > 2) return "...";
	
	//The padding given at the beginning of the line.
	var level_padding = "";
	for(var j=0;j<level+1;j++) level_padding += "    ";
	
	if(typeof(arr) == 'object') { //Array/Hashes/Objects 
		for(var item in arr) {
			var value = arr[item];
			
			if(typeof(value) == 'object') { //If it is an array,
				dumped_text += level_padding + "'" + item + "' ::=\n";
				dumped_text += dumptxt(value,level+1);
			} else {
				dumped_text += level_padding + "'" + item + "' => \"" + value + "\"\n";
			}
		}
	} else { //Strings/Chars/Numbers etc.
		dumped_text = "===>"+arr+"<===("+typeof(arr)+")";
	}
	return dumped_text;
}

function setWaitCursor() { document.body.style.cursor = "progress"; }
function clearCursor()   { document.body.style.cursor = "auto"; }

var preloadSpinningBikeIntoCache = new Image();
preloadSpinningBikeIntoCache.src = "images/BioBike_Animated.gif";  //this should do it. You don't reuse this, just call it once.

function setBikeAnimation() { $("#palette-logo").attr("src", "images/BioBike_Animated.gif");}
function clearBikeAnimation() { $("#palette-logo").attr("src",  "images/Bike.gif");}

function SetWaitCursors() { setWaitCursor(); setBikeAnimation(); }

// called every time a msg or ACK comes in from server.  Dead man's switch.  Redefine this to clear cursor, or whatever.
function ServerStillAliveAndKicking_Callback() { KillKickinWaitCursor(); showBike(); } 
function ServerMajorTimeout_Callback() {hideBike();} //hiding bike reveals big red sign.
function hideBike() { $("#palette-logo").hide(); }
function showBike() { $("#palette-logo").show(); }


function inputMaxWidthChars() {
  var widthOfEntireInsideWindowMinusGoodluck_inPixels = window.innerWidth - 50;

  // Apparently the following only works in more modern CSS3 browsers.  We should be safe.
  //  var scopeElement = $('input[type=text]');  //Does this work if we have no inputs on the screen?  I hope so.
  //var scopeElement = $('textarea');  //Does this work if we have no inputs on the screen?  I hope so.
  //var scopeElement = $('#workspace-help-search');  //Does this work if we have no inputs on the screen?  I hope so.
  // These are all returning Inifity.  Fall back to rough approximation.  Sorry.
  var widthOfEmInInput = //$(widthOfEntireInsideWindowMinusGoodluck_inPixels).toEm({'scope': scopeElement})
                           widthOfEntireInsideWindowMinusGoodluck_inPixels / 9;  //fallback if previous bombs out,
                         // perhaps if due to no inputs on the field and we're only working with textareas?
  return widthOfEmInInput;
}




var WaitCursorKickinTimer;
function KickinWaitCursorAfterSecs(secs)
// ...As we are using a single global to store this delay,
// it becomes necessary to clear out any older, pending one
// or else clearing the current one will NOT clear all pending ones.
// Note that calling clearTimeout(undefined) is perfectly safe, by the manual.
//
{  clearTimeout( WaitCursorKickinTimer );  //surprise, this line is needed.
   WaitCursorKickinTimer = setTimeout( SetWaitCursors, secs * 1000);
   //  alert("set " + secs );
}
function KillKickinWaitCursor() 
{  clearCursor();   //Main one to do work, in case next function fails.
   clearBikeAnimation();
   clearTimeout( WaitCursorKickinTimer );
   clearCursor();  //This one's 'cause I'm superstitious.  What if it's on hairy edge?
//...in other words, it could be possible to clear the cursor; have the animation fire between lines; 
// then kill the Timer; and still have the animation going.
 // However, if clearTimeout fails, then the third set could never fire.  All unlikely, but hence the double wham for luck.
   clearBikeAnimation();
   //   alert("cleared!");
}



/*  The following is black magic to create our own local "afterpaste" event.
  This is necessary as "onpaste" fires AS the paste is happening <right before>,
  but before it is completed.  So you can't count the length of the paste until after it's finished.
  This magic allows you to hook up your own afterpaste event handler.
  Note that the delayed timer with a 0 apparently fires after everything is finished;
  I'm setting the delay to 1 ms just for good luck.
  Execute these lines at the top level.

        CURRENTLY *DEPRECATED*

  ...This WORKS, BUT it wipes out the onpaste event!  NO GOOD!
  So better simply to fire the event after each individual paste!

Hoho.  NOW we DON'T want the onpaste, but WANT the afterpaste, so this is APPROPRIATE and USED.
Oops!  Nope!  Event goes on OLD pasted slot, we need it to trigger on NEW one.

// Define a fake afterpaste event:
// (Works by allowing browser to complete the clipboard action before raising custom event)
  $('HTML').live('paste', function(e){
    e = $.extend({},e,{type:'afterpaste'});
    window.setTimeout(function(){ $(e.target).trigger(e) },5);  //JKM was 0. Time in milliseconds.
  });
//  End of afterpaste black magic.
*/


var VPL = (function () {

  function debug (x) {  alert(x); }

  function debugWhere(x) {
    var where = functionName(arguments.callee.caller);
    debug('in ' + where + ': ' + x);
  }

  function functionName (fn) {
    var str = fn.toString();
    return (str.substring(str.indexOf("function") + 8, str.indexOf("(")) || "anoynmous");
  }

// Function for handling graceful shutdown.
//  Must be exposed (exported) to the outside world using particular voodoo at bottom of page.
    function shutdown () { 
	    sendJson({ type: "browser-page-closed" });
 	}	

  var globalDefaultTextColor = "#000000";
  var boxFlashColor = "#00AA00";
  var lastMouseX = "0";
  var lastMouseY = "0";
  var mainExpression = false;
  var selectedHole = false;
  var menuVisible = false; // menu being shown
  var savedTextNode = null; // text to be restored if nothing entered
  var copiedItem = null; // copy of a node we might paste
  var copiedSexp = null; // copy of a sexp we might paste
  var emptySexp = null;
  var DDmenuID = 0; // uniquification for menus.
  var DDmenus = [];
  var toolTips = [];

  var sessionCounter = null;

  //
  // SettingsStorage
  //
  // Author: Johnny Casey
  // Converted from lispscript by Peter Seibel
  //
  // Provides a class to manage registering, storing and accessing
  // application client settings.
  //

  var SettingsStorage = (function () {
    
    // Private static functions
    function defaultUpdate (value) {
      // Generic client-setting variable update function. Either get
      // a value or set to false
      return (value || false);
    }

    var emptySetting = {};

    // Instance variable
    var mySelf = this;
    var mySettings = {};

    // Constructor
    function settingsStorageConstructor () {

      // Instance functions

      // Initializes a client-setting variable
      mySelf.initialize = function (name, initialValue, valueUpdate) {
        var setting = {};
        setting.value = (initialValue || false);
        setting.update = (valueUpdate || defaultUpdate);
        if (!(mySettings[name])) {
          mySettings[name] = setting;
        }
      };

      // Returns the requested client-setting
      mySelf.get = function (name, defaultValue) {
        return ((mySettings[name] || emptySetting).value || defaultValue);
      };

      // Updates a client-setting to the specified value using an
      // appropriate updater.
      mySelf.set = function (name, value) {
        var setting = mySettings[name];
        if (setting) {
          setting.value = setting.update(value);
        }
      };
      mySelf.normalizeName = settingsStorageConstructor.normalizeName;
      return mySelf;
    }

    // Public static functions
    settingsStorageConstructor.normalizeName = function (name) {
      return name.toLowerCase();
    };

    return settingsStorageConstructor;

  }());

  var clientSettings = new SettingsStorage();
  clientSettings.initialize("new-hole-opening-mode");
  clientSettings.initialize("show-status-overflow-link", true);
  clientSettings.initialize("default-jbml-dnd-drag", true);
  clientSettings.initialize("default-jbml-dnd-drop");
  clientSettings.initialize("single-click-delay", 30);
  clientSettings.initialize("drag-click-delay", 100);

  ////////////////////////////////////////////////////////////////

  // Collects an associative array of values from the URL.
  function getArgsFromURL () {
    var myargs = {};
    var query = location.search.substring(1); // Rip off initial '?'
    var pairs = query.split(","); // Break apart by commas
    for (var i = 0; i < pairs.length; i++) {
      var pos = pairs[i].indexOf("="); // Find position of '='
      var argname = "";
      var value = "";
      if (pos > -1) {
        argname = pairs[i].substring(0, pos);
        value = pairs[i].substring(pos + 1);
        myargs[argname] = unescape(value);
      }
    }
    return myargs;
  }

  // Retrieve the session-id from the current URL or "0" if not found
  function getSessionId () {
    return (getArgsFromURL().PKG || "0");
  }

  // 
  // Client side incoming message dispatch function
  //

  // "This is the function for dispatching incoming pipe messages." No.  See next vplJsonOnMessage after assoc array.
  function vplOnMessage (message) {
    alert("Uh oh! Got non JSON message:\n\n" +  (new XMLSerializer().serializeToString(message)) + "\nCheck the Lisp code.");
  }

//  Jeff has requested that the Client Status feedback be cleared
//  "after the results of a box execution come back".   WHICH execution is unspecified.
//  Rather than bury these in random places,
//  I have modified them here in the dispatch lambda functions that seem to make sense.
//  Lurch says "Uuhhhhhhnnnnn...." but takes care of it anyway.
//  Note that clearClientStatus kills things on a three-second timer, so you won't miss things anyway.

//  Note this is an associative array, not a function.  Switch uses key-word lookup, so is done implicitly through the hash.
  var vplJsonMessageDispatcher = {
    // Uncategorized 
    "change-client-settings":  function (m) { changeClientSettings(m.settings); },
    "popup-url-window":        function (m) { popupURLWindow(m.windowID, m.url, m.specs, m.relative); },
    "kill-window":             function (m) { killWindowId(m.windowID); },
    "browser-title":           function (m) { document.title = m.title; },
    "start-execution":         function (m) { 
      //                                 KickinWaitCursorAfterSecs(1);   //This one is required, or else block the previous turnoff.
			executionStatusBox.init(m.opcode,m.timeout); },
    "update-execution-status": function (m) { clearClientStatus();    executionStatusBox.update('update',m); }, 
    "end-execution":           function (m) { clearClientStatus();    
      //			KillKickinWaitCursor();  //JKM Oct 1 '12 
			executionStatusBox.update('end',''); }, 

    // Workspace commands
    "clear-workspace":         function (m) { clearClientStatus();   clearWorkspace(); },
    "focus-box":               function (m) { focusBox(m.boxID); },

    // Results commands
    "clear-results":           function (m) { clearClientStatus();   clearResults(); },

   // Help Pane commands (incoming from Server)...obsolete.
   "show-help-pane":           function (m) { showHelpPane(); },
   "clear-help-pane":            function (m) { clearHelpPane(); },
   "hide-help-pane":             function (m) { hideHelpPane(); },

    // Palette commands
    "clear-palette":          function (m) { clearPalette(); },
    "make-menus":             function (m) { makeMenus(m.menus); },
    "make-menu":              function (m) { makeMenu(m.menu, false); },
    "remove-menu":            function (m) { removeMenu(m.menuID); },
    "replace-menu":           function (m) { makeMenu(m.menu, true); }, // Note: Only for palette menus!  Use redraw-box for others!
    "palettemenutooltips":	  function (m) { for(tt in m.data) toolTips[tt]=m.data[tt]; },
    
    // Overall general commands
    "flashing-hilight-box":   function (m) { flashingHilightBoxID(m.boxID); },
    "unflashing-hilight-box": function (m) { unflashingHilightBoxID(m.boxID); },
    "error-message":          function (m) { createDisplayErrorModalPopup(m.error); },
    "show-dialog":            function (m) { showDialog(m.id, m.title, m.label, m.dialogType, m.dialogTypeOptions); },
    "show-status":            function (m) { showServerStatus(m.message); },

    // boxes
    "add-workspace":          function (m) { clearClientStatus();   addWorkspaceBox(m.boxes); },
    "redraw-box":             function (m) { clearClientStatus();   redrawBox(m.boxes); },
    "add-results":            function (m) { clearClientStatus();   addResultsBox(m.boxes); }

  };

  function vplJsonOnMessage (message) {
    var parsed  = JSON.parse(DomLibrary.getTextContent(message));
    var handler = vplJsonMessageDispatcher[parsed.type];    //this is a hash table, see directly above.

    if (parsed.sessionID) {
      // In the client we have a distinct session id and session
      // counter but for some reason the server uses 'sessionID' to
      // send us what we think of as the session counter. Go figure.
      // -Peter
      sessionCounter = parsed.sessionID;
    }

    if (handler) {
      //alert("came back " + parsed.type);
      if( parsed.type != "show-status" && parsed.type != "update-execution-status"  && parsed.type != "start-execution") {
	//      alert("clear!");
      KillKickinWaitCursor();  //JKM Sept 17 '12 
	} 
    //  else alert("IGNORE!");
      handler(parsed)
    } else {
      alert("Unrecognized JSON message: " + message);
    }
  }

  function geckoRevision () {
    return (("Gecko" === navigator.product) && 
            parseFloat(navigator.userAgent.substring(navigator.userAgent.indexOf("rv:") + 3)));
  }

  function isReallyChrome () 
    {
    return (-1 < (navigator.userAgent.indexOf("Chrome")));
    }

  function redirectOnIncompatibleBrowser () {
    // See http://www.seehowitruns.org/index.php for an online database of browser javascript signatures.
    // See https://addons.mozilla.org/firefox/59/ for "User Agent Switcher" by Chris Pederick addon to test old code (only used appName/appVersion).
    // See http://developer.mozilla.org/en/docs/CVS_Tags for the dates of various Mozilla/Firefox releases.  Firefox 1.5 release date is 2005/11/11.

    // JJC 2007/01/21
    // Keep this simple.  Only use builtin JavaScript 1.2 compatible code.
    // If the test fails, the redirection does not happen.
    // No string-equals.
    var browserName = navigator.appName;

    // Use parseFloat, which is a builtin Javascript function which
    // converts a string to a float. Trailing characters are ignored,
    // must begin with a number (which is what the original call to
    // .substring() did).
    var browserVers = parseFloat(navigator.appVersion);
    var browserProduct = navigator.product;
    var browserProductVers = parseInt(navigator.productSub, 10);

    var isNetscape = (browserName === "Netscape");
    var versionOK = (browserVers >= 5.0);
    var isGecko = (browserProduct === "Gecko"); 
    var revisionOK = (1.8 <= geckoRevision());
    var isFirefox = (isNetscape && versionOK && isGecko && revisionOK);
    var isChrome = isReallyChrome();

    if (!isChrome && !isFirefox)
    {
      window.location = "vpl-other.html";
    }
  }

  function handleDocumentKeypress (e) {
    e = EventLibrary.getEvent(e);
    if (e.ctrlKey) {
	// took out "Pressed Global Key" , JP per Jeff E
      sendJson({ type: "global-keypress", key: EventLibrary.getKeypressCharCode(e), ctrl: e.ctrlKey }, "")
    }
    // Allow the event to propagate to the browser
  }

  function handleDocumentResize (e) {
    updateToolboxes();
  }




  // JP 06/18/06 -- finally figured out a way to make the menu clear by
  // attaching an onclick method to the underlying object representing
  // the workspace.


  // This is the function for getting the server to set up the client
  // properly.
  function setup () {
    redirectOnIncompatibleBrowser();

    // Get server to set up.
    sendJson({ type: "hello" });
    setupPalette();
    setupWorkspace();
    setupResults();
    /* replaced with liquid layout - need to remove references from codebase - Sam Wright 03/29/2010
        updateToolboxes();*/
    document.onkeypress = handleDocumentKeypress;
    // document.onresize = handleDocumentResize;
    
    // initialize layout
    liquidLayout();
	
	// layer in tooltips
	sendJson({ type: "palettemenutooltips" });
	window.setTimeout(applyToolTips,2000);
    
    // adjust layout on resize
    $(window).resize(function() {
      liquidLayout();
    });
    
    // dragAndDropSetup();
	
	// initialize click handler on search box in workspace
	$('#workspace-help-search')
	.keydown(function(e){
        if ( (e.keyCode == 13) || (e.keyCode == 9) ) {
            if(this.value) sendJson({ type: "search-box-request", text: this.value }, "");
        }
    });
  }




  function liquidLayout () {
        // raw height of available window space
        var rawHeight = $(window).height() - 126;
        
        // if workspace not visible
        if($('#workspace-box').is(':hidden')) {
                var rbh = rawHeight-12;
                var rh = rbh-$('#results-toolbox').height();
                $('#results-box').height(rbh).css('margin','0 10px 10px 10px');
                $('#results').height(rh);
        // if results not visible
        } else if($('#results-box').is(':hidden')) {
                var wbh = rawHeight-12;
                var wh = wbh-$('#workspace-toolbox').height();
                $('#workspace-box').height(wbh).css('margin','0 10px 10px 10px');
                $('#workspace').height(wh);
        // if workspace is visible
        } else {
                // if results is set at intermediate height
                if($('#workspace-box').is(':visible') && ($('#results-shade').is(':visible'))) {
                        var rawWbh = (rawHeight*.5);
                        var wbh = Math.round(rawWbh)-2;
                        var wh = wbh-$('#workspace-toolbox').height();
                        var rbh = rawHeight-wbh-17;
                        var rh = rbh-$('#results-toolbox').height();
                        $('#workspace-box').height(wbh).css('margin','0 10px');
                        $('#workspace').height(wh);
                        $('#results-box').height(rbh).css('margin','5px 10px 10px 10px');
                        $('#results').height(rh);                       
                // default configuration
                } else {
                        var rawWbh = (rawHeight*.85);
                        var wbh = Math.round(rawWbh)-2;
                        var wh = wbh-$('#workspace-toolbox').height();
                        var rbh = rawHeight-wbh-17;
                        var rh = rbh-$('#results-toolbox').height();
                        $('#workspace-box').height(wbh).css('margin','0 10px');
                        $('#workspace').height(wh);
                        $('#results-box').height(rbh).css('margin','5px 10px 10px 10px');
                        $('#results').height(rh);
                }
        }
  }

  function setupWorkspace () {
    var dummy = {};
    var workspace = document.getElementById("workspace");
    var undo = document.getElementById("left-undo");
    var redo = document.getElementById("right-redo");
    var clear = document.getElementById("workspace-clear");
    var expand = document.getElementById("workspace-expand");
    var shade = document.getElementById("workspace-shade");

    (workspace || dummy).onclick = doClickWorkspace;
    (undo || dummy).onclick = doLeftUndoClick;
    (redo || dummy).onclick = doRightRedoClick;
    (clear || dummy).onclick = doClearWorkspace;
    /*(expand || dummy).onclick = makeShadeExpandHandler('workspaceExpandable');
    (shade || dummy).onclick = makeShadeExpandHandler('workspaceShadable');*/
        $('#workspace-expand').click(function() {
                $('#results-box').hide();
                if($('#workspace-box').is(':hidden')) $('#workspace-box').show();
                if($('#workspace-shade').is(':hidden')) $('#workspace-shade').show();
                $('#workspace-expand').hide();
                
                liquidLayout();
        });
    $('#workspace-shade').click(function() {
                if($('#results-box').is(':hidden')) $('#results-box').show();
                if($('#workspace-expand').is(':hidden')) $('#workspace-expand').show();
                $('#workspace-shade').hide();
                
                liquidLayout();
        });
  }

  function setupResults () {
    var dummy = {};
    var results = document.getElementById("results");
    var clear = document.getElementById("results-clear");
    var expand = document.getElementById("results-expand");
    var shade = document.getElementById("results-shade");

    (results || dummy).onclick = doClickResults;
    (clear || dummy).onclick = doClearResults;
    /*(expand || dummy).onclick = makeShadeExpandHandler('resultsExpandable');
    (shade || dummy).onclick = makeShadeExpandHandler('resultsShadable');
    splitStates.smallResults.apply();*/
        $('#results-expand').click(function() {
                // raw height of available window space
                var rawHeight = $(window).height() - 126;
                
                // if results is set at intermediate height
                if($('#workspace-box').is(':visible') && ($('#results-shade').is(':visible'))) {
                        $('#workspace-box').hide();
                        if($('#results-box').is(':hidden')) $('#results-box').show();
                        if($('#results-shade').is(':hidden')) $('#results-shade').show();
                        $('#results-expand').hide();
                        
                        liquidLayout();
                                                
                // default configuration
                } else {
                        if($('#results-box').is(':hidden')) $('#results-box').show();
                        if($('#results-shade').is(':hidden')) $('#results-shade').show();
                        
                        liquidLayout();
                }
        });
    $('#results-shade').click(function() {
                if($('#workspace-box').is(':hidden')) {
                        $('#workspace-box').show();
                        if($('#results-expand').is(':hidden')) $('#results-expand').show();
                } else {
                        if($('#results-expand').is(':hidden')) $('#results-expand').show();
                        $('#results-shade').hide();
                }
                
                liquidLayout();
        });
  }

  function setupPalette () {
    var logo = 
      $('<div width="122px" height="107px" style="float: right; position: relative;"><img id="BigRed-logo"  width="122px" height="107px" style="" SRC="images/BigRed.gif" border="0px" /><img id="palette-logo"  width="122px" height="107px" style="float: right; position: absolute; top: 0; right: 0; zIndex: 100" SRC="images/Bike.gif" border="0px" /></div>')
      .click(function () { alert("VPL Client Version: " + vplVersion); });
    $('#palette').append(logo);
  }

  function noOp () {}

  // Execution Status Box
  var executionStatusBox = {
	shown: false,
	delay: 2000,
	execDelay: 0,
	init: function(pid,timeout) {
		// set the executing process id
		executionStatusBox.pid = pid;
		// set the initial timeout
		executionStatusBox.timerID = window.setTimeout(executionStatusBox.show,executionStatusBox.delay);
		// set the timeout (passed from server)
		executionStatusBox.timeout = timeout;
	},
	show: function() {
		// remove delay timer
		window.clearTimeout(executionStatusBox.timerID);
		// show the box
		$('#execution-status-box').show();
		// flag that the box has been shown
		executionStatusBox.shown=true;
		// update contents
		executionStatusBox.update('init');
	},
	update: function(type,msg) {
		if(type=='end') {
			// if still within delay, just delete the timer
			if(!executionStatusBox.shown) {
				// remove delay timer
				window.clearTimeout(executionStatusBox.timerID);
			// otherwise, update the existing message box with the end execution message
			} else {
				// reset shown flag
				executionStatusBox.shown=false;
				// remove delay timer
				window.clearTimeout(executionStatusBox.timerID);
				// hide the kill button
				$('#execution-status-box-kill').hide();
				// show the close button
				if (!$('#execution-status-box-close').length) {
					$('#execution-status-box').append($('<img/>').attr({
						id: 'execution-status-box-close',
						width: '23',
						height: '23',
						alt: 'Close Execution Status Box',
						src: 'images/bbtn_close.gif'
					}).click(function(){
						$('#execution-status-box').hide()
					}).hide());
				} else {
					$('#execution-status-box-close').show();
				}
				// show user updated status
				$('#status-msg').addClass('success').html('Completed.');
				$('#status-timer').html('');
				// hide the execution status box on completion.
				$('#execution-status-box').hide();
			}
		// if 2 second delay is triggered
		} else if(type=='init') {
			// initial delay in seconds
			executionStatusBox.execDelay = (executionStatusBox.delay/1000);
			// show user the initial delay
			$('#statusTimer').html(executionStatusBox.execDelay + ' seconds.');
			// set a timer to update the delay every second
			executionStatusBox.timerID = window.setTimeout(executionStatusBox.updateTimer,1000);
			// hide the close button (relevant when there are repeat executions)
			$('#execution-status-box-close').hide();
			// show the kill button
			if (!$('#execution-status-box-kill').length) {
				$('#execution-status-box').append($('<img/>').attr({
					id: 'execution-status-box-kill',
					width: '23',
					height: '23',
					title: 'Kill Process',
					src: 'images/bbtn_kill-red.gif'
				}).click(function(){
					// kill the process
					sendJson({
						type: "palettemenumouseclick",
						id: executionStatusBox.pid
					}, 
		 		// "Killing currently executing process."
				""
					);
					// remove delay timer
					window.clearTimeout(executionStatusBox.timerID);
					// remove the execution status box
					$('#execution-status-box').hide();
				}).show());
			} else {
				$('#execution-status-box-kill').show();
			}
			// show user updated status
			$('#status-msg').html('Executing...');
		// if server sends execution update
		} else if(type=='update') {
			// update execution status if past delay...
			if(executionStatusBox.shown) $('#status-msg').html(msg);
			// remove delay timer
			window.clearTimeout(executionStatusBox.timerID);
		}
	},
	updateTimer: function() {
		// new time value
		executionStatusBox.execDelay = executionStatusBox.execDelay+1;
		// timeout message
		if(executionStatusBox.timeout) var tMsg = ' Timeout at ' + executionStatusBox.timeout + 's.';
		else var tMsg = '';
		// show user new time value
		$('#status-timer').html(executionStatusBox.execDelay + 's.' + tMsg);
		// set new timer to update in a second
		executionStatusBox.timerID = window.setTimeout(executionStatusBox.updateTimer,1000);
	}
  }

  channel.registerMessageHandler("vpl", vplOnMessage);
  channel.registerMessageHandler("vpl-json", vplJsonOnMessage);
  //channel.logger = function (x) { alert(x); };

  //
  // Click handler functions
  //

  // Retrieves the timer stored on a node
  function getMouseDelayTimer (node) {
    return (node.xMouseDelayTimer || false);
  }

  // Stores a timer on a node for later so it can be cleared.
  function setMouseDelayTimer (node, timer) {
    node.xMouseDelayTimer = timer;
  }

  // Clears a timer stored in a node and removes it.
  function clearMouseDelayTimer (node) {
    if (getMouseDelayTimer(node)) {
      // Stop single click count down, if any.
      clearTimeout(getMouseDelayTimer(node));
    }
    setMouseDelayTimer(node, null);
  }

  // Wraps a single click for conflict against double clicks on same
  // object. NOTE: the second argument must be a function object Because
  // this is node-based, it handles different objects at same time w/ no
  // problem.
  function singleClickWrapper (node, callback, clickDelay) {
    function augmentedCallback () {
      callback();
      clearMouseDelayTimer(node);
    }
    clickDelay = (clickDelay || clientSettings.get("single-click-delay"));
    clearMouseDelayTimer(node);
    setMouseDelayTimer(node, setTimeout(augmentedCallback, clickDelay));
  }

  ////////////////////////////////////////////////////////////////////////
  // Messaging to server -- the outer layer of the messages we send is
  // XML because that's what the Monkeylib messaging layer expects. But
  // the payload of the message is a string containing JSON-encoded data.

  function makeVplJsonMessage (payload) {
    var message = makeDocumentObject("", "VPL-JSON", null);
    var payloadText = document.createTextNode(JSON.stringify(payload));
    message.documentElement.appendChild(payloadText);
   //DUMP( payloadText ); //This is quite huge, about 100 lines, apparently with much unnecessary stuff. Not useful. Use payload.type.
    return message;
  }

  function sendJson (payload, statusMessage) {  //statusMessage contains informative text, which may change easily.

    dbg( "sendJson( " + payload.type +  (statusMessage ? (", " +'"'+statusMessage + '"') : "") + " )" );  //shows up in debug window, underneath.  green.
//    alert("called sendJson( " + payload.type +  (statusMessage ? (", " +'"'+statusMessage + '"') : "") + " )." );     //Rip this out later when this works.
	//DUMP(payload);
    //DUMP(statusMessage);  // not very useful.

    //~Oct '12: Putting this here is too aggressive.  E.g. mouse moves.  Needs to be on command-by-command basis.
    //~Nov '12: Fix is to list all the mouse moves, etc., that you DON'T want the cursor to kick in on.  Then exclude them.
    //Dec 31 '12: NewCoke dogma is to turn cursor on every time you blink.
    if( true ||
	payload.type != "box-out"  && 
        payload.type != "click-workspace"  && 
        payload.type != "browser-page-closed" && 
        payload.type != "update-open-box-contents" && 
        payload.type != "box-click" )            //JKM Oct 2 '12, Dec 27 '12
      {
	//	alert("Started!");
    KickinWaitCursorAfterSecs(1.0);  //Start the whirling wait cursor after 1.0 seconds, if not canceled when reply comes back.  JKM Sept 17 '12
      }

    showClientStatus(statusMessage);

    // Augment message with session-id and session-counter. (The
    // latter not sent on the 'hello' message.)
    payload.sessionID = getSessionId();
    if (sessionCounter !== null) {
      payload.sessionCounter = sessionCounter;
    }

    channel.send(makeVplJsonMessage(payload));   //see 3/4 page up ^
  }

  function doClickWorkspace () {
    Menu.closeMenus();
    // JP per Jeff E -- changed second argument from "Clicked on Workspace"
    sendJson({ type: "click-workspace" }, "");
  }

  function doClickResults () {
    Menu.closeMenus();
    // JP per Jeff E -- changed second argument from "Clicked on Results"
    sendJson({ type: "click-results" }, "");
  }

  // Sends a 'clear-workspace' message to the server. Currently sent
  // when the user selects the clear workspace icon.
  function doClearWorkspace () {
    doUpdateOpenBoxContents(JbmlTracked.getTrackedObjects(document.getElementById("workspace")));
    // JP per Jeff E -- removed second arg: "Clear workspace"
    sendJson({ type: "clear-workspace" }, "");
    if($('#execution-status-box').length) $('#execution-status-box').remove();
  }

  // Sends a 'clear-results' message to the server. Currently sent when
  // the user selects the clear results icon.
  function doClearResults () {
    doUpdateOpenBoxContents(JbmlTracked.getTrackedObjects(document.getElementById("results")));
    // JP per Jeff E -- removed second arg: "Clear Results"
    sendJson({ type: "clear-results" }, "");
  }

  // Sends a 'left-undo' message to the server. Currently sent when the
  // user selects the undo workspace icon.
  function doLeftUndoClick () {
    doUpdateOpenBoxContents(JbmlTracked.getTrackedObjects(document.getElementById("workspace")));
    sendJson({ type: "left-undo" }, 
	""
 // "Undo."
	);
  }

  // Sends a 'right-redo' message to the server. Currently sent when the
  // user selects the redo workspace icon.
  function doRightRedoClick () {
    sendJson({ type: "right-redo" }, 
	""
	// "Redo."
	);
  }

  // Sends a 'boxmenumouseclick' message containing a menuentry code and
  // box ID code to the server.
  function doBoxmenuMouseClick (menuId, boxId, title) {
    // JP per Jeff E -- changed second argument from 
    // "Clicked " + (title || "Box Menu") + "."
    sendJson({ type: "boxmenumouseclick", opcode: menuId, boxid: boxId }, "");
  }

  function doUpdateOpenBoxContents (boxes) {
    // This message is used to tell the server about the values of
    // open boxes in the workspace which may have changed only in the
    // client, e.g. the user clicks in a box and edits it and then
    // clicks to another box without hitting enter. The server can
    // then send back opened holes with the correct value.
    // It also is a general-purpose message that transmits cleared workspace, 
    // cleared output space, etc.
    // The value should have been set properly by now.
    // THE ROUND-TRIP FUNCTIONALITY OF THIS IS NOT WORKING PROPERLY YET--
    // OPEN HOLE CONTENTS NOT REFRESHING PROPERLY  -- JKM Dec '12
    // FIX ME

    if (boxes.length) {
      var boxesJson = [];
      
      function boxJson (box) {
	var jbml  = Jbml.getJbml(box);
	var boxId = "" + Jbml.getBoxId(box);
	var type  = jbml.getObjectType();
	var value = box.value;
	return { boxid: boxId, type: type, value: value };
      }
      
      for (var i = 0; i < boxes.length; i++) {
	boxesJson.push(boxJson(boxes[i]));
      }
      
      sendJson({ type: "update-open-box-contents", boxes: boxesJson }, "Upated Box Contents"); 
    }
  }

  // Sends an 'input-text' message containing a box ID and value to the server.
  // Currently sent when an open hole receives a carriage return.
  function doInputText (boxId, value) {
    doUpdateOpenBoxContents(JbmlTracked.getParentTrackedObjects(document.getElementById(boxId), boxId));
    // JP per Jeff E -- changed second argument from "Entered Input"
    sendJson({ type: "input-text", boxid: boxId, value: value }, "");

  }

  // Sends an 'input-text-tab' message containing a box ID and value to
  // the server. Currently sent when an open hole receives a tab.
  function doInputTextTab (boxId, value) {
    doUpdateOpenBoxContents(JbmlTracked.getParentTrackedObjects(document.getElementById(boxId), boxId));
    // JP per Jeff E -- changed second argument from "Entered Input + Tab"
    sendJson({ type: "input-text-tab", boxid: boxId, value: value }, "");
	return false;
  }

  // Sends an 'input-multiline-text' message containing a box ID and a
  // list of line values to the server. Currently sent when an open
  // multiline hole receives a send signal.
  function doInputMultilineText (boxId, value) {
    doUpdateOpenBoxContents(JbmlTracked.getParentTrackedObjects(document.getElementById(boxId), boxId));
    sendJson({ type: "input-multiline-text", boxid: boxId, value: value }, "Entered Multiline Input.");
  }

  // Sends an 'input-multiline-text-tab' message containing a box ID and a
  // list of line values to the server. Currently sent when an open
  // multiline hole receives a send signal XXX tab key, presumably.  Doc was copied from prev but not modified, uncertain JKM 9/7/12 clean when sure.
  function doInputMultilineTextTab (boxId, value) {
    doUpdateOpenBoxContents(JbmlTracked.getParentTrackedObjects(document.getElementById(boxId), boxId));
    sendJson({ type: "input-multiline-text-tab", boxid: boxId, value: value }, "Entered Multiline Input + Tab.");
  }

  // This just clutters up the console.  These commands are not
  // processed by the server.  Please don't put them back until we
  // need them.

  // Sends a 'box-out' message containing a box ID to the server.
  // Currently disabled.
  function doBoxMouseOut (boxId) {

    doUpdateOpenBoxContents(JbmlTracked.getTrackedObjects(document.getElementById(boxId)));  //JKM Dec '12

    // Not needed

    // FIXME 2010-04-19 <peter@gigamonkeys.com> -- yet the code to
    // send the message wasn't commented out. It *seems* to be ignored
    // on the server side so this could probably go away but it'd need
    // more checking than I want to do right now.
    sendJson({ type: "box-out", boxId: boxId });
  }

  function doHoleBlur(boxId) {

    doUpdateOpenBoxContents(JbmlTracked.getTrackedObjects(document.getElementById(boxId)));  //JKM Dec '12
  }



  // Sends a 'box-click' message to the server. Disable all right-click
  // messages to server. Server treats them as a noop now anyway. No
  // sense even sending them. -- JP
  function doBoxMouseClick (boxId, mouseCode) {
    if (EventLibrary.rightMouseButton !== mouseCode) {

      // DEPRECATED, DELETE SOON --too general, all kinds of boxes.
      //alert("clicked " + boxId);
      //JKM Dec '12  FIX ME
      //var box = document.getElementById(boxId);
      //box.focus();

      doUpdateOpenBoxContents(JbmlTracked.getTrackedObjects(document.getElementById(boxId)));
      // JP per Jeff E -- changed second argument from "Clicked Box."
      sendJson({ type: "box-click", boxid: boxId, mousecode: mouseCode }, "");
    }
  }

  // This routine is called when a closed hole box region is clicked. It
  // swaps the label out for a hole by itself locally.
  //
  // ! Must turn itself off when opening hole!!
  //
  // IF YOU CLICK ON A CLOSED HOLE, THEN IT AUTOEXPANDS INTO AN INPUTBOX *LOCALLY*.
  function doClosedHoleMouseClick (boxId, mouseCode) {
    if (!(clientSettings.get("new-hole-opening-mode"))) {
      var box = document.getElementById(boxId);
      var setup = Jbml.getJbml(box).open;
      if (setup) {
        setup(box, "");
      }
      //JKM Dec '12 
      box.focus();

    }
    // There should be nothing to track if the hole is "closed"
   // JP per Jeff E -- changed second arg from "Clicked Closed Hole."
    sendJson({ type: "box-click", boxid: boxId, mousecode: mouseCode }, "");
  }

  function doBoxClearHoleMouseClick (boxId, mouseCode) {
    if (!(clientSettings.get("new-hole-opening-mode"))) {
      var box = document.getElementById(boxId);
      var setup = Jbml.getJbml(box).close;
      if (setup) {
        setup(box);
      }
    }
    doUpdateOpenBoxContents(JbmlTracked.getTrackedObjects(document.getElementById(boxId)));
    sendJson({ type: "box-clear-hole-click", boxid: boxId, mousecode: mouseCode }, "Clear Hole.");
  }

  // Sends a 'box-double-click' message containing a box ID and mouse
  // button to the server.
  function doBoxMouseDoubleClick (boxId, mouseCode) {
    doUpdateOpenBoxContents(JbmlTracked.getTrackedObjects(document.getElementById(boxId)));
    // JP per Jeff E -- changed second argument from "Double Clicked Box."
    sendJson({ type: "box-double-click", boxid: boxId, mousecode: mouseCode }, "");
	// show client message if boxId is within the workspace
	if($(boxId,'#workspace-box')) showClientStatus('Submitted!');
  }

  // Sends an 'icon-click' message containing a box ID and icon code to
  // the server.
  // Seems to be obsolete.  Is not called within vpl.js and the corresponding
  // server function does not exist -- JP
  function doBoxIconClick (boxId, iconCode) {
    doUpdateOpenBoxContents(JbmlTracked.getParentTrackedObjects(document.getElementById(boxId)));
    sendJson({ type: "icon-click", boxid: boxId, iconcode: iconCode }, "Clicked Icon.");
  }

  // Sends a 'box-delete-click' message containing a box ID and mouse
  // button to the server.
  function doBoxDeleteMouseClick (boxId, mouseCode) {
    doUpdateOpenBoxContents(JbmlTracked.getParentTrackedObjects(document.getElementById(boxId)));
    // JP per Jeff E -- changed second argument from "Clicked Delete Box."
    sendJson({ type: "box-delete-click", boxid: boxId, mousecode: mouseCode }, "");
	showClientStatus('Deleting');
  }

  // Sends a 'box-clear-click' message containing a box ID and mouse
  // button to the server.
  function doBoxClearMouseClick (boxId, mouseCode) {
    doUpdateOpenBoxContents(JbmlTracked.getParentTrackedObjects(document.getElementById(boxId)));
    // JP per Jeff E -- changed second argument from "Clicked Clear Box."
    sendJson({ type: "box-clear-click", boxid: boxId, mousecode: mouseCode }, "");
  }

  // Sends a 'box-clear-delete-click' message containing a box ID and
  // mouse button to the server.
  function doBoxClearDeleteMouseClick (boxId, mouseCode) {
    doUpdateOpenBoxContents(JbmlTracked.getParentTrackedObjects(document.getElementById(boxId)));
    // JP per Jeff E -- changed second argument from "Clicked Clear + Delete Box."
    sendJson({ type: "box-clear-delete-click", boxid: boxId, mousecode: mouseCode }, "");
  }

  function doDndBox (sourceId, boxId, sourceBox) {
    doUpdateOpenBoxContents(JbmlTracked.getTrackedObjects(sourceBox, JbmlTracked.getParentTrackedObjects(document.getElementById(boxId))));
    // JP per Jeff E -- changed second argument to ""
    sendJson({ type: "dnd-box", sourceid: sourceId, boxid: boxId }, "");
/*
	     "Dropped " + Jbml.getBoxName(sourceBox) + " on " + Jbml.getBoxName(document.getElementById(boxId)));
*/
  }

  function doDndBoxAfter (sourceId, boxId, sourceBox) {
    doUpdateOpenBoxContents(JbmlTracked.getTrackedObjects(sourceBox, JbmlTracked.getParentTrackedObjects(document.getElementById(boxId))));
    // JP per Jeff E -- changed second argument to ""
    sendJson({ type: "dnd-box-after", sourceid: sourceId, boxid: boxId }, "");
/*
	     "Dropped " + Jbml.getBoxName(sourceBox) + " after " + Jbml.getBoxName(document.getElementById(boxId)));
*/
  }

  function doDndBoxBefore (sourceId, boxId, sourceBox) {
    doUpdateOpenBoxContents(JbmlTracked.getTrackedObjects(sourceBox, JbmlTracked.getParentTrackedObjects(document.getElementById(boxId))));
    // JP per Jeff E -- changed second argument to ""
    sendJson({ type: "dnd-box-before", sourceid: sourceId, boxid: boxId }, "");
/*
	     "Dropped " + Jbml.getBoxName(sourceBox) + " before " + Jbml.getBoxName(document.getElementById(boxId)));
*/
  }



//
//  HELP PANE HANDLER COMMANDS (OUTBOUND GLUE)   John Myers Oct '11
//

  // Sends 
  // button to the server.
  function doHelpPaneCloseMouseClick() 
  // This is handled locally, don't see any reason to bother the server about this.
  {
	//Clear the Help Pane
//	clearHelpPane ();

	// Hide the Help Pane
	hideHelpPane ();

    //sendJson({ type: "box-delete-click", boxid: boxId, mousecode: mouseCode }, "Help Pane Closed");
  }

  //
  // HANDLER METHODS FOR INCOMING DISPATCH
  //

  //
  // "change-client-settings" message code
  //
  function changeClientSettings (settings) {
    for (var setting in settings) {
      clientSettings.set(setting, settings[setting]);
    }
  }

  function makeMenus (menusJson) {
    for (var i = 0; i < menusJson.length; i++) {
      attachJbmlMenu(menusJson[i], false);
    }
  }

  function makeMenu (menuJson, replaceOldBoxnode) {
    attachJbmlMenu(menuJson, replaceOldBoxnode);
  }

  function attachJbmlMenu (menu, replaceMenu) {
    var menuId = menu.id
    var replaced = (replaceMenu && document.getElementById(menuId));
    var container = ((replaced && replaced.parentNode) || document.getElementById("palette"));
    var menu = expandJbmlMenu(menu);

    // All done.  Stick the resulting menu onto the palette area. ...do this last so it doesn't spaz?
    // Hang box off of super.  Are we REPLACING?
    if (replaced) {
      replacingChild(container, menu, replaced);
    } else {
      container.appendChild(menu);
    }
    
    // Keep JSlint happy
    var m = new Menu(menuId);
    m.quickinit();
  }

  function expandJbmlMenu (menu) {
    var menuId = menu.id;
    var title = menu.title;
    var menuClass = ("palette-menu menu menu-centered-text " + (menu.color || ""));
    var entries = Menu.makeContainer();

    expandJbmlToplevelSubmenu(menu, Menu.openContainer(entries), 0);

    var menuHeader = $('<a>')
      .attr('href', 'javascript:void(0)')
      .text(title)
      .addClass('menu-section-header')
      .addClass('menu-header')
      .get(0);

    return $('<div>')
      .attr('id', menuId)
      .addClass(menuClass)
      .append(menuHeader)
      .append(entries)
      .get(0);
  }

  function expandJbmlToplevelSubmenu (menuJson, menu, boxId, parentTitle) {
    var submenus = menuJson.submenus;
    var entries = menuJson.entries;

    boxId = (boxId || 0);
    parentTitle = (parentTitle || "");

    // Expand submenus
    for (var i = 0; i < submenus.length; i++) {
      (function (submenu) {
        createJbmlMenu(menu, submenu.title, submenu, boxId, parentTitle);
      }(submenus[i]));
    }

    // Expand entries
    for (var i = 0; i < entries.length; i++) {
      (function (entry) {
        createJbmlMenuEntry(menu, entry.title, entry.id, boxId, parentTitle);
      }(entries[i]));
    }
  }

  function expandJbmlBoxSubmenu (menuJson, menu, boxId, parentTitle) {

    var submenus = menuJson.submenus;
    var entries = menuJson.entries;
	var multientries = menuJson.multientries;

    boxId = (boxId || 0);
    parentTitle = (parentTitle || "");

    // Expand submenus
    if (submenus) {
      for (var i = 0; i < submenus.length; i++) {
        (function (submenu) {
          createJbmlMenu(menu, submenu.title, submenu, boxId, parentTitle);
        }(submenus[i]));
      }
    }

    // Expand entries
    if (entries) {
      for (var i = 0; i < entries.length; i++) {
        (function (entry) {
          if (boxId === 0) {
            createJbmlMenuEntry(menu, entry.title, entry.id, boxId, parentTitle);
          } else {
            createJbmlBoxmenuEntry(menu, entry.title, entry.id, boxId, parentTitle, false);
          }
        }(entries[i]));
      }
    }
	
    // Expand Multi-Selectable Entries
    if (multientries) {
      for (var i = 0; i < multientries.length; i++) {
        (function (multientry) {
          if (boxId === 0) {
            createJbmlMenuEntry(menu, multientry.title, multientry.id, boxId, parentTitle);
          } else {
            createJbmlBoxmenuEntry(menu, multientry.title, multientry.id, boxId, parentTitle, true);
          }
        }(multientries[i]));
      }
    }
  }


  function createJbmlMenuEntry (menu, title, menuId, parentTitle) {
  	var myClass = "mid-"+menuId;
    var entry = $('<a>')
      .addClass('menu-section-header')
      .addClass('menu-subheader')
      .attr('href', 'javascript:void(0)')
      .text(title)
      .click(makeMenuClickHandler(menuId, title))
	  .mouseleave(function(){
	  	if ($(this).children('div.toolTip').length) {
				$(this).children('div.toolTip').remove();
			}
	  })
	  .append(
		$('<img/>')
		.attr({
			width: '10',
			height: '12',
			alt: 'Menu ToolTip',
			src: 'images/qmark.gif'
		})
		.addClass(myClass)
		.mouseenter(function(){
			if (!$(this).parent('a.menu-subheader').children('div.toolTip').length) {
				// menu item offset relative to the document body
				var offset = $(this).parent('a.menu-subheader').offset();
				// width of the menu item
				var posRight = ($(this).parent('a.menu-subheader').width()+22);
				// float left or right depending on whether toolTip is larger than the body width or not...
				if( (offset.left + posRight + 300) > $('body').width() ) {
					var myPos = 'right: '+posRight+'px';
				} else {
					var myPos = 'left: '+posRight+'px';
				}
				// add the tooltip!
				$(this).parent('a.menu-subheader').append(
					$('<div/>')
					.attr({style: myPos})
					.addClass('toolTip')
					.text(toolTips[menuId])
					.click(function(){
						sendJson({
							type: "palettemenuitemhelp",
							id: menuId
						});
						return false;
					})
				);
			}
		})
		.click(function(){
			sendJson({
				type: "palettemenuitemhelp",
				id: menuId
			});
			return false;
		})
		.hide()
	  );
    menu.appendChild(entry.get(0));
  }

  function createJbmlBoxmenuEntry (menu, title, menuId, boxId, parentTitle, isMulti) {
	if (!isMulti) {
		var entry = $('<a>')
		.addClass('menu-section-header')
		.addClass('menu-subheader')
		.attr('href', 'javascript:void(0)')
		.text(title)
		.click(makeBoxmenuClickHandler(menuId, boxId, title));
	} else {
		var entry = $('<a>')
		.addClass('menu-section-header')
		.addClass('menu-subheader')	
		.addClass('multi')
		.attr('href', 'javascript:void(0)')
		.attr('rev', menuId+'-'+boxId)
		.text(title)
		.toggle(function(){$(this).addClass('selected')},function(){$(this).removeClass('selected')});
	}
    menu.appendChild(entry.get(0));
  }

  function createJbmlMenu (menu, title, menuJson, boxId, parentTitle) {
    var entries = Menu.makeContainer();
    expandJbmlToplevelSubmenu(menuJson, Menu.openContainer(entries), boxId, parentTitle + title + " >> ");

    var img = $('<img>')
      .attr('src', 'images/arrow1.gif')
      .width('10px')
      .height('12px')
      .attr('alt', ' ');

    var header = $('<a>')
      .addClass('menu-section-header')
      .addClass('menu-subheader')
      .addClass('menu-arrow')
      .attr('href', 'javascript:void(0)')
      .append(title)
      .append(img);

    menu.appendChild(header.get(0));
    menu.appendChild(entries);
  }

  function removeMenu (menuID) {
    var removed = document.getElementById(menuID);
    if (removed) {
      removingChild(removed.parentNode, removed);
    }
  }
  
  function applyToolTips () {
  	// if toolTips don't yet exist, try again in 2 seconds
  	if (toolTips == '') {
		window.setTimeout(function(){
			applyToolTips();
		}, 2000);
	// if toolTips exist, show relevant question marks
	} else {
		for (tt in toolTips) {
			var myClass = 'mid-' + tt;
			if (mid = $('.mid-' + tt)) {
				mid.show();
			}
		}
	}
  }

  function focusBox (boxID) {
    var focusNode = document.getElementById(boxID);
    if (focusNode) {
      scrollJbmlBoxIntoView(focusNode);
    }
  }

  function addWorkspaceBox (boxes) {
    var workspace = document.getElementById("workspace");
    attachJbmlBoxes(boxes, workspace, "0");
    updateToolboxes(); // TODO Use something better
  }

  function redrawBox (box) {
    var oldNode = document.getElementById(box.id);
    var parent = oldNode ? oldNode.parentNode : document.getElementById("workspace");
    // Note that deleting and then adding will shove it onto end, not what we want.
    subbox = attachJbmlBoxes(box, parent, "0", oldNode);
    scrollJbmlBoxIntoView(subbox); // Could be problem for results or palette??
  }

  function addResultsBox (boxes) {
    var results = document.getElementById("results");

    // Hack to clear out msg, if it's still hanging around.
    if (document.getElementById("results-message")) {
      clearResults();
    }
    attachJbmlBoxes(boxes, results, "0", false, true);
    updateToolboxes(); // TODO Use something better
  }

  function attachJbmlBoxes (jbml, parentBox, superboxId, replacedBox, prepend) {
    var box = Jbml.createBox(jbml.id);
    var container = Jbml.createBoxContainer(box, parentBox);

    if (replacedBox) {
      Jbml.replacingChild(parentBox, container, replacedBox);
    } else if (parentBox.hasChildNodes() && (prepend || jbml.type === "jbml-main-box-menu")) {
      // Push onto front of superbox, no matter what. Takes care of
      // Open Holes, which have already added an Input box at the
      // beginning. We want Main Menu to come before Input box.
      parentBox.insertBefore(container, parentBox.firstChild);
    } else {
      // Else, regular addition; append onto back of list.
      parentBox.appendChild(container);
    }
    return expandJbmlBoxes(box, jbml, superboxId);
  }

  function expandJbmlBoxes (box, jbml, superboxId) {
    var boxId = jbml.id;
    var boxTextJustify = "left";

    if (clientSettings.get("default-jbml-dnd-drag")) { Jbml.enableDrag(box); }
    if (clientSettings.get("default-jbml-dnd-drop")) { Jbml.enableDrop(box); }

    function attachIconImg (box, extraClass, width, height, src) {
      box.appendChild(
        $('<img>')
          .addClass('icon-jbml')
          .addClass(extraClass)
          .width(width)
          .height(height)
          .attr('src', src)
          .get(0));
    }

    function addToBox (child) {

      if (child.type !== "text") {
        processModifiers(box, child.modifiers);
      }

      switch (child.type) {
        // Special content
      case "jbml-cr":           box.appendChild($('<div>').addClass('cr-jbml').get(0)); break;
      case "jbml-dotdotdot":    box.appendChild($('<div>').addClass('a-jbml').addClass('jbml-dotdotdot').text('. . .').get(0)); break;
        
        // ad hoc icons 
      case "jbml-exec-icon":    attachIconImg(box, 'jbml-exec-icon', '22', '22', 'images/greenarrowright_button_22x22.gif'); break;
      case "jbml-go-icon":      attachIconImg(box, 'jbml-go-icon', '16', '16', 'images/whitearrowgreen_16x16.gif'); break;
      case "jbml-close-icon":   attachIconImg(box, 'jbml-close-icon', '16', '16', 'images/redX.jpg'); break;
      case "jbml-menu-icon":    attachIconImg(box, 'jbml-menu-icon', '16', '16', 'images/greenarrowdown_16x16.gif'); break;
      case "jbml-icon":         attachIconImg(box, 'jbml-icon', '22', '22', 'images/greenarrowright_button_22x22.gif'); break;
        
        // standard icons
      case "jbml-delete":       JbmlIconDelete.attachIcon(box); break;
      case "jbml-clear":        JbmlIconClear.attachIcon(box); break;
      case "jbml-clear-delete": JbmlIconClearDelete.attachIcon(box); break;

        // holes
      case "jbml-input-text":
        JbmlInputText.initHole(box);
        JbmlInputText.open(box, "");
        break;
        
      case "jbml-hole":
        JbmlHole.initHole(box);
        JbmlHole.close(box);
        break; // This one is the Potential Hole, waiting to be clicked.
        
      case "jbml-hole-opened":  
        (function () {
          var input;
          var valueChild = false;
          var otherChildren = [];
          JbmlHole.initHole(box);
          input = JbmlHole.open(box, ""); 

          // KLUDGE -- when a box that has been filled in already is
          // opened for edit the old value is a child. But we want to
          // pull it out and put it in the hole we just made. So we
          // do.
          for (var i = 0; i < child.children.length; i++) {
            if (child.children[i].type === 'text') {
              valueChild = child.children[i];
            } else {
              otherChildren.push(child.children[i]);
            }
          }
          child.children = otherChildren;
          
          input.value = valueChild ? valueChild.value : "";
          input.size = (input.value + " ").length;                                      // FIX ME
	  
	  // FIXME 2010-04-26 <peter@gigamonkeys.com> -- I can't quite
	  // figure out if this call is necessary. It seems that the
	  // call to JbmLHole.open() above is going to result in a
	  // call to trackObject already. Though the value argument is
	  // different. But I can't tell what child.value ought to be.
	  // Emperically it often seems to be undefined. Perhaps we
	  // should be passing input.value here?
          JbmlHole.trackObject(input, child.value);
        }());
        break;
        
      case "jbml-multiline-hole":
        JbmlMultilineHole.initHole(box);
        JbmlMultilineHole.close(box);
        break;
        
      case "jbml-multiline-hole-opened":
        JbmlMultilineHole.initHole(box);
        JbmlMultilineHole.open(box, child.children[0].value);
        child.children = child.children.slice(1);
        break;

      case "jbml-options-menu": // New green menu, appears at the end.
      case "jbml-options-menu2":
	  case "jbml-kf-multiselect-menu":
        Jbml.disableDrag(box);
        (function () {
          createJbmlOptionsMenu(box, child, superboxId);
          throw {}; // blow away the rest of the submenus, so we don't redisplay them.
        }());
        
      case "jbml-main-box-menu": // Appears in the upper left.
        (function () {
          Jbml.disableDrag(box);
          createJbmlMainBoxMenu(box, child, superboxId);
          throw {}; // blow away the rest of the submenus, so we don't redisplay them.
        }());
        
      case "text":
        (function () {
          var currentWord = $('<div>').addClass('a-jbml').addClass('boxtext-jbml').text(child.value).get(0);
          // I think this could be done with jQuery .css('textAlign', boxTextJustify) -- PBS
          currentWord.style.textAlign = boxTextJustify;
          processModifiers(currentWord, child.modifiers);
          box.appendChild(currentWord);
          if (! Jbml.isBoxNameSet(box)) {
            // FIXME 2010-02-11 <peter@gigamonkeys.com> -- seems
            // like we should probably get rid of this path by
            // making sure the server always sends an appropriate
            // JBML-NAME modifier.
            Jbml.setBoxName(box, child.value);
          }
        }());
        break;

      case "jbml-outdent":
        // NYI -- wasn't in XML version either.
        break;

      case "anonymous":
        // This goes with kludge in boxes-to-json.
        break;
        
      default:
        alert('Unexpected child type: ' + child.type + ' in child: ' + JSON.stringify(child));
        break;
      }

      addChildren(child.children);
    }

    // FIXME 2010-02-11 <peter@gigamonkeys.com> -- eventually should
    // probably remove the jbml- prefix.
    function processModifiers (box, modifiers) {
      if (modifiers) {
        for (var modifier in modifiers) {
          switch (modifier) {
          case "jbml-b":                box.style.fontWeight = "bold"; break;
          case "jbml-i":                box.style.fontStyle = "italic"; break;
          case "jbml-ul":               box.style.textDecoration = "underline"; break;
          case "jbml-courier":          box.style.fontFamily = "'Courier New', Courier, monospace"; break;
          case "jbml-left-justify":     boxTextJustify = box.style.textAlign = "left"; break;
          case "jbml-center-justify":   boxTextJustify = box.style.textAlign = "center"; break;
          case "jbml-right-justify":    boxTextJustify = box.style.textAlign = "right"; break;
          case "jbml-thick":            Jbml.setBorderWidth(box, "jbml-thick"); break;
          case "jbml-medium":           Jbml.setBorderWidth(box, "jbml-medium"); break;
          case "jbml-thin":             Jbml.setBorderWidth(box, "jbml-thin"); break;
          case "jbml-no-outline":       Jbml.setBorderStyle(box, "jbml-no-outline"); Jbml.disableDrag(box); break;
          case "jbml-dotted":           Jbml.setBorderStyle(box, "jbml-dotted"); break;
          case "jbml-dotted-blink":     dottedBlinkifyBox(box); break;
          case "jbml-button":           box.style.backgroundColor = "#880"; break;
          case "jbml-outdent":          /* FIXME! Unimplemented */ break;
          case "jbml-dnd-drag":         Jbml.enableDrag(box); break;
          case "jbml-dnd-no-drag":      Jbml.disableDrag(box); break;
          case "jbml-dnd-drop":         Jbml.enableDrop(box); break;
          case "jbml-dnd-no-drop":      Jbml.disableDrop(box); break;
          case "jbml-box-color":        Jbml.setBorderColor(box, modifiers[modifier]); break;
          case "jbml-color":            box.style.color = modifiers[modifier]; break;
          case "jbml-background-color": box.style.backgroundColor = modifiers[modifier]; break;
          case "jbml-name":             Jbml.setBoxName(box, modifiers[modifier]);
          default:                      alert("Unhandled modifier:" + modifier);
          }
        }
      }
    }

    function addChildren (children) {
      if (children) {
        for (var i = 0; i < children.length; i++) {
          if (children[i].id) {
            // If it has an ID it's a first-class box so we need to go
            // through the whole stack.
            attachJbmlBoxes(children[i], box, boxId);
          } else {
            // Otherwise (is this only for text children?) we just add
            // to the current box. Which necessitates the special
            // check in addToBox so we don't add a text node's
            // modifiers to the main box. This could be easier to
            // understand.
            addToBox(children[i]);
          }
        }
      }
    }
  
    // FIXME 2010-02-11 <peter@gigamonkeys.com> -- not sure if this
    // try/catch is actually necessary.

    // FIXME 2010-02-15 <peter@gigamonkeys.com> -- Seems like it is
    // but I haven't quite wrapped my head around why. Or maybe I'm
    // still confused.
    try {
      addToBox(jbml);
    } catch (e) {
    }
    return box;
  }

  function createJbmlOptionsMenu (box, menuJson, superboxId) {

    var title = menuJson.title;
    var menuEntries = menuJson.entries;
    var iconSrc = menuJson.iconSrc || "images/greenarrowright_box.gif";

    // Box is now the invisible box that surrounds the menu. Hang
    // everything off of this.
    box.style.borderStyle = "none"; // Make box invisible.
    box.style.background = "inherit";
    box.style.margin = "0px"; // Shrink-wrap box to fit.
    box.onclick = EventLibrary.stopPropagation;

    // JP 01/28/07 -- Stopped dblclick from causing internal error
    box.ondblclick = EventLibrary.stopPropagation;
    var menuId = ("ddmenu" + ++DDmenuID);

    // NOTE: DDMX menu classes will override other classes at the momement

    var menuIcon = $('<img>')
      .addClass('menu-section-header')
      .addClass('menu-header')
      .addClass('boxmenu')
      .attr('src', iconSrc)
      .attr('border', '0')
      .css({'top': '0px', 'border-style' : 'none', 'margin' : 'auto'})
      .get(0);

    var menuTitle = $('<div>')
      .addClass('menu-title-jbml')
      .css('clear', 'both')
      .text(title)
      .get(0);

    var entries = Menu.makeContainer();

    menuIcon.onclick = EventLibrary.stopPropagation;
    expandJbmlBoxSubmenu(menuJson, Menu.openContainer(entries), superboxId, '');

    var menu = $('<div>')
      .attr('id', menuId)
      .addClass('menu-menu-jbml')
      .addClass('menu')
      .addClass('boxmenu')
      .addClass('menu-centered-text')
      .append(menuIcon)
      .append(entries)
      .append(menuTitle)
      .get(0);
	  
	//console.log(entries);

    box.appendChild(menu); // Menublock goes inline filled in larger long greater box
    new Menu(menuId, true).quickinit();
	
	
	// multiselect menu additions (button top and bottom of menu to submit selections)
	// command for click: sendJson({ type: "boxmenumultiselect", opcode: menuId, boxid: boxId }, "");
	if (menuJson.multientries) {
		var menuOpts = $('#' + menuId + ' .menu-section-contents');
		var multiSelBtn = $('<button/>').text('Apply').click(function(){
			var menuOptsSel = $('#' + menuId + ' .menu-section-contents .selected');
			var opCode = new Array;
			if (!menuOptsSel.length) 
				alert('Please choose one or more options!');
			else {
				// boxId is unique for a given multi select set (e.g., only need to retrieve once)
				var boxId = menuOptsSel[0].rev.split("-")[1];
				// collect the menuId's
				for(i=0,l=menuOptsSel.length;i<l;i++) {
					opCode[i]=menuOptsSel[i].rev.split("-")[0];
				}
				// send to the server
				sendJson({ type: "boxmenumultiselect", opcode: opCode, boxid: boxId }, "");
				//console.log('fired!');
			}
		});
		menuOpts.prepend(multiSelBtn);
		menuOpts.append(multiSelBtn.clone(true));
	}
  }

  function createJbmlMainBoxMenu (box, menuJson, superboxId) {

    var title = menuJson.title;

    // BECAUSE MAIN-BOX-MENU IS A *SUBLIST*, IT ALREADY HAS ALLOCATED
    // AN ADDITIONAL BOX THAT'S BEEN HUNG OFF THE MAIN PARENT-BOX (NOW
    // OUT OF SCOPE), NOW CALLED "BOX". THEN THE MENU GOES INSIDE
    // THIS.

    // Box is now the invisible box that surrounds the menu. Hang
    // everything off of this.
    box.style.borderStyle = "none"; // Make box invisible.
    box.style.margin = "0px"; // Shrink-wrap box to fit.
    box.style.padding = "0px"; // Shrink-wrap box to fit.
    box.style.background = "inherit";
    box.onclick = EventLibrary.stopPropagation;
    box.ondblclick = EventLibrary.stopPropagation;

    // BOX NEEDS TO BE GIVEN A MYMENUID. BUT IT HAS AN ID ALREADY.
    // HUMH. it's thus necessary to make yet another sub div to hold
    // the menuID for the menu.
    var menuId = ("ddmenu" + ++DDmenuID);

    // NOTE: DDMX menu classes will override other classes at the momement

    var menuIcon = $('<img>')
      .addClass('menu-section-header')
      .addClass('menu-header')
      .addClass('boxmenu')
      .width('14')
      .height('11')
      .attr('src', 'images/greenarrowright_button_18x18.gif')
      .css({ 'margin' : '0px', 'margin-right' : '3pt', 'vertical-align' : 'bottom' })
      .get(0);

    var entries = Menu.makeContainer();

    menuIcon.onclick = EventLibrary.stopPropagation;
    expandJbmlBoxSubmenu(menuJson, Menu.openContainer(entries), superboxId);

    var menu = $('<div>')
      .attr('id', menuId)
      .addClass('menu-menu-jbml')
      .addClass('menu')
      .addClass('boxmenu')
      .append(menuIcon)
      .append(entries)
      .get(0);

    if (title) {
      menu.appendChild(
        $('<div>')
          .addClass('menu-title-jbml')
          .css('clear', 'left')
          .text(title)
          .get(0));
    }
    
    box.appendChild(menu);
	new Menu(menuId, true).quickinit();
  }

  function undottedBlinkifyBox (box) {
    stopAnimation(box);
    box.style.borderStyle = "";
    box.style.borderWidth = "";
  }

  function dottedBlinkifyBox (box) {
    stopAnimation(box);
    box.xAnimationState = 0;
    box.xAnimationTimer = setInterval(function () {
      var border = "solid";
      var nextState = (1 + box.xAnimationState) % 4;
      switch (box.xAnimationState) {
      case 0: // Solid
        break;
      case 1: // Dashed
        border = "dashed";
        break;
      case 2: // Dotted
        border = "dotted";
        break;
      case 3: // Dashed
        border = "dashed";
        break;
      default: // Solid
        nextState = 0;
        break;
      }
      box.xAnimationState = nextState;
      box.style.borderStyle = border;
      // 300 kinda frenetic, 400 brisk walk, 450 quiet walk, 500 too
      // stately. 500 tends to be hypnotic and put me to sleep. Very
      // quiet. Sucks too much attention.
    }, 450); // Msec.
  }

  function stopAnimation (box) {
    if (box.xAnimationTimer) {
      clearInterval(box.xAnimationTimer);
    }
    box.xAnimationTimer = null;
  }

  //
  // ROLLER ANIMATION
  //

  function updateToolbox (toolboxId, boxId, containerId) {
    var toolbox = document.getElementById(toolboxId);
    var box = document.getElementById(boxId);
    var container = document.getElementById(containerId);
    if (toolbox && box && container) {
      toolbox.style.right = ((box.clientWidth - container.clientWidth) + "px");
    }
  }

  function updateToolboxes () {
    /*updateToolbox("workspace-toolbox", "workspace-box", "workspace");
    updateToolbox("results-toolbox", "results-box", "results");*/
  }
  
  //
  // Sizing of workspace and results areas. Probably should all be
  // replaced with the jQuery splitter plugin.
  //
  
  function SplitState (workspacePercentage, resultsExpandable, resultsShadable, workspaceExpandable, workspaceShadable) {
    this.workspacePercentage    = workspacePercentage + "%";
    this.resultsPercentage      = (100 - workspacePercentage) + "%";
    this.resultsExpandable      = resultsExpandable;
    this.resultsShadable        = resultsShadable;
    this.workspaceExpandable    = workspaceExpandable;
    this.workspaceShadable      = workspaceShadable;
    this.resultsVisible         = workspacePercentage < 100;
    this.workspaceVisible       = workspacePercentage > 0;
  };

  SplitState.prototype.apply = function () {
    /*var workspaceStyle = document.getElementById("workspace-bigbox").style;
    var resultsStyle   = document.getElementById("results-bigbox").style;
    
    workspaceStyle.maxHeight = this.workspacePercentage;
    resultsStyle.top         = this.workspacePercentage;
    workspaceStyle.bottom    = this.resultsPercentage;
    resultsStyle.minHeight   = this.resultsPercentage;

    displayElement("workspace-bigbox", this.workspaceVisible);
    displayElement("results-bigbox", this.resultsVisible);
    displayElement("results-shade", this.resultsShadable);
    displayElement("results-expand", this.resultsExpandable);
    displayElement("workspace-shade", this.workspaceShadable);
    displayElement("workspace-expand", this.workspaceExpandable);

    splitStates.previous = splitStates.current;
    splitStates.current = this;

    updateToolboxes();*/
  }

  function makeShadeExpandHandler (what) {
    return function () {
      splitStates[splitStates.current[what]].apply();
    };
  }

  // Wire up the states with proper transitions.
  var splitStates = {
    smallResults:  new SplitState(85, 'mediumResults', false, 'fullWorkspace', false),
    mediumResults: new SplitState(70, 'fullResults', 'smallResults', 'fullWorkspace', false),
    fullResults:   new SplitState(0, false, 'mediumResults', false, false),
    fullWorkspace: new SplitState(100, false, false, false, 'previous'),
    current: null,
    previous: null
  };

  //
  // ERROR MODAL POP-UPS
  //

  function removePopupHandler (e) {
    document.getElementById("palette").removeChild(this);
  }

  function createDisplayErrorModalPopup (messageString) {
    $('#palette').append(
      $('<div>')
        .addClass('modal-popup')
        .addClass('modal-popup-error')
        .append($('<pre>').text(messageString))
        .append($('<br>'))
        .append(' (Click to get rid of window.)')
        .click(removePopupHandler));
  }

  function createDisplayDisplayModalPopup (messageString) {
    $('#palette').append(
      $('<div>')
        .addClass('modal-popup')
        .addClass('modal-popup-normal')
        .append($('<pre>').text(messageString))
        .append($('<br>'))
        .append(' (Click to get rid of window.)')
        .click(removePopupHandler));
  }

  // 
  // "show-status" message code
  //

  // Displays a message to a status node and the status bar with a given
  // prefix.
  function showStatus (text, statusId, prefix) {
    prefix = (prefix || "");
    var status = document.getElementById(statusId);

    if (status) {
      clearElement(statusId);
      if (text) {
        var textBlock = $('<p>')
          .addClass('workspace-status-message')
          .append(text)
          .get(0);
        
        var alertFullText = "alert('" + text.replace(/\'/g, "\\'").replace(/\"/g, '\\"') + "'); return false;";

        status.appendChild(textBlock);

        if (clientSettings.get("show-status-overflow-link") && 
            ((status.clientWidth < status.scrollWidth) ||
             (status.clientHeight < status.scrollHeight)))
        {
          textBlock.style.paddingRight = "2em";
          textBlock.appendChild($('<a>')
                                .addClass('workspace-status-ellipsis')
                                .attr('href', '#')
                                .attr('onclick', alertFullText)
                                .append('...')
                                .get(0));

        }
      }
    }
	
    if (text) {
      if (!(window.xDefaultStatus)) {
        window.xDefaultStatus = window.defaultStatus;
      }
      window.status = window.defaultStatus = (prefix + text);
    } else if (window.xDefaultStatus) {
      window.status = window.defaultStatus = window.xDefaultStatus;
    }
  }

  var clearClientStatus = (function () {
    var timer = false;

    function clearStatus () {
      showClientStatus("");
    }

    return function (reset) {
      // Clears the client status line after a timeout.
      if (timer) {
        clearTimeout(timer);
      }
      timer = (!(reset) && setTimeout(clearStatus, 3000));
    };
  }());

  // Displays a message to the client portion of the status line.
  function showClientStatus (text) {
    clearClientStatus(true);
//  showStatus(text, "workspace-status-client", "client: ");
    showStatus(text, "workspace-status-server", "client: ");  //JKM 9/11/12
  }

  // Displays a message to the server portion of the status line.
  function showServerStatus (text) {
    showStatus(text, "workspace-status-server", "server: ");
  }

  //
  // "show-dialog" message cod
  //

  // Creates a function to process completing a dialog and submitting
  // the input.
  function makeDialogSubmitProcessor (dialog, id, uniqueId) {
    return function (form, e) {
      // Prevent recursion (if used)
      dialog.onclose = noOp;
      dialog.close();

      // Must not fail, so protect against this case
      if (form) {
	// JP per Jeff E -- changed second arg from "Entered Popup Input."
	sendJson({ type: "user-popup-input", id: id, userinput: form[uniqueId].value }, "");
      }
      return EventLibrary.stopPropagation(e);
    };
  }

  function makeDialogFormHandler (process) {
    return function (e) {
      return process(this, e);
    };
  }

  // Creates an event handler for the submit button of the dialog's form.
  function makeDialogFormButtonHandler (process, buttonId) {
    return function (e) {
      var form = this;
      if (EventLibrary.getTarget(e).id === buttonId) {
        return process(form, e);
      } else {
        return EventLibrary.stopPropagation(e);
      }
    };
  }

  // Displays a Dialog using a given title, label and form element.
  function showDialog (id, title, label, dialogType, dialogTypeOptions) {
    var dialog = (Dialog.getDialogById(id) || new Dialog(id));

    // These must be globally unique, what ever they are
    var uniqueId = ("dialog-input1-" + id);
    var buttonId = ("dialog-button1-" + id);
    var process = makeDialogSubmitProcessor(dialog, id, uniqueId);

    // Also used as the action for the form element
    var formHandler = makeDialogFormHandler(process);
    var formElement;

    // Helper function
    function unpackSelectionList (select, dialogTypeOptions) {
      for (var i = 0, len = dialogTypeOptions.length; i < len; i++) {
        var option = dialogTypeOptions[i];
        var optionElement = $('<option>').attr('value', option.value || option.label).text(option.label);
      
        if (option.selected) {
          optionElement.attr('selected', 'selected');
        } else if (option.disabled) {
          optionElement.attr('disabled', 'disabled');
        }
        select.appendChild(optionElement.get(0));
      }
    }

    switch (dialogType.toLowerCase()) {
    case "single-select":
      formElement = $('<select>').attr('name', uniqueId).attr('id', uniqueId).get(0);
      unpackSelectionList(formElement, dialogTypeOptions);
      break;
    case "select":
      formElement = $('<select>')
        .attr('name', uniqueId)
        .attr('id', uniqueId)
        .attr('multiple', 'multiple')
        .attr('size', '6')
        .get(0);
      unpackSelectionList(formElement, dialogTypeOptions);
      // TODO: Use an option to specify the size
      if (formElement.length && (formElement.size > formElement.length)) {
        formElement.size = formElement.length;
      }
      break;
    case "1": // Obsolete, creates a textarea
    case "textarea":
      formElement = $('<textarea>')
        .attr("name", uniqueId)
        .attr("id", uniqueId)
        .attr("cols", "40")
        .attr("rows", "4")
        .get(0);
      break;
    case "0": // Obsolete, creates a single line input box
    case "input":
      formElement = $('<input>')
        .attr("type", "input")
        .attr("name", uniqueId)
        .attr("id", uniqueId)
        .attr("size", "40")
        .get(0);
      break;
    default:
      formElement = $('<div>')
        .append($('<input>')
                .attr("type", "hidden")
                .attr("name", uniqueId)
                .attr("id", uniqueId)
                .attr("value", "invalid-dialog-type:" + dialogType))
        .append($('<p>').text("Server sent invalid dialog type: " + dialogType))
        .get(0);
    }


    // This used to set the 'action' attribute to a function but that
    // doesn't seem like it makes any sense. And things seem to work
    // without it. --PBS
    var form = $('<form>')
      .attr("name", id)
      .css({ 'margin' : '0px',  'margin-top' : '5px' })
      .append($('<div>')
              .css({ 'text-align' : 'center', 'margin-bottom' : '5px' })
              .append($('<label>')
                      .attr("for", uniqueId)
                      .css('vertical-align', 'top')
                      .append(label))
              .append(' ')
              .append(formElement))
      .append($('<div>')
              .css('text-align', 'right')
              .append($('<input>')
                      .attr("type", "button")
                      .attr("id", buttonId)
                      .attr("value", "Submit")))
      .get(0);

    var contents = $('<div>')
      .css({ 'display' : 'table', 'height' : '100%', 'position' : 'relative', 'width' : '100%' })
      .append($('<div>')
              // This centers the content vertically
              .css({ 'display' : 'table-cell', 'vertical-align' : 'middle' })
              .append($('<div>').css('text-align', 'center').append(form)))
      .get(0);

    form.style.margin = "0px auto";
    form.onsubmit = formHandler;
    form.onclick = makeDialogFormButtonHandler(process, buttonId);

    dialog.setTitle(title);
    dialog.setContents(contents);
    dialog.show();

    contents.style.minWidth = DomLibrary.getStyleProperty(form, "width");
    contents.style.minHeight = DomLibrary.getStyleProperty(form, "height");

    // Once the dialog is shown, measure the contents
    dialog.sizeToContents();
    dialog.center();
  }

  //
  // BOX MENU AND CLOTHES-PINS
  //

  function makeChangeBackgroundColorHandler (color) {
    return function (e) {
      this.style.backgroundColor = color;
      return EventLibrary.stopPropagation(e);
    };
  }

  //
  // MENU STUFF
  //

  // This function handles taking care of the mouse click on a pulldown menu item inside a box.
  // Note this function is called with the MENU-ENTRY object, NOT the MENU,
  // but it must change the MENU around and reset it to the top.
  function makeMenuentryClickHandler (superboxId, title) {
    return function (e) {
      var themenuentry = this;
      var themenu = themenuentry.parentNode;
      // Turn the background light green temporarily.
      themenu.style.backgroundColor = "#e0ffe0";
      themenu.style.backgroundColor = "#00ff00";
      themenu.style.backgroundColor = "#e0ffe0";

      // Send the mouse click results down the pipe.
      doBoxmenuMouseClick(themenuentry.value, superboxId, title);
      // if you wanted to return the name for some reason, you could
      // stick it here as well.

      // Turn the background back to white.
      themenu.style.backgroundColor = "#ffffff";
      // Reset the element item to display the top element, the title.
      themenu.selectedIndex = 0;

      // let the system know you handled the event, don't try anything else.
      return EventLibrary.stopPropagation(e);
    };
  }

  // This function handles taking care of the mouse click on a popup boxmenu item attached to a box.
  // Note this function is called with the MENU and MENUENTRY objects.
  // It must change the MENUENTRY around and reset it to the top.
  // Menu's "boxid" attribute must be set to current box.
  // Menu only allowed to be attached to one box at a time.
  // MOSTLY OBSOLETE, WITH NEW INLINE MENU STYLES.
  function makeBoxmenuentryClickHandler (menu, title) {
    return function (e) {
      var menuEntry = this;
      var menuId = menu.boxid;

      // Turn the background light green temporarily.
      menuEntry.style.backgroundColor = "#e0ffe0";
      menuEntry.style.backgroundColor = "#00ff00";
      menuEntry.style.backgroundColor = "#e0ffe0";

      //  Send the mouse click results down the pipe.
      doBoxmenuMouseClick(menuEntry.id, menuId, title);

      //  Turn the background back to white.
      menuEntry.style.backgroundColor = "#ffffff";

      // Bury the menu, make it disappear.
      DomLibrary.setVisible(menu, false);

      // let the system know you handled the event, don't try anything else.
      return EventLibrary.stopPropagation(e);
    };
  }

  // This function handles taking care of the mouse click on a pulldown
  // menu item. BoxID is super-box-id, or 0 for palette menus.
// Used for main menus up top.
  function makeMenuClickHandler (menuId, title) {
    return function (e) {
      //  Send the mouse click results down the pipe.
      // JP per Jeff E -- removed second arg:
      // "Selected " + (title || "Palette Menu") + "."
      sendJson({ type: "palettemenumouseclick", id: menuId }, "");
      // if you wanted to return the name for some reason, you could stick it here as well.
    };
  }

  // This function handles taking care of the mouse click on a pulldown
  // menu item. BoxID is super-box-id, or 0 for palette menus.
  function makeBoxmenuClickHandler (menuId, boxId, title) {
    return function (e) {
      //  Send the mouse click results down the pipe.

      doBoxmenuMouseClick(menuId, boxId, title);
      // if you wanted to return the name for some reason, you could stick it here as well.

      // JKM: Oct '11 as per request by Jeff Elhai:
      if( title == "Execute" || title == "EXECUTE")   
      {
	//KickinWaitCursorAfterSecs(0);   // JKM test Oct '12
	showClientStatus('Submitted!');   
       }
//showClientStatus('This is an amazingly, unbelievably, totally, ridiculously and also extremely long message that tells you that your Execution Request has been Submitted!');
      // This line must go AFTER the previous doBoxmenuMouseClick,
      // as if it goes BEFORE somehow it gets cleared out so rapidly that you never see it.
      // On the other hand, this one never gets cleared out until the next mouse click on the screen,
      // but that's the standing behavior from the previous instance, so we'll duplicate it here as well.
      // Putting feedback afterwards also makes sense, as what happens if do-etc. crashes in the middle?
      // You'd want to know that it wasn't submitted.  So this is the right place for it.  JKMyers 

      // let the system know you handled the event, don't try anything else.
      // WHY WAS THIS COMMENTED OUT?
      return EventLibrary.stopPropagation(e);
    };
  }



  //
  // HELP PANE COMMANDS			John Myers, Oct '11.  Deprecated, obsolete.
  //


 function showHelpPane () {
    var element = document.getElementById( "help-pane" );
    if (element) {
      	element.style.visibility = "visible";
	}
  }

 function clearHelpPane () {
    clearElement("help-pane-contents");
  }

 function hideHelpPane () {
    var element = document.getElementById( "help-pane" );
    if (element) {
      	element.style.visibility = "hidden";
	}
  }



  //
  // WORKSPACE COMMANDS
  //

  function clearWorkspace () {
    clearJbmlElement("workspace");
    updateToolboxes();
  }

  function clearResults () {
    clearJbmlElement("results");
    updateToolboxes();
  }

  function clearPalette () {
    clearElement("palette");
    setupPalette();
  }


  //
  // HILIGHT COMMANDS
  //

  // Temporarily highlights the box for rollover effects.
  function hilightBoxID (boxId) {
    var box = document.getElementById(boxId);
    if (box) {
      box.style.borderColor = boxFlashColor;
    }
  }

  function unhilightBoxID (boxId) {
    var box = document.getElementById(boxId);
    if (box) {
      Jbml.resetBorderColor(box);
    }
  }

  // Highlights the box for Selection.
  function flashingHilightBoxID (boxId) {
    var box = document.getElementById(boxId);
    if (box) {
      DomLibrary.addClass(box, "hilight-jbml");
    }
  }

  function unflashingHilightBoxID (boxId) {
    var box = document.getElementById(boxId);
    if (box) {
      DomLibrary.removeClass(box, "hilight-jbml");
    }
  }


  //
  // SCROLLING
  //

  var getBrowserWindow = (function () {
    var ourWindows = {};
    var ourWindowCount = 0;
    var ourDialogCount = 0;
    var ourNonBlockedCount = 0;
    function testId (id) {
      return (id || isNumber(id));
    }

    ourWindows["0"] = window;

    function verifyWindow (windowId) {
      if (!(testId(windowId) && ourWindows[windowId])) {
        return false;
      } else if (ourWindows[windowId].closed) {
        ourWindows[windowId] = null;
        return null; // This is probably a Lispscript artifact.
      } else {
        return true;
      }
    }

    function cleanUpWindows () {
      for (var i in ourWindows) {
        var value = ourWindows[i];
        if (!(value) || value.closed) {
          // Window as closed elsewhere
          ourWindows[i] = null;
          delete(ourWindows[i]);
        }
      }
    }

    function makeClickedPopup (windowId, url, title, specs, setup) {
      ourDialogCount++;

      var dialog = new Dialog(("popup-clicked-" + ourDialogCount));
      var link = $('<a>').attr("HREF", url).append('here').get(0);

      link.onclick = function () {
        getBrowserWindow(windowId, url, title, specs, setup, true);
        dialog.close();
        return false;
      };

      var message = ((ourNonBlockedCount &&

                      $('<div>')
                      .addClass('blocked-popup-instructions')
                      .append($('<p>')
                              .append($('<em>').text("NOTE:"))
                              .append(" Your browser enforces a limit on VPL opened windows."))
                      .append($('<p>')
                              .append("You may want to increase this limit by modifying the ")
                              .append($('<code>').text("about:config"))
                              .append(" variable ")
                              .append($('<code>').text("dom.popup_maximum"))
                              .append("."))
                      .get(0)) ||
                     
                     $('<div>')
                     .addClass("blocked-popup-instructions")
                     .append($('<p>').append($('<em>').text("NOTE:")).append(" Your browser is using a popup blocker."))
                     .append($('<p>').append("You may want to unblock this site."))
                     .append($('<p>').append("You might have other VPL windows already open."))
                     .get(0));

      var contents = $('<div>')
        .addClass("blocked-popup-message")
        .append($('<p>')
                .append("The popup window for ")
                .append($('<span>').addClass("blocked-popup-title").text(title))
                .append(" was blocked."))
        .append($('<p>').append("Please click ").append(link).append(" to continue."))
        .append(message)
        .get(0);

      dialog.setTitle("Blocked Popup Window");
      dialog.setContents(contents);
      dialog.show();
      dialog.sizeToContents();
      return dialog.center();
    }

    return function (windowId, url, title, specs, setup, failedOpen) {
      if (verifyWindow(windowId) || (1 == arguments.length)) {
        return ourWindows[windowId];
      } else {
        var myWindow = null;
        cleanUpWindows();
        try {
          myWindow = window.open(url, title, specs);
        } catch (e) {
        }
        if (myWindow) {
          var myWindowClose = myWindow.close;
          ourWindowCount++;
          var myWindowIndex = ourWindowCount;
          var myWindowId = function () {
            if (testId(windowId)) {
              return windowId;
            } else {
              return myWindowIndex;
            }
          }.call(this);

          myWindow.getWindowId = function () {
            return myWindowId;
          };

          myWindow.getWindowIndex = function () {
            return myWindowIndex;
          };

          myWindow.close = function () {
            myWindowClose.call(myWindow);
            ourWindows[myWindowId] = null;
          };
          if (!(myWindow.opener)) {
            myWindow.opener = self;
          }
          if (setup) {
            setup(myWindow);
          }
          if (!(failedOpen)) {
            ++ourNonBlockedCount;
          }
          ourWindows[myWindowId] = myWindow;
        } else {
          makeClickedPopup(windowId, url, title, specs, setup);
          return null;
        }
      }
    };
  }());

  // The original code had (when relative-p ...), which was true for both
  // 1 and 0.  We changed it to (when (= relative-p 1) ...)
  // so now when we specify 0 on the server it pops up the window absolute to the screen.
  // (Note that relative-p is actually a string; the string "1" is equal to
  // the number 1 in javascript, likewise for 0, but while the number 0
  // is considered false, the string "0" is not considered false.

  // -- JP
  function setupPopupURLWindow (browserWindow, windowId, relativeP) {
    if (!(windowId)) {
      browserWindow.name += browserWindow.getWindowIndex();
    }
    if (relativeP == 1) {
      browserWindow.moveBy(window.screenX, window.screenY);
    }
    // JP 1/28/06 -- Added this to try to make sure that popup windows
    // come up visible instead of sometimes hidden. Don't know whether
    // this really works, but it doesn't seem to do any harm.
    browserWindow.focus();
  }

  function popupURLWindow (windowId, url, specs, relativeP) {
    var title = ("VPL Message Window: " + windowId);
    var setup = function (browserWindow) {
      setupPopupURLWindow(browserWindow, windowId, relativeP);
    };
    getBrowserWindow(windowId, url, title, specs, setup);
    return false;
  }

  function killWindowId (windowID) {
    var browserWindow = getBrowserWindow(windowID);
    if (browserWindow) {
      browserWindow.close();
    }
  }

  //
  // SCROLL ROUTINES
  //

  function scrollJbmlBoxIntoView (box) {
    while (!(DomLibrary.hasClass(box, "a-jbml"))) {
      box = box.parentNode;
    }
    var container = box.parentNode;
    while (!(DomLibrary.hasClass(container, "contains-jbml"))) {
      container = container.parentNode;
    }
    scrollBoxIntoView(box, container);
  }

  function scrollBoxIntoView (box, container) {
    container.scrollTop = computeScrollBox(box, container);
  }

  // Ensures the given box is as completely visible as possible. The
  // returned value will be clipped according to the browser
  function computeScrollBox (box, container) {

    if (container.scrollHeight > container.clientHeight) {
      // Container is scrolled
      var boxTop = (DomLibrary.computeAbsolutePosition(box).top - DomLibrary.computeAbsolutePosition(container).top);
      var scrollTop = container.scrollTop;
      var paddingTop = parseInt(DomLibrary.getStyleProperty(container, "paddingTop", "padding-top"), 10);
      var paddingBottom = parseInt(DomLibrary.getStyleProperty(container, "paddingBottom", "padding-bottom"), 10);
      if ((boxTop - paddingTop) < scrollTop) {
        return (boxTop - paddingTop);
      } else if ((boxTop + paddingBottom + box.clientHeight) > (scrollTop + container.clientHeight)) {
        return (boxTop - Math.min(paddingTop, (box.clientHeight + paddingBottom) - container.clientHeight));
      } else {
        return scrollTop;
      }
    } else {
      return 0;
    }
  }

  //
  // Utility functions
  //

  function displayElement (elementId, isShown) {
    var element = document.getElementById(elementId);
    if (element) {
      if (!(element.xInitialDisplay)) {
        element.xInitialDisplay = DomLibrary.getStyleProperty(element, "display");
      }
      DomLibrary.setVisible(element, isShown, element.xInitialDisplay);
    }
  }

  function clearElement (elementId) {
    var element = document.getElementById(elementId);
    if (element && element.childNodes) {
      while (element.childNodes.length) {
        removingChild(element, element.firstChild);
      }
    }
  }

  function clearJbmlElement (boxId) {
    var element = document.getElementById(boxId);
    if (element && element.childNodes) {
      while (element.childNodes.length) {
        Jbml.removingChild(element, element.firstChild);
      }
    }
  }

  function replacingChild (container, newNode, oldNode) {
    if (isVisibleMenuInside(oldNode)) {
      Menu.closeMenus();
    }
    container.replaceChild(newNode, oldNode);
  }

  function removingChild (container, child) {
    if (isVisibleMenuInside(child)) {
      Menu.closeMenus();
    }
    container.removeChild(child);
  }

  function isVisibleMenuInside (node) {
    var menuId = Menu.getVisibleId();
    var menuElement = (menuId && document.getElementById(menuId));
    while (menuElement && !((node === menuElement))) {
      menuElement = menuElement.parentNode;
    }
    return menuElement;
  }

  //
  // Base-library
  // 
  // Author: Johnny Casey
  // 
  // Some simple functions to capture common JavaScript tasks
  //

  function isNumber (a) {
    return ("number" === typeof(a));
  }

  function isString (a) {
    return ("string" === typeof(a));
  }

  function isFunction (a) {
    return ("function" === typeof(a));
  }

  function isObject (a) {
    return ((a && ("object" === typeof(a))) || isFunction(a));
  }

  function isArray (a) {
    return (isObject(a) && (a.constructor === Array));
  }

  function isUndefined (a) {
    return ("undefined" === typeof(a));
  }

  function findProperty (get, aliases, offset) {
    var retval = "";
    var i = (offset || 0);
    while ((i < aliases.length) && !(retval)) {
      retval = (get(aliases[i]) || retval);
      i = (i + 1);
    }
    return retval;
  }

  function getProperty (object, aliases) {
    if ((2 == arguments.length) && isArray(aliases)) {
      findProperty(function (property) { return object[property]; }, aliases);
    } else {
      findProperty(function (property) { return object[property]; }, arguments, 1);
    }
  }

  //
  // get-clone
  // 
  // Takes as parameter a JavaScript object to clone. The resulting
  // object is a deep copy of the original object. Part of Base-library
  //

  function getClone (object) {
    if ((null === object) || !(isObject(object)) || (window === object) || object.nodeType) {
      return object;
    } else {
      var copy = isArray(object) ? [] : {};
      for (var i in object) {
        var value = object[i];
        copy[i] = (function () {
          try {
            return getClone(value);
          } catch (e) {
            return false;
          }
        }()) || value;
      }
      return copy;
    }
  }

  //
  // Position
  // 
  // Author: Johnny Casey
  // 
  // A class to represent a position.
  //

  var Position = (function () {
    // Private static variables
    var ourDummy = {};
    var ourKeyCounter = 0;

    // Constructor
    // 
    // Takes an optional initial position.
    function positionConstructor (initialValue) {
      var mySelf = this;
      var myKey = ourKeyCounter += 1;
      var copy = (initialValue || ourDummy);
      mySelf.left = (copy.left || 0);
      mySelf.top = (copy.top || 0);
      mySelf.getKey = function () {
        return myKey;
      };
      copy = initialValue = null;
      return mySelf;
    }

    // Private static functions
    function add (object, dx, dy) {
      object.left += dx;
      object.top += dy;
      return object;
    }

    function sub (object, dx, dy) {
      object.left -= dx;
      object.top -= dy;
      return object;
    }

    // Public instance functions


    // Converts the position to a string.
    positionConstructor.prototype.toString = function () {
      return ("(" + this.left + "," + this.top + ")");
    };

    // Adds an offset to the position (both values optional).
    positionConstructor.prototype.add = function (dx, dy) {
      return add(this, dx || 0, dy || 0);
    };

    // Subtracts an offset from the position (both values optional).
    positionConstructor.prototype.sub = function (dx, dy) {
      return sub(this, dx || 0, dy || 0);
    };

    // Adds a position-like object to this position. prefix is an
    // optional string to prepend to "Left" and "Top". Otherwise uses
    // "left" and "top".
    positionConstructor.prototype.addPosition = function (other, prefix) {
      return add(this, other[((prefix && (prefix + "Left")) || "left")] || 0, other[((prefix && (prefix + "Top")) || "top")] || 0);
    };

    // Subtracts a position-like object from this position.  prefix is an
    // optional string to prepend to "Left" and "Top".
    // Otherwise uses "left" and "top".
    positionConstructor.prototype.subPosition = function (other, prefix) {
      return sub(this, other[((prefix && (prefix + "Left")) || "left")] || 0, other[((prefix && (prefix + "Top")) || "top")] || 0);
    };

    // Adds a size-like object to this position.  prefix is an optional
    // string to prepend to "Width" and "Height".
    // Otherwise uses "width" and "height".
    positionConstructor.prototype.addDimension = function (other, prefix) {
      return add(this, other[((prefix && (prefix + "Width")) || "width")] || 0, other[((prefix && (prefix + "Height")) || "height")] || 0);
    };

    // Subtracts a size-like object from this position.  prefix is an optional
    // string to prepend to "Width" and "Height".
    // Otherwise uses "width" and "height".
    positionConstructor.prototype.subDimension = function (other, prefix) {
      return sub(this, other[((prefix && (prefix + "Width")) || "width")] || 0, other[((prefix && (prefix + "Height")) || "height")] || 0);
    };

    // Compares two positions vertically then horizontally.
    positionConstructor.prototype.compare = function (other) {
      return ((this.top - other.top) || (this.left - other.left));
    };

    return positionConstructor;

  }());





  // Dom-library
  // 
  // Author: Johnny Casey
  // Dependencies: Base-library
  // 
  // Provides a cross platform interface for accessing and manipulating Document
  // Object Model information.
  // 
  // This class is a collection of static methods.

  var DomLibrary = (function () {
    // Private static variables
    var ourRootElement = null;
    var ourDummy = {};
    var ourWhitespace = ((window.RegExp && new RegExp((String.fromCharCode(92) + "s+"))) || " ");
    var ourClass = {};
    var ourLastStId = 0;

    var ourHackBrokenOverflowStackingContext = null;
    var ourHasGetComputedStyle = false;

      // Private static functions
      function getRootElement () {
        ourRootElement = ourRootElement || ((0 <= ("" + document.compatMode).indexOf("CSS")) && document.documentElement) || document.body;
        return ourRootElement;
      }

      function hasGetComputedStyle () {
      ourHasGetComputedStyle = ourHasGetComputedStyle || (document.defaultView && document.defaultView.getComputedStyle);
      return ourHasGetComputedStyle;
      }

      function isStackingContext (node) {
        if (null === ourHackBrokenOverflowStackingContext) {
        // Firefox before 3.0 brokenly assumed that overflowed
        // elements created stacking contexts
        ourHackBrokenOverflowStackingContext = (1.8 == geckoRevision());
        }
      return (!(("static" === ourClass.getStyleProperty(node, "position"))) && !(("auto" === ourClass.getStyleProperty(node, "zIndex", "z-index"))))
        || (ourHackBrokenOverflowStackingContext &&  (node.clientHeight < node.scrollHeight))
        || (node === ourClass.getRootElement());
      }

      function hasClass (node, className) {
        var words = ("" + node.className).split(ourWhitespace);
        for (var i = 0; (i < words.length); i++) {
          if (words[i] === className) {
            return true
          }
        }  //end for
        return false;
      }

      function showNode (node, display) {
        node.style.visibility = "visible";
        node.style.display = (display || "");
      }

      function hideNode (node) {
        node.style.visibility = "hidden";
        node.style.display = "none";
      }

      function addEnclosingOffsetBorders (node, position) {
        var top = 0;
        var left = 0;
        if (node) {
          while (node) {
            left += (parseInt(ourClass.getStyleProperty(node, "borderLeftWidth", "border-left-width"), 10) || 0);
            top += (parseInt(ourClass.getStyleProperty(node, "borderTopWidth", "border-top-width"), 10) || 0);
            node = node.offsetParent;
          }
        }
        if (position) {
          position.top += top;
          position.left += left;
        }
      }




    // Public static functions   (of DomLibrary)

    // Return the root element of the document
    ourClass.getRootElement = function () {
      return getRootElement();
    };

    // Retrieve the stacking context of a DOM node
    ourClass.getStackingContext = function (node) {
      var retval = null;
      while (node) {
        retval = node;
        node = (!(isStackingContext(node)) && node.parentNode);
      }
      return (retval || getRootElement());
    };

    // Lowers the stacking context of a DOM node, causing it to appear
    // behind other stacking contexts.
    ourClass.lowerStackingContext = function (node) {
      var root = getRootElement();
      var context = ourClass.getStackingContext(node);

      while (!(context === root)) {
        node = context;
        if (!("undefined" === typeof(node.xNormalZIndex))) {
          node.style.zIndex = node.xNormalZIndex;
        }
        context = ourClass.getStackingContext(node.parentNode);
      }
    };

    // Raises the stacking context of a DOM node, causing it to appear
    // above other stacking contexts.
    ourClass.raiseStackingContext = function (node) {
      var root = getRootElement();
      var context = ourClass.getStackingContext(node);
      while (!(context === root)) {
        node = context;
        context = ourClass.getStackingContext(node.parentNode);
        // Save initial z-index
        if ("undefined" === typeof(node.xNormalZIndex)) {
          node.xNormalZIndex = DomLibrary.getStyleProperty(node, "zIndex", "z-index");
          // Ensure that the node has an id
          if ("undefined" === typeof(node.id)) {
            ++ourLastStId;
            node.id = ("domlib-st-" + ourLastStId);
          }
          // Ensure that the context has a list of children
          if (!(isArray(context.xChildrenContexts))) {
            context.xChildrenContexts = [];
          }
          context.xChildrenContexts.push(node.id);
        }

        var i = 0;
        var siblingId = null;
        var sibling = null;
        var children = context.xChildrenContexts;

        while (i < children.length) {
          siblingId = children[i];
          sibling = document.getElementById(siblingId);
          if (siblingId && sibling) {
            var zIndex = DomLibrary.getStyleProperty(sibling, "zIndex", "z-index");
            if (2000 != zIndex) {
              sibling.xNormalZIndex = zIndex;
            } else {
              sibling.style.zIndex = sibling.xNormalZIndex;
            }
            ++i;
          } else {
            var last = children[(children.length - 1)];
            children[i] = last;
            --children.length;
          }
        }
        // Raise the node
        node.style.zIndex = 2000;
      }
    };

    // Retrieve the width of the browser display area.
    ourClass.getBrowserWidth = function () {
      return parseInt((self.innerWidth || (getRootElement() || ourDummy).clientWidth || 0), 10);
    };

    // Retrieve the height of the browser display area.
    ourClass.getBrowserHeight = function () {
      return parseInt((self.innerHeight || (getRootElement() || ourDummy).clientHeight || 0), 10);
    };

    // Retrieve the current horizontal scroll position.
    if (isNumber(window.pageXOffset)) {
      ourClass.getScrollLeft = function () {
        return window.pageXOffset;
      };
    } else {
      ourClass.getScrollLeft = function () {
        return ((getRootElement() || ourDummy).scrollLeft || 0);
      };
    }

    // Retrieve the current vertical scroll position.
    if (isNumber(window.pageYOffset)) {
      ourClass.getScrollTop = function () {
        return window.pageYOffset;
      };
    } else {
      ourClass.getScrollTop = function () {
        return ((getRootElement() || ourDummy).scrollTop || 0);
      };
    }

    // Computes the style of a DOM node.
    ourClass.computeStyle = function (node) {
      // TODO: Try to recursively build the style by looking at style sheets, ugh!
      return (node.currentStyle || (hasGetComputedStyle() && document.defaultView.getComputedStyle(node, null)) || node.style);
    };

    // Copy the computed style of a DOM node.
    ourClass.copyComputedStyle = function (node) {
      if (node.currentStyle) {
        return getClone(node.currentStyle);
      } else if (hasGetComputedStyle()) {
        var computedStyle = document.defaultView.getComputedStyle(node, null);
        var style = {};
        for (var i = 0; (i < computedStyle.length); i++) {
          var property = computedStyle.item(i);
          style[property] = computedStyle.getPropertyValue(property);
        }
        return style;
      } else {
        return getClone(node.style);
      }
    };

    // Retrieve a style from a node given a list of possible style
    // names.
    ourClass.getStyleProperty = function (node, aliases) {
      var args = arguments;
      var getter = (function () {
        if (node.currentStyle) {
          return function (property) {
            return node.currentStyle[property];
          }
        } else if (hasGetComputedStyle()) {
          var computedStyle = document.defaultView.getComputedStyle(node, null);
          return function (property) {
            return computedStyle.getPropertyValue(property);
          }
        } else {
          return function (property) {
            return node.style[property];
          }
        }
      }());

      if ((2 == args.length) && isArray(aliases)) {
        return findProperty(getter, aliases);
      } else {
        return findProperty(getter, args, 1);
      }
    };

    // Retrieve the text content of a DOM node. This includes the
    // concatenation of all the text nodes inside this DOM node.
    ourClass.getTextContent = function (node) {
      return (node.textContent || node.innerText || node.text || "");
    };

    // Determine if a DOM node has a particular class.
    ourClass.hasClass = function (node, className) {
      return hasClass(node, className);
    };

    // Adds a class to a DOM node.
    ourClass.addClass = function (node, className) {
      if (!(hasClass(node, className))) {
        node.className = (((node.className && (node.className + " ")) || "") + className);
      }
    };

    // Removes a class from a DOM node.
    ourClass.removeClass = function (node, className) {
      var words = ("" + node.className).split(ourWhitespace);
      var initialLength = words.length;
      var i = words.length;

      while (0 <= --i) {
        if (words[i] === className) {
          // Erase the removed entry with one from the end
          // (which has already been tested)
          words[i] = words[(words.length - 1)];
          // Reduce the length of the words *AFTER* replacing
          // the element...
          --words.length;
        }
      }
      if (words.length < initialLength) {
        node.className = (words.join(" ") || "");
      }
    };

    ourClass.show = showNode;
    ourClass.hide = hideNode;

    // Changes the visibility of a DOM node.
    ourClass.setVisible = function (node, isVisible, display) {
      if (isVisible) {
        showNode(node, display);
      } else {
        hideNode(node);
      }
    };

    // Change the alpha transparency of a DOM node.
    ourClass.setAlpha = function (node, alpha) {
      node.style.opacity = (alpha / 10);
      node.style.filter = ("alpha(opacity=" + (alpha * 10) + ")");
    };

    // Removes any IDs from a DOM node tree.
    ourClass.clearIds = function (node) {
      var stack = [];
      var child = null;
      var children = null;
      stack.push(node);
      while (stack.length) {
        child = stack.pop();
        child.id = "";
        children = child.childNodes;
        for (var i = 0; i < children.length; i++) {
          stack.push(children[i]);
        }
      }
      return node;
    };

    // Determine the absolute position of a DOM node.
    ourClass.computeAbsolutePosition = function (node) {
      var retval = new Position();
      addEnclosingOffsetBorders(node, retval);
      while (node) {
        // Firefox sometimes sets offset-top/left to negative
        // values which should be ignored.
        retval.top += Math.max(0, node.offsetTop);
        retval.left += Math.max(0, node.offsetLeft);
        node = node.offsetParent;
      }
      return retval;
    };

    // Determine the scrolled position of a DOM node.
    ourClass.computeScrolledPosition = function (node) {
      if (node.getBoundingClientRect) {
        return new Position(node.getBoundingClientRect()).addPosition(getRootElement(), "scroll");
      } else {
        var retval = ourClass.computeAbsolutePosition(node);
        node = node.parentNode;
        while (node) {
          retval.subPosition(node, "scroll");
          node = node.parentNode;
        }
        return retval;
      }
    };

    // Locate the nearest child in a container to a given position.
    ourClass.findNearestChild = function (container, x, y) {
      if (container.hasChildNodes()) {
        var children = container.childNodes;
        var position = ourClass.computeScrolledPosition(container);
        var retval = {};

        // The following values are offset by 1
        var candidateAbove = 0;
        var candidateBelow = 0;
        var candidateBefore = 0;
        var candidateAfter = 0;

        var candidateAboveMax = new Position(position).subPosition(container, "scroll");
        var candidateBelowMin = new Position(candidateAboveMax).addDimension(container, "scroll");
        var candidateBeforeMax = candidateAboveMax;
        var candidateAfterMin = candidateBelowMin;

        for (var i = 0; (i < children.length); i++) {
          position = ourClass.computeScrolledPosition(children[i]);
          if (y < position.top) {  // position is below point
            // Find the left-most minimum position
            if (0 < candidateBelowMin.compare(position)) {
              candidateBelow = (i + 1);
              candidateBelowMin = position;
            }
          } else if (y > (position.top + children[i].offsetHeight)) {  // position + height is above point
            // Find the right-most maximum position
            position.add(children[i].offsetWidth, 0);  // ignore the offset-height
            if (0 > candidateAboveMax.compare(position)) {
              candidateAbove = (i + 1);
              candidateAboveMax = position;
            }
          } else if (x < position.left) {  // position is after point
            if (0 < candidateAfterMin.compare(position)) {
              candidateAfter = (i + 1);
              candidateAfterMin = position;
            }
          } else if (x > (position.left + children[i].offsetWidth)) {  // position + width is before point
            position.add(children[i].offsetWidth, 0);  // ignore the offset-height
            if (0 > candidateBeforeMax.compare(position)) {
              candidateBefore = (i + 1);
              candidateBeforeMax = position;
            }
          } else {  // x,y is inside the child...  should not happen!
            // alert("Found point in a child, while trying to find nearest child!");
          }
        }
        if (0 == (candidateBefore + candidateAfter)) {
          // Only use the fallbacks when there are no candidates.
          candidateBefore = candidateAbove;
          candidateAfter = candidateBelow;
        }
        retval.before = (candidateBefore && children[(candidateBefore - 1)]);
        retval.after = (candidateAfter && children[(candidateAfter - 1)]);
        return retval;
      } else {
        return false;
      }
    };
    return ourClass;
  }());
// end of DomLibrary definition.





  // Position-cache
  // 
  // Author: Johnny Casey
  // Dependencies: Position, Dom-library
  // 
  // Calculates the position of objects and caches the absolute position for the
  // duration of this object.

  var PositionCache = (function () {

    // Private static variables
    var ourDummy = {};

    // Constructor
    function cacheConstructor () {
      var mySelf = this;
      mySelf.positions = {};
      return mySelf;
    }

    // Private static functions
    function getCache (cache) {
      return cache.positions;
    }

    function setKey (node, key) {
      node.xPositionKey = key;
      return key;
    }

    function getKey (node) {
      return ((node || ourDummy).xPositionKey || 0);
    }

    // Public instance functions

    // Computes the abosulte position of a DOM node.
    cacheConstructor.prototype.computeAbsolutePosition = function (node) {
      return (getCache(this)[getKey(node)] ||
              function () {
                var retval = new Position();
                var cache = getCache(this);
                var positionNode = node;
                while (node && !(cache[getKey(node)])) {
                  retval.add(parseInt(DomLibrary.getStyleProperty(node, "border-left-width", "borderLeftWidth"), 10) || 0,
                             parseInt(DomLibrary.getStyleProperty(node, "border-top-width", "borderTopWidth"), 10) || 0);
                  // Firefox sometimes sets offset-top/left to
                  // negative values which should be ignored.
                  retval.add(Math.max(0, node.offsetLeft), Math.max(0, node.offsetTop));
                  node = node.offsetParent;
                }
                if (node) {
                  retval.addPosition(cache[getKey(node)]);
                }
                return cache[setKey(positionNode, retval.getKey())] = retval;
              }.call(this));
    };

    // Compute the scrolled position of a DOM node.
    cacheConstructor.prototype.computeScrolledPosition = function (node) {
      return ((node.getBoundingClientRect &&
               new Position(node.getBoundingClientRect()).addPosition(DomLibrary.getRootElement(), "scroll")) ||
              function () {
                var retval = new Position(this.computeAbsolutePosition(node));
                var node = node.parentNode;
                while (node) {
                  retval.subPosition(node, "scroll");
                  node = node.parentNode;
                }
                return retval;
              }.call(this));
    };

    // Finds the child of a container nearest a point.
    cacheConstructor.prototype.findNearestChild = function (container, x, y) {
      return ((container.hasChildNodes() &&
               function () {
                 var children = container.childNodes;
                 var position = this.computeScrolledPosition(container);
                 var retval = {};

                 // The following values are offset by 1
                 var candidateAbove = 0;
                 var candidateBelow = 0;
                 var candidateBefore = 0;
                 var candidateAfter = 0;

                 var candidateAboveMax = new Position(position).subPosition(container, "scroll");
                 var candidateBelowMin = new Position(candidateAboveMax).addDimension(container, "scroll");
                 var candidateBeforeMax = candidateAboveMax;
                 var candidateAfterMin = candidateBelowMin;
                 for (var i = 0; (i < children.length); i++) {
                   position = this.computeScrolledPosition(children[i]);
                   if (y < position.top) {  // position is below point
                     // Find the left-most minimum position
                     if (0 < candidateBelowMin.compare(position)) {
                       candidateBelow = (i + 1);
                       candidateBelowMin = position;
                     }
                   } else if (y > (position.top + children[i].offsetHeight)) {  // position + height is above point
                     // Find the right-most maximum position
                     if (0 > candidateAboveMax.compare(position)) {
                       candidateAbove = (i + 1);
                       candidateAboveMax = position;
                     }
                   } else if (x < position.left) { // position is after point
                     if (0 < candidateAfterMin.compare(position)) {
                       candidateAfter = (i + 1);
                       candidateAfterMin = position;
                     }
                   } else if (x > (position.left + children[i].offsetWidth)) {  // position + width is before point
                     if (0 > candidateBeforeMax.compare(position)) {
                       candidateBefore = (i + 1);
                       candidateBeforeMax = position;
                     }
                   } else {  // x,y is inside the child...  should not happen!
                     // alert("Found point in a child, while trying to find nearest child!");
                   }
                 }
                 if (0 == (candidateBefore + candidateAfter)) {
                   // Only use the fallbacks when there are no candidates.
                   candidateBefore = candidateAbove;
                   candidateAfter = candidateBelow;
                 }
                 retval.before = (candidateBefore && children[(candidateBefore - 1)]);
                 retval.after = (candidateAfter && children[(candidateAfter - 1)]);
                 return retval;
               }.call(this)) || 
              false);
    };
    return cacheConstructor;
  }());
// end of PositionCache definition.




  // Event-library
  // 
  // Author: Johnny Casey
  // Dependencies: Base-library, Dom-library
  // 
  // Provides a cross platform interface for accessing and manipulating event
  // information.
  // 
  // This class is a collection of static methods.

  var EventLibrary = (function () {

    // Private static variables
    var ourUseEventPageXy = false;
    var ourUseEventClientXy = false;
    var ourMousedownSaneButton = false;
    var ourMousedownWeirdWhich = false;
    var ourClass = {};

    // Private static functions
    function getEvent (e) {
      return (e || window.event);
    }

    // Public static functions

    // Add Event Listeners
    if (document.addEventListener) {
      ourClass.addEventListener = function (node, e, listener, capturing) {
        node.addEventListener(e, listener, capturing);
      };

    } else if (document.attachEvent) {
      // TODO: Provide a means to access the registered element for IE
      ourClass.addEventListener = function (node, e, listener) {
        node.attachEvent(e, listener);
      };

    } else {
      ourClass.addEventListener = function (node, e, listener) {
        node[("on" + e)] = listener;
      };
    }

    // Remove Event Listeners
    if (document.removeEventListener) {
      ourClass.removeEventListener = function (node, e, listener, capturing) {
        node.removeEventListener(e, listener, capturing);
      };

    } else if (document.detachEvent) {
      // TODO: Provide a means to access the registered element for IE
      ourClass.removeEventListener = function (node, e, listener) {
        node.detachEvent(e, listener);
      };

    } else {
      ourClass.removeEventListener = function (node, e, listener) {
        node[("on" + e)] = null;
      };
    }

    // Wrap functions using this fixup to ensure that get-event is
    // called appropriately
    function fixupEvent (f) {
      return function (e) {
        return f(getEvent(e));
      }
    }

    ourClass.getEvent = function (e) {
      return getEvent(e);
    };

    ourClass.getTarget = fixupEvent(function (e) {
      var target = e.target || e.srcElement;
      // Avoid a safari (and early mozilla) bug
      return (target && (3 == target.nodeType) && target.parentNode) || target;
    });

    // Retrieve the event target.
    ourClass.getCurrentTarget = function (eventThis, e) {
      return ((!((window === eventThis)) && eventThis) || getEvent(e).currentTarget || null);
    };

    // Retrieve the keypress character code.
    ourClass.getKeypressCharCode = fixupEvent(function (e) {

      if ("keypress" === ("" + e.type)) {
        return e.keyCode || e.charCode || e.which;
      } else {
        return 0;
      }
    });

    // Mouse button constants
    ourClass.leftMouseButton = 1;
    ourClass.middleMouseButton = 2;
    ourClass.rightMouseButton = 4;

    // Retrieve the mouse button that was pushed.
    ourClass.getButton = fixupEvent(function (e) {
      var retval = null;
      if (ourMousedownSaneButton) {
        retval = e.button || 0;
      } else if (ourMousedownWeirdWhich) {
        // Converts 1 2 3 to 1 2 4
        retval = (1 << (e.which - 1)) || 0;
      } else {
        if (isNumber(e.button)) { // May be 0
          if (e.which) { // Always > 0
            if (e.button == e.which) {
              // Safari (but could also be weird Opera <8b, oh well)
              ourMousedownSaneButton = true;
            } else {
              // Firefox / Opera 8+
              ourMousedownWeirdWhich = true;
            }
          } else {
            // IE
            ourMousedownSaneButton = true;
          }
        } else if ("mousedown" === ("" + e.type)) {
          // N4
          ourMousedownWeirdWhich = true;
        } else {
          // unknown, probably wasn't a mouse event...
          retval = 0;
        }
        // Call ourself to get the actual value
        if (!(0 == retval)) {
          retval = ourClass.getButton(e);
        }
      }
      return retval;
    });

    // Mouse event position information
    ourClass.getPageX = fixupEvent(function (e) {
      // Returns the X coordinate of the event
      if (ourUseEventPageXy) {
        return e.pageX;
      } else if (ourUseEventClientXy) {
        return e.clientX + DomLibrary.getScrollLeft();
      } else if (isNumber(e.clientX)) {
        if (isNumber(e.pageX)) {
          ourUseEventPageXy = true;
        } else {
          ourUseEventClientXy = true;
        }
        return ourClass.getPageX(e);
      } else {
        return 0;
      }
    });

    ourClass.getPageY = fixupEvent(function (e) {
      // Returns the Y coordinate of the event
      if (ourUseEventPageXy) {
        return e.pageY;
      } else if (ourUseEventClientXy) {
        return e.clientY + DomLibrary.getScrollTop();
      } else if (isNumber(e.clientY)) {
        if (isNumber(e.pageY)) {
          ourUseEventPageXy = true;
        } else {
          ourUseEventClientXy = true;
        }
        return ourClass.getPageY(e);
      } else {
        return 0;
      }
    });

    // Retrieve the to-element of an event.
    ourClass.getToElement = fixupEvent(function (e) {
      return e.relatedTarget || e.toElement;
    });

    // Retrieve the from-element of an event.
    ourClass.getFromElement = fixupEvent(function (e) {
      return e.relatedTarget || e.fromElement;
    });

    // Determine if the mouse event indicates the mouse left an
    // element. This breaks the define-class-method macro, because it
    // doesn't properly return values
    ourClass.isMouseexit = function (e) {
      var target = ourClass.getTarget(e);
      var fromElement = ourClass.getFromElement(e);
      while (fromElement && !((fromElement === target))) {
        fromElement = fromElement.parentNode;
      }
      return !(fromElement);
    };

    // Event propagation
    ourClass.stopPropagation = fixupEvent(function (e) {
      e.returnValue = false;
      e.cancelBubble = true;
      if (e.stopPropagation) {
        e.stopPropagation();
      }
      return false;
    });

    ourClass.preventDefault = fixupEvent(function (e) {
      if (e.preventDefault) {
        e.preventDefault();
      }
    });

    return ourClass;

  }());
// end EventLibrary definition.




  // Drag code
  // 
  // Author: Johnny Casey
  // Dependencies: Base-library, Dom-library, Event-library
  // 
  // Provides a class to manipulate position by dragging a dom element.

  var Drag = (function () {
    // Private static variables
    var ourLastZIndex = 1000;

    // Private static functions
    function defaultDragXy (node, style, x, y) {
      // Basic drag function
      // JKM John Myers, Oct 11 '11:  Fix for whack-a-mole:
	//Drag was giving problems when right edge of ghost block
	//fell off right edge of screen, causing scrolling,
	//and somehow causing last block to get an extra CR and jump to next line left.
	//Looks like bumping edge inserts a vertical scroll bar in addition to a horiz one,
	//which chews up an extra 1/4" on the right, which forces stuff right on hairy edge
	//to jump to next line.
      // Solution:  When ghost dragging block hits right edge, it should...[choose one:]
	// a.  not move right, even when cursor moves right.  Still see whole block.
	// b.  Pretend to be occluded, move right with cursor.  Can't see whole block, off edge.
	// c.  Do something silly like compress horizontally or shrink
	// d.  Disappear.
	// We will try (a), it should work out.
      //What to do:
	// Monitor right side of window.
	// When ghost box hits right side of window, kick it sideways relative to cursor.
      var newX = x;
      var myWidth = parseInt(style.width );
      var windowWidth = parseInt( window.innerWidth );  //not sure this is necessary but good luck.
     if( x + myWidth >= windowWidth - 100 )  //Need 100, it can get weird otherwise.
     {	newX = windowWidth - 100 - myWidth;
     }
      node.style.left = (parseInt(style.left, 10) + newX + "px");   //base 10.  not needed. 
      node.style.top = (parseInt(style.top, 10) + y + "px");
    }

    // Constructor
    // 
    // Creates a drag object for a node (optional) with an update
    // function (a default is used if not provided).
    function dragConstructor (node, dragXy) {
      // Constructs a Drag object
      var mySelf = this;
      var myState = null;
      var myIsDragging = false;
      var myDragXy = (dragXy || defaultDragXy);

      // Private instance functions
      function go (e) {
        // Called during the mouse move event
        myState.dragXy(myState.node, myState.style, EventLibrary.getPageX(e) - myState.cursorStartX, EventLibrary.getPageY(e) - myState.cursorStartY);
        EventLibrary.preventDefault(e);
      }

      function stop (e) {
        // Called during the mouse up event
        myIsDragging = false;
        EventLibrary.removeEventListener(document, "mousemove", go, true);
        EventLibrary.removeEventListener(document, "mouseup", stop, true);
        if (mySelf.onStop) {
          mySelf.onStop.call(this, e);
        }
        // Allow on-stop to access getter functions of the drag operation
        myState = null;
      }

      // Public instance functions

      // Prepares dragging information
      mySelf.prep = function (eventThis, e, dragXy) {
        var e = EventLibrary.getEvent(e);
        myState = {};
        myState.node = (node || EventLibrary.getCurrentTarget(eventThis, e));
        myState.dragXy = (dragXy || myDragXy);
        myState.cursorStartX = EventLibrary.getPageX(e);
        myState.cursorStartY = EventLibrary.getPageY(e);
        myState.style = DomLibrary.copyComputedStyle(myState.node);
      };

      // Begins the drag by assigning listeners and raising the
      // z-index. This breaks the define-class-method macro, because
      // it tries to return the first statement
      mySelf.prepStart = function () {
        myState.node.style.zIndex = ourLastZIndex = (ourLastZIndex + 1);
        EventLibrary.addEventListener(document, "mousemove", go, true);
        EventLibrary.addEventListener(document, "mouseup", stop, true);
        myIsDragging = true;
      };

      // Event handler that begins dragging, takes an optional
      // callback. This breaks the define-class-method macro,
      // because it tries to return the first statement
      mySelf.start = function (eventThis, e, dragXy) {
        this.prep(eventThis, e, dragXy);
        this.prepStart();
        return EventLibrary.stopPropagation(e);
      };

      // Retrieve the dragged node.
      mySelf.getDragged = function () {
        return (myState && myState.node);
      };

      // Deterimine if the mouse is currently dragging /any/ node.
      mySelf.isDragging = function () {
        return myIsDragging;
      };

      return mySelf;
    }


    // Public static functions

    // Retrieve the last used z-index of a dragged object.

    /* Why isn't this dragContructor.prototype.getZIndex? - PBS */
    dragConstructor.getZIndex = function () {
      // Retrieves the last used z-index
      return ourLastZIndex;
    };

    return dragConstructor;

  }());
//end of Drag definition.




  // Scroll-bar
  // 
  // Author: Johnny Casey
  // Dependencies: Base-library, Dom-library, Event-library, Drag
  // 
  // Creates a scroll bar using HTML.

  var ScrollBar = (function () {

    // Private static variables
    var ourDrag = new Drag();
    var ourScrollBarId = 0;
    var ourDummyNode = {};
    var ourScrolling = {};
    var ourDefaultType = "generic";
    ourDummyNode.parentNode = false;

    // Private static accessor functions
    function setContentId (node, contentId) {
      node.xScrollContentId = contentId;
    }

    function getContentId (node) {
      return (node.xScrollContentId || 0);
    }

    function setHorizontal (node, isHorizontal) {
      node.xScrollHorizontal = isHorizontal;
    }

    function getHorizontal (node) {
      return (node.xScrollHorizontal || false);
    }

    function setType (node, type) {
      node.xScrollType = type;
    }

    function getType (node) {
      return (node.xScrollType || ourDefaultType);
    }

    function getBarId (scrollId) {
      return (scrollId + "-bar");
    }

    // Private static functions
    function copyScrollStyleProperties (node) {
      var style = {};
      style.left = DomLibrary.getStyleProperty(node, "left");
      style.top = DomLibrary.getStyleProperty(node, "top");
      return style;
    }

    function scrollVertical (node, style, x, y) {
      var content = document.getElementById(getContentId(node));
      var scroll = node.parentNode;
      if (content && scroll && (0 < content.clientHeight) && (content.clientHeight < content.scrollHeight)) {
        // Determine the current height of the scroll bar
        var visible = ((content.clientHeight / content.scrollHeight) * scroll.clientHeight);

        // Determine the max delta of the scroll bar
        var maxPos = (scroll.clientHeight - visible - 1);

        // Determine the top of the scroll bar
        var pos = Math.min(maxPos, Math.max(0, parseInt(style.top, 10) + y));
        var scrollPos = (((maxPos && (pos / maxPos)) || 0) * (content.scrollHeight - content.clientHeight));
        node.style.height = (visible + "px");
        node.style.top = (pos + "px");
        content.scrollTop = scrollPos;
      }
    }

    function scrollHorizontal (node, style, x, y) {
      var content = document.getElementById(getContentId(node));
      var scroll = node.parentNode;
      if (content && scroll && (0 < content.clientWidth) && (content.clientWidth < content.scrollWidth)) {
        var visible = ((content.clientWidth / content.scrollWidth) * scroll.clientWidth);
        var maxPos = (scroll.clientWidth - visible - 1);
        var pos = Math.min(maxPos, Math.max(0, parseInt(style.left, 10) + x));
        var scrollPos = (((maxPos && (pos / maxPos)) || 0) * (content.scrollWidth - content.clientWidth));
        node.style.width = (visible + "px");
        node.style.left = (pos + "px");
        content.scrollLeft = scrollPos;
      }
    }

    function scrollStop () {
      var bar = ourDrag.getDragged();
      if (bar) {
        ourScrolling[getType(bar)] = false;
        bar.style.zIndex = "";
      }
    }

    // Event Handlers
    function mousedownBarHandler (e) {
      var bar = EventLibrary.getCurrentTarget(this, e);
      ourScrolling[getType(bar)] = true;
      ourDrag.start(this, e, (getHorizontal(bar) && scrollHorizontal) || scrollVertical);
    }

    function mousedownScrollLessHandler (e) {
      var bar = document.getElementById(getContentId(EventLibrary.getCurrentTarget(this, e)));
      if (getHorizontal(bar)) {
        scrollHorizontal(bar, copyScrollStyleProperties(bar), 0 - (parseInt(DomLibrary.getStyleProperty(bar, "width"), 10) / 4), 0);
      } else {
        scrollVertical(bar, copyScrollStyleProperties(bar), 0, 0 - (parseInt(DomLibrary.getStyleProperty(bar, "height"), 10) / 4));
      }
      return EventLibrary.stopPropagation(e);
    }

    function mousedownScrollMoreHandler (e) {
      var bar = document.getElementById(getContentId(EventLibrary.getCurrentTarget(this, e)));
      if (getHorizontal(bar)) {
        scrollHorizontal(bar, copyScrollStyleProperties(bar), parseInt(DomLibrary.getStyleProperty(bar, "width"), 10) / 4, 0);
      } else {
        scrollVertical(bar, copyScrollStyleProperties(bar), 0, parseInt(DomLibrary.getStyleProperty(bar, "height"), 10) / 4);
      }
      return EventLibrary.stopPropagation(e);
    }

    function createScrollBar (scrollId, isHorizontal, type) {
      var barId = getBarId(scrollId);

      var bar = $('<div>')
        .attr("id", barId)
        .addClass('scroll-bar-button')
        .addClass('scroll-bar-bar')
        .append($('<div>').addClass('scroll-bar-cap').addClass('scroll-bar-begin'))
        .append($('<div>').addClass('scroll-bar-cap').addClass('scroll-bar-end'))
        .get(0);
      
      var scrollLess = $('<div>').addClass('scroll-bar-button').addClass('scroll-bar-less').get(0);
      var scrollMore = $('<div>').addClass('scroll-bar-button').addClass('scroll-bar-more').get(0);

      var scrollBar = $('<div>')
        .attr("id", scrollId)
        .addClass('scroll-bar')
        .append($('<div>').addClass('scroll-bar-track').append(bar))
        .append(scrollLess)
        .append(scrollMore)
        .get(0);

      setHorizontal(bar, isHorizontal);
      setType(bar, type);
      bar.onmousedown = mousedownBarHandler;

      // The buttons refer to the bar which references the content
      setContentId(scrollLess, barId);
      scrollLess.onmousedown = mousedownScrollLessHandler;
      setContentId(scrollMore, barId);
      scrollMore.onmousedown = mousedownScrollMoreHandler;
      scrollBar.className += ((isHorizontal && " scroll-horizontal") || " scroll-vertical");
      return scrollBar;
    }

    function updatePosition (scrollId, isHorizontal) {
      // Must come after we have attached the scroll-bar
      var bar = document.getElementById(getBarId(scrollId));
      if (bar) {
        if (isHorizontal) {
          scrollHorizontal(bar, copyScrollStyleProperties(bar), 0, 0);
        } else {
          scrollVertical(bar, copyScrollStyleProperties(bar), 0, 0);
        }
      }
    }

    function show (scrollId, contentId, isHorizontal) {
      var scrollBar = document.getElementById(scrollId);
      var content = (document.getElementById(contentId) || ourDummyNode);
      DomLibrary.setVisible(scrollBar, !(function () {
        if (isHorizontal) {
          return content.clientWidth === content.scrollWidth;
        } else {
          return content.clientHeight === content.scrollHeight;
        }
      }.call(this)));

      updatePosition(scrollId, isHorizontal);
    }

    function attach (scrollId, contentId, isHorizontal, type) {
      var scrollBar = (document.getElementById(scrollId) || createScrollBar(scrollId, isHorizontal, type));
      var content = (document.getElementById(contentId) || ourDummyNode);
      var container = content.parentNode;
      if (container) {
        if (!(scrollBar.parentNode)) { // Scroll-bar doesn't have a parent
          container.insertBefore(scrollBar, content);
        } else if (!((container === scrollBar.parentNode)) || !((content === scrollBar.nextSibling))) {
          scrollBar.parentNode.removeChild(scrollBar); // We don't want to kill menus if possible
          container.insertBefore(scrollBar, content);
        }
        // Must come after we have attached the scroll-bar
        setContentId(document.getElementById(getBarId(scrollId)), contentId);
      }
    }

    function detach (scrollId, contentId) {
      var scrollBar = document.getElementById(scrollId);
      var content = document.getElementById(contentId);
      if (scrollBar) {
        scrollBar.parentNode.removeChild(scrollBar);
      }
      if (content) {
        // FIXME Remove onmousescroll and DOMMouseScroll events
      }
    }

    // Constructor
    // 
    // Takes a flag to make the scrollbar horizontal and an optional
    // type of scrollbar.
    function scrollBarConstructor (isHorizontal, type) {
      var mySelf = this;
      var myScrollId = ("scroll-bar-" + ourScrollBarId);
      var myContentId = "";
      var myType = (type || ourDefaultType);
      ++ourScrollBarId;

      // Public instance functions

      // Attaches the scrollbar.
      mySelf.attach = function (contentId) {
        attach(myScrollId, myContentId = contentId, isHorizontal, myType);
      };

      // Shows the scrollbar.
      mySelf.show = function () {
        show(myScrollId, myContentId, isHorizontal);
      };

      // Detaches the scrollbar.
      mySelf.detach = function () {
        detach(myScrollId, myContentId);
      };

      return mySelf;
    }

    ourDrag.onStop = scrollStop;

    // Public static functions

    // Retrieve the fixed dimension of the scrollbar.
    scrollBarConstructor.getSize = function () {
      return 20;
    };

    // Determines if a particular type of scrollbar is scrolling.
    scrollBarConstructor.isScrolling = function (type) {
      return (ourScrolling[(type || ourDefaultType)] || false);
    };

    return scrollBarConstructor;

  }());
// end ScrollBar definition.


  // Dialog
  // 
  // Author: Johnny Casey
  // Dependencies: Dom-library, Drag
  // 
  // This class implements an in window dialog class.  The dialog can contain
  // arbitrary HTML and is dismissible.
  // 
  // The dialog is draggable using the Drag class.

  var Dialog = (function () {

    // Private static variables
    var ourDialogs = {};
    var ourBorders = {};

    // Border related private functions
    function makeMoveMin (edgeName, sizeName, minSizeNames) {
      return function (node, style, pos) {
        var size = (parseInt(style[sizeName], 10) - pos);
        var min = parseInt(getProperty(style, minSizeNames), 10);
        if (size < min) {
          pos -= (min - size);
          size = min;
        }
        node.style[edgeName] = (parseInt(style[edgeName], 10) + pos + "px");
        node.style[sizeName] = (size + "px");
      }
    }

    function makeMoveMax (edgeName, sizeName) {
      return function (node, style, pos) {
        node.style[edgeName] = (parseInt(style[edgeName], 10) + pos + "px");
        node.style[sizeName] = (parseInt(style[sizeName], 10) + pos + "px");
      }
    }

    function fixupHeights (f) {
      return function (node) {
        f.apply(this, arguments);
        node.xFixHeights();
      }
    }

    var moveTopEdge = fixupHeights(makeMoveMin("top", "height", [ "minHeight", "min-height" ]));

    var moveBottomEdge = fixupHeights(makeMoveMax("bottom", "height"));

    var moveLeftEdge = makeMoveMin("left", "width", [ "minWidth", "min-width" ]);

    var moveRightEdge = makeMoveMax("right", "width");

    // Borders and functions to drag with
    // we iterate over each of these when creating the borders.
    ourBorders.n = function (node, style, x, y) {
      moveTopEdge(node, style, y);
    };

    ourBorders.w = function (node, style, x, y) {
      moveLeftEdge(node, style, x);
    };

    ourBorders.s = function (node, style, x, y) {
      moveBottomEdge(node, style, y);
    };

    ourBorders.e = function (node, style, x, y) {
      moveRightEdge(node, style, x);
    };

    ourBorders.nw = function (node, style, x, y) {
      moveLeftEdge(node, style, x);
      moveTopEdge(node, style, y);
    };

    ourBorders.sw = function (node, style, x, y) {
      moveLeftEdge(node, style, x);
      moveBottomEdge(node, style, y);
    };

    ourBorders.se = function (node, style, x, y) {
      moveRightEdge(node, style, x);
      moveBottomEdge(node, style, y);
    };

    ourBorders.ne = function (node, style, x, y) {
      moveRightEdge(node, style, x);
      moveTopEdge(node, style, y);
    };

    // Private static functions
    function attachDialog (id, dialogElement, dialog) {
      DomLibrary.getRootElement().appendChild(dialogElement);
      ourDialogs[id] = dialog;
    }

    function removeDialog (id, dialogElement) {
      if (ourDialogs[id]) {
        ourDialogs[id] = null;
        DomLibrary.getRootElement().removeChild(dialogElement);
      }
    }

    function sizeBox (box, width, height, minWidth, minHeight) {
      var browserWidth = DomLibrary.getBrowserWidth();
      var browserHeight = DomLibrary.getBrowserHeight();
      var width = Math.min(width, (browserWidth));
      var height = Math.min(height, (browserHeight));
      var minWidth = Math.max(minWidth || 0, parseInt(box.style.minWidth, 10) || 0, 0);
      var minHeight = Math.max(minHeight || 0, parseInt(box.style.minHeight, 10) || 0, 0);
      box.style.width = (width + "px");
      box.style.height = (height + "px");
      box.style.minWidth = (Math.min(width, minWidth) + "px");
      return box.style.minHeight = (Math.min(height, minHeight) + "px");
    }

    function centerBox (box) {
      var browserWidth = DomLibrary.getBrowserWidth();
      var browserHeight = DomLibrary.getBrowserHeight();
      var left = ((browserWidth - box.clientWidth) / 2);
      var top = ((browserHeight - box.clientHeight) / 2);
      box.style.left = (left + "px");
      return box.style.top = (top + "px");
    }

    // Slightly more complicated elements
    function createButtons (dialog) {
      var close = $('<img>').attr("src", "images/mauveX16x16.gif").get(0);
      close.onclick = function (e) {dialog.close(e); };
      close.onmousedown = EventLibrary.stopPropagation;
      return $('<div>').addClass('dialog-buttons').append(close).get(0);
    }

    function makeHeaderMousedown (drag) {
      return function (e) {
        drag.start(this, e);
      }
    }


    //
    // Constructor
    // 

    // Takes the ID to use with this dialog.
    function dialogConstructor (dialogId) {
      var mySelf = this;
      // Header elements
      var myButtons = createButtons(mySelf);
      var myButtonsBalance = $('<div>').addClass('dialog-buttons-balance').append(' ').get(0);
      var myTitle = $('<div>').addClass('dialog-title').get(0);

      var myHeader = $('<div>')
        .addClass('dialog-header')
        .append(myButtons)
        .append(myButtonsBalance)
        .append(myTitle)
        .get(0);

      // Contents elements
      var myContents = $('<div>').addClass('dialog-contents').get(0);
      var myBody = $('<div>').addClass('dialog-body').append(myContents).get(0);

      // Main frame
      var myInterior = $('<div>').addClass('dialog-interior').append(myHeader).append(myBody).get(0);
      var myDialog = $('<div>').addClass('dialog').attr("id", dialogId).append(myInterior).get(0);

      var myDrag = new Drag(myDialog);

      var fixHeights = function () {
        var dialogWidth = parseInt(DomLibrary.getStyleProperty(myDialog, "width"), 10);
        var interiorWidth = parseInt(DomLibrary.getStyleProperty(myInterior, "width"), 10);
        var dialogHeight = parseInt(DomLibrary.getStyleProperty(myDialog, "height"), 10);
        var headerHeight = parseInt(DomLibrary.getStyleProperty(myHeader, "height"), 10);
        myInterior.style.height = ((dialogHeight - (dialogWidth - interiorWidth)) + "px");
        myBody.style.height = ((dialogHeight - (dialogWidth - interiorWidth) - headerHeight) + "px");
      };

      // Put the dialog on top
      myDialog.style.zIndex = Drag.getZIndex();

      // Make available for later
      myDialog.xFixHeights = fixHeights;

      // Allow the dialog header for dragging
      myHeader.onmousedown = makeHeaderMousedown(myDrag);

      // Attach the borders
      for (i in ourBorders) {
        var adjust = ourBorders[i];
        var className = ("dialog-border dialog-border-" + i);
        var border = $('<div>').addClass(className).get(0);

        border.onmousedown = function (e) {
          return myDrag.start(this, e, adjust);
        };
        
        myDialog.appendChild(border);
      }

      //
      // Public instance functions
      //

      // Set the title of the dialog.
      mySelf.setTitle = function (title) {
        myTitle.innerHTML = title;
      };

      // Set the contents of the dialog.
      mySelf.setContents = function (contents) {
        var contents = (contents || "");
        myContents.innerHTML = "";
        if (isString(contents)) {
          myContents.innerHTML = contents;
        } else {
          myContents.appendChild(contents);
        }
      };

      // Size the dialog to the contents with an optional width and
      // height.
      mySelf.sizeToContents = function (width, height) {
        myDialog.style.minWidth = "0px";
        myDialog.style.minHeight = "0px";
        myButtonsBalance.style.width = DomLibrary.getStyleProperty(myButtons, "width");
        myButtonsBalance.style.height = DomLibrary.getStyleProperty(myButtons, "height");

        var dialogWidth = parseInt(DomLibrary.getStyleProperty(myDialog, "width"), 10);
        var dialogHeight = parseInt(DomLibrary.getStyleProperty(myDialog, "height"), 10);
        var contentsWidth = parseInt(DomLibrary.getStyleProperty(myContents, "width"), 10);
        var contentsHeight = parseInt(DomLibrary.getStyleProperty(myContents, "height"), 10);
        var titleWidth = parseInt(DomLibrary.getStyleProperty(myTitle, "width"), 10);
        var headerHeight = parseInt(DomLibrary.getStyleProperty(myHeader, "height"), 10);
        var contentsEdgeHeight = (dialogHeight - (contentsHeight + headerHeight));

        // Set the width
        sizeBox(myDialog,
                Math.max(dialogWidth + (myContents.scrollWidth - myContents.clientWidth), (dialogWidth - contentsWidth) + (width || 0), dialogWidth + (myTitle.scrollWidth - myTitle.clientWidth), (dialogWidth - titleWidth) + (parseInt(myButtonsBalance.style.width, 10) * 5)),
                dialogHeight,
                // Minimum width is determined by the contents and the size of the buttons
                Math.max(dialogWidth + (myContents.scrollWidth - myContents.clientWidth), (dialogWidth - titleWidth) + (parseInt(myButtonsBalance.style.width, 10) * 5)));

        // Update values
        dialogWidth = parseInt(DomLibrary.getStyleProperty(myDialog, "width"), 10);
        contentsHeight = parseInt(DomLibrary.getStyleProperty(myContents, "height"), 10);
        headerHeight = parseInt(DomLibrary.getStyleProperty(myHeader, "height"), 10);

        // Set the height
        sizeBox(myDialog,
                dialogWidth,
                contentsEdgeHeight + headerHeight + (height || (contentsHeight + (myContents.scrollHeight - myContents.clientHeight))),
                0, // igored
                contentsEdgeHeight + headerHeight + contentsHeight + (myContents.scrollHeight - myContents.clientHeight));
        fixHeights();
      };

      // Centers the dialog in the browser window.
      mySelf.center = function () {
        centerBox(myDialog);
      };

      // Shows the dialog.
      mySelf.show = function () {
        attachDialog(dialogId, myDialog, mySelf);
      };

      // Closes the dialog.
      mySelf.close = function (e) {
        (mySelf.onclose && mySelf.onclose(e));
        removeDialog(dialogId, myDialog);
      };
      return mySelf;
    }

    //
    // Public static functions
    //

    /* Why not dialogConstructor.prototype.getDialogById = ? - PBS */
    dialogConstructor.getDialogById = function (id) {
      return ourDialogs[id];
    };

    return dialogConstructor;

  }());
// end Dialog declaration.




  // Menu
  // 
  // Author: Johnny Casey (based on DropDownMenuX)
  // Dependencies: Dom-library, Event-library
  // 
  // Nestable pop-up menus.
  var Menu = (function () {
    // Private static constants
    var ourDelayHide = 500; // Delay in Millisecs before closing a menu
    var ourDelayShow = 200; // Delay in Millisecs before displaying a menu

    var ourScrollbarWidth = ScrollBar.getSize();
    var ourBrowserPadding = 5; // Border of browser window the menu will avoid
    var ourRemainingOverlapMin = 40; // Minimum space to leave when overlapping another menu
    var ourSubLeftOffset = -5; // Distance submenus overlap previous menu, negative values work best
    var ourSubTopOffset = 5; // Distance submenus overlap previous menu, positive values work best

    var ourDummyNode = {}; // Used by get-node-header and get-node-section

    // Private static data
    var ourSections = {}; // Guard values for each section.  Reset when a new menu is created.
    var ourSectionsHideGuard = 0; // Guard value for hiding all sections
    var ourHeaderShownGuard = 0; // Guard value for showing headers
    var ourVisible = []; // List of visible sections
    var ourTimers = []; // List of currently counting down timers
    var ourScrollBars = []; // List of unused scrollbars

    ourDummyNode.className = "";
    ourDummyNode.offsetLeft = 0;
    ourDummyNode.offsetWidth = 0;
    ourDummyNode.offsetTop = 0;
    ourDummyNode.offsetHeight = 0;
    ourDummyNode.style = {};
    ourDummyNode.hasChildNodes = function () {
      return false;
    };

    // Private static id functions
    function getNodeId (node) {
      return (node.xMenuId + node.xHeaderId);
    }

    function getNodeSectionId (node) {
      return (node.xMenuId + node.xHeaderId + "-section");
    }

    function getNodeContentsId (node) {
      return (node.xMenuId + node.xHeaderId + "-contents");
    }

    // Private static functions

    //
    // Node relationship functions
    //

    // Gets the header for the given header/section
    function getNodeHeader (node) {
      if (node) {
        var id = getNodeId(node);
        if (id === node.id) {
          return node;
        } else if (node.previousSibling && (id === node.previousSibling.id)) {
          return node.previousSibling;
        } else {
          return (document.getElementById(id) || ourDummyNode);
        }
      } else {
        return ourDummyNode;
      }
    }

    // Gets the section for the given header/section
    function getNodeSection (node) {
      if (node) {
        var id = getNodeSectionId(node);
        if (id === node.id) {
          return node;
        } else if (node.nextSibling && (id === node.nextSibling.id)) {
          return node.nextSibling;
        } else {
          return (document.getElementById(id) || ourDummyNode);
        }
      } else {
        return ourDummyNode;
      }
    }

    // Gets the container for the given header/section
    function getNodeContainer (node) {
      if (node) {
        var levels = node.xHeaderId.split("-");
        levels.pop();
        var id = (node.xMenuId + levels.join("-") + "-section");
        if (node.parentNode && (id === node.parentNode.id)) {
          return node.parentNode;
        } else {
          return (document.getElementById(id) || ourDummyNode);
        }
      } else {
        return ourDummyNode;
      }
    }

    // Gets the scrollbar attached to a node, finds an unused one or
    // allocates a new one.
    function getScrollBar (node) {
      return node.xScrollBar = (node.xScrollBar || ourScrollBars.pop() || new ScrollBar(false, "menu-scroll-bar"));
    }

    // Retrieve the ids of containing sections
    function getParentSections (node) {
      var parents = {};
      if (node && node.xHeaderId) {
        var levels = node.xHeaderId.split("-");
        var id = node.xMenuId;
        levels.shift();
        levels.pop();
        for (var i = 0; (i < levels.length); i++) {
          id += ("-" + levels[i]);
          parents[(id + "-section")] = true;
        }
      }
      return parents;
    }

    //
    // parse-menu functions
    //

    function setupParsedNode (handlers, menuId, baseId, node, headerId, type) {
      node.xMenuId = menuId;
      node.xHeaderId = baseId;
      node.xItemType = type;
      node.id = (menuId + headerId);
      node.onmouseover = handlers[(type + "Mouseover")];
      node.onmouseout = handlers[(type + "Mouseout")];

      if (handlers[type + "Hide"]) {
        node.xOnhide = handlers[(type + "Hide")];
      }

      if (handlers[type + "Show"]) {
        node.xOnshow = handlers[(type + "Show")];
      }
    }

    function newGuard () {
      var retval = {};
      retval.show = 0;
      retval.hide = 0;
      return retval;
    }

    function copyMenuStyleProperties (node) {
      var style = {};
      style.borderTopWidth = (parseInt(DomLibrary.getStyleProperty(node, "borderTopWidth", "border-top-width"), 10) || 0);
      style.borderBottomWidth = (parseInt(DomLibrary.getStyleProperty(node, "borderBottomWidth", "border-bottom-width"), 10) || 0);
      style.borderLeftWidth = (parseInt(DomLibrary.getStyleProperty(node, "borderLeftWidth", "border-left-width"), 10) || 0);
      return style;
    }

    function resetSectionNode (section) {
      section.style.display = "";

      // Reset modified style values
      section.style.width = "";
      section.style.height = "";
      if (section.xScrollBar) {
        var scrollBar = section.xScrollBar;
        section.xScrollBar = false;
        scrollBar.detach();
        ourScrollBars.push(scrollBar);
      }
    }

    function hideSectionNode (section) {
      DomLibrary.setVisible(section, false);
      if (section.xOnhide) {
        section.xOnhide();
      }
    }

    function showSectionNode (section) {
      DomLibrary.setVisible(section, true);
      if (section.xScrollBar) {
        section.xScrollBar.show();
      }

      if (section.xOnshow) {
        section.xOnshow();
      }
    }

    //
    // Mouse handler functions
    //

    function clearTimers (timers) {
      while (timers.length) {
        clearTimeout(timers.pop());
      }
    }

    function hashPosition (node) {
      return (DomLibrary.computeScrolledPosition(node).toString() + ";(" + DomLibrary.getBrowserWidth() + "," + DomLibrary.getBrowserHeight() + ")");
    }

    function makeNodeActive (node) {
      DomLibrary.addClass(getNodeHeader(node), "menu-active");
    }

    function makeNodeInactive (node) {
      DomLibrary.removeClass(getNodeHeader(node), "menu-active");
    }

    // Hide sections
    function hideSection (id, section) {
      var section = (section || document.getElementById(id));
      if (section) {
        var visibleId = ourVisible.pop();
        makeNodeInactive(section);
        hideSectionNode(section);
        // TODO: IE <select> bug fix: hide iframe
        if (id === visibleId) {
          // Prevent other calls from trying to hide the section
          ++ourSections[id].hide;
        } else {
          // Put it back on the stack.
          ourVisible.push(visibleId);
        }
        return true;
      } else {
        // section no longer part of the document
        return false;
      }
    }

    function hideSectionGuarded (id, section, guard, guardAll) {
      if (guard === ourSections[id].hide) {
        if ((guardAll === ourSectionsHideGuard) && (id === ourVisible[(ourVisible.length - 1)])) {
          while (ourVisible.length) {
            if (!(hideSection(ourVisible[ourVisible.length - 1]))) {
              ourVisible.pop();
            }
          }
        } else {
          return hideSection(id, section);
        }
      }
    }

    // Returns a callback that calls hide-section-guarded saves the
    // current value of our-sections[ id ].hide for comparison
    function hideSectionCallback (id, node) {
      var guard = ourSections[id].hide;
      var guardAll = ourSectionsHideGuard;
      var section = (node && getNodeSection(node));
      return function () {
        hideSectionGuarded(id, section, guard, guardAll);
      };
    }

    // Hide visible sections which aren't parents of this node
    function hideNonAncestorSections (node) {
      var i = ourVisible.length;
      if (i) {
        var parents = getParentSections(node);
        while ((0 < i) && !(parents[ourVisible[--i]])) {
          hideSection(ourVisible[i]);
        }
      }
    }

    // Show sections
    function showSectionGuarded (id, section, guard) {
      if (guard === ourSections[id].show) {
        var section = (section || document.getElementById(id));
        // Prevent current show-section calls from running
        ++ourSections[id].show;
        hideNonAncestorSections(section);
        showSectionNode(section);
        // TODO: IE <select> bug fix: show iframe
        ourVisible.push(id);
      }
    }

    // Returns a callback that calls show-section-guarded saves the
    // current value of our-sections[ id ].show for comparison
    function showSectionCallback (id, node) {
      var guard = ourSections[id].show;
      var section = (node && getNodeSection(node));
      return function () {
        showSectionGuarded(id, section, guard);
      };
    }

    // Show headers
    function showHeaderGuarded (id, node, guard) {
      if (guard === ourHeaderShownGuard) {
        ++ourHeaderShownGuard;
        hideNonAncestorSections(node);
      }
    }

    // Returns a callback that calls show-section-guarded saves the
    // current value of our-header-shown-guard for comparison
    function showHeaderCallback (id, node) {
      var guard = ourHeaderShownGuard;
      return function () {
        showHeaderGuarded(id, node, guard);
      };
    }

    // Common mouse handlers
    function headerMouseoverHandler (e) {
      if (!(ScrollBar.isScrolling("menu-scroll-bar"))) {
        var id = (this.id + "-section");
        // Prevent any previous calls to show-header
        ++ourHeaderShownGuard;
        if (ourVisible.length) {
          makeNodeInactive(document.getElementById(ourVisible[ourVisible.length - 1]));
        }
        if (ourSections[id]) {
          clearTimers(ourTimers);
          // Prevent the section from being hidden
          ++ourSections[id].hide;
          // Prevent hidding other sections too
          ++ourSectionsHideGuard;
          ourTimers.push(setTimeout(showSectionCallback(id, this), ourDelayShow));
        } else if (ourVisible.length) {
          clearTimers(ourTimers);
          ourTimers.push(setTimeout(showHeaderCallback(this.id, this), ourDelayShow));
        }
      }
    }

    function headerMouseoutHandler (e) {
      if (!(ScrollBar.isScrolling("menu-scroll-bar")) && EventLibrary.isMouseexit(e)) {
        var id = (this.id + "-section");
        // Prevent headers from being shown
        ++ourHeaderShownGuard;
        if (ourSections[id]) {
          // Prevent the section from being shown
          ++ourSections[id].show;
          // See if it is visible
          for (var i = 0; (i < ourVisible.length); i++) {
            if (id === ourVisible[i]) {
              ourTimers.push(setTimeout(hideSectionCallback(id, this), ourDelayHide));
              i = ourVisible.length;
            }
          }
        }
      }
    }

    function sectionMouseoverHandler (e) {
      if (!(ScrollBar.isScrolling("menu-scroll-bar"))) {
        var id = this.id;
        // Prevent the section from being hidden
        ++ourSections[id].hide;
        // Prevent hidding other sections too
        ++ourSectionsHideGuard;
        makeNodeActive(this);
        return EventLibrary.stopPropagation(e);
      } /* Should this return true otherwise? -PBS */
    }

    function sectionMouseoutHandler (e) {
      if (!(ScrollBar.isScrolling("menu-scroll-bar")) && EventLibrary.isMouseexit(e)) {
        var id = this.id;
        // Prevent the section from being shown
        ++ourSections[id].show;
        ourTimers.push(setTimeout(hideSectionCallback(id, this), ourDelayHide));
        return EventLibrary.stopPropagation(e);
      } /* Should this return true otherwise? -PBS */
    }

    // Recursive functions...
    function parseMenu (menuId, handlers, container, id, tree) {
      var id = (id || "");
      var tree = (tree || []);
      var nodeStack = [];
      nodeStack.push(container.firstChild);

      while (nodeStack.length) {
        var node = nodeStack.pop();
        var headerId = null;
        var headerType = "subheader";
        var headerTree = null;
        var isSection = false;
        while (node) {
          if (1 == node.nodeType) {
            isSection = false;
            if (DomLibrary.hasClass(node, "menu-header")) {
              headerId = (id + "-" + tree.length);
              headerType = "header";
              tree.push(headerTree = []);
              setupParsedNode(handlers, menuId, headerId, node, headerId, "header");
            } else if (DomLibrary.hasClass(node, "menu-subheader")) {
              headerId = (id + "-" + tree.length);
              headerType = "subheader";
              tree.push(headerTree = []);
              setupParsedNode(handlers, menuId, headerId, node, headerId, "subheader");
            } else if (DomLibrary.hasClass(node, "menu-section")) {
              // NOTE: Sections *follow* the headers they are associated with
              hideSectionNode(node);
              isSection = true;

              // Must have an header-id
              if (headerId) {
                setupParsedNode(handlers, menuId, headerId, node, headerId + "-section", headerType + "Section");
                ourSections[node.id] = newGuard();

                // TODO: Apply an <SELECT> IE bug fix

                // Ensure that the menu has at least one element
                if (!(menuConstructor.openContainer(node).hasChildNodes())) {
                  menuConstructor.openContainer(node).appendChild(
                    $('<span>')
                      .addClass('menu-empty')
                      .addClass('menu-section-header')
                      .addClass('menu-subheader')
                      .append("(empty)")
                      .get(0));
                }
                // Tag the container as having a submenu
                getNodeContainer(node).xHasSubSections = true;

                // Visit the submenu
                parseMenu(menuId, handlers, node, headerId, headerTree);
              }
            } else if (DomLibrary.hasClass(node, "menu-section-contents")) {
              // Only present as a direct child of a
              // "menu-section" when using the Scroll-bar
              // class. Will automatically descend into and
              // parse the menu entries.
              node.id = getNodeContentsId(node.parentNode);
            }
            if (!(isSection) && node.hasChildNodes()) {
              nodeStack.push(node.firstChild);
            }
          }
          node = node.nextSibling;
        }
      }
    }

    // Layout the top level menu. The "section" is below an
    // "menu-header". Current behaviour is specific to FireFox's box
    // model.
    function layoutTopSection (node, useFixedPosition) {
      if (useFixedPosition) {
        node.style.position = "fixed";
      }

      // Root of a menu, place it above or below the header (menu-header).
      var browserHeight = (DomLibrary.getBrowserHeight() - (ourBrowserPadding * 2));
      var browserWidth = (DomLibrary.getBrowserWidth() - (ourBrowserPadding * 2));
      var header = getNodeHeader(node);
      var style = copyMenuStyleProperties(node);
      resetSectionNode(node);
      var headerPosition = DomLibrary.computeScrolledPosition(header);
      var headerWidth = header.offsetWidth;
      var headerHeight = header.offsetHeight;
      var nodeWidth = node.offsetWidth;
      var nodeHeight = node.scrollHeight;
      var borderHeight = (style.borderTopWidth + style.borderBottomWidth);
      var headerTop = headerPosition.top;
      var headerLeft = headerPosition.left;
      var nodeTop = (((useFixedPosition && headerPosition.top) || header.offsetTop) + headerHeight);
      var nodeLeft = ((useFixedPosition && headerPosition.left) || header.offsetLeft);

      // Determine vertical placement and height
      if ((ourBrowserPadding + browserHeight + ourBrowserPadding) > (headerTop + headerHeight + nodeHeight)) {
        // Menu is below the header
      } else if (ourBrowserPadding > (headerTop - nodeHeight)) {
        // Menu is below the header with a scroll bar
        nodeWidth += ourScrollbarWidth;
        node.style.height = ((browserHeight - (headerTop + headerHeight + borderHeight)) + ourBrowserPadding + "px");
        node.style.width = (nodeWidth + "px");
        getScrollBar(node).attach(getNodeContentsId(node));

      } else {
        // Menu is above the header
        nodeTop -= (headerHeight + nodeHeight);
      }

      node.style.top = (nodeTop + "px");

      // Determeine horizontal placement
      // TODO: Special case IE 5
      if (browserWidth > (headerLeft + nodeWidth)) {
        // Menu flows to the right
      } else if ((ourBrowserPadding + nodeWidth) > (headerLeft + headerWidth)) {
        // Menu won't fit either way, so just put it at the edge of the window. 
        nodeLeft += (ourBrowserPadding - headerLeft);

      } else {
        // Menu flows to the left
        nodeLeft += (headerWidth - nodeWidth);
      }


      // Hack to ensure proper rendering when the width of the title
      // is longer than the width of the top menu. Only applied to
      // palette menus ATM.
      if (!(useFixedPosition) && ((headerWidth - nodeLeft) > nodeWidth)) {
        node.style.width = ((headerWidth - nodeLeft) + "px");
      }

      node.style.left = (nodeLeft + "px");
      hideSectionNode(node);
    }

    // Layout sub level menu.  The "section" is below an "menu-subheader".
    // Current behaviour is specific to FireFox's box model.
    function layoutSubSection (node, sectionWidth, sectionStyle) {
      var browserHeight = (DomLibrary.getBrowserHeight() - (ourBrowserPadding * 2));
      var browserWidth = (DomLibrary.getBrowserWidth() - (ourBrowserPadding * 2));
      var leftOffset = (0 - sectionStyle.borderLeftWidth);
      var topOffset = (0 - sectionStyle.borderTopWidth);
      var header = getNodeHeader(node);
      var style = copyMenuStyleProperties(node);
      resetSectionNode(node);
      var headerPosition = DomLibrary.computeScrolledPosition(header);
      var nodeWidth = node.offsetWidth;
      var nodeHeight = node.scrollHeight;
      var borderHeight = (style.borderTopWidth + style.borderBottomWidth);
      var headerTop = (headerPosition.top + ourSubTopOffset + topOffset);
      var headerLeft = (headerPosition.left + ourSubLeftOffset + leftOffset);
      var nodeTop = (header.offsetTop - header.parentNode.scrollTop);
      var nodeLeft = header.offsetLeft;

      // Determine vertical placement and height
      if (browserHeight > (headerTop + nodeHeight)) {
        // Menu is next to the header
        nodeTop += (ourSubTopOffset + topOffset);

      } else if (browserHeight > nodeHeight) {
        // Menu is above the header
        nodeTop += ((browserHeight - (headerTop + nodeHeight + borderHeight)) + ourSubTopOffset + topOffset + ourBrowserPadding);

      } else {
        // Menu is at the top of the screen with a scrollbar
        nodeWidth += ourScrollbarWidth;
        nodeTop += ((0 - headerTop - borderHeight) + ourSubTopOffset + topOffset + ourBrowserPadding);
        node.style.height = ((browserHeight - borderHeight) + "px");
        node.style.width = (nodeWidth + "px");
        getScrollBar(node).attach(getNodeContentsId(node));
      }

      node.style.top = (nodeTop + "px");

      // Determine horizontal placement

      if (browserWidth > (headerLeft + sectionWidth + nodeWidth)) {
        // Menu flows to the right
        nodeLeft += (sectionWidth + ourSubLeftOffset + leftOffset);

      } else if (ourBrowserPadding > (headerLeft - nodeWidth)) {
        // Menu overlaps the original menu
        var overlap = Math.max(ourRemainingOverlapMin, browserWidth - (headerLeft + nodeWidth));
        nodeLeft += (overlap + ourSubLeftOffset + leftOffset);

      } else {
        // Menu flows to the left
        nodeLeft += (leftOffset - nodeWidth - ourSubLeftOffset);
      }

      // NOTE: If the top menu is position: fixed and node-left
      // is less than about 10, then the submenu appears
      // distored...
      // Why 10?  I don't know...
      if (10 > nodeLeft) {
        node.style.left = "";
        node.style.right = (header.offsetWidth + nodeWidth + nodeLeft + ourSubLeftOffset + ourSubLeftOffset + "px");

      } else {
        node.style.right = "";
        node.style.left = (nodeLeft + "px");
      }

      hideSectionNode(node);
    }

    // Constructor
    // 
    // Takes the ID of the menu and whether to use a fixed position
    // (non-palette only).
    // There are a lot of menus, so keep this brief.
    function menuConstructor (menuId, useFixedPosition) {
      var mySelf = this;
      var myMenuId = menuId;
      var myHandlers = {};
      var myDisplayed = "";
      var myUseFixedPosition = useFixedPosition;
      var myModifyStackingContext = useFixedPosition;

      myHandlers.headerMouseover = function (e) {
        var menu = document.getElementById(myMenuId);
        var displayed = hashPosition(menu);
        // Check that the root of the menu hasn't moved or the display adjusted
        if (!(myDisplayed === displayed)) {
          myDisplayed = displayed;
          // TODO: If IE call fixSections(menu)
          var section = getNodeSection(this);
          if (!(section === ourDummyNode)) {
            layoutTopSection(section, myUseFixedPosition);
          }
        }
        if (!(Jbml.isDragging())) {
          // Call with the original parameters
          headerMouseoverHandler.call(this, e);
        }
      };

      myHandlers.subheaderMouseover = function (e) {
        var thisDisplayed = (this.xDisplayed || "");
        var displayedScrolled = (myDisplayed + ";" + (this.parentNode.scrollTop || 0));
        // Check that the root of the menu hasn't moved or the display adjusted
        if (!(thisDisplayed === displayedScrolled)) {
          this.xDisplayed = displayedScrolled;
          var section = getNodeSection(this);
          // Will always have a container
          var container = getNodeContainer(this);
          if (!(section === ourDummyNode)) {
            layoutSubSection(section, container.offsetWidth, copyMenuStyleProperties(container));
          }
        }
        // Call with the original parameters
        headerMouseoverHandler.call(this, e);
      };

      myHandlers.headerMouseout = headerMouseoutHandler;
      myHandlers.subheaderMouseout = headerMouseoutHandler;
      myHandlers.headerSectionMouseover = sectionMouseoverHandler;
      myHandlers.subheaderSectionMouseover = sectionMouseoverHandler;
      myHandlers.headerSectionMouseout = sectionMouseoutHandler;
      myHandlers.subheaderSectionMouseout = sectionMouseoutHandler;

      if (myModifyStackingContext) {
        myHandlers.headerSectionShow = function () {
          var menu = document.getElementById(myMenuId);
          DomLibrary.raiseStackingContext(menu);
        };

        myHandlers.headerSectionHide = function () {
          var menu = document.getElementById(myMenuId);
          DomLibrary.lowerStackingContext(menu);
        };
      }

      // Public instance functions

      // Initialize the menu.
      mySelf.quickinit = function () {
        var menu = document.getElementById(myMenuId);
        if (!(menu)) {
          throw("Unable to locate menu : " + myMenuId);
        }
        // TODO: If IE5 call fixTextWrapping(...)
        menu.onmousedown = EventLibrary.stopPropagation;
        parseMenu(myMenuId, myHandlers, menu);
      };
      return mySelf;
    }

    // Public static functions

    // Creates a container to use for a menu.
    menuConstructor.makeContainer = function () {
      return $('<div>').addClass('menu-section').append($('<div>').addClass('menu-section-contents')).get(0);
    };

    // Locate the inner node of the menu container.
    menuConstructor.openContainer = function (node) {
      return (node.lastChild || node);
    };

    // Close all visible menus.
    menuConstructor.closeMenus = function () {
      var i = ourVisible.length;
      if (i) {
        while (0 < i) {
          --i;
          hideSection(ourVisible[i]);
        }
      }
    };

    // Retrieve the ID of the currently visible menu.
    menuConstructor.getVisibleId = function () {
      (ourVisible.length && ourVisible[0]);
    };

    return menuConstructor;

  }());
// end of Menu declaration.






  var JbmlBase = (function () {
    // Class constructor
    function ourClass () {
      return ("Jbml-base" && this);
    }

    // Private static variables
    var ourInvalidId = "-1";
    var ourInstance = new ourClass(); // Class singleton instance
    var ourDummy = {};
    var ourDefaultBorderStyle = "jbml-solid";
    var ourDefaultBorderWidth = "jbml-thin";

    ourClass.prototype.getInvalidId = function () {
      return ourInvalidId;
    };

    // Provide accessors for accessing static Jbml class instance stored on Jbml elements
    ourClass.prototype.setJbml = function (node) { // Private function
      (node || {}).xJbml = this;
    };

    ourClass.prototype.getJbml = function (node) {
      return ((node || ourDummy).xJbml || ourInstance);
    };

    // Accessor for the "boxId"
    ourClass.prototype.setBoxId = function (node, boxId) {
      (node || {}).xBoxId = boxId;
    };

    ourClass.prototype.getBoxId = function (node) {
      return ((node || ourDummy).xBoxId || ourInvalidId);
    };

    // Event-centric accessor for "boxId"
    ourClass.prototype.getEventBoxId = function (eventThis, e, node) {
      return this.getBoxId(EventLibrary.getCurrentTarget(eventThis, e) || node);
    };

    ourClass.prototype.setBoxName = function (node, name) {
      (node || {}).xBoxName = name;
    };

    ourClass.prototype.isBoxNameSet = function (node) {
      return node.xBoxName;
    };

    ourClass.prototype.getBoxName = function (node) {
      return ((node || ourDummy).xBoxName || this.getBoxId(node));
    };

    ourClass.prototype.updateClass = function (box, oldClass, newClass) {
      if (!(oldClass === newClass)) {
        DomLibrary.removeClass(box, oldClass);
        box.className += (" " + newClass);
      }
    };

    ourClass.prototype.setBorderColor = function (box, borderColor) {
      box.xBoxBorderColor = box.style.borderColor = (borderColor || "");
    };

    ourClass.prototype.resetBorderColor = function (box) {
      this.setBorderColor(box, box.xBoxBorderColor);
    };

    ourClass.prototype.setBorderStyle = function (box, borderStyle) {
      this.updateClass(box, box.xBoxBorderStyle || ourDefaultBorderStyle, borderStyle);
      box.xBoxBorderStyle = borderStyle;
    };

    ourClass.prototype.setBorderWidth = function (box, borderWidth) {
      this.updateClass(box, box.xBoxBorderWidth || ourDefaultBorderWidth, borderWidth);
      box.xBoxBorderWidth = borderWidth;
    };

    ourClass.prototype.init = function (box, boxId) {
      this.setJbml(box);
      this.setBoxId(box, boxId);
    };

    ourClass.prototype.makeSingleClickHandler = function (jsFuncName) { // Protected function
      //This gets called on a wide assortment, including close buttons, about 8 others.  But we probably shouldn't grab focus.
      return function (e) {
        var boxId = ourInstance.getEventBoxId(this, e);
        var button = EventLibrary.getButton(e);
        singleClickWrapper(this, function () {
          jsFuncName(boxId, button);
        });
        return EventLibrary.stopPropagation(e);
      }
    };


    /* //DEPRECATED, DELETE SOON
    ourClass.prototype.makeSingleClickGrabFocusHandler = function (jsFuncName) { // Protected function
      //JKM Dec '12.  Grabs the focus at same time click.  
      return function (e) {
	ourInstance.focus();    //Grab the focus.  For input slots, multi-line single-input slots, multi-line boxes.
        var boxId = ourInstance.getEventBoxId(this, e);
        var button = EventLibrary.getButton(e);
        singleClickWrapper(this, function () {
          jsFuncName(boxId, button);
        });
        return EventLibrary.stopPropagation(e);
      }
    };
    */

    // Event handlers
    var singleClickHandler = ourInstance.makeSingleClickHandler(doBoxMouseClick);

    function doubleClickHandler (e) {
      var boxId = ourInstance.getEventBoxId(this, e);
      var button = EventLibrary.getButton(e);
      clearMouseDelayTimer(this);
      doBoxMouseDoubleClick(boxId, button);
      return EventLibrary.stopPropagation(e);
    }

    function contextmenuHandler (e) {
      var boxId = ourInstance.getEventBoxId(this, e);
      var button = EventLibrary.rightMouseButton;
      doBoxMouseClick(boxId, button);
      lastMouseX = EventLibrary.getPageX(e);
      lastMouseY = EventLibrary.getPageY(e);
      return EventLibrary.stopPropagation(e);
    }

    function mousedownHandler (e) {
      if ((EventLibrary.rightMouseButton === EventLibrary.getButton(e)) || DomLibrary.hasClass(this, "hilight-jbml")) {
        /* This was not returned in the lispscript but it seems it should be. -PBS */
        EventLibrary.stopPropagation(e);
      }
      return true;
    }

    //"prototype" is of course system magic to define Class constants, which get inherited by Instances.
    //  inside  JbmlBase ...
    ourClass.prototype.createBox = function (boxId) {
      var className = ("a-jbml box-jbml " + ourDefaultBorderStyle + " " + ourDefaultBorderWidth);
      var box = $('<div>').addClass(className).attr('id', boxId).get(0);
      this.init(box, boxId);
      box.style.color = globalDefaultTextColor;
      box.onclick = singleClickHandler;
      box.ondblclick = doubleClickHandler;
      box.oncontextmenu = contextmenuHandler;
      box.onmousedown = mousedownHandler;
      return box;
    };

//  inside  JbmlBase ...
    ourClass.prototype.createBoxContainer = function (box, parentBox) {
      return box;
    };

    ourClass.prototype.removingChild = function (box, child) {
      removingChild(box, child);
    };

    ourClass.prototype.replacingChild = function (box, newChild, oldChild) {
      replacingChild(box, newChild, oldChild);
    };

    ourInstance.setBoxId(ourDummy, ourInvalidId);
    ourInstance.setBoxName(ourDummy, "unknown");

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());
// end of JbmlBase declaration.




  var JbmlBaseStructure = (function () {
    // Class constructor
    function ourClass () {
      return ("Jbml-base-structure" && this);
    }

    var ourParentClass = JbmlBase;
    var ourInvalidId = JbmlBase.getInvalidId();
    ourClass.prototype = new ourParentClass.constructor(); // Set parent class
    var ourInstance = new ourClass(); // Class singleton instance
    var ourDummy = {};

    ourClass.prototype.setParentId = function (node, boxId) {
      (node || {}).xParentId = boxId;
    };

    ourClass.prototype.getParentId = function (node) {
      return ((node || ourDummy).xParentId || ourInvalidId);
    };

    ourClass.prototype.getParent = function (node) {
      var parentId = this.getParentId(node);
      if ((parentId === ourInvalidId) || (parentId === node.id)) {
        return false;
      } else {
        while (node && !((node.id === parentId))) {
          node = node.parentNode;
        }
        return (node || (parentId && document.getElementById(parentId)) || false);
      }
    };

    function getJbmlBox (box) {
      var boxId = ourInstance.getBoxId(box);
      if (boxId === ourInvalidId) {
        return ourDummy;
      } else {
        return (((boxId === box.id) && box) || (box.firstChild && (boxId === box.firstChild.id) && box.firstChild) || document.getElementById(boxId));
      }
    }

    function getChildBoxes (box) {
      return (box.xChildBoxes || (box.xChildBoxes = {}));
    }

    function getChildBoxType (box, type) {
      var boxes = getChildBoxes(box);
      return (boxes[type] || (boxes[type] = {}));
    }

    function setValue (node, box, type, value) {
      getChildBoxType(node, type)[box.id] = value;
    }

    function removeValues (node, box) {
      var childBoxes = getChildBoxes(box);
      for (type in childBoxes) {
        var boxes = childBoxes[type];
        var nodeBoxes = getChildBoxType(node, type);
        for (child in boxes) {
          var value = boxes[child];
          delete(nodeBoxes[child]);
        }
      }
    }

	// in JbmlBaseStructure.
    ourClass.prototype.createBoxContainer = function (box, parentBox) {
      var node = parentBox;
      while (node && (("" === node.id) || (this.getBoxId(node) === this.getBoxId(box)))) {
        node = node.parentNode;
      }
      if (node) {
        this.setParentId(box, node.id);
      }
      return ourParentClass.createBoxContainer(box, parentBox);
    };

    ourClass.prototype.removingChild = function (box, child) {
      this.removeBoxValue(getJbmlBox(child));
      ourParentClass.removingChild(box, child);
    };

    ourClass.prototype.replacingChild = function (box, newChild, oldChild) {
      this.removeBoxValue(getJbmlBox(oldChild));
      ourParentClass.replacingChild(box, newChild, oldChild);
    };

    ourClass.prototype.setBoxValue = function (box, type, value) {
      var node = box;
      while (node) {
        setValue(node, box, type, value);
        node = this.getParent(node);
      }
    };

    ourClass.prototype.removeBoxValue = function (box) {
      if (box && box.xChildBoxes) {
        var node = this.getParent(box);
        while (node) {
          removeValues(node, box);
          node = this.getParent(node);
        }
        // Must come last so that there is something to remove
        removeValues(box, box);
      }
    };

    ourClass.prototype.visitChildBoxes = function (node, type, visit) {
      var childBoxes = getChildBoxType(node, type);
      for (boxId in childBoxes) {
        var value = childBoxes[boxId];
        visit(boxId, value);
      }
    };

    ourInstance.setParentId(ourDummy, ourInvalidId);

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());
// end of JbmlBaseStructure declaration.




  var JbmlBaseDnd = (function () {
    // Class constructor
    function ourClass () {
      return ("Jbml-base-dnd" && this);
    }

    // Private static variables
    var ourParentClass = JbmlBaseStructure;
    var ourInvalidId = JbmlBase.getInvalidId();
    ourClass.prototype = new ourParentClass.constructor(); // Set parent class
    var ourInstance = new ourClass(); // Class singleton instance
    var ourDummy = {};
    var ourDrag = new Drag();
    var ourPositionCache = null;
    var ourDraggedId = ourInvalidId;
    var ourDropId = ourInvalidId;
    var ourDropWhere = "";
    var ourForcedSetDropId = false;

    ourClass.prototype.isDragging = function () {
      return ourDrag.isDragging();
    };

    // Private Drag-n-Drop related functions
    function setDropId (boxId, where) {
      if (ourForcedSetDropId && !((ourDropId === ourInvalidId))) {
        var topLevel = document.getElementById(ourDropId).parentNode;
        ourForcedSetDropId = false;
        DomLibrary.removeClass(topLevel, "dragged-" + ourDropWhere);
        DomLibrary.removeClass(topLevel.parentNode, "drag-forced");
      }
      ourDropId = (boxId || ourInvalidId);
      ourDropWhere = (where || "");
    }

    function forceDrop (before, after) { // Calls setDropId
      if (ourInstance.isDragging()) {
        var boxId = ourInstance.getBoxId(after || before);
        var where = ((after && "after") || (before && "before") || "");
        if (!((boxId === ourDropId) && (where === ourDropWhere) && ourForcedSetDropId)) {
          var topLevel = document.getElementById(boxId).parentNode;
          setDropId(boxId, where);
          ourForcedSetDropId = true;
          DomLibrary.addClass(topLevel, "dragged-" + ourDropWhere);
          DomLibrary.addClass(topLevel.parentNode, "drag-forced");
        }
      }
    }

    function getDragTimer (node) {
      return node.xDragTimer;
    }

    function setDraggable (node, draggable) {
      (node || {}).xBoxDraggable = draggable;
    }

    function getDraggable (node) {
      return ((node || ourDummy).xBoxDraggable || false);
    }

    function getDroppable (node) {
      return ((node || ourDummy).xBoxDroppable || false);
    }

    function setDroppable (node, droppable) { // Calls getDroppable
      var node = (node || {});
      node.xBoxPreviousDroppable = getDroppable(node);
      node.xBoxDroppable = droppable;
    }

    function resetDroppable (node) { // Calls setDroppable
      setDroppable(node, node || ourDummy.xBoxPreviousDroppable);
    }

    function makeGetContainingBoxId (test) {
      return function (node) {
        while ((ourInvalidId === ourInstance.getBoxId(node)) || !(test(node))) {
          node = node.parentNode;
        }
        return ourInstance.getBoxId(node);
      }
    }

    var getDraggableBoxId = makeGetContainingBoxId(getDraggable);

    var getDroppableBoxId = makeGetContainingBoxId(getDroppable);

    function disableChildDrop (boxId, value) {
      if (value) {
        var box = document.getElementById(boxId);
        ourInstance.disableDrop(box);
        ourInstance.setBoxValue(box, "dnd-drop", true);
      }
    }

    function resetChildDrop (boxId, value) {
      ourInstance.resetDrop(document.getElementById(boxId));
    }

    // Begins dragging after a drag-click-delay. Closes any open
    // menus, otherwise their event handlers would fire during DND.
    // Creates a clone of the dragged item and attaches it to the
    // "drag-proxy" layer. Adds the "dragging" CSS class to the
    // document root. Adds an event handler for selectstart to prevent
    // IE from selecting text during DND. Starts the dragging.
    // However, it creates thet dragged object immediately.
    function startDrag (boxId, e) {
      clearElement("drag-proxy");
      Menu.closeMenus();
      var box = document.getElementById(boxId);
      var proxy = document.getElementById("drag-proxy");
      var root = DomLibrary.getRootElement();
      ourInstance.setBoxId(proxy, ourDraggedId = boxId);
      proxy.style.left = (EventLibrary.getPageX(e) + 5 + "px");
      proxy.style.top = (EventLibrary.getPageY(e) + 5 + "px");
      proxy.style.width = (box.offsetWidth + 2 + "px");
      // For some bizarre reason, all of the final styles must be
      // applied before the style is copied by the Drag code.
      // However, we remove the style immediately since we aren't
      // quite ready to drag yet.
      DomLibrary.addClass(root, "dragging");
      ourDrag.prep(proxy, e);

      // Remove the dragging class.
      DomLibrary.removeClass(root, "dragging");
      singleClickWrapper(getDragTimer(box), 
                         function () {
                           proxy.appendChild(DomLibrary.clearIds(box.cloneNode(true)));
                           ourInstance.visitChildBoxes(box, "dnd-drop", disableChildDrop);
                           DomLibrary.addClass(box, "drag-source");
                           DomLibrary.addClass(root, "dragging");
                           ourDrag.prepStart();
                           // JP.  Took out dragging message.  1/26/12.
                           // showClientStatus("Dragging " + ourInstance.getBoxName(box));
                         },
                         clientSettings.get("drag-click-delay"));
    }

    function makeDropMouseoverHandler (where) { // Calls setDropId
      return function (e) {
        // While dragging, updates ourDropId.
        if (ourInstance.isDragging()) {
          setDropId(ourInstance.getEventBoxId(this, e), where);
          return EventLibrary.stopPropagation(e);
        }
      }
    }

    function makeDropMouseoutHandler (where) { // Calls setDropId
      return function (e) {
        if (ourInstance.isDragging() && 
            (where === ourDropWhere) && 
            (ourDropId === ourInstance.getEventBoxId(this, e)) &&
            (ourDropId === ourInstance.getBoxId(EventLibrary.getTarget(e))))
        {
          setDropId();
          return EventLibrary.stopPropagation(e);
        }
      }
    }

    function createPositionCache () {
      return ourPositionCache = new PositionCache();
    }

    //
    // Event handlers
    //

    // Cancel any dragging
    function dragMouseupHandler (e) {
      clearMouseDelayTimer(getDragTimer(this));
    }

    // Cancel any dragging
    function dragMousemoveHandler (e) {
      clearMouseDelayTimer(getDragTimer(this));
    }

    // Performs the same functions as mousedown-handler.
    function dragMousedownHandler (e) {
      var boxId = ourInstance.getEventBoxId(this, e);
      if ((EventLibrary.leftMouseButton === EventLibrary.getButton(e)) &&
          (boxId === getDraggableBoxId(EventLibrary.getTarget(e))) &&
          !(DomLibrary.hasClass(this, "hilight-jbml")))
      {
        startDrag(boxId, e);
//      Very nasty Heisenbug.  If alert is in, EventLibrary FF 15++ drag works; if not, it doesn't.
//      Best guess at this point is some kind of timing issue.  See Oct 31 '12 black magic patch, below.
//	alert( "BoxID " + boxId + " return stop= " +  EventLibrary.stopPropagation(e)  ); 
        return EventLibrary.stopPropagation(e);
      }
    }

    // Called when dragging is finished. Undoes dragMousedownHandler
    // and checks if ourDropId is valid.
    function stopDragHandler (e) {
      DomLibrary.removeClass(DomLibrary.getRootElement(), "dragging");
      clearElement("drag-proxy");
      if (!(ourDraggedId === ourInvalidId)) {
        var box = document.getElementById(ourDraggedId);
        DomLibrary.removeClass(box, "drag-source");
        ourInstance.visitChildBoxes(box, "dnd-drop", resetChildDrop);
        if (!(ourDropId === ourInvalidId)) {
          switch (ourDropWhere) {
          case "on":
            doDndBox(ourDraggedId, ourDropId, box);
            break;
          case "after":
            doDndBoxAfter(ourDraggedId, ourDropId, box);
            break;
          case "before":
            doDndBoxBefore(ourDraggedId, ourDropId, box);
            break;
          default:
            // FIXME: Animate the box returning on failed drop
            break;
          }
        }
        setDropId();
        ourDraggedId = ourInvalidId;
      }
    }

    var dropMouseoverHandler = makeDropMouseoverHandler("on");
    var dropMouseoutHandler = makeDropMouseoutHandler("on");
    var dropAfterMouseoverHandler = makeDropMouseoverHandler("after");
    var dropAfterMouseoutHandler = makeDropMouseoutHandler("after");
    var dropBeforeMouseoverHandler = makeDropMouseoverHandler("before");
    var dropBeforeMouseoutHandler = makeDropMouseoutHandler("before");

    function dropContainerMouseoverHandler (e) {
      if (ourInstance.isDragging() && (EventLibrary.getCurrentTarget(this, e) === EventLibrary.getTarget(e))) {
        var nearest = createPositionCache().findNearestChild(this, EventLibrary.getPageX(e), EventLibrary.getPageY(e));
        if (nearest) {
          forceDrop(nearest.after, nearest.before);
        }
        EventLibrary.addEventListener(this, "mousemove", dropContainerMousemoveHandler);
        return EventLibrary.stopPropagation(e);
      }
    }

    function dropContainerMouseoutHandler (e) {
      if (ourInstance.isDragging() && ourForcedSetDropId && EventLibrary.isMouseexit(e)) {
        setDropId();
        EventLibrary.removeEventListener(this, "mousemove", dropContainerMousemoveHandler);
        this.xDragLastX = -1;
        this.xDragLastY = -1;
        return EventLibrary.stopPropagation(e);
      }
    }

    function dropContainerMousemoveHandler (e) {
      if (ourInstance.isDragging() && (EventLibrary.getCurrentTarget(this, e) === EventLibrary.getTarget(e))) {
        var x = EventLibrary.getPageX(e);
        var y = EventLibrary.getPageY(e);
        if (!((x === this.xDragLastX) && (y === this.xDragLastY))) {
          var nearest = (ourPositionCache || createPositionCache()).findNearestChild(this, x, y);
          if (nearest) {
            this.xDragLastX = x;
            this.xDragLastY = y;
            forceDrop(nearest.after, nearest.before);
          }
        }
      }
    }

    ourDrag.onStop = stopDragHandler;

    ourClass.prototype.enableDrag = function (box) {
      setDraggable(box, true);
      box.xDragTimer = (box.xDragTimer || {});
      EventLibrary.addEventListener(box, "mouseup", dragMouseupHandler);
      EventLibrary.addEventListener(box, "mousemove", dragMousemoveHandler);
      // Oct 31st '12 John Myers.  Black magic.  For some reason,
      // invoking the mousedown handler through the EventLibrary system works but sets up a subtle poison context
      // whereby uncle and cousin boxes are blocked from hover rollover / probably mousein messages,
      // preventing the workspace from ever being aware of results dragging, at the deep "hover" level,
      // whereas invoking the SAME routine via onmousedown
      // does not have any problems in this area.
      // THE SAME ROUTINE GETS INVOKED IN ANY CASE.   :-/
      // Some side effect is bolluxing up the action.
      // It took four days to get this far, and we're running out of time,
      // so "do what works".
      // EventLibrary.addEventListener(box, "mousedown", dragMousedownHandler); // This runs but no longer flies FF v.15++
      box.onmousedown =  dragMousedownHandler;  //Needed.
    };

    ourClass.prototype.disableDrag = function (box) {
      setDraggable(box, false);
      EventLibrary.removeEventListener(box, "mouseup", dragMouseupHandler);
      EventLibrary.removeEventListener(box, "mousemove", dragMousemoveHandler);
      EventLibrary.removeEventListener(box, "mousedown", dragMousedownHandler);
    };

    ourClass.prototype.enableDrop = function (box) {
      setDroppable(box, true);
      DomLibrary.addClass(box, "drag-target");
      EventLibrary.addEventListener(box, "mouseout", dropMouseoutHandler);
      EventLibrary.addEventListener(box, "mouseover", dropMouseoverHandler);
      this.setBoxValue(box, "dnd-drop", true);
    };

    ourClass.prototype.disableDrop = function (box) {
      setDroppable(box, false);
      DomLibrary.removeClass(box, "drag-target");
      EventLibrary.removeEventListener(box, "mouseout", dropMouseoutHandler);
      EventLibrary.removeEventListener(box, "mouseover", dropMouseoverHandler);
      this.setBoxValue(box, "dnd-drop", false);
    };

    // Sets the drop state to the previous drop state.
    ourClass.prototype.resetDrop = function (box) {
      resetDroppable(box);
      if (getDroppable(box)) {
        this.enableDrop(box);
      } else {
        this.disableDrop(box);
      }
    };

    ourClass.prototype.createBoxContainer = function (box, parentBox) {
      var box = ourParentClass.createBoxContainer(box, parentBox);
      if (!(DomLibrary.hasClass(parentBox, "contains-jbml"))) {
        return box;
      } else {
        var top = $('<div>')
          .addClass('top-level-edge')
          .addClass('top-level-horiz')
          .addClass('top-level-top')
          .get(0);

        var left = $('<div>')
          .addClass('top-level-edge')
          .addClass('top-level-vert')
          .addClass('top-level-left')
          .append($('<div>').addClass('top-level-hilight'))
          .get(0);

        var bottom = $('<div>')
          .addClass('top-level-edge')
          .addClass('top-level-horiz')
          .addClass('top-level-bottom')
          .get(0);

        var right = $('<div>')
          .addClass('top-level-edge')
          .addClass('top-level-vert')
          .addClass('top-level-right')
          .append($('<div>').addClass('top-level-hilight'))
          .get(0);

        var boxId = this.getBoxId(box);

        // May get set multiple times
        parentBox.onmouseout = dropContainerMouseoutHandler;
        parentBox.onmouseover = dropContainerMouseoverHandler;

        this.init(top, boxId);
        this.init(left, boxId);

        left.onmouseout = top.onmouseout = dropBeforeMouseoutHandler;
        left.onmouseover = top.onmouseover = dropBeforeMouseoverHandler;

        this.init(bottom, boxId);
        this.init(right, boxId);

        right.onmouseout = bottom.onmouseout = dropAfterMouseoutHandler;
        right.onmouseover = bottom.onmouseover = dropAfterMouseoverHandler;

        var child = $('<div>')
          .addClass("top-level-jbml")
          .append(box)
          .append(top)
          .append(left)
          .append(bottom)
          .append(right)
          .get(0);

        this.init(child, boxId);

        return child;
      }
    };

    ourInstance.setBoxId(ourDummy, ourInvalidId);
    setDraggable(ourDummy, false);
    setDroppable(ourDummy, false);

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());
// end JbmlBaseDnd declaration.





  var Jbml = (function () {
    // Class constructor
    function ourClass () {
      return ("Jbml" && this);
    }

    //whang the prototype object so that this object thinks it's a JbmlBaseDnd. (Inherit all its constants / methods.)

    ourClass.prototype = new JbmlBaseDnd.constructor(); // Set parent class
    var ourInstance = new ourClass(); // Class singleton instance

    // "Using prototype inheritance with static methods, returns a singleton"
    //...but reset our constructor so we make one of us, not one of them.
    ourInstance.constructor = ourClass;

    //this short-circuits the class record/constructor of the local ourInstance object 
    //to use the second-level internal named ourClassfunction.

    // anyway, we now seem to have a Jbml singleton, on which we can hang things.

    return ourInstance;

  }());
// End of Jbml declaration.  




  var JbmlIconBase = (function () {

    // Class constructor
    function ourClass () {
      return ("Jbml-icon-base" && this);
    }

    ourClass.prototype = new Jbml.constructor(); // Set parent class

    var ourInstance = new ourClass(); // Class singleton instance

    // Overrideable functions
    ourClass.prototype.getIconSrc = function () {
      alert("Override Jbml-icon-base.get-icon-src");
    };

    ourClass.prototype.getIconClassName = function () {
      return "icon-jbml action-icon-jbml";
    };
    ourClass.prototype.singleClickHandler = function (e) {
      alert("Override Jbml-icon-base.single-click-handler");
    };

    ourClass.prototype.attachIcon = function (box) {
      var src = this.getIconSrc();
      var className = this.getIconClassName();
      var icon = $('<img>')
        .attr("width", "8")
        .attr("height", "8")
        .attr("src", src)
        .addClass(className)
        .get(0);

      this.init(icon, this.getBoxId(box));
      icon.onclick = this.singleClickHandler;
      box.appendChild(icon);
      return icon;
    };

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());
// end of JbmlIconBase declaration.




  var JbmlIconDelete = (function () {
    // Class constructor
    function ourClass () {
      return ("Jbml-icon-delete" && this);
    }
    ourClass.prototype = new JbmlIconBase.constructor(); // Set parent class
    var ourInstance = new ourClass(); // Class singleton instance
    var className = (ourInstance.getIconClassName() + " jbml-delete");
    ourClass.prototype.getIconSrc = function () {
      return "images/redXhandlebox.gif";
    };
    ourClass.prototype.getIconClassName = function () {
      return className;
    };
    ourClass.prototype.singleClickHandler = ourInstance.makeSingleClickHandler(doBoxDeleteMouseClick);

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());
// end of JbmlIconDelete declaration.




  var JbmlIconClear = (function () {
    // Class constructor
    function ourClass () {
      return ("Jbml-icon-clear" && this);
    }
    ourClass.prototype = new JbmlIconBase.constructor(); // Set parent class
    var ourInstance = new ourClass(); // Class singleton instance
    var className = (ourInstance.getIconClassName() + " jbml-clear");
    ourClass.prototype.getIconSrc = function () {
      return "images/whitemauveXhandlebox.gif";
    };
    ourClass.prototype.getIconClassName = function () {
      return className;
    };
    ourClass.prototype.singleClickHandler = ourInstance.makeSingleClickHandler(doBoxClearMouseClick);

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());
// end of JbmlIconClear declaration.



  var JbmlIconHoleClear = (function () {
    // Class constructor
    function ourClass () {
      return ("Jbml-icon-hole-clear" && this);
    }
    ourClass.prototype = new JbmlIconClear.constructor(); // Set parent class
    var ourInstance = new ourClass(); // Class singleton instance
    ourClass.prototype.singleClickHandler = ourInstance.makeSingleClickHandler(doBoxClearHoleMouseClick);

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());
// end of JbmlIconHoleClear declaration.




  var JbmlIconClearDelete = (function () {
    // Class constructor
    function ourClass () {
      return ("Jbml-icon-clear-delete" && this);
    }

    ourClass.prototype = new JbmlIconBase.constructor(); // Set parent class

    var ourInstance = new ourClass(); // Class singleton instance
    var className = (ourInstance.getIconClassName() + " jbml-clear-delete");

    ourClass.prototype.getIconSrc = function () {
      return "images/whiteXhandlebox.gif";
    };
    ourClass.prototype.getIconClassName = function () {
      return className;
    };

    ourClass.prototype.singleClickHandler = ourInstance.makeSingleClickHandler(doBoxClearDeleteMouseClick);

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());
// end JbmlIconClearDelete declaration.




  var JbmlTracked = (function () {
    // Class constructor
    function ourClass () {
      return ("Jbml-tracked" && this);
    }
    var ourDummy = {};
    ourClass.prototype = new Jbml.constructor(); // Set parent class
    var ourInstance = new ourClass(); // Class singleton instance

    // Overrideable methods
    ourClass.prototype.getObjectType = function () {
      alert("Override Jbml-stateful.get-state-type");
    };

    ourClass.prototype.extractValue = function (object) {
      alert("Override Jbml-stateful.extract-value");
    };

    // .init and .set-parent-id must have been called for object
    ourClass.prototype.trackObject = function (object, value) {
      //save value if it exists, or empty string if unbound.
      this.setBoxValue(object, "tracked", value || "");   
    };

    ourClass.prototype.getTrackedObjects = function (node, retval, excludedBoxId) {
      var retval = (retval || []);
      if (node) {
        this.visitChildBoxes(node, "tracked",
                             function (boxId, value) {
                               var box = document.getElementById(boxId);
                               if (box &&
                                   // If excluding a box, check the box-id
                                   (!(excludedBoxId) || !((ourInstance.getBoxId(box) === excludedBoxId))))
                               {
                                 var extractedValue = ourInstance.getJbml(box).extractValue(box);
                                 if (!(value === extractedValue)) {
                                   ourInstance.setBoxValue(box, "tracked", extractedValue);
                                   retval.push(box);
                                 }
                               }
                             });
      }
      return retval;
    };

    ourClass.prototype.getParentTrackedObjects = function (child, excludedBoxId) {
      return this.getTrackedObjects(child && this.getParent(child), null, excludedBoxId);
    };

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());
// end JbmlTracked declaration.




  var JbmlHoleBase = (function () {
    // Class constructor
    function ourClass () {
      return ("Jbml-hole-base" && this);
    }

    var ourInputIndex = 0;
    var ourDummy = {};

    ourClass.prototype = new JbmlTracked.constructor(); // Set parent class
    ourDummy.value = "";

    var ourInstance = new ourClass(); // Class singleton instance

    function removeInputAndVisitBoxtext (parent, inputType, visit) {
      var child = parent.firstChild;
      while (child) {
        var nextChild = child.nextSibling; // incase child gets deleted
        if ((child.nodeName && (child.nodeName === inputType)) ||
            DomLibrary.hasClass(child, "jbml-clear"))
        {
          // Remove input boxes (either INPUT or TEXTAREA)
          parent.removeChild(child); // Remove jbml-clear icons

        } else if (DomLibrary.hasClass(child, "boxtext-jbml") ||
                   (child.style && (child.style.fontStyle === "italic")) || // apparently not used anymore?
                   (child.nodeName && (child.nodeName === "I"))) // OBSOLETE
          {
            visit(child);
          }
        child = nextChild;
      };
    }

    function setInputId (node, inputId) {
      node.xJbmlInputId = inputId;
    }

    function getInputId (node) {
      return (node.xJbmlInputId || false);
    }

    ourClass.prototype.extractValue = function (object) {
      return ((getInputId(object) && document.getElementById(getInputId(object))) || object || ourDummy).value;
    };

    // Overrideable methods
    ourClass.prototype.getInputType = function () {
      alert("Override jbml-hole-base.get-input-type");
    };

    ourClass.prototype.createOpenHoleInput = function (box, value) {
      alert("Override jbml-hole-base.create-open-hole-input");
    };

    // Event handlers
    function holeMouseoverHandler (e) {
      var boxId = ourInstance.getEventBoxId(this, e);
      // Could do something with boxId
      return EventLibrary.stopPropagation(e);
    }

    function holeMouseoutHandler (e) {
      var boxId = ourInstance.getEventBoxId(this, e);
      doBoxMouseOut(boxId);
      // Could do something with boxId
      return EventLibrary.stopPropagation(e);
    }

    function holeOnBlurHandler (e) {
      var boxId = ourInstance.getEventBoxId(this, e);
      doHoleBlur(boxId);
      return EventLibrary.stopPropagation(e);
    }


    function doubleClickHandler (e) {
      var boxId = ourInstance.getEventBoxId(this, e);
      var button = EventLibrary.getButton(e);
      clearMouseDelayTimer(this);
      doBoxMouseDoubleClick(boxId, button);
      return EventLibrary.stopPropagation(e);
    }

    var openHoleClickHandler = ourInstance.makeSingleClickHandler(doBoxMouseClick);

    var closedHoleClickHandler = ourInstance.makeSingleClickHandler(doClosedHoleMouseClick);

    ourClass.prototype.initHole = function (box) {
      this.init(box, this.getBoxId(box));
      this.setBorderStyle(box, "jbml-dotted");
      box.ondblclick = doubleClickHandler;
      box.onmouseover = holeMouseoverHandler;
      box.onmouseout = holeMouseoutHandler;

      box.onblur = holeOnBlurHandler;

      // NOTE: A hidden box is no longer used
      this.enableDrop(box);
      this.disableDrag(box);
    };

    // Turns a hole into an open hole                                               // FIX ME needs it?
    //...We don't want to grab focus, here, because this might be done automatically as part of display. 
    ourClass.prototype.open = function (hole, value) {
      removeInputAndVisitBoxtext(hole, this.getInputType(), DomLibrary.hide);
      hole.onclick = openHoleClickHandler;
      hole.ondblclick = EventLibrary.stopPropagation;
      var input = this.createOpenHoleInput(hole, value);
      this.init(input, this.getBoxId(hole));
      if (!(input.id)) {
        // Ensure the input has an ID.
        ++ourInputIndex;
        input.id = ("jbml-input-" + ourInputIndex);
      }

      this.setParentId(input, this.getBoxId(hole));
      this.trackObject(input, value);
      setInputId(hole, input.id);
      JbmlIconHoleClear.attachIcon(hole);
      input.focus();
      // In some cases we might not already know "value"
      return input;
    };

    // Turns a hole into a closed hole
    ourClass.prototype.close = function (hole) {
      removeInputAndVisitBoxtext(hole, this.getInputType(), DomLibrary.show);
      hole.onclick = closedHoleClickHandler;
      hole.ondblclick = doubleClickHandler;
    };

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());
// end JbmlHoleBase declaration.




  var JbmlHole = (function () {
    // Class constructor
    function ourClass () {
      return ("Jbml-hole" && this);
    }

    ourClass.prototype = new JbmlHoleBase.constructor(); // Set parent class

    var ourInstance = new ourClass(); // Class singleton instance

    ourClass.prototype.getObjectType = function () {
      return "input";
    };

    ourClass.prototype.getInputType = function () {
      return "INPUT";
    };

    //Prep.  NOTE PATTERNS DO NOT TAKE ""'s IN JS, THEY ARE SURPRISINGLY RAW.  /g means "global", across the whole string.
    function containsCR( mystring ) {
      //      alert(mystring); alert( ""+mystring.search( /\r\n/ ) ); alert( ""+(-1 != mystring.search( /\r\n/ ))  );
return  (-1 != mystring.search( /\r\n/ ));}  //true or false if have CR. -1 if NOT there, so not NOT there.
    //    function wash_no_CRs( mystring ) { alert( mystring.replace( /[\r\n]/g, " " ));  return mystring.replace( /[\r\n]/g, " " );}  //replaces with a space.  FIX ME NOT WORKING TEST
    function wash_no_CRs( mystring ) { return mystring.replace( /[\r\n]/g, " " );}  //replaces with a space.  FIX ME NOT WORKING TEST


    function inputOnClickHandler(e) {
      var input = EventLibrary.getCurrentTarget(this, e);
      input.focus();
      return true;  //true bubbles; false prevents; except onmouseover is backwards.
    }


    // Event handlers
    function inputKeypressHandler (e) {
      var input = EventLibrary.getCurrentTarget(this, e);
      var boxId = ourInstance.getBoxId(input);
      // TODO: TAB, ENTER and ESC are supposedly not passed by IE, we might need a keyup/down handler instead...
      //alert( "" + EventLibrary.getKeypressCharCode(e)  );

      switch (EventLibrary.getKeypressCharCode(e)) {
        /* Seems like maybe these first two cases should return true or false. -PBS */
      case 9: // TAB
	var washed_no_CR_input_value = wash_no_CRs( input.value );
        return doInputTextTab(boxId, washed_no_CR_input_value);  //sole place called.
      case 13: // ENTER
	// alert("CR HIT--SENDING");
//alert( input.value );
	var washed_no_CR_input_value = wash_no_CRs( input.value );
//	alert( washed_no_CR_input_value );
        return doInputText(boxId, washed_no_CR_input_value);  //sole place called.
      case 27: // ESC
        doBoxClearHoleMouseClick(boxId, EventLibrary.leftMouseButton);
        return EventLibrary.stopPropagation(e);
      case 118:  // Was 86...why the change???   V, we are trapping Cntrl-V BEFORE it turns into an onpaste event...
	if (e.ctrlKey)
	  {
	    /*
	    alert( "Invoking Cntrl-V handler!");
	    var paletteZ = $("#palette").css('z-index');
	    var wheelZ   = $("#palette-logo").css('z-index');
	    var wheelV   = $("#palette-logo").css('visibility');
	    var wheelD   = $("#palette-logo").css('display');
	    alert( "palette Z: " + paletteZ + " wheelZ:" + wheelZ + " wheelV: " + wheelV + " wheelD:" + wheelD );
	    */

	    inputOnCntrlVHandler(e); 	    //my beforeonpaste!
	  }
        return true;  //true bubbles; false prevents; except onmouseover is backwards.

      default:
	//alert("default");
	checkSinglelineInputslot_LocalMultilineIfLarge( input );

        return true;  //true bubbles; false prevents; except onmouseover is backwards.
      }
    }


    function findTextAreaCols( value )
    //Tell me how many columns I should create for a given input text stream.
    {
      var max = 10;
      var count = 0;

      for(var i=0; i<value.length; i++)
      {  count++;
	if( value.charAt(i) == '\n' )
	  {   if(count > max)  max = count;
	      count = 0;
	  }
      }
      if(count > max)  max = count;  //Last (pussibly first, too) line too long?

      if( max > inputMaxWidthChars() ) max = inputMaxWidthChars(); //clamp the ceiling.

      // alert("Columns recommended: " + max);
      return(max);
    }

    function findTextAreaRows( value, cols )
    //Tell me how many rows I should create for a given input text stream,
    // given the columns count.
    {
      /* 
      var output = "";
      for(var i=0; i<value.length; i++)
	output += " " + value.charCodeAt(i);
      alert(output);
      */
      //alert(value);

      var rows = 1;
      var count = 0;
      for(var i=0; i<value.length; i++)
      {  count++;
	if( count > cols)  //Wrap at end of width, already properly pre-determined.
	  { count = 0;
	    rows++;
	  }
	if( value.charAt(i) == '\n' )  //Wrap at CR.
	  {   rows++;
	      count = 0;
	  }
      }

      //alert("Rows recommended: " + rows);
      return(rows);
    }

    function swapInput_forTextArea( input )       //USED!
    //May or may not have CR in it already, or be amazingly long, or both.
    //Replace an existing single-line input slot with a multi-line textarea,
    //but one that's pretending to still be a single-line input slot.
    //Put a flag on it so it doesn't get converted twice.
    // JKM, Nov '12.
    {

      if(input.textflag)
	{ //alert("Already swapped, not doing it again.");
	  input.focus();
	  return (input);
	}

      // Get contents of old input slot.
      var value = input.value;
      // Make the new multiline textarea, that we're going to be copying then swapping
      var textInput = document.createElement('textarea');

      // Set up access to the major class functions, by sticking the xJbml superclass container on it.
      textInput['xJbml'] = input['xJbml'];   //shallow copy.

      //Copy over its guts:
      /*
      alert("Copying...");

      for( var key in input)
      {
	alert( key );
	alert( input[key] );
	textInput[key] = input[key];   //shallow copy.
      }
      */
      //alert("Finished!");

      //alert("where...");
      var caretstart = $(input).caret().start;
      //alert("startcursor at " + caretstart);
      var caretend = $(input).caret().end;
      //alert("endcursor at " + caretend);

      textInput.setAttribute('name', input.getAttribute('name'));
      textInput.setAttribute('id', input.getAttribute('id'));
      //textInput.cols  = input.size;   //ummm.... not really...
      textInput.textflag  = true;    //Set my flag.  True when this is a textArea.

      textInput.setAttribute('onkeypress', input.getAttribute('onkeypress'));
      //Previous didn't pick up, which is worrisome.
      textInput.onkeypress = inputKeypressHandler;
      // textInput.setAttribute('onpaste', input.getAttribute('onpaste')); //deprecated
      //      textInput.setAttribute('afterpaste', input.getAttribute('afterpaste'));

      // Set the magic BoxID
      //DUMP( input );
      //alert("Resetting ID to " + input.xBoxId );
      textInput.xBoxId = input.xBoxId;  //probably too low level, but...

      textInput.onclick = input.onclick;    //handles shifting focus here if you click on it.
      textInput.ondoubleclick = EventLibrary.stopPropagation;      //voodoo to stop propagation. Not sure how needed but I'm superstitious.
      textInput.onmousedown = function (e) { e.stopPropagation();  return true;};  // Jan 8 '13 needed for caret from mouseclick

      //DUMP(textInput);


      textInput.textflag  = true;    //Set my flag.  True when this is a textArea.

      //set the size properly.
      //  0 rows gives 3.2 rows as output.  0.5 does the same thing.
      //      textInput.rows = 0;  //For some unGodly reason the number of rows appears to be 0-based. No idea why.
      textInput.rows = 1;
      textInput.cols = (input.size > 10) ? (input.size + 1) : 10;  //1 for an extra space.  It gets dropped when typing the next letter, though.


      //var lineHeight = parseFloat($(this).css("line-height"));
      //    var lineHeight = parseFloat( textInput.css("line-height"));
      //    var lines = 1;                    // $(this).attr("rows")*1 || $(this).prop("rows")*1;
      //    textInput.css("height", lines*lineHeight);

      textInput.style.height = "3.0ex";  //ex is height, em is width but works for height too.  Both are lower case m, x.


      //Now, install it!
      input.parentNode.replaceChild(textInput, input);
      //input.parentNode.appendChild(textInput, input);

      // alert("swapped!");

      //Set the focus on the new object, so the paste happens into it, not the old one
      textInput.focus();  //Prob. MUST COME BEFORE set value or cursor set at beginning...

      //Copy the OLD text over.  Put this after focus to ensure cursor at end.
      textInput.value = "";   //two people recommended this, not sure why.  Voodoo to help cursor go to end.
      textInput.value = input.value;

      //New magic to position cursor properly, Jan 4 '13:
      $(textInput).caret( caretstart, caretend );

      //Magic to remember the value of the input open hole upon refreshing the screen.
      //Unclear whether this should be called at the box or at the hole level.
      //textInput.setParentId(textInput, textInput.getBoxId(hole));  //I think something like this has been done already by now.
      //textInput.trackObject(textInput, textInput.value);  //breaks. No handler at this level.
      JbmlHole.trackObject(textInput, textInput.value);  //use this one instead. Class object function.


      return( textInput );
    }



    function checkSinglelineInputslot_LocalMultilineIfLarge( input )  //JKM Oct 5 '12
    //Returns true if changed or if already textInput; returns false if still one line.
    //Used in auto-expansion of single line.  USED!
    //Note that single line might be a textarea sometimes...
    {
      if( input.textflag )               // Are we a textbox already?? Don't expand it twice!!!
	{
	  //ALREADY A TEXTAREA--adjust rows & cols
	  resizeTextArea( input );
	}
      else
	{
	  // Note textArea uses fixed-width font, so works nicely, whereas input uses variable-width font, needs padding.
	  //	  var required_size = 1.23 * (input.value + " ").length; //Two spaces because a pixel is getting lost every 20 or so. Unclear why. Now mult booster, too.
	  var required_size = 1.0 * (input.value + " ").length; //Two spaces because a pixel is getting lost every 20 or so. Unclear why. Now mult booster, too.
	  if( required_size > inputMaxWidthChars() ) // input.maxwidthchars might not exist, play it safe...
	    {
	      //alert( "uh, " + required_size + "~~" + input.value.length + " > " + inputMaxWidthChars() + ", swapping" );
	      swapInput_forTextArea( input );
	      //return true;
	    }
	  else
	    {
	      //Stretch single input if needed.

	      //alert( "Oh! " + input.value.length + " NOT > " + inputMaxWidthChars() );

	      // JKM code to stretch single-line input box.  NEEDS CAP, provided by previous section.
	      if( input.size < required_size )  
		input.size = required_size;
	    }
      }
      //return false;
    }



    function inputOnCntrlVHandler(e)
    // The Cntrl-V handler for the single-line, and textarea pretending to be single-line, input slot.
    //now hotwired to our own catch, called from ^ + V keypress interception, BEFORE onpaste.
    // This one takes an event as its arg.
    {
      //alert("input's onCntrlV paste");      

      /*
	ALWAYS turn a Cntrl-V into a text-box.  Make it single-line so it pretends.
	This is necessary because
	regular single-line inputs quietly wash the CRs into spaces, and never put them in!
	But the textbox is OK!
      */

      var input = EventLibrary.getCurrentTarget(this, e);  //Get the single-slot "input" box.

      //Equivalent selection handling done inside here.  Have to copy the Selection swipe over as well.
      var newTextArea = swapInput_forTextArea( input );
      //also sets the focus to the new one.
      //Focus does not need to be reset if we're  not swapping because it's already a textArea.

      //alert("onCntrlV mostly finished, about to queue After.  Tah.");

     //Now queue up the following handler.
      //     window.setTimeout(function(){ inputAfterCntrlVHandler(e) },5);  //JKM was 0. Time in milliseconds.
//    e = $.extend({},e,{type:'afterpaste'});
    //    window.setTimeout(function(){ $(e.target).trigger(e) },5);  //JKM was 0. Time in milliseconds.
      //    window.setTimeout(function(){ newTextArea.trigger(e) },5);  //JKM was 0. Time in milliseconds.
      window.setTimeout(function(){ inputAfterCntrlVHandler(newTextArea) },5);  //JKM was 0. Time in milliseconds.

    //alert("done with swap; rows:" + textInput.rows);
    }



    function resizeTextArea( textInput )
    //Grow a known textArea to proper size cols and rows.
    {
      var value = textInput.value;

      //Think for a minute, and set the size properly.
      textInput.cols   = findTextAreaCols( value );
      var desired_rows = findTextAreaRows( value, textInput.cols );

      //alert("Resized to " + desired_rows + " x " + textInput.cols);

      // Hack to make the height of a textarea be 1 row if you want 1:
      if( desired_rows > 1 )
      {
	//alert("larger rows, killing hammer.");
	textInput.style.height = "auto";  //Get rid of the height hammer. MUST BE FIRST.
	textInput.rows = desired_rows - 1;   //FF adds one extra row onto end, but why not.
      }
      // otherwise just leave it as it is.
    }



    function inputAfterCntrlVHandler (textInput)                               //JKM Oct 5 '12
    //Invoked by a timer, slightly after text has been pasted in an input slot that just got quietly swapped into a textarea.
    // This one now takes the textInput box itself as its arg.  Is invoked from a timeout.
    {
      //alert("on After Paste:...");
      //var textInput = EventLibrary.getCurrentTarget(this, e);
      var value = textInput.value;

      //alert( value );
      
      /* 
      var output = "";
      for(var i=0; i<textInput.value.length; i++)
	output += " " + value.charCodeAt(i);
      alert(output);
      */

      //At this point we are now guaranteed to have a textarray in any case.
      //Resize it properly.

      resizeTextArea( textInput );

      //alert("After Cntrl-V finished.");
    }



    ourClass.prototype.createOpenHoleInput = function (box, value) {
      var input = $('<input>')
        .attr("type", "text")
        .attr("name", "Text Input box")
        .attr("size", "5")
        .attr("value", value)
      //        .attr("maxwidthchars", inputMaxWidthChars())   //I'm using single global for safety now.
        .get(0);

      this.init(input, this.getBoxId(box));
      input.onkeypress = inputKeypressHandler;

      //Jan 8 '13  This was put in because the cursor caret was not showing up in input text
      //boxes upon click, or only on the even clicks.
      //It seems to have something to do with the clickstream falling through to the
      //behind box, which then grabs focus by having the dotted lines put on 
      //or, weirdly, taken off of it.  This is simply deleting a hang-tag
      //so it should have no effect.  There's no excuse for this problem in any case.
      // Cursor carets are controlled by onmousedown.
      input.onmousedown = function (e) { e.stopPropagation();  return true;};  

      input.onclick = inputOnClickHandler;   //Dec '12

      //input.onpaste = "alert('onpaste for real')";  //this one never seemed to work. String/func wrong somehow.
      //input.onpaste = inputCntrlVHandler;  //No good.  We need the stuff already pasted in before working with it.
      //Aaannnd...... we need to swap the input box with a textbox BEFORE the paste happens.
      // Ha ha, although onbeforepaste works in IE, neither of these work in FF.
      //input.onbeforepaste="alert('onbeforepaste')" ;  //but this assumes alert syntax is right. Bad test?
      //input.beforepaste="alert('beforepaste')" ;
      //      input.onpaste=inputOnCntrlVHandler; //CAN'T USE THIS because it blocks out .focus() shifts.  Happens BEFORE paste.
      //      input.onbeforepaste = quickswapBox; //CAN'T USE THIS unsupported in FF, use onpaste for before anyway.
      //Required again, we're going back through it.  No, deprecated again.
      //input.afterpaste = inputAfterCntrlVHandler;

      box.appendChild(input);
      return input;
    };

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());
// end JbmlHole declaration.






  var JbmlInputText = (function () {
    // Class constructor
    function ourClass () {
      return ("Jbml-input-text" && this);
    }
    ourClass.prototype = new JbmlHole.constructor(); // Set parent class
    var ourInstance = new ourClass(); // Class singleton instance

    ourClass.prototype.initHole = function (box) {
      this.init(box, this.getBoxId(box));
      box.style.borderStyle = "none";
      box.style.margin = "0px";
    };

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());
// end JbmlInputText declaration.



  var JbmlMultilineHole = (function () {
    // Class constructor
    function ourClass () {
      return ("Jbml-multiline-hole" && this);
    }
    var ourMaxCols = 80;

    ourClass.prototype = new JbmlHoleBase.constructor(); // Set parent class

    var ourInstance = new ourClass(); // Class singleton instance

    // Private static functions
    function setAdditionalRows (textarea, rows) {
      textarea.xAdditionalRows = rows;
    }

    function getAdditionalRows (textarea) {
      return (textarea.xAdditionalRows || 0);
    }

    function getEventTextarea (eventThis, e) {
      return document.getElementById(ourInstance.getEventBoxId(eventThis, e) + "-textarea");
    }

    function prepareValue (entriesNode) {
      var entryNode = ((entriesNode && entriesNode.firstChild) || null);
      var content = ((isString(entriesNode) && entriesNode) || "");
      var join = ""; // Set after the first iteration of the loop to "\n"
      while (entryNode) {
        content += (join + DomLibrary.getTextContent(entryNode));
        join = '\n';
        entryNode = entryNode.nextSibling;
      }
      return content;
    }

    function splitRows (textarea) {
      if (!(textarea.value === textarea.xPrevValue)) {
        textarea.xPrevValue = textarea.value;
        var rows = ("" + textarea.value).split('\n');
        var i = 0;
        var maxCols = (ourMaxCols + 2); // The space for the vertical scroll bar adds 2 columns
        while (i < rows.length) {
          if (ourMaxCols < rows[i].length) {
            var row = rows[i];
            // Find a place to break the line moving back from
            // max-cols, or after max-cols (scrolled), or we
            // cannot break the line
            var split = ((row.lastIndexOf(" ", maxCols) + 1) ||
                         (row.indexOf(" ", maxCols) + 1) ||
                         row.length);

            // If the line was broken
            if (split < row.length) {
              rows[i] = row.substr(0, split);
              if ((i + 1) < rows.length) {
                rows = rows.slice(0, i + 1).concat(row.substr(split), rows.slice(i + 1));
              } else {
                // We were at the end of the text, so just push it
                rows.push(row.substr(split));
              }
            }
          }
          ++i;
        }
        textarea.xSplitRows = rows;
      }
      return textarea.xSplitRows;
    }

    function updateRowsAndCols (textarea) {
      var rows = splitRows(textarea);
      var cols = 10;
      for (var i = 0; (i < rows.length); i++) {
        if (cols < rows[i].length) {
          cols = rows[i].length;
        }
      }
      if (ourMaxCols < cols) {
        cols = ourMaxCols;
      }
      textarea.cols = cols;
      textarea.rows = (rows.length + getAdditionalRows(textarea));
    }

    ourClass.prototype.getObjectType = function () {
      return "multiline-input";
    };

    ourClass.prototype.getInputType = function () {
      return "TEXTAREA";
    };

    // Event handlers
    function textareaKeypressHandler (e) {
      if (EventLibrary.getEvent(e).ctrlKey) {
        /* This seems screwy--shouldn't we be returning the value of stopPropagation() Maybe a Lispscript WHEN instead of IF? -PBS */
        EventLibrary.stopPropagation(e);
        return true;
      }
    }

    function textareaKeydownHandler (e) {
      var textarea = EventLibrary.getCurrentTarget(this, e);
      var boxId = ourInstance.getBoxId(textarea);
      switch (EventLibrary.getEvent(e).keyCode) {
      case 9: // TAB
        return doInputMultilineTextTab(boxId, textarea.value);
      case 27: // ESC
        doBoxClearHoleMouseClick(boxId, EventLibrary.leftMouseButton);
        return EventLibrary.stopPropagation(e);
      default:
        return true;
      }
    }

    function textareaValueModifiedHandler (e) {
      updateRowsAndCols(EventLibrary.getCurrentTarget(this, e));
    }

    function buttonEnterHandler (e) {
      var textarea = getEventTextarea(this, e);
      if (textarea) {
        doInputMultilineText(ourInstance.getBoxId(textarea), textarea.value);
      }
      return EventLibrary.stopPropagation(e);
    }

    function buttonExpandHandler (e) {
      var textarea = getEventTextarea(this, e);
      if (textarea) {
        setAdditionalRows(textarea, getAdditionalRows(textarea) + 1);
        updateRowsAndCols(textarea);
        textarea.focus();
      }
      return EventLibrary.stopPropagation(e);
    }

    function buttonResizeHandler (e) {
      var textarea = getEventTextarea(this, e);
      if (textarea) {
        setAdditionalRows(textarea, 0);
        updateRowsAndCols(textarea);
        textarea.focus();
      }
      return EventLibrary.stopPropagation(e);
    }

    function oldmultilineinputOnClickHandler(e) {
      var input = EventLibrary.getCurrentTarget(this, e);
      input.focus();
      return true;  //true bubbles; false prevents; except onmouseover is backwards.
    }



    ourClass.prototype.createOpenHoleInput = function (box, value) {

      var that = this;

      function makeButton (label, onclick, boxId) {
        var button = $('<input>')
          .attr('type', 'button')
          .attr('value', label)
          .click(onclick)
          .css('width', '45px')
          .css('height', '23px')
          .css('font-size', '70%')
          .get(0);
        
        
        button.ondblclick = EventLibrary.stopPropagation;
        that.setBoxId(button, boxId);
        return button;
      }


      var value = prepareValue(value);

      var textareaId = (this.getBoxId(box) + "-textarea");

      var boxId = this.getBoxId(box);

      var buttonEnter = makeButton('Enter', buttonEnterHandler, boxId);
      var buttonExpand = makeButton('More', buttonExpandHandler, boxId);
      var buttonResize = makeButton('Resize', buttonResizeHandler, boxId);

      var input = $('<textarea>')
        .attr('id', textareaId)
        .css('overflow', 'visible') // TODO: Move to CSS class
        .css('white-space', 'nowrap')
        .text(value)
        .get(0);

      this.init(input, boxId);
      input.onkeyup = textareaValueModifiedHandler;
      input.onkeydown = textareaKeydownHandler;
      input.onkeypress = textareaKeypressHandler;

      input.onclick = oldmultilineinputOnClickHandler;   //Dec '12
      input.onmousedown = function (e) { e.stopPropagation();  return true;};  //Jan '13


      // This is not supported on all browsers
      EventLibrary.addEventListener(input, "DOMAttrModified", textareaValueModifiedHandler);

      box.appendChild($('<div>')
                      .css('clear', 'none')
                      .css('float', 'left')
                      .css('text-align', 'right')
                      .append(buttonEnter)
                      .append($('<br>').css('clear', 'none'))
                      .append(buttonExpand)
                      .append($('<br>').css('clear', 'none'))
                      .append(buttonResize)
                      .get(0));

      box.appendChild($('<span>').css('white-space', 'pre').text(' ').get(0));
      box.appendChild(input);

      updateRowsAndCols(input);
      return input;
    };

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;
    
  }());
  //end JbmlMultilineHole declaration.




  return {  setup: setup,		//Public functions [variables] are here declared, separated by commas.
	shutdown: shutdown	//Anything not declared is private, internal to the object.
};

}())
// And the whole thing lives inside the VPL declaration.

// The preceding is a non-standard new tricky way of working with objects in JavaScript.
// Instead of using a named function as an object / instantiator, exposing all its member functions, 
// then NOT EXPLICITLY RETURNING A VALUE which returns "this", as used to be customary,
// the setup is creating a singleton object using the "module pattern".  Search on "module" to find more documentation on this.
// It is an unnamed lambda.
//  The setup : setup has the special effect of binding routine "setup" to path VPL.setup,
//  and then making the rest of the object disappear to view.
//  I believe it may also return an identical object if the code were to be somehow instantiated again, ensuring a true singleton.
// The net effect is that all of its functions are now private and unexposed, except setup, which is public. 
// Which means you can't call any of its functions from outside, and it's self-contained.


