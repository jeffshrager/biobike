/*
This file supports creating Boxes That Don't Do Anything 
for the independent Help Window system.
This is similar to but substantially different from the JBML code for the VLP window,
which supports Boxes That Do Something.
In reality, OUR boxes are one big button, that must invoke a special fancy, magic URL when clicked.

The boxes have to look just like the ones in VPL, however.
So we are copying massive amounts of code, with massive surgery edits, to get the right effect.

Our boxes have to be positioned in a vertical layout, they can't be loose filled.

User Interface:

attachJbmlBoxes (jbml, parentBox, superboxId)
where jbml is a xxxstring containing the Json commandsxxx   no, a series of boxes CREATED by parsing the JSON command.
superboxId is the string ID of the box you're going to stick things in
parentBox is the box itself that you're going to stick things in, currently document.getElementById("id").

John Myers, Oct '11.


"Output is going all the way through to Popup, but it's displaying my error code, indicating there's an undefined in the chain somewhere.
It could be because I wasn't explicitly setting the id of the box definition I was feeding it.
We'll need to change this so it calls the processor inside Lisp."    may or may not be fixed by now.

...There is a potential problem with id assignment collisions when multiple boxes are being displayed
on the same web page.  Boxes look like they're using the ID of the enclosing superbox
to locate where to insert the subboxes, when the presentation is being constructed.
If this always picks the last one on the page, it's not going to be a problem.
If it picks the first one on the page, it should be, 
but only if some random documentation box happens to re-use the same id from long ago
and they both come up at the same time.
I've done some prelimary tests, and even with four boxes with the same ID,
I can't make it break.  So I'm guessing it's alright as it is.
Nonetheless, if boxes start coming up nested in weird places, this is where to look.

Click handling solution is obvious in retrospect.  I had toyed with the idea of still supporting
the opening of pull-down menus, so you could see what the function thinks it wants to do;
but it is truly a can of worms.  Rather than modifying the handlers at each level of the
nested boxes hierarchy, just wipe everything out, then only install a handler
at the top level of the box.  It should fall through to this.
...For some reason, we do seem to still be getting some ringing
from internal boxes, perhaps the text box?.  This shows up as "undefined"--the return routine
is getting called three times, but only once does it have a decent value.
Let it go.  It will only post when the value is good.

Have to make sure that the box is only labeled with a JDocID at the top level,
so it does not post and repost its information.  This would invoke the docs twice.


*/


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
  var IDStamp =0;

  var sessionCounter = null;
  
  

// "TOP LEVEL FUNCTION FOR ADDING ANOTHER BOX." kept for historical reasons.  Never called, I think. See next.
// Make sure to call it with the div of where you want the box instantiated.
//
  function addHelpwindowBox (boxes) 
  {
    var helpwindow = document.getElementById("helpwindow");  
    //attachJbmlBoxes(boxes, helpwindow, "0", false, true);
    attachJbmlBoxes(boxes, helpwindow, IDStamp++); //, false, true);
  }


// THIS WILL BE THE MAIN LEVEL NOW, AS THE INTERMEDIATE BOXES WILL HAVE VARIOUS IDs.

// Second-level master function for adding a box (nested box tree).
  function attachJbmlBoxes (jbml, parentBox, superboxId, JDocID, mysessionID)
{  
  // jbml is a list of instructions coming in the top here.
  // parentBox is the actual box of where we're going to stick it.
  // superboxID is duplicated, it's the id of the parent box, for historical reasons.
  // JDocID is an integer (could be a string, no prob) of the code that will get returned when you click this one.
  //
  // First thing we gotta do is create a single top-level box out of thin air, for the representation.
  // This is still floating, unbound.
  // createBox only does ONE LEVEL and stuffs the id into it.  Other levels come from expandJbmlBoxes, last line.

    var box = Jbml.createBox(jbml.id);
    box.JDocID = JDocID;
    box.onclick = function (e) {
    				new_report_back( JDocID, mysessionID );
     // alert("Bonk Bonk");
      };
    //"alert('bonk');";
    //

//alert("CreateBox returned.");
    // And then it looks like we've got to create a place to stick them.  Figure this out.  
    //   ...this doesn't do anything any more, just returns its first argument. (!)  Hm'mm.
//    var container = Jbml.createBoxContainer(box, parentBox);
    var container =box;

//alert("parentBox.hasChildNodes() =" + parentBox.hasChildNodes() );
//    if (replacedBox) {                   // JKM:  Replacing boxes not supported in Help window.
//      Jbml.replacingChild(parentBox, container, replacedBox);
//    } else 
     if (parentBox.hasChildNodes() && ( /*prepend ||*/ jbml.type === "jbml-main-box-menu")) {
      // Push onto front of superbox, no matter what. Takes care of
      // Open Holes, which have already added an Input box at the
      // beginning. We want Main Menu to come before Input box.
      //alert("menu inserting before:");
	     parentBox.insertBefore(container, parentBox.firstChild);     //JS
	     //alert("inserted.");
	   } else {
//alert("non-menu inserting after:");
      // Else, regular addition; append onto back of list.
      parentBox.appendChild(container);
    }

// and HERE'S where we inflate the subchildren tree.  See the massive third level.
    return expandJbmlBoxes(box, jbml, superboxId);
  }



//OUTPUT GLUE

// version 5:  this is now deprecated.  Use the next one.
function call_popupExpressionInBiobike_with_myID (boxId, mouseCode) 
// Send a message to the server.
// We are going in the front door through REST html, rather than in the back door through Ajax / Json.
// It's a little bit tricky, but the server drops this msg on the floor, 
// then uses its internal session-tracking capabilities 
// to go and pop up the data in the user's completely different session.
// If this were quantum, we would be calling it Action At A Distance.
// Modify a completely different webbrowser, which is entangled up in our completely separate page here.  Why not.
//
// This is quite nasty, but there is apparently some subterrainian code that ensures that boxID
// points to something reasonable.  I think.
// So, perhaps, it might be safe to find its box.
// Then at that point we ask for the JDocID that's hung off the box.
// this is the theory, at least.
// Yes you could return the JDocID into this routine instead of the boxID,
// but it all gets very convoluted, and even harder to maintain than it is already.
{
	var JDocID = document.getElementById(boxId).JDocID;
//alert( "Popup boxID" + JDocID +" and mousecode " +mouseCode );
//      doUpdateOpenBoxContents(  JbmlTracked.getTrackedObjects(document.getElementById(boxId))  );

//      sendJson({ type: "box-click", boxid: boxId, mousecode: mouseCode }, "");
  }
  
function new_report_back(JDocID, sessionID)
{
	if(JDocID)
	{
	  //	alert("New Report Back JDoc=" + JDocID + " for session " + sessionID );
		$.get( "/box-wksp-req?jdocid=" + JDocID + "&PKG=" + sessionID );    //JQuery invocation.  Lots more options if want to pick up results, but we're 1-way.
	}
} 




// Third-level mega-function for adding nested boxes:


  function expandJbmlBoxes (box, jbml, superboxId) 
  {
    var boxId = jbml.id;
    var boxTextJustify = "left";

//    if (clientSettings.get("default-jbml-dnd-drag")) { Jbml.enableDrag(box); }
//    if (clientSettings.get("default-jbml-dnd-drop")) { Jbml.enableDrop(box); }


    function attachIconImg (box, extraClass, width, height, src) 
    {
	//And this looks like JQuery stuff.
      box.appendChild(
        $('<img>')
          .addClass('icon-jbml')
          .addClass(extraClass)
          .width(width)
          .height(height)
          .attr('src', src)
          .get(0));
    }




    function addToBox (child) 
    {

      if (child.type !== "text") {
        processModifiers(box, child.modifiers);
      }

      switch (child.type) {
        // Special content
      case "jbml-cr":           box.appendChild($('<div>').addClass('cr-jbml').get(0)); break;
      case "jbml-dotdotdot":    box.appendChild($('<div>').addClass('a-jbml').addClass('jbml-dotdotdot').text('. . .').get(0)); break;
        
        // ad hoc icons 
      case "jbml-exec-icon":    attachIconImg(box, 'jbml-exec-icon', '22', '22', '/ajax/images/greenarrowright_button_22x22.gif'); break;
      case "jbml-go-icon":      attachIconImg(box, 'jbml-go-icon', '16', '16', '/ajax/images/whitearrowgreen_16x16.gif'); break;
      case "jbml-close-icon":   attachIconImg(box, 'jbml-close-icon', '16', '16', '/ajax/images/redX.jpg'); break;
      case "jbml-menu-icon":    attachIconImg(box, 'jbml-menu-icon', '16', '16', '/ajax/images/greenarrowdown_16x16.gif'); break;
      case "jbml-icon":         attachIconImg(box, 'jbml-icon', '22', '22', '/ajax/images/greenarrowright_button_22x22.gif'); break;
        
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
          input.size = (input.value + " ").length;
	  
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
       // alert("Handling main-box-menu");
          Jbml.disableDrag(box);
       //   alert("DisableDrag finished. Calling CreateJ.");
          createJbmlMainBoxMenu(box, child, superboxId);
       //       alert("done Handling main-box-menu");
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
        }());			// case "text".
        break;

      case "jbml-outdent":
        // NotYetImp -- wasn't in XML version either.
        break;

      case "anonymous":
        // This goes with kludge in boxes-to-json.
        break;
        
      default:
        alert('addToBox: Switch on unexpected child.type: |' + child.type + '| failed, in child: |' + JSON.stringify(child) + '| actually ' + child);
        break;
      }					// switch (child.type)

      addChildren(child.children);
    }    					//addToBox (child)



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
    }				//  processModifiers



    function addChildren (children) {
      if (children) {
        for (var i = 0; i < children.length; i++) {
        //alert("Adding children in loop, iteration#" + (i+1));
          if (children[i].id) {
            // If it has an ID it's a first-class box so we need to go
            // through the whole stack.
            //alert("Calling attachJBMLBoxes on child, with...");
            //alert("boxID = " + boxId + " and box=" + box );
            attachJbmlBoxes(children[i], box, boxId);
          } else {
            // Otherwise (is this only for text children?) we just add
            // to the current box. Which necessitates the special
            // check in addToBox so we don't add a text node's
            // modifiers to the main box. This could be easier to
            // understand.
            //alert("Calling addToBox on simple child."); 
            addToBox(children[i]);
          } 				//else / if
        }  				//for
      }   				//if
    }  				//addChildren()
  
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
//    var menuEntries = menuJson.entries;
    var iconSrc = menuJson.iconSrc || "/ajax/images/greenarrowright_box.gif";

    // Box is now the invisible box that surrounds the menu. Hang
    // everything off of this.
    box.style.borderStyle = "none"; // Make box invisible.
    box.style.background = "inherit";
    box.style.margin = "0px"; // Shrink-wrap box to fit.
//    box.onclick = EventLibrary.stopPropagation;

    // JP 01/28/07 -- Stopped dblclick from causing internal error
//    box.ondblclick = EventLibrary.stopPropagation;
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

//    var entries = Menu.makeContainer();

//    menuIcon.onclick = EventLibrary.stopPropagation;
//    expandJbmlBoxSubmenu(menuJson, Menu.openContainer(entries), superboxId, '');

    var menu = $('<div>')
      .attr('id', menuId)
      .addClass('menu-menu-jbml')
      .addClass('menu')
      .addClass('boxmenu')
      .addClass('menu-centered-text')
      .append(menuIcon)
//      .append(entries)
      .append(menuTitle)
      .get(0);
	  
	//console.log(entries);

    box.appendChild(menu); // Menublock goes inline filled in larger long greater box
//    new Menu(menuId, true).quickinit();
	
/*	
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
	*/
  }

  function createJbmlMainBoxMenu (box, menuJson, superboxId) {

//alert("called createJbmlMainBoxM");
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
//    box.onclick = EventLibrary.stopPropagation;
//    box.ondblclick = EventLibrary.stopPropagation;

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
      .attr('src', '/ajax/images/greenarrowright_button_18x18.gif')
      .css({ 'margin' : '0px', 'margin-right' : '3pt', 'vertical-align' : 'bottom' })
      .get(0);

//    var entries = Menu.makeContainer();

   // menuIcon.onclick = EventLibrary.stopPropagation;  //put this back if it breaks.
   // expandJbmlBoxSubmenu(menuJson, Menu.openContainer(entries), superboxId);  //JKM. Trimmed.

    var menu = $('<div>')
      .attr('id', menuId)
      .addClass('menu-menu-jbml')
      .addClass('menu')
      .addClass('boxmenu')
      .append(menuIcon)
 //     .append(entries)
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
	//new Menu(menuId, true).quickinit();  //JKM. We should not need this...
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






// BACKGROUND CLASSES FOR SUPPORTING JBML
// JbmlBase		boxIDs, Names, Classes, borders, colors.  doBoxMouseClick.  doBoxMouseDoubleClick.
// JbmlBaseStructure	parents, children, lists of boxes inside a box;  Values.
// JbmlBaseDnd      for drag & drop, not Dungeons & Dragons.
// Jbml
// JbmlIconBase (probably)



///////////////////////////////////

//  THESE ARE USED.
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
      return this.getBoxId(EventLibrary.getCurrentTarget(eventThis, e) || node);  // JKM  "EVENT LIBRARY IS BREAKING, NOT THERE!!!
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



	// Apparently supports calling of  doBoxMouseClick.  See below.

    ourClass.prototype.makeSingleClickHandler = function (jsFuncName) { // Protected function
      return function (e) {
        var boxId = ourInstance.getEventBoxId(this, e);
        
        /// alert( "Function arg " + e );   // e is  object mouseEvent.
        
        var button = EventLibrary.getButton(e);                 
        singleClickWrapper(this, function () {
          jsFuncName(boxId, button);
        });
        return EventLibrary.stopPropagation(e);
      }
    };

    // Event handlers
    var singleClickHandler = ourInstance.makeSingleClickHandler( call_popupExpressionInBiobike_with_myID );  //JKM from up top.

    function doubleClickHandler (e) {
      var boxId = ourInstance.getEventBoxId(this, e);
      var button = EventLibrary.getButton(e);
      clearMouseDelayTimer(this);
      doBoxMouseDoubleClick(boxId, button);
      return EventLibrary.stopPropagation(e);
    }

// FIX ME
    function contextmenuHandler (e) {
      var boxId = ourInstance.getEventBoxId(this, e);
      var button = EventLibrary.rightMouseButton;   // JKM
     // doBoxMouseClick(boxId, button);   //JKM trimmed for Help boxes display.  Don't do anything.
      lastMouseX = EventLibrary.getPageX(e);
      lastMouseY = EventLibrary.getPageY(e);
      return EventLibrary.stopPropagation(e);
    }

// FIX ME
var foobar = 1369;

    function mousedownHandler (e) {
      if ((EventLibrary.rightMouseButton === EventLibrary.getButton(e)) || DomLibrary.hasClass(this, "hilight-jbml")) {
        /* This was not returned in the lispscript but it seems it should be. -PBS */
        EventLibrary.stopPropagation(e);
      }
      return true;
    }

    ourClass.prototype.createBox = function (boxId) {
      var className = ("a-jbml box-jbml " + ourDefaultBorderStyle + " " + ourDefaultBorderWidth);
      //alert( "CreateBox( |" + className + "| , " + boxId + " )" );
//      var box = $('<div>').addClass(className).attr('id', boxId).get(0);
      var box = $('<div>');
      box = box.addClass(className);
      if(boxId==undefined) boxId = foobar++;   //WHY IS THIS UNDEFINED?  JKM
      box = box.attr('id', boxId);
      box = box.get(0);
      
      
      
      
      
      
      
      
      this.init(box, boxId);
      box.style.color = globalDefaultTextColor;
//      box.onclick = singleClickHandler;
//      box.ondblclick = singleClickHandler;  //doubleClickHandler;
//      box.oncontextmenu = contextmenuHandler;		// FIX ME
//      box.onmousedown = mousedownHandler;		// FIX ME
      return box;
    };

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


///////////////////////////////////

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

///////////////////////////////////

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
    var ourDrag = {};  //new Drag();    //Save this for next year, if you get really fancy.
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

    // Begins dragging after a drag-click-delay. 
    // Closes any open menus, otherwise their event handlers would fire during DND.  [deprecated]
    // Creates a clone of the dragged item and attaches it to the
    // "drag-proxy" layer. Adds the "dragging" CSS class to the
    // document root. Adds an event handler for selectstart to prevent
    // IE from selecting text during DND. Starts the dragging.
    // However, it creates thet dragged object immediately.
    function startDrag (boxId, e) {

	alert( "Dragging and dropping not supported for the Help window.  Click on the box instead.");
	return;

      clearElement("drag-proxy");
//      Menu.closeMenus();		// JKM
      var box = document.getElementById(boxId);
//      var proxy = document.getElementById("drag-proxy");
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
                           showClientStatus("Dragging " + ourInstance.getBoxName(box));
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
      EventLibrary.addEventListener(box, "mousedown", dragMousedownHandler);
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



///////////////////////////////////


 var Jbml = (function () {
    // Class constructor
    function ourClass () {
      return ("Jbml" && this);
    }
    ourClass.prototype = new JbmlBaseDnd.constructor(); // Set parent class
    var ourInstance = new ourClass(); // Class singleton instance

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());


///////////////////////////////////

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
      call_popupExpressionInBiobike_with_myID();     //Kill this if it causes trouble.  "Should never be called."
      alert("Override Jbml-icon-base.single-click-handler"
      );
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
//      icon.onclick = this.singleClickHandler;
      box.appendChild(icon);
      return icon;
    };

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());
// end of JbmlIconBase declaration.

///////////////////////////////////

  var JbmlIconDelete = (function () {
    // Class constructor
    function ourClass () {
      return ("Jbml-icon-delete" && this);
    }
    ourClass.prototype = new JbmlIconBase.constructor(); // Set parent class
    var ourInstance = new ourClass(); // Class singleton instance
    var className = (ourInstance.getIconClassName() + " jbml-delete");
    ourClass.prototype.getIconSrc = function () {
      return "/ajax/images/redXhandlebox.gif";
    };
    ourClass.prototype.getIconClassName = function () {
      return className;
    };
    ourClass.prototype.singleClickHandler = ourInstance.makeSingleClickHandler( call_popupExpressionInBiobike_with_myID );  //JKM

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
      return "/ajax/images/whitemauveXhandlebox.gif";
    };
    ourClass.prototype.getIconClassName = function () {
      return className;
    };
    ourClass.prototype.singleClickHandler = ourInstance.makeSingleClickHandler( call_popupExpressionInBiobike_with_myID ); //JKM

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
    ourClass.prototype.singleClickHandler = ourInstance.makeSingleClickHandler( call_popupExpressionInBiobike_with_myID ); //JKM

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
      return "/ajax/images/whiteXhandlebox.gif";
    };
    ourClass.prototype.getIconClassName = function () {
      return className;
    };

    ourClass.prototype.singleClickHandler = ourInstance.makeSingleClickHandler( call_popupExpressionInBiobike_with_myID ); //JKM

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());
// end JbmlIconClearDelete declaration.


//////////////////////////////////
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

    function doubleClickHandler (e) {
      var boxId = ourInstance.getEventBoxId(this, e);
      var button = EventLibrary.getButton(e);
      clearMouseDelayTimer(this);
      doBoxMouseDoubleClick(boxId, button);
      return EventLibrary.stopPropagation(e);
    }

    var openHoleClickHandler = ourInstance.makeSingleClickHandler(  call_popupExpressionInBiobike_with_myID ); //doBoxMouseClick);

    var closedHoleClickHandler = ourInstance.makeSingleClickHandler(  call_popupExpressionInBiobike_with_myID ); //doClosedHoleMouseClick);

    ourClass.prototype.initHole = function (box) {
   //alert("initHole");
      this.init(box, this.getBoxId(box));
      this.setBorderStyle(box, "jbml-dotted");
//     box.ondblclick = doubleClickHandler;
//      box.onmouseover = holeMouseoverHandler;
//      box.onmouseout = holeMouseoutHandler;
      // NOTE: A hidden box is no longer used
//      this.enableDrop(box);
//      this.disableDrag(box);
    };

    // Turns a hole into an open hole
    ourClass.prototype.open = function (hole, value) {
      removeInputAndVisitBoxtext(hole, this.getInputType(), DomLibrary.hide);
//      hole.onclick = openHoleClickHandler;
//      hole.ondblclick = EventLibrary.stopPropagation;
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
    //alert("Turn hole Closed.");
      removeInputAndVisitBoxtext(hole, this.getInputType(), DomLibrary.show);
      //alert("Hole now closed.");
 //     hole.onclick = closedHoleClickHandler;
 //     hole.ondblclick = doubleClickHandler;
    };

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());

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

    // Event handlers
    function inputKeypressHandler (e) {
      var input = EventLibrary.getCurrentTarget(this, e);
      var boxId = ourInstance.getBoxId(input);
      // TODO: TAB, ENTER and ESC are supposedly not passed by IE, we might need a keyup/down handler instead...
      switch (EventLibrary.getKeypressCharCode(e)) {
        /* Seems like maybe these first two cases should return true or false. -PBS */
      case 9: // TAB
        return doInputTextTab(boxId, input.value);
      case 13: // ENTER
        return doInputText(boxId, input.value);
      case 27: // ESC
        doBoxClearHoleMouseClick(boxId, EventLibrary.leftMouseButton);
        return EventLibrary.stopPropagation(e);
      default:
        return true;
      }
    }

    ourClass.prototype.createOpenHoleInput = function (box, value) {
      var input = $('<input>')
        .attr("type", "text")
        .attr("name", "Text Input box")
        .attr("size", "5")
        .attr("value", value)
        .get(0);

      this.init(input, this.getBoxId(box));
      input.onkeypress = inputKeypressHandler;
      box.appendChild(input);
      return input;
    };

    // Using prototype inheritance with static methods, returns a singleton
    ourInstance.constructor = ourClass;
    return ourInstance;

  }());

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

    ourClass.prototype.createOpenHoleInput = function (box, value) {

      var that = this;

      function makeButton (label, onclick, boxId) {
        var button = $('<input>')
          .attr('type', 'button')
          .attr('value', label)
//          .click(onclick)
          .css('width', '45px')
          .css('height', '23px')
          .css('font-size', '70%')
          .get(0);
        
        
//        button.ondblclick = EventLibrary.stopPropagation;  //JKM want it to feed thru to base.
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




// PositionCache elided.


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

 // About 15 more elided.  Get if you need them.  JKM
 
 
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
    clickDelay = (clickDelay  );   // || clientSettings.get("single-click-delay"));
    clearMouseDelayTimer(node);
    setMouseDelayTimer(node, setTimeout(augmentedCallback, clickDelay));
  }


