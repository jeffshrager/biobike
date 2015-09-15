/*
 * DO NOT REMOVE THIS NOTICE
 *
 * PROJECT:   mygosuMenu
 * VERSION:   1.1.6
 * COPYRIGHT: (c) 2003,2004 Cezary Tomczak
   extensions copyright 2006 DragonLord Enterprises, Inc.
 * LINK:      http://gosu.pl/dhtml/mygosumenu.html
 * LICENSE:   BSD (revised)
 */

/*
Nov 20 '06  JKM  Messed with getting index counts to work, failure.
            Some nodes are not menu-entries.  The table goes down three levels before
            starting, but you don't want to recurse.  All subsections have a submenu title
            in front of them, which should take a increment, but the subsection has the same
            count as the title, so you have to temporarily decrement it.
            However, the top level section has no decrement in front of it.
Nov 15 '06  JKM: Note:  self = this inside ddmx, local variable.
Oct 18 '06  JKM:  Take absolute position through .offsetLeft and .offsetTop.
            Try recursing through .offsetParent.
	    It's necessary to have the first "if" there, since obj changes.

	    Put in checks for off edge of screen on right, bottom,
	    for 1 and ongoing 2nd level menus.


  Todo, bugs to fix:
  - delay.show = 400 , delay.hide = 400
    go Product Three -> Live Demo -> Test Drive -> Test Three , go fast to Product Four.
    Result: 2 elements highlighted in the same section
  - delay.show = 0 , delay.hide = 400
    go Product Three -> Live Demo , section out , section over, seciont out.
    Result: Live Demo is not highlighted
  - active className changing, unnecessary blink
  - opera: hideSection() exceptions are throwed
*/

function DropDownMenuX(id) {

    /* Type of the menu: "horizontal" or "vertical" */
  this.type = "horizontal";  // Zeroth level of menu is spread horizontally:  AAA BBB CCC.

    /* Delay (in miliseconds >= 0): show-hide menu
     * Hide must be > 0 */
    this.delay = {
        "show": 0,
        "hide": 180
    }
    /* Change the default position of sub-menu by Y pixels from top and X pixels from left
     * Negative values are allowed */
    this.position = {
        "level1": { "top": 0, "left": 0},
        "levelX": { "top": 0, "left": 2, "right": 2}  //JKM
    }

    /* fix ie selectbox bug ? */
    this.fixIeSelectBoxBug = true;

    /* Z-index property for .section */
    this.zIndex = {
        "visible": 500,
        "hidden": -1
    };

    // Browser detection
    this.browser = {
        "ie": Boolean(document.body.currentStyle),
        "ie5": (navigator.appVersion.indexOf("MSIE 5.5") != -1 || navigator.appVersion.indexOf("MSIE 5.0") != -1),
        "ie6": (navigator.appVersion.indexOf("MSIE 6.0") != -1)
    };
    
    if (!this.browser.ie) {
        this.browser.ie5 = false;
        this.browser.ie6 = false;
    }

    /* Initialize the menu */
    //NOTE THIS IS CALLED WITH A DDMX OBJECT, NOT A MENUBLOCK. MENUID IS USED TO LINK THE TWO TOGETHER.
    this.init = function() {
        if (!document.getElementById(this.id)) { return alert("Msg 2: DropDownMenuX.init() real init() failed. Element '"+ this.id +"' does not exist."); }
        if (this.type != "horizontal" && this.type != "vertical") { return alert("DropDownMenuX.init() failed. Unknown menu type: '"+this.type+"'"); }
        if (this.browser.ie && this.browser.ie5) { fixWrap(); }

	//	alert("Init object:" + this + " tree: " + this.tree);
	//dumpMsec("Inside .init.");
	//	 fixSections();  //FIRST WE SET THE WIDTH OF THE SECTIONS.  IMPORTANT.  ..."THIS" is **MASTER WINDOW***
	 fixSections(this.id);  //FIRST WE SET THE WIDTH OF THE SECTIONS.  IMPORTANT.  ..."THIS" is **MASTER WINDOW***
	// *VERY* SUBTLE BUG.  no, this.fixsections didn't work, don't know why.
        //this.fixSections();  //FIRST WE SET THE WIDTH OF THE SECTIONS.  IMPORTANT.  ..."THIS" is **MASTER WINDOW***
	//dumpMsec("Finished fixSections.");
        parse(document.getElementById(this.id).childNodes, this.tree, this.id);  //*THEN* WE SET THE POSITIONS.

	//dumpMsec("Finished Parse.");
    }


    /* Initialize the menu */
    //"this" is a brand new DropDownMenuX object with mymenuID.  It is random & has nothing to do with 
    //themenu, which is a div class "section" beside themenutitle, an A class "item1".
    //mymenuID refers to themenublock, a table or div that contains tablerow->tablecell->themenutitle.
    this.quickinit = function() {
      //So we use the id to go get the actual menublock, not the "this" ddmx controlling object.
        if (!document.getElementById(this.id)) { return alert("Msg 3: DropDownMenuX.quickinit() quick version failed. Element '"+ this.id +"' does not exist."); }
        if (this.type != "horizontal" && this.type != "vertical") { return alert("DropDownMenuX.init() failed. Unknown menu type: '"+this.type+"'"); }
        if (this.browser.ie && this.browser.ie5) { fixWrap(); }

	//dumpMsec("Inside .init.");
	//       fixSections();  //FIRST WE SET THE WIDTH OF THE SECTIONS.  IMPORTANT.
	//dumpMsec("Finished fixSections.");
	//	alert("Length of outside tree=" + this.tree.length);
	quickparse(document.getElementById(this.id).childNodes, this.treeq, this.id, this);  //Handle only mouseover.
	//	alert("Resulting treeq: " + this.treeq );
	//	alert( "self.sections="+ self.sections + " secShowCnt=" + self.sectionsShowCnt + " Hide=" + self.sectionsHideCnt );

	//dumpMsec("Finished Parse.");
    }



    /* Search for ALL .section elements and set width for them */
    function fixSections(myID) {
      //      var arr = document.getElementById(self.id).getElementsByTagName("div");  //get ALL divs under this menublock. Sigh.
      var arr = document.getElementById(myID).getElementsByTagName("div");  //get ALL divs under this menublock. Sigh.
        var sections = new Array();
        var widths = new Array();
	//Somehow self is an Object, id comes through correctly.  ...Self is local variable bound to this!
	//	alert("Init FIXSECTIONS self "+ self + " id " + self.id + " this=" + this + " this.id=" + this.id);
        
	//dumpMsec("fix: got subelements div.");

        for (var i = 0; i < arr.length; i++) {          //Grab all "section" menucards, including submenus-cards.
            if (arr[i].className == "section") {
                sections.push(arr[i]);
            }
        }
	//dumpMsec("fix: Resizing " + arr.length + " divs to give " + sections.length + " sections.");
	//	alert( "Resizing " + arr.length + " divs to give " + sections.length + " sections.");

        for (var i = 0; i < sections.length; i++) {            //For each section, process its entries.
            widths.push(getMaxWidth(sections[i].childNodes));
        }
	//dumpMsec("fix: pushed widths.");

        for (var i = 0; i < sections.length; i++) {
            sections[i].style.width = (widths[i]) + "px";
        }
	//dumpMsec("fix: set width times " + sections.length + ". ");

        if (self.browser.ie) {
            for (var i = 0; i < sections.length; i++) {
                setMaxWidth(sections[i].childNodes, widths[i]);
            }
        }
    }



    function fixWrap() {
        var elements = document.getElementById(self.id).getElementsByTagName("a");
        for (var i = 0; i < elements.length; i++) {
            if (/item2/.test(elements[i].className)) {
                elements[i].innerHTML = '<div nowrap="nowrap">'+elements[i].innerHTML+'</div>';
            }
        }
    }

    /* Search for an element with highest width among given nodes, return that width */
    function getMaxWidth(nodes) {
      //dumpMsec("start of getMaxWidth with " + nodes.length + " nodes in loop.");
        var maxWidth = 0;
        for (var i = 0; i < nodes.length; i++) {
	  //	  dumpMsec("start.");
	  //	  dumpMsec("testing '" + nodes[i].className + "' type:" + nodes[i].nodeType + " ");
	  //	  dumpMsec("results '" + /section/.test(nodes[i].className) + "'");
	  //	  dumpMsec("!");
	  //	  dumpMsec("node.offsewith " + nodes[i].offsetWidth );   // 70 msec first time.  WTF.
	  //	  dumpMsec("!");
            if (nodes[i].nodeType != 1 || /section/.test(nodes[i].className)) { continue; }
	    //	    dumpMsec(" end 1st.");
            if (nodes[i].offsetWidth > maxWidth) { maxWidth = nodes[i].offsetWidth; }
	    //	    dumpMsec(" end 2nd.");
        }
	//dumpMsec("end of getMaxWidth.");
        return maxWidth;
    }

    /* Set width for item2 elements */
    function setMaxWidth(nodes, maxWidth) {
        for (var i = 0; i < nodes.length; i++) {
            if (nodes[i].nodeType == 1 && /item2/.test(nodes[i].className) && nodes[i].currentStyle) {
                if (self.browser.ie5) {
                    nodes[i].style.width = (maxWidth) + "px";
                } else {
                    nodes[i].style.width = (maxWidth - parseInt(nodes[i].currentStyle.paddingLeft) - parseInt(nodes[i].currentStyle.paddingRight)) + "px";
                }
            }
        }
    }

    /* Parse nodes, create events, position elements */
    //Called with  parse(document.getElementById(this.id).childNodes, this.tree, this.id); 
    // id is magic number; childnodes is TableRow under menublock; this is random raw ddmx obj; this.tree is ?; menuID#.
    // Has to eat objects recursively until it gets to an item1.


    function parse(nodes, tree, id) {
      var oldleft;
      var oldtop;
      //                  alert("Parse full self="+self+" id "+self.id+" called with perhaps subnodes " + nodes.length);

        for (var i = 0; i < nodes.length; i++) {
            if (1 != nodes[i].nodeType) {
                continue;
            }
            switch (true) {
                // .item1
                case /\bitem1\b/.test(nodes[i].className):
		  //                    nodes[i].id = id + "-" + tree.length;
                    tree.push(new Array());
		  //		nodes[i].onmouseover = quickitemOver;  //required to not shoot yourself once established.
		// nodes[i].onmouseover = itemOver;  //required to not shoot yourself once established.
                    nodes[i].onmouseout = itemOut;
                    break;

                // .item2
                case /\bitem2\b/.test(nodes[i].className):
		  //                    nodes[i].id = id + "-" + tree.length;
                    tree.push(new Array());
                    nodes[i].onmouseover = itemOver;
                    nodes[i].onmouseout = itemOut;
                    break;


                // .section
                case /\bsection\b/.test(nodes[i].className):
                    // id, events
		  //                    nodes[i].id = id + "-" + (tree.length - 1) + "-section";
                    nodes[i].onmouseover = sectionOver;
                    nodes[i].onmouseout = sectionOut;
                    // position
		    var box1ID = id + "-" + (tree.length - 1);
        	    var box1 = document.getElementById( box1ID );
		    if( !box1 )
		      { alert("Error!  Unable to find box ID " + box1ID + " at menu top, menu broken!");
			break;
		      }
                    var box2 = document.getElementById(nodes[i].id);
                    var el = new Element(box1ID);
		    //Are we pulling down the FIRST LEVEL?
                    if (1 == el.level) {
		      // Zeroth level of menu is spread horizontally:  AAA BBB CCC  ?
                        if ("horizontal" == self.type) {
			  //In this case for a HORIZONTAL menu, the Level 1 submenus are stacked VERTICALLY
			  //so that the LEFT is the same, and the TOP comes under the previous BOTTOM.
			  		  
			  oldtop = findAbsoluteTop(box1);
			  if(oldtop + self.position.levelX.top + box2.offsetHeight + 5	 < window.innerHeight)
			    {
				   //Regular style.  Stick it down.
			     box2.style.top = box1.offsetTop + box1.offsetHeight + self.position.level1.top + "px";
			     }
			    else
			    {
			      //Backwards style.  Stick it up.
			      box2.style.top = box1.offsetTop - self.position.level1.top // + box1.offsetHeight
				- box2.offsetHeight + "px";

			    }


			    oldleft = findAbsoluteLeft(box1);

                            if (self.browser.ie5) {
                                box2.style.left = self.position.level1.left + "px";
                            } else {
			      //  alert( "Oldleft " + oldleft + " sum " + 
			      //    (oldleft +  box1.offsetWidth + self.position.levelX.left + box2.offsetWidth)
			      //     + " browser=" + window.innerWidth );

			       if(oldleft + self.position.levelX.left + box2.offsetWidth + 5
			      			 < window.innerWidth)
			      //if(false ) //oldleft <  window.innerWidth / 2.0)
				 {
				   //Regular style.  Stick it towards the right.
				   box2.style.left = box1.offsetLeft + self.position.level1.left + "px";
				 }
		      else
			{
			  //Backwards style.  Stick it towards the left.
			   box2.style.left = box1.offsetLeft  + box1.offsetWidth
			     - box2.offsetWidth + "px";

			}







                            }

                        } else if ("vertical" == self.type) {
                            box2.style.top = box1.offsetTop + self.position.level1.top + "px";
                            if (self.browser.ie5) {
                                box2.style.left = box1.offsetWidth + self.position.level1.left + "px";
                            } else {
                                box2.style.left = box1.offsetLeft + box1.offsetWidth + self.position.level1.left + "px";
                            }
                        }
                    } 
		    else 
		    {
		      //Now we're doing the SECOND AND SUCCEEDING LEVELS.  In either case, Level 2, 3, 4 menus
		      //flow off the previous one to the RIGHT, at the SAME HEIGHT.
		      //window.innerWidth is magic for width of window, works in "most" browsers.
		      //sections[i].style.width   offsetWidth

		      // alert("Msg 4: DropDownMenuX.init() failed. Element '"+ this.id +"' does not exist."); }


		      			  oldtop = findAbsoluteTop(box1);
		         if(oldtop + box1.offsetTop + self.position.levelX.top + box2.offsetHeight + 5 
			    < window.innerHeight)
			   {
				   //Regular style.  Stick it down.
			     box2.style.top = box1.offsetTop + self.position.levelX.top + "px";
			          }
			     			    else
			     			    {
			     ///Backwards style.  Stick it up.
			     			      box2.style.top = box1.offsetTop + box1.offsetHeight - self.position.level1.top
			     				- box2.offsetHeight + "px";
		     	    }



		      
		      //We have to check the RIGHT edge of the NEW position in order to set the LEFT edge.
		      //alert( "b1offLeft: " + box1.offsetLeft + " offWidth: " + box1.offsetWidth + " levX: " + 
		      //	     self.position.levelX.left + " b2offWidth: " + box2.offsetWidth + " < winWidth: " +
		      //	     window.innerWidth );
		      // box1.offsetLeft:            0.
		      // box1.offsetWidth:           156-235, width of level 1 box
		      // self.position.levelX.left:  2.  Sorry.
		      // box2.offsetWidth:           around 100, 41 to 169.  Width of level 2 box.
		      // window.innerWidth:          around 754, 430, seems accurate.
		      //
		      oldleft = findAbsoluteLeft(box1);

		      if(oldleft +  box1.offsetWidth + self.position.levelX.left + box2.offsetWidth
			 < window.innerWidth)
			{
			  //Regular style.  Stick it on the right.
                        box2.style.left = box1.offsetLeft + box1.offsetWidth + self.position.levelX.left + "px";
			}
		      else
			{
			  //Backwards style.  Stick it on the left.
			   box2.style.left = box1.offsetLeft - box2.offsetWidth - self.position.levelX.left + "px";

			}

                    }


                    // sections, sectionsShowCnt, sectionsHideCnt
		    /*  Can/t have in PARSE now, moved to QUICKPARSE.  Stateful info stomps previously in use.

		    */		    alert("parse self=" + self + " sections=" + self.sections + "| id=>> " + nodes[i].id + " <<");
		    /*
                    self.sections.push(nodes[i].id);
                    self.sectionsShowCnt.push(0);
                    self.sectionsHideCnt.push(0);
                    if (self.fixIeSelectBoxBug && self.browser.ie6) 
		    {   nodes[i].innerHTML = nodes[i].innerHTML + '<iframe id="'+nodes[i].id+'-iframe" src="javascript:false;" scrolling="no" frameborder="0" style="position: absolute; top: 0px; left: 0px; display: none; filter:alpha(opacity=0);"></iframe>';
                    }
		    **/  //End of trim out.
                    break;


            }
            if (nodes[i].childNodes) {
                if (/\bsection\b/.test(nodes[i].className)) {
                    parse(nodes[i].childNodes, tree[tree.length - 1], id + "-" + (tree.length - 1));
                } else {
                    parse(nodes[i].childNodes, tree, id);
                }
            }
        }
    }//end of PARSE.



    function parse3(nodes, tree, id) {
      var oldleft;
      var oldtop;
      //                  alert("Parse3 full self="+self+" id "+self.id+" called with perhaps subnodes " + nodes.length);
      // alert("Parse3 lite(N,tL="+tree.length+", "+id+") self="+self+"  self.id="+self.id+"    class="+self.class+" this="+this+" called with subnodes# " + nodes.length);

        for (var i = 0; i < nodes.length; i++) {
            if (1 != nodes[i].nodeType) {
                continue;
            }
            switch (true) {
                // .item1
                case /\bitem1\b/.test(nodes[i].className):
		//                    nodes[i].id = id + "-" + tree.length;
                    tree.push(new Array());
		//		nodes[i].onmouseover = quickitemOver;  //required to not shoot yourself once established.
		// nodes[i].onmouseover = itemOver;  //required to not shoot yourself once established.
                //    nodes[i].onmouseout = itemOut;
                    break;

                // .item2
                case /\bitem2\b/.test(nodes[i].className):
		  //   nodes[i].id = id + "-" + tree.length;
                    tree.push(new Array());
                  //  nodes[i].onmouseover = itemOver;
                  //  nodes[i].onmouseout = itemOut;
                    break;


                // .section
                case /\bsection\b/.test(nodes[i].className):
                    // id, events
		  //  nodes[i].id = id + "-" + (tree.length - 1) + "-section";
                  //nodes[i].onmouseover = sectionOver;
                  //nodes[i].onmouseout = sectionOut;
                    // position
		    var box1ID = id + "-" + (tree.length - 1);
        	    var box1 = document.getElementById( box1ID );
		    if( !box1 )
		      { alert("Error!  Unable to find box ID " + box1ID + " at menu top, menu broken!");
			break;
		      }
                    var box2 = document.getElementById(nodes[i].id);
                    var el = new Element(box1ID);
		    //Are we pulling down the FIRST LEVEL?
                    if (1 == el.level) {
		      // Zeroth level of menu is spread horizontally:  AAA BBB CCC  ?
                        if ("horizontal" == self.type) {
			  //In this case for a HORIZONTAL menu, the Level 1 submenus are stacked VERTICALLY
			  //so that the LEFT is the same, and the TOP comes under the previous BOTTOM.
			  		  
			  oldtop = findAbsoluteTop(box1);
			  if(oldtop + self.position.levelX.top + box2.offsetHeight + 5	 < window.innerHeight)
			    {
				   //Regular style.  Stick it down.
			     box2.style.top = box1.offsetTop + box1.offsetHeight + self.position.level1.top + "px";
			     }
			    else
			    {
			      //Backwards style.  Stick it up.
			      box2.style.top = box1.offsetTop - self.position.level1.top // + box1.offsetHeight
				- box2.offsetHeight + "px";

			    }


			    oldleft = findAbsoluteLeft(box1);

                            if (self.browser.ie5) {
                                box2.style.left = self.position.level1.left + "px";
                            } else {
			      //  alert( "Oldleft " + oldleft + " sum " + 
			      //    (oldleft +  box1.offsetWidth + self.position.levelX.left + box2.offsetWidth)
			      //     + " browser=" + window.innerWidth );

			       if(oldleft + self.position.levelX.left + box2.offsetWidth + 5
			      			 < window.innerWidth)
			      //if(false ) //oldleft <  window.innerWidth / 2.0)
				 {
				   //Regular style.  Stick it towards the right.
				   box2.style.left = box1.offsetLeft + self.position.level1.left + "px";
				 }
		      else
			{
			  //Backwards style.  Stick it towards the left.
			   box2.style.left = box1.offsetLeft  + box1.offsetWidth
			     - box2.offsetWidth + "px";

			}







                            }

                        } else if ("vertical" == self.type) {
                            box2.style.top = box1.offsetTop + self.position.level1.top + "px";
                            if (self.browser.ie5) {
                                box2.style.left = box1.offsetWidth + self.position.level1.left + "px";
                            } else {
                                box2.style.left = box1.offsetLeft + box1.offsetWidth + self.position.level1.left + "px";
                            }
                        }
                    } 
		    else 
		    {
		      //Now we're doing the SECOND AND SUCCEEDING LEVELS.  In either case, Level 2, 3, 4 menus
		      //flow off the previous one to the RIGHT, at the SAME HEIGHT.
		      //window.innerWidth is magic for width of window, works in "most" browsers.
		      //sections[i].style.width   offsetWidth

		      // alert("Msg 5: DropDownMenuX.init() failed. Element '"+ this.id +"' does not exist."); }


		      			  oldtop = findAbsoluteTop(box1);
		         if(oldtop + box1.offsetTop + self.position.levelX.top + box2.offsetHeight + 5 
			    < window.innerHeight)
			   {
				   //Regular style.  Stick it down.
			     box2.style.top = box1.offsetTop + self.position.levelX.top + "px";
			          }
			     			    else
			     			    {
			     ///Backwards style.  Stick it up.
			     			      box2.style.top = box1.offsetTop + box1.offsetHeight - self.position.level1.top
			     				- box2.offsetHeight + "px";
		     	    }



		      
		      //We have to check the RIGHT edge of the NEW position in order to set the LEFT edge.
		      //alert( "b1offLeft: " + box1.offsetLeft + " offWidth: " + box1.offsetWidth + " levX: " + 
		      //	     self.position.levelX.left + " b2offWidth: " + box2.offsetWidth + " < winWidth: " +
		      //	     window.innerWidth );
		      // box1.offsetLeft:            0.
		      // box1.offsetWidth:           156-235, width of level 1 box
		      // self.position.levelX.left:  2.  Sorry.
		      // box2.offsetWidth:           around 100, 41 to 169.  Width of level 2 box.
		      // window.innerWidth:          around 754, 430, seems accurate.
		      //
		      oldleft = findAbsoluteLeft(box1);

		      if(oldleft +  box1.offsetWidth + self.position.levelX.left + box2.offsetWidth
			 < window.innerWidth)
			{
			  //Regular style.  Stick it on the right.
                        box2.style.left = box1.offsetLeft + box1.offsetWidth + self.position.levelX.left + "px";
			}
		      else
			{
			  //Backwards style.  Stick it on the left.
			   box2.style.left = box1.offsetLeft - box2.offsetWidth - self.position.levelX.left + "px";

			}

                    }


                    // sections, sectionsShowCnt, sectionsHideCnt
		    /*  Can/t have in PARSE now, moved to QUICKPARSE.  Stateful info stomps previously in use.

		    */		    
		    //alert("parse3 self=" + self + " sections=" + self.sections + "| id=>> " + nodes[i].id + " <<");
		    /*
                   // self.sections.push(nodes[i].id);
                   // self.sectionsShowCnt.push(0);
                   // self.sectionsHideCnt.push(0);
                   // if (self.fixIeSelectBoxBug && self.browser.ie6) 
		   // {   nodes[i].innerHTML = nodes[i].innerHTML + '<iframe id="'+nodes[i].id+'-iframe" src="javascript:false;" scrolling="no" frameborder="0" style="position: absolute; top: 0px; left: 0px; display: none; filter:alpha(opacity=0);"></iframe>';
                   // }
		    **/  //End of trim out.
                    break;


            }
            if (nodes[i].childNodes) {
                if (/\bsection\b/.test(nodes[i].className)) {
                    parse3(nodes[i].childNodes, tree[tree.length - 1], id + "-" + (tree.length - 1));
                } else {
                    parse3(nodes[i].childNodes, tree, id);
                }
            }
        }
    }//end of PARSE3.







    /* Parse nodes, create events, position elements */
    //Called with  parse(document.getElementById(this.id).childNodes, this.tree, this.id); 
    // id is magic number; childnodes is TableRow under menublock; this is random raw ddmx obj; this.tree is ?; menuID#.
    // Has to eat objects recursively until it gets to an item1.
    function quickparse(nodes, treeq, id, my_ddmx) {
      var oldleft;
      var oldtop;
      //Thre are a bunch of nodes that could be NOT menus.
      //The tree thing solves this with advancing only when needed...
      //	alert("Length of inside tree=" + treeq.length);
      //            alert("QuickParse self="+self+" called with perhaps subnodes " + nodes.length);
      //	alert("QuickParse " + nodes.length + " nodes.");
        for (var i = 0; i < nodes.length; i++) {
	  //	  alert("nodeType "+ nodes[i].nodeType + " classname " + nodes[i].className);
            if (1 != nodes[i].nodeType) {
                continue;
            }
            switch (true) {
                // .item1
                case /\bitem1\b/.test(nodes[i].className):
		    // alert("Got item1.");
		    nodes[i].id = id + "-" + treeq.length;
		    treeq.push(new Array());

                    nodes[i].onmouseover = quickitemOver;
                    nodes[i].myddmx = my_ddmx;
		    // alert("saving ++" + my_ddmx + "++");
                    nodes[i].onmouseout = itemOut;
		    // return;
                    break;

                // .item2
                case /\bitem2\b/.test(nodes[i].className):
                    nodes[i].id = id + "-" + treeq.length;
                    treeq.push(new Array());
                    nodes[i].onmouseover = itemOver;
                    nodes[i].myddmx = my_ddmx;  //prob not needed.
		    // alert("saving ++" + my_ddmx + "++");
                    nodes[i].onmouseout = itemOut;

                    break;

                // .section
                case /\bsection\b/.test(nodes[i].className):
                    // id, events
                    nodes[i].id = id + "-" + (treeq.length - 1) + "-section";
                    nodes[i].onmouseover = sectionOver;
                    nodes[i].onmouseout = sectionOut;
		    // ...stuff deleted, not needed for quick...
		    //
		    //
		    //  ...state information for initialization of menu sections.
                    // sections, sectionsShowCnt, sectionsHideCnt
		  //	  alert("self=" + self + " sections=" + self.sections);
 //		  alert("quickparse self=" + self + " sections=" + self.sections + "| id=>> " + nodes[i].id + " <<");
                    self.sections.push(nodes[i].id);
                    self.sectionsShowCnt.push(0);
                    self.sectionsHideCnt.push(0);
                    if (self.fixIeSelectBoxBug && self.browser.ie6) 
		    {   nodes[i].innerHTML = nodes[i].innerHTML + '<iframe id="'+nodes[i].id+'-iframe" src="javascript:false;" scrolling="no" frameborder="0" style="position: absolute; top: 0px; left: 0px; display: none; filter:alpha(opacity=0);"></iframe>';
                    }
                    break;
            }
	    //	    	    alert("children " + nodes[i].childNodes);
            if (nodes[i].childNodes) {
                if (/\bsection\b/.test(nodes[i].className)) {
		  quickparse(nodes[i].childNodes, treeq[treeq.length - 1], id + "-" + (treeq.length - 1), my_ddmx);
                } else {
		  quickparse(nodes[i].childNodes, treeq, id, my_ddmx);
                }
            }
        }
    }//end of QUICKPARSE.







    /* event, item:onmouseover */
    //NOTE "THIS" IS DDMX, *NOT* MENUBLOCK.  HAHAHA.   <<UNCLEAR, MAY BE REVERSE!!
    function quickitemOver() {

      //      alert("quickitemOver("+this.id+") , visible = " + self.visible);

      //this.init();   //didn't work
      // .init();
      // init(); 
      // screw it

      //init function contents:
      var myddmx = this.myddmx;      //THIS NOT SELF. SELF MEANS THE TOPLEVEL WINDOW in this outside context.
      //      alert("recall++" + this.myddmx + "++ id " + myddmx.id);

      //JKM This error msg was offending and now is disabled.
      if (!document.getElementById(this.myddmx.id)) { return; alert("Msg 1: menu's quickitemOver() failed to find menu ID. Element '"+ this.myddmx.id +"' does not exist."); }
        if (myddmx.type != "horizontal" && myddmx.type != "vertical") { return alert("DropDownMenuX.init() failed. Unknown menu type: '"+myddmx.type+"' neither horizontal nor vertical."); }
        if (myddmx.browser.ie && myddmx.browser.ie5) { myddmx.fixWrap(); }

	//dumpMsec("Inside .init.");
	//  XXX      myddmx.fixSections(myddmx.id);  //FIRST WE SET THE WIDTH OF THE SECTIONS.  IMPORTANT.
        fixSections(myddmx.id);  //FIRST WE SET THE WIDTH OF THE SECTIONS.  IMPORTANT.  can't call with myddmx.fix.
	//dumpMsec("Finished fixSections.");
	myddmx.tree = [];  //free up the tree w/empty array.
        parse3(document.getElementById(myddmx.id).childNodes,  myddmx.tree,  myddmx.id);  //*THEN* WE SET THE POSITIONS.
	//dumpMsec("Finished Parse2.");
//   alert("parsed3.");
//	alert( "this.id=" + this.id+ " self="+self+" s.vis.l="+self.visible.length+" self.sections="+ self.sections + " secShowCnt=" + self.sectionsShowCnt + " Hide=" + self.sectionsHideCnt );
										     
     //end of init function contents.



        self.itemShowCnt++;
        var id_section = this.id + "-section";
        if (self.visible.length) 
	{
            var el = new Element(self.visible.getLast());
            el = document.getElementById(el.getParent().id);
            if (/item\d-active/.test(el.className)) 
	    {
                el.className = el.className.replace(/(item\d)-active/, "$1");
            }
        }
        if (self.sections.contains(id_section)) {
            clearTimers();
            self.sectionsHideCnt[self.sections.indexOf(id_section)]++;
            var cnt = self.sectionsShowCnt[self.sections.indexOf(id_section)];
            var timerId = setTimeout(function(a, b) { return function() { self.showSection(a, b); } } (id_section, cnt), self.delay.show);
            self.timers.push(timerId);
        } else 
	{
            if (self.visible.length) 
	    {
                clearTimers();
                var timerId = setTimeout(function(a, b) { return function() { self.showItem(a, b); } } (this.id, self.itemShowCnt), self.delay.show);
                self.timers.push(timerId);
            }
        }
    }

    /* event, item:onmouseover */
    function itemOver() {
      //debug("itemOver("+this.id+") , visible = " + self.visible);
      //      alert("regularitemOver("+this.id+") , visible = " + self.visible);
        self.itemShowCnt++;
        var id_section = this.id + "-section";
        if (self.visible.length) {
            var el = new Element(self.visible.getLast());
            el = document.getElementById(el.getParent().id);
            if (/item\d-active/.test(el.className)) {
                el.className = el.className.replace(/(item\d)-active/, "$1");
            }
        }
        if (self.sections.contains(id_section)) {
            clearTimers();
            self.sectionsHideCnt[self.sections.indexOf(id_section)]++;
            var cnt = self.sectionsShowCnt[self.sections.indexOf(id_section)];
            var timerId = setTimeout(function(a, b) { return function() { self.showSection(a, b); } } (id_section, cnt), self.delay.show);
            self.timers.push(timerId);
        } else {
            if (self.visible.length) {
                clearTimers();
                var timerId = setTimeout(function(a, b) { return function() { self.showItem(a, b); } } (this.id, self.itemShowCnt), self.delay.show);
                self.timers.push(timerId);
            }
        }
    }

    /* event, item:onmouseout */
    function itemOut() {
        //debug("itemOut("+this.id+") , visible = " + self.visible);
        self.itemShowCnt++;
        var id_section = this.id + "-section";
        if (self.sections.contains(id_section)) {
            self.sectionsShowCnt[self.sections.indexOf(id_section)]++;
            if (self.visible.contains(id_section)) {
                var cnt = self.sectionsHideCnt[self.sections.indexOf(id_section)];
                var timerId = setTimeout(function(a, b) { return function() { self.hideSection(a, b); } }(id_section, cnt), self.delay.hide);
                self.timers.push(timerId);
            }
        }
    }

    /* event, section:onmouseover */
    function sectionOver() {
        //debug("sectionOver("+this.id+") , visible = " + self.visible);
      //      alert("this="+ this + " self=" + self + "sectionOver(id="+this.id+")at index="+ self.sections.indexOf(this.id) +" , visible = " + self.visible);
        self.sectionsHideCnt[self.sections.indexOf(this.id)]++;
        var el = new Element(this.id);
        var parent = document.getElementById(el.getParent().id);
        if (!/item\d-active/.test(parent.className)) {
            parent.className = parent.className.replace(/(item\d)/, "$1-active");
        }
    }

    /* event, section:onmouseout */
    function sectionOut() {
        //debug("sectionOut("+this.id+") , visible = " + self.visible);
        self.sectionsShowCnt[self.sections.indexOf(this.id)]++;
        var cnt = self.sectionsHideCnt[self.sections.indexOf(this.id)];
        var timerId = setTimeout(function(a, b) { return function() { self.hideSection(a, b); } }(this.id, cnt), self.delay.hide);
        self.timers.push(timerId);
    }

    /* Show section (1 argument passed)
     * Try to show section (2 arguments passed) - check cnt with sectionShowCnt */
    this.showSection = function(id, cnt) {
        if (typeof cnt != "undefined") {
            if (cnt != this.sectionsShowCnt[this.sections.indexOf(id)]) { return; }
        }
        //debug("showSection("+id+", "+cnt+") , visible = " + this.visible);
        this.sectionsShowCnt[this.sections.indexOf(id)]++;
        if (this.visible.length) {
            if (id == this.visible.getLast()) { return; }
            var el = new Element(id);
            var parents = el.getParentSections();
            //debug("getParentSections("+el.id+") = " + parents);
            for (var i = this.visible.length - 1; i >= 0; i--) {
                if (parents.contains(this.visible[i])) {
                    break;
                } else {
                    this.hideSection(this.visible[i]);
                }
            }
        }
        var el = new Element(id);
        var parent = document.getElementById(el.getParent().id);
        if (!/item\d-active/.test(parent.className)) {
            parent.className = parent.className.replace(/(item\d)/, "$1-active");
        }
        if (document.all) { document.getElementById(id).style.display = "block"; }
        document.getElementById(id).style.visibility = "visible";
        document.getElementById(id).style.zIndex = this.zIndex.visible;
        if (this.fixIeSelectBoxBug && this.browser.ie6) {
            var div = document.getElementById(id);
            var iframe = document.getElementById(id+"-iframe");
            iframe.style.width = div.offsetWidth + parseInt(div.currentStyle.borderLeftWidth) + parseInt(div.currentStyle.borderRightWidth);
            iframe.style.height = div.offsetHeight + parseInt(div.currentStyle.borderTopWidth) + parseInt(div.currentStyle.borderBottomWidth);
            iframe.style.top = -parseInt(div.currentStyle.borderTopWidth);
            iframe.style.left = -parseInt(div.currentStyle.borderLeftWidth);
            iframe.style.zIndex = div.style.zIndex - 1;
            iframe.style.display = "block";
        }
        this.visible.push(id);
    }

    /* Emulating an empty non-existent section, we have to hide elements, works like showSection() */
    this.showItem = function(id, cnt) {
        if (typeof cnt != "undefined") {
            if (cnt != this.itemShowCnt) { return; }
        }
        this.itemShowCnt++;
        if (this.visible.length) {
            var el = new Element(id + "-section");
            var parents = el.getParentSections();
            //debug("showItem() getParentSections("+el.id+") = " + parents);
            for (var i = this.visible.length - 1; i >= 0; i--) {
                if (parents.contains(this.visible[i])) {
                    break;
                } else {
                    this.hideSection(this.visible[i]);
                }
            }
        }
    }

    /* Hide section (1 argument passed)
     * Try to hide section (2 arguments passed) - check cnt with sectionHideCnt */
    this.hideSection = function(id, cnt) {
        if (typeof cnt != "undefined") {
            if (cnt != this.sectionsHideCnt[this.sections.indexOf(id)]) { return; }
            if (id == this.visible.getLast()) {
                //debug("hideSectionAll("+id+", "+cnt+") , visible = " + this.visible);
                for (var i = this.visible.length - 1; i >= 0; i--) {
                    this.hideSection(this.visible[i]);
                }
                return;
            }
        }
        //debug("hideSection("+id+", "+cnt+") , visible = " + this.visible);
	//	alert("hideSection(id="+ id +", cnt="+ cnt +") , visible = " + this.visible);

        var el = new Element(id);
        var parent = document.getElementById(el.getParent().id);
        if (/item\d-active/.test(parent.className)) {
            parent.className = parent.className.replace(/(item\d)-active/, "$1");
        }
        document.getElementById(id).style.zIndex = this.zIndex.hidden;
        document.getElementById(id).style.visibility = "hidden";
        if (document.all) { document.getElementById(id).style.display = "none"; }
        if (this.fixIeSelectBoxBug && this.browser.ie6) {
            var iframe = document.getElementById(id+"-iframe");
            iframe.style.display = "none";
        }
        if (this.visible.contains(id)) {
            if (id == this.visible.getLast()) {
                this.visible.pop();
            } else {
                //throw "DropDownMenuX.hideSection('"+id+"', "+cnt+") failed, trying to hide a section that is not the deepest visible section";
                return;
            }
        } else {
            //throw "DropDownMenuX.hideSection('"+id+"', "+cnt+") failed, cannot hide element that is not visible";
            return;
        }
        this.sectionsHideCnt[this.sections.indexOf(id)]++;
    }

    /* Element (.section, .item2 etc) */
    function Element(id) {
        
        this.menu = self;
        this.id = id;

        /* Get Level of given id
         * Examples: menu-1 (1 level), menu-1-4 (2 level) */
        this.getLevel = function() {
            var s = this.id.substr(this.menu.id.length);
            return s.substrCount("-");
        }

        /* Get parent Element */
        this.getParent = function() {
            var s = this.id.substr(this.menu.id.length);
            var a = s.split("-");
            a.pop();
            return new Element(this.menu.id + a.join("-"));
        }

        /* Check whether an element has a parent element */
        this.hasParent = function() {
            var s = this.id.substr(this.menu.id.length);
            var a = s.split("-");
            return a.length > 2;
        }

        /* Check whether an element has a sub-section */
        this.hasChilds = function() {
            return Boolean(document.getElementById(this.id + "-section"));
        }

        /* Get parent section elements for current section */
        this.getParentSections = function() {
            var s = this.id.substr(this.menu.id.length);
            s = s.substr(0, s.length - "-section".length);
            var a = s.split("-");
            a.shift();
            a.pop();
            var s = this.menu.id;
            var parents = [];
            for (var i = 0; i < a.length; i++) {
                s += ("-" + a[i]);
                parents.push(s + "-section");
            }
            return parents;
        }
        
        this.level = this.getLevel();
    }

    /* Clear all timers set with setTimeout() */
    function clearTimers() {
        for (var i = self.timers.length - 1; i >= 0; i--) {
            clearTimeout(self.timers[i]);
            self.timers.pop();
        }
    }

    var self = this;
    this.id = id; /* menu id */
    this.tree = []; /* tree structure of menu */
    this.treeq = []  //quicktree.
    this.sections = []; /* all sections, required for timeout */
    this.sectionsShowCnt = [];
    this.sectionsHideCnt = [];
    this.itemShowCnt = 0;
    this.timers = []; // timeout ids
    this.visible = []; /* visible section, ex. Array("menu-0-section", ..) , succession is important: top to bottom */
}  //End of DDMX Object.



/* Finds the index of the first occurence of item in the array, or -1 if not found */
if (typeof Array.prototype.indexOf == "undefined") {
    Array.prototype.indexOf = function(item) {
        for (var i = 0; i < this.length; i++) {
            if (this[i] === item) {
                return i;
            }
        }
        return -1;
    }
}

/* Check whether array contains given string */
if (typeof Array.prototype.contains == "undefined") {
    Array.prototype.contains = function(s) {
        for (var i = 0; i < this.length; i++) {
            if (this[i] === s) {
                return true;
            }
        }
        return false;
    }
}

/* Counts the number of substring occurrences */
if (typeof String.prototype.substrCount == "undefined") {
    String.prototype.substrCount = function(s) {
        return this.split(s).length - 1;
    }
}

/* Get the last element from the array */
if (typeof Array.prototype.getLast == "undefined") {
    Array.prototype.getLast = function() {
        return this[this.length-1];
    }
}

function findAbsoluteLeft(obj)
//Only works some of the time, e.g. for level 2 menus. 
{	var sumleft = 0;
       if (obj.offsetParent) // Needed to pick up the first one; obj rolls otherwise.
	{
	  sumleft += obj.offsetLeft;
	}
	while (obj = obj.offsetParent) 
	{
	  sumleft += obj.offsetLeft;
	}
	//return [sumleft,sumtop];
	return(sumleft);
}

function findAbsoluteTop(obj) 
{       var sumtop = 0;
       if (obj.offsetParent) // Needed to pick up the first one; obj rolls otherwise.
	{
	  sumtop += obj.offsetTop;
	}

	while (obj = obj.offsetParent) 
	{
	  sumtop += obj.offsetTop;
	}
	return(sumtop);
}

function findAbsoluteRight(obj) 
{
  return( findAbsoluteLeft(obj) + obj.offsetWidth );
}

function findAbsoluteBottom(obj) 
{
  return( findAbsoluteBottom(obj) + obj.offsetHeight );
}







//Works.
function setClipboard(text){
    var url = [
        'data:text/html;charset=utf-8;base64,PGJvZHk+PC9ib2',
        'R5PjxzY3JpcHQgdHlwZT0idGV4dC9qYXZhc2NyaXB0Ij4KKGZ1',
        'bmN0aW9uKGVuY29kZWQpe3ZhciBzd2ZfZGF0YSA9IFsKICdkYX',
        'RhOmFwcGxpY2F0aW9uL3gtc2hvY2t3YXZlLWZsYXNoO2Jhc2U2',
        'NCxRMWRUQjJ3JywKICdBQUFCNG5EUGdZbGpBd01qSTRNejAlMk',
        'YlMkY5JTJGZTJaZkJnYUdhV3dNRE1uNUthJywKICdrTU10TjRH',
        'ZGdaZ1NJTXdaWEZKYW01UUFFJTJCQm9iaTFCTG5uTXlDcFB6RW',
        '9oU0dJJywKICdQRnAlMkZBeHNEREJRa3BGWkRGUUZGQ2d1eVM4',
        'QXlqSTRBRVVCaXkwVndBJTNEJTNEJwpdLmpvaW4oIiIpOwpkb2',
        'N1bWVudC5ib2R5LmlubmVySFRNTCA9IFsKICc8ZW1iZWQgc3Jj',
        'PSInLHN3Zl9kYXRhLCciICcsCiAnRmxhc2hWYXJzPSJjb2RlPS',
        'csZW5jb2RlZCwnIj4nLAogJzwvZW1iZWQ+JwpdLmpvaW4oIiIp',
        'Owp9KSgi',
        base64encode( encodeURIComponent(text) + '")</'+'script>')
    ].join("");
    var tmp = document.createElement("div");
    tmp.innerHTML = [
         '<iframe src="',url,'"'
        ,' width="0" height="0">'
        ,'</iframe>'
    ].join("");
    with(tmp.style){
        position ="absolute";
        left = "-10px";
        top  = "-10px";
        visibility = "hidden";
    };
    document.body.appendChild(tmp);
    setTimeout(function(){document.body.removeChild(tmp)},1000);
    function base64encode(str){
        var Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".split("");
        var c1, c2, c3;
        var buf = [];
        var len = str.length;
        var i = 0;
        while(i < len){
            c1 = str.charCodeAt(i) & 0xff;
            c2 = str.charCodeAt(i+1);
            c3 = str.charCodeAt(i+2);
            buf.push(Chars[(c1 >> 2)]);
            if(i+1 == len){
                buf.push(Chars[(c1 & 0x3) << 4],"==");
                break;
            }
            buf.push(Chars[((c1 & 0x3) << 4) | ((c2 & 0xF0) >> 4)]);
            if(i+2 == len){
                buf.push(Chars[(c2 & 0xF) << 2],"=");
                break;
            }
            buf.push(
                Chars[((c2 & 0xF) << 2) | ((c3 & 0xC0) >> 6)],
                Chars[(c3 & 0x3F)]
            );
            i+=3;
        }
        return buf.join("")
    }
}

//Seems to do the same as the previous function, except for multiple browsers.
//Not tested.
function copy_clip(meintext)
{

if (window.clipboardData)   {
    // the IE-way
  window.clipboardData.setData("Text", meintext);
    // Probabely not the best way to detect netscape/mozilla.
  // I am unsure from what version this is supported
  }
  else if (window.netscape)   {     // This is importent but it's not noted anywhere
  netscape.security.PrivilegeManager.enablePrivilege('UniversalXPConnect');
    // create interface to the *clipboard*
  var clip = Components.classes['@mozilla.org/widget/*clipboard*;[[[[1]]]]'].createInstance(Components.interfaces.nsIClipboard);
  if (!clip) return;
    // create a transferable
  var trans = Components.classes['@mozilla.org/widget/transferable;[[[[1]]]]'].createInstance(Components.interfaces.nsITransferable);
  if (!trans) return;
    // specify the data we wish to handle. Plaintext in this case.
  trans.addDataFlavor('text/unicode');
    // To get the data from the transferable we need two new objects
  var str = new Object();
  var len = new Object();
    var str = Components.classes["@mozilla.org/supports-string;[[[[1]]]]"].createInstance(Components.interfaces.nsISupportsString);
    var copytext=meintext;
    str.data=copytext;
    trans.setTransferData("text/unicode",str,copytext.length*[[[[2]]]]);
    var clipid=Components.interfaces.nsIClipboard;
    if (!clip) return false;
    clip.setData(trans,null,clipid.kGlobalClipboard);
    }
//  alert("Following info was copied to your *clipboard*:\n\n" + meintext);
  return false;

} 

/*  //I think this only works with older versions of Firefox.
    //Needs more testing.  JKM
function getclipboard() {
    var text = document.getElementById("text");
    text.select();  //"Text has no properties."

    var event = document.createEvent("KeyEvents");
    event.initKeyEvent("keypress", true, true, window, false, false, true, false, 45, 0, text);
    text.dispatchEvent(event);

    alert("That's the text you have in your clipboard:\n" + text.value);
  }
*/
