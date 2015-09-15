// 
// 	HISTORY
//  !! Handler is not here, it's in function vpl-on-message in vpl.ls!!
// 
//  Jul 21 '06	Menu.  MenuEntry. Menu.add-menu-entry(menu-entry).
//    xml-element-to-object.
// 
// 
// 
// 
// 
//  (MENU  "MenuTitle Name"
//      (MENUENTRY  "MenuEntry Name"  ID# )
//  )
// 
// 
//  Format for XML SEXP nodes:  <nodeType nodeName> value
//  <1 LIST>
//    <1 SYMBOL> 
//       MENU </SYMBOL>
//    <1 STRING>                             Note:  Not a 3 "text node" type!
//       "MenuTitle Name" </STRING>
//    <1 LIST>
//       <1 SYMBOL>
//          MENUENTRY
//       <1 STRING>
//          "MenuEntry Name"  </STRING>
//       <1 NUMBER>
//           12
// 
// 


// The following types are used by the XML Transport layer
// to pack and unpack information.
// They are not part of SEXP.

window.NodeTypes = new Object();

NodeTypes.ELEMENT_NODE = 1;

NodeTypes.ATTRIBUTE_NODE = 2;

NodeTypes.TEXT_NODE = 3;

NodeTypes.CDATA_SECTION_NODE = 4;

NodeTypes.ENTITY_REFERENCE_NODE = 5;

NodeTypes.ENTITY_NODE = 6;

NodeTypes.PROCESSING_INSTRUCTION_NODE = 7;

NodeTypes.COMMENT_NODE = 8;

NodeTypes.DOCUMENT_NODE = 9;

NodeTypes.DOCUMENT_TYPE_NODE = 10;

NodeTypes.DOCUMENT_FRAGMENT_NODE = 11;

NodeTypes.NOTATION_NODE = 12;

NodeTypes.names = [false, "ELEMENT_NODE", "ATTRIBUTE_NODE", "TEXT_NODE", "CDATA_SECTION_NODE", "ENTITY_REFERENCE_NODE", "ENTITY_NODE", "PROCESSING_INSTRUCTION_NODE", "COMMENT_NODE", "DOCUMENT_NODE", "DOCUMENT_TYPE_NODE", "DOCUMENT_FRAGMENT_NODE", "NOTATION_NODE"];

//  ID stamp.
window.currentId = 0;  // Was 1, which causes ++code to start at 2.  

// OBJECTS      Pseudo-constructors

window.SexpList = function () {
    this.sexpType = "list";
    this.uniqueId = ++currentId;
    this.listItems = new Array();
    return this;
};

window.SexpMenu = function (titleString) { // JKM 07/21/06
    // This Menu object represents a single menu.  It contains a list of MenuEntries.
    this.sexpType = "menu";
    this.uniqueId = ++currentId;
    this.menuEntries = new Array();
    this.titleString = titleString;
    return this;
};

window.SexpMenuEntry = function (commandId, titleString) {
    // This low-level MenuEntry object represents a single entry line on a particular menu.
    // Note:  In actuality, args are ignored; actual contents are stuffed in build-from-xml.
    this.sexpType = "menuentry";
    this.commandId = commandId;
    this.titleString = titleString;
    return this;
};

window.SexpSymbol = function (name, package) {
    this.sexpType = "symbol";
    if (package) {
        this.package = package;
    }
    this.name = name;
    return this;
};

window.SexpNumber = function (value) {
    this.sexpType = "number";
    this.value = value;
    return this;
};

window.SexpString = function (value) {
    this.sexpType = "string";
    this.value = value;
    return this;
};

window.SexpT = function (value) {
    this.sexpType = "t";
    this.value = value;
    return this;
};

// N.B. A hole has 'content' while a multihole has 'contents'
window.SexpHole = function () {
    this.sexpType = "hole";
    this.content = false;
    return this;
};

window.SexpMultiHole = function () {
    this.sexpType = "mhole";
    this.contents = false;
    return this;
};

//
// METHODS   
//

// LIST OPERATIONS

SexpList.prototype.addListItem = function (item) {
    item.uniqueId = ++currentId;
    item.parent = this;
    item.index = this.listItems.length;
    this.listItems.push(item);
};

SexpList.prototype.replaceListItem = function (item, index) {
    // This method overwrites zero-based entry #index in the list, with 'item'.
    item.uniqueId = ++currentId;
    item.parent = this;
    this.listItems[index] = item;
};

SexpMenu.prototype.addMenuEntry = function (menuEntry) {
    // This Menu method pushes a unique menu-entry onto the end of the Menu's list.
    entry.parent = this;
    entry.index = this.menuEntries.length;
    this.menuEntries.push(menuEntry);
};

SexpMenu.prototype.addListItem = function (menuEntry) {
    // A hack requires using add-list-item, so here it is for Menu.

    // This is how it was in the Lispscript. But I suspect it was as I have it below. -- peter@gigamonkeys.com
    // this.addMenuEntry.menuEntry;
    this.addMenuEntry(menuEntry);
};

SexpMultiHole.prototype.replaceListItem = function (item, index) {
    item.uniqueId = ++currentId;
    item.parent = this.parent;
    this.contents[index] = item;
};

//
// fill holes with content
//

SexpHole.prototype.fill = function (content) {
    this.content = content;
};

SexpHole.prototype.fillWithLiteral = function (text) {
    var type = this.type;
    if (!((text[0] == "\"") && (text[(text.length - 1)] == "\""))) {
        text = text.toUpperCase();
    }
    if (type == "symbol") {
        this.fill(new SexpSymbol(text));
    } else if (type == "string") {
        this.fill(new SexpString(text));
    } else if (type == "number") {
        this.fill(new SexpNumber(text));
    } else if (type == "boolean") {
        this.fill(new SexpT(text));
    } else if (type == "t") {
        this.fill(new SexpT(text));
    } else {
        alert("Don't know how to fill hole of type " + type);
    }
};

SexpMultiHole.prototype.fill = function (contents) {
    this.contents = contents;
};

SexpMultiHole.prototype.fillWithLiteral = function (text) {
    text = text.toUpperCase();
    var type = this.type;
    if (type == "symbol") {
        this.fill(new SexpSymbol(text));
    } else if (type == "string") {
        this.fill(new SexpString(text));
    } else if (type == "number") {
        this.fill(new SexpNumber(text));
    } else if (type == "boolean") {
        this.fill(new SexpT(text));
    } else if (type == "t") {
        this.fill(new SexpT(text));
    } else {
        alert("Don't know how to fill hole of type " + type);
    }
};

window.copySexpUp = function (sexp) {
    if (sexp.parent) {
        sexp.parent.replaceListItem(sexp, sexp.index);
        copySexpUp(sexp(parent));
    }
};

window.getEmptySexp = function () {
    return new SexpT("delete");
};

//
// deepCopy
//

SexpT.prototype.deepCopy = function () {
    return new SexpT(this.value);
};

SexpString.prototype.deepCopy = function () {
    return new SexpString(this.value);
};

SexpNumber.prototype.deepCopy = function () {
    return new SexpNumber(this.value);
};

SexpSymbol.prototype.deepCopy = function () {
    return new SexpSymbol(this.name);
};

SexpList.prototype.deepCopy = function () {
    var newList = new SexpList();
    var g$236 = this.listItems;
    for (g$235 in g$236) {
        var thing = g$236[g$235];
        newList.addListItem(thing);
    }
    return newList;
};

SexpHole.prototype.deepCopy = function () {
    var sexp = new SexpHole();
    sexp.name = this.name;
    sexp.type = this.type;
    sexp.inline = this.inline;
    sexp.editable = this.editable;
    sexp.content = this.content.deepCopy();
    return sexp;
};

SexpMultiHole.prototype.deepCopy = function () {
    var sexp = new SexpMultiHole();
    sexp.name = this.name;
    sexp.type = this.type;
    sexp.inline = this.inline;
    sexp.editable = this.editable;
    sexp.contents = this.copyContents();
    sexp.template = this.copyTemplate();
    return sexp;
};

SexpMultiHole.prototype.copyContents = function () {
    var contents = false;
    if (this.contents.length > 0) {
        contents = new Array();
        var g$245 = this.contents;
        for (g$244 in g$245) {
            var item = g$245[g$244];
            contents.push(item.deepCopy());
        }
    } else if (this.contents) {
        alert("Multi-hole with one item?");
        contents = this.contents.deepCopy();
    } else {
        alert("Multi-hole with no contents?");
    }
    return contents;
};

SexpMultiHole.prototype.copyTemplate = function () {
    var template = false;
    if (this.template.length > 0) {
        template = new Array();
        var g$252 = this.template;
        for (g$251 in g$252) {
            var item = g$252[g$251];
            template.push(item.cloneNode(true));
        }
    } else if (this.template) {
        alert("Multi-hole with empty template?");
    } else {
        alert("Multi-hole with no template?");
    }
    return template;
};

//
// to-HTML METHODS
//

//  list
SexpList.prototype.toHtml = function () {
    var list = function (ELEMENT) {
        ELEMENT.setAttribute("class", "list");
        return ELEMENT;
    }.call(this, document.createElement("div"));
    list.sexp = this;
    var g$257 = this.listItems.length;
    for (var i = 0; (i < g$257); i++) {
        var thing = this.listItems[i];
        list.appendChild(thing.toHtml());
    }
    return list;
};

//  Menu      
//        wanted:  <select name="selectName" size="1">   ...  </select>
SexpMenu.prototype.toHtml = function () {
    var themenu = function (ELEMENT) {
        ELEMENT.setAttribute("class", "menu");
        ELEMENT.setAttribute("name", NIL);
        ELEMENT.setAttribute("size", "1");
        return ELEMENT;
    }.call(this, document.createElement("select"));
    themenu.sexp = this;
    var g$262 = this.menuEntries.length;
    for (var i = 0; (i < g$262); i++) {
        var thisMenuEntry = this.menuEntries[i];
        themenu.appendChild(thisMenuEntry.toHtml());
    }
    return themenu;
};

//  MenuEntry
//      wanted:  <option value="id1">first</option>
// 
SexpMenuEntry.prototype.toHtml = function () {
    var commandId = this.commandId;
    var titleString = this.titleString;
    return function (ELEMENT) {
        ELEMENT.setAttribute("value", "command-id");
        ELEMENT.appendChild(function () {
            if (typeof(titleString) == "string") {
                return document.createTextNode(titleString);
            } else {
                return titleString;
            }
        }.call(this));
	return ELEMENT;
    }.call(this, document.createElement("option"));
};

//  symbol
SexpSymbol.prototype.toHtml = function () {
    var name = this.name;
    var package = this.package;
    return function (ELEMENT) {
        ELEMENT.setAttribute("class", "symbol");
        ELEMENT.appendChild(function () {
            if (typeof(name) == "string") {
                return document.createTextNode(name);
            } else {
                return name;
            }
        }.call(this));
        return ELEMENT;
    }.call(this, document.createElement("span"));
};

//  number
SexpNumber.prototype.toHtml = function () {
    var value = this.value;
    return function (ELEMENT) {
        ELEMENT.setAttribute("class", "value");
        ELEMENT.appendChild(function () {
            if (typeof(value) == "string") {
                return document.createTextNode(value);
            } else {
                return value;
            }
        }.call(this));
        return ELEMENT;
    }.call(this, document.createElement("span"));
};

//  string
SexpString.prototype.toHtml = function () {
    var value = this.value;
    return function (ELEMENT) {
        ELEMENT.setAttribute("class", "value");
        ELEMENT.appendChild(function () {
            if (typeof(value) == "string") {
                return document.createTextNode(value);
            } else {
                return value;
            }
        }.call(this));
        return ELEMENT;
    }.call(this, document.createElement("span"));
};

//  T
SexpT.prototype.toHtml = function () {
    var value = this.value;
    return function (ELEMENT) {
        ELEMENT.setAttribute("class", "value");
        ELEMENT.appendChild(function () {
            if (typeof(value) == "string") {
                return document.createTextNode(value);
            } else {
                return value;
            }
        }.call(this));
        return ELEMENT;
    }.call(this, document.createElement("span"));
};

//  Hole
SexpHole.prototype.toHtml = function () {
    // alert("hole.toHtml: template: " + this.template +  "; name: " + this.name);
    var hole = function () {
        if (this.inline) {
            return function (ELEMENT) {
                ELEMENT.setAttribute("class", "hole");
                return ELEMENT;
            }.call(this, document.createElement("span"));
        } else {
            return function (ELEMENT) {
                ELEMENT.setAttribute("class", "hole");
                return ELEMENT;
            }.call(this, document.createElement("div"));
        }
    }.call(this);
    hole.sexp = this;
    var type = this.type;
    if (this.listItems) {
        var g$274 = this.listItems;
        for (g$273 in g$274) {
            var item = g$274[g$273];
            hole.appendChild(item.toHtml());
        }
    } else if (this.contents) {
        hole.appendChild(this.contents.toHtml());
    } else if (this.content) {
        if (type == "symbol") {
            hole.appendChild(this.content.toHtml());
        } else if (type == "number") {
            hole.appendChild(this.content.toHtml());
        } else if (type == "string") {
            hole.appendChild(this.content.toHtml());
        } else if (type == "boolean") {
            hole.appendChild(this.content.toHtml());
        } else if (type == "t") {
            hole.appendChild(this.content.toHtml());
        } else {
            alert("Hole contains: " + type);
        }
    } else if (this.value) {
        this.value.toHtml().appendChild();
    } else if (this.name) {
        var name = this.name;
        hole.appendChild(function (ELEMENT) {
            ELEMENT.appendChild(function () {
                if (typeof(name) == "string") {
                    return document.createTextNode(name);
                } else {
                    return name;
                }
            }.call(this));
            return ELEMENT;
        }.call(this, document.createElement("i")));
    }
    setupHole(hole, this);
    addMenuBox(hole);
    return hole;
};

//  MultiHole
SexpMultiHole.prototype.toHtml = function () {
    // alert("multihole.toHtml: template: " + this.template + "; name: " + this.name);
    var hole = function () {
        if (this.inline) {
            return function (ELEMENT) {
                ELEMENT.setAttribute("class", "mhole");
                return ELEMENT;
            }.call(this, document.createElement("span"));
        } else {
            return function (ELEMENT) {
                ELEMENT.setAttribute("class", "mhole");
                return ELEMENT;
            }.call(this, document.createElement("div"));
        }
    }.call(this);
    hole.sexp = this;
    var contents = this.contents;
    var template = this.template;
    if (contents.length > 0) {
        var g$283 = this.contents.length;
        for (var i = 0; (i < g$283); i++) {
            hole.appendChild(this.contents[i].toHtml());
            setupHole(hole, this);
        }
    } else if (template.length > 0) {
        var g$285 = this.template.length;
        for (var i = 0; (i < g$285); i++) {
            var thing = xmlToSexp(this.template[i]);
            thing.parent = this;
            thing.index = i;
            this.contents.push(thing);
            hole.appendChild(thing.toHtml());
        }
        if ((this.type == "list") || (this.type == "multi")) {
            var ellipses = function (ELEMENT) {
                ELEMENT.appendChild(document.createTextNode("..."));
                return ELEMENT;
            }.call(this, document.createElement("i"));
            ellipses.onclick = function () {
                var g$281 = template.length;
                for (var i = 0; (i < g$281); i++) {
                    var thing = xmlToSexp(template[i]);
                    contents.push(thing);
                    hole.insertBefore(thing.toHtml(), ellipses);
                }
            };
            hole.appendChild(ellipses);
        }
    } else {
        alert("What the...?!?!");
    }
    addMenuBox(hole);
    return hole;
};

//
// to-xml
//

SexpList.prototype.toXml = function () {
    var xmlList = function (ELEMENT) {
        return ELEMENT;
    }.call(this, document.createElement("list"));
    var g$298 = this.listItems;
    for (g$297 in g$298) {
        var thing = g$298[g$297];
	// if multi hole, call other method to return xml list
        if (thing.type == "multi") {
            var g$304 = thing.contents;
            for (g$303 in g$304) {
                var item = g$304[g$303];
                xmlList.appendChild(item.toXml());
            }
        } else {
            xmlList.appendChild(thing.toXml());
        }
    }
    return xmlList;
};

SexpMenu.prototype.toXml = function () {
    // Converts an existing Menu into an xml blob, on the client side.
    // FIX ME.  IS THIS NEEDED?
    var xmlList = function (ELEMENT) {
        return ELEMENT;
    }.call(this, document.createElement("list"));
    var g$311 = this.menuEntries;
    for (g$310 in g$311) {
        var thing = g$311[g$310];
        xmlList.appendChild(thing.toXml());
    }
    return xmlList;
};

SexpMenuEntry.prototype.toXml = function () {
    // Converts an existing MenuEntry into an xml blob, on the client side.
    // FIX ME.  IS THIS NEEDED?
    var package = this.package;
    var name = this.name;
    if (package) {
        return function (ELEMENT) {
            ELEMENT.setAttribute("package", package);
            ELEMENT.appendChild(function () {
                if (typeof(name) == "string") {
                    return document.createTextNode(name);
                } else {
                    return name;
                }
            }.call(this));
            return ELEMENT;
        }.call(this, document.createElement("symbol"));
    } else {
        return function (ELEMENT) {
            ELEMENT.appendChild(function () {
                if (typeof(name) == "string") {
                    return document.createTextNode(name);
                } else {
                    return name;
                }
            }.call(this));
            return ELEMENT;
        }.call(this, document.createElement("symbol"));
    }
};

SexpSymbol.prototype.toXml = function () {
    var package = this.package;
    var name = this.name;
    if (package) {
        return function (ELEMENT) {
            ELEMENT.setAttribute("package", package);
            ELEMENT.appendChild(function () {
                if (typeof(name) == "string") {
                    return document.createTextNode(name);
                } else {
                    return name;
                }
            }.call(this));
            return ELEMENT;
        }.call(this, document.createElement("symbol"));
    } else {
        return function (ELEMENT) {
            ELEMENT.appendChild(function () {
                if (typeof(name) == "string") {
                    return document.createTextNode(name);
                } else {
                    return name;
                }
            }.call(this));
            return ELEMENT;
        }.call(this, document.createElement("symbol"));
    }
};

SexpNumber.prototype.toXml = function () {
    var value = this.value;
    return function (ELEMENT) {
        ELEMENT.appendChild(function () {
            if (typeof(value) == "string") {
                return document.createTextNode(value);
            } else {
                return value;
            }
        }.call(this));
        return ELEMENT;
    }.call(this, document.createElement("number"));
};

SexpString.prototype.toXml = function () {
    var value = this.value;
    return function (ELEMENT) {
        ELEMENT.appendChild(function () {
            if (typeof(value) == "string") {
                return document.createTextNode(value);
            } else {
                return value;
            }
        }.call(this));
        return ELEMENT;
    }.call(this, document.createElement("string"));
};

SexpT.prototype.toXml = function () {
    var value = this.value;
    return function (ELEMENT) {
        ELEMENT.appendChild(function () {
            if (typeof(value) == "string") {
                return document.createTextNode(value);
            } else {
                return value;
            }
        }.call(this));
        return ELEMENT;
    }.call(this, document.createElement("t"));
};

SexpHole.prototype.toXml = function () {
    if (this.content) {
        return this.content.toXml();
    } else {
        return function (ELEMENT) {
            return ELEMENT;
        }.call(this, document.createElement("unfilled-hole"));
    }
};

SexpMultiHole.prototype.toXml = function () {
    if (this.contents) {
        this.contents.toXml();
    } else {
        return function (ELEMENT) {
            return ELEMENT;
        }.call(this, document.createElement("unfilled-hole"));
    }
};

//
// from-xml Functions.
//

window.xmlToSexp = function (xml) {
    var type = xml.nodeType;
    if (type == NodeTypes.ELEMENT_NODE) {
        return xmlElementToObject(xml);
    } else if (type == NodeTypes.TEXT_NODE) {
        return xml.data;
    } else if (type == NodeTypes.DOCUMENT_NODE) {
        return xmlToSexp(xml.documentElement);
    } else {
        if ((1 <= type) && (type <= 12)) {
            alert(NodeTypes.names[type] + " not supported");
        } else {
            alert("Unrecognized nodeType " + type);
        }
    }
};

window.xmlElementToObject = function (element) {
    var tag = element.nodeName;
    // alert("Making Sexp out of XML element with tag: " + tag);
    var obj = function () {
        if (stringEqual(tag, "LIST")) {
            return new SexpList();
        } else {
            if (stringEqual(tag, "MENU")) {
                document.getElementById("palette").appendChild(document.createTextNode("new SexpMenu..."));
                return new SexpMenu();
            } else {
                if (stringEqual(tag, "MENUENTRY")) {
                    document.getElementById("palette").appendChild(document.createTextNode("new SexpMenuEntry..."));
                    return new SexpMenuEntry();
                } else if (stringEqual(tag, "SYMBOL")) {
                    return new SexpSymbol();
                } else if (stringEqual(tag, "NUMBER")) {
                    return new SexpNumber();
                } else if (stringEqual(tag, "STRING")) {
                    return new SexpString();
                } else if (stringEqual(tag, "HOLE")) {
                    return new SexpHole();
                } else if (stringEqual(tag, "MHOLE")) {
                    return new SexpMultiHole();
                }
            };
        }
    }.call(this);
    obj.buildFromXml(element);
    return obj;
};

window.stringEqual = function (s1, s2) {
    return (s1.toUpperCase() == s2.toUpperCase());
};

//
//  BUILD-FROM-XML METHODS
//

SexpList.prototype.buildFromXml = function (element) {
    var children = element.childNodes;
    var g$326 = children.length;
    for (var i = 0; (i < g$326); i++) {
        var child = children.item(i);
	// sort of a hack for the moment.
        if (!((child.nodeType == 3) && allBlank(child.data))) {
            var sexp = xmlToSexp(child);
            this.addListItem(sexp);
        }
    }
};

window.allBlank = function (string) {
    var allBlank = true;
    var g$331 = string.length;
    for (var i = 0; (i < g$331); i++) {
        var char = string.charAt(i);
        if (!((char === ' ')) && !((char === '\n')) && !((char === '\t'))) {
            allBlank = false;
        }
    }
    // In the Lispscript it actually always returned undefined. Which
    // is presumably not right. Though who knows if anything now
    // depends on this always returning that rather than the real answer.
    return allBlank;
};

SexpMenu.prototype.buildFromXml = function (element) {
    // This method fills in an existing menu node.
    this.titleString = xmlToSexp(element.firstChild);
};

SexpMenuEntry.prototype.buildFromXml = function (element) {
    // This method fills in an existing menu entry node.
    var first = element.firstChild;
    var second = first.nextSibling();
    this.commandId = xmlToSexp(second);
    this.titleString = xmlToSexp(first);
};

SexpSymbol.prototype.buildFromXml = function (element) {
    this.name = xmlToSexp(element.firstChild);
    this.package = element.getAttribute("package");
};

SexpNumber.prototype.buildFromXml = function (element) {
    this.value = xmlToSexp(element.firstChild);
};

SexpString.prototype.buildFromXml = function (element) {
    this.value = xmlToSexp(element.firstChild);
};

SexpHole.prototype.buildFromXml = function (element) {
    this.name = element.getAttribute("name");
    this.type = element.getAttribute("type");
    this.editable = element.getAttribute("editable");
    this.inline = element.getAttribute("inline");
    if (this.type == "list") {
        this.min = element.getAttribute("min");
        this.max = element.getAttribute("max");
    }
};

SexpMultiHole.prototype.buildFromXml = function (element) {
    this.name = element.getAttribute("name");
    this.type = element.getAttribute("type");
    this.editable = element.getAttribute("editable");
    this.inline = element.getAttribute("inline");
    var children = element.childNodes;
    if (0 < children.length) {
        this.contents = new Array();
        this.template = new Array();
        var g$342 = children.length;
        for (var i = 0; (i < g$342); i++) {
            var child = children.item(i);
            this.template.push(child);
        }
    }
};

