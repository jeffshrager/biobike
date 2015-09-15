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

window.currentId = 0;

window.SexpList = (function () {
  this.sexpType = "list";
  this.uniqueId = ++currentId;
  this.listItems = new Array();
  return this;
});

window.SexpMenu = (function (titleString) {
  this.sexpType = "menu";
  this.uniqueId = ++currentId;
  this.menuEntries = new Array();
  this.titleString = titleString;
  return this;
});

window.SexpMenuEntry = (function (commandId, titleString) {
  this.sexpType = "menuentry";
  this.commandId = commandId;
  this.titleString = titleString;
  return this;
});

window.SexpSymbol = (function (name, package) {
  this.sexpType = "symbol";
  if (package) {
    this.package = package;
  }
  this.name = name;
  return this;
});

window.SexpNumber = (function (value) {
  this.sexpType = "number";
  this.value = value;
  return this;
});

window.SexpString = (function (value) {
  this.sexpType = "string";
  this.value = value;
  return this;
});

window.SexpT = (function (value) {
  this.sexpType = "t";
  this.value = value;
  return this;
});

window.SexpHole = (function () {
  this.sexpType = "hole";
  this.content = false;
  return this;
});

window.SexpMultiHole = (function () {
  this.sexpType = "mhole";
  this.contents = false;
  return this;
});

SexpList.prototype.addListItem = (function (item) {
  item.uniqueId = ++currentId;
  item.parent = this;
  item.index = this.listItems.length;
  return this.listItems.push(item);
});

SexpList.prototype.replaceListItem = (function (item, index) {
  item.uniqueId = ++currentId;
  item.parent = this;
  return this.listItems[index] = item;
});

SexpMenu.prototype.addMenuEntry = (function (menuEntry) {
  entry.parent = this;
  entry.index = this.menuEntries.length;
  return this.menuEntries.push(menuEntry);
});

SexpMenu.prototype.addListItem = (function (menuEntry) {
  return this.addMenuEntry.menuEntry;
});

SexpMultiHole.prototype.replaceListItem = (function (item, index) {
  item.uniqueId = ++currentId;
  item.parent = this.parent;
  return this.contents[index] = item;
});

SexpHole.prototype.fill = (function (content) {
  return this.content = content;
});

SexpHole.prototype.fillWithLiteral = (function (text) {
  return (function (type) {
    if (!(((("" + text[0]) == ("" + "\"")) && (("" + text[(text.length - 1)]) == ("" + "\""))))) {
      text = text.toUpperCase();
    }
    if ((("" + type) == ("" + "symbol"))) {
      return this.fill(new SexpSymbol(text));
    } else {
      if ((("" + type) == ("" + "string"))) {
        return this.fill(new SexpString(text));
      } else {
        if ((("" + type) == ("" + "number"))) {
          return this.fill(new SexpNumber(text));
        } else {
          if ((("" + type) == ("" + "boolean"))) {
            return this.fill(new SexpT(text));
          } else {
            if ((("" + type) == ("" + "t"))) {
              return this.fill(new SexpT(text));
            } else {
              if (true) {
                return alert(("Don't know how to fill hole of type " + type));
              } else {
                return undefined;
              }
            }
          }
        }
      }
    }
  }).call(this, this.type);
});

SexpMultiHole.prototype.fill = (function (contents) {
  return this.contents = contents;
});

SexpMultiHole.prototype.fillWithLiteral = (function (text) {
  text = text.toUpperCase();
  return (function (type) {
    if ((("" + type) == ("" + "symbol"))) {
      return this.fill(new SexpSymbol(text));
    } else {
      if ((("" + type) == ("" + "string"))) {
        return this.fill(new SexpString(text));
      } else {
        if ((("" + type) == ("" + "number"))) {
          return this.fill(new SexpNumber(text));
        } else {
          if ((("" + type) == ("" + "boolean"))) {
            return this.fill(new SexpT(text));
          } else {
            if ((("" + type) == ("" + "t"))) {
              return this.fill(new SexpT(text));
            } else {
              if (true) {
                return alert(("Don't know how to fill hole of type " + type));
              } else {
                return undefined;
              }
            }
          }
        }
      }
    }
  }).call(this, this.type);
});

window.copySexpUp = (function (sexp) {
  if (sexp.parent) {
    sexp.parent.replaceListItem(sexp, sexp.index);
    return copySexpUp(sexp(parent));
  } else {
    return undefined;
  }
});

window.getEmptySexp = (function () {
  return new SexpT("delete");
});

SexpT.prototype.deepCopy = (function () {
  return new SexpT(this.value);
});

SexpString.prototype.deepCopy = (function () {
  return new SexpString(this.value);
});

SexpNumber.prototype.deepCopy = (function () {
  return new SexpNumber(this.value);
});

SexpSymbol.prototype.deepCopy = (function () {
  return new SexpSymbol(this.name);
});

SexpList.prototype.deepCopy = (function () {
  return (function (newList) {
    var g$480 = this.listItems;
    for (g$479 in g$480) {
      (function (thing) {
        newList.addListItem(thing);
      }).call(this, g$480[g$479]);
    }
    return newList;
  }).call(this, new SexpList());
});

SexpHole.prototype.deepCopy = (function () {
  return (function (sexp) {
    sexp.name = this.name;
    sexp.type = this.type;
    sexp.inline = this.inline;
    sexp.editable = this.editable;
    sexp.content = this.content.deepCopy();
    return sexp;
  }).call(this, new SexpHole());
});

SexpMultiHole.prototype.deepCopy = (function () {
  return (function (sexp) {
    sexp.name = this.name;
    sexp.type = this.type;
    sexp.inline = this.inline;
    sexp.editable = this.editable;
    sexp.contents = this.copyContents();
    sexp.template = this.copyTemplate();
    return sexp;
  }).call(this, new SexpMultiHole());
});

SexpMultiHole.prototype.copyContents = (function () {
  return (function (contents) {
    if ((this.contents.length > 0)) {
      contents = new Array();
      var g$489 = this.contents;
      for (g$488 in g$489) {
        (function (item) {
          contents.push(item.deepCopy());
        }).call(this, g$489[g$488]);
      }
    } else {
      if (this.contents) {
        alert(("Multi-hole with one item?"));
        contents = this.contents.deepCopy();
      } else {
        if (true) {
          alert(("Multi-hole with no contents?"));
        }
      }
    }
    return contents;
  }).call(this, false);
});

SexpMultiHole.prototype.copyTemplate = (function () {
  return (function (template) {
    if ((this.template.length > 0)) {
      template = new Array();
      var g$496 = this.template;
      for (g$495 in g$496) {
        (function (item) {
          template.push(item.cloneNode(true));
        }).call(this, g$496[g$495]);
      }
    } else {
      if (this.template) {
        alert(("Multi-hole with empty template?"));
      } else {
        if (true) {
          alert(("Multi-hole with no template?"));
        }
      }
    }
    return template;
  }).call(this, false);
});

SexpList.prototype.toHtml = (function () {
  return (function (list) {
    list.sexp = this;
    var g$501 = this.listItems.length;
    for (var i = 0; (i < g$501); i++) {
      (function (thing) {
        list.appendChild(thing.toHtml());
      }).call(this, this.listItems[i]);
    }
    return list;
  }).call(this, (function (ELEMENT) {
    ELEMENT.setAttribute("CLASS", "list");
    return ELEMENT;
  }).call(this, document.createElement("DIV")));
});

SexpMenu.prototype.toHtml = (function () {
  return (function (themenu) {
    themenu.sexp = this;
    var g$506 = this.menuEntries.length;
    for (var i = 0; (i < g$506); i++) {
      (function (thisMenuEntry) {
        themenu.appendChild(thisMenuEntry.toHtml());
      }).call(this, this.menuEntries[i]);
    }
    return themenu;
  }).call(this, (function (ELEMENT) {
    ELEMENT.setAttribute("CLASS", "menu");
    ELEMENT.setAttribute("NAME", NIL);
    ELEMENT.setAttribute("SIZE", "1");
    return ELEMENT;
  }).call(this, document.createElement("SELECT")));
});

SexpMenuEntry.prototype.toHtml = (function () {
  return (function (commandId, titleString) {
    return (function (ELEMENT) {
      ELEMENT.setAttribute("VALUE", "command-id");
      ELEMENT.appendChild((function () {
        if ((("" + typeof(titleString)) == ("" + "string"))) {
          return document.createTextNode(titleString);
        } else {
          return titleString;
        }
      }).call(this));
      return ELEMENT;
    }).call(this, document.createElement("OPTION"));
  }).call(this, this.commandId, this.titleString);
});

SexpSymbol.prototype.toHtml = (function () {
  return (function (name, package) {
    return (function (ELEMENT) {
      ELEMENT.setAttribute("CLASS", "symbol");
      ELEMENT.appendChild((function () {
        if ((("" + typeof(name)) == ("" + "string"))) {
          return document.createTextNode(name);
        } else {
          return name;
        }
      }).call(this));
      return ELEMENT;
    }).call(this, document.createElement("SPAN"));
  }).call(this, this.name, this.package);
});

SexpNumber.prototype.toHtml = (function () {
  return (function (value) {
    return (function (ELEMENT) {
      ELEMENT.setAttribute("CLASS", "value");
      ELEMENT.appendChild((function () {
        if ((("" + typeof(value)) == ("" + "string"))) {
          return document.createTextNode(value);
        } else {
          return value;
        }
      }).call(this));
      return ELEMENT;
    }).call(this, document.createElement("SPAN"));
  }).call(this, this.value);
});

SexpString.prototype.toHtml = (function () {
  return (function (value) {
    return (function (ELEMENT) {
      ELEMENT.setAttribute("CLASS", "value");
      ELEMENT.appendChild((function () {
        if ((("" + typeof(value)) == ("" + "string"))) {
          return document.createTextNode(value);
        } else {
          return value;
        }
      }).call(this));
      return ELEMENT;
    }).call(this, document.createElement("SPAN"));
  }).call(this, this.value);
});

SexpT.prototype.toHtml = (function () {
  return (function (value) {
    return (function (ELEMENT) {
      ELEMENT.setAttribute("CLASS", "value");
      ELEMENT.appendChild((function () {
        if ((("" + typeof(value)) == ("" + "string"))) {
          return document.createTextNode(value);
        } else {
          return value;
        }
      }).call(this));
      return ELEMENT;
    }).call(this, document.createElement("SPAN"));
  }).call(this, this.value);
});

SexpHole.prototype.toHtml = (function () {
  return (function (hole) {
    hole.sexp = this;
    (function (type) {
      if (this.listItems) {
        var g$518 = this.listItems;
        for (g$517 in g$518) {
          (function (item) {
            hole.appendChild(item.toHtml());
          }).call(this, g$518[g$517]);
        }
      } else {
        if (this.contents) {
          hole.appendChild(this.contents.toHtml());
        } else {
          if (this.content) {
            if ((("" + type) == ("" + "symbol"))) {
              hole.appendChild(this.content.toHtml());
            } else {
              if ((("" + type) == ("" + "number"))) {
                hole.appendChild(this.content.toHtml());
              } else {
                if ((("" + type) == ("" + "string"))) {
                  hole.appendChild(this.content.toHtml());
                } else {
                  if ((("" + type) == ("" + "boolean"))) {
                    hole.appendChild(this.content.toHtml());
                  } else {
                    if ((("" + type) == ("" + "t"))) {
                      hole.appendChild(this.content.toHtml());
                    } else {
                      if (true) {
                        alert(("Hole contains: " + type));
                      }
                    }
                  }
                }
              }
            }
          } else {
            if (this.value) {
              this.value.toHtml().appendChild();
            } else {
              if (this.name) {
                (function (name) {
                  hole.appendChild((function (ELEMENT) {
                    ELEMENT.appendChild((function () {
                      if ((("" + typeof(name)) == ("" + "string"))) {
                        return document.createTextNode(name);
                      } else {
                        return name;
                      }
                    }).call(this));
                    return ELEMENT;
                  }).call(this, document.createElement("I")));
                }).call(this, this.name);
              }
            }
          }
        }
      }
    }).call(this, this.type);
    setupHole(hole, this);
    addMenuBox(hole);
    return hole;
  }).call(this, (function () {
    if (this.inline) {
      return (function (ELEMENT) {
        ELEMENT.setAttribute("CLASS", "hole");
        return ELEMENT;
      }).call(this, document.createElement("SPAN"));
    } else {
      return (function (ELEMENT) {
        ELEMENT.setAttribute("CLASS", "hole");
        return ELEMENT;
      }).call(this, document.createElement("DIV"));
    }
  }).call(this));
});

SexpMultiHole.prototype.toHtml = (function () {
  return (function (hole) {
    hole.sexp = this;
    (function (contents, template) {
      if ((contents.length > 0)) {
        var g$529 = this.contents.length;
        for (var i = 0; (i < g$529); i++) {
          hole.appendChild(this.contents[i].toHtml());
          setupHole(hole, this);
        }
        if (((("" + this.type) == ("" + "list")) || (("" + this.type) == ("" + "multi")))) {
          (function (ellipses) {
            ellipses.onclick = (function () {
              var g$531 = template.length;
              for (var i = 0; (i < g$531); i++) {
                (function (thing) {
                  contents.push(thing);
                  hole.insertBefore(thing.toHtml(), ellipses);
                }).call(this, xmlToSexp(template[i]));
              }
              return undefined;
            });
            hole.appendChild(ellipses);
          }).call(this, (function (ELEMENT) {
            ELEMENT.appendChild(document.createTextNode("..."));
            return ELEMENT;
          }).call(this, document.createElement("I")));
        }
      } else {
        if ((template.length > 0)) {
          var g$533 = this.template.length;
          for (var i = 0; (i < g$533); i++) {
            (function (thing) {
              thing.parent = this;
              thing.index = i;
              this.contents.push(thing);
              hole.appendChild(thing.toHtml());
            }).call(this, xmlToSexp(this.template[i]));
          }
          if (((("" + this.type) == ("" + "list")) || (("" + this.type) == ("" + "multi")))) {
            (function (ellipses) {
              ellipses.onclick = (function () {
                var g$535 = template.length;
                for (var i = 0; (i < g$535); i++) {
                  (function (thing) {
                    contents.push(thing);
                    hole.insertBefore(thing.toHtml(), ellipses);
                  }).call(this, xmlToSexp(template[i]));
                }
                return undefined;
              });
              hole.appendChild(ellipses);
            }).call(this, (function (ELEMENT) {
              ELEMENT.appendChild(document.createTextNode("..."));
              return ELEMENT;
            }).call(this, document.createElement("I")));
          }
        } else {
          if (true) {
            alert(("What the...?!?!"));
          }
        }
      }
    }).call(this, this.contents, this.template);
    addMenuBox(hole);
    return hole;
  }).call(this, (function () {
    if (this.inline) {
      return (function (ELEMENT) {
        ELEMENT.setAttribute("CLASS", "mhole");
        return ELEMENT;
      }).call(this, document.createElement("SPAN"));
    } else {
      return (function (ELEMENT) {
        ELEMENT.setAttribute("CLASS", "mhole");
        return ELEMENT;
      }).call(this, document.createElement("DIV"));
    }
  }).call(this));
});

SexpList.prototype.toXml = (function () {
  return (function (xmlList) {
    var g$548 = this.listItems;
    for (g$547 in g$548) {
      (function (thing) {
        if ((("" + thing.type) == ("" + "multi"))) {
          var g$554 = thing.contents;
          for (g$553 in g$554) {
            (function (item) {
              xmlList.appendChild(item.toXml());
            }).call(this, g$554[g$553]);
          }
        } else {
          xmlList.appendChild(thing.toXml());
        }
      }).call(this, g$548[g$547]);
    }
    return xmlList;
  }).call(this, (function (ELEMENT) {
    return ELEMENT;
  }).call(this, document.createElement("LIST")));
});

SexpMenu.prototype.toXml = (function () {
  return (function (xmlList) {
    var g$561 = this.menuEntries;
    for (g$560 in g$561) {
      (function (thing) {
        xmlList.appendChild(thing.toXml());
      }).call(this, g$561[g$560]);
    }
    return xmlList;
  }).call(this, (function (ELEMENT) {
    return ELEMENT;
  }).call(this, document.createElement("LIST")));
});

SexpMenuEntry.prototype.toXml = (function () {
  return (function (package, name) {
    if (package) {
      return (function (ELEMENT) {
        ELEMENT.setAttribute("PACKAGE", package);
        ELEMENT.appendChild((function () {
          if ((("" + typeof(name)) == ("" + "string"))) {
            return document.createTextNode(name);
          } else {
            return name;
          }
        }).call(this));
        return ELEMENT;
      }).call(this, document.createElement("SYMBOL"));
    } else {
      return (function (ELEMENT) {
        ELEMENT.appendChild((function () {
          if ((("" + typeof(name)) == ("" + "string"))) {
            return document.createTextNode(name);
          } else {
            return name;
          }
        }).call(this));
        return ELEMENT;
      }).call(this, document.createElement("SYMBOL"));
    }
  }).call(this, this.package, this.name);
});

SexpSymbol.prototype.toXml = (function () {
  return (function (package, name) {
    if (package) {
      return (function (ELEMENT) {
        ELEMENT.setAttribute("PACKAGE", package);
        ELEMENT.appendChild((function () {
          if ((("" + typeof(name)) == ("" + "string"))) {
            return document.createTextNode(name);
          } else {
            return name;
          }
        }).call(this));
        return ELEMENT;
      }).call(this, document.createElement("SYMBOL"));
    } else {
      return (function (ELEMENT) {
        ELEMENT.appendChild((function () {
          if ((("" + typeof(name)) == ("" + "string"))) {
            return document.createTextNode(name);
          } else {
            return name;
          }
        }).call(this));
        return ELEMENT;
      }).call(this, document.createElement("SYMBOL"));
    }
  }).call(this, this.package, this.name);
});

SexpNumber.prototype.toXml = (function () {
  return (function (value) {
    return (function (ELEMENT) {
      ELEMENT.appendChild((function () {
        if ((("" + typeof(value)) == ("" + "string"))) {
          return document.createTextNode(value);
        } else {
          return value;
        }
      }).call(this));
      return ELEMENT;
    }).call(this, document.createElement("NUMBER"));
  }).call(this, this.value);
});

SexpString.prototype.toXml = (function () {
  return (function (value) {
    return (function (ELEMENT) {
      ELEMENT.appendChild((function () {
        if ((("" + typeof(value)) == ("" + "string"))) {
          return document.createTextNode(value);
        } else {
          return value;
        }
      }).call(this));
      return ELEMENT;
    }).call(this, document.createElement("STRING"));
  }).call(this, this.value);
});

SexpT.prototype.toXml = (function () {
  return (function (value) {
    return (function (ELEMENT) {
      ELEMENT.appendChild((function () {
        if ((("" + typeof(value)) == ("" + "string"))) {
          return document.createTextNode(value);
        } else {
          return value;
        }
      }).call(this));
      return ELEMENT;
    }).call(this, document.createElement("T"));
  }).call(this, this.value);
});

SexpHole.prototype.toXml = (function () {
  if (this.content) {
    return this.content.toXml();
  } else {
    return (function (ELEMENT) {
      return ELEMENT;
    }).call(this, document.createElement("UNFILLED-HOLE"));
  }
});

SexpMultiHole.prototype.toXml = (function () {
  if (this.contents) {
    return this.contents.toXml();
  } else {
    return (function (ELEMENT) {
      return ELEMENT;
    }).call(this, document.createElement("UNFILLED-HOLE"));
  }
});

window.xmlToSexp = (function (xml) {
  return (function (type) {
    if (((type - 0) == (NodeTypes.ELEMENT_NODE - 0))) {
      return xmlElementToObject(xml);
    } else {
      if (((type - 0) == (NodeTypes.TEXT_NODE - 0))) {
        return xml.data;
      } else {
        if (((type - 0) == (NodeTypes.DOCUMENT_NODE - 0))) {
          return xmlToSexp(xml.documentElement);
        } else {
          if (true) {
            if (((1 <= type) && (type <= 12))) {
              return alert((NodeTypes.names[type] + " not supported"));
            } else {
              return alert(("Unrecognized nodeType " + type));
            }
          } else {
            return undefined;
          }
        }
      }
    }
  }).call(this, xml.nodeType);
});

window.xmlElementToObject = (function (element) {
  return (function (tag) {
    return (function (obj) {
      obj.buildFromXml(element);
      return obj;
    }).call(this, (function () {
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
          } else {
            if (stringEqual(tag, "SYMBOL")) {
              return new SexpSymbol();
            } else {
              if (stringEqual(tag, "NUMBER")) {
                return new SexpNumber();
              } else {
                if (stringEqual(tag, "STRING")) {
                  return new SexpString();
                } else {
                  if (stringEqual(tag, "HOLE")) {
                    return new SexpHole();
                  } else {
                    if (stringEqual(tag, "MHOLE")) {
                      return new SexpMultiHole();
                    } else {
                      return undefined;
                    }
                  }
                }
              }
            }
          }
        }
      }
    }).call(this));
  }).call(this, element.nodeName);
});

window.stringEqual = (function (s1, s2) {
  return (("" + s1.toUpperCase()) == ("" + s2.toUpperCase()));
});

SexpList.prototype.buildFromXml = (function (element) {
  return (function (children) {
    var g$576 = children.length;
    for (var i = 0; (i < g$576); i++) {
      (function (child) {
        if (!((((child.nodeType - 0) == (3 - 0)) && allBlank(child.data)))) {
          (function (sexp) {
            this.addListItem(sexp);
          }).call(this, xmlToSexp(child));
        }
      }).call(this, children.item(i));
    }
    return undefined;
  }).call(this, element.childNodes);
});

window.allBlank = (function (string) {
  return (function (allBlank) {
    var g$581 = string.length;
    for (var i = 0; (i < g$581); i++) {
      (function (char) {
        if ((!((char === ' ')) && !((char === '\n')) && !((char === '\t')))) {
          allBlank = false;
        }
      }).call(this, string.charAt(i));
    }
    return undefined;
  }).call(this, true);
});

SexpMenu.prototype.buildFromXml = (function (element) {
  return this.titleString = xmlToSexp(element.firstChild);
});

SexpMenuEntry.prototype.buildFromXml = (function (element) {
  return (function (first) {
    return (function (second) {
      this.commandId = xmlToSexp(second);
      return this.titleString = xmlToSexp(first);
    }).call(this, first.nextSibling());
  }).call(this, element.firstChild);
});

SexpSymbol.prototype.buildFromXml = (function (element) {
  this.name = xmlToSexp(element.firstChild);
  return this.package = element.getAttribute("package");
});

SexpNumber.prototype.buildFromXml = (function (element) {
  return this.value = xmlToSexp(element.firstChild);
});

SexpString.prototype.buildFromXml = (function (element) {
  return this.value = xmlToSexp(element.firstChild);
});

SexpHole.prototype.buildFromXml = (function (element) {
  this.name = element.getAttribute("name");
  this.type = element.getAttribute("type");
  this.editable = element.getAttribute("editable");
  this.inline = element.getAttribute("inline");
  if ((("" + this.type) == ("" + "list"))) {
    this.min = element.getAttribute("min");
    return this.max = element.getAttribute("max");
  } else {
    return undefined;
  }
});

SexpMultiHole.prototype.buildFromXml = (function (element) {
  this.name = element.getAttribute("name");
  this.type = element.getAttribute("type");
  this.editable = element.getAttribute("editable");
  this.inline = element.getAttribute("inline");
  return (function (children) {
    if ((0 < children.length)) {
      this.contents = new Array();
      this.template = new Array();
      var g$592 = children.length;
      for (var i = 0; (i < g$592); i++) {
        (function (child) {
          this.template.push(child);
        }).call(this, children.item(i));
      }
      return undefined;
    } else {
      return undefined;
    }
  }).call(this, element.childNodes);
});

