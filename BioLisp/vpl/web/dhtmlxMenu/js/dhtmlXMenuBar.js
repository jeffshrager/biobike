 

 
 

 function dhtmlXMenuBarObject(htmlObject,width,height,name,vMode,gfxPath){
 if(typeof(htmlObject)!="object")
 this.parentObject=document.getElementById(htmlObject);
 else
 this.parentObject=htmlObject;
 
 if(!this.parentObject)this.parentObject=document.body;
 
 if(!vMode)this.flsmd=true;
 this.tname=name;
 this.topMenu=this;
 this.width=width;
 this.height=height;
 this.topNod=0;
 this.ieFix=true;

 var detect = navigator.userAgent.toLowerCase();
 place = detect.indexOf('opera')+1;
 if(place)this.ieFix=false;

 this.ieWinMode=true;
 this.ieWinModeState=0;
 this.zIndex=3;
 this.maxItems=99;
 this.echsw=true;
 this.sysGfxPath=gfxPath;
 this.dividerCell=0;
 this.firstCell=0;
 this.nameCell=0;
 this.crossCell=0;
 this.echsw2=true;
 
 this.tableCSS="menuTable";
 this.titleCSS="menuName";
 this.secTableCSS="secondMenuTable";
 
 this.extraMode=convertStringToBoolean(vMode);
 
 this.defaultAction=0;
 this.onShow=0;
 this.onHide=0;
 
 var lastOpenedPanel=0;
 var lastSelectedItem=0;
 
 this.items=new Array();
 this.itemsCount=0;
 this.gitems=new Array();
 this.gitemsCount=0;
 
 this.subMenus=new Array();
 this.subMenusCount=0;
 
 
 if(!this.extraMode)
 this._createPanel();
 else
 this._createVPanel();
 
 
 this.hiddenForm=document.createElement("FORM");
 this.parentObject.appendChild(this.hiddenForm);
 this.hiddenForm.style.display="none";

 
 if(this._extendedInit)this._extendedInit();
 this.xmlUnit=new dtmlXMLLoaderObject(this._parseXMLTree,this);
 this.setMenuMode("classic");
 
 this.showBar();
 return this;
}
 
 dhtmlXMenuBarObject.prototype = new dhtmlXProtobarObject;

 

 
 function dhtmlXMenuBarPanelObject(parentPanel,parentItem,mode,minWidth,withoutImages){
 this.extraMode=!convertStringToBoolean(mode);
 this.parentPanel=parentPanel;
 this.parentItem=parentItem;
 this.parentObject=parentPanel.parentObject;
 this.topMenu=parentPanel.topMenu;
 this.topNod=0;
 this.dividerCell=0;
 this.firstCell=0;
 this.nameCell=0;
 this.crossCell=0;
 this.maxItems=99;
 this.items=new Array();
 this.itemsCount=0;
 this.withoutImages=(withoutImages==1);
 this.mixedImages=(withoutImages==2);
 if(minWidth)this.width=minWidth;

 if((!this.topMenu.flsmd)||(this.topMenu!=this.parentPanel))
 this.parentItem.setHasChild(true);
 
 if(mode)
 this._createVPanel=this.topMenu._createPanel;
 else
 this._createVPanel=this.topMenu._createVPanel;

 

 this._createVPanel();
 
 this.topNod.panel=this;
 this.topNod.onmouseover=this.topMenu._onmouseoverZ;
 this.topNod.onmouseout=this.topMenu._onmouseoutZ;
 
 parentItem.subMenu=this;
 this.topNod.style.position="absolute";
 this.topMenu.subMenus[this.topMenu.subMenusCount]=this;
 this.topMenu.subMenusCount++;
 return this;
}
 
 
 
 dhtmlXMenuBarObject.prototype.enableIESelectBoxFix=function(mode){
 this.ieFix=convertStringToBoolean(mode);
};
 
 dhtmlXMenuBarObject.prototype.enableMenuHandle=function(mode){
 if(convertStringToBoolean(mode))this.dividerCell.style.display="";
 else this.dividerCell.style.display="none";
};
 
 dhtmlXMenuBarObject.prototype.enableChildIcon=function(mode){
 this.echsw=convertStringToBoolean(mode);
};
 
 dhtmlXMenuBarObject.prototype.enableChildIconSwitching=function(mode){
 this.echsw2=convertStringToBoolean(mode);
};
 
 dhtmlXMenuBarObject.prototype.enableWindowOpenMode=function(mode){
 this.ieWinMode=convertStringToBoolean(mode);
};
 
 
 dhtmlXProtobarObject.prototype._getItemIndex=function(id){
 for(var i=0;i<this.gitemsCount;i++)
{
 if(this.gitems[i].id==id)return i;
};
 return -1;
};
 
 dhtmlXProtobarObject.prototype.getItem=function(itemId){
 var z=this._getItemIndex(itemId);
 if(z>=0)return this.gitems[z];
};
 

 
 
 dhtmlXMenuBarObject.prototype._showScroll=function(node,order,rest,mode){
 if(!node.items[order])return;
 
 if(mode=="")
 if(rest==1)node.cAr[2]++;
 else node.cAr[0]--;
 else 
 if(rest==1)node.cAr[0]++;
 else node.cAr[2]--;
 
 if(!node.items[order].CSSTag)
{
 node.items[order].topNod.parentNode.style.display=mode;
 this._showScroll(node,order*1+rest*1,rest,mode);
}
 else
{
 node.items[order].topNod.parentNode.style.display=mode;
}

}
 
 
 dhtmlXMenuBarObject.prototype._fixScrollState=function(node,rest){
 
 if(parseInt(node.topNod.offsetWidth)>parseInt(node.topNod.width))
 node.topNod.width=node.topNod.offsetWidth;
 
 var flag=0;

 if(rest>0){
 this._showScroll(node,node.cAr[0],1,"none");
 this._showScroll(node,node.cAr[2],1,"");
}
 else{
 this._showScroll(node,node.cAr[0]-1,-1,"");
 this._showScroll(node,node.cAr[2]-1,-1,"none");
}
 z.scrollState+=rest*1;
 
 
 if((node.scrollState*1+node.maxItems*1)==(node.realItemsCount))
 node.scrollDown.className="menuscrollerdisabled";
 else
{
 node.scrollDown.className="menuscrollerenabled";
 flag++;
}
 
 if(node.scrollState==0)
 node.scrollUp.className="menuscrollerdisabled";
 else
{
 node.scrollUp.className="menuscrollerenabled";
 flag++;
}
 
 
 return flag;
}



 
 dhtmlXMenuBarObject.prototype._scrollstart=function(){
 if(this.timeoutops)clearTimeout(this.timeoutops);
 z=this.parentNode.parentNode.parentNode.menu;
 if(z.scrollDown==this)
 var rest=1;
 else
 var rest=-1;
 
 if(z.topMenu._fixScrollState(z,rest)==2)
 this.timeoutops=setTimeout(new z.topMenu._delayedTimerCall(this,"onmouseover",0,0),333);
}
 
 dhtmlXMenuBarObject.prototype._scrollend=function(node){
 if(this.timeoutops)window.clearTimeout(this.timeoutops);
}
 
 dhtmlXMenuBarObject.prototype._scrollCheck=function(node){
 var z_count=0;
 for(var i=0;i<node.itemsCount;i++)
 if(node.items[i].CSSTag)z_count++;
 node.realItemsCount=z_count;
 node.cAr=new Array(0,0,0);
 if((node.realItemsCount>node.maxItems)&&(node.scrollDown)){
 node.scrollDown.style.display="";
 node.scrollUp.style.display="";
 node.scrollDown.className='menuscrollerenabled';
 node.scrollState=0;
 node.scrollUp.onmouseover=this._scrollstart;
 node.scrollUp.onmouseout=this._scrollend;
 node.scrollDown.onmouseover=this._scrollstart;
 node.scrollDown.onmouseout=this._scrollend;
 
 var z_count=0;
 for(var i=0;i<node.itemsCount;i++)
{
 if(node.items[i].CSSTag)z_count++;
 if(z_count>node.maxItems){
 node.items[i].topNod.parentNode.style.display="none";
 if(node.cAr[2]==0)node.cAr[2]=i;}
 
}
}
}

 
 dhtmlXMenuBarObject.prototype.getPanel=function(panelId){
 var z=this._getGItemIndex(panelId);
 if(z<0)return this;
 else return this.gitems[z].subMenu;
}
 
 
 dhtmlXMenuBarObject.prototype.addItem=function(panel,item){
 if(this==panel)this.addFirstLevel(panel,item);
 else this.addSecondLevel(panel,item);
}
 
 
 dhtmlXMenuBarObject.prototype._addItem=function(panel,item){
 panel.items[panel.itemsCount]=item;
 panel.firstCell.parentNode.insertBefore(item.getTopNode(),panel.firstCell);
 item.getTopNode().style.marginBottom="20px";
 item.parentNod=this;
 item.parentPanel=panel;
 if(this.defaultAction)item.setAction(this.defaultAction);
 panel.itemsCount++;

 this.gitems[this.gitemsCount]=item;
 this.gitemsCount++;
}
 
 
 dhtmlXMenuBarObject.prototype.addItem_vertical=function(panel,item){
 panel.items[panel.itemsCount]=item;
 var tr=document.createElement("tr");
 tr.style.verticalAlign="top";
 tr.appendChild(item.getTopNode());
 panel.firstCell.parentNode.insertBefore(tr,panel.firstCell);
 item.parentNod=this;
 item.parentPanel=panel;
 if(this.defaultAction)item.setAction(this.defaultAction);
 panel.itemsCount++;

 this.gitems[this.gitemsCount]=item;
 this.gitemsCount++;
}

 
 dhtmlXProtobarObject.prototype._getGItemIndex=function(id){
 for(var i=0;i<this.gitemsCount;i++)
{
 if(this.gitems[i].id==id)return i;
};
 return -1;
};
 
 
 dhtmlXMenuBarObject.prototype.removeItem=function(id){
 var z=this._getGItemIndex(id);
 if(z>=0){
 var panel=this.gitems[z].parentPanel;
 if(this.gitems[z].removeItem)this.gitems[z].removeItem();
 
 if(panel.firstCell.tagName=="TR")
 panel.firstCell.parentNode.removeChild(this.gitems[z].getTopNode().parentNode);
 else
 panel.firstCell.parentNode.removeChild(this.gitems[z].getTopNode());
 
 
 
 
 panel.itemsCount--;var j=0;
 for(var i=0;i<panel.itemsCount;i++)
 if(panel.items[i]==this.gitems[z])
 this.gitems[i]=this.gitems[i+1];
 
 this.gitems[z]=0;
 this.gitemsCount--;
 for(var i=z;i<this.gitemsCount;i++)
 this.gitems[i]=this.gitems[i+1];
 
}
}
 
 
 dhtmlXMenuBarObject.prototype._parseXMLTree=function(that,node,level,parentNode,aTempNode,mode){
 if(!node){
 node=that.xmlUnit.getXMLTopNode("menu");
 level=0;
 parentNode=that;
 
 mode=node.getAttribute("mode");
 if(mode)that.setMenuMode(mode);
 
 var menuAlign=node.getAttribute("menuAlign");
 if(menuAlign)that.setBarAlign(menuAlign);
 
 that.maxItems=node.getAttribute("maxItems")||99;
 var absolutePosition=node.getAttribute("absolutePosition");
 var aleft=node.getAttribute("left");
 var atop=node.getAttribute("top");
 if(absolutePosition)that.topNod.style.top=atop || 0;
 if(absolutePosition)that.topNod.style.left=aleft || 0;
 if(absolutePosition=="yes")that.topNod.style.position="absolute";
 
 var name=node.getAttribute("name");
 if(name)that.setTitleText(name);

 var width=node.getAttribute("width");
 var height=node.getAttribute("height");
 that.setBarSize(width,height);

 var imageTextButtonCssClass=node.getAttribute("imageTextButtonCssClass");
 var globalTextCss=node.getAttribute("globalTextCss");
 
 that.globalSecondCss=node.getAttribute("globalSecondCss");
 that.globalCss=node.getAttribute("globalCss");
 that.globalTextCss=node.getAttribute("globalTextCss");
 if(node.getAttribute("withoutImages"))that.withoutImages=true;
 if(node.getAttribute("mixedImages"))that.mixedImages=true;
 that.type=node.getAttribute("type")||"a1";
 
}
 

 
 
 if(level){
 var parentNode=new dhtmlXMenuBarPanelObject(parentNode,aTempNode,((mode!="classic")&&(mode!="popup")),node.getAttribute("panelWidth"),(node.getAttribute("withoutImages")?1:(node.getAttribute("mixedImages")?2:0)));
 parentNode.maxItems=node.getAttribute("maxItems")||that.maxItems;
}

 for(var i=0;i<node.childNodes.length;i++)
{
 if(node.childNodes[i].nodeType==1)
{
 var localItem=node.childNodes[i]
 
 if(!level)
{
 if((!localItem.getAttribute("className"))&&(that.globalCss))
 localItem.setAttribute("className",that.globalCss);
}
 else
 if(!localItem.getAttribute("className"))
{
 if(that.globalSecondCss)
 localItem.setAttribute("className",that.globalSecondCss);
 else 
 localItem.setAttribute("className","menuButtonSecond");
}
 
 
 
 if((!localItem.getAttribute("textClassName"))&&(that.globalTextCss))
 localItem.setAttribute("textClassName",that.globalTextCss);

 
 var tempsrc=localItem.getAttribute("src");
 if(parentNode.withoutImages)
 localItem.setAttribute("src","");
 else 
{
 if((parentNode.mixedImages)&&(tempsrc==null))
 tempsrc=that.sysGfxPath+"blank.gif";
 else 
 tempsrc=that.sysGfxPath+(tempsrc||"blank.gif");
 
 localItem.setAttribute("src",tempsrc);
}
 tempsrc=localItem.getAttribute("wide");
 if(tempsrc==null){
 localItem.setAttribute("width","100%");
}
 
 
 var z=eval("window.dhtmlX"+localItem.tagName+"Object");
 if(z)
 var TempNode= new z(localItem,node.getAttribute("type")||that.type);
 else 
 var TempNode=null;
 if(localItem.tagName=="divider")
 if(level)
 that.addItem(parentNode,new dhtmlXMenuDividerYObject(localItem.getAttribute("id")));
 else
 that.addItem(parentNode,new dhtmlXMenuDividerXObject(localItem.getAttribute("id")));
 else
 if(TempNode)
 if(level)
 that.addItem(parentNode,TempNode);
 else
 that.addItem(parentNode,TempNode);
 

 if(localItem.childNodes.length)that._parseXMLTree(that,localItem,level+1,parentNode,TempNode,mode);
}
}

 that._scrollCheck(parentNode);


 if((level==0)&&(that.topMenu.waitCall))that.topMenu.waitCall();
}


 
 
 dhtmlXMenuBarObject.prototype._createPanel=function()
{
 if(!this.width)this.width=1;
 if(!this.height)this.height=1;
 
 var div=document.createElement("div");
 div.innerHTML='<table cellpadding="0" cellspacing="0" class="'+this.topMenu.tableCSS+'" style="display:none" width="'+this.width+'" height="'+this.height+'"><tbody>'+
 '<tr>'+
 '<td width="3px" style="display:none"><div class="menuHandle">&nbsp;</div></td>'+
 '<td class="'+this.topMenu.titleCSS+'" style="display:none">'+this.topMenu.tname+'</td>'+
 '<td></td>'+
 '<td align="right" width="100%" class="'+this.topMenu.titleCSS+'">&nbsp;'+this.topMenu.tname+'</td>'+
 '<td >&nbsp;</td>'+
 '</tr></tbody></table>';
 var table=div.childNodes[0];
 table.setAttribute("UNSELECTABLE","on");
 table.onselectstart=this.topMenu.badDummy;
 this.topNod=table;
 this.dividerCell=table.childNodes[0].childNodes[0].childNodes[0];
 this.dividerCell.menu=this;
 this.preNameCell=this.dividerCell.nextSibling;
 this.firstCell=this.preNameCell.nextSibling;
 this.nameCell=this.firstCell.nextSibling;
 this.crossCell=this.nameCell.nextSibling;
 if(this.topMenu!=this){
 this.dividerCell.style.display="none";
 this.preNameCell.style.display="none";
 this.nameCell.style.display="none";
 this.crossCell.style.display="none";
 table.className=this.topMenu.secTableCSS;
}
 this.topNod.style.zIndex=this.topMenu.zIndex;
 
 if(this.topMenu.ieFix)
{
 var iframe=document.createElement("IFRAME");
 if(this.topMenu.sysGfxPath)iframe.src=this.topMenu.sysGfxPath+"blank.html";
 iframe.style.zIndex=this.topMenu.zIndex-1;iframe.style.position="absolute";
 iframe.style.display="none";iframe.scrolling="no";iframe.frameBorder=0;
 this.parentObject.appendChild(iframe);
 this.topNod.ieFix=iframe;
}
 
 this.parentObject.appendChild(table);
};
 
 
 
 dhtmlXMenuBarObject.prototype.setMenuCSS=function(table,title,secTable){
 this.tableCSS=table;
 this.titleCSS=title;
 this.secTableCSS=secTable;
 this.topNod.className=this.tableCSS;
 this.preNameCell.className=this.titleCSS;
 this.nameCell.className=this.titleCSS;
 
}
 
 
 dhtmlXMenuBarObject.prototype._createVPanel=function()
{
 if(!this.width)this.width=120;
 if(!this.height)this.height=20;

 var div=document.createElement("div");
 div.innerHTML='<table cellpadding="0" cellspacing="0" class="'+this.topMenu.tableCSS+'" style="display:none" width="'+this.width+'" ><tbody>'+
 '<tr ><td class="menuscrollerdisabled" style="display:none"><img src="'+this.topMenu.sysGfxPath+'btn_up1.gif"></td></tr>'+
 '<tr><td class="'+this.topMenu.titleCSS+'" style="display:none">'+this.tname+'</td></tr>'+
 '<tr><td></td></tr>'+
 '<tr><td class="menuscrollerdisabled" style="display:none"><img src="'+this.topMenu.sysGfxPath+'btn_up2.gif"></td></tr>'+
 '<tr><td align="right" class="'+this.topMenu.titleCSS+'" style="display:none">'+this.tname+'</td></tr>'+
 '<tr><td></td></tr>'+
 '</tbody></table>';

 var table=div.childNodes[0];
 this.topNod=table;
 table.onselectstart=this.topMenu.badDummy;
 table.setAttribute("UNSELECTABLE","on");

 this.dividerCell=table.childNodes[0].childNodes[0].childNodes[0];
 table.menu=this;
 this.scrollUp=this.dividerCell;
 this.scrollonmouseover="";
 this.preNameCell=table.childNodes[0].childNodes[1].childNodes[0];
 this.firstCell=table.childNodes[0].childNodes[2];
 this.scrollDown=table.childNodes[0].childNodes[3].childNodes[0];
 this.nameCell=table.childNodes[0].childNodes[4].childNodes[0];
 this.crossCell=table.childNodes[0].childNodes[5].childNodes[0];

 if(this.topMenu!=this){
 this.preNameCell.parentNode.style.display="none";
 this.nameCell.parentNode.style.display="none";
 this.crossCell.parentNode.style.display="none";
 table.className=this.topMenu.secTableCSS;
}
 
 this.topNod.style.zIndex=this.topMenu.zIndex;

 if(this.topMenu.ieFix)
{
 var iframe=document.createElement("IFRAME");
 iframe.style.zIndex=this.topMenu.zIndex-1;iframe.style.position="absolute";
 iframe.style.display="none";iframe.scrolling="no";iframe.frameBorder=0;
 this.parentObject.appendChild(iframe);
 this.topNod.ieFix=iframe;
}
 this.parentObject.appendChild(table);
};
 
 
 
 

 
 
 
 
 function dhtmlXMenuItemObject(id,text,width,src,className,disableImage,href,target,type){
 type=type||"a1";
 if(id.tagName=="MenuItem")
{
 type=text||"a1";
 src=id.getAttribute("src");
 text=id.getAttribute("name");
 className=id.getAttribute("className");
 disableImage=id.getAttribute("disableImage");
 width=id.getAttribute("width");
 href=id.getAttribute("href");
 target=id.getAttribute("target");
 var tooltip=id.getAttribute("tooltip");
 id=id.getAttribute("id");
}
 if(id)this.id=id;
 else this.id=(new Date()).valueOf();
 


 this.topNod=0;
 this.action=0;
 this.persAction=0;
 this.src=src;
 this.text=text;
 this.href=href;
 this.target=target;

 this.className=className||"menuButton";
 this.textclassName="defaultMenuText";
 
 this.disableImage=disableImage;
 
 td=document.createElement("td");
 this.topNod=td;td.align="center";
 td.noWrap=true;
 
 td.innerHTML="<table align='left' cellpadding='0' cellspacing='0' border='0' "+(width?("width='"+width+"'"):"")+" height='100%'><tr><td width='20px' style=' "+(src?"":"display:none;")+"'><img src='"+src+"' border='0' width='18px' height='18px'></td><td width='100%' align='left' style=' "+(src?" padding-left:2px;":"")+" overflow:hidden;' ><table width='100%' height='100%' cellpadding='0' cellspacing='0'><tr><td title='"+tooltip+"' class='"+this.textclassName+"' nowrap >"+this.text+"</td><td width='12px'><img style='display:none'></td></tr></table></td></tr></table>";
 this.imageTag=td.childNodes[0].childNodes[0].childNodes[0].childNodes[0].childNodes[0];
 this.childMenuTag=td.childNodes[0].childNodes[0].childNodes[0].childNodes[1].childNodes[0].childNodes[0].childNodes[0].childNodes[1].childNodes[0];
 
 switch(type){
 case "a1":
 this.CSSTag=td;
 this.CSSImageTag=null;
 break;
 case "a2":
 this.CSSTag=td.childNodes[0];
 this.CSSImageTag=null;
 break;
 case "a3":
 this.CSSTag=td.childNodes[0].childNodes[0].childNodes[0].childNodes[1];
 this.CSSImageTag=null;
 break;
 case "b1":
 this.CSSTag=td;
 this.CSSImageTag=this.imageTag.parentNode;
 break;
 case "b2":
 this.CSSTag=td.childNodes[0];
 this.CSSImageTag=this.imageTag.parentNode;
 break;
 case "b3":
 this.CSSTag=td.childNodes[0].childNodes[0].childNodes[0].childNodes[1];
 this.CSSImageTag=this.imageTag.parentNode;
 break;
}
 td.id="menuItem_"+this.id;
 this.CSSTag.className=this.className;
 td.objectNode=this;
 this.enable();
 return this;
};
 
 
 
 dhtmlXMenuItemObject.prototype=new dhtmlXButtonPrototypeObject;

 
 dhtmlXMenuItemObject.prototype.enable=function(){
 if(this.disableImage)this.imageTag.src=this.src;
 else 
 if(!this.className)
 this.topNod.className=this.objectNode.className;
 else 
 this.topNod.className=this.className;

 if(this.textTag)
 this.textTag.className=this.textClassName;
 
 this.topNod.onmouseover=this._onmouseoverX;
 this.topNod.onmouseout=this._onmouseoutX;
 this.topNod.onmousedown=this._onmousedownX;
 this.topNod.onmouseup=this._onclickX;
};
 
 
 dhtmlXMenuItemObject.prototype._onmousedownX=function(e){if((!this.objectNode.parentPanel.parentPanel)&&(this.objectNode.subMenu))this.objectNode._onclickX(e,this.objectNode);};
 
 
 dhtmlXMenuItemObject.prototype.setHasChild = function(mode){
 if((convertStringToBoolean(mode))&&(this.parentPanel.topMenu.echsw)){
 this.childMenuTag.src=this.parentPanel.topMenu.sysGfxPath+'btn_rt1.gif';
 this.childMenuTag.style.display='';
}
 else this.childMenuTag.style.display='none';
};
 
 
 dhtmlXMenuItemObject.prototype.setText = function(newText){
 this.topNod.childNodes[0].childNodes[0].childNodes[0].childNodes[1].innerHTML=newText;
};
 

 dhtmlXMenuItemObject.prototype._onclickX=function(e,that){
 if(!e)e=event;
 e.cancelBubble=true;

 if(!that)that=this.objectNode;
 var thatM=that.parentPanel.topMenu;
 if(that.topNod.dstatus)return;
 
 if((thatM.ieWinMode)&&(!thatM.ieWinModeState)&&(that.subMenu))
{
 that.parentPanel.topMenu._onItemOver(that,that.subMenu);
 if(document.body.currentActiveMenu!=thatM)
 if(document.body.onmouseup){document.body.onmouseup();}
 
 if(thatM.realWinModeStart)
{

 that.parentPanel.topMenu.ieWinModeState=1;
 

 if(document.body.onmouseup!=thatM._onclickclose)
{
 thatM.olddoc=document.body.onmouseup;
 document.body.onmouseup=thatM._onclickclose;
}

 document.body.currentActiveMenu=that.parentPanel.topMenu;
 
 thatM.realWinModeStart=0;
}
 else thatM.realWinModeStart=1;
 return;
}
 
 if((thatM.ieWinMode)&&(thatM.ieWinModeState)&&(that.subMenu)){
 if(!thatM.realWinModeStart)
{
 thatM.realWinModeStart=1;
 return;
}
 else
 thatM.realWinModeStart=0;
}
 
 thatM.realWinModeStart=0;
 thatM.ieWinModeState=0;

 thatM.dropTimers(that.parentPanel);
 thatM._closePanel(that.parentPanel);
 thatM.lastSelectedItem=0;
 thatM.probablyCloseMe=0;
 thatM.lastOpenedPanel="";
 
 if(that.parentPanel.topMenu.ieWinMode){
 if(that.parentPanel.topMenu.olddoc)
 document.body.onclick=that.parentPanel.topMenu.olddoc;
 else
 document.body.onclick=null;
}

 if((that.parentPanel.topMenu.ieWinMode)&&(!that.parentPanel.parentPanel)&&(that.subMenu))
{
 return;
}
 
 that.parentPanel.topMenu._unMarkItem(that,"down");
 
 setTimeout(new that.parentPanel.topMenu._delayedTimerCall(that,"_onclickY",0,0),100);
 return;
 
};
 
 dhtmlXMenuBarObject.prototype._onclickclose=function(e){
 var that=this.currentActiveMenu;
 if(that.olddoc)
 document.body.onmouseup=that.olddoc;
 else
 document.body.onmouseup=null;
 that.realWinModeStart=0;
 that.lastOpenedPanel="";
 that.ieWinModeState=0;
 that.dropTimers(that);
 that._closePanel(that);
 that.lastSelectedItem=0;
}
 
 dhtmlXMenuItemObject.prototype._onclickY=function(){
 if(this.href){
 if(this.target){
 var form=this.parentPanel.topMenu.hiddenForm;
 form.action=this.href;
 form.target=this.target;
 form.method="GET";
 form.submit();
}
 else document.location.href=this.href;
 return false;
}
 if((!this.persAction)||(this.persAction()))
 if(this.action){this.action(this.id);}
 return false;
};
 
 
 dhtmlXMenuItemObject.prototype._onmouseoverY=function(e){
 var that=this.parentPanel.topMenu;
 if((!that.ieWinMode)||(that.ieWinModeState))
 this.parentPanel.topMenu._onItemOver(this,this.subMenu);
 else
{
 if((that.lastSelectedItem)&&(that.lastSelectedItem!=item))
 this.parentPanel.topMenu._unMarkItem(that.lastSelectedItem);
 
 this.parentPanel.topMenu._markItem(this);
 
}
};
 
 
 dhtmlXMenuItemObject.prototype._onmouseoutY=function(e){
 this.parentPanel.topMenu._onItemOut(this,this.subMenu);
};

 
 dhtmlXMenuBarObject.prototype.dropTimers=function(panel){
 if(!panel)return;
 z=panel.topNod.timeoutop;
 if(z)clearTimeout(z);
 
 if(!panel.parentPanel)return;
 var z=panel.parentPanel.topNod.timeoutop;
 if(z)clearTimeout(z);
 
 var z=panel.parentItem.topNod.timeoutop;
 if(z)clearTimeout(z);
 
 
};

 
 dhtmlXMenuBarObject.prototype._onmouseoverZ=function(e){
 if(!e)e=event;e.cancelBubble=true;
 that=this.panel.topMenu;
 if(this.timeoutop)clearTimeout(this.timeoutop);
 that.dropTimers(this.panel);
 that._onItemOver(0,this.panel);
 
};

 
 dhtmlXMenuBarObject.prototype._onmouseoutZ=function(e){
 if(!e)e=event;e.cancelBubble=true;
 that=this.panel.topMenu;
 if(this.timeoutop)clearTimeout(this.timeoutop);
 
 if((!that.ieWinMode))
{
 this.timeoutop=setTimeout(new that._delayedTimerCall(this.panel.topMenu,"_onItemOut",0,this.panel),200);
}
};
 
 
 dhtmlXMenuBarObject.prototype._delayedTimerCall=function(object,functionName,a,b,time){
 this.callFunc=function(){
 var ax=a;
 var bx=b;
 object[functionName](ax,bx);
}
 return this.callFunc;
}
 
 
 
 dhtmlXMenuBarObject.prototype._onItemOut=function(item,panel){
 
 if(!panel){
 if((item)&&(!item.subMenu))
 this._unMarkItem(item);
 return;
}
 if(this.ieWinMode)
 if((panel.topMenu==panel)||((item)&&(this.ieWinModeState==1)))return;
 if(item)this._unMarkItem(item);
 this._closePanel();
}
 
 
 dhtmlXMenuBarObject.prototype._onItemOver=function(item,panel){
 if(item){
 if((this.lastSelectedItem)&&(this.lastSelectedItem!=item))
{
 if((!this.lastSelectedItem.subMenu)||(this.lastSelectedItem.subMenu!=item.parentPanel))
 this._unMarkItem(this.lastSelectedItem);
}
 
 this.lastSelectedItem=item;
 
 if(this.ieWinMode)
{
 this._markItem(item,"down");
}
 else 
{
 this._markItem(item);
}
 var zp=item.parentPanel;
 if((zp._lastSelectedItem)&&(zp._lastSelectedItem!=item))
 if(zp._lastSelectedItem.subMenu)
 this._closePanel(zp._lastSelectedItem.subMenu);

 
 item.parentPanel._lastSelectedItem=item;
}
 if(panel)this._openPanel(panel);
}

 
 dhtmlXMenuBarObject.prototype._openPanel=function(panel){
 
 if((this.lastOpenedPanel)&&(this.lastOpenedPanel!=panel)&&(this.lastOpenedPanel.parentPanel!=panel)&&(this.lastOpenedPanel!=panel.parentPanel))
{
 this._closePanel(this.lastOpenedPanel);
}
 
 var z=panel.topNod.timeoutop;
 if(z)clearTimeout(z);
 if(panel.topNod.style.display=="")return;
 if(this.lastOpenedPanel!=panel)
{
 this.lastOpenedPanel=panel;
 switch(this.modeValue){
 case "classic":
 if(panel.topMenu!=panel.parentPanel){
 panel.topNod.style.left=getAbsoluteLeft(panel.parentItem.topNod)*1+panel.parentItem.topNod.offsetWidth*1;
 panel.topNod.style.top=getAbsoluteTop(panel.parentItem.topNod);}
 else{
 panel.topNod.style.left=getAbsoluteLeft(panel.parentItem.topNod);
 panel.topNod.style.top=getAbsoluteTop(panel.parentItem.topNod)*1+panel.parentItem.topNod.offsetHeight*1-1;}
 break;
 case "popup":
 panel.topNod.style.left=getAbsoluteLeft(panel.parentItem.topNod)*1+panel.parentItem.topNod.offsetWidth*1;
 panel.topNod.style.top=getAbsoluteTop(panel.parentItem.topNod);
 break;
 case "betta":
 if(panel.topMenu!=panel.parentPanel){
 panel.topNod.style.left=getAbsoluteLeft(panel.parentItem.topNod)*1;
 panel.topNod.style.top=getAbsoluteTop(panel.parentItem.topNod)+panel.parentItem.topNod.offsetHeight*1-1;}
 else{
 panel.topNod.style.left=getAbsoluteLeft(panel.parentItem.topNod)*1+panel.parentItem.topNod.offsetWidth*1;
 panel.topNod.style.top=getAbsoluteTop(panel.parentItem.topNod);}
 break;
 case "alfa":
 panel.topNod.style.top=getAbsoluteTop(panel.parentItem.topNod)*1+panel.parentItem.topNod.offsetHeight*1-1;
 panel.topNod.style.left=getAbsoluteLeft(panel.parentItem.topNod);
 break;
}
 panel.topNod.style.display="";
 if(panel.topNod.ieFix)
{
 panel.topNod.ieFix.style.top=panel.topNod.style.top;
 panel.topNod.ieFix.style.left=panel.topNod.style.left;
 panel.topNod.ieFix.style.width=panel.topNod.offsetWidth;
 panel.topNod.ieFix.style.height=panel.topNod.offsetHeight;
 panel.topNod.ieFix.style.display="";
}
}
 this._fixPanelPosition(panel);
}

 dhtmlXMenuBarObject.prototype._fixPanelPosition=function(panel,mode){
 var uf=0;
 if(panel.parentPanel){
 var xs=document.body.offsetWidth-15+document.body.scrollLeft;
 var ys=document.body.offsetHeight-15+document.body.scrollTop;
 if((panel.topNod.offsetWidth+parseInt(panel.topNod.style.left))>xs)
{
 if(!panel.parentPanel.extraMode){
 var z=xs-panel.topNod.offsetWidth;
 if(z<0)z=0;
 panel.topNod.style.left=z;
 if(panel.topNod.ieFix)panel.topNod.ieFix.style.left=z;
}
 else{
 var z=parseInt(panel.topNod.style.left)-panel.topNod.offsetWidth-panel.parentItem.topNod.offsetWidth;
 if(z<0){
 var z2=parseInt(panel.topNod.style.top)+panel.parentItem.topNod.offsetHeight;
 panel.topNod.style.top=z2;
 if(panel.topNod.ieFix)panel.topNod.ieFix.style.top=z2;

 z=xs-panel.topNod.offsetWidth;if(z<0)z=0;}
 panel.topNod.style.left=z;
 if(panel.topNod.ieFix)panel.topNod.ieFix.style.left=z;
}

}
 if((panel.topNod.offsetHeight+parseInt(panel.topNod.style.top))>ys)
{
 
 if(panel.parentPanel.extraMode){
 var z=ys-panel.topNod.offsetHeight;
 if(z<0)z=0;
 panel.topNod.style.top=z;
 if(panel.topNod.ieFix)panel.topNod.ieFix.style.top=z;
}
 else{
 var z=parseInt(panel.topNod.style.top)-panel.topNod.offsetHeight-panel.parentItem.topNod.offsetHeight;
 if(z<0){
 var z2=parseInt(panel.topNod.style.left)+panel.parentItem.topNod.offsetWidth;
 panel.topNod.style.left=z2;
 if(panel.topNod.ieFix)panel.topNod.ieFix.style.left=z2;
 uf=1;
 z=ys-panel.topNod.offsetHeight;if(z<0)z=0;}
 panel.topNod.style.top=z;
 if(panel.topNod.ieFix)panel.topNod.ieFix.style.top=z;
}
 
}
 
}
 
 if((uf)&&(!mode))this._fixPanelPosition(panel,1);
}
 
 dhtmlXMenuBarObject.prototype._closePanel=function(panel){
 if(!panel)return;
 if((this.lastSelectedItem)&&(this.lastSelectedItem.parentPanel==panel))this._unMarkItem(this.lastSelectedItem);
 this._closeBottomPanels(panel);
 this._closeTopPanels(panel);
 
 this.lastOpenedPanel="";
}

 dhtmlXMenuBarObject.prototype._closeTopPanels=function(panel){
 if((this.lastSelectedItem)&&(this.lastSelectedItem.parentPanel==panel))this._unMarkItem(this.lastSelectedItem);
 for(var i=0;i<panel.itemsCount;i++)
{
 var zi=panel.items[i];
 if((zi.subMenu)&&(zi.subMenu.topNod.style.display!="none"))
{
 zi.subMenu.topNod.style.display="none";
 this._unMarkItem(zi.subMenu.parentItem);
 if(zi.subMenu.topNod.ieFix)zi.subMenu.topNod.ieFix.style.display="none"
 this._closeTopPanels(zi.subMenu);
 return;
}
}
 
}
 
 dhtmlXMenuBarObject.prototype._closeBottomPanels=function(panel,ieWinMode){
 if(panel.parentPanel)
{
 
 
 if((!this.lastSelectedItem)||(this.lastSelectedItem.parentPanel!=panel.parentPanel))
{
 this._closeBottomPanels(panel.parentPanel);
}
 else this.lastOpenedPanel=panel;
 panel.topNod.style.display="none";
 this._unMarkItem(panel.parentItem);
 if(panel.topNod.ieFix)panel.topNod.ieFix.style.display="none"
}
}
 
 
 dhtmlXMenuBarObject.prototype._unMarkItem=function(item){
 item.CSSTag.className=item.className;
 if(item.CSSImageTag)item.CSSImageTag.className="";
 if((item.childMenuTag.src!="")&&(this.topMenu.echsw)&&(this.topMenu.echsw2))
 item.childMenuTag.src=this.sysGfxPath+"btn_rt1.gif";
 
}
 
 dhtmlXMenuBarObject.prototype._markItem=function(item,over){
 over=over||"over";
 item.CSSTag.className=item.className+over;
 if(item.CSSImageTag)item.CSSImageTag.className=item.className+over+"img";
 
 if((item.childMenuTag.src!="")&&(this.topMenu.echsw)&&(this.topMenu.echsw2))
 item.childMenuTag.src=this.sysGfxPath+"btn_rt2.gif";
 
}
 
 
 dhtmlXMenuBarObject.prototype.setMenuMode=function(modeValue){
 this.modeValue=modeValue;
 switch(modeValue){
 case "classic":
 this.addFirstLevel=this._addItem;
 this.addSecondLevel=this.addItem_vertical;
 break;
 case "alfa":
 this.addFirstLevel=this._addItem;
 this.addSecondLevel=this._addItem;
 break;
 case "popup":
 this.addFirstLevel=this.addItem_vertical;
 this.addSecondLevel=this.addItem_vertical;
 break;
 case "betta":
 this.addFirstLevel=this.addItem_vertical;
 this.addSecondLevel=this._addItem;
 break;
};
}
 
 

 
 
 
 function dhtmlXMenuDividerYObject(id){
 this.topNod=0;
 if(id)this.id=id;else this.id=0;
 td=document.createElement("td");
 this.topNod=td;td.align="center";td.style.padding="2 2 1 2";
 td.innerHTML="<div class='menuDividerY'>&nbsp;</div>";
 if(!document.all)td.childNodes[0].style.height="0px";
 return this;
};
 dhtmlXMenuDividerYObject.prototype = new dhtmlXButtonPrototypeObject;
 
 

 
 
 function dhtmlXMenuDividerXObject(id){
 this.topNod=0;
 if(id)this.id=id;else this.id=0;
 td=document.createElement("td");
 this.topNod=td;td.align="center";td.style.paddingRight="2";td.style.paddingLeft="2";td.width="4px";
 td.innerHTML="<div class='menuDivider'></div >";
 if(!document.all){td.childNodes[0].style.width="0px";td.style.padding="0 0 0 0";td.style.margin="0 0 0 0";}
 return this;
};
 dhtmlXMenuDividerXObject.prototype = new dhtmlXButtonPrototypeObject;
 

