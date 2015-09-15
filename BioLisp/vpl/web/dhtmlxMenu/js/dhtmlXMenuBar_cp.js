 
function dhtmlXContextMenuObject(width,height){
 var foo = document.body;
 if (document.body == null) alert("I ain't got nobody");
 this.menu=new dhtmlXMenuBarObject(document.body,width,height,name,1);
 this.menu.setMenuMode("popup");
 this.menu.hideBar();
 this.menu.contextMenu=this;
 this.menu.enableWindowOpenMode(false);
 this.menu.setOnClickHandler(this._innerOnClick);
 this.aframes=new Array();
 this.registerFrame(window);

 return this;
};

dhtmlXContextMenuObject.prototype.registerFrame=function(awin){
 this.aframes[this.aframes.length]=awin;
}



 
dhtmlXContextMenuObject.prototype.setContextMenuHandler=function(func){
 if(typeof(func)=="function")this.onClickHandler=func;else this.onClickHandler=eval(func);
}
 
dhtmlXContextMenuObject.prototype.openAt=function(x,y,id,smartPosition){
 this.espc=convertStringToBoolean(smartPosition);
 var f=new Object;f.button=2;f.clientX=x;f.clientY=y;
 var start=new Object;start.contextMenuId=id;start.contextMenu=this;start.a=this._contextStart;
 start.a(document.body,f);
 this.espc=null;
}


 
 
dhtmlXContextMenuObject.prototype.setOnShowMenuHandler=function(func){
 if(typeof(func)=="function")this.onShowHandler=func;else this.onShowHandler=eval(func);
}
 
 
dhtmlXContextMenuObject.prototype._innerOnClick=function(id){
 var that=document.body.contextMenu;
 if(that.contextZone.ownerDocument.body.onclick)that.contextZone.ownerDocument.body.onclick();
 if(that.onClickHandler)return that.onClickHandler(id,that.zoneId,that.contextZone);
 return true;
}

 
dhtmlXContextMenuObject.prototype.setContextZone=function(htmlObject,zoneId){
    if(typeof(htmlObject)!="object")
    htmlObject=document.getElementById(htmlObject);

 htmlObject.contextOnclick=htmlObject.onmousedown;
 htmlObject.selfobj = this;htmlObject.onmousedown= function(e){this.selfobj._contextStart(this,e)};htmlObject.contextMenu=this;
 htmlObject.contextMenuId=zoneId;
}
 
dhtmlXContextMenuObject.prototype._contextStart=function(obj,e){
    if (obj.contextMenuId != null) { // mt addition
	if(!this.contextMenu)this.contextMenu = this;
	var win = obj.ownerDocument.parentWindow;
	if(!win){
	    win = obj.ownerDocument.defaultView;
	}

	if(!e){
	    e=win.event;
	}

        // mt addition, supports menus on nested objects
	e.cancelBubble = true;
	if (e.stopPropagation) e.stopPropagation();

	if(document.body.onclick)document.body.onclick();

	if((!e)||(e.button!=2))
	{
	    if(obj.contextOnclick)obj.contextOnclick();
	    return true;
	}
	else{
	    if(this.contextMenu.onShowHandler)
	    this.contextMenu.onShowHandler(obj.contextMenuId);
	    this.contextMenu.menu.showBar();
	}
	var a=this.contextMenu.menu.topNod;
	var winScreenTop = window.screenTop;
	if((!winScreenTop)&&(winScreenTop!=0)){
	    winScreenTop = window.screenY+window.outerHeight-window.innerHeight;}
	var winScreenLeft = window.screenLeft;
	if((!winScreenLeft)&&(winScreenLeft!=0))
	winScreenLeft = window.screenX+window.outerWidth-window.innerWidth-4;
	if(e.screenY-winScreenTop+a.offsetHeight-window.document.body.scrollTop > window.document.body.clientHeight){var verCor = a.offsetHeight
															 }else
	var verCor = -14;
	var corrector = new Array(window.document.body.scrollLeft+5,verCor-window.document.body.scrollTop);
	a.style.position="absolute";


 


 if((!e.screenY)&&(e.clientX))
{
 a.style.top = e.clientY;
 a.style.left = e.clientX;
}else
{
 a.style.top = e.screenY-winScreenTop-corrector[1];a.style.left = e.screenX-winScreenLeft+corrector[0];}
 this.contextMenu._fixMenuPosition(a);


 a.ieFix.style.top=a.style.top;
 a.ieFix.style.left=a.style.left;

 win.document.body.oncontextmenu=new Function("document.body.oncontextmenu=new Function('if(document.body.onclick)document.body.onclick();return false;');return false;");

 for(var i=0;i<this.contextMenu.aframes.length;i++){
 if(this.contextMenu.aframes[i].document)
 this.contextMenu.aframes[i].document.body.selfobj = this;
 this.contextMenu.aframes[i].document.body.onclick=function(e){this.selfobj.contextMenu._contextEnd(e)};
}
 document.body.contextMenu=this.contextMenu;
 this.contextMenu.contextZone=obj;
 this.contextMenu.zoneId=obj.contextMenuId;return false;
}
}

 
dhtmlXContextMenuObject.prototype._fixMenuPosition=function(panel,mode){
 var xs=document.body.offsetWidth+document.body.scrollLeft;
 var ys=document.body.offsetHeight-15+document.body.scrollTop;
 if((panel.offsetWidth+parseInt(panel.style.left))>xs)
{
 var z=parseInt(panel.style.left)-panel.offsetWidth;
 if((z<0)||(this.espc))
 z=xs-panel.offsetWidth;
 if(z<0)z=0;
 panel.style.left=z;
 if(panel.ieFix)panel.ieFix.style.left=z;
}

 if((panel.offsetHeight+parseInt(panel.style.top))>ys)
{
 var z=parseInt(panel.style.top)- panel.offsetHeight;
 if((z<0)||(this.espc))
 z=ys-panel.offsetHeight;
 if(z<0)z=0;
 panel.style.top=z;
 if(panel.ieFix)panel.ieFix.style.top=z;
}
 
 if(!mode)this._fixMenuPosition(panel,1);
}
 
 
dhtmlXContextMenuObject.prototype._contextEnd=function(e){
 var menu=this.menu;
 menu._closePanel(menu);
 menu.lastOpenedPanel="";
 menu.lastSelectedItem=0;
 menu.hideBar();
 for(var i=0;i<this.aframes.length;i++)
 if(this.aframes[i].document)
 this.aframes[i].document.body.onclick=null;

 return false;
}

