/*
 * jQuery Tooltip plugin 1.3
 *
 * http://bassistance.de/jquery-plugins/jquery-plugin-tooltip/
 * http://docs.jquery.com/Plugins/Tooltip
 *
 * Copyright (c) 2006 - 2008 Jörn Zaefferer
 *
 * $Id: jquery.tooltip.js 5741 2008-06-21 15:22:16Z joern.zaefferer $
 * 
 * Dual licensed under the MIT and GPL licenses:
 *   http://www.opensource.org/licenses/mit-license.php
 *   http://www.gnu.org/licenses/gpl.html
 */;(function($){var helper={},current,title,tID,IE=$.browser.msie&&/MSIE\s(5\.5|6\.)/.test(navigator.userAgent),track=false;$.tooltip={blocked:false,defaults:{delay:200,fade:false,showURL:true,extraClass:"",top:15,left:15,id:"tooltip"},block:function(){$.tooltip.blocked=!$.tooltip.blocked;}};$.fn.extend({tooltip:function(settings){settings=$.extend({},$.tooltip.defaults,settings);createHelper(settings);return this.each(function(){$.data(this,"tooltip",settings);this.tOpacity=helper.parent.css("opacity");this.tooltipText=this.title;$(this).removeAttr("title");this.alt="";}).mouseover(save).mouseout(hide).click(hide);},fixPNG:IE?function(){return this.each(function(){var image=$(this).css('backgroundImage');if(image.match(/^url\(["']?(.*\.png)["']?\)$/i)){image=RegExp.$1;$(this).css({'backgroundImage':'none','filter':"progid:DXImageTransform.Microsoft.AlphaImageLoader(enabled=true, sizingMethod=crop, src='"+image+"')"}).each(function(){var position=$(this).css('position');if(position!='absolute'&&position!='relative')$(this).css('position','relative');});}});}:function(){return this;},unfixPNG:IE?function(){return this.each(function(){$(this).css({'filter':'',backgroundImage:''});});}:function(){return this;},hideWhenEmpty:function(){return this.each(function(){$(this)[$(this).html()?"show":"hide"]();});},url:function(){return this.attr('href')||this.attr('src');}});function createHelper(settings){if(helper.parent)return;helper.parent=$('<div id="'+settings.id+'"><h3></h3><div class="body"></div><div class="url"></div></div>').appendTo(document.body).hide();if($.fn.bgiframe)helper.parent.bgiframe();helper.title=$('h3',helper.parent);helper.body=$('div.body',helper.parent);helper.url=$('div.url',helper.parent);}function settings(element){return $.data(element,"tooltip");}function handle(event){if(settings(this).delay)tID=setTimeout(show,settings(this).delay);else
show();track=!!settings(this).track;$(document.body).bind('mousemove',update);update(event);}function save(){if($.tooltip.blocked||this==current||(!this.tooltipText&&!settings(this).bodyHandler))return;current=this;title=this.tooltipText;if(settings(this).bodyHandler){helper.title.hide();var bodyContent=settings(this).bodyHandler.call(this);if(bodyContent.nodeType||bodyContent.jquery){helper.body.empty().append(bodyContent)}else{helper.body.html(bodyContent);}helper.body.show();}else if(settings(this).showBody){var parts=title.split(settings(this).showBody);helper.title.html(parts.shift()).show();helper.body.empty();for(var i=0,part;(part=parts[i]);i++){if(i>0)helper.body.append("<br/>");helper.body.append(part);}helper.body.hideWhenEmpty();}else{helper.title.html(title).show();helper.body.hide();}if(settings(this).showURL&&$(this).url())helper.url.html($(this).url().replace('http://','')).show();else
helper.url.hide();helper.parent.addClass(settings(this).extraClass);if(settings(this).fixPNG)helper.parent.fixPNG();handle.apply(this,arguments);}function show(){tID=null;if((!IE||!$.fn.bgiframe)&&settings(current).fade){if(helper.parent.is(":animated"))helper.parent.stop().show().fadeTo(settings(current).fade,current.tOpacity);else
helper.parent.is(':visible')?helper.parent.fadeTo(settings(current).fade,current.tOpacity):helper.parent.fadeIn(settings(current).fade);}else{helper.parent.show();}update();}function update(event){if($.tooltip.blocked)return;if(event&&event.target.tagName=="OPTION"){return;}if(!track&&helper.parent.is(":visible")){$(document.body).unbind('mousemove',update)}if(current==null){$(document.body).unbind('mousemove',update);return;}helper.parent.removeClass("viewport-right").removeClass("viewport-bottom");var left=helper.parent[0].offsetLeft;var top=helper.parent[0].offsetTop;if(event){left=event.pageX+settings(current).left;top=event.pageY+settings(current).top;var right='auto';if(settings(current).positionLeft){right=$(window).width()-left;left='auto';}helper.parent.css({left:left,right:right,top:top});}var v=viewport(),h=helper.parent[0];if(v.x+v.cx<h.offsetLeft+h.offsetWidth){left-=h.offsetWidth+20+settings(current).left;helper.parent.css({left:left+'px'}).addClass("viewport-right");}if(v.y+v.cy<h.offsetTop+h.offsetHeight){top-=h.offsetHeight+20+settings(current).top;helper.parent.css({top:top+'px'}).addClass("viewport-bottom");}}function viewport(){return{x:$(window).scrollLeft(),y:$(window).scrollTop(),cx:$(window).width(),cy:$(window).height()};}function hide(event){if($.tooltip.blocked)return;if(tID)clearTimeout(tID);current=null;var tsettings=settings(this);function complete(){helper.parent.removeClass(tsettings.extraClass).hide().css("opacity","");}if((!IE||!$.fn.bgiframe)&&tsettings.fade){if(helper.parent.is(':animated'))helper.parent.stop().fadeTo(tsettings.fade,0,complete);else
helper.parent.stop().fadeOut(tsettings.fade,complete);}else
complete();if(settings(this).fixPNG)helper.parent.unfixPNG();}})(jQuery);
/*
 * jQuery.upload v1.0.2
 *
 * Copyright (c) 2010 lagos
 * Dual licensed under the MIT and GPL licenses.
 *
 * http://lagoscript.org
 */
(function(b){function m(e){return b.map(n(e),function(d){return'<input type="hidden" name="'+d.name+'" value="'+d.value+'"/>'}).join("")}function n(e){function d(c,f){a.push({name:c,value:f})}if(b.isArray(e))return e;var a=[];if(typeof e==="object")b.each(e,function(c){b.isArray(this)?b.each(this,function(){d(c,this)}):d(c,b.isFunction(this)?this():this)});else typeof e==="string"&&b.each(e.split("&"),function(){var c=b.map(this.split("="),function(f){return decodeURIComponent(f.replace(/\+/g," "))});
d(c[0],c[1])});return a}function o(e,d){var a;a=b(e).contents().get(0);if(b.isXMLDoc(a)||a.XMLDocument)return a.XMLDocument||a;a=b(a).find("body").html();switch(d){case "xml":a=a;if(window.DOMParser)a=(new DOMParser).parseFromString(a,"application/xml");else{var c=new ActiveXObject("Microsoft.XMLDOM");c.async=false;c.loadXML(a);a=c}break;case "json":a=window.eval("("+a+")");break}return a}var p=0;b.fn.upload=function(e,d,a,c){var f=this,g,j,h;h="jquery_upload"+ ++p;var k=b('<iframe name="'+h+'" style="position:absolute;top:-9999px" />').appendTo("body"),
i='<form target="'+h+'" method="post" enctype="multipart/form-data" />';if(b.isFunction(d)){c=a;a=d;d={}}j=b("input:checkbox",this);h=b("input:checked",this);i=f.wrapAll(i).parent("form").attr("action",e);j.removeAttr("checked");h.attr("checked",true);g=(g=m(d))?b(g).appendTo(i):null;i.submit(function(){k.load(function(){var l=o(this,c),q=b("input:checked",f);i.after(f).remove();j.removeAttr("checked");q.attr("checked",true);g&&g.remove();setTimeout(function(){k.remove();c==="script"&&b.globalEval(l);
a&&a.call(f,l)},0)})}).submit();return this}})(jQuery);
help = function () {
      $("#help").empty();
      $("#help").append("nessun aiuto presente");
      $("#help").load (encodeURI ('/help/' + $(this).text().trim()));
      $("#help").dialog();
       
    };

clean = function (p) {
	var t = p.find(".passobox");
	p.empty();
	t.appendTo(p);
	trigs(p);
	}; 
update = function (p) {
	return function (r) {
			p.empty();
			var rr = $(r);
			if (rr.hasClass("passobox")) {rr.appendTo(p);} //risposta innocua
				else {$(".passobox").each(reload);rr.find(".passobox").appendTo(p);}
			trigs(p);
			};	
	};
refresh = function (p,uri) {
	p.load(uri,function () {
		if (p.children().hasClass("passobox")) {trigs (p);} //risposta innocua
		else {	var t = p.find(".passobox");
			p.empty();
			$(".passobox").each(reload);
			p.append(t);
			trigs(p);
			}

		});

	};
refreshC = function (p,uri,d) {
	p.load(uri,function () {
		if (p.children().hasClass("passobox")) {trigs (p);} //risposta innocua
		else {	var t = p.find(".passobox");
			p.empty();
			$(".passobox").each(reload);
			p.append(t);
			trigs(p);
			}
		d ();
		});

	};

reform = function(event) {
	var valore = $(this).find("input#valore").val();
	var hkey = $(this).find("input#hkey").val();
	var p = $(this).parent().parent();
	var fkey = $(this).find("input#fkey").val();
	var action = $(this).attr("action");

	
	var uri = encodeURI ('/unaform?' + 'valore=' + valore + '&hkey=' + hkey + '&fkey=' + fkey + '&rand=' + Math.random()*999999);
	if (action === '/interazione') {	
		event.preventDefault();	refresh (p,uri);
		}
	else {p.find(".continua").show();}
	};
relink = function(event) {
	var uri = $(this).attr("href").replace("interazione","unaform") + '&rand=' + Math.random()*999999;
	event.preventDefault();
	var p = $(this).parent().parent().parent().parent();
	refresh (p,uri);
	};

reback = function(event) {
	var uri = $(this).attr("href").replace("interazione","ricarica") + '&rand=' + Math.random()*999999;
	event.preventDefault();
	var p = $(this).parent().parent();
	refresh (p,uri);
	};

reload = function (n,y) {
	var x = $(y);
	var hkey = x.attr("hkey");
	var fkey = x.attr("fkey");
	var uri = encodeURI ('/ricarica?' + 'hkey=' + hkey + '&fkey=' + fkey + '&rand=' + Math.random()*999999);	
	var p = x.parent();
	refresh(p,uri);
};
clona = function (x,d) {
	var hkey = x.attr("hkey");
	var fkey = x.attr("fkey");
	var uri = encodeURI ('/clona?' + 'hkey=' + hkey + '&fkey=' + fkey + '&rand=' + Math.random()*999999);	
	var p = x.parent();
	refreshC(p,uri,d);
};
reset = function (x) {
	var hkey = x.attr("hkey");
	var fkey = x.attr("fkey");
	var uri = encodeURI ('/reset?' + 'hkey=' + hkey + '&fkey=' + fkey + '&rand=' + Math.random()*999999);	
	var p = x.parent();
	refresh(p,uri);
};
dila = function (z) {
	z.find(".expand").click(function () {
			var q = $(this).parent(); // passobox
			var y = q.parent(); // boxes, dimension
			var x = y.clone(true,true); // clone della dimension 
			x.css("left","0").css("top","0");
			var d = function () {
				x.removeData();
				x.dialog({ 
					height: 330,
					width: 450,
					position: [Math.random()*400,Math.random()*100],
					});
				}
			clona (x.find(".passobox"),d);
			});
		
	};
dilax = function (z) {
	z.find(".resetF").click(function () {reset ($(this).parent());});
	};

trigs = function (x) {	
	x.find(".passobox > .errore").before ('<div title="clona" class="expand">&#x238c;</div>' +
		'<div title = "a capo" class="resetF">&#x21e4;</div>');
	x.find(".passobox > .responso").before ('<div title="clona" class="expand">&#x238c;</div>'+
		'<div title = "a capo" class="resetF">&#x21e4;</div>');
	x.find(".passobox > .output").before ('<div title="clona" class="expand">&#x238c;</div>'+
		'<div title = "a capo" class="resetF">&#x21e4;</div>');	
	x.find(".expand").tooltip();
	x.find(".back").tooltip({showURL: false});
	x.find(".resetF").tooltip();
	trigs0 (x); 
	dila(x);
	dilax(x);
	};
trigs0 = function (x) {	
	x.find("a.back").click(reback);
	x.find(".quiet").filter(function (i) {
		var enctype = $(this).attr("enctype");
		return (enctype === "multipart/form-data");
		}).removeClass("quiet").addClass("quietU");
	x.find(".quiet").submit(reform);
	x.find(".quietU > #valore").change(function() {
				var p = $(this).parent().parent().parent();
				var hkey = $(this).parent().find("input#hkey").val();
				var fkey = $(this).parent().find("input#fkey").val();
				$(this).upload('/unaform?fkey=' + fkey + '&hkey=' + hkey, update(p));
				});
	x.find("a.quietL").click(relink);
	x.find(".continua").hide();
	x.find(".scarica").show();
	};



fill = function () {
	var w = $(window).width();
	var h = $(window).height();
	var fw = w / $gw;
	var fh = h / $gh;
	$gw = w;
	$gh = h;
	$(".boxes").each (larger (fw));
	$(".boxes").each (taller (fh));
	}
larger = function (y) {
	var l = function (q,z) { 
		var l = parseInt($(this).css("left"));
		var w = parseInt($(this).css("width")); 
		if (isNaN(l) || isNaN (w)) {} else {
			$(z).css(
				{ 
				left: (l * y) + "px", 
				width: (w * y) + "px"
				});
			};
		};
			
	return l;
	};
taller = function (y) {
	var l = function (q,z) { 
		var l = parseInt($(this).css("top"));
		var w = parseInt($(this).css("height")); 
		if (isNaN(l) || isNaN (w)) {} else {
			$(z).animate(
				{ 
				top: (l * y) + "px", 
				height: (w * y) + "px"
				},100);
			};
		};
			
	return l;
	};

	
refresher = function (x) {
	var hkey = x.attr("hkey");
	var fkey = x.attr("fkey");
	var uri = encodeURI ('/ricarica?' + 'hkey=' + hkey + '&fkey=' + fkey + '&rand=' + Math.random()*999999);	
	var p = x.parent();
	refresh(p,uri);
	};

var $gw = 820;
var $gh = 550;

$(document).ready(function () {
	$(window).resize(fill);
	fill();
	$(".project").tooltip({showURL:false});

	trigs ($(".utente"));
	setInterval(function() {refresher($(".dimensione1 > .passobox").first());},20000);

	});


