help = function () {
      $("#help").empty();
      $("#help").append("nessun aiuto presente");
      $("#help"). load (encodeURI ('/help/' + $(this).text().trim()));
      $("#help").dialog();	
       
    };

refresh = function (p,uri) {
	p.load(uri,function () {
		if (p.children().hasClass("passobox")) { trigs (p);} 
		else {	t = p.find(".passobox");
			t.detach();
			p.empty();
			$(".passobox").fadeOut('fast').each(reload).fadeIn('slow');
			t.appendTo(p);
			trigs(p);
			} 
		}
		);
	};
reform = function(event) {
	var valore = $(this).find("input#valore").val();
	var hkey = $(this).find("input#hkey").val();
	var fkey = $(this).find("input#fkey").val();
	event.preventDefault();
	var p = $(this).parent().parent();
	var uri = encodeURI ('/unaform?' + 'valore=' + valore + '&hkey=' + hkey + '&fkey=' + fkey);
	refresh (p,uri);
	};
relink = function(event) {
	var uri = $(this).attr("href").replace("interazione","unaform");
	event.preventDefault();
	var p = $(this).parent().parent().parent().parent();
	refresh (p,uri);
	};

reback = function(event) {
	var uri = $(this).attr("href").replace("interazione","ricarica");
	event.preventDefault();
	var p = $(this).parent();
	refresh (p,uri);
	};

reload = function (n,y) {
	var x = $(y)
	var hkey = x.attr("hkey");
	var fkey = x.attr("fkey");
	var uri = encodeURI ('/ricarica?' + 'hkey=' + hkey + '&fkey=' + fkey)	
	var p = x.parent();
	p.load (uri,function () {trigs(p)});
};

trigs = function (x) {	
	x.find("a.back").click(reback);
   	x.find(".quiet").submit(reform);
	x.find("a.quietL").click(relink);
	x.find(".responso").click(help);   
	x.find(".output > dt").click(help);
	x.find(".errore > dt").click(help);
   	};
$(document).ready(function () {
	trigs ($(".utente"));
	})

