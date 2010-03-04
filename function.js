function rewrite () {
	var lista_td = document.getElementsByTagName("form");
	for(i=0; i < lista_td.length; i++){
		var e = lista_td.item(i)
		fid = e.getAttribute("id");
		ev = e.valore
		if (ev != null) {
		if (ev.tagName == "SELECT" && ev.parentNode.parentNode.className == "passobox") {ev.setAttribute ("size","8")}
		if (ev!=null && fid != null) {ev.setAttribute("onchange",'submity("' + fid + '")')}}
		} 
	}
function submity(y)
	{document.getElementById(y).submit();}

