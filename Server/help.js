help = function () {
      $("#help").empty();
      $("#help").append("nessun aiuto presente");
      $("#help"). load (encodeURI ('/help/' + $(this).text().trim()));
	$("#help").dialog();	
       
    };

 $(document).ready(function() {
   // generate markup
   $(".responso").click(help);   
   $(".output > dt").click(help);
   $(".errore > dt").click(help);

}); 

