
edit:
	gvim Lib/*.hs Lib/Server.hs Core/*.hs Eventi/*.hs Applicazioni/*.hs  
		README LICENSE makefile reactivegas.cabal
modules:
	/home/paolino/.cabal/bin/graphmod *.hs Core/*.hs Lib/*.hs Eventi/*.hs | dot -Tpng  > modules.png


