
edit:
	gvim `find . -name "*.hs"`  README LICENSE makefile reactivegas.cabal
modules:
	/home/paolino/.cabal/bin/graphmod *.hs Core/*.hs Lib/*.hs Eventi/*.hs | dot -Tpng  > modules.png


