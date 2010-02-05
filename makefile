
clean:
	rm -f *.o 
	rm -f *.hi
	rm -f `find . -maxdepth 1 -perm -u=x -type f`

edit:
	gvim Lib/*.hs Core/*.hs Eventi/*.hs Applicazioni/*.hs Applicazioni/reactivegas.glade README LICENSE makefile
modules:
	/home/paolino/.cabal/bin/graphmod *.hs Core/*.hs Lib/*.hs Eventi/*.hs | dot -Tpng  > modules.png


