
clean:
	rm -f *.o 
	rm -f *.hi
	rm -f `find . -maxdepth 1 -perm -u=x -type f`
git:
	git add Lib/*.hs
	git add Core/*.hs
	git add Eventi/*.hs
	git add Applicazioni/*.hs
	git add Tests.hs
	git add Applicazioni/reactivegas.glade
	git add README
	git add LICENSE
	git add modules.svg
	git add makefile
	git commit
	git push

edit:
	gvim Lib/*.hs Core/*.hs Eventi/*.hs Applicazioni/*.hs Applicazioni/reactivegas.glade README LICENSE makefile
modules:
	/home/paolino/.cabal/bin/graphmod *.hs Core/*.hs Lib/*.hs Eventi/*.hs | dot -Tpng  > modules.png


