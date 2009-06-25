applicativi:Cliente.hs Servente.hs
	cp reactivegas.glade Applicazioni
	ghc --make ClienteGTK
	mv ClienteGTK Reactivegas
	ghc --make Servente
	mv Servente Reactivegas.server
	cp Reactivegas Reactivegas.server Applicazioni

clean:
	rm -f *.o 
	rm -f *.hi
	rm -f `find . -maxdepth 1 -perm -u=x -type f`
git:
	git add *.hs
	git add README
	git add LICENSE
	git add makefile
	git add reactivegas.glade
	git commit
	git push

release:applicativi
	tar cjvf release.tbz Reactivegas.server Reactivegas reactivegas.glade
edit:
	gvim *.hs README LICENSE makefile

