applicativi:Cliente.hs Servente.hs
	ghc --make ClienteGTK
	ghc --make Servente
	cp cliente.glade ClienteGTK Servente Applicazioni
clean:
	rm -f *.o 
	rm -f *.hi
	rm -f `find . -maxdepth 1 -perm -u=x -type f`
git:
	git add *.hs
	git add README
	git add LICENSE
	git add makefile
	git add cliente.glade
	git commit
	git push

release:applicativi
	tar cjvf release.tbz Servente ClienteGTK
edit:
	gvim *.hs README LICENSE makefile

