applicativi:Cliente.hs Boot.hs Servente.hs
	ghc --make Cliente
	ghc --make Servente
	ghc --make Boot
	cp Cliente Servente Boot Applicazioni
clean:
	rm -f *.o 
	rm -f *.hi
	rm -f `find . -maxdepth 1 -perm -u=x -type f`
git:
	git add *.hs
	git add README
	git add LICENSE
	git add makefile
	git commit
	git push

release:applicativi
	tar cjvf release.tbz Cliente Servente Boot
edit:
	gvim *.hs README LICENSE makefile

