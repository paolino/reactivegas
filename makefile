applicativi:Chiavi.hs Sincronizza.hs Eventi.hs Inizio.hs Server.hs
	ghc --make Cliente
	ghc --make Server
	ghc --make Boot
	cp Cliente Server Boot Applicazioni
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
	mkdir release
	cp Cliente Server Boot release
	tar cjvf release.tbz release
	rm -rf release
edit:
	gvim *.hs README LICENSE makefile

