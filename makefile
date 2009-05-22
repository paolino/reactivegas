applicativi:NuoveChiavi.hs PatchSincronizzatore.hs PatchResponsabile.hs StatoIniziale.hs Server.hs
	ghc --make NuoveChiavi
	ghc --make PatchSincronizzatore
	ghc --make PatchResponsabile
	ghc --make Server
	ghc --make StatoIniziale
	cp NuoveChiavi PatchResponsabile PatchSincronizzatore Server StatoIniziale Applicazioni
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
	cp NuoveChiavi PatchResponsabile PatchSincronizzatore Server StatoIniziale release
	tar cjvf release.tbz release
	rm -rf release
edit:
	gvim *.hs README LICENSE

