applicativi:NuovoResponsabile.hs PatchSincronizzatore.hs PatchResponsabile.hs StatoIniziale.hs Server.hs
	ghc --make NuovoResponsabile
	ghc --make PatchSincronizzatore
	ghc --make PatchResponsabile
	ghc --make Server
	ghc --make StatoIniziale
	cp NuovoResponsabile PatchResponsabile PatchSincronizzatore Server StatoIniziale Applicazioni
clean:
	rm *.o 
	rm *.hi
git:
	git add *.hs
	git add README
	git add LICENSE
	git add makefile
	git commit
	git push
release:applicativi
	mkdir release
	cp NuovoResponsabile PatchResponsabile PatchSincronizzatore Server StatoIniziale release
	tar cjvf release.tbz release
	rm -rf release
