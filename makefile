applicativi:NuovoResponsabile.hs PatchSincronizzatore.hs PatchResponsabile.hs StatoIniziale.hs Server.hs
	ghc --make NuovoResponsabile
	ghc --make PatchSincronizzatore
	ghc --make PatchResponsabile
	ghc --make Server
	ghc --make StatoIniziale
	cp NuovoResponsabile PatchResponsabile PatchSincronizzatore Server StatoIniziale Applicazioni
