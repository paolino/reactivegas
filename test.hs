import System.Console.ParseArgs

data Argomenti = Pippo | Pluto | Topolino | Frase

s = [
	Arg Pippo (Just 'p') (Just "pippo") Nothing "parla pippo",
	Arg Pluto (Just 'P') (Just "pluto") Nothing "parla pluto",
	Arg Pippo (Just 't') (Just "topolino") Nothing "parla topolino",
	Arg Frase Nothing Nothing (argDataRequired "frase" ArgtypeString (Just p))
	]
	
main = parseArgsIO
