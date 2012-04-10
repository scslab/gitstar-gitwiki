dev: 
#	for e in `cat .env`; do export $e; done
	ghc -hide-package monads-tf --make Main.hs && ./Main 
prod: 
#	for e in `cat .env`; do export $e; done
	ghc -hide-package monads-tf --make Main.hs -DPRODUCTION=1 && ./Main 
clean:
	-rm *.{hi,o}
	-rm */*.{hi,o}
	-rm Main
