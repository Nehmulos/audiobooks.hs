all:
	pkill Main || true
	cd src; ghc Main.hs
	./src/Main &
