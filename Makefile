all: latte

latte: ./src/Main.hs ./src/TypeChecker.hs ./src/latte.cf
#	cd src; bnfc Latte.cf
#	cd src; happy -gca ParLatte.y
#	cd src; alex -g LexLatte.x
	cd src; ghc --make  Main.hs -o ../latc_x86

clean:
	cd src; rm -f *.log *.aux *.hi *.o *.dvi *.info
	cd lib; rm -f *.o
