all: latte

latte: ./src/Main.hs ./src/TypeChecker.hs ./src/latte.cf
#	cd src; bnfc Latte.cf
#	cd src; happy -gca ParLatte.y
#	cd src; alex -g LexLatte.x
	cd lib; gcc -m32 -O2 -c runtime.c -o runtime.o
	cd src; ghc --make -freverse-errors  Main.hs -o ../latc_x86

clean:
	cd src; rm -f *.log *.aux *.hi *.o *.dvi *.info
	cd lib; rm -f *.o
