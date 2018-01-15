all: latte

latte: ./src/Main.hs ./src/TypeChecker.hs ./src/latte.cf
#	cd src; bnfc Latte.cf
#	cd src; happy -gca ParLatte.y
#	cd src; alex -g LexLatte.x
	cd lib; gcc -c -nostdlib -m32 runtime.c
	stack build --copy-bins

clean:
	cd src; rm -f *.log *.aux *.hi *.o *.dvi *.info
	cd lib; rm -f *.o
