all: latte

latte: ./src/Main.hs ./src/latte.cf
	cd src; bnfc Latte.cf
	cd src; happy -gca ParLatte.y
	cd src; alex -g LexLatte.x
	cd src; ghc --make Main.hs -o ../latc_x86_64

clean:
	cd src; rm -f *.log *.aux *.hi *.o *.dvi *.info
