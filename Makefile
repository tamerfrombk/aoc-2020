.PHONY: all clean 0*

all: 0*

0*:
	cd $@ && ghc --make -W -O2 -o $@ Main.hs

clean:
	git clean -xfd
