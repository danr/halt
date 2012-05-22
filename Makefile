GHC_OPTS   = -Wall -O -package ghc -isrc -hidir dist -odir dist
GHC        = ghc $(GHC_OPTS) --make
TEST_OPTS  = +RTS -N4 -K32M -la -s
EXECUTABLE = halt

halt:   src/Halt/**hs src/Main.hs src/Contracts/*hs
	$(GHC) src/Main.hs -o $(EXECUTABLE)

clean:
	rm -rf $(EXECUTABLE) dist/**
