GHC_OPTS   = -O -package ghc -hidir dist -odir dist
GHC        = ghc $(GHC_OPTS) --make
TEST_OPTS  = +RTS -N4 -K32M -la -s
EXECUTABLE = halt

halt:   Halt/*hs FOL/*hs Main.hs
	$(GHC) Main.hs -o $(EXECUTABLE)

clean:
	rm -rf $(EXECUTABLE) dist/**
