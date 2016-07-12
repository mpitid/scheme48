
.PHONY: clean distclean

OUT := schemer

all: $(OUT)

%: %.hs
	ghc --make $<

clean:
	$(RM) $(OUT).hi $(OUT).o

distclean: clean
	$(RM) $(OUT)
