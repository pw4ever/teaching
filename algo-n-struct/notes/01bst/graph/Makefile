GRAPHS=arch.pdf test.pdf
PROG_TEX=lualatex

.PHONY: all clean

all: $(GRAPHS)

%.pdf: %.tex
	$(PROG_TEX) $<
	$(PROG_TEX) $<

clean:
	rm -rf *.log *.aux
