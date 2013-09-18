#TARGET:=slides.pdf slides-notes.pdf handouts.pdf article.pdf
TARGET:=slides.pdf

TEX_PROG:=lualatex
RM:=rm -f

SRC_PARTS:=
SRC_COMMON:=config.tex main.tex title.tex
SRC:=$(SRC_ALL) $(SRC_COMMON)

.PHONY: all clean dist-clean graph

all: $(TARGET)

%.pdf: %.tex $(SRC)
	$(TEX_PROG) $<
	bibtex $* || true
	$(TEX_PROG) $<
	$(TEX_PROG) $<

graph:
	cd graph && make || true

dist-clean: clean
	@$(RM) *.pdf

clean: 
	@$(RM) *.aux *.log *.blg *.nav *.out *.snm *.toc *.bbl *.sta
