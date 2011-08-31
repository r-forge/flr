.SUFFIXES: .tex .pdf .Rnw .R

MAIN = FLBioDym

all: $(MAIN).pdf
$(MAIN).pdf: $(MAIN).tex

view: all
	evince $(MAIN).pdf &

.Rnw.R:
	R CMD Stangle $<

.Rnw.tex:
	R CMD Sweave $<

.tex.pdf:
	pdflatex $<
	makeindex $<
	bibtex $*
	pdflatex $<
	pdflatex $<

clean:
	rm -fv $(MAIN).pdf $(MAIN).tex
	rm -fv *.aux *.dvi *.log *.toc *.bak *~ *.blg *.bbl *.lot *.lof *.idx
	rm -fv figs/$(MAIN)-fig*.pdf
	rm -fv *.nav *.snm *.out *.pyc \#*\# _region_* _tmp.* *.vrb
	rm -fv Rplots.pdf *.RData
