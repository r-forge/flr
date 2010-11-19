all:  sweave pdf clean

sweave:
	R CMD Sweave intro2FLR.Rnw

pdf:
	pdflatex intro2FLR.Rnw
	pdflatex intro2FLR.Rnw
	pdflatex intro2FLR.Rnw

clean:
	rm -f *.aux
	rm -f *.log
	rm -f *.out
	rm -f *.nav
	rm -f *.snm
	rm -f *.toc
	rm -f *-*.pdf
	rm -f *.eps
	rm Rplots.pdf

cleanall: clean
	rm -f *.tex
	rm -f *.pdf

