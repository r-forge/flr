all:  sweave pdf clean

sweave:
	R CMD Sweave intro_to_flr_bergen.rnw

pdf:
	pdflatex intro_to_flr_bergen
	pdflatex intro_to_flr_bergen
	pdflatex intro_to_flr_bergen

clean:
	rm -f *.aux
	rm -f *.log
	rm -f *.out
	rm -f *.nav
	rm -f *.snm
	rm -f *.toc

cleanall: clean
	rm -f *.tex
	rm -f *.pdf
	rm -f *.pdf
