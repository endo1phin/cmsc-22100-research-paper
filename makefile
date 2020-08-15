TEX  = pdflatex
FILE = researchreport

.PHONY : clean

all : $(FILE).tex $(FILE).bib
	$(TEX) $(FILE) ; bibtex $(FILE) ; $(TEX) $(FILE) ; $(TEX) $(FILE)

clean :
	rm *pdf *aux *log *bbl *blg