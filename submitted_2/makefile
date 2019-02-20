AE_paper.pdf: AE_paper.tex refs.bib
	pdflatex AE_paper
	bibtex AE_paper
	pdflatex AE_paper
	pdflatex AE_paper
	pdflatex AE_paper
	pdflatex AE_paper
	pdftk AE_paper.pdf output AE_paper_repaired.pdf
	mv AE_paper_repaired.pdf  AE_paper.pdf

diff.tex: original.tex AE_paper.tex
	latexdiff original.tex AE_paper.tex > diff.tex

diff.pdf: diff.tex refs.bib
	pdflatex diff
	bibtex diff
	pdflatex diff
	pdflatex diff
	pdflatex diff
	pdflatex diff
	pdftk diff.pdf output diff_repaired.pdf
	mv diff_repaired.pdf  diff.pdf	

local_refs.bib: AE_paper.aux refs.bib
	cat refs.bib | aux2bib AE_paper.aux > local_refs.bib

AE_paper.aux: AE_paper.tex refs.bib

make clean:
	rm *.aux *.bbl *.blg *.log *.dvi *.lof *.ttt *.fff *.out *.run.xml *-blx.bib

