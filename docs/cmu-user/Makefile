# -*- makefile -*-

LATEX = latex
BIBTEX = bibtex
PDFLATEX = pdflatex

FILES = *.tex

.SUFFIXES: .aux .bbl .tex .dvi .ps .ps1 .pdf .html .info .gz $(.SUFFIXES)



.tex.dvi: stateart.bbl index
	$(LATEX) $<
	rm $*.log

.tex.aux:
	$(LATEX) $<

.aux.bbl: biblio.bib
	$(BIBTEX) $(basename $<)

.tex.html:
	hevea cmucl-user.hva $<
	hacha -tocbis $@
	-tidy -m *.html

.tex.info:
	hevea -info cmucl-user.hva $<

.tex.pdf: index
	$(PDFLATEX) $<


.dvi.ps1:
	dvips -o $@ $<

.ps1.ps:
	psset -d -o $@ $<

%.gz : %
	gzip --best $<

.INTERMEDIATE: cmucl--user-letter.tex
cmucl-user-letter.tex: cmucl-user.tex
	cp $< $@
	perl -pi -e 's/documentclass\[a4paper\]/documentclass\[letter\]/' $@

install: index cmucl-user.pdf cmucl-user.ps.gz cmucl-user-letter.ps.gz cmucl-user.html cmucl-user.info
	mv *.ps.gz *.pdf $(CONSORG)/doc
	mv *.html *.gif $(CONSORG)/doc/cmucl-user
	tar czf $(CONSORG)/doc/cmucl-user-info.tar.gz *.info
	chmod 644 $(CONSORG)/doc/cmucl-user/*

clean:
	-rm *.log *.bbl *.blg *.ps *.aux *.lof *.toc *.vdx *.cdx *.tdx *.idx *.gif *.out *.ilg

.PHONY: clean


index:
	makeindex cmucl-user.tdx -o cmucl-user.tnd
	makeindex cmucl-user.vdx -o cmucl-user.vnd
	makeindex cmucl-user.fdx -o cmucl-user.fnd
	makeindex cmucl-user.cdx -o cmucl-user.cnd
