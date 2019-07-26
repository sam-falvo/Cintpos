# -*- Makefile -*- for MCPL documentation

info:	mcpl.info
	mv mcpl.info* /homes/mr/doc/info/mcpl

all: mcpl.info mcpl.dvi mcpl.html

dvi:	mcpl.dvi

html:	mcpl.html
	mv mcpl*.html /homes/mr/doc/html/mcpl

mcpl.info: mcpl.texinfo
	makeinfo mcpl.texinfo

mcpl.dvi: mcpl.texinfo
	texi2dvi mcpl.texinfo

mcpl.html: mcpl.texinfo
	texi2html -split_chapter -verbose mcpl.texinfo

clean:
	rm -f *~
