# Makefile to help development of the reconstructed BCPL compiler
# for the BBC Microcomputer.

help:
	@echo
	@echo "make xref         Create xrefall"
	@echo "make pdfs         Create the .pdf files"
	@echo


xref:
	cintsys -c c bc32 bcpl xref >rawxref
	cintsys -c c bc32 bcplarg xref >>rawxref
	cintsys -c c bc32 bcplsyn xref >>rawxref
	cintsys -c c bc32 bcpltrn xref >>rawxref
	cintsys -c c bc32 bcplccg xref >>rawxref
	cintsys -c sortxref rawxref to xrefall
	rm rawxref

xrefccg:
	cintsys -c c bc32 bcplccg xref >rawxref
	cintsys -c sortxref rawxref to xrefccg
	rm rawxref

xbbcbcpl:	bbcbcpl.b
	cintsys -c bcpl bbcbcpl.b xref >rawxref
	cintsys -c sortxref rawxref to xbbcbcpl
	rm rawxref

pdfs:	
	a2ps bcpl.b -o bcpl.ps
	ps2pdf bcpl.ps  bcpl.pdf
	rm bcpl.ps
	a2ps bcplarg.b -o bcplarg.ps
	ps2pdf bcplarg.ps  bcplarg.pdf
	rm bcplarg.ps
	a2ps bcplsyn.b -o bcplsyn.ps
	ps2pdf bcplsyn.ps  bcplsyn.pdf
	rm bcplsyn.ps
	a2ps bcpltrn.b -o bcpltrn.ps
	ps2pdf bcpltrn.ps  bcpltrn.pdf
	rm bcpltrn.ps
	a2ps bcplccg.b -o bcplccg.ps
	ps2pdf bcplccg.ps  bcplccg.pdf
	rm bcplccg.ps
	touch pdfs

clean:
	rm -f pdfs

