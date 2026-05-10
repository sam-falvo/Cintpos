# Makefile to help development of the reconstructed BCPL compiler
# for the BBC Microcomputer.

help:
	@echo
	@echo "make pdfs      Make all the .pdf files"
	@echo "make xref      Create xrefbcpl"
	@echo


xref:
	cintsys -c c bc32all xref >rawxref
	cintsys -c sortxref rawxref to xrefbcpl
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

