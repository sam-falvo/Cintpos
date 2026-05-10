all:	pdfs
	echo "all done"

clean:	
	rm -f BCPL.pdf BCPLARG.pdf BCPLSYN.pdf BCPLTRN.pdf BCPLCCG.pdf ED.pdf bcplrom.pdf 
	rm -f maps pdfs

maps:	BCPL.map BCPLARG.map BCPLSYN.map BCPLTRN.map BCPLCCG.map ED.map bcplrom.map 
	(cd ..; cintsys -c c dfall)
	touch maps

pdfs:	maps
	a2ps BCPL.map -o BCPL.ps
	ps2pdf BCPL.ps  BCPL.pdf
	rm BCPL.ps
	a2ps BCPLARG.map -o BCPLARG.ps
	ps2pdf BCPLARG.ps  BCPLARG.pdf
	rm BCPLARG.ps
	a2ps BCPLSYN.map -o BCPLSYN.ps
	ps2pdf BCPLSYN.ps  BCPLSYN.pdf
	rm BCPLSYN.ps
	a2ps BCPLTRN.map -o BCPLTRN.ps
	ps2pdf BCPLTRN.ps  BCPLTRN.pdf
	rm BCPLTRN.ps
	a2ps BCPLCCG.map -o BCPLCCG.ps
	ps2pdf BCPLCCG.ps  BCPLCCG.pdf
	rm BCPLCCG.ps
	a2ps ED.map -o ED.ps
	ps2pdf ED.ps  ED.pdf
	rm ED.ps
	a2ps bcplrom.map -o bcplrom.ps
	ps2pdf bcplrom.ps  bcplrom.pdf
	rm bcplrom.ps
	touch pdfs


