gltst:	tigermothmodel.mdl gltst.b
	cintsys -c c b gltst
	cintsys -- gltst


gltst2.o:	gltst2.c
	gcc -c gltst2.c

gltst2:	gltst2.o
	gcc -o gltst2 gltst2.o -L./.i386-linux-gnu -lglut

sdltst:	tigermothmodel.mdl
	gcc -o sdltst sdltst.c `sdl-config --cflags --libs`

tigermothmodel.mdl: mktigermothmodel
	cintsys -c mktigermothmodel


mktigermothmodel: mktigermothmodel.b
	cintsys -c c b mktigermothmodel

xreftiger:
	cintsys -c c b tiger xref >rawxref
	cintsys -c sortxref rawxref to xtiger
	rm rawxref

xrefrubik:
	cintsys -c c b rubik xref >rawxref
	cintsys -c sortxref rawxref to xrubik
	rm rawxref

xrefrobots:
	cintsys -c c b robots xref >rawxref
	cintsys -c sortxref rawxref to xrobots
	rm rawxref

xrefastar:
	cintsys -c c b astar xref >rawxref
	cintsys -c sortxref rawxref to xastar
	rm rawxref

xrefgltst:	gltst.b
	cintsys -c c b gltst xref >rawxref
	cintsys -c sortxref rawxref to xrefgltst
	rm rawxref

xrefgltext:	gltext.b
	cintsys -c c b gltext xref >rawxref
	cintsys -c sortxref rawxref to xrefgltext
	rm rawxref

clean:
	rm -f mktigermothmodel tigermothmodel.mdl gltst
