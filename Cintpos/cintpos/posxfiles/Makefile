help:
	@echo
	@echo "make          Output this help information"
	@echo "make all      Create cintpos.xc"
	@echo "make SONY8GB  Copy posfiles directory to SONY8GB mem stick"
	@echo "make HPA8GB   Copy posfiles directory to HPA8GB mem stick"
	@echo "make clean    Remove unnecessary files"
	@echo

all:
	(cd ..; \
	cintsys -c xcencode bin list posxfiles/cintpos-files to posxfiles/cintpos.xc)

send:	all
	mail mrich177@ford.com -s cintpos-files <cintpos.xc

SONY8GB:	all
	(cd ..; cp -r posxfiles /media/SONY8GB/)

HPA8GB:	all
	(cd ..; cp -r posxfiles /media/HPA8GB/)

SANDISC32:	all
	(cd ..; cp -r posxfiles /media/SANDISC32/)

clean:
	rm -f *~
