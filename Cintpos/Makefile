# This makefile is used to create cintpos.tgz

PUB = /homes/mr/public_html

# My public HTML directory when not mountable on this machine.
# Remember to call ssh-add before calling make sshpub
SSHPUB = mr10@ely.cl.cam.ac.uk:public_html

help:	
	@echo
	@echo "make all      Construct the files: cintpos.tgz and cintpos.zip"
	@echo "make dose     Put them in /dose"
	@echo "make sshpube  Put them in /dose and my home page using scp"
	@echo "make pub      Same as make sshpube"
	@echo

all:
	rm -f *~ */*~
	rm -f TGZFILES
	date >TGZDATE
	cp -p cintpos/doc/README .
	(cd cintpos; make vclean)
	(cd ..; tar zcvf cintpos.tgz Cintpos)
	(cd ..; rm -f cintpos.zip; zip -rv9 cintpos.zip Cintpos)
	ls -l ../cintpos.tgz ../cintpos.zip >TGZFILES

pub:	sshpube

sshpube:	dose
	scp README TGZFILES ../cintpos.tgz ../cintpos.zip $(SSHPUB)/Cintpos
	cp TGZDATE PUBDATE
	cp TGZFILES PUBFILES
	@cat TGZFILES
	@cat TGZDATE

dose:	all
	cp ../cintpos.tgz ../cintpos.zip /dose



