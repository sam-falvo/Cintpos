# This makefile is used to create cintpos.tgz

PUB = /homes/mr/public_html

# My public HTML directory when not mountable on this machine.
# Remember to call ssh-add before calling make sshpub
SSHPUB = ely.cl.cam.ac.uk:public_html

help:	
	@echo
	@echo "make all      Construct the files: cintpos.tgz and cintpos.zip"
	@echo "make dosd     Put them in /dosd"
	@echo "make dose     Put them in /dose"
	@echo "make pub      Put them in my home page"
	@echo "make sshpubd  Put them in /dosd and my home page using scp"
	@echo "make sshpube  Put them in /dose and my home page using scp"
	@echo

all:
	rm -f *~ */*~
	echo >TGZDATE
	echo -n "Distributed from machine: " >>TGZDATE
	hostname >>TGZDATE
	date >>TGZDATE
	rm -f FILES
	cp -p cintpos/doc/README .
	(cd cintpos; make vclean)
	(cd ..; tar zcvf cintpos.tgz Cintpos)
	(cd ..; rm -f cintpos.zip; zip -rv9 cintpos.zip Cintpos)
	cp TGZDATE FILES
	ls -l ../cintpos.tgz ../cintpos.zip>>FILES

pub:	dosd
	cp README FILES ../cintpos.tgz ../cintpos.zip $(PUB)/Cintpos
	cat FILES

sshpubd:	dosd
	scp README FILES ../cintpos.tgz ../cintpos.zip $(SSHPUB)/Cintpos
	cat FILES

sshpube:	dose
	scp README FILES ../cintpos.tgz ../cintpos.zip $(SSHPUB)/Cintpos
	cat FILES

dosd:	all
	cp ../cintpos.tgz ../cintpos.zip /dosd

dose:	all
	cp ../cintpos.tgz ../cintpos.zip /dose



