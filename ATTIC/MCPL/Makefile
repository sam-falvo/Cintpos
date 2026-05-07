# This Makefile creates .tgz and .zip versions
# of the MCPL directory

PUB = /homes/mr/public_html

# Public HTML directory if not mountable on this machine
# and the shared drive is called E: (/dose on Linux).
# Remember to call ssh-add before calling make sshpub.
SSHPUB = sandy.cl.cam.ac.uk:public_html

help:
	@echo
	@echo "make all      Construct the files: mcpl.tgz and mcpl.zip"
	@echo "make dosd     Put them in my D drive (/dosd)"
	@echo "make dose     Put them in my E drive (/dose)"
	@echo "make pub      Put them also in my home page"
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
	cp -p mintcode/doc/README .
	(cd natmcpl; make clean)
	(cd mintcode; make clean)
	(cd mcplprogs; make clean)
	(cd mcplprogs/taut/check; make clean)
	(cd ..; tar cvzf mcpl.tgz MCPL)
	(cd ..;  zip -rv9 mcpl.zip MCPL)
	cp TGZDATE FILES
	ls -l ../mcpl.tgz ../mcpl.zip>>FILES

pub:	dosd
	cp README FILES ../mcpl.tgz ../mcpl.zip $(PUB)/MCPL
	cat FILES

sshpubd:	dosd
	scp README FILES ../mcpl.tgz ../mcpl.zip $(SSHPUB)/MCPL
	cat FILES

sshpube:	dose
	scp README FILES ../mcpl.tgz ../mcpl.zip $(SSHPUB)/MCPL
	cat FILES

dosd:	all
	cp ../mcpl.tgz ../mcpl.zip /dosd

dose:	all
	cp ../mcpl.tgz ../mcpl.zip /dose
