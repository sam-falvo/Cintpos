# This makefile is used to create .tgz versions
# of the bcplman distribution.

# If BCPL/ is not yet ready for distribution on my home page it can
# still be sent there as bcplnew.tgz so that it can be copied easily
# to other machines. This is done by: make sshbcplnew.

PUB = /homes/mr/public_html

# Public HTML directory if not mountable on this machine
# and the shared drive is called E: (/dose on Linux).
# Remember to call ssh-add before calling make sshpub.
SSHPUB = mr10@ely.cl.cam.ac.uk:public_html

help:
	@echo
	@echo "make all          Construct: bcpl.tgz"
	@echo "                  leaving them in distribution/"
	@echo "make sshpube      Put them in /dose and my home page using scp"
	@echo
	@echo "make allnew       Construct: bcplnew.tgz"
	@echo "make dosenew      Put them in my E drive"
	@echo "make sshpubenew   Put them in /dose and my home page using scp"
	@echo
	@echo "make copytoprev   Copy bcpl.tgz and FILES to" 
	@echo "                  bcplprev.tgz and FILESPREV" 
	@echo "                  on my homepage" 
	@echo 


all:
	rm -f *~ */*~
	rm -f TGZFILES
	date >TGZDATE
	cp TGZDATE cintcode/TGZDATE
	cp cintcode/doc/README .
	(cd cintcode; make vclean)
	(cd natbcpl; make clean)
	(cd bcplprogs; make vclean)
	chmod 664 .git/objects/*/*
	(cd ..; tar czf bcpl.tgz BCPL)
	ls -l ../bcpl.tgz >TGZFILES

allnew:
	rm -f *~ */*~
	rm -f TGZFILESNEW
	date >TGZDATENEW
	cp TGZDATENEW cintcode/TGZDATENEW
	cp cintcode/doc/README .
	(cd cintcode; make vclean)
	(cd natbcpl; make clean)
	(cd bcplprogs; make vclean)
	(cd ..; tar czf bcplnew.tgz BCPL)
	ls -l ../bcplnew.tgz >TGZFILESNEW

sshpube:	dose
	scp README TGZFILES ../bcpl.tgz $(SSHPUB)/BCPL
	cp TGZDATE PUBDATE
	cp TGZDATE cintcode/PUBDATE
	cp TGZFILES PUBFILES
	@cat TGZFILES
	@cat TGZDATE

sshpubenew:	dosenew
	scp README TGZFILESNEW ../bcplnew.tgz $(SSHPUB)/BCPL
	cp TGZDATENEW PUBDATENEW
	cp TGZDATENEW cintcode/PUBDATENEW
	cp TGZFILESNEW PUBFILESNEW
	@cat TGZFILESNEW
	@cat TGZDATENEW

copytoprev:
	scp $(SSHPUB)/BCPL/TGZFILES $(SSHPUB)/BCPL/TGZFILESPREV
	scp $(SSHPUB)/BCPL/bcpl.tgz $(SSHPUB)/BCPL/bcplprev.tgz
	@echo "bcplprev.tgz TGZFILESPREV updated"
	@echo


dose:	all
	cp ../bcpl.tgz /dose

dosenew:	allnew
	cp ../bcplnew.tgz /dose
