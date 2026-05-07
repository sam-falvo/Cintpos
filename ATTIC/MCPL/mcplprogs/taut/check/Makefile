# Last modified: 18 December 1998

chkprog:	chk r2obj r3obj r8obj tgobj
	echo "chk now up-to-date"

chk:	chk.m chk.h
	echo "c mp chk; logout" | cinterp

r2obj:	r2obj.m chk.h
	echo "c mp r2obj; logout" | cinterp

r3obj:	r3obj.m chk.h
	echo "c mp r3obj; logout" | cinterp

r8obj:	r8obj.m chk.h
	echo "c mp r8obj; logout" | cinterp

tgobj:	tgobj.m chk.h
	echo "c mp tgobj; logout" | cinterp

clean:
	rm -f chk r2obj r3obj r8obj tgobj *~ MCODE




