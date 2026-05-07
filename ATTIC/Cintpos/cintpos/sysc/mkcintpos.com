$ cc/decc /DEFINE="forVmsItanium" cintpos.c
$ cc/decc /DEFINE="forVmsItanium" cinterp.c
$ cc/decc /DEFINE="forVmsItanium" kblib.c
$ cc/decc /DEFINE="forVmsItanium" nullrastlib.c
$ cc/decc /DEFINE="forVmsItanium" devices.c
$ link cintpos, cinterp, kblib, nullratlib, devices /threads=upcalls
