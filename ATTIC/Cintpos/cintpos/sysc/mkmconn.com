$ cc/decc /DEFINE="forVmsItanium" mconn.c
$ cc/decc /DEFINE="forVmsItanium" kblib.c
$ link mconn, kblib
