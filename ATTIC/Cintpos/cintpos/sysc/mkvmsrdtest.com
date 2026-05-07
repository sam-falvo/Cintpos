$ cc/decc /DEFINE="forVmsItanium" vmsrdtest.c
$ link vmsrdtest
