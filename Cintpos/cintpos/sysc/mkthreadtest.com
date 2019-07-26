$ cc/decc /DEFINE="forVmsItanium" threadtest.c
$ link threadtest
