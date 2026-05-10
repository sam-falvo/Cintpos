SECTION "prog"

GET "libhdr"

LET start() = VALOF
{ wrs("ABCD*n")
  wrx(#x3770, 6)
  wrs("XXX*n")
  RESULTIS 0
}

AND wrs(s) BE FOR i = 1 TO s%0 DO sawrch(s%i)

AND wrx(n, d) BE
{ IF d>1 DO wrx(n>>4, d-1)
  sawrch((n&15)!TABLE '0','1','2','3','4','5','6','7',
                      '8','9','A','B','C','D','E','F' )
}



