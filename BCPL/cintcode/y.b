
GLOBAL {
start:1; sys:3; a; b f:100; w:128
wrc:200; wrs; wrx; wrd; wrd; wrn; nl
}

LET wrc(ch) BE sys(11,ch)   //sawrch(ch)

AND wrs(s) BE
  FOR i = 1 TO s%0 DO wrc(s%i)

AND nl() BE wrc('*n')

AND wrd(n, d) BE //wrx(n,4)
{ LET t = VEC 30
  AND i, k = 0, -n
  IF n<0 DO d, k := d-1, n
  t!i, i, k := -(k REM 10), i+1, k/10 REPEATUNTIL k=0
  FOR j = i+1 TO d DO wrc('*s')
  IF n<0 DO wrc('-')
  FOR j = i-1 TO 0 BY -1 DO wrc(t!j+'0')
}

AND wrn(n) BE wrd(n, 0)

AND wrx(n, d) BE
{ IF d>1 DO wrx(n>>4, d-1)
  wrc((n&15)!TABLE '0','1','2','3','4','5','6','7',
                   '8','9','A','B','C','D','E','F' )
}


LET start(x) = VALOF
{ wrx(#xF, 2)
a := 12  // a is G4
  nl()
  RESULTIS 0
}




