GET "libhdr"

GLOBAL { f:200; g:401; h:602
         testno:203; failcount:204
         v:205; testcount:206; quiet:207; t:208
         bitsperword:210; msb:211; allones:212
         on64:213 // TRUE if running on a 64-bit system 
}

STATIC { a=10.0; b=11.0; c=12.0; w=0.0  }

MANIFEST {
  i0=0; i1=1; i2=2
  f0=0.0; f1=1.0; f2=2.0
}

LET wrc(ch) BE sys(11,ch)   //wrch(ch)

AND wrs(s) BE
  FOR i = 1 TO s%0 DO wrc(s%i)

AND nl() BE wrc('*n')

AND wrd1(n, d) BE wrx(n,8)

AND wrd(n, d) BE
{ LET t = VEC 30
  AND i, k = 0, -n
  IF n<0 DO d, k := d-1, n
  t!i, i, k := -(k REM 10), i+1, k/10 REPEATUNTIL k=0
  FOR j = i+1 TO d DO wrc('*s')
  IF n<0 DO wrc('-')
  FOR j = i-1 TO 0 BY -1 DO wrc(t!j+'0')
}

AND wrn(n) BE wrd(n, 0)

AND wrz(n, d) BE
{ IF d>1 DO wrz(n / 10, d-1)
  wrc(n MOD 10 + '0')
}

AND wrf(x, w, d) BE
{ LET pow = 1
  LET n, frac = 0, 0
  LET neg = x < 0.0
  IF neg DO x := #-x
  FOR i = 1 TO d DO x, pow := x #* 10.0, pow * 10
  n := FIX (x #+ 0.5)
  frac := n MOD pow
  n := n / pow
  IF neg DO n := -n
  TEST n=0 & frac~=0 & neg
  THEN { FOR i = 1 TO w - d - 3 DO wrc(' ')
         wrs("-0")
       }
  ELSE { wrd(n, w - d - 1)
       }
  wrc('.')
  wrz(frac, d)
}

AND wrx(n, d) BE
{ IF d>1 DO wrx(n>>4, d-1)
  wrc((n&15)!TABLE '0','1','2','3','4','5','6','7',
                   '8','9','A','B','C','D','E','F' )
}

LET t(x, y) = VALOF
{ testcount := testcount + 1
  wrd(testno, 4)
  wrs("         ")
  wrd(x, (on64->21,13))
  wrc('(')
  wrx(x, (on64->16,8))
  wrs(")    ")
  wrd(y, (on64->21,13))
  wrc('(')
  wrx(y, (on64->16,8))
  wrs(")")
  TEST x=y
  THEN { wrs(" OK")
       }
  ELSE { wrs(" FAILED")
         failcount := failcount + 1
//abort(1001)
       }
  nl()
  testno := testno + 1
  RESULTIS y
}

LET ft(x, y) = VALOF
{ testcount := testcount + 1
  wrd(testno, 4)
  wrc(' ')
  wrx(x, 8)
  wrc(' ')
  wrd(FIX x, 8)
  wrc('.')
  wrn((FIX (x #* 10.0)) MOD 10)
  wrc(' ')
//sawritef("x=%13e y=%13e*n", x, y)
  TEST x #= y
  THEN wrs("OK")
  ELSE { wrs("FAILED  It should be ")
         wrx(y, 8)
         wrc(' ')
         wrn(FIX y)
         wrc('.')
         wrn((FIX (y #* 10.0)) MOD 10)
         failcount := failcount + 1
//abort(1000)
       }
  nl()
  testno := testno + 1
  RESULTIS y
}

LET t1(a,b,c,d,e,f,g) = t(a+b+c+d+e+f, g)

LET start(parm) = VALOF
{ LET ww = 65
  LET v1 = VEC 200
  AND v2 = VEC 200
  wrs("*nFcmpltest running on a ")
  bitsperword, msb, allones := 1, 1, 1
  UNTIL (msb<<1)=0 DO
    bitsperword, msb, allones := bitsperword+1, msb<<1, allones<<1 | 1
  on64 := bitsperword=64 // =TRUE if running on a 64-bit system
  TEST (@ww)%0=65
  THEN wrs("little")
  ELSE wrs("big")
  wrs(" ender machine*n")
  wrs("The BCPL word is ")
  wrd(bitsperword, 0)
  wrs(" bits long*n*n*n")

  RESULTIS 0  
}


