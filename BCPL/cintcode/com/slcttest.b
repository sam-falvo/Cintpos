/*
This is a test program for op:= assignments.

Last updated by Martin Richards (c) December 2018

This version will in due course be added to cmpltest.

It tests all floating point operations including all the
sys(Sys_flt,op,...) operations, the SLCT-OF operations.
It has been designed to test both 32 and 64 bit versions of BCPL.

The ONLY free variable of this program is: sys (for wrch) and calls of
sys(Sys_flt, op, ...)

This program uses floating point constants and the new FLT features.
*/

SECTION "fcmpltest"

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
  sawritef("%13.5f ", x)
  //wrd(FIX x, 8)
  //wrc('.')
  //wrn((FIX (x #* 10.0)) MOD 10)
  //wrc(' ')
//sawritef("x=%13e y=%13e*n", x, y)
  TEST x-1 = y | x=y | x+1=y
  THEN wrs("OK")
  ELSE { wrs("FAILED  It should be ")
         wrx(y, 8)
         wrc(' ')
         sawritef("%13.5f", y)
         //wrn(FIX y)
         //wrc('.')
         //wrn((FIX (y #* 10.0)) MOD 10)
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

  
  //wrz(12345678, 4); nl()

  //wrf(12.34, 10, 4); nl()
  //wrf(#-12.34, 10, 4); nl()
  //wrf(0.1234, 10, 4); nl()
  //wrf(#-0.1234, 10, 4); nl()
//RESULTIS 0
//abort(1000)    
  tester(0.0, 1.0, 2.0, v1, v2)

//{ LET n = 1   // special test for the << and >> operators
//  FOR i = -5 TO 80 DO writef("%i4 %xP*n", i, 1<<i)
//  FOR i = -5 TO 80 DO writef("%i4 %xP*n", i, msb>>i)
//}
    
  RESULTIS 0
}

AND tester(x, y, z, v1, v2) BE
{ LET n0, n1, n2, n3, n4 = 0.0, 1.0, 2.0, 3.0, 4.0
  LET n5, n6, n7, n8, n9 = 5.0, 6.0, 7.0, 8.0, 9.0
  LET oct1775 = #1775

//  wrs("*n Floating point cgtester entered*N")

//  FIRST INITIALIZE CERTAIN VARIABLES

  f, g, h := 100.0, 101.0, 102.0
  testno, testcount, failcount := 0.0, 0.0, 0.0
  v, w := v1, v2

  FOR i = 0 TO 200 DO v!i, w!i := 1000.0 #+ FLOAT i, 10000.0 #+ FLOAT i

  quiet := FALSE

//  TEST SIMPLE VARIABLES AND EXPRESSIONS
//sawritef("x=%x8 y=%x8 z=%x8*n", x, y, z)
//sawritef("n0=%x8 n1=%x8 n2=%x8*n", n0, n1, n2)
  testno := 1

  ft(a#+b#+c, 33.0)        // 1
  ft(f#+g#+h, 303.0)
  ft(x#+y#+z, 3.0)

  ft(123.0#+321.0#-400.0, 44.0)  // 4

  testno := 5
//sawritef("testno=%n*n", testno)
//sawritef("i0=%n i1=%n i2=%n*n", i0, i1, i2)
//sawritef("f0=%14e f1=%13e f2=%13e*n", f0, f1, f2)
  t(x #= 0.0, TRUE)       // 5
//sawritef("testno=%n Calling ft(%13e #= %13e, %n)   y#=0.0=%n*n", testno, y, 0.0, FALSE, y#=0.0)
//abort(1003)
  t(y #= 0.0, FALSE)
//sawritef("testno=%n Calling ft(%13e, %13e)*n", testno, !(@y+i0), 1.0)
  ft(!(@y+i0), 1.0)
//abort(1002)
  ft(!(@b+i0), 11.0)
  ft(!(@g+i0), 101.0)

  testno := 10
  x, a, f := 5.0, 15.0, 105.0
  ft(x, 5.0)            // 10
  ft(a, 15.0)
  ft(f, 105.0)

  v!1, v!2 := 1234.0, 5678.0
  ft(v!1, 1234.0)       // 13
  ft(v!i2, 5678.0)

  ft(x #* a, 75.0)         //  15
  ft(1.0 #* x #+ 2.0 #* y #+ 3.0 #* z #+ f #* 4.0, 433.0)

  testno := 17

  ft(x #* a #+ a #* x, 150.0)  // 17

  testno := 18

  ft(100.0 #/ (a #- a #+ 2.0), 50.0) //  18
  ft(a #/ x, 3.0)
  ft(a #/ #- x, #- 3.0)  // 20
  ft((#- a) #/ x, #- 3.0)
  ft((#-a)#/(#-x), 3.0)
  ft((a#+a) #/ a, 2.0)
  ft((a #* x) #/ (x #* a), 1.0)
  ft((a #+ b) #* (x #+ y) #* 123.0 #/ (6.0 #* 123.0), 26.0)

  ft(#-f, #-105.0)       //  26

  t(FIX 110.534, 111)
  t(FIX (#-110.534), -111)

  testno := 30

  f := 105.0
  t(f #= 105.0, TRUE)   // 30
  t(f#~= 105.0, FALSE)
  t(f #< 105.0, FALSE)
  t(f#>= 105.0, TRUE)
  t(f #> 105.0, FALSE)
  t(f#<= 105.0, TRUE)

  f := 104.0
  t(f #= 105.0, FALSE)  // 36
  t(f#~= 105.0, TRUE)
  t(f #< 105.0, TRUE)
  t(f#>= 105.0, FALSE)
  t(f #> 105.0, FALSE)
  t(f#<= 105.0, TRUE)

  f := 0.0
  testno := 42

  t(f #= 0.0, TRUE)    // 42
  t(f#~= 0.0, FALSE)
  t(f #< 0.0, FALSE)
  t(f#>= 0.0, TRUE)
  t(f #> 0.0, FALSE)
  t(f#<= 0.0, TRUE)

  f := 1.0
  t(f #= 0.0, FALSE)   // 48
  t(f#~= 0.0, TRUE)

  testno := 50

  t(f #< 0.0, FALSE)
  t(f#>= 0.0, TRUE)
  t(f #> 0.0, TRUE)
  t(f#<= 0.0, FALSE)

  testno := 300  // TEST EXPRESSION OPERATORS

  f := 105.0
  ft((2.0#+3.0)#+f#+6.0,116.0)   // 300
  ft(f#+2.0#+3.0#+6.0,116.0)
  ft(6.0 #+ 3.0 #+ 2.0 #+ f, 116.0)
  ft(f#-104.0, 1.0)
  t((x#+2.0)#=(x#+2.0)->99,98, 99)

  testno := 305

  t(f #< f#+1.0 -> 21,22, 21)    // 305

  testno := 306

  t(f #> f#+1.0 ->31,32, 32)    // 306
  t(f #<= 105.0 ->41,42, 41)
  t(f #>= 105.0 ->51,52, 51)

  testflt()

  testslct()

  nl()
  wrn(testcount)
  wrs(" TESTS COMPLETED, ")
  wrn(failcount)
  wrs(" FAILURE(S)*N")
}

AND testflt() BE
{ LET x, y = 0,0
  testno := 3000
  t(FIX #-(FLOAT 123456), FIX FLOAT -123456) // 3000
  t(#- (FLOAT 123456), FLOAT -123456) 
  t(FIX(#ABS (FLOAT -123456)), FIX(FLOAT 123456)) 
  t(#ABS (FLOAT -1), FLOAT 1) 
  t(FLOAT 123456 #* FLOAT 2, FLOAT 246912) 

  testno := 3005

  t(FLOAT 246912 #/ FLOAT 2,     FLOAT 123456)    // 3005
  t(FLOAT  12345 #+ FLOAT 54321, FLOAT 66666) 
  t(FLOAT  12345 #- FLOAT 1234,  FLOAT 11111) 

  UNLESS sys(Sys_flt, fl_avail)=-1 DO
  { wrs("sys(Sys_flt, ...) not available*n")
  }

  t(sys(Sys_flt, fl_mk, 12345, -1), 1234.5)

  testno := 3010

  x := sys(Sys_flt, fl_unmk, 1234.5)
  y := result2

  t(x, 12345)                                 // 3010
  t(y, -1)

  x := sys(Sys_flt, fl_unmk, #-1234.5)
  y := result2
  t(x, -12345)
  t(y, -1)

  testno := 3014

  x := sys(Sys_flt, fl_float, 123456); t(x, FLOAT 123456) // 3014
  x := sys(Sys_flt, fl_fix, 12345.6); t(x, FIX 12345.6)
  x := sys(Sys_flt, fl_abs, 12345.6); t(x, #ABS 12345.6)
  x := sys(Sys_flt, fl_abs, #-12345.6); t(x, #ABS #-12345.6)
  x := sys(Sys_flt, fl_mul, 12.5, 43.5); t(x, 12.5 #* 43.5)
  x := sys(Sys_flt, fl_div, 12.5, #-43.5); t(x, 12.5 #/ #-43.5)

  testno := 3020

  x := sys(Sys_flt, fl_add, 12.5, 43.5); t(x, 12.5 #+ 43.5) // 3020
  x := sys(Sys_flt, fl_sub, 12.5, 43.5); t(x, 12.5 #- 43.5)
  x := sys(Sys_flt, fl_pos, #-12345.6); t(x, #+ #-12345.6)
  x := sys(Sys_flt, fl_neg, #-12345.6); t(x, #- #-12345.6)

  testno := 3030

  FOR a = -2 TO 2 FOR b = -2 TO 2 DO
  { x, y := FLOAT a, FLOAT b
    //wrn(testno,10); nl()
    t(sys(Sys_flt, fl_eq, x, y), x #=  y)
    t(sys(Sys_flt, fl_ne, x, y), x #~= y)
    t(sys(Sys_flt, fl_ls, x, y), x #<  y)
    t(sys(Sys_flt, fl_gr, x, y), x #>  y)
    t(sys(Sys_flt, fl_le, x, y), x #<= y)
    t(sys(Sys_flt, fl_ge, x, y), x #>= y)
  }

  testno := 3200

  t(FIX(sys(Sys_flt, fl_acos, 0.5)#*1000000.0), 1047198) // 3200
  t(FIX(sys(Sys_flt, fl_asin, 0.5)#*1000000.0),  523599)
  t(FIX(sys(Sys_flt, fl_atan, 0.5)#*1000000.0),  463648)
  t(FIX(sys(Sys_flt, fl_atan2, 0.5, 0.4)#*1000000.0),    896055)
  t(FIX(sys(Sys_flt, fl_cos, 0.5)#*1000000.0),   877583)

  testno := 3205

  t(FIX(sys(Sys_flt, fl_sin,  0.5)#*1000000.0),  479426) // 3205
  t(FIX(sys(Sys_flt, fl_tan,  0.5)#*1000000.0 #+ 0.1),  546303) //????
  t(FIX(sys(Sys_flt, fl_cosh, 0.5)#*1000000.0), 1127626)
  t(FIX(sys(Sys_flt, fl_sinh, 0.5)#*1000000.0),  521095) //?
  t(FIX(sys(Sys_flt, fl_tanh, 0.5)#*1000000.0),  462117)
  t(FIX(sys(Sys_flt, fl_exp,  1.0)#*1000000.0), 2718282)

  x := sys(Sys_flt, fl_frexp, 1023.0)
  y := result2
  t(x, 1023.0#/1024.0)
  t(y, 10)

  x := sys(Sys_flt, fl_ldexp, 1023.0#/1024.0, 10)
  t(x, 1023.0)

  t(FIX(sys(Sys_flt, fl_log, 0.5)#*1000000.0),    -693147)
  t(FIX(sys(Sys_flt, fl_log10, 0.5)#*1000000.0),  -301030)

  x := sys(Sys_flt, fl_modf, 123.25)
  y := result2
  t(x, 0.25)
  t(y, 123.0)

  t(FIX(sys(Sys_flt, fl_pow, 2.0, 0.5)#*1000000.0),  1414214)
  t(FIX(sys(Sys_flt, fl_sqrt, 2.0)#*1000000.0),  1414214)
  t(FIX(sys(Sys_flt, fl_ceil, 2.5)#*1000000.0),  3000000)
  t(FIX(sys(Sys_flt, fl_ceil, #-2.5)#*1000000.0),  -2000000)
  t(FIX(sys(Sys_flt, fl_floor, 2.5)#*1000000.0), 2000000)
  t(FIX(sys(Sys_flt, fl_floor, #-2.5)#*1000000.0), -3000000)

  t(FIX(sys(Sys_flt, fl_mod, 100.25, 25.0)#*1000000.0),  250000)
  t(100.25 #MOD 25.0, 0.25)

  testno := 3500
  t(sys(Sys_flt, fl_F2N, 1_000, 1.234),  1_234)
  t(sys(Sys_flt, fl_N2F, 1_000, 1_234),  1.234)
}

AND testslct() BE
{ LET a, b, c = 1, 2, 3
  LET p = @a
  LET f1_1 = sys(Sys_flt, fl_mk, 11, -1)
  LET f2_2 = sys(Sys_flt, fl_mk, 22, -1)
  LET f3_3 = sys(Sys_flt, fl_mk, 33, -1)
  LET FLT x, FLT y, FLT z = f1_1, f2_2, f3_3
  LET fp = @ x

  testno := 4000

  // Testing E1 op:= E2 for
  // op = ! * #* / #/ MOD #MOD + #+ - #- << >> & | EQV and XOR
  // #* #/ #MOD #+ #-

  writef("Testing !:=*n")
  a, b := 1, 2                // !:=
  p := @a
  p !:= 0; t(p, a)
  p := @a
  p !:= 1; t(p, b)

  writef("Testing:  **:=  /:=  MOD:=  +:=  -:=*n")
  a, b := 3, 5                // *:=
  a *:= b; t(a, 15)
  a, b := -3, 5
  a *:= b; t(a, -15)
  p := @a
  b, c := #x12345118, 2
  (SLCT 8:4:1) OF p *:= c; t(b, #x12345228)

  a, b := 30, 5               // /:=
  a /:= b; t(a, 6)
  a, b := -30, 5
  a /:= b; t(a, -6)

  a, b := 30, 7               // MOD:=
  a MOD:= b; t(a, 2)
  a, b := -30, 7
  a MOD:= b; t(a, -2)

  a, b := 3, 5                // +:=
  a +:= b; t(a, 8)
  a, b := -3, 5
  a +:= b; t(a, 2)

  a, b := 3, 5                // -:=
  a -:= b; t(a, -2)
  a, b := -3, 5
  a -:= b; t(a, -8)

  writef("Testing:  <<:=  >>:=  &:=  |:=  EQV:=  XOR:=*n")

  a, b := #b111, 4            // <<:=
  a <<:= b; t(a, #b1110000)
  a, b := -#b111, 4
  a <<:= b; t(a, -#b1110000)

  a, b := #b1110000, 2        // >>:=
  a >>:= b; t(a, #b11100)
  a, b := -#b1110000, 2
  a >>:= b; t(a, -#b11100 & (~(3<<(BITSPERBCPLWORD-2))))

  a, b := #b1100, #b1010      // &:=
  a &:= b; t(a, #b1000)

  a, b := #b1100, #b1010      // |:=
  a |:= b; t(a, #b1110)

  a, b := #b1100, #b1010      // EQV:=
  a EQV:= b; t(a, ~#b0110)

  a, b := #b1100, #b1010      // XOR:=
  a XOR:= b; t(a, #b0110)

  writef("Testing #**:= #/:= #MOD:=*n")

  x, y := f1_1, 3.0           // #*:=
  x #*:= y; ft(x, f3_3)
  x, y := #-f1_1, 3.0
  x #*:= y; ft(x, #-f3_3)

  x, y := f1_1, f2_2           // #/:=
  x #/:= y; ft(x, 0.5)
  x, y := #-f1_1, f2_2
  x #/:= y; ft(x, #-0.5)

  x, y := f3_3, f2_2           // #MOD:=
  x #MOD:= y; ft(x, f1_1)
  x, y := #-f3_3, f2_2
  x #MOD:= y; ft(x, #-f1_1)

  writef("Testing #+:= #-:=*n")

  x, y := f1_1, f2_2           // #+:=
  x #+:= y; ft(x, f3_3)
  x, y := #-f1_1, f3_3
  x #+:= y; ft(x, f2_2)

  x, y := f1_1, f2_2           // #-:=
  x #-:= y; ft(x, #-f1_1)
  x, y := #-f1_1, f2_2
  x #-:= y; ft(x, #-f3_3)


}

