/*
This is a test program for the BCPL compiler and Cintcode interpreter

Last updated by Martin Richards (c) December 2018

This version is similar to cmpltest but includes tests for the
floating point operations now available in standard BCPL.

It tests all floating point operations including all the
sys(Sys_flt,op,...) operations, and the SLCT-OF operations.

This program uses floating point constants and the new FLT features.
*/

SECTION "fcmpltest"

GET "libhdr"

GLOBAL {// f:200; g:401; h:602
         FLT ff:300; FLT fg:301; FLT fh:702
         testno:203; failcount:204
         v:205; testcount:206; quiet:207; t:208
         bitsperword:210; msb:211; allones:212
         on64:213 // TRUE if running on a 64-bit system 
}

STATIC { a=10; b=11; c=12; w=0
         FLT fa=10; FLT fb=11; FLT fc=12; FLT fw=0
}

MANIFEST {
  i0=0; i1=1; i2=2
  FLT f0=0.0; FLT f1=1.0; FLT f2=2.0
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

AND wrf(FLT x, w, d) BE
{ LET pow = 1
  LET n, frac = 0, 0
  LET neg = x < 0.0
  IF neg DO x := -x
  FOR i = 1 TO d DO x, pow := x * 10.0, pow * 10
  n := FIX (x + 0.5)
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
  //TEST on64
  //THEN writef("%21i(%16x)    %21i(%16x)", x, x, y, y)
  //ELSE writef("%13i(%8x)    %13i(%8x)", x, x, y, y)

  wrd(x, (on64->21,13))
  wrc('(')
  wrx(x, (on64->16,8))
  wrs(")    ")
  wrd(y, (on64->21,13))
  wrc('(')
  wrx(y, (on64->16,8))
  wrs(")")

  TEST x=y
  THEN { wrs(" OK*n")
       }
  ELSE { wrs(" FAILED*n")
         failcount := failcount + 1
//abort(1001)
       }
  testno := testno + 1
  RESULTIS y
}

LET ft(FLT fx, FLT fy) = VALOF
{ testcount := testcount + 1
  wrd(testno, 4)
  wrc(' ')
  wrx(fx, 8)
  wrc(' ')
  wrd(FIX fx, 8)
  wrc('.')
  wrn((FIX (fx * 10.0)) MOD 10)
  wrc(' ')
//sawritef("fx=%13e fy=%13e*n", fx, fy)
  TEST fx = fy
  THEN wrs("OK")
  ELSE { wrs("FAILED  It should be ")
         wrx(fy, 8)
         wrc(' ')
         wrn(FIX fy)
         wrc('.')
         wrn((FIX (fy * 10.0)) MOD 10)
         failcount := failcount + 1
//abort(1000)
       }
  nl()
  testno := testno + 1
  RESULTIS fy
}

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
  //wrf(-12.34, 10, 4); nl()
  //wrf(0.1234, 10, 4); nl()
  //wrf(-0.1234, 10, 4); nl()
//RESULTIS 0
//abort(1000)    
  tester(0.0, 1.0, 2.0, v1, v2)

//{ LET n = 1   // special test for the << and >> operators
//  FOR i = -5 TO 80 DO writef("%i4 %xP*n", i, 1<<i)
//  FOR i = -5 TO 80 DO writef("%i4 %xP*n", i, msb>>i)
//}
    
  RESULTIS 0
}

AND tester(FLT fx, FLT fy, FLT fz, v1, v2) BE
{ LET n0, n1, n2, n3, n4 = 0.0, 1.0, 2.0, 3.0, 4.0
  LET n5, n6, n7, n8, n9 = 5.0, 6.0, 7.0, 8.0, 9.0

//  wrs("*n Floating point cgtester entered*N")

//  FIRST INITIALIZE CERTAIN VARIABLES

  ff, fg, fh := 100.0, 101.0, 102.0
  testno, testcount, failcount := 0, 0, 0
  v, w := v1, v2

  FOR i = 0 TO 200 DO v!i, w!i := 1000.0 + FLOAT i, 10000.0 + FLOAT i

  quiet := FALSE

//  TEST SIMPLE VARIABLES AND EXPRESSIONS
//sawritef("x=%x8 y=%x8 z=%x8*n", x, y, z)
//sawritef("n0=%x8 n1=%x8 n2=%x8*n", n0, n1, n2)
  testno := 1

  ft(fa+fb+fc, 33.0)        // 1
  ft(ff+fg+fh, 303.0)
  ft(fx+fy+fz, 3.0)

  ft(123.0+321.0-400.0, 44.0)  // 4

  testno := 5
//sawritef("testno=%n*n", testno)
//sawritef("i0=%n i1=%n i2=%n*n", i0, i1, i2)
//sawritef("f0=%14e f1=%13e f2=%13e fx=%13.2f fy=%13.2f fz=%13.2f*n", f0, f1, f2, fx, fy, fz)
//abort(1000)
  t(fx = 0.0, TRUE)       // 5
//sawritef("testno=%n Calling ft(%13e = %13e, %n)   fy=0.0=%n*n", testno, fy, 0.0, FALSE, fy=0.0)
//abort(1003)
  ft(fy = 0.0, FALSE)
//sawritef("testno=%n Calling ft(%13e, %13e)*n", testno, !(@y+i0), 1.0)
  ft(!(@fy+i0), 1.0)
//abort(1002)
  ft(!(@fb+i0), 11.0)
  ft(!(@fg+i0), 101.0)

  testno := 10
  fx, fa, ff := 5.0, 15.0, 105.0
  ft(fx, 5.0)            // 10
  ft(fa, 15.0)
  ft(ff, 105.0)

  v!1, v!2 := 1234.0, 5678.0
  ft(v!1, 1234.0)       // 13
  ft(v!i2, 5678.0)

  ft(fx * fa, 75.0)         //  15
  ft(1.0 * fx + 2.0 * fy + 3.0 * fz + ff * 4.0, 433.0)

  testno := 17

  ft(fx * fa + fa * fx, 150.0)  // 17

  testno := 18

  ft(100.0 / (fa - fa + 2.0), 50.0) //  18
  ft(fa / fx, 3.0)
  ft(fa / - fx, - 3.0)  // 20
  ft((- fa) / fx, - 3.0)
  ft((-fa)/(-fx), 3.0)
  ft((fa+fa) / fa, 2.0)
  ft((fa * fx) / (fx * fa), 1.0)
  ft((fa + fb) * (fx + fy) * 123.0 / (6.0 * 123.0), 26.0)

  ft(-ff, -105.0)       //  26

  t(FIX 110.534, 111)
  t(FIX (-110.534), -111)

  testno := 30

  writef("fcmpltest: Test relations between 105.0 and 105.0*n")
  ff := 105.0
  t(ff = 105.0, TRUE)   // 30
  t(ff~= 105.0, FALSE)
  t(ff < 105.0, FALSE)
  t(ff>= 105.0, TRUE)
  t(ff > 105.0, FALSE)
  t(ff<= 105.0, TRUE)

  writef("fcmpltest: Test relations between 104.0 and 105.0*n")
  ff := 104.0
  t(ff = 105.0, FALSE)  // 36
  t(ff~= 105.0, TRUE)
  t(ff < 105.0, TRUE)
  t(ff>= 105.0, FALSE)
  t(ff > 105.0, FALSE)
  t(ff<= 105.0, TRUE)

  ff := 0.0
  testno := 42

  writef("fcmpltest: Test relations between 0.0 and 0.0*n")
  t(ff = 0.0, TRUE)    // 42
  t(ff~= 0.0, FALSE)
  t(ff < 0.0, FALSE)
  t(ff>= 0.0, TRUE)
  t(ff > 0.0, FALSE)
  t(ff<= 0.0, TRUE)

  ff := 1.0
  writef("fcmpltest: Test relations between 1.0 and 0.0*n")
  t(ff = 0.0, FALSE)   // 48
  t(ff~= 0.0, TRUE)

  testno := 50

  t(ff < 0.0, FALSE)
  t(ff>= 0.0, TRUE)
  t(ff > 0.0, TRUE)
  t(ff<= 0.0, FALSE)

  testno := 80
  ff := 105.0
  writef("fcmpltest: Testing monadic #-*n")
  ft(-ff, -105.0)             // 80

  testno := 130
  ff := 105.0
  writef("fcmpltest: Testing conditional jumps on 105.0 and 105.0*n")
  t(ff = 105 -> 1, 2, 1)   // 130
  t(ff~= 105 -> 1, 2, 2)
  t(ff < 105 -> 1, 2, 2)
  t(ff>= 105 -> 1, 2, 1)
  t(ff > 105 -> 1, 2, 2)
  t(ff<= 105 -> 1, 2, 1)

  ff := 104.0
  writef("fcmpltest: Testing conditional jumps on 104.0 and 105.0*n")
  t(ff = 105 -> 1, 2, 2)  // 136
  t(ff~= 105 -> 1, 2, 1)
testno := 99900          // To make the compiled code easy to find
  t(ff < 105 -> 1, 2, 1)  // failed
testno := 139
  t(ff>= 105 -> 1, 2, 2)
  t(ff > 105 -> 1, 2, 2)
  t(ff<= 105 -> 1, 2, 1)
testno := 142
  ff := 0.0
  writef("fcmpltest: Testing conditional jumps on 0.0 and 0.0*n")
  t(ff = 0 -> 1, 2, 1)    // 142
  t(ff~= 0 -> 1, 2, 2)
  t(ff < 0 -> 1, 2, 2)
  t(ff>= 0 -> 1, 2, 1)
  t(ff > 0 -> 1, 2, 2)
  t(ff<= 0 -> 1, 2, 1)
  ff := 1.0
  writef("fcmpltest: Testing conditional jumps on 1.0 and 0.0*n")
  t(ff = 0 -> 1, 2, 2)   // 148
  t(ff~= 0 -> 1, 2, 1)
  t(ff < 0 -> 1, 2, 2)
  t(ff>= 0 -> 1, 2, 1)
  t(ff > 0 -> 1, 2, 1)
  t(ff<= 0 -> 1, 2, 2)

  testno := 300  // TEST EXPRESSION OPERATORS

  ff := 105.0
  ft((2.0+3.0)+ff+6.0,116.0)   // 300
  ft(ff+2.0+3.0+6.0,116.0)
  ft(6.0 + 3.0 + 2.0 + ff, 116.0)
  ft(ff-104.0, 1.0)
  t((fx+2.0)=(fx+2.0)->99,98, 99)

  testno := 305

  t(ff < ff+1.0 -> 21,22, 21)    // 305

  testno := 99901     // To make the compiled code easy to find

  t(ff  > ff+1.0 ->31,32, 32)    // 306    failed

  testno := 307
  t(ff <= 105.0 ->41,42, 41)
  t(ff >= 105.0 ->51,52, 51)

  testno := 3000
  testflt()
//t(1,2)  // To test %pS substitution item in writef below.
//t(1,2)
  nl()
  wrn(testcount)
  wrs(" TESTS COMPLETED, ")
  wrn(failcount)
  writef(" FAILURE%pS*N", failcount)
}

AND testflt() BE
{ LET x, y = 0, 0
  LET FLT fx, FLT fy = 0, 0
  writef("fcmpltest: testing FIX, FLOAT, #ABS and monadic #-*n")
  t(FIX -(FLOAT 123456), FIX FLOAT -123456) // 3000
  ft(- (FLOAT 123456), FLOAT -123456) 
  t(FIX(ABS (FLOAT -123456)), FIX(FLOAT 123456)) 
  ft(ABS (FLOAT -1), FLOAT 1) 
  writef("fcmpltest: testing #**, #/, mk and unmk*n")
  ft(FLOAT 123456 * FLOAT 2, FLOAT 246912) 

  testno := 3005

  ft(FLOAT 246912 / FLOAT 2,     FLOAT 123456)    // 3005
  ft(FLOAT  12345 + FLOAT 54321, FLOAT 66666) 
  ft(FLOAT  12345 - FLOAT 1234,  FLOAT 11111) 

  UNLESS sys(Sys_flt, fl_avail)=-1 DO
  { wrs("sys(Sys_flt, ...) not available*n")
  }

  ft(sys(Sys_flt, fl_mk, 12345, -1), 1234.5)
//abort(1000)
  testno := 3010

  //x := sys(Sys_flt, fl_unmk, sys(Sys_flt, fl_mk, 12345, -1))
//abort(1001)
  x := sys(Sys_flt, fl_unmk, 1234.5)
  y := result2
//abort(1002)

//wrs("x="); wrn(x); wrs(" y="); wrn(y); nl()

  t(x, 12345)                                 // 3010
  t(y, -1)

  x := sys(Sys_flt, fl_unmk, -1234.5)
  y := result2
  t(x, -12345)
  t(y, -1)

  testno := 3014

  x := sys(Sys_flt, fl_float, 123456);    ft(x, FLOAT 123456) // 3014
  x := sys(Sys_flt, fl_fix,  12345.6);     t(x, FIX  12345.6)
  x := sys(Sys_flt, fl_abs,  12345.6);    ft(x, ABS  12345.6)
  x := sys(Sys_flt, fl_abs, -12345.6);    ft(x, ABS -12345.6)
  x := sys(Sys_flt, fl_mul, 12.5,  43.5); ft(x, 12.5 * 43.5)
  x := sys(Sys_flt, fl_div, 12.5, -43.5); ft(x, 12.5 / -43.5)

  testno := 3020

  x := sys(Sys_flt, fl_add, 12.5, 43.5); ft(x, 12.5 + 43.5) // 3020
  x := sys(Sys_flt, fl_sub, 12.5, 43.5); ft(x, 12.5 - 43.5)
  x := sys(Sys_flt, fl_pos, -12345.6);   ft(x, + -12345.6)
  x := sys(Sys_flt, fl_neg, -12345.6);   ft(x, - -12345.6)

  testno := 3030

  FOR a = -2 TO 2 FOR b = -2 TO 2 DO
  { fx, fy := FLOAT a, FLOAT b
    wrn(testno,10); nl()
    writef("fcmpltest: comparing %n = %n with the sys version*n", a, b)
    t(sys(Sys_flt, fl_eq, fx, fy), fx =  fy)
    writef("fcmpltest: comparing %n ~= %n with the sys version*n", a, b)
    t(sys(Sys_flt, fl_ne, fx, fy), fx ~= fy)
    writef("fcmpltest: comparing %n < %n with the sys version*n", a, b)
    t(sys(Sys_flt, fl_ls, fx, fy), fx <  fy)
    writef("fcmpltest: comparing %n > %n with the sys version*n", a, b)
    t(sys(Sys_flt, fl_gr, fx, fy), fx >  fy)
    writef("fcmpltest: comparing %n <= %n with the sys version*n", a, b)
    t(sys(Sys_flt, fl_le, fx, fy), fx <= fy)
    writef("fcmpltest: comparing %n >= %n with the sys version*n", a, b)
    t(sys(Sys_flt, fl_ge, fx, fy), fx >= fy)
  }

  testno := 3200

  writef("fcmpltest: Testing the trig functions*n")
  t(FIX(sys(Sys_flt, fl_acos, 0.5)*1000000.0),      1047198) // 3200
  t(FIX(sys(Sys_flt, fl_asin, 0.5)*1000000.0),       523599)
  t(FIX(sys(Sys_flt, fl_atan, 0.5)*1000000.0),       463648)
  t(FIX(sys(Sys_flt, fl_atan2, 0.5, 0.4)*1000000.0), 896055)
  t(FIX(sys(Sys_flt, fl_cos, 0.5)*1000000.0),        877583)

  testno := 3205

  t(FIX(sys(Sys_flt, fl_sin,  0.5)*1000000.0),       479426) // 3205
  t(FIX(sys(Sys_flt, fl_tan,  0.5)*1000000.0 + 0.1), 546303) //????
  t(FIX(sys(Sys_flt, fl_cosh, 0.5)*1000000.0),      1127626)
  t(FIX(sys(Sys_flt, fl_sinh, 0.5)*1000000.0),       521095) //?
  t(FIX(sys(Sys_flt, fl_tanh, 0.5)*1000000.0),       462117)
  writef("fcmpltest: Testing the other maths functions*n")
  t(FIX(sys(Sys_flt, fl_exp,  1.0)*1000000.0),      2718282)

  x := sys(Sys_flt, fl_frexp, 1023.0)
  y := result2
  ft(x, 1023.0/1024.0)
  t(y, 10)

  x := sys(Sys_flt, fl_ldexp, 1023.0/1024.0, 10)
  ft(x, 1023.0) //3213

  t(FIX(sys(Sys_flt, fl_log, 0.5)*1000000.0),    -693147)
  t(FIX(sys(Sys_flt, fl_log10, 0.5)*1000000.0),  -301030)

  writef("fcmpltest: Testing sys call of fl_modf*n")

  x := sys(Sys_flt, fl_modf, 123.25)
  y := result2
  ft(x, 0.25)   // 3216
  ft(y, 123.0)  // 3217

  testno := 3218

  t(FIX(sys(Sys_flt, fl_pow, 2.0, 0.5)*1000000.0),  1414214)
  t(FIX(sys(Sys_flt, fl_sqrt, 2.0)*1000000.0),      1414214)
  t(FIX(sys(Sys_flt, fl_ceil, 2.5)*1000000.0),      3000000)
  t(FIX(sys(Sys_flt, fl_ceil, -2.5)*1000000.0),    -2000000)
  t(FIX(sys(Sys_flt, fl_floor, 2.5)*1000000.0),     2000000)
  t(FIX(sys(Sys_flt, fl_floor, -2.5)*1000000.0),   -3000000)

  t(FIX(sys(Sys_flt, fl_mod, 100.25, 25.0)*1000000.0),  250000)
  ft(100.25 MOD 25.0, 0.25)

  testno := 4000
  writef("fcmpltest: Testing F2N and N2F*n")

   t(sys(Sys_flt, fl_F2N, 1_000, 1.234),  1_234)
  ft(sys(Sys_flt, fl_N2F, 1_000, 1_234),  1.234)
}
