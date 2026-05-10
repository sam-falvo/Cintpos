/*
This is a test program for the BCPL to Z80 compiler and the
z80emu Z80 emulator. It is based on cmpltest.b but modified to
test a version of BCPL using a 16 bit word length. The program
had to be reduced in size to keep z80bcpl happy.

This test program is currently under develpment.

Last updated by Martin Richards (c) 27 Sep 2022

This program has the function sys as its only free variable. This provides
an implementation of sawrch.
*/

SECTION "z80cmpltest"

GLOBAL {
  start:1
  sys:3
  brkfn:199
  f:200; g:401; h:802

  w1:201
  w2
  dummy
  testno:203; failcount
  v; testcount; quiet; t
  bitsperword; msb; allones

  ga:300
  gb;gc;gd;ge
  
  wrc
  wrd
  wrx
  wrs
  nl
  
  tstescapes
  tstloop
  tstbreak
  tstendcase
  tstreturn
  tstresultis
  tstnext
  tstexit
}

STATIC {
a=10; b=11; c=12;
w=15;
minus1=-1
}

MANIFEST { k0=0; k1=1; k2=2  }

LET ts(x) BE
{ LET a = 0
  LET w = VALOF
  { a := a+1
    IF a>10 RESULTIS 123
    a := a+12
  } REPEAT

  a := 101
}

LET brkfn(x) BE
{ RETURN
  wrs("brkfn*n")
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

LET t(x, y) = VALOF
{ testcount := testcount + 1

  wrd(testno, 4)
  wrs("         ")
  wrd(x, 6)
  wrc('(')
  wrx(x, 4)
  wrs(")    ")
  wrd(y, 6)
  wrc('(')
  wrx(y, 4)
  wrs(")")
  TEST x=y
  THEN { wrs(" OK")
       }
  ELSE { wrs(" FAILED")
         failcount := failcount + 1
       }
  nl()
  IF testno=202 DO
  { //sys(4) // Sys_tracing
  }
  testno := testno + 1
  RESULTIS y
}

LET t1(a,b,c,d,e,f,g) = t(a+b+c+d+e+f, g)

LET start(parm) = VALOF
{ LET ww = 65
  LET y=1
  //LET n8, n9 = #xFFFF, #xFFFF
  LET v1 = VEC 200
  AND v2 = VEC 200
//    wrs("A")
//FINISH
  testno := 0
//  t(y=0, FALSE)
//FINISH
//  IF FALSE DO
  { // Test the basic output functions.

    //wrc('X'); wrc('Y'); wrc('Z')
    //wrc('*n')
    wrs("ABCD*n")
//FINISH
    wrs("PQR*n")
    //RESULTIS 112
    wrx(#x7FF, 4)
    wrc('*n')
      wrd(123, 0)
    nl()
    //RESULTIS 0
  }

//  wrs("*nz80cmpltest running on a ")
  bitsperword, msb, allones := 1, 1, 1
  UNTIL (msb<<1)=0 DO
  { bitsperword, msb, allones := bitsperword+1, msb<<1, allones<<1 | 1
    //wrs("bitsperword="); wrx(bitsperword, 4)
    //wrs(" msb="); wrx(msb, 4)
    //wrs(" allones="); wrx(allones, 4); wrc('*n')
  }
  //FINISH
/*
  TEST (@ww)%0=65
  THEN wrs("little")
  ELSE wrs("big")
  //FINISH
  wrs(" ender machine*n")
  wrs("The BCPL word is ")
  wrn(bitsperword)
  wrs(" bits long*n*n")
//wrs("XXX*n")
//abort(1000)
*/
  tester(0, 1, 2, v1, v2)

  RESULTIS 0
}

AND tester(x, y, z, v1, v2) BE
{ LET n0, n1, n2, n3, n4 = 0, 1, 2, 3, 4
  LET n5, n6, n7, n8, n9 = 5, 6, 7, 8, 9
  LET oct1775 = #1775

//wrs("AB*n")
//FINISH
wrs("*nBCPL compiler tester entered*n")

//  FIRST INITIALIZE CERTAIN VARIABLES

  f, g, h := 100, 101, 102
  testno, testcount, failcount := 0, 0, 0
  v, w := v1, v2

  FOR i = 0 TO 200 DO v!i, w!i := 1000+i, 10000+i

  quiet := FALSE

//  TEST SIMPLE VARIABLES AND EXPRESSIONS

  testno := 1

  t(a+b+c, 33)        // 1
  t(f+g+h, 303)
  t(x+y+z, 3)

  t(123+321-400, 44)  // 4
  t(x=0, TRUE)
  t(y=0, FALSE)
  t(!(@y+x), 1)
  t(!(@b+x), 11)
  t(!(@g+x), 101)

  testno := 20
  x, a, f := 5, 15, 105

  t(x, 5)            // 20
  t(a, 15)
  t(f, 105)

  v!1, v!2 := 1234, 5678
  t(v!1, 1234)       // 23
  t(v!z, 5678)                // Note z=2

  t(x*a, 75)         //  25
  t(1*x+2*y+3*z+f*4,433)

  t(x*a+a*x, 150)

  testno := 40

  t(100/(a-a+2), 50) //  40
  t(a/x, 3)
  t(a/-x, -3)
  t((-a)/x, -3)
  t((-a)/(-x), 3)
  t((a+a)/a, 2)
  t((a*x)/(x*a), 1)

  testno := 50
  t((a+b)*(x+y)*123/(6*123), 26)

  t(n7 REM 2, 1)     //  51
  t(f REM 100, 5)
  t(a REM x, 0)

  t(-f, -105)        //  54
  
  testno := 60

  f := 105
  t(f = 105, TRUE)   // 60
  t(f~= 105, FALSE)
  t(f < 105, FALSE)
  t(f>= 105, TRUE)
  t(f > 105, FALSE)
  t(f<= 105, TRUE)

  testno := 70

  f := 104
  t(f = 105, FALSE)  // 70
  t(f~= 105, TRUE)
  t(f < 105, TRUE)
  t(f>= 105, FALSE)
  t(f > 105, FALSE)
  t(f<= 105, TRUE)

  testno := 80

  f := 0
  t(f = 0, TRUE)    // 80
  t(f~= 0, FALSE)
  t(f < 0, FALSE)
  t(f>= 0, TRUE)
  t(f > 0, FALSE)
  t(f<= 0, TRUE)

  testno := 90

  f := 1
  t(f = 0, FALSE)   // 90
  t(f~= 0, TRUE)
  t(f < 0, FALSE)
  t(f>= 0, TRUE)
  t(f > 0, TRUE)
  t(f<= 0, FALSE)

  testno := 100

  t(oct1775<<3, #17750)  // 100
  t(oct1775>>3, #177)
  t(oct1775<<z+1, #17750)
  t(oct1775>>z+1, #177)

  { LET b1100 = #b1100
    LET b1010 = #b1010
    LET yes, no = TRUE, FALSE

    testno := 110

    t(b1100&#B1010, #B1000)    // 110
    t(b1100 | #B1010, #B1110)
    t((b1100 EQV   #B1010) & #B11111, #B11001)
    t(b1100 NEQV  #B1010, #B0110)
    t(b1100 XOR  #B1010, #B0110)

    t(NOT yes, no)         // 115
    t(NOT no, yes)
    t(NOT(b1100 EQV -b1010), b1100 NEQV -b1010)
    t(NOT(b1100 EQV -b1010), b1100 XOR  -b1010)
  }

  testno := 120
  f := 105
  t(-f, -105)               // 120

  t(!v, 1000)               // 121
  t(v!0, 1000)
  t(v!1, 1234)
  t(v!(!v-998), 5678)

  testno := 130

  t(!w, 10000)              // 130
  t(w!0, 10000)
  t(0!w, 10000)
  t(1!w, 10001)             // 133
  t(w!1, 10001)
  t(!(w+200), 10200)
  a := TRUE
  b := FALSE

  IF a DO x := 16
  t(x, 16)                  // 136
  x := 16

  IF b DO x := 15
  t(x, 16)                  // 137
  x := 15

  { LET w = VEC 20
    a := l1
    GOTO a
l2: wrs("GOTO ERROR*N")
    failcount := failcount+1
  }

l1:
  a := VALOF RESULTIS 11
  t(a, 11)                  // 138

  testno := 140  // TEST SIMULATED STACK ROUTINES

  { LET v1 = VEC 1
    v1!0, v1!1 := -1, -2
    { LET v2 = VEC 10
      FOR i = 0 TO 10 DO v2!i := -i
      t(v2!5, -5)           //  140
    }
    t(v1!1, -2)             //  141
  }

  x := x + t(x,15, t(f, 105), t(a, 11)) - 15   // 142-124
  t(x, 15)                                     // 145

  x := x+1
  t(x, 16)   // 146
  x := x-1
  t(x, 15)   // 147
  x := x+7
  t(x,22)    // 148
  x := x-22
  t(x, 0)    // 149
  x := x+15
  t(x, 15)   // 150
  x := x + f
  t(x, 120)  // 151
  x := 1

  testno := 160
  f := 105
  t(f = 105 -> 1, 2, 1)   // 160
  t(f~= 105 -> 1, 2, 2)
  t(f < 105 -> 1, 2, 2)
  t(f>= 105 -> 1, 2, 1)
  t(f > 105 -> 1, 2, 2)
  t(f<= 105 -> 1, 2, 1)

  testno := 170
  f := 104
  t(f = 105 -> 1, 2, 2)  // 170
  t(f~= 105 -> 1, 2, 1)
  t(f < 105 -> 1, 2, 1)
  t(f>= 105 -> 1, 2, 2)
  t(f > 105 -> 1, 2, 2)
  t(f<= 105 -> 1, 2, 1)

  f := 0
  testno := 180
  t(f = 0 -> 1, 2, 1)    // 180
  t(f~= 0 -> 1, 2, 2)
  t(f < 0 -> 1, 2, 2)
  t(f>= 0 -> 1, 2, 1)
  t(f > 0 -> 1, 2, 2)

  testno := 190
  t(f<= 0 -> 1, 2, 1)   // 190

  f := 1
  t(f = 0 -> 1, 2, 2)
  t(f~= 0 -> 1, 2, 1)
  t(f < 0 -> 1, 2, 2)
  t(f>= 0 -> 1, 2, 1)
  t(f > 0 -> 1, 2, 1)
  t(f<= 0 -> 1, 2, 2)

  { LET s1, s1f = 0, 0
    AND s2, s2f = 0, 0
    AND s3, s3f = 0, 0
    FOR i = -200 TO 200 DO
    //FOR i = -200 TO -200 DO
    { LET x = 7
//wrs("Test SWITCHON 1*n")
///*****
//wrs("i="); wrd(i, 4); nl()
//sys(4)
      SWITCHON i INTO
      { DEFAULT: s1 := s1+10; ENDCASE
      
        CASE -1000: s1f := s1f + i; ENDCASE
	
        CASE -200: s1 := s1 + 1
        CASE -190: s1 := s1 + 1
        CASE -180: s1 := s1 + 1
        CASE   -5: s1 := s1 + 1
        CASE    0: s1 := s1 + 1
        CASE -145: s1 := s1 + 1
        CASE    7: s1 := s1 + 1
        CASE    8: s1 := s1 + 1
	
        CASE  200: s1 := s1 + 1
        CASE  190: s1 := s1 + 1
        CASE  100: s1 := s1 + 1
        CASE   90: s1 := s1 + 1
        CASE  199: s1 := s1 + 1
        CASE   95: s1 := s1 + 1
        CASE   76: s1 := s1 + 1
        CASE   88: s1 := s1 + 1
	
        CASE   99: s1 := s1 + 1
        CASE  -98: s1 := s1 + 1
        CASE   11: s1 := s1 + 1
        CASE   12: s1 := s1 + 1
        CASE   13: s1 := s1 + 1
        CASE   41: s1 := s1 + 1
        CASE   91: s1 := s1 + 1
        CASE   92: s1 := s1 + 1
	
        CASE   71: s1 := s1 + 1
        CASE   73: s1 := s1 + 1
        CASE   74: s1 := s1 + 1
        CASE   81: s1 := s1 + 1
        CASE   82: s1 := s1 + 1
        CASE   61: s1 := s1 + 1
        CASE -171: s1 := s1 + 1
        CASE -162: s1 := s1 + 1
      }
//wrs("i="); wrd(i, 4); wrs("  s1="); wrd(s1, 5); wrs("  s1f="); wrn(s1f); nl()

//wrs("Test SWITCHON 2*n")
brkfn(#xDCBA)
      SWITCHON i+10000 INTO
      { DEFAULT: s2 := s2+10; ENDCASE
      
        CASE 10020: s2 := s2 + 1
        CASE 10021: s2 := s2 + 1
        CASE 10022: s2 := s2 + 1
        CASE 10023: s2 := s2 + 1
        CASE 10024: s2 := s2 + 1
        CASE 10025: s2 := s2 + 1
        CASE 10026: s2 := s2 + 1
        CASE 10027: s2 := s2 + 1
	
        CASE 10028: s2 := s2 + 1
        CASE 10029: s2 := s2 + 1
        CASE 10010: s2 := s2 + 1
        CASE 10011: s2 := s2 + 1
        CASE 10012: s2 := s2 + 1
        CASE 10013: s2 := s2 + 1
        CASE 10014: s2 := s2 + 1
        CASE 10015: s2 := s2 + 1
      }

//wrs("Test SWITCHON 3*n")
//wrn(i*10); nl()
testno := #x4567
      SWITCHON i*10 INTO
      { DEFAULT: s3 := s3+10; ENDCASE
      
        CASE -10000: s3f := s3f + 1; ENDCASE
	
        CASE -2000: s3 := s3 + 1
        CASE -1900: s3 := s3 + 1
        CASE -1800: s3 := s3 + 1
        CASE   -50: s3 := s3 + 1
        CASE    00: s3 := s3 + 1
        CASE -1450: s3 := s3 + 1
        CASE    70: s3 := s3 + 1
        CASE    80: s3 := s3 + 1
	
        CASE  2000: s3 := s3 + 1
        CASE  1900: s3 := s3 + 1
        CASE  1000: s3 := s3 + 1
        CASE   900: s3 := s3 + 1
        CASE  1990: s3 := s3 + 1
        CASE   950: s3 := s3 + 1
        CASE   760: s3 := s3 + 1
        CASE   880: s3 := s3 + 1
	
        CASE   990: s3 := s3 + 1
        CASE  -980: s3 := s3 + 1
        CASE   110: s3 := s3 + 1
        CASE   120: s3 := s3 + 1
        CASE   130: s3 := s3 + 1
        CASE   410: s3 := s3 + 1
        CASE   910: s3 := s3 + 1
        CASE   920: s3 := s3 + 1
	
        CASE   710: s3 := s3 + 1
        CASE   730: s3 := s3 + 1
        CASE   740: s3 := s3 + 1
        CASE   810: s3 := s3 + 1
        CASE   820: s3 := s3 + 1
        CASE   610: s3 := s3 + 1
        CASE -1710: s3 := s3 + 1
        CASE -1620: s3 := s3 + 1
      }

      //wrs("i**10="); wrd(i*10, 6)
      //wrs("  s3f="); wrn(s3f)
      //wrs("  s3="); wrn(s3); nl()
    }
//wrs("Test SWITCHON 4*n")
    testno := 200
   
    t(s1f, 0)                                       // 200
    t(s2f, 0)                                       // 201
    t(s3f, 0)                                       // 202
    t(s1, (401-32)*10 + 32* (32+1)/2)  //369528     // 203
    t(s2, (401-16)*10 + 16* (16+1)/2)  //385136     // 204
    t(s3, (401-32)*10 + 32* (32+1)/2)  //369528     // 205
    //FINISH
}

  testno := 250  // TEST FUNCTION CALLING

  t1(1,2,3,4,5,6, 21)
  t1(t(1,1), t(2,2), t(3,3), t(4,4), t(5,5), t(6,6),
     t(21,21))
  t1(VALOF RESULTIS 1,
     VALOF RESULTIS 2,
     VALOF RESULTIS 3,
     VALOF RESULTIS 4,
     VALOF RESULTIS 5,
     VALOF RESULTIS 6,
     21)
  t1(VALOF RESULTIS 1,
     t(2,2),
     VALOF RESULTIS 3,
     t(4,4),
     VALOF RESULTIS 5,
     t(6,6),
     21)
  t1( 1, t(2,2), VALOF RESULTIS 3,
      4, t(5,5), VALOF RESULTIS 6,
      21)
  t1(!v,v!0,v!200,!w,w!0,w!200, 2*1000+1200+2*10000+10200)
  (t1+(x+x)/x-2)(1,1,1,1,1,1,6)

  testno := 300  // TEST EXPRESSION OPERATORS

  f := 105
  t((2+3)+f+6,116)
  t(f+2+3+6,116)
  t(6+3+2+f, 116)
  t(f-104, 1)
  t((x+2)=(x+2)->99,98, 99)
  t(f<f+1->21,22, 21)
  t(f>f+1->31,32, 32)
  t(f<=105->41,42, 41)
  t(f>=105->51,52, 51)

  testno := 400  // TEST REGISTER ALLOCATION ETC.

  x := 0
  y := 1
  z := 2
  t(x, 0)
  t(y, 1)
  t(z, 2)
  f,g,h := 101,102,103
  a,b,c := 11,12,13
  t(x+1,1)
  t(f+1, 102)
  t(a+1, 12)
  t(!(@a*2/2+f-101),11)
  a := @f
  t(!a, 101)
  b := @g
  a := @b
  t(!!a, 102)
  w!0 := @w!1
  w!1 := @h
  t(z*y+(w!0)!0!0-2, 103)
  t(z*y+w!1!0-2, 103)
  t(t(123,123),t(123,123))

  testno := 500 // test 16 and 32  bit cintcode operands

  x := 100
  t(x*x, 10000)               // LH
  t(x*x*x*x, 100000000)       // LW
  t(x*x+10000, 20000)         // AH
  t(x*x+100000000, 100010000) // AW
  t(x*x-10000, 0)             // SH
  t(x*x-100000000, -99990000) // AW

  testno := 600

  locals(103,104,105,106,107,108,109,110,111,112,113,114,115,116,117)

  testno := 700

  a := 1
  b := msb
  c :=  allones
  t(a<<0, 1)
  t(a<<1, 2)
  t(a<<2, 4)
  t(a<<bitsperword-1, msb)
  t(a<<bitsperword,     0)
  t(a<<bitsperword+1,   0)

  t(a>>0, 1)
  t(b>>bitsperword-1, 1)
  t(c>>bitsperword-1, 1)
  t(b>>bitsperword,   0)
  t(c>>bitsperword,   0)

  testno := 800
  a, b, c := 20, -30, 0
  t(ABS a, 20)
  t(ABS b, 30)
  t(ABS c, 0)

  testno := 810

  v!0 := 1001
  t(v!0, 1001)

  v!1 := 1002
  t(v!1, 1002)

  v!2 := 1003
  t(v!2, 1003)

  v!3 := 1004
  t(v!3, 1004)

  v!4 := 1005
  t(v!4, 1005)
  w!0 := 2001
  t(w!0, 2001)
  w!1 := 2002
  t(w!1, 2002)
  w!2 := 2003
  t(w!2, 2003)
  w!3 := 2004
  t(w!3, 2004)
  w!4 := 2005
  t(w!4, 2005)

  testno := 850

  w%0 := 21
  t(w%0, 21)
  w%1 := 22
  t(w%1, 22)
  w%2 := 23
  t(w%2, 23)
  w%3 := 3
  t(w%3, 3) // compiles xpbyt instruction

  a := 10
  b := a<<5
  w%4 := a  // compiles a btc instruction
  t(w%4, 10)

  a, b, g := 100,101,300
  a := a+1
  t(a, 101)
  a := a+b
  t(a, 202)
  g := g+b
  t(g, 401)

  g := 8
  b := 3
  a := g REM b
  t(a, 2)

  g := 20
  b := 12
  a := g - b
  t(a, 8)

  testno := 900

  // Test Unicode character and string escapes
  // assuming the compiler has UTF8 as the default encoding.
  t('*#1234', #x1234)
  t("*#1234"%0, 3)                // 0001 0010 0011 0100
  t("*#1234"%1, #b1110_0001)      // 0001
  t("*#1234"%2, #b10_001000)      //      0010 00
  t("*#1234"%3, #b10_110100)      //             11 0100

  t('*##1234_5678', #x1234_5678)
  t("*##1234_5678"%0, 6)          // 0001 0010 0011 0100 0101 0110 0111 1000
  t("*##1234_5678"%1, #b1111110_0)//  0
  t("*##1234_5678"%2, #b10_010010)//   01 0010
  t("*##1234_5678"%3, #b10_001101)//           0011 01
  t("*##1234_5678"%4, #b10_000101)//                  00 0101
  t("*##1234_5678"%5, #b10_011001)//                          0110 01
  t("*##1234_5678"%6, #b10_111000)//                                 11 1000

  // Test GB2312 character and string escapes
  // assuming the compiler has UTF8 as the default encoding.
  t('*#g*#4566', 4566)
  t("*#g*#4566"%0, 2)     // row 45  col 66  = character 'foreign'
  t("*#g*#4566"%1, #xE2)  // #xE2 = 66 + 160
  t("*#g*#4566"%2, #xCD)  // #xCD = 45 + 160
  testno := 1000

  testno := 2000
  wrs("Testing switches*n")
  testno := #x33dc
  testswitches()
//GOTO done

  testno := 3100
  wrs("Testing static -1*n")
  t(12,12)
  t(minus1, -1)

  testno := 3100

  wrs("Testing escape commands*n")
  FOR i = 1 TO 10 DO tstescape(i)

  testno := 3200

  wrs("Testing LOOP commands in different environments*n")
  FOR i = 1 TO 10 DO tstloop(i)
 
  testno := 3300

  wrs("Testing BREAK commands in different environments*n")
  FOR i = 1 TO 10 DO tstbreak(i)
//GOTO done

  testno := 3400

  wrs("Testing ENDCASE commands in different environments*n")
  FOR i = 1 TO 10 DO tstendcase(i)

  testno := 3600

  wrs("Testing RETURN commands in different environments*n")
  FOR i = 1 TO 10 DO tstreturn(i)

  testno := 3800

  wrs("Testing RESULTIS commands in different environments*n")
  FOR i = 1 TO 10 DO tstresultis(i)

done:
  nl()
  wrn(testcount)
  wrs(" TESTS COMPLETED, ")
  wrn(failcount)
  wrs(" FAILURE(S)*N")
}

AND locals(p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17) BE
{ t(p3, 103)
  t(p4, 104)
  t(p5, 105)
  t(p6, 106)
  t(p7, 107)
  t(p8, 108)
  t(p9, 109)
  t(p10,110)
  t(p11,111)
  t(p12,112)
  t(p13,113)
  t(p14,114)
  t(p15,115)
  t(p16,116)
  t(p17,117)
}

AND testswitches() BE
{ 
  FOR i = -5 TO +5 DO
  { wrs("testswitches i=")
    wrn(i)
    nl()
    try(i)
    try (i-1_000)
    try (i-2_000)
    try (i+1_000)
    try (i+2_000)
    try (i+3_000)
    try (i+4_000)
    try (i+5_000)
    try (i+6_000)
    try (i+7_000)
    try (i+7_100)
    try (i+7_200)
    try (i+7_300)
    try (i+7_400)
    try (i+7_500)
    try (i+7_600)
    try (i+7_700)
    try (i+7_800)
    try (i+7_900)
  }
}

AND try(x) BE
{ LET a = swtest(x)
  LET b = answer(x)
//wrn(x, 10); wrc(' ')
//wrn(a, 10); wrc(' ')
//wrn(b, 10); wrc('*n')
  t(a, b)
  //IF testno=13411 DO a := 1/(a-a)
}

AND swtest(n) = VALOF SWITCHON n INTO
{ DEFAULT:     RESULTIS 123

  CASE -3:     RESULTIS 1
  CASE -2:     RESULTIS 2
  CASE -1:     RESULTIS 3
  CASE  0:     RESULTIS 4

  CASE  2:     RESULTIS 6
  CASE  3:     RESULTIS 7

  ///*
  CASE -1_000: RESULTIS 8

  CASE -2_000: RESULTIS 9
  CASE -2_001: RESULTIS 10

  CASE  1_000: RESULTIS 11
  CASE  1_001: RESULTIS 12
  CASE  1_002: RESULTIS 13

  CASE  2_000: RESULTIS 14
  CASE  2_001: RESULTIS 15
  CASE  2_002: RESULTIS 16
  CASE  2_003: RESULTIS 17
//*/
// The next five/ten tests should be commented out if using an
// old BCPL Cintcode compiler.

/*
  CASE minint+0:     RESULTIS 18
  CASE minint+1:     RESULTIS 19
  CASE minint+2:     RESULTIS 20
  CASE minint+3:     RESULTIS 21
  CASE minint+4:     RESULTIS 22

  CASE maxint-0:     RESULTIS 23
  CASE maxint-1:     RESULTIS 24
  CASE maxint-2:     RESULTIS 25
  CASE maxint-3:     RESULTIS 26
  CASE maxint-4:     RESULTIS 27
*/
///*
  CASE  3_000:   RESULTIS 28
  CASE  4_000:   RESULTIS 29
  CASE  5_000:   RESULTIS 30
  CASE  6_000:   RESULTIS 31

  CASE  7_000:   RESULTIS 32
  CASE  7_100:   RESULTIS 33
  CASE  7_200:   RESULTIS 34
  CASE  7_300:   RESULTIS 35
  CASE  7_400:   RESULTIS 36
  CASE  7_500:   RESULTIS 37
  CASE  7_600:   RESULTIS 38
  CASE  7_700:   RESULTIS 39
  CASE  7_800:   RESULTIS 40
  CASE  7_900:   RESULTIS 41
//*/
}

AND answer(n) = VALOF
{ IF n = -3           RESULTIS 1
  IF n = -2           RESULTIS 2
  IF n = -1           RESULTIS 3
  IF n =  0           RESULTIS 4

  IF n =  2           RESULTIS 6
  IF n =  3           RESULTIS 7

  IF n = -1_000   RESULTIS 8

  IF n = -2_000   RESULTIS 9
  IF n = -2_001   RESULTIS 10

  IF n =  1_000   RESULTIS 11
  IF n =  1_001   RESULTIS 12
  IF n =  1_002   RESULTIS 13

  IF n =  2_000   RESULTIS 14
  IF n =  2_001   RESULTIS 15
  IF n =  2_002   RESULTIS 16
  IF n =  2_003   RESULTIS 17

/*
  IF n = minint+0     RESULTIS 18
  IF n = minint+1     RESULTIS 19
  IF n = minint+2     RESULTIS 20
  IF n = minint+3     RESULTIS 21
  IF n = minint+4     RESULTIS 22

  IF n = maxint-0     RESULTIS 23
  IF n = maxint-1     RESULTIS 24
  IF n = maxint-2     RESULTIS 25
  IF n = maxint-3     RESULTIS 26
  IF n = maxint-4     RESULTIS 27
*/
  IF n =  3_000   RESULTIS 28
  IF n =  4_000   RESULTIS 29
  IF n =  5_000   RESULTIS 30
  IF n =  6_000   RESULTIS 31

  IF n =  7_000   RESULTIS 32
  IF n =  7_100   RESULTIS 33
  IF n =  7_200   RESULTIS 34
  IF n =  7_300   RESULTIS 35
  IF n =  7_400   RESULTIS 36
  IF n =  7_500   RESULTIS 37
  IF n =  7_600   RESULTIS 38
  IF n =  7_700   RESULTIS 39
  IF n =  7_800   RESULTIS 40
  IF n =  7_900   RESULTIS 41

  RESULTIS 123
}

AND tstescape(n) BE SWITCHON n INTO
{ CASE 1:
  { LET tst(t) BE SWITCHON t INTO
    { DEFAULT: w1 := 10; RETURN
      CASE 1:  w1 := 11; RETURN
      CASE 2:  w1 := 12; RETURN
    }
    //sawritef("Escape test a*n")
    w1 := 0; tst(1); t(w1, 11)
    w1 := 0; tst(2); t(w1, 12)
    w1 := 0; tst(3); t(w1, 10)
    RETURN
  }

  CASE 2:
  { LET tst(t) BE SWITCHON t INTO
    { CASE 1:  w1 := 11; RETURN
      CASE 2:  w1 := 12; RETURN
    }
    //sawritef("Escape test b*n")
    w1 := 0; tst(1); t(w1, 11)
    w1 := 0; tst(2); t(w1, 12)
    w1 := 0; tst(3); t(w1,  0)
    RETURN
  }
  
  CASE 3:
  { LET tst(t) BE SWITCHON t INTO
    { DEFAULT: w1 := 10; ENDCASE
      CASE 1:  w1 := 11; ENDCASE
      CASE 2:  w1 := 12; ENDCASE
    }
    //sawritef("Escape test c*n")
    w1 := 0; tst(1); t(w1, 11)
    w1 := 0; tst(2); t(w1, 12)
    w1 := 0; tst(3); t(w1, 10)
    RETURN
  }

  CASE 4:
  { LET tst(t) BE SWITCHON t INTO
    { CASE 1:  w1 := 11; ENDCASE
      CASE 2:  w1 := 12; ENDCASE
    }
    //sawritef("Escape test d*n")
    w1 := 0; tst(1); t(w1, 11)
    w1 := 0; tst(2); t(w1, 12)
    w1 := 0; tst(3); t(w1,  0)
    RETURN
  }

  CASE 5:
  { LET tst(t) BE SWITCHON t INTO
    { DEFAULT: w1 := 10; RETURN
      CASE 1:  w1 := 11; RETURN
      CASE 2:  w1 := 12; RETURN
    } REPEAT
    //sawritef("Escape test e*n")
    w1 := 0; tst(1); t(w1, 11)
    w1 := 0; tst(2); t(w1, 12)
    w1 := 0; tst(3); t(w1, 10)
    RETURN
  }

  CASE 6:
  { LET tst(t) BE SWITCHON t INTO
    { DEFAULT: w1 := w1+1;   IF w1<=300 LOOP; RETURN
      CASE 1:  w1 := w1+10;  IF w1<=300 LOOP; RETURN
      CASE 2:  w1 := w1+100; IF w1<=300 LOOP; RETURN
    } REPEAT
    //sawritef("Escape test f*n")
    w1 := 0; tst(1); t(w1, 310)
    w1 := 0; tst(2); t(w1, 400)
    w1 := 0; tst(3); t(w1, 301)
    RETURN
  }

  CASE 7:
  { LET tst(t) BE SWITCHON t INTO
    { DEFAULT: w1 := w1+1;   IF w1<=300 ENDCASE; RETURN
      CASE 1:  w1 := w1+10;  IF w1<=300 ENDCASE; RETURN
      CASE 2:  w1 := w1+100; IF w1<=300 ENDCASE; RETURN
    } REPEAT
    //sawritef("Escape test g*n")
    w1 := 0; tst(1); t(w1, 310)
    w1 := 0; tst(2); t(w1, 400)
    w1 := 0; tst(3); t(w1, 301)
    RETURN
  }

}

AND tstloop(n) BE SWITCHON n INTO
{ // Test LOOP on various contexts.
  DEFAULT: RETURN

  CASE 1:          // Simple LOOP tests
  { LET tst(x) BE 
    { w1:=0
      { w1 := w1+1
        IF w1>1 BREAK
	w1 := w1+10
	IF x=1 LOOP
	w1 := w1+100
	IF x=2 LOOP
	w1 := w1+1000
	LOOP
	w1 := -1
      } REPEAT
    }
     
    //writef("testloop: n=%n*n", n)
    tst(0); t(w1,1112)
    tst(1); t(w1,12)
    tst(2); t(w1,112)
    RETURN
  }
  
  CASE 2:          // Test LOOP escaping from SWITCHON and VALOF
  { LET tst(x, y) BE 
    { w1:=0
      { IF w1>1 BREAK
        SWITCHON y INTO
        { DEFAULT:
	    w1 := w1+1
	  CASE 10:
	    w1 := w1+10
	    IF x=1 LOOP
	  CASE 11:
	    w1 := w1+100
	    IF x=2 DO w1 := VALOF LOOP
	  CASE 12:
	    w1 := w1+1000
	    LOOP
	    w1 := w1+10000
	}
      } REPEAT
    }
     
    //writef("testloop: n=%n*n", n)
    tst(0,  0); t(w1,1111)
    tst(0, 10); t(w1,1110)
    tst(0, 11); t(w1,1100)
    tst(0, 12); t(w1,1000)
    tst(1,  0); t(w1,11)
    tst(1, 10); t(w1,10)
    tst(1, 11); t(w1,1100)
    tst(1, 12); t(w1,1000)
    tst(2,  0); t(w1,111)
    tst(2, 10); t(w1,110)
    tst(2, 11); t(w1,100)
    tst(2, 12); t(w1,1000)
    RETURN
  }
}

AND tstbreak(n) BE SWITCHON n INTO
{ // Test BREAK on various contexts.
  DEFAULT: RETURN

  CASE 1:          // Simple LOOP tests
  { LET tst(x) BE 
    { w1:=0
      { w1 := w1+1
        IF w1>1 BREAK
	w1 := w1+10
	IF x=1 BREAK
	w1 := w1+100
	IF x=2 BREAK
	w1 := w1+1000
	BREAK
	w1 := -1
      } REPEAT
    }
     
    //writef("testbreak: n=%n*n", n)
    tst(0); t(w1,1111)
    tst(1); t(w1,11)
    tst(2); t(w1,111)
    RETURN
  }
  
  CASE 2:          // Test BREAK escaping from SWITCHON and VALOF
  { LET tst(x, y) BE 
    { w1:=0
      { IF w1>1 BREAK
        SWITCHON y INTO
        { DEFAULT:
	    w1 := w1+1
	  CASE 10:
	    w1 := w1+10
	    IF x=1 BREAK
	  CASE 11:
	    w1 := w1+100
	    IF x=2 DO w1 := VALOF BREAK
	  CASE 12:
	    w1 := w1+1000
	    BREAK
	    w1 := w1+10000
	}
      } REPEAT
    }
     
    //writef("testbreak: n=%n*n", n)
    tst(0,  0); t(w1,1111)
    tst(0, 10); t(w1,1110)
    tst(0, 11); t(w1,1100)
    tst(0, 12); t(w1,1000)
    tst(1,  0); t(w1,11)
    tst(1, 10); t(w1,10)
    tst(1, 11); t(w1,1100)
    tst(1, 12); t(w1,1000)
    tst(2,  0); t(w1,111)
    tst(2, 10); t(w1,110)
    tst(2, 11); t(w1,100)
    tst(2, 12); t(w1,1000)
    RETURN
  }
}

AND tstendcase(n) BE SWITCHON n INTO
{ // Test ENDCASE on various contexts.
  DEFAULT: RETURN

  CASE 1:          // Test ENDCASE escaping from SWITCHON and VALOF
  { LET tst(x, y) = VALOF
    { w1:=0
      SWITCHON y INTO
      { DEFAULT:
          w1 := w1+1
        CASE 10:
          w1 := w1+10
          IF x=1 ENDCASE
        CASE 11:
          w1 := w1+100
          IF x=2 DO w1 := VALOF ENDCASE
        CASE 12:
          w1 := w1+1000
          ENDCASE
          w1 := w1+10000
      }
      RESULTIS w1
    }
     
    //writef("testendcase: n=%n*n", n)
    t(tst(0,  0),1111)
    t(tst(0, 10),1110)
    t(tst(0, 11),1100)
    t(tst(0, 12),1000)
    t(tst(1,  0),11)
    t(tst(1, 10),10)
    t(tst(1, 11),1100)
    t(tst(1, 12),1000)
    t(tst(2,  0),111)
    t(tst(2, 10),110)
    t(tst(2, 11),100)
    t(tst(2, 12),1000)
    RETURN
  }
  
  CASE 2:          // Test ENDCASE escaping from SWITCHON and VALOF
  { LET tst(x, y) BE
    { w1:=0
      SWITCHON y INTO
      { DEFAULT:
          w1 := w1+1
        CASE 10:
          w1 := w1+10
          IF x=1 ENDCASE
        CASE 11:
          w1 := w1+100
          IF x=2 DO w1 := VALOF ENDCASE
        CASE 12:
          w1 := w1+1000
          ENDCASE
          w1 := w1+10000
      }
    }
     
    //writef("testendcase: n=%n*n", n)
    tst(0,  0); t(w1,1111)
    tst(0, 10); t(w1,1110)
    tst(0, 11); t(w1,1100)
    tst(0, 12); t(w1,1000)
    tst(1,  0); t(w1,11)
    tst(1, 10); t(w1,10)
    tst(1, 11); t(w1,1100)
    tst(1, 12); t(w1,1000)
    tst(2,  0); t(w1,111)
    tst(2, 10); t(w1,110)
    tst(2, 11); t(w1,100)
    tst(2, 12); t(w1,1000)
    RETURN
  }
  
  CASE 3:          // Test ENDCASE escaping from SWITCHON and VALOF
  { LET tst(x, y) BE 
    { w1:=0
      { IF w1>1 BREAK
        SWITCHON y INTO
        { DEFAULT:
	    w1 := w1+1
	  CASE 10:
	    w1 := w1+10
	    IF x=1 ENDCASE
	  CASE 11:
	    w1 := w1+100
	    IF x=2 DO w1 := VALOF ENDCASE
	  CASE 12:
	    w1 := w1+1000
	    ENDCASE
	    w1 := w1+10000
	}
      } REPEAT
    }
     
    //writef("testendcase: n=%n*n", n)
    tst(0,  0); t(w1,1111)
    tst(0, 10); t(w1,1110)
    tst(0, 11); t(w1,1100)
    tst(0, 12); t(w1,1000)
    tst(1,  0); t(w1,11)
    tst(1, 10); t(w1,10)
    tst(1, 11); t(w1,1100)
    tst(1, 12); t(w1,1000)
    tst(2,  0); t(w1,111)
    tst(2, 10); t(w1,110)
    tst(2, 11); t(w1,100)
    tst(2, 12); t(w1,1000)
    RETURN
  }
}

AND tstreturn(n) BE SWITCHON n INTO
{ // Test LOOP on various contexts.
  DEFAULT: RETURN

  CASE 1:          // Simple RETURN tests
  { LET tst(x) BE 
    { w1:=0
      { w1 := w1+1
        IF w1>1 BREAK
	w1 := w1+10
	IF x=1 RETURN
	w1 := w1+100
	IF x=2 RETURN
	w1 := w1+1000
	RETURN
	w1 := -1
      } REPEAT
    }
     
    //writef("testreturn: n=%n*n", n)
    tst(0); t(w1,1111)
    tst(1); t(w1,11)
    tst(2); t(w1,111)
    RETURN
  }
  
  CASE 2:          // Test RETURN escaping from REPEAT, SWITCHON and VALOF
  { LET tst(x, y) BE 
    { w1:=0
      { IF w1>1 BREAK
        SWITCHON y INTO
        { DEFAULT:
	    w1 := w1+1
	  CASE 10:
	    w1 := w1+10
	    IF x=1 RETURN
	  CASE 11:
	    w1 := w1+100
	    IF x=2 DO w1 := VALOF RETURN
	  CASE 12:
	    w1 := w1+1000
	    RETURN
	    w1 := w1+10000
	}
      } REPEAT
    }
     
    //writef("testreturn: n=%n*n", n)
    tst(0,  0); t(w1,1111)
    tst(0, 10); t(w1,1110)
    tst(0, 11); t(w1,1100)
    tst(0, 12); t(w1,1000)
    tst(1,  0); t(w1,11)
    tst(1, 10); t(w1,10)
    tst(1, 11); t(w1,1100)
    tst(1, 12); t(w1,1000)
    tst(2,  0); t(w1,111)
    tst(2, 10); t(w1,110)
    tst(2, 11); t(w1,100)
    tst(2, 12); t(w1,1000)
    RETURN
  }
}

AND tstresultis(n) BE SWITCHON n INTO
{ // Test LOOP on various contexts.
  DEFAULT: RETURN

  CASE 1:          // Simple RESULTIS tests
  { LET tst(x) BE 
    { w1:=0
      { w1 := w1+1
        IF w1>1 BREAK
	w1 := w1+10
	IF x=1 LOOP
	w1 := w1+100
	IF x=2 LOOP
	w1 := w1+1000
	LOOP
	w1 := -1
      } REPEAT
    }
     
    //writef("testresultis: n=%n*n", n)
    tst(0); t(w1,1112)
    tst(1); t(w1,12)
    tst(2); t(w1,112)
    RETURN
  }
  
  CASE 2:          // Test RESULTIS escaping from SWITCHON and VALOF
  { LET tst(x, y) BE 
    { w1:=0
      w2 := 222
      w2 := VALOF
      { IF w1>1 BREAK
        SWITCHON y INTO
        { DEFAULT:
	    w1 := w1+1
	  CASE 10:
	    w1 := w1+10
	    IF x=1 RESULTIS 444
	  CASE 11:
	    w1 := w1+100
	    IF x=2 RESULTIS 555
	  CASE 12:
	    w1 := w1+1000
	    RESULTIS 666
	    w1 := w1+10000
	}
      } REPEAT
    }
     
    //writef("testresultis: n=%n*n", n)
    tst(0,  0); t(w1,1111); t(w2, 666)
    tst(0, 10); t(w1,1110); t(w2, 666)
    tst(0, 11); t(w1,1100); t(w2, 666)
    tst(0, 12); t(w1,1000); t(w2, 666)
    tst(1,  0); t(w1,11);   t(w2, 444)
    tst(1, 10); t(w1,10);   t(w2, 444)
    tst(1, 11); t(w1,1100); t(w2, 666)
    tst(1, 12); t(w1,1000); t(w2, 666)
    tst(2,  0); t(w1,111);  t(w2, 555)
    tst(2, 10); t(w1,110);  t(w2, 555)
    tst(2, 11); t(w1,100);  t(w2, 555)
    tst(2, 12); t(w1,1000); t(w2, 666)
    RETURN
  }
}

