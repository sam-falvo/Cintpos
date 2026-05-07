GET "mcpl.h"

// This is a systematic compiler test for MCPL based on
// a similar one for the BCPL compiler.

// Copyright: Martin Richards May 1997

// The ONLY free variable of this program is: sys  (or wrch)

GLOBAL   f:200, g:401, h:602,
         testno:203, failcount,
         v, testcount, quiet, t,
         bitsperword, msb:211, allones

STATIC   a=10, b=11, c=12, w=0 

MANIFEST K0=0, K1=1, K2=2 

FUN wrc : ch => sys(11,ch)   //wrch(ch)

FUN wrs : s => WHILE %s DO wrc(%s++)

FUN nl : => wrc '\n' 

FUN wrd : n, d =>
  LET t = VEC 10
  LET i=0, k=-n
  IF n<0 DO d, k := d-1, n
  t!i, i, k := -(k MOD 10), i+1, k/10 REPEATUNTIL k=0
  FOR j = i+1 TO d DO wrc '\s'
  IF n<0 DO wrc '-'
  FOR j = i-1 TO 0 BY -1 DO wrc(t!j+'0')


FUN wrn : n => wrd(n, 0)

FUN wrx : n, d =>
  IF d>1 DO wrx(n>>4, d-1)
  wrc("0123456789ABCDEF"%(n&15))


FUN t : x, y =>
  testcount++
  wrd(testno, 3)
  wrc ' '
  wrd(x, 11)
  wrc ' '
  TEST x=y THEN wrs "OK"
           ELSE { wrs("FAILED, it should be ")
                  wrd(y, 11)
                  failcount++
                }
  nl()
  testno++
  RETURN y


FUN t1 : a,b,c,d,e,f,g => t(a+b+c+d+e+f, g)

FUN start : =>
  LET v1 = VEC 200
  LET v2 = VEC 200
  wrs "\nCgtester on a "
  bitsperword, msb, allones := 1, 1, 1

  WHILE msb<<1 DO 
    bitsperword, msb, allones := bitsperword+1, msb<<1, allones<<1 | 1

  wrd(bitsperword, 0)
  wrs " bits implementation\n\n"
    
  tester(0, 1, 2, v1, v2)
    
  RETURN 0


FUN tester : x, y, z, v1, v2 =>
  LET n0=0, n1=1, n2=2, n3=3, n4=4
  LET n5=5, n6=6, n7=7, n8=8, n9=9
  LET oct1775 = #1775

  wrs "\nCgtester entered\n"

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

  x, a, f := 5, 15, 105
  t(x, 5)            // 10
  t(a, 15)
  t(f, 105)

  v!1, v!2 := 1234, 5678
  t(v!1, 1234)       // 13
  t(v!z, 5678)

  t(x*a, 75)         //  15
  t(1*x+2*y+3*z+f*4,433)
  t(x*a+a*x, 150)

  t(100/(a-a+2), 50) //  18
  t(a/x, 3)
  t(a/-x, -3)
  t((-a)/x, -3)
  t((-a)/(-x), 3)
  t((a+a)/a, 2)
  t((a*x)/(x*a), 1)
  t((a+b)*(x+y)*123/(6*123), 26)

  t(n7 MOD 2, 1)      //  26
  t(f MOD 100, 5)
  t(a MOD x, 0)

  t(-f, -105)       //  29

  f := 105
  t(f = 105, TRUE)   // 30
  t(f~= 105, FALSE)
  t(f < 105, FALSE)
  t(f>= 105, TRUE)
  t(f > 105, FALSE)
  t(f<= 105, TRUE)

  f := 104
  t(f = 105, FALSE)  // 36
  t(f~= 105, TRUE)
  t(f < 105, TRUE)
  t(f>= 105, FALSE)
  t(f > 105, FALSE)
  t(f<= 105, TRUE)

  f := 0
  t(f = 0, TRUE)    // 42
  t(f~= 0, FALSE)
  t(f < 0, FALSE)
  t(f>= 0, TRUE)
  t(f > 0, FALSE)
  t(f<= 0, TRUE)

  f := 1
  t(f = 0, FALSE)   // 48
  t(f~= 0, TRUE)
  t(f < 0, FALSE)
  t(f>= 0, TRUE)
  t(f > 0, TRUE)
  t(f<= 0, FALSE)

  testno := 60

  t(oct1775<<3, #17750)  // 60
  t(oct1775>>3, #177)
  t(oct1775<<(z+1), #17750)
  t(oct1775>>(z+1), #177)

  LET b1100 = #b1100
  LET b1010 = #b1010
  LET yes=TRUE, no=FALSE

  testno := 70

  t(b1100&#B1010, #B1000)    //  70
  t(b1100 | #B1010, #B1110)
  t(b1100 XOR  #B1010, #B0110)

  t(NOT yes, no)         // 73
  t(NOT no, yes)
  
  testno := 80
  f := 105
  t(-f, -105)               // 80

  t(!v, 1000)               // 81
  t(v!0, 1000)
  t(v!1, 1234)
  t(v!(!v-998), 5678)

  testno := 90

  t(!w, 10000)              // 90
  t(w!0, 10000)
  t(w!1, 10001)

  a := TRUE
  b := FALSE

  IF a DO x := 16
  t(x, 16)                  // 93
  x := 16

  IF b DO x := 15
  t(x, 16)                  // 94
  x := 15

  testno := 100  // TEST SIMULATED STACK ROUTINES

  LET va = VEC 1
  va!0, va!1 := -1, -2
  LET vb = VEC 10
  FOR i = 0 TO 10 DO vb!i := -i
  t(vb!5, -5)        //  101
  t(va!1, -2)        //  102

  a := 11
  x := x + t(x,15, t(f, 105), t(a, 11)) - 15   // 103-105
  t(x, 15)                                     // 106

  x := x+1
  t(x, 16)   // 107
  x := x-1
  t(x, 15)   // 108
  x := x+7
  t(x,22)    // 109
  x := x-22
  t(x, 0)    // 110
  x := x+15
  t(x, 15)   // 111
  x := x + f
  t(x, 120)  // 112
  x := 1

  testno := 130
  f := 105
  t(f = 105 -> 1, 2, 1)   // 130
  t(f~= 105 -> 1, 2, 2)
  t(f < 105 -> 1, 2, 2)
  t(f>= 105 -> 1, 2, 1)
  t(f > 105 -> 1, 2, 2)
  t(f<= 105 -> 1, 2, 1)

  f := 104
  t(f = 105 -> 1, 2, 2)  // 136
  t(f~= 105 -> 1, 2, 1)
  t(f < 105 -> 1, 2, 1)
  t(f>= 105 -> 1, 2, 2)
  t(f > 105 -> 1, 2, 2)
  t(f<= 105 -> 1, 2, 1)

  f := 0
  t(f = 0 -> 1, 2, 1)    // 142
  t(f~= 0 -> 1, 2, 2)
  t(f < 0 -> 1, 2, 2)
  t(f>= 0 -> 1, 2, 1)
  t(f > 0 -> 1, 2, 2)
  t(f<= 0 -> 1, 2, 1)

  f := 1
  t(f = 0 -> 1, 2, 2)   // 148
  t(f~= 0 -> 1, 2, 1)
  t(f < 0 -> 1, 2, 2)
  t(f>= 0 -> 1, 2, 1)
  t(f > 0 -> 1, 2, 1)
  t(f<= 0 -> 1, 2, 2)

  testno := 200  // TEST SWITCHON COMMANDS

  LET s1=0, s1f=0
  LET s2=0, s2f=0
  LET s3=0, s3f=0
  FOR i = -200 TO 200 DO
  { LET x = 7
    MATCH i
    :-1000 => s1f +:= i
    : -200 => s1++
    : -190 => s1++
    : -180 => s1++
    :   -5 => s1++
    :    0 => s1++
    : -145 => s1++
    :    7 => s1++
    :    8 => s1++
    :  200 => s1++
    :  190 => s1++
    :  100 => s1++
    :   90 => s1++
    :  199 => s1++
    :   95 => s1++
    :   76 => s1++
    :   88 => s1++
    :   99 => s1++
    :  -98 => s1++
    :   11 => s1++
    :   12 => s1++
    :   13 => s1++
    :   41 => s1++
    :   91 => s1++
    :   92 => s1++
    :   71 => s1++
    :   73 => s1++
    :   74 => s1++
    :   81 => s1++
    :   82 => s1++
    :   61 => s1++
    : -171 => s1++
    : -162 => s1++
    :      => s1 +:= 1000
    .

    MATCH (i+10000)
    : 10020 => s2++
    : 10021 => s2++
    : 10022 => s2++
    : 10023 => s2++
    : 10024 => s2++
    : 10025 => s2++
    : 10026 => s2++
    : 10027 => s2++
    : 10028 => s2++
    : 10029 => s2++
    : 10010 => s2++
    : 10011 => s2++
    : 10012 => s2++
    : 10013 => s2++
    : 10014 => s2++
    : 10015 => s2++
    :       => s2 +:= 1000
    .

    MATCH (i*100)
    :-100000 => s3f +:= i
    : -20000 => s3++
    : -19000 => s3++
    : -18000 => s3++
    :   -500 => s3++
    :    000 => s3++
    : -14500 => s3++
    :    700 => s3++
    :    800 => s3++
    :  20000 => s3++
    :  19000 => s3++
    :  10000 => s3++
    :   9000 => s3++
    :  19900 => s3++
    :   9500 => s3++
    :   7600 => s3++
    :   8800 => s3++
    :   9900 => s3++
    :  -9800 => s3++
    :   1100 => s3++
    :   1200 => s3++
    :   1300 => s3++
    :   4100 => s3++
    :   9100 => s3++
    :   9200 => s3++
    :   7100 => s3++
    :   7300 => s3++
    :   7400 => s3++
    :   8100 => s3++
    :   8200 => s3++
    :   6100 => s3++
    : -17100 => s3++
    : -16200 => s3++
    :        => s3 +:= 1000
    .
  }
  t(s1f, 0)                               // 200
  t(s2f, 0)                               // 201
  t(s3f, 0)                               // 202
  t(s1, (401-32)*1000 + 32)  //369032     // 203
  t(s2, (401-16)*1000 + 16)  //385016     // 204
  t(s3, (401-32)*1000 + 32)  //369032     // 205


  testno := 250  // TEST FUNCTION CALLING

  t1(1,2,3,4,5,6, 21)
  t1(t(1,1), t(2,2), t(3,3), t(4,4), t(5,5), t(6,6),
         t(21,21))
  t1(VALOF RESULT 1,
     VALOF RESULT 2,
     VALOF RESULT 3,
     VALOF RESULT 4,
     VALOF RESULT 5,
     VALOF RESULT 6,
     21)
  t1(VALOF RESULT 1,
     t(2,2),
     VALOF RESULT 3,
     t(4,4),
     VALOF RESULT 5,
     t(6,6),
     21)
  t1( 1, t(2,2), VALOF RESULT 3,
      4, t(5,5), VALOF RESULT 6,
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
  t(a<<(bitsperword-1), msb)
  t(a<<bitsperword,       0)
  t(a<<(bitsperword+1),   0)

  t(a>>0, 1)
  t(b>>(bitsperword-1), 1)
  t(c>>(bitsperword-1), 1)
  t(b>>bitsperword,     0)
  t(c>>bitsperword,     0)

  testno := 800
  a, b, c := 20, -30, 0
  t(ABS a, 20)
  t(ABS b, 30)
  t(ABS c, 0)

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
  a := g MOD b
  t(a, 2)

  g := 20
  b := 12
  a := g - b
  t(a, 8)

  testno := 850

  nl()
  wrn testcount
  wrs " tests completed, "
  wrn failcount
  wrs " failure(s)\n"



FUN locals : p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17 =>
  t(p3, 103)
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
