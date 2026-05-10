/*
This program tests the pattern matching features of BCPL.
It also will test various sys operations such as memmovebytes
and memmovewords.

Implemented by Martin Richards (c) 6 Jun 2022

History

14/06/2023
Added a test for sys(Sys, dest, src, n)

1/10/2021
This program is currently under development and being used
to test the pattern matching features of the current BCPL
Cintcode system.

*/

SECTION "mcmpltest"

GET "libhdr"

GLOBAL {
f:ug; g; h
testno; failcount
testcount; quiet; t
bitsperword; msb; allones
on64 // TRUE if running on a 64-bit system 
spacev; spacep; spacet

g300:300; g301; g302; g303; g304; g305
}

MANIFEST{
k0=0; k1=1; k2=2
bugtestno=-1  //510
}

GLOBAL {
}


LET t(x, y) = VALOF
{ testcount := testcount + 1
  TEST on64
  THEN writef("%i4 %21i(%16x)    %21i(%16x) %s*n",
               testno, x, x, y, y, x=y -> "OK", "FAILED")
  ELSE writef("%i4 %13i(%08x)    %13i(%08x) %s*n",
               testno, x, x, y, y, x=y -> "OK", "FAILED")
  UNLESS x=y DO failcount := failcount + 1
  testno := testno + 1
  IF testno=bugtestno DO abort(1002)
  RESULTIS y
}

LET t1(a,b,c,d,e,f,g) = t(a+b+c+d+e+f, g)

LET start(parm) = VALOF
{ LET ww = 65

  bitsperword, msb, allones := 1, 1, 1
  
  UNTIL (msb<<1)=0 DO
    bitsperword, msb, allones := bitsperword+1, msb<<1, allones<<1 | 1
  on64 := bitsperword=64 // =TRUE if running on a 64-bit system

  writes("*nBCPL compiler tester for the MCPL extensions entered*n")
  writef("*nRunning on a %n bit %s ender system*n",
          bitsperword,
          (@ww)%0=65 -> "little", "big")

  tester(0, 1, 2)

  RESULTIS 0
}

AND tester(x, y, z) BE
{ 
  f, g, h := 100, 101, 102
  testno, testcount, failcount := 1, 0, 0

  quiet := FALSE

  FOR n = 1 TO 19 DO
  { testno := 100*n
    //writef("Before testno = %n n=%n*n", testno, n)
    //abort(8823)
    testpatterns(n)
  }

  testno := 2000
  testmemmove()
  
  writef("*n%n TESTS COMPLETED, %n FAILURE%-%pS*n", testcount, failcount)
}

AND testpatterns(n) BE SWITCHON n INTO
{ DEFAULT:
    RETURN

  CASE 1:
  { LET tst : x, y, z, a, b BE
    { t(x, 11)
      t(y, 22)
      //abort(6631)
      t(z, 33)
      t(a, 44)
      t(b, 55)
    }
    writef("%i4: Simple pattern variables*n", n)
    tst(11, 22, 33, 44, 55)
    FOR i = 0 TO 4 DO
      t( ( MATCH (i)
         : 0 => 100
         : 1 => 200
         : 2 => 300
         ),
	 i!TABLE 100,200,300,0,0)
    RETURN
  }

  CASE 2:
  { LET u, v, w = 301, 302, 303
    LET r, s, t1 = 201, @u,  203
    LET x, y, z, a, b, c = 101, 102, 103, @r, 105, 106
    LET p = @x

    // p -> [101, 102, 103, [201, [301,302,303], 203], 105, 106]
    
    LET tst : [a1, a2, a3, [a41, [a421,a422,a423], a43], a5, a6] BE
    { t(a1,   101)
      t(a2,   102)
      t(a3,   103)
      t(a41,  201)
      t(a421, 301)
      t(a422, 302)
      t(a423, 303)
      t(a43,  203)
      t(a5,   105)
      t(a6,   106)
    }

    LET tst1 : [a1, a2, a3, [a41, [a421,a422,a423], a43], a5, a6] BE
    { t(a1,   801)
      t(a2,   802)
      t(a3,   803)
      t(a41,  804)
      t(a421, 805)
      t(a422, 806)
      t(a423, 807)
      t(a43,  808)
      t(a5,   809)
      t(a6,   810)
    }

    LET upd : [a1, a2, a3, [a41, [a421,a422,a423], a43], a5, a6] BE
    { a1   := 801
      a2   := 802
      a3   := 803
      a41  := 804
      a421 := 805
      a422 := 806
      a423 := 807
      a43  := 808
      a5   := 809
      a6   := 810
    }
    writef("%i4: Pattern indirect variables*n", n)
    tst(p)
    upd(p) 
    tst1(p)
    RETURN
  }

  CASE 3:
  { LET tst(x) = MATCH(x)
    :  1 => 100
    :  2 => 200
    :  3 => 300
    :  4 => 400
    :  5 => 500
    :  a => 600    // This will always succeed

    writef("%i4: Simple MATCH commands*n", n)
    t(tst(0), 600)
    t(tst(1), 100)
    t(tst(2), 200)
    t(tst(3), 300)
    t(tst(4), 400)
    t(tst(5), 500)
    t(tst(6), 600)
    t(tst(7), 600)
    RETURN
  }

  CASE 4:
  { LET tst(x, y, z) = MATCH (x, y, z, 123)
    :  1 => 100   // If all match item fail the result is zero.
    :  2 => 200
    :  3 => 300
    :  4 => 400
    :  5 => 500
    :  6 => 600

    writef("%i4: Simple MATCH expressions*n", n)
    t(tst(0),   0)
    t(tst(1), 100)
    t(tst(2), 200)
    t(tst(3), 300)
    t(tst(4), 400)
    t(tst(5), 500)
    t(tst(6), 600)
    t(tst(7),   0)
    RETURN
  }
  
  CASE 5:
  { LET tst(x, y, z) = EVERY (x, y, z, 123)
    :  <1 =>  100   // If all match item fail the result is zero.
    :  <2 =>  200
    :  <3 =>  300
    :  <4 =>  400
    :  <5 =>  500
    :   a =>  600

    writef("%i4: Simple EVERY expressions*n", n)
    t(tst(0), 2100)
    t(tst(1), 2000)
    t(tst(2), 1800)
    t(tst(3), 1500)
    t(tst(4), 1100)
    t(tst(5),  600)
    t(tst(6),  600)
    t(tst(7),  600)
    RETURN
  }

  CASE 6:
  { LET tst(x, y, z) = EVERY (x, y, z, 123)
    :  <1 => 100   // If all match item fail the result is zero.
    :  <2 => 200
    :  <3 => 300
    :  <4 => 400
    :  <5 => 500
    :  <6 => 600

    writef("%i4: More EVERY expressions*n", n)
    t(tst(0), 2100)
    t(tst(1), 2000)
    t(tst(2), 1800)
    t(tst(3), 1500)
    t(tst(4), 1100)
    t(tst(5),  600)
    t(tst(6),    0)
    t(tst(7),    0)
    RETURN
  }

  CASE 7:
  { LET tst(x, y, z) = MATCH(x, y, z, 123)
      :  k1     => 10
      :  'A'    => 20
      :  TRUE   => 30
      :  FALSE  => 40
      :  -5     => 50
      :  +6     => 60
      :   7     => 0

    writef("%i4: Simple constants*n", n)
    t(tst(0),     40)
    t(tst(k1),    10)
    t(tst('A'),   20)
    t(tst(TRUE),  30)
    t(tst(FALSE), 40)
    t(tst(-5),    50)
    t(tst(6),     60)
    t(tst(7),      0)
    RETURN
  }

  CASE 8:
  { LET tst(x, y, z) = VALOF
    { RESULTIS MATCH(x, y, z,123)
      :   1.25       => 10
      :  -1.25       => 20
      :  +1.25       => 30
      : =(150/100.0) => 40
    }

    writef("%i4: Floating point bpat terms*n", n)
    t(tst( 0),        0)
    t(tst( 1.25),    10)
    t(tst(-1.25),    20)
    t(tst(+1.25),    10)
    t(tst( 1.50),    40)
    RETURN
  }

  CASE 9:
  { LET tst(x, y, z) = VALOF
    { LET a, b = 110, 120
      RESULTIS MATCH(x, y, z, 123)
      :   1..10      => 1
      :  50.0#..75.0 => 2    // The # should be optional
      :  76..100     => 3
      : a..b         => 4
    }

    writef("%i4: Integer and floating point ranges*n", n)
    t(tst(    0),   0)
    t(tst(    1),   1)
    t(tst(    2),   1)
    t(tst(    9),   1)
    t(tst(   10),   1)
    t(tst(   11),   0)
    t(tst( 65.0),   2)
    t(tst(  100),   3)
    t(tst(  101),   0)
    RETURN
  }

  CASE 10:
  { LET tst(x, y, z) = VALOF
    { LET a, b = 110, 120
      RESULTIS MATCH(x, y, z, 123)
      :   1..10      => 1
      :  50.0#..75.0 => 2    // The # should be optional
      :  76..100     => 3
      : a..b         => 4
    }

    writef("%i4: Integer and floating point ranges*n", n)
    t(tst(    0),   0)
    t(tst(    1),   1)
    t(tst(    2),   1)
    t(tst(    9),   1)
    t(tst(   10),   1)
    t(tst(   11),   0)
    t(tst( 65.0),   2)
    t(tst(  100),   3)
    t(tst(  101),   0)

                          // Variable ranges
    t(tst(  109),   0)
    t(tst(  110),   4)
    t(tst(  115),   4)
    t(tst(  120),   4)
    t(tst(  121),   0)
    RETURN
  }

  CASE 11:
  { LET tst(x) = VALOF
    { RESULTIS MATCH(x)
      :   1|2|5|6|20..30|45 => 1
      :  100.0|200.0        => 2
    }

    writef("%i4: Integer and Floating alternations*n", n)

    t(tst(    0),   0)
    t(tst(    1),   1)
    t(tst(    2),   1)
    t(tst(    3),   0)
    t(tst(    4),   0)
    t(tst(    5),   1)
    t(tst(    6),   1)
    t(tst(    7),   0)
    t(tst(    8),   0)
    t(tst(    9),   0)
  
    t(tst(   20),   1)
    t(tst(   30),   1)
    t(tst(   45),   1)

    t(tst(  100.0), 2)
    t(tst(  200.0), 2)
    RETURN
  }

  CASE 12:
  { LET tsteq(a, b) = MATCH(a, b) : =y,  y => -1
    AND tstne(a, b) = MATCH(a, b) : ~=y, y => -1
    AND tstls(a, b) = MATCH(a, b) : <y,  y => -1
    AND tstgr(a, b) = MATCH(a, b) : >y,  y => -1
    AND tstle(a, b) = MATCH(a, b) : <=y, y => -1
    AND tstge(a, b) = MATCH(a, b) : >=y, y => -1

    writef("%i4: Integer relations*n", n)

    t(tsteq(13, 13), -1)
    t(tsteq(12, 13),  0)
    t(tsteq(14, 13),  0)

    t(tstne(13, 13),  0)
    t(tstne(12, 13), -1)
    t(tstne(14, 13), -1)

    t(tstls(13, 13),  0)
    t(tstls(12, 13), -1)
    t(tstls(14, 13),  0)

    t(tstgr(13, 13),  0)
    t(tstgr(12, 13),  0)
    t(tstgr(14, 13), -1)

    t(tstle(13, 13), -1)
    t(tstle(12, 13), -1)
    t(tstle(14, 13),  0)

    t(tstge(13, 13), -1)
    t(tstge(12, 13),  0)
    t(tstge(14, 13), -1)
    RETURN
  }

  CASE 13:
  { LET tstfeq(a, b) = MATCH(0.5*FLOAT a, 0.5*FLOAT b) : #=y,  y  => -1
    AND tstfne(a, b) = MATCH(0.5*FLOAT a, 0.5*FLOAT b) : #~=y, y  => -1
    AND tstfls(a, b) = MATCH(0.5*FLOAT a, 0.5*FLOAT b) : #<y,  y  => -1
    AND tstfgr(a, b) = MATCH(0.5*FLOAT a, 0.5*FLOAT b) : #>y,  y  => -1
    AND tstfle(a, b) = MATCH(0.5*FLOAT a, 0.5*FLOAT b) : #<=y, y  => -1
    AND tstfge(a, b) = MATCH(0.5*FLOAT a, 0.5*FLOAT b) : #>=y, y  => -1

    writef("%i4: Floating point relations*n", n)

    t(tstfeq(13, 13), -1)
    t(tstfeq(12, 13),  0)
    t(tstfeq(14, 13),  0)

    t(tstfne(13, 13),  0)
    t(tstfne(12, 13), -1)
    t(tstfne(14, 13), -1)

    t(tstfls(13, 13),  0)
    t(tstfls(12, 13), -1)
    t(tstfls(14, 13),  0)

    t(tstfgr(13, 13),  0)
    t(tstfgr(12, 13),  0)
    t(tstfgr(14, 13), -1)

    t(tstfle(13, 13), -1)
    t(tstfle(12, 13), -1)
    t(tstfle(14, 13),  0)

    t(tstfge(13, 13), -1)
    t(tstfge(12, 13),  0)
    t(tstfge(14, 13), -1)
    RETURN
  }

  CASE 14:
    writef("%i4: Simple match expressions*n", n)

    t( ( MATCH (0)
         : 0 => 100
         : 1 => 200
         : 2 => 300
       ), 100)

    t( ( MATCH (1)
         : 0 => 100
         : 1 => 200
         : 2 => 300
       ), 200)

    t( ( MATCH (2)
         : 0 => 100
         : 1 => 200
         : 2 => 300
       ), 300)

    t( ( MATCH (3)
         : 0 => 100
         : 1 => 200
         : 2 => 300
       ), 0)
    RETURN

  CASE 15:
    writef("%i4: Match expressions involving NEXT*n", n)
    t( ( MATCH (1)
         : 0 => 100
         : 1, NEXT => 200
         : 1 => 300
       ), 300)
     
    t( ( MATCH (1)
         : 0 => 100
         : 1 => 200+NEXT*2
         : x => 300
       ), 300)

    t( ( MATCH (1)
         : 0 => 100
         : x => VALOF { WHILE x < 100 TEST x>10
                                      THEN NEXT
	  			      ELSE x := x+x
                        RESULTIS x
                      }
         : y => y
       ), 16)
     
    t( ( MATCH (1)
         : 0 => 100
         : 1 => 200+NEXT*2
         : x => 300
       ), 300)

    FOR i = 0 TO 4 DO
      t( ( MATCH (i)
           : 0 => 100
           : 1 => 200+NEXT*2
           : 2 => 300
           : x => x
         ), i!TABLE 100, 1, 300, 3, 4)
    RETURN

  CASE 16:
    writef("%i4: Match expressions involving EXIT*n", n)
    t( ( MATCH (0)
         : 0 => 100
         : 1 => 200+EXIT*2
         : x => 300
       ), 100)

    t( ( MATCH (0)
         : 0   => 100
         : x 1 => x -> EXIT, 200
         : x   => 300
       ), 100)

    t( ( MATCH (0)
         : 0 => 100
         : x 1 => x -> 200, EXIT
         : x => 300
       ), 100)
       
    t( ( MATCH (1)
         : 0 => 100
         : 1 => 200+EXIT*2
         : x => 300
       ), 0)

    t( ( MATCH (1)
         : 0   => 100
         : x 1 => x -> EXIT, 200
         : x   => 300
       ), 0)

    t( ( MATCH (1)
         : 0 => 100
         : x 1 => x -> 200, EXIT
         : x => 300
       ), 200)
       
    t( ( MATCH (2)
         : 0 => 100
         : 1 => 200+EXIT*2
         : x => 300
       ), 300)

    t( ( MATCH (2)
         : 0   => 100
         : x 1 => x -> EXIT, 200
         : x   => 300
       ), 300)

    t( ( MATCH (2)
         : 0 => 100
         : x 1 => x -> 200, EXIT
         : x => 300
       ), 300)
    RETURN

  CASE 17:
    writef("%i4: Match expressions involving NEXT and EXIT*n", n)
  FOR i = 1 TO 4 DO t( ( EVERY (i)
       : >0 => 10
       : >1 => 20
       : >2 => 30
     ), i!TABLE 0, 10, 30, 60, 60)
     
  FOR i = 0 TO 4 DO t( ( EVERY (i)
       : >0 => 10
       : >1 => NEXT
       : >2 => 30
     ), i!TABLE 0, 10, 10, 40, 40)

  FOR i = 0 TO 4 DO t( ( EVERY (i)
       : >0 => 10
       : >1 => EXIT
       : >2 => 30
     ), i!TABLE 0, 10, 10, 10, 10)
  RETURN
  
  CASE 18:
  { LET w = 0
    writef("%i4: Match expressions involving LOOP and BREAK*n", n)
    w := 0
    
    FOR i = 0 TO 10 DO
    { LET j = 0
      w := i
      WHILE j<8 DO
      { w := w+1+( MATCH (i)
                   : 0 => 10
                   : 1 => 20
                   : 2 => 30//+LOOP
                   : 3 => 40
                   : 4 => 50+BREAK
                   : 5 => 60
                 )
        j := j+1
      }
      t(w, i!TABLE 88,169,250,331,4,493,14,15,16,17,18)
    }
    RETURN
  }

  writef("End of tests*n")
}

AND testmemmove() BE
{ LET v  = VEC 255
  LET bv = v<<W2Bsh
  
  writef("Testing Sys_memmovebytes*n")
 // writef("v=%n bv=%n bv+10=%n bv+9=%n W2Bsh=%n*n", v, bv, bv+10, bv+9, W2Bsh)

  FOR i = 0 TO 255 DO v%i := i
  //writef("Bytes: "); FOR i = 0 TO 15 DO writef(" %2i", v%i)
  //newline()
  //writef("Calling sys(Sys_memmovebytes, dest=%n, srv=%n, n=%n)*n",
  //        bv+10, bv+9, 5)
  sys(Sys_memmovebytes, bv+10, bv+9, 5)
  //writef("Bytes: "); FOR i = 0 TO 15 DO writef(" %2i", v%i)
  //newline()
  t(v%9, 9); t(v%10, 9); t(v%14, 13); t(v%15, 15)
  //abort(1001)

  FOR i = 0 TO 255 DO v%i := i
  //writef("Bytes: "); FOR i = 0 TO 15 DO writef(" %2i", v%i)
  //newline()
  //writef("Calling sys(Sys_memmovebytes, %n, %n, %n)*n", bv+10, bv+11, 5)
  sys(Sys_memmovebytes, bv+10, bv+11, 5)
  //writef("Bytes: "); FOR i = 0 TO 15 DO writef(" %2i", v%i)
  //newline()
  t(v%9, 9); t(v%10, 11); t(v%14, 15); t(v%15, 15)
  //abort(1002)


  writef("Testing Sys_memmovewords*n")
  //writef("v=%n v+10=%n v+9=%n*n", v, v+10, v+9)

  FOR i = 0 TO 255 DO v!i := i
  //writef("Words: "); FOR i = 0 TO 15 DO writef(" %2i", v!i)
  //newline()
  //writef("Calling sys(Sys_memmovewords, %n, %n, %n)*n", v+10, v+9, 5)
  sys(Sys_memmovewords, v+10, v+9, 5)
  //writef("Words: "); FOR i = 0 TO 15 DO writef(" %2i", v!i)
  //newline()
  t(v!9, 9); t(v!10, 9); t(v!14, 13); t(v!15, 15)
  //abort(1003)

  FOR i = 0 TO 255 DO v!i := i
  //writef("Words: "); FOR i = 0 TO 15 DO writef(" %2i", v!i)
  //newline()
  //writef("Calling sys(Sys_memmovewords, %n, %n, %n)*n", v+10, v+11, 5)
  sys(Sys_memmovewords, v+10, v+11, 5)
  //writef("Words: "); FOR i = 0 TO 15 DO writef(" %2i", v!i)
  //newline()
  t(v!9, 9); t(v!10, 11); t(v!14, 15); t(v!15, 15)
  //abort(1004)
}
