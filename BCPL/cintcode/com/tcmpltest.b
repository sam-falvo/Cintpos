/*

This program tests the escape commands BREAK, LOOP, ENDCASE, NEXT and
EXIT in many contol environments.

Implemented by Martin Richards (c) 69 Aug 2022

History

09/08/2022
Initial implementation.

*/

SECTION "tcmpltest"

GET "libhdr"

GLOBAL {
f:ug; g; h
testno; failcount
testcount; quiet; t
bugtestno // If testno equals this t will call abort(1002)
bitsperword; msb; allones
on64 // TRUE if running on a 64-bit system 
spacev; spacep; spacet

g300:300; g301; g302; g303; g304; g305
w:350
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

LET start(parm) = VALOF
{ LET ww = 65

 bitsperword, msb, allones := 1, 1, 1
  
  UNTIL (msb<<1)=0 DO
    bitsperword, msb, allones := bitsperword+1, msb<<1, allones<<1 | 1
  on64 := bitsperword=64 // =TRUE if running on a 64-bit system

  writes("*ntcmpltest entered*n")
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
  bugtestno := 0
  
  quiet := FALSE

  FOR n = 1 TO 50 DO
  { testno := 100*n
    tstbreak(n)
    tstloop(n)
    tstendcase(n)
    tstnext(n)
    tstexit(n)
    tstreturn(n)
  }

  writef("*n%n tests completed, %n failure%-%ps*n", testcount, failcount)
}

AND tstbreak(n) BE SWITCHON n INTO
{ DEFAULT:
    RETURN

  CASE 1:
  { abort(4536)
    WHILE n<20 DO
    { w := n
      IF n>9 BREAK
      w := 123
      n := n+1
    }
    t(w, 10)
    ENDCASE
  }

  CASE 2:
  { FOR i = 1 TO 20 DO
    { w := i
      IF i>10 BREAK
      w := 123
    }
    t(w, 11)
    ENDCASE
  }

  CASE 3:
  { { w := n
      IF n>11 BREAK
      w := 123
      n := n+1
    } REPEATWHILE n<20
    t(w, 12)
    ENDCASE
  }

  CASE 4:
  { { w := n
      IF n>12 BREAK
      w := 123
      n := n+1
    } REPEAT
    t(w, 13)
    ENDCASE
  }
}

AND tstloop(n) BE SWITCHON n INTO
{ DEFAULT:
    RETURN
}

AND tstendcase(n) BE SWITCHON n INTO
{ DEFAULT:
    RETURN
}

AND tstnext(n) BE SWITCHON n INTO
{ DEFAULT:
    RETURN
}

AND tstexit(n) BE SWITCHON n INTO
{ DEFAULT:
    RETURN
}

AND tstreturn(n) BE SWITCHON n INTO
{ DEFAULT:
    RETURN
}


