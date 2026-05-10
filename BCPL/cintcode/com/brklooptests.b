GET "libhdr.h"

GLOBAL {
  testno: ug
  errcount
}
LET start() = VALOF
{ testno := 1
  errcount := 0
  newline()
writef("Repetition tests*n*n")

  tst(tstf1(10), 1064)
  tst(tstf2(10), 1064)
  tst(tstf3(10), 1064)
  tst(tstf4(10), 1064)
  tst(tstf5(10), 1064)
  tst(tstf6(10), 1064)
  tst(tstf7(10), 1064)
  tst(tstf8(10),  970)
  tst(tstf9(10),  530)

  testno := 1001
  
  tst(tstg1(10), 11)
  tst(tstg2(10), 54)
  tst(tstg3(10), 6)
  tst(tstg4(10), 1)
  tst(tstg5(10), 23)
  tst(tstg6(10), 23)
  tst(tstg7(10), 378)
  tst(tstg8(10), 245)
  tst(tstg9(10), 24)

  testno := 2001

  tst(tsth1(10), 36)
  tst(tsth2(10), 36)
  tst(tsth3(10), 288)
  tst(tsth4(10), 288)
  tst(tsth5(10), 50)
  tst(tsth6(10), 0)

  testno := 3001
  tst(tstk1(10), 0)
  tst(tstk2(10), 0)
  tst(tstk3(10), 37)
  tst(tstk4(10), 0)
  tst(tstk5(10), 66)
  tst(tstk6(10), 66)
  tst(tstk7(10), 66)

  testno := 4001
  writef("*nTesting repetitive loop optimizations of bcpltrn*n")
  tst(tstm1(10), 0)
  tst(tstm2(10), 31)
  tst(tstm3(10), 31)
  tst(tstm4(10), 61)
  tst(tstm5(10), 50)
  tst(tstm6(10), 61)
  tst(tstm7(10), 73)

fin:
  writef("*nError count = %n*n", errcount)
  RESULTIS 0
}

AND tst(x, y) BE
{ LET res = x=y -> "Good", "Bad"
  UNLESS x=y DO errcount, res := errcount+1, "Bad"
  writef("testno=%i5: x=%i5 y=%i5 %s*n", testno, x, y, res)
  testno := testno+1
}

AND tstf1(n) = VALOF
{ // Test simple WHILE with normal loop and BREAK
  // with out FOR loop having constant init and final values.
  LET res = 0
//writef("*ntstf1*n")

  FOR i = 1 TO 10 DO
  { LET j = 1
    //writef("i=%i2 res=%i4*n", i, res)

    WHILE j <= 8 DO
    {
      //writef("     j=%i2 res=%i4*n", j, res)
      j := j+1
      IF i=3 LOOP
      IF i=5 BREAK
      res := res+1 // Obeyed 64 times,
                   // ie Incremented by 8 for all i except 3 and 5
    }

    res := res+100 // Obeyed 10 times
  }
//writef("res=%n*n", res)
  RESULTIS res     // Should be 1064
}

AND tstf2(n) = VALOF
{ // Test simple WHILE with normal loop and BREAK
  // with out FOR loop having non constant init and final values.
  LET res = 0
//writef("*ntstf2*n")

  FOR i = n-9 TO n DO // ie 1 to 10
  { LET j = 1
    //writef("i=%i2 res=%i4*n", i, res)

    WHILE j <= 8 DO
    {
      //writef("     j=%i2 res=%i4*n", j, res)
      j := j+1
      IF i=3 LOOP
      IF i=5 BREAK
      res := res+1 // Obeyed 64 times,
                   // ie Incremented by 8 for all i except 3 and 5
    }

    res := res+100 // Obeyed 10 times
  }
//writef("res=%n*n", res)

  RESULTIS res     // Should be 1064
}

AND tstf3(n) = VALOF
{ // Test simple WHILE with normal loop and 2 BREAKs
  // with out FOR loop having constant init and final values.
  LET res = 0
//writef("*ntstf3*n")

  FOR i = 1 TO 10 DO // ie 1 to 10
  { LET j = 1
    //writef("i=%i2 res=%i4*n", i, res)

    WHILE 1 <= 8 DO
    { IF j>8 BREAK
      //writef("     j=%i2 res=%i4*n", j, res)
      j := j+1
      IF i=3 LOOP
      IF i=5 BREAK
      res := res+1 // Obeyed 64 times,
                   // ie Incremented by 8 for all i except 3 and 5
    }

    res := res+100 // Obeyed 10 times
  }
  //writef("res=%i4*n", res)

  RESULTIS res     // Should be 1064
}

AND tstf4(n) = VALOF
{ // Test simple simple FOR loop with normal loop and 2 BREAKs
  // with outer FOR loop having constant init and final values.
  LET res = 0
//writef("*ntstf4*n")

  FOR i = 1 TO 10 DO // ie 1 to 10
  { 
    //writef("i=%i2 res=%i4*n", i, res)

    FOR j = 1 TO 8 DO
    { IF j>8 BREAK
      //writef("     j=%i2 res=%i4*n", j, res)
      IF i=3 LOOP
      IF i=5 BREAK
      res := res+1 // Obeyed 64 times,
                   // ie Incremented by 8 for all i except 3 and 5
    }

    res := res+100 // Obeyed 10 times
  }
  //writef("res=%i4*n", res)

  RESULTIS res     // Should be 1064
}

AND tstf5(n) = VALOF
{ // Test simple simple FOR loop with zero step length and normal
  // LOOP and 2 BREAKs
  // with outer FOR loop having constant init and final values.
  LET res = 0
//writef("*ntstf5*n")

  FOR i = 1 TO 10 DO // ie 1 to 10
  { LET j = 1
    //writef("i=%i2 res=%i4*n", i, res)

    FOR j = 1 TO 8 BY 0 DO
    { //writef("i=%i2 j=%i3 res=%i4*n", i, j, res)
      IF j>8 BREAK
      ////writef("     j=%i2 res=%i4*n", j, res)
      j := j+1
      IF i=3 LOOP
      IF i=5 BREAK
      res := res+1 // Obeyed 64 times,
                   // ie Incremented by 8 for all i except 3 and 5
    }

    res := res+100 // Obeyed 10 times
  }
  //writef("res=%i4*n", res)

  RESULTIS res     // Should be 1064
}

AND tstf6(n) = VALOF
{ // Test simple simple FOR loop with zero step length and no
  // limit value and normal LOOP and 2 BREAKs
  // with outer FOR loop having constant init and final values.
  LET res = 0
  //writef("*ntstf6*n")
  
  FOR i = 1 TO 10 DO // ie 1 to 10
  { LET j = 1
    //writef("i=%i2 res=%i4*n", i, res)

    FOR j = 1 BY 0 DO // Step of zero, no limit
    { IF j>8 BREAK
      //writef("     j=%i2 res=%i4*n", j, res)
      j := j+1
      IF i=3 LOOP
      IF i=5 BREAK
      res := res+1 // Obeyed 64 times,
                   // ie Incremented by 8 for all i except 3 and 5
      // Destination of LOOP
    }
    // Destination of both BREAKs

    res := res+100 // Obeyed 10 times
  }
//writef("res=%n*n", res)
  RESULTIS res     // Should be 1064
}


AND tstf7(n) = VALOF
{ // Test simple simple FOR loop with no given step length and no
  // limit value and normal LOOP and 2 BREAKs
  // with outer FOR loop having constant init and final values.
  LET res = 0
  //writef("*ntstf7*n")

  FOR i = 1 TO 10 DO // ie 1 to 10
  { LET j = 1
    //writef("i=%i2 res=%i4*n", i, res)

    FOR j = 1 DO
    { IF j>8 BREAK
      //writef("     j=%i2 res=%i4*n", j, res)
      IF i=3 LOOP
      IF i=5 BREAK
      res := res+1 // Obeyed 64 times,
                   // ie Incremented by 8 for all i except 3 and 5
      // Desination of LOOP
    }
    // Destination of both BREAKs
    
    res := res+100 // Obeyed 10 times
  }
//writef("res=%n*n", res)

  RESULTIS res     // Should be 1064
}



AND tstf8(n) = VALOF
{ // Test simple FOR loop with LOOP in the limit expression
  // with outer FOR loop having constant init and final values.
  LET res = 0
  //writef("tstf8*n")

  FOR i = 1 TO 10 DO // ie 1 to 10
  {
    //writef("i=%i2 res=%i4*n", i, res)

    FOR j = 1 TO i=2 -> LOOP, 10 DO
    { // j is set successively to 1, 3, 4, 5, 6, 7, 8, 9, 10
      //writef("     j=%i2 res=%i4*n", j, res)
      IF i=3 LOOP
      IF i=5 BREAK
      // This point is reached when
      // i    and    j      have the following value
      // 1           1-10     ie 10 times
      // 4           1-10     ie 10 times
      // 6-10        1-10     ie 50 times
      res := res+1 // Obeyed 70 times,
    }
    // This point is reached when i = 1, 3-10

    res := res+100 // Obeyed 9 times
  }
  //writef("res=%i4*n", res)

  RESULTIS res     // Should be 970
}

AND tstf9(n) = VALOF
{ // Test simple FOR loop with BREAK in the limit expression
  // with outer FOR loop having constant init and final values.
  LET res = 0
  //writef("tstf9*n")

  FOR i = 1 TO 10 DO // ie 1 to 10
  {
    //writef("i=%i2 res=%i4*n", i, res)

    FOR j = 1 TO i=6 -> BREAK, 10 DO
    { // This point is only reached when j = 1
      //writef("     j=%i2 res=%i4*n", j, res)
      IF i=3 LOOP
      IF i=5 BREAK
      // This point is reached when
      // i    and    j      have the following value
      // 1           1-10     ie 10 times
      res := res+1 // Obeyed 10 times,
    }

    res := res+100 // Obeyed 1 time
  }
  //writef("res=%i4*n", res)

  RESULTIS res     // Should be 110
}


AND tstg1(n) = VALOF
{ // Simple WHILE loop
  LET res = 0
  LET i   = 0
  //writef("tstg1*n")

  WHILE i <= 10 DO
  {
    //writef("i=%i2 res=%i4*n", i, res)
    i := i+1
    res := res+1
  }
  //writef("res=%i4*n", res)

  RESULTIS res
}

AND tstg2(n) = VALOF
{ // Simple WHILE loop containing LOOP
  LET res = 0
  LET i = 0
  //writef("tstg2*n")

  WHILE i <= 10 DO
  {
    //writef("i=%i2 res=%i4*n", i, res)
    i := i+1
    IF i=4 | i=8 LOOP
    res := res+i
  }
  //writef("res=%i4*n", res)

  RESULTIS res
}

AND tstg3(n) = VALOF
{ // Simple UNTIL loop containing BREAK
  LET i = 0
  LET res = 0
  //writef("tstg3*n")

  UNTIL i > 10 DO
  {
    //writef("i=%i2 res=%i4*n", i, res)
    i := i+1
    IF i=4 | i=8 BREAK
    res := res+i
  }
  //writef("res=%i4*n", res)

  RESULTIS res
}

AND tstg4(n) = VALOF
{ // Simple REPEATWHILE FALSE loop containing LOOP and BREAK
  LET i = 0
  LET res = 0
  //writef("tstg4*n")

  {
    //writef("i=%i2 res=%i4*n", i, res)
    i := i+1
    IF 2<=i<=3 LOOP
    IF i=5 BREAK
    res := res+i
  } REPEATWHILE FALSE
  //writef("res=%i4*n", res)

  RESULTIS res
}

AND tstg5(n) = VALOF
{ // Simple REPEATWHILE TRUE loop containing LOOP and BREAK
  LET i = 0
  LET res = 0
  //writef("tstg5*n")

  {
    //writef("i=%i2 res=%i4*n", i, res)
    i := i+1
    IF 2<=i<=3 LOOP
    IF i=8 BREAK
    res := res+i
  } REPEATWHILE TRUE
  //writef("res=%i4*n", res)

  RESULTIS res
}

AND tstg6(n) = VALOF
{ // Simple REPEATWHILE loop containing LOOP and BREAK
  LET i = 0
  LET res = 0
  //writef("tstg6*n")
  
  { //writef("i=%i2 res=%i4*n", i, res)
    i := i+1
    IF 2<=i<=3 LOOP
    IF i=8 BREAK
    res := res+i
  } REPEATWHILE i<n
  //writef("res=%i4*n", res)

  RESULTIS res
}

AND tstg7(n) = VALOF
{ LET res = 0
  //writef("tstg7*n")

  FOR i = 1 TO 10 DO
  { LET j = 1
    //writef("i=%i2 res=%i4*n", i, res)

    WHILE j<10 & (i=3 -> LOOP, TRUE) DO
    { //writef("i=%i2 j=%i2 res=%i4*n", i, j, res)
      j := j+1
      IF i=4 LOOP
      IF i=8 BREAK
      res := res+j
    }
  }
  //writef("res=%i4*n", res)

  RESULTIS res
}

AND tstg8(n) = VALOF
{ LET res = 0
  //writef("tstg8*n")

  FOR i = 1 TO 10 DO
  { LET j = 1
    //writef("i=%i2 res=%i4*n", i, res)
    WHILE j<8 & (i=3 -> LOOP, TRUE) DO
    { //writef("i=%i2 j=%i2 res=%i4*n", i, j, res)
      j := j+1
      IF i=4 LOOP
      IF i=8 BREAK
      res := res+j
    }
  }
  //writef("res=%i4*n", res)

  RESULTIS res
}

AND tstg9(n) = VALOF
{ LET res = 0
  //writef("tstg9*n")

  FOR i = 1 DO
  { //writef("i=%i2 res=%i4*n", i, res)
    IF i=4 LOOP
    IF i=8 BREAK
    res := res+i
  }
  //writef("res=%i4*n", res)

  RESULTIS res
}

AND tsth1(n) = VALOF
{ LET res = 0
  LET x = 5566
  //writef("tsth1*n")
  
  FOR i = 1 TO 10 DO //p6
  { LET j = 0        //p7
    //writef("     i=%i2 res=%i4*n", i, res)
    WHILE (j=8 & BREAK) | TRUE DO
    { j := j+1
      //writef("     i=%i2 j=%i2 res=%i4*n", i, j, res)
      IF i=4 | i=8 LOOP
      res := res+j
    }
  }
  x:= 5567
  // Destination of BREAK
  
  //writef("res=%i4*n", res)

  RESULTIS res
}

AND tsth2(n) = VALOF
{ LET res = 0
  //writef("tsth2*n")
  
  FOR i = 1 TO 10 DO
  { LET j = 0
    //writef("     i=%i2 res=%i4*n", i, res)
    WHILE (j=8 & GOTO L) | TRUE DO
    { j := j+1
      //writef("     i=%i2 j=%i2 res=%i4*n", i, j, res)
      IF i=4 | i=8 LOOP
      res := res+j
    }
  }
L:// Desitnation of BREAK

  //writef("res=%i4*n", res)
  //abort(1002)
  RESULTIS res
}

AND tsth3(n) = VALOF
{ LET res = 0
  //writef("tsth3*n")
  
  FOR i = 1 TO 10 DO
  { LET j = 0
    //writef("     i=%i2 res=%i4*n", i, res)
    WHILE (j=8 & LOOP) | TRUE DO
    { j := j+1
      //writef("     i=%i2 j=%i2 res=%i4*n", i, j, res)
      IF i=4 | i=8 LOOP
      res := res+j
    }
  L:
    // Destination of LOOP
  }
  // Destination of BREAK
  //writef("res=%i4*n", res)

  RESULTIS res
}

AND tsth4(n) = VALOF
{ LET res = 0
  //writef("tsth4*n")
  
  FOR i = 1 TO 10 DO
  { LET j = 0
    //writef("     i=%i2 res=%i4*n", i, res)
    WHILE (j=8 & GOTO L) | TRUE DO
    { j := j+1
      //writef("     i=%i2 j=%i2 res=%i4*n", i, j, res)
      IF i=4 | i=8 LOOP
      res := res+j
    }
  L:
    // Destination of LOOP
  }
  // Destination of BREAK
  //writef("res=%i4*n", res)

  RESULTIS res
}

AND tsth5(n) = VALOF
{ LET res = 0
  //writef("tsth5*n")
  
  FOR i = 1 TO 10 DO
  { LET j = 0
    //writef("    i=%i2 res=%i5*n", i, res)
    WHILE j < 10 | RESULTIS 50 DO
    { //writef("    i=%i2 j=%i2 res=%i5*n", i, j, res)
      j := j+1
      IF i=4 | i=8 LOOP
      res := res+j
    }
  }
  //writef("res=%i5*n", res)

  RESULTIS res
}

AND tsth6(n) = VALOF
{ LET res = 0
  //writef("tsth6*n")
  
  FOR i = 1 TO 10 DO
  { LET j = 0
    //writef("    i=%i2 res=%i5*n", i, res)
    WHILE j < 10 | RETURN DO
    { //writef("    i=%i2 j=%i2 res=%i5*n", i, j, res)
      j := j+1
      IF i=4 | i=8 LOOP
      res := res+j
    }
  }
  //writef("res=%i5*n", res)

  RESULTIS res
}

AND tstk1(n) = VALOF
{ LET res = 0
  LET i = 0
  //writef("tstk1*n")
  
  WHILE FALSE DO
  { //writef("    i=%i2 res=%i5*n", i, res)
    i := i+1
    IF i>8 LOOP
    res := res+i
  }

  //writef("res=%i5*n", res)

  RESULTIS res
}

AND tstk2(n) = VALOF
{ LET res = 0
  LET i = 0
  //writef("tstk2*n")
  
  WHILE FALSE DO
  { //writef("    i=%i2 res=%i5*n", i, res)
    i := i+1
    IF i>8 LOOP
    res := res+i
  }

  //writef("res=%i5*n", res)

  RESULTIS res
}

AND tstk3(n) = VALOF
{ LET res = 0
  LET i = 0
  //writef("tstk3*n")
  
  WHILE TRUE & n=n+0 DO
  { //writef("    i=%i2 res=%i5*n", i, res)
    i := i+1
    IF i=8 LOOP
    IF i>9 BREAK
    res := res+i
  }

  //writef("res=%i5*n", res)

  RESULTIS res
}

AND tstk4(n) = VALOF
{ LET res = 0
  LET i = 0
  //writef("tstk4*n")
  
  WHILE FALSE & n=n+0 DO
  { //writef("    i=%i2 res=%i5*n", i, res)
    i := i+1
    IF i=8 LOOP
    res := res+i
  }

  //writef("res=%i5*n", res)

  RESULTIS res
}

AND tstk5(n) = VALOF
{ LET res = 0
  LET i = 0
  //writef("tstk5*n")
  
  WHILE i<=n DO
  { //writef("    i=%i2 res=%i5*n", i, res)
    i := i+1
    res := res+i
  }

  //writef("res=%i5*n", res)

  RESULTIS res
}

AND tstk6(n) = VALOF
{ LET res = 0
  LET i = 0
  //writef("tstk6*n")
  
  WHILE i<=n+0 DO
  { //writef("    i=%i2 res=%i5*n", i, res)
    i := i+1
    res := res+i
  }

  //writef("res=%i5*n", res)

  RESULTIS res
}

AND tstk7(n) = VALOF
{ LET res = 0
  LET i = 0
  //writef("tstk7*n")
  
  WHILE i<=n+0 DO
  { //writef("    i=%i2 res=%i5*n", i, res)
    i := i+1
    res := res+i
  }

  //writef("res=%i5*n", res)

  RESULTIS res
}

// Systematic test of WHILE/UNTIL optimisations

AND tstm1(n) = VALOF
{ // Testing manifest constant small condition (FALSE)
  LET x = 22201 // Cintcode location marker
  LET res = 0
  LET i = 0
  //writef("tstm1*n")
  
  WHILE FALSE DO
  { //writef("    i=%i2 res=%i5*n", i, res)
    i := i+1
    IF i>8 BREAK
    res := res+i
  }

  //writef("res=%i5*n", res)

  RESULTIS res
}

AND tstm2(n) = VALOF
{ // Testing manifest constant small condition (TRUE)
  LET x = 22202 // Cintcode location marker
  LET res = 0
  LET i = 0
  //writef("tstm2*n")
  
  WHILE TRUE DO
  { //writef("    i=%i2 res=%i5*n", i, res)
    i := i+1
    IF i=5 LOOP    // Should jump to start of loop
    IF i>8 BREAK
    res := res+i
  }

  //writef("res=%i5*n", res)

  RESULTIS res
}

AND tstm3(n) = VALOF
{ // Testing manifest constant small condition (TRUE)
  LET x = 22203 // Cintcode location marker
  LET res = 0
  LET i = 0
  //writef("tstm3*n")
  
  WHILE i<n DO
  { //writef("    i=%i2 res=%i5*n", i, res)
    i := i+1
    IF i=5 LOOP    // Should jump to start of loop
    IF i>8 BREAK
    res := res+i
  }

  //writef("res=%i5*n", res)

  RESULTIS res
}

AND tstm4(n) = VALOF
{ // Testing manifest constant small condition (TRUE)
  LET x = 22204 // Cintcode location marker
  LET res = 0
  LET i = 0
  //writef("tstm4*n")
  
  UNTIL i>=n+1 DO
  { //writef("    i=%i2 res=%i5*n", i, res)
    i := i+1
    IF i=5 LOOP    // Should jump to start of loop
    IF i>12 BREAK
    res := res+i
  }

  //writef("res=%i5*n", res)

  RESULTIS res
}

AND tstm5(n) = VALOF
{ // Testing manifest constant small condition (TRUE)
  LET x = 22205 // Cintcode location marker
  LET res = 0
  LET i = 0
  //writef("tstm5*n")
  
  { //writef("    i=%i2 res=%i5*n", i, res)
    i := i+1
    IF i=5 LOOP    // Should jump to start of loop
    IF i>12 BREAK
    res := res+i
  } REPEATWHILE i<n

  //writef("res=%i5*n", res)

  RESULTIS res
}

AND tstm6(n) = VALOF
{ // Testing manifest constant small condition (TRUE)
  LET x = 22205 // Cintcode location marker
  LET res = 0
  LET i = 0
  //writef("tstm6*n")
  
  { //writef("    i=%i2 res=%i5*n", i, res)
    i := i+1
    IF i=5 LOOP    // Should jump to start of loop
    IF i>12 BREAK
    res := res+i
  } REPEATUNTIL i>n

  //writef("res=%i5*n", res)

  RESULTIS res
}

AND tstm7(n) = VALOF
{ // Testing manifest constant small condition (TRUE)
  LET x = 22205 // Cintcode location marker
  LET res = 0
  LET i = 0
  //writef("tstm7*n")
  
  { //writef("    i=%i2 res=%i5*n", i, res)
    i := i+1
    IF i=5 LOOP    // Should jump to start of loop
    IF i>12 BREAK
    res := res+i
  } REPEAT

  //writef("res=%i5*n", res)

  RESULTIS res
}









