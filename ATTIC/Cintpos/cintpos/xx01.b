// Test for xbcpl

GET "libhdr.h"

LET start() = VALOF
{ LET a, b, c, d = 10, 11, 12, #x12345678
  LET p = @a
  //writef("Test entered*n")

  writef("a=%8x b=%8x c=%8x c=%8x*n", a, b, c, d) 
  //p!3 := 1 
  (SLCT 12:4:3) OF p := 5
  a := (SLCT 8:4:3) OF p
  a := (SLCT 8:4:0) OF p
  a := (SLCT 8:0:0) OF p
  a := (SLCT 0:0:0) OF p
  a := (SLCT 0:0:3) OF p
  //(SLCT 3:2:3) OF p +:= 1
  writef("a=%8x b=%8x c=%8x c=%8x*n", a, b, c, d) 
  //(SLCT 8:12:1) OF p +:= 2
  //writef("a=%8x b=%8x c=%8x c=%8x*n", a, b, c, d) 

  RESULTIS 0
}
