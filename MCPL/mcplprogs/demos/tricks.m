GET "mcpl.h"

FUN start : =>
  FOR i = 1 TO 63 DO
  { LET a = i
    a -:= (a>>1)&#o3333333333
    a -:= (a>>1)&#o3333333333
    a := ((a>>3) + a) & #o0707070707

    LET b = i & #x3f
    b := ((b * #o02020202) & #o0104422010) MOD 255

    LET c = i & -i
    LET r = i + c
    c := (((r XOR i)>>2)/c) | r

    writef("%6b => %6b   %8b   %8b\n", i, a&63, b, c)
  }
.