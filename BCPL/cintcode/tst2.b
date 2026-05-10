GET "libhdr.h"

LET start() = VALOF
{ LET w = 0
  FOR a = 0 TO 1_000_000 DO IF a MOD 10_000 < 10 DO w := !a
  result2 := 0
  RESULTIS 100
}
