GET "libhdr.h"

LET start() = VALOF
{ LET v = getvec(100_000)
  LET w = v + 50_000


  FOR i = 1 TO 50_000 DO v!i, w!i := 0, 0
  freevec(v)
  RESULTIS 0

}
