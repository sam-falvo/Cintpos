GET "libhdr.h"

LET start() = VALOF
{ LET a = readn()
  LET b = readn()
  writef("%n*n", a+b)
  RESULTIS 0
}
