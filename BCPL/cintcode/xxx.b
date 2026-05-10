GET "libhdr.h"

LET start() = VALOF
{
  sys(Sys_gl, 1234)
  RESULTIS 0
}
