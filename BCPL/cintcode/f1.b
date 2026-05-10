GET "libhdr.h"

LET start() = VALOF
{ LET x = 1.1
  LET y = #x7_7777_7777
  LET z = #x8_8888_8888
  LET t = TABLE #x7_7777_7777, #x8_8888_8888
  LET a = 0 //sys(Sys_flt, fl_64to32, x)
  a := "ABCD123456789"
  //writef("x = %16X*na = %16X*n", x, a)
  
  RESULTIS 0
}
