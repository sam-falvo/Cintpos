GET "libhdr.h"

STATIC {
  a = -1.25
  b =  1.25
}

LET start() = VALOF
{ LET x = -1.25
  LET y = a
  LET t = TABLE 7, -1.0, -7, 1.0
  writef("x   = %12.8f*n", x)
  writef("t!1 = %12.8f*n", t!1)
  writef("t!3 = %12.8f*n", t!3)
  writef("BCPLWORD length = %n*n", BITSPERBCPLWORD)
  TEST ON64
  THEN { writef("x = %16x*n", x)
         x := sys(Sys_flt, fl_64to32, x)
         writef("x = %8x in single precision*n", x)
       }
  ELSE { LET lw = sys(Sys_flt, fl_32to64, x)
         LET mw = result2
         writef("x     = %8x %9.8f*n", x, x)
         writef("Coverting x to 64 bit floating point using fl_32to64*n")
         writef("mw,lw = %8x %8x*n", mw, lw)
       }
  RESULTIS 0
}
