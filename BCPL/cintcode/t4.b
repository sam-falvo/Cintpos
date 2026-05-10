GET "libhdr"

LET start() = VALOF
{ writef("Test conversion from 64 to 32 bit floating point*n")
  writef("It should only be run under 64 bit BCPL*n")
  writef("The current system is %n bit*n*n", BITSPERBCPLWORD)
  tst(0.0)
  tst(1.0)
  tst(-1.0)
  tst(1.2345)
  tst(-1.2345)
  RESULTIS 0
}

AND tst(x) BE
{ LET x32 = sys(Sys_flt, fl_64to32, x)
  writef("%64b*n%64b*n*n", x, x32)
}
