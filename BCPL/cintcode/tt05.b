GET "libhdr"

LET start() = VALOF
{ LET a = 1_123456789
  LET FLT f1  = sys(Sys_flt, fl_mk, 1, 0)
  LET FLT f10 = sys(Sys_flt, fl_mk, 3, 0)
  LET FLT f.1 = sys(Sys_flt, fl_div, f1, f10)
  writef("f1 = %23.12f  %16x*n", f1, f1)
  writef("f10= %23.12f  %16x*n", f10, f10)
  writef("f.1= %23.12f  %16x*n", f.1, f.1)

  writef("a  = %12n  %16x*n", a, a)
  a := sys(Sys_flt, fl_mk, a, 0)
  a := FIX a
  writef("a  = %12n  %16x*n", a, a)

  RESULTIS 0
}
