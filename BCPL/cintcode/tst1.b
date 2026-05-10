GET "libhdr"

MANIFEST {
  amodb = 15.234 #MOD 1.000
}

LET start() = VALOF
{ LET a = 15.234
  LET b = 1.000
  LET res = 0
  LET r1 = a #MOD b
  LET r2 = ?

  writef("Testing floating point sys functions*n")

  writef("amodb = %8.3f*n", amodb)

  res := sys(Sys_flt, fl_frexp, a)
  writef("frexp of %8.3f is %8.6f  result2 is %8i*n",
           a, res, result2)

  writef("modf of %8.3f is %8.6f  result2 is %8.3f*n",
           a, sys(Sys_flt, fl_modf, a), result2)

  writef("sys(Sys_flt, fl_radius2, 3.0, 4.0) => %8.3f*n",
          sys(Sys_flt, fl_radius2, 3.0, 4.0))
  writef("sys(Sys_flt, fl_radius3, 1.0, 2.0, 2.0) => %8.3f*n",
          sys(Sys_flt, fl_radius3, 1.0, 2.0, 2.0))

  writef("sys(Sys_flt, fl_N2F, 1_000, 1_234) => %8.3f*n",
          sys(Sys_flt, fl_N2F, 1_000, 1_234))
  writef("sys(Sys_flt, fl_F2N, 1_000, 1.234) => %i8*n",
          sys(Sys_flt, fl_F2N, 1_000, 1.234))
  writef("sys(Sys_flt, fl_F2N, 1_000, -1.234) => %i8*n",
          sys(Sys_flt, fl_F2N, 1_000, -1.234))

  writef("sys(Sys_flt, fl_pow, 4.000, 0.5) => %8.3f*n",
          sys(Sys_flt, fl_pow, 4.000, 0.5))

  writef("sys(Sys_flt, fl_ldexp, 1.010, 3) => %8.3f*n",
          sys(Sys_flt, fl_ldexp, 1.010, 3))


  writef("remainder of %8.3f / %8.3f is %8.3f*n", a, 1.000, sys(Sys_flt, fl_mod, a, b))
  writef("%8.3f #MOD %8.3f is %8.3f*n", a, 1.000, a #MOD b)
  r1 := sys(Sys_flt, fl_modf, a)
  r2 := result2
  writef("fractional part of %8.3f is %8.3f*n", a, r1)
  writef("integer    part of %8.3f is %8.3f*n", a, r2)

  RESULTIS 0
}
