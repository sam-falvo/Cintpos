GET "libhdr.h"

LET Q_rsqrt(FLT x) = VALOF
{ // Calculate 1/sqrt(x) approx
  LET FLT xby2 = x / 2
  LET y = #x5F3759DF - (x>>1) // Good initial guess
  y  := y * ( 1.5 - (xby2*y*y))    // Newton Rapson
  y  := y * ( 1.5 - (xby2*y*y))    // Newton Rapson
  RESULTIS y
}

LET start() = VALOF
{ newline()
  writef("#x5F3759DF = %9.6f*n", #x5F3759DF)
  tst(1.0)
  tst(2.0)
  tst(3.0)
  tst(4.0)
  tst(5.0)
  tst(6.0)
  RESULTIS 0
}

AND tst(FLT x) BE
{ LET FLT a = Q_rsqrt(x)
  writef("1/sqrt(%8.6f) = a=%9.6f   a^2=%8.6f   1/a^2=%8.6f*n", x, a, a*a, 1.0/(a*a))
}

