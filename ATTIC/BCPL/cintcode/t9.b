GET "libhdr"

GLOBAL {
  g250:250
  FLT g251
  FLT g252
      g253
}

STATIC {
  FLT s1=1
  FLT s2
      s3
  FLT s4
}

MANIFEST {
      m0=0
  FLT f1
      i2
  FLT f3
}

LET f(x, FLT y, z) = VALOF
{ //LET p, FLT q, r, FLT s, t = 12, 34, 56, 78, 90
  //LET a, FLT b = p, q
  //LET v = VEC 10
  //LET d = 0.00000000000
  //LET e = 1.00000000000
  //LET a = 2.00000000000
  LET b = 1.0

  writef("s1=%6.3f  s2=%6.3f  s3=%6i  s4=%6.3f*n", s1, s2, s3, s4)
  writef("m0=%6i  m1=%6.3f  i2=%6i  f3=%6.3f*n", m0, f1, i2, f3)
 
  b := 1 / y / (x / z)
  //FOR i = 1 TO 10 LOOP
  RESULTIS 0  //x+y
}

//LET g(a,b) BE a := b
LET start() = VALOF
{ f(123, 2.0, 456)
  RESULTIS 0
}
