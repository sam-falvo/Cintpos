// This is to test the new FLT feature that allows integer operators
// to be converted to their floating point versions depending on
// the context.

GET "libhdr"

GLOBAL {
  g250:250  // If no global number is given the next available
  FLT g251  // number is used.
  FLT g252
      g253
}

STATIC {
  FLT s1=1 // FLT statics are given floating point values
  FLT s2   // If no value is specified 0 or 0.0 is used
      s3
  FLT s4
}

MANIFEST {
      m0=0
  FLT f1    // If no value is specified a value 1 or 1.0 larger
      i2    // than the previous value is used.
  FLT f3
}

LET f(x, FLT y, z) = VALOF
{ // Function arguments and the right hand sides of simple varibale
  // declartions are evaluated in non FLT mode.
  LET i12, FLT f34, i56, FLT f78, i90 = 12, FLOAT 34, 56, 78#+1.0, 90
  //LET a, FLT b = p, q
  //LET v = VEC 10
  //LET d = 0.00000000000
  //LET e = 1.00000000000
  //LET a = 2.00000000000
  LET b = 1.0

  writef("s1=%6.3f  s2=%6.3f  s3=%6i  s4=%6.3f*n", s1, s2, s3, s4)
  writef("m0=%6i  m1=%6.3f  i2=%6i  f3=%6.3f*n", m0, f1, i2, f3)
 
  writef("i12=%6i f34=%6.3f i56=%6i f78=%6.3f i90=%6i*n", i12, f34, i56, f78, i90)

  b := 1 / y / (x / z)
  //FOR i = 1 TO 10 LOOP
  RESULTIS 0  //x+y
}

//LET g(a,b) BE a := b
LET start() = VALOF
{ f(123, 2.0, 456)
  RESULTIS 0
}
