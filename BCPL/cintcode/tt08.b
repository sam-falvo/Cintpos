// Test integer and floating point manifest constants.
// Changing MANIFEST to STATIC tests static variables.
// They all work as specified in the manual.

GET "libhdr"

MANIFEST {
 FLT f0
     i1
     i2
 FLT f3
 FLT f4
     i10=10
     i11
 FLT f12
 FLT f13=13.6
 FLT f14
     i15
 FLT f16 = 7/2
}

LET start() = VALOF
{ writef("f0  %16x%- %24.1f*n", f0)
  writef("i1  %16x%- %24i*n",   i1)
  writef("i2  %16x%- %24i*n",   i2)
  writef("f3  %16x%- %24.1f*n", f3)
  writef("f4  %16x%- %24.1f*n", f4)
  writef("i10 %16x%- %24i*n",   i10)
  writef("i11 %16x%- %24i*n",   i11)
  writef("f12 %16x%- %24.1f*n", f12)
  writef("f13 %16x%- %24.1f*n", f13)
  writef("f14 %16x%- %24.1f*n", f14)
  writef("i15 %16x%- %24i*n",   i15)
  writef("f16 %16x%- %24.1f*n", f16)

  RESULTIS 0
}
