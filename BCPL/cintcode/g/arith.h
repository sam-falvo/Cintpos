/*
This is the header file for the arith high precision library arith.b.
For more details see arith.b.

Implemented by Martin Richards (c) April 2016

Programs using this library normally start as follows:

GET "libhdr"
MANIFEST {
  ArithGlobs=350  // The first global used by the arith library
  numupb=2+25     // Room for the sign, the exponent and 25 radix
                  // digits, equivalent to 100 decimal digits.
}                 // Each radix digit is in the range 0 to 9999
GET "arith.h"
GET "arith.b"

Followed by the rest of the program.
*/

GLOBAL {
  str2num: ArithGlobs
  setzero
  settok
  copy
  copyu
  addcarry
  roundnum
  standardize
  addu
  add
  subu
  sub
  neg
  mul
  mulbyk
  inv
  div
  divbyk
  exptok
  sqrt
  inprod
  crossprod
  radius
  normalize
  iszero
  numcmpu
  numcmp
  integerpart
  roundtoint
  prnum
  checknum
}



