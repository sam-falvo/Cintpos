
// This program tries out various methods to count the number
// of ones in a vector of 1000000 words.
// Implemented in Cintcode BCPL by Martin Richards (c) July 2016

GET "libhdr"

GLOBAL {
  tracing:ug
  // The following are used by try8
  c00; c01; c02; c03; c04; c05; c06; c07; c08; c09 
  c10; c11; c12; c13; c14; c15; c16; c17; c18; c19 
  c20
  inc
}

LET start() = VALOF
{ LET upb  = 10
  LET data = 0
  LET seed = 12345
  LET n    = -1    // ie about 5 ones in each word
  LET argv = VEC 50

  UNLESS rdargs("n/n,upb/n,seed/n,-t/s", argv, 50) DO
  { writef("Bad arguments for vbits*n")
    RESULTIS 0
  }

  IF argv!0 DO n    := !argv!0     // n/n
  IF argv!1 DO upb  := !argv!1     // upb/n
  IF argv!2 DO seed := !argv!2     // seed/n
  tracing := argv!3                // -t/s

  IF n<0 DO n := 5 * upb  // ie about 5 ones in each word

  data := getvec(upb)

  writef("*nTest various implementations of vbits, n=%n upb=%n*n*n", n, upb)

  setseed(seed)

  setdata(data, upb, n)

  IF tracing DO
  { FOR i = 1 TO upb DO writef("%32b*n", data!i)
    newline()
  }

  try("try0", try0, data, upb)
  try("try1", try1, data, upb)
  ///try("try2", try2, data, upb)
  try("try7", try7, data, upb)
  try("try8", try8, data, upb)
  try("try9", try9, data, upb)

  newline()
  freevec(data)
  writef("*n*nEnd of test*n")
  RESULTIS 0
}

AND setdata(data, upb, n) BE
{ // Initialise data so that it holds exactly n one bits
  // uniformly scattered in data!0 to data!upb
  FOR i = 0 TO upb DO data!i := 0
  // Set n one bits
  FOR i = 1 TO n UNTIL setrandbit(data, upb) LOOP
}

AND setrandbit(v, upb) BE
{ LET p   = randno(upb)           // 1 to upb
  LET sh  = randno(bitsperword)-1 // Typically 0 to 31
  LET bit = 1<<sh
  LET w   = v!p
  IF (w & bit) = 0 DO
  { v!p := w + bit
    RETURN
  }
} REPEAT

AND try(name, f, data, upb) BE
{ LET count = instrcount(f, data, upb)
  LET res = result2
  writef("%6t %i7 words  %i9 bits  res = %n", name, upb, try1(data,upb), res)
  writef(" %i9%c instructions", count, try1(data, upb)=res -> ' ', '#')
  newline()
}

AND try0(data, upb) = VALOF
{ LET sum = 0
  FOR i = 1 TO upb DO sum := sum + bts(data!i)
  RESULTIS sum
}

AND try1(data, upb) = VALOF
{ LET sum = 0
  FOR i = 1 TO upb DO sum := sum + bts1(data!i)
  RESULTIS sum
}

AND try7(data, upb) = VALOF
{ LET sum = 0
  FOR i = 1 TO upb DO sum := sum + bts7(data!i)
  RESULTIS sum
}

AND try8(data, upb) = VALOF
{ // Try using longitudinal numbers.
  // The speed is disappointing.
  LET sum = 0
  LET numv = VEC 20
  FOR i = 0 TO 20 DO numv!i := 0

  FOR i = 1 TO upb DO
  { // Increment some of the 32 longitudinal numbers
    LET bits = data!i // The sent of numbers to increment
    LET p = numv

    WHILE bits DO
    { LET w = !p
      !p   := w XOR bits // The new digits
      bits := w  &  bits // The carry bit pattern
      p := p+1
    }
  }

  // This part is very cheap
  FOR i = 20 TO 0 BY -1 DO sum := sum+sum+bts7(numv!i)

  IF tracing DO
  { writef("*nLongitudinal numbers, sum=%n*n", sum)
    FOR i = 0 TO 20 DO writef("%i2: %32b*n", i, numv!i)
  }

  RESULTIS sum
}

AND try9(data, upb) = VALOF
{ // Try using longitudinal numbers using 21 variables c00 to c20 in globals.
  // The speed is disappointing.
  LET sum = 0

  c00,c01,c02,c03,c04,c05,c06,c07,c08,c09 := 0,0,0,0,0,0,0,0,0,0
  c10,c11,c12,c13,c14,c15,c16,c17,c18,c19 := 0,0,0,0,0,0,0,0,0,0
  c20 := 0

  FOR i = 1 TO upb DO inc(data!i)

  // This part is very cheap
  FOR i = 20 TO 0 BY -1 DO sum := sum+sum+bts7((@c00)!i)

  IF tracing DO
  { writef("*nLongitudinal numbers, sum=%n*n", sum)
    FOR i = 0 TO 20 DO writef("%i2: %32b*n", i, (@c00)!i)
  }

  RESULTIS sum
}

AND inc(bits) BE
{ LET x = ?

  x := c00; c00 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c01; c01 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c02; c02 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c03; c03 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c04; c04 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c05; c05 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c06; c06 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c07; c07 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c08; c08 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c09; c09 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c10; c10 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c11; c11 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c12; c12 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c13; c13 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c14; c14 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c15; c15 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c16; c16 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c17; c17 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c18; c18 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c19; c19 := x XOR bits; bits := x & bits; UNLESS bits RETURN
  x := c20; c20 := x XOR bits; bits := x & bits; UNLESS bits RETURN

  writef("*nERROR: Longitudinal number too large*n")
  abort(999)
}

AND bts(w) = w=0 -> 0, (w&1) + bts(w>>1)

AND bts1(w) = VALOF
{ LET r = 0
  WHILE w DO r, w := r+1, w & (w-1)
  RESULTIS r
}

AND bts2(w) = VALOF
{ w := (w    & #x11111111) +
       (w>>1 & #x11111111) +
       (w>>2 & #x11111111) +
       (w>>3 & #x11111111)

  w := (w    & #x0f0f0f0f) +
       (w>>4 & #x0f0f0f0f)

  RESULTIS (w * #x01010101) >> 24
}

AND bts3a(w) = VALOF
{ w := (w    & #10101010101) +
       (w>>1 & #10101010101) +
       (w>>2 & #10101010101) +
       (w>>3 & #10101010101) +
       (w>>4 & #10101010101) +
       (w>>5 & #10101010101)

  w := (w    & #07777777777) + (w>>30)
  w :=  w    * #00101010101

  RESULTIS (w >> 24) & 63
}

AND bts3(w) = VALOF
{ w := (w    & #04444444445) +
       (w>>1 & #04444444445) +
       (w>>2 & #04444444444)
  w := w + (w&3)*3             // w contains 10 3-bit numbers
  w := (w    & #03434343434) +
       (w>>3 & #03434343434)
  RESULTIS (w* #00101010101) >> 26
}

AND bts4(w) = VALOF
{ w := (w & #x55555555) + ((w>> 1) & #x55555555)
  w := (w & #x33333333) + ((w>> 2) & #x33333333)
  w := (w & #x0f0f0f0f) + ((w>> 4) & #x0f0f0f0f)
  w := (w & #x00ff00ff) + ((w>> 8) & #x00ff00ff)
  w := (w & #x0000ffff) + ((w>>16) & #x0000ffff)
  RESULTIS w
}

AND add(bts, p) BE WHILE bts DO
{ LET w = !p
  !p := w XOR bts
  bts, p := w & bts, p+1
}

AND val(p) = VALOF
{ LET r = p!5 & 1
  FOR i = 4 TO 0 BY -1 DO r := 2*r + (p!i & 1)
  RESULTIS r
}

AND bts5(a,b,c,d,e,f) = VALOF
{ b, c, d, e, f := 0, 0, 0, 0, 0
  add(a>>16, @a)
  add(b>> 8, @b); add(a>>8,@a)
  add(c>> 4, @c); add(b>>4,@b); add(a>>4,@a)
  add(d>> 2, @d); add(c>>2,@c); add(b>>2,@b); add(a>>2,@a)
  add(e>> 1, @e); add(d>>1,@d); add(c>>1,@c); add(b>>1,@b); add(a>>1,@a)
  RESULTIS val(@a)
}

AND bts6(w) = VALOF
{ LET r = 0
  LET t = TABLE 0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4
  WHILE w DO { r := r + t!(w&15); w := w>>4 }
  RESULTIS r
}

AND bts7(w) = VALOF
{ LET t = TABLE
    0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8

  RESULTIS t!(w & 255)+t!(w>>8 & 255)+t!(w>>16 & 255)+t!(w>>24 & 255)
}

