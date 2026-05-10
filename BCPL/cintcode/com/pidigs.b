/*
This is a BCPL implementation of the program that appears in section
10.7 of the book "Number Theory, A Programmer's Guide" by Mark
Herkommer. He uses a miraculous formula for pi discovered by David
Bailey, Peter Borwein and Simon Plouffe. The formula is

pi = the sum from n = 0 to infinity of

  (4/(8n+1) - 2/(8n+4) - 1/(8n+5) - 1/(8n+6))/(16**n)

Using modulo arithmetic, it is possible to find the nth hexadecimal
digit of pi without having to compute the others.

Herkommer's program uses double length floating point values, but mine
uses 32 or 64-bit scaled fixed point arithmetic Using scaled numbers
with 28 bits after the decimal point allows this program to compute
the hex digits of pi from position 0 to 5000 correctly. It also
calculates the digits from position 100000 to 100050 correctly as well
as the digit at position one million. There is no guarantee that all
the other positions will be computed correctly since errors can arise
when long sequences of ones occur in the binary representation of pi,
and this is unpredictable. Using 64 bit BCPL the number of
correct digits is ar greater.

Implemented in BCPL by Martin Richards (c) october 2014

*/

GET "libhdr"

GLOBAL {
  stdin:ug
  stdout
  tofilename
  tostream
  upb
  a
  b

  fracbits
  fraclen
  One
  Two
  Four
  fracmask
}

LET start() = VALOF
{ LET argv = VEC 50

  UNLESS rdargs("to/K,upb/N/K,a/N/K,b/N/K,fraclen/N/K", argv, 50) DO
  { writef("Bad arguments for pidigs*n")
    RESULTIS 0
  }

  tostream := 0
  upb := ON64 -> 2000, 1000
  
  a, b, fraclen := -1, -1, -1
  
  tofilename           := argv!0     // to/K
  IF argv!1 DO upb     := !argv!1    // upb/N/K
  IF argv!2 DO a       := !argv!2    // a/N/K
  IF argv!3 DO b       := !argv!3    // b/N/K
  IF argv!4 DO fraclen := !argv!4    // fraclen/N/K

  IF upb<0 DO upb := ON64 -> 10000, 1000

  IF a>0 & b<0  DO b := a + 10
  IF a<0 & b>10 DO a := b - 10

  fracbits := BITSPERBCPLWORD-4 // Use signed scaled arithmetic with one hex
                                // digit before the decimal point and 7 or 15
  	 		        // hex fractional digits after the decimal
			        // point. In either case the in the range of
			        // values is -8.0 <= x < 8.0
  One  := 1<<fracbits           // eg #x10000000 or #x1000000000000000 = 1.0
  Two  := 2*One                 // eg #x20000000 or #x2000000000000000 = 2.0
  Four := 4*One                 // eg #x40000000 or #x4000000000000000 = 4.0

  fracmask := One - 1           // eg #x0FFFFFFF or #x0FFFFFFFFFFFFFFF

  // If fraclen>0 & fraclen < fracbits the scaled arithmetic will
  // be done at lower precision using a fracmask fracbits-fraclen
  // zero bits at the leat significant end.

  IF fraclen>0 & fraclen<fracbits DO
    fracmask := fracmask - (1<<(fracbits-fraclen)) + 1

  writef("upb=%n*n", upb)
  writef("fracbits=%n fraclen=%n*n", fracbits, fraclen)
  TEST ON64
  THEN writef("facmask = %16X*n", fracmask)
  ELSE writef("facmask = %8X*n", fracmask)

  IF tofilename DO
  { tostream := findoutput(tofilename)
    UNLESS tostream DO
    { writef("Unable to open stream %s*n", tofilename)
      RESULTIS 0
    }
    selectoutput(tostream)
  }
  
  TEST a>0
  THEN { writef("Hex digits of Pi from position %n to %n*n", a, b)
         FOR i = a TO b DO
         { IF (i-a) MOD 50 = 0 DO writef("*n%i6: ", i)
           writef("%1x", pihexdig(i-1))
	   deplete(cos)
         }
         newline()
       }
  ELSE { LET hexdig = getvec(upb)
         writef("*nPi in hex*n")
         writef("*n       3.")
         hexdig!0 := 3
         FOR n = 1 TO upb DO
         { LET dig = pihexdig(n-1)
           hexdig!n := dig  // Save the hex digits in hexdig
           IF n MOD 50 = 1 DO writef("*n%5i: ", n)
           writef("%x1", dig); deplete(cos)
         }
         newline()

         writef("*nPi in decimal*n")
         writef("*n       3.")

         FOR i = 1 TO upb DO
         { IF i MOD 50 = 1 DO writef("*n%5i: ", i)
           hexdig!0 := 0          // Remove the integer part then
           mulby10(hexdig, upb)   // multiply the fraction by 10 to obtain
           writef("%n", hexdig!0) // the next decimal digit in hexdig!0
           deplete(cos)
         }
         newline()
         freevec(hexdig)
       }

  IF tostream DO endstream(tostream)
  RESULTIS 0
}

AND mulby10(v, upb) BE
{ // v contains one hex digit per element with the
  // decimal point between v!0 and v!1
  LET carry = 0
  FOR i = upb TO 0 BY -1 DO
  { LET d = v!i*10 + carry
    v!i, carry := d MOD 16, d/16
  }
}

AND pihexdig(n) = VALOF
{ // By convention, the first hex digit after the decimal point
  // is at position n=0
  LET s = 0 // A scaled number with fraclen binary digits
            // after the decimal point
  LET t = One

  //writef("*nn = %n*n", n)

  FOR i = 0 TO n-1 DO
  { LET a = muldiv(Four, powmod(16, n-i, 8*i+1), 8*i+1)
    LET b = muldiv( Two, powmod(16, n-i, 8*i+4), 8*i+4)
    LET c = muldiv( One, powmod(16, n-i, 8*i+5), 8*i+5)
    LET d = muldiv( One, powmod(16, n-i, 8*i+6), 8*i+6)

    s := s + a - b - c - d & fracmask

    //tr("a", a); tr("b", b); tr("c", c); tr("d", d); tr("s", s)
    //newline()
  }

  // Now add more terms until they are too small to matter
  { LET i = n
    WHILE t DO
    { LET a = 4 * t / (8*i+1)
      LET b = 2 * t / (8*i+4)
      LET c =     t / (8*i+5)
      LET d =     t / (8*i+6)

      s := s + a - b - c - d & fracmask

      //tr("a", a); tr("b", b); tr("c", c); tr("d", d); tr("s", s)
      //newline()

      i, t := i+1, t/16
    }
  }

  RESULTIS (s>>(fracbits-4)) & #xF
}

AND powmod(x, n, m) = VALOF
{ LET res = 1
  LET p = x MOD m
  WHILE n DO
  { UNLESS (n & 1)=0 DO res := (res * p) MOD m
    n := n>>1
    p := (p*p) MOD m
  }
  RESULTIS res
}

AND tr(str, x) BE
{ // Output scaled number x in decimal and hex
  LET d = muldiv( 1_000_000, x, One)
  LET h = muldiv(#x10000000, x, One) // Just in case fraclen is not 28
  writef("%s = %9.6d  %8x*n", str, d, h)
}
