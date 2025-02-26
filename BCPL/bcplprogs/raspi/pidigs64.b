/*
This is a version of pidig.b designed to be run under 64-bit BCPL.
It allows fraclen to be as large as 60. To run it make sure cintsys64
has been installed by typing make sys and make sys64 while in the
BCPL/cintcode directory. Then type

cintsys64
c b64 pidigs64
pidigs64

This is a BCPL implementation of the program that appears in section
10.7 of the book "Number Theory, A Programmer's Guide" by Mark
Herkommer. He uses a miraculous formula for pi discovered by David
Bailey, Peter Borwein and Simon Plouffe. The formula is

pi = the sum from n = 0 to infinity of

  (4/(8n+1) - 2/(8n+4) - 1/(8n+5) - 1/(8n+6))/(16**n)

Using modulo arithmetic, it is possible to find the nth hexadecimal
digit of pi without having to compute the others.

Herkommer's program uses double length floating point values, but mine
uses 32-bit (or 64-bit) scaled fixed point arithmetic, as a result my
version suffer rounding errors for smaller values of n. Using scaled
numbers with 28 bits after the decimal point allows this program to
compute the hex digits of pi from position 0 to 5000 correctly. It
also calculates the digits from position 100000 to 100050 correctly as
well as the digit at position one million. There is no guarantee that
all the other positions will be computed correctly since errors can
arise when long sequences of ones occur in the binary representation
of pi, and this is unpredictable. Using 64-bit BCPL and 60 fractional
bits the accuracy is far better.

This program automatically takes advantage of 64-bit precision if the BCPL
word length allows. It outputs the hex digits of Pi from position 999950
to 1000000.

Calculating digits of Pi using 64-bit BCPL and a fraclen=60

       3.
 999950: BC89273ABBCED2884ADAA7F46C59B44C28E672C29FFD342362
1000000: 6
1241.710> 


Implemented in BCPL by Martin Richards (c) July 2012 (updated October 2017)



Calculating digits of Pi using 64-bit BCPL and a fraclen=60

       3.4C28E672C29FFD342362
1000000: 6
508.490> 

*/

GET "libhdr"

GLOBAL {
  // Define the scaled arithmetic parameters
  fraclen:ug
  One
  Two
  Four
  fracmask
}

LET start() = VALOF
{ LET bperword = 1
  UNTIL (1<<bperword)=0 DO bperword := bperword+1

  fraclen := bperword-4 // Number of binary digits after the decimal point
                        // eg 28 allows numbers in the range -8.0 <= x < 8.0
  One  := 1<<fraclen    // eg #x10000000
  Two  := 2*One         // eg #x20000000
  Four := 4*One         // eg #x40000000

  fracmask := One - 1   // eg #x0FFFFFFF 

  writef("Calculating digits of Pi using %n-bit BCPL and a fraclen=%n*n",
          bperword, fraclen)

  writef("*n       3.")
  FOR n = 1000000-20 TO 1000000 DO {
  //FOR n = 0 TO 6000 DO {
    IF n MOD 50 = 0 DO writef("*n%7i: ", n)
    writef("%x1", pihexdig(n)); deplete(cos)
  }
  newline()

  RESULTIS 0
}

AND pihexdig(n) = VALOF
{ LET s = 0 // A scaled number with fraclen binary digits
            // after the decimal point
  LET t = One

  //writef("*nn = %n*n", n)

  FOR i = 0 TO n-1 DO
  { LET a = muldiv(Four, powmod(16, n-i, 8*i+1), 8*i+1)
    LET b = muldiv( Two, powmod(16, n-i, 8*i+4), 8*i+4)
    LET c = muldiv( One, powmod(16, n-i, 8*i+5), 8*i+5)
    LET d = muldiv( One, powmod(16, n-i, 8*i+6), 8*i+6)

    s := (s + a - b - c - d) & fracmask

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

      s := (s + a - b - c - d) & fracmask

      //tr("a", a); tr("b", b); tr("c", c); tr("d", d); tr("s", s)
      //newline()

      i, t := i+1, t/16
    }
  }

  RESULTIS (s>>(fraclen-4)) & #xF
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
