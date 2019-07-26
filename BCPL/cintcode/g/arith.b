/*

This file contains a library of functions to perform high precision
arithmetic. It header file is arith.h.

Implemented by Martin Richards (c) June 2016

Programs using this library normally start as follows:

GET "libhdr"
MANIFEST {
  ArithGlobs=350  // The first global used by the arith library
  numupb=2+25     // Room for the sign, the exponent and 25 radix
                  // digits, equivalent to 100 decimal digits.
}                 // Each radix digit is in the range 0 to 9999
GET "arith.h"
GET "arith.b"
...


The numbers are represented in floating point form using digits of
radix 10000 held in vectors. If N is such a number then

N!0 = TRUE  if the number is negative.
    = FALSE if the number is greater than or equal to 0.
N!1 is the exponent, ie the value of the number is
        fractional_part * 10000^(N!1).
    The exponent may be positive or negative.
n!2 to N!upb hold the fractional part with an assumed decimal point
        to the left of the first radix digit (in n!2).

If the number is in standard form either N!2 is non zero or N
represents zero with all elements of N set to zero.

All these library functions that compute numbers leave them in standard
form. While they were being debugged they called checknum for safety.

The library functions provided are as follows.

str2num(s, n,upb)      Set n to the number represented by string s
settok(k, n1,upb1)     Set n1 to a signed constant k
iszero(n1,upb1)        Return TRUE if n1 is zero
integerpart(n1,upb1)   Return the integer part of n1
prnum(n1,upb1)         Output a number followed by newline.
numcmpu(n1,upb1, n2,upb2)  Return -1 if n1<n2   Unsigned comparison
                                   0 if n1=n2
                                  +1 if n1>n2
numcmp(n1,upb1, n2,upb2)   Return the signed comparison of n1 and n2.
neg(n1,upb1)            Standardize and negate the number n1
addu(n1,upb1, n2,upb2, n3,upb3) Set n3 = ABS n1 + ABS n2
subu(n1,upb1, n2,upb2, n3,upb3) Set n3 = ABS n1 - ABS n2
                                assuming ABS n1 >= ABS n2
setzero(n1,upb1)      Set n1 to zero
add(n1,upb1, n2,upb2, n3,upb3)  Set n3 = n1 + n2
sub(n1,upb1, n2,upb2, n3,upb3)  Set n3 = n1 - n2
mulbyk(k, n1,upb1)              Multiply n1 by k, k may be negative
mul(n1,upb1, n2,upb2, n3,upb3)  Set n3 = n1 * n2
inv(n1,upb1, n2,upb2)           Set n2 = 1/n1
divbyk(k, n1,upb1)              Set n1 = n1 / k, k may be negative
div(n1,upb1, n2,upb2, n3,upb3)  Set n3 = n1 / n2
exptok(k, n1,upb1, n2,upb2)     Set n2 = n1^k
                                where k >= 0
sqrt(n1,upb1, n2,upb2)          Set n2 = sqrt n1.
copy(n1,upb1, n2,upb2)          Set n1 = n2, rounding if necessary.
copyu(n1,upb1, n2,upb2)         Copy abs n1 to abs n2 with rounding.
addcarry(n1,p)                  Increment n1 at position p
standardize(n1,upb1)  Replace n1 by n1 in standard form. That is
                      if n1 is zero all elements of n1 are set to zero
                      otherwise the fractional part is shifted left
                      until n1!2 is non zero correcting the exponent
                      appropriately.
radius((n1,upb1, n2,upb2, n3,upb3, n4,upb4)  Set n4 to the square root
                                             of n1^2+n2^2+n3^2.
inprod(dir1,upb1, dir2,upb2, n3,upb3) Set n3 to the inner product of
                                      the 3D vectors dir1 and dir2. The
                                      components of dir1 are of size upb1,
                                      and those of dir2 are of size upb2.
crossprod(dir1,upb1, dir2,upb2, dir3,upb3) Set dir3 to the cross product of
                                      the 3D vectors dir1 and dir2. The
                                      components of dir1, dir2 and dir3
                                      are upb1, upb1 and upb1, respectively.
normalize((n1,upb1, n2,upb2, n3,upb3)  Replace each of n1, n2 and n3 by
                                the result of dividing them by r where
                                r is radius(n1,upb1, n2,upb2, n3,upb3).
checknum(n1,upb1)     Check that n1 is a valid stadardized number.
*/

LET str2num(s, n1, upb1) = VALOF
{ LET p = 0     // Count of decimal digits not including
                // leading zeroes
  LET fp   = -1 // Count of decimal digits after the decimal point, if any.
  LET pos  =  2 // Position of next radix digit
  LET dig  =  0 // To hold the next radix digit
  LET dexp =  ?
  LET e = 0     // For the exponent specified by En
//writef("str2num: 1 n1!(upb1+1)=%n*n", n1!(upb1+1))
  n1!0 := -2    // No sign yet
  FOR i = 1 TO upb1 DO n1!i := 0
//writef("str2num: 2 n1!(upb1+1)=%n*n", n1!(upb1+1))
  
  FOR i = 1 TO s%0 DO
  { LET ch = s%i

    SWITCHON ch INTO
    { DEFAULT: RESULTIS FALSE

      CASE ' ': LOOP // Ignore spaces

      CASE '-': UNLESS n1!0=-2 RESULTIS FALSE
                n1!0 := TRUE
                LOOP

      CASE '+': UNLESS n1!0=-2 RESULTIS FALSE
                n1!0 := FALSE
                LOOP

      CASE '.': IF fp>=0 RESULTIS FALSE // Invalid decimal point
                fp := 0  // Count of digits after the decimal point.
                LOOP
      CASE 'E':
      CASE 'e': { // Read a possibly signed exponent leaving
                  // its value in e.
                  LET nege = -2
                  FOR j = i+1 TO s%0 DO
                  { ch := s%j
                    SWITCHON ch INTO
                    { DEFAULT:  RESULTIS FALSE

                      CASE ' ': LOOP // Ignore spaces

                      CASE '-': UNLESS nege=-2 RESULTIS FALSE
                                nege := TRUE
                                LOOP 

                      CASE '+': UNLESS nege=-2 RESULTIS FALSE
                                nege := FALSE
                                LOOP

                      CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
                      CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
                                IF nege=-2 DO nege := FALSE
                                e := 10*e + ch - '0'
                                LOOP
                    }
                  }
                  IF nege DO e := -e
                  BREAK
                }

      CASE '0': IF p=0 DO
                { // No significant decimal digits yet
                  // If sign unset make it positive
                  IF n1!0=-2 DO n1!0 := FALSE
                  IF fp>=0 DO fp := fp+1
                  LOOP
                }

      CASE '1':CASE '2':CASE '3':CASE '4':
      CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
                { // A significant digit
                  // If sign unset make it positive
                  IF n1!0=-2 DO n1!0 := FALSE 
                  p := p+1     // Increment count of significant digits
                  IF fp>=0 DO fp := fp+1 // Count of fractional digit
                  dig := 10*dig + ch - '0'
                  IF p MOD 4 = 0 DO
                  { // Just completed a radix digit
                    // Store it digit, if possible
                    IF pos<=upb1 DO n1!pos := dig
//writef("str2num: 3 n1!(upb1+1)=%n*n", n1!(upb1+1))
                    dig := 0
                    pos := pos+1
                  }
                  LOOP
                }
    }
  }

  IF p=0 DO
  { // No significant digits, so the result is zero.
    setzero(n1,upb1)
//writef("str2num: 4 n1!(upb1+1)=%n*n", n1!(upb1+1))
    RESULTIS TRUE
  }

  // Place a decimal point here if not already present.
  IF fp<0 DO fp := 0

  // Pad last radix digit by adding fractional decimal zeroes.

  UNTIL p MOD 4 = 0 DO
  { dig := dig * 10
    p := p+1
    IF fp >= 0 DO fp := fp+1
  }

  // Store the last digit, if room.
  IF pos<= upb1 DO n1!pos := dig
//writef("str2num: 5 pos=%n dig=%z4 n1!(upb1+1)=%n*n", pos, dig, n1!(upb1+1))

  // n1!2 contains 4 decimal digits including the padding zeroes.

  dexp := p-fp  // Decimal exponent

  // p  is the number of decimal digits including padding.
  // p  is a multiple of 4.
  // fp is the number of fractional decimal digits including padding.

  // We require dexp to be a multiple of 4, so
  // until dexp is a multiple of 4, increment dexp and divide
  // the fractional value by 10.
  
  UNTIL dexp MOD 4 = 0 DO
  { divbyk(10, n1,upb1)
//writef("str2num: 6 n1!(upb1+1)=%n*n", n1!(upb1+1))
    dexp:= dexp+1
  }

  // The decimal exponent dexp is now a multiple of 4.

  n1!1 := dexp/4  // Set the radix exponent.

  // Now add in the En exponent
  n1!1 := n1!1 + e

  //checknum(n1,upb1)
  RESULTIS TRUE
}

AND settok(k, n1,upb1) = VALOF
{ // k must be in range -9999 to +9999
  // Return TRUE is k is in range
  setzero(n1,upb1)
  IF k=0 RESULTIS TRUE
  IF k<0 DO n1!0, k := TRUE, -k
  IF k>=10000 RESULTIS FALSE
  n1!1, n1!2 := 1, k
  RESULTIS TRUE
}

AND integerpart(n1,upb1) = VALOF
{ // Only two radix digits of integer part is allowed,
  // so the correct result is in the range -9999_9999 to +9999_9999.
  // If the integer part is out of range the result is
  // +1_0000_0000 or -1_0000_0000.
  LET e = n1!1
  LET x = n1!2 * 10000
  IF upb1>=3 DO x := x + n1!3
  IF e > 2 DO x := 1_0000_0000
  IF n1!0 DO x := -x
  IF e <= 0  RESULTIS 0
  IF e  = 1  RESULTIS x / 10000
  RESULTIS x
}

AND roundtoint(n1,upb1) = VALOF
{ // Return an integer representing the first 8 decimal digits
  // after the decimal point before rounding. The round n1 to the
  // nearest setting the fractional part to zero.
  LET e = n1!1   // The exponent
  LET frac = 0

  IF e > 0 DO
  { // The integer part is non zero

    // e > 0
    LET p = e+2 // Position of the first fractional radix digit
    LET carry = p<upb1 & n1!p>=5000 -> 1, 0
    IF p <= upb1 DO frac := n1!p * 10000    // Fractional digits 1 to 4
    IF p <  upb1 DO frac := frac + n1!(p+1) // Fractional digits 5 to 8

    FOR i = p TO upb1 DO n1!i := 0
    IF carry DO addcarry(n1,p-1)

    //checknum(n1,upb1)
    RESULTIS frac
  }

  // The unrounded integer part is zero, so the rounded
  // integer part is zero or 1.
  // e <= 0
  IF e= 0   DO frac := n1!2*10000 + n1!3 // 8 fractional digits
  IF e=-1   DO frac := n1!2              // 4 fractional digits

  TEST e=0 & frac >= 50000000
  THEN settok(1, n1,upb1)   // n1 was in range 0.5 to 0.99999999
  ELSE setzero(n1,upb1)     // n1 was in range 0.0 to 0.49999999

  //checknum(n1,upb1)
  RESULTIS frac}

AND prnum(n, upb) BE
{ // Output a number n of size upb, followed by a newline().
  writef("%c0.", n!0->'-','+')
  FOR i = 2 TO upb DO
  { writef("%z4 ", n!i)
    IF (i-2) MOD 10 = 9 DO writef("*n   ")
  }
  IF n!1 DO writef("E%n", n!1)
  newline()
}

AND numcmpu(n1,upb1, n2,upb2) = VALOF
{ // Return  1 if abs n1 > abs n2
  // Return  0 if abs n1 = abs n2
  // Return -1 if abs n1 < abs n2
  // n1 and n2 are assumed to be in standard form.
  LET upb = upb1<=upb2 -> upb1, upb2
  // upb is the smaller upper bound

  // Deal with the cases when n1 or n2 is zero.
  IF n1!2=0 DO
  { IF n2!2=0 RESULTIS 0  // n1= 0  n2= 0
    RESULTIS -1           // n1= 0  n2~=0
  }
    
  IF n2!2=0 RESULTIS 1    // n1~=0  n2= 0

  // Neither n1 nor n2 is zero
  FOR i = 1 TO upb DO
  { // Compare the exponents and digit os n1 and n2.
    LET a, b = n1!i, n2!i
    IF a > b RESULTIS  1  // n1 > n2
    IF a < b RESULTIS -1  // n1 < n2
  }

  IF upb1=upb2 RESULTIS 0

  TEST upb1>upb
  THEN FOR i = upb+1 TO upb1 IF n1!i RESULTIS  1
  ELSE FOR i = upb+1 TO upb2 IF n2!i RESULTIS -1

  RESULTIS 0
}

AND numcmp(n1,upb1, n2,upb2) = VALOF
{ // Return  1 if n1 > n2
  // Return  0 if n1 = n2
  // Return -1 if n1 < n2
  // n1 and n2 are assumed to be in standard form.
  IF n1!0 DO
  { IF n2!0 RESULTIS - numcmpu(n1,upb1, n2,upb2) // n1< 0, n2< 0
    RESULTIS -1                                  // n1< 0, n2>=0
  }

  IF n2!0 RESULTIS 1                             // n1>=0, n2< 0

  RESULTIS numcmpu(n1,upb1, n2,upb2)             // n1>=0  n2>=0
}

AND neg(n1,upb1) = VALOF
{ // Standardize n1 and the set n1 to -n1
  standardize(n1,upb1)
  IF n1!2 DO n1!0 := ~ n1!0
  RESULTIS TRUE
}

AND iszero(n1,upb1) = VALOF
{ // n1 is zero if all the fraction digits are zero
  FOR i = 2 TO upb1 UNLESS n1!i=0 RESULTIS FALSE
  RESULTIS TRUE
}

AND addu(n1,upb1, n2,upb2, n3,upb3) = VALOF
{ // Set n3 to abs n1 + abs n2 rounded
  // n1 and n2 are assumed to be in standard form.
  LET carry = 0
  LET t1,u1 = n1,upb1 // To hold the number with the larger exponent
  LET t2,u2 = n2,upb2 // To hold the number with the smaller exponent
  LET offset = ?
  LET p, q = ?, ?
  LET tmp = VEC numupb

  //checknum(n1,upb1)
  //checknum(n2,upb2)

  IF n1!2=0 RESULTIS copyu(n2,upb2, n3,upb3)
  IF n2!2=0 RESULTIS copyu(n1,upb1, n3,upb3)
  
  // Neither n1 nor n2 are zero.

  IF n1!1<n2!1 DO // Compare their exponents
  { t1,u1 := n2,upb2
    t2,u2 := n1,upb1
  }

  // t1!1 >= t2!1     So t1 has the larger exponent

  offset := t1!1-t2!1
  // offset is >= 0
  // It is the amount t2 must be shifted before adding to t1

  p := u2         // Position of the last digit of t2
  q := u2+offset  // Position in tmp of where to add it.

  IF q > numupb DO
  { // Reduce both p and q so the q=numupb
    p := p - (u2+offset-numupb)
    q := numupb
  }

  // Form the sum in tmp
  copyu(t1,u1, tmp,numupb)
//writef("p=%i2 q=%i2 tmp= ", p, q); prnum(tmp,numupb)

  // Add t2 suitably shifted into tmp
  WHILE p >= 2 DO
  { LET x = tmp!q + t2!p + carry
    tmp!q := x MOD 10000
    carry := x  /  10000
//writef("p=%i2 q=%i2 tmp= ", p, q); prnum(tmp,numupb)
//writef("carry=%n*n", carry)
    p, q := p-1, q-1
  }

  // There are no more digits of t2, but the may still be a carry
  // to deal with
  WHILE carry & q >= 2 DO
  { LET x = tmp!q + carry
    tmp!q := x MOD 10000
    carry := x  /  10000
//writef("q=%i2 tmp= ", q); prnum(tmp,numupb)
    q := q-1
  }

  // If there is a carry out of the senior digit, tmp must be
  // shifted right and the exponent corrected.
  IF carry DO
  { // Shift the radix digits to the right by one position.
    FOR i = numupb-1 TO 2 BY -1 DO tmp!(i+1) := tmp!i
    tmp!2 := carry
    tmp!1 := tmp!1 + 1 // Adjust the exponent
//writef("tmp= "); prnum(tmp,numupb)
  }

  copy(tmp,numupb, n3,upb3) // Set n3 = tmp rounded
//writef("n3=  "); prnum(n3,upb3)
//abort(1000)

  //checknum(n3,upb3)
  RESULTIS TRUE
}

AND subu(n1,upb1, n2,upb2, n3,upb3) = VALOF
{ // Set abs n3 = abs n1 - abs n2 with rounding
  // It is assumed that abs n1 >= abs n2
  // The sign of n3 (n3!0) will be positive.
  LET borrow = 0
  LET t1,u1 = n1,upb1 // To hold the number with the larger exponent
  LET t2,u2 = n2,upb2 // To hold the number with the smaller exponent
  LET offset = ?
  LET p, q = ?, ?
  LET tmp = VEC numupb

  //checknum(n1,upb1)
  //checknum(n2,upb2)

  IF n2!2=0 RESULTIS copyu(n1,upb1, n3,upb3)

  IF n1!2=0 DO
  { // Since abs n1 >= abs n2 and n1=0 the so does n2.
    setzero(n3,upb3)
    RESULTIS TRUE
  }

  // Neither n1 nor n2 are zero.

  // Since abs n1 >= abs n2 and they are both non zero,
  // the exponent of n1 must be >= exponent of n2

  // is n1!1 >= n2!1     So t1 has the larger exponent

  offset := n1!1-n2!1
  // offset is >= 0
  // It is the amount n2 must be shifted before adding to n1

  p := upb2         // Position of the last digit of n2
  q := upb2+offset  // Position in tmp of where to bubtract it.

  IF q > numupb DO
  { // Reduce both p and q so the q=numupb
    p := p - (u2+offset-numupb)
    q := numupb
  }

  // Form the difference in tmp
  copyu(t1,u1, tmp,numupb)
//writef("p=%i2 q=%i2 tmp= ", p, q); prnum(tmp,numupb)

  // Subtract n2 suitably shifted from tmp
  WHILE p >= 2 DO
  { LET x = tmp!q - borrow - t2!p
    borrow := 0
    IF x < 0 DO borrow, x := 1, x + 10000
    tmp!q := x
//writef("p=%i2 q=%i2 tmp= ", p, q); prnum(tmp,numupb)
//writef("borrow=%n*n", borrow)
    p, q := p-1, q-1
  }

  // There are no more digits of n2, but the may still be a borrow
  // to deal with
  WHILE borrow & q >= 2 DO
  { LET x = tmp!q - borrow
    borrow := 0
    IF x < 0 DO borrow, x := 1, x + 10000
    tmp!q := x
//writef("q=%i2 tmp= ", q); prnum(tmp,numupb)
    q := q-1
  }

  IF borrow DO
  { // There was a borrow out of the senior radix digit.
    // This is a system error since abs n1 is >= abs n2.
    writef("SYSTEM ERROR: in subu*n")
    abort(999)
    RESULTIS FALSE
  }

  standardize(tmp,numupb)

  copy(tmp,numupb, n3,upb3) // Set n3 = tmp rounded
//writef("tmp=  "); prnum(tmp,numupb)
//writef("copy(tmp,%n, n3,%n)*n", numupb, upb3)
//writef("n3=  "); prnum(n3,upb3)
//abort(1000)

  //checknum(n3,upb3)
  RESULTIS TRUE
}

AND setzero(n1, upb1) BE FOR i = 0 TO upb1 DO n1!i := 0

AND add(n1,upb1, n2,upb2, n3,upb3) = VALOF
{ // Add signed numbers n1 and n2 placing the rounded result in n3

  LET rc = ?
  LET t = VEC numupb

  //checknum(n1,upb1)
  //checknum(n2,upb2)

  IF n1!2=0 DO
  { copy(n2,upb2, n3,upb3) // n1 is zero
    RESULTIS TRUE
  }

  IF n2!2=0 DO
  { copy(n1,upb1, n3,upb3) // n2 is zero
    RESULTIS TRUE
  }

  // Neither n1 nor n2 are zero

  IF n1!0=n2!0 DO
  { // eg +5 + +3 =>  +  (5+3)
    // eg -3 + -5 =>  -  (5+3)
    // So add the absolute values and then set the sign
    rc := addu(n1,upb1, n2,upb2, n3,upb3)
    UNLESS n3!2=0 DO n3!0 := n1!0
    RESULTIS rc
  }

//writef("The signs of n1 and n2 are different, n1!0=%n n2!0=%n*n", n1!0, n2!0)
  // The signs are different
  rc := numcmpu(n1,upb1, n2,upb2)

  IF rc=0 RESULTIS setzero(n3,upb3)

  TEST n1!0
  THEN TEST rc>0
       THEN { // eg -5 + +3 =>  -  (5-3)
//writef("n1<=0 n2>0 and abs n1 > abs n2*n")
//writef("Calling subu(n1,%n, n2,%n, n3,%n)*n", upb1, upb2, upb3)
              rc := subu(n1,upb1, n2,upb2, n3,upb3)
              UNLESS n3!2=0 DO n3!0 := TRUE
              RESULTIS rc
            }
       ELSE { // eg -3 + +5 =>  +  (5-3)
//writef("n1<=0 n2>0 and abs n1 < abs n2*n")
              rc := subu(n2,upb2, n1,upb1, n3,upb3)
              n3!0 := FALSE
              RESULTIS rc
            }
  ELSE TEST rc>0
       THEN { // eg +5 + -3 =>  +  (5-3)
              rc := subu(n1,upb1, n2,upb2, n3,upb3)
              n3!0 := FALSE
              RESULTIS rc
            }
       ELSE { // eg +3 + -5 =>  -  (5-3)
              rc := subu(n2,upb2, n1,upb1, n3,upb3)
              UNLESS n3!2=0 DO n3!0 := TRUE
              RESULTIS rc
            }
}

AND sub(n1,upb1, n2,upb2, n3,upb3) = VALOF
{ // Subtract n2 from n1 using signed arithmetic placing
  // the rounded result in n3.
  LET rc = ?
  LET n2sign = n2!0

  //checknum(n1,upb1)
  //checknum(n2,upb2)

  IF n2!2=0 DO
  { 
    //writef("sub: n2!2=0 so calling copy(n1,%n,n3,%n)*n", upb1,upb3)
    //writef("n1= "); prnum(n1,upb1)
//abort(7654)
    copy(n1,upb1, n3,upb3) // n2 is zero
    //writef("n3= "); prnum(n3,upb3)
//abort(7655)
    RESULTIS TRUE
  }

  // n2 is non zero
  //newline()
  //writef("sub(n1,%n, n2,%n, n3,%n)*n", upb1, upb2, upb3)
  //writef("n1= "); prnum(n1,upb1)
  //writef("n2= "); prnum(n2,upb2)
  n2!0 := ~ n2!0 // Negate n2
  //newline()
  //writef("Negating n2*n")
  //writef("n2= "); prnum(n2,upb2)

  //newline()
  //writef("Calling add(n1,upb1, n2,upb2, n3,upb3)*n")
  //writef("n1= "); prnum(n1,upb1)
  //writef("n2= "); prnum(n2,upb2)

  rc := add(n1,upb1, n2,upb2, n3,upb3)
//writef("After calling add*n")
  //writef("n3= "); prnum(n3,upb3)
//checknum(n3,upb3)
  n2!0 := n2sign // Restore the sign of n2
//writef("Restoring sign of n2*n")
  //writef("n1= "); prnum(n1,upb1)
  //writef("n2= "); prnum(n2,upb2)
//writef("In sub after call of add*n")

  // No need to call checknum since add has already done so.
  //writef("n3= "); prnum(n3,upb3)
  standardize(n3,upb3)
//writef("After calling standardize(n3,upb3)*n")
  //writef("n3= "); prnum(n3,upb3)
  //checknum(n3,upb3)
  RESULTIS rc
}


AND mul(n1,upb1, n2,upb2, n3,upb3) = VALOF
{ LET sign = n1!0 XOR n2!0       // Set the sign of the result
  LET e1 = n1!1
  LET e2 = n2!1
  LET exponent = e1 + e2         // Initial exponent
  LET carry = ?
  LET t1 = VEC numupb

  IF iszero(n1,upb1) | iszero(n2,upb2) DO
  { setzero(n3,upb3)
    RESULTIS TRUE
  }

//writef("*n*nmul: entered*n")
//writef("n1=    "); prnum(n1,upb1)
//writef("n2=    "); prnum(n2,upb2)

  setzero(t1,numupb)

  // Neither n1 nor n2 are zero.

  // Set the exponents of n1 and n2 to zero
  n1!1, n2!1 := 0, 0
  // Form the product n1*n2 in t1.
  // Both n1 and n2 are less than 1.0 so the product will be less than 1.0.
  FOR i = 2 TO upb1 DO
  { // Take each digit of n1
    LET n1i = n1!i
    IF n1i DO
    { LET p, x = ?, ?
      LET jlim = numupb+1-i
      IF jlim > upb2 DO jlim := upb2
      // j is in the range 2 to upb2, but the destination
      // position i+j-1 must be <= numupb, so
      // i+j-1 <= numupb  ie j <= numupb+1-i
      // j must also be <= upb2
//writef("i=%i2 jlim=%i2 upb1=%i2 upb2=%i2 numupb=%i2*n", i, jlim, upb1, upb2, numupb)
//writef("n1!i= %z4*n", n1i)
      FOR j = jlim TO 2 BY -1 DO
      { p := i + j - 1    // The destination position
        t1!p := t1!p + n1i * n2!j
//writef("%i2 %i2: t1= ", j, p); prnum(t1,numupb)
//abort(2000)
      }
//abort(1000)

//writef("Do carryingn*n")
      carry := 0
      FOR j = numupb TO 1 BY -1 DO
      { LET x = t1!j + carry
//writef("carrying j=%n*n", j)
        t1!j  := x MOD 10000
        carry := x  /  10000
//writef("%i2 t1= ", j); prnum(t1,numupb)
//abort(1001)
      }
IF carry DO abort(987)
//writef("t1=    "); prnum(t1,numupb)
//abort(1002)
    }
  }

  t1!0 := sign
  t1!1 := t1!1 + exponent
//writef("t1=    "); prnum(t1,numupb)
  standardize(t1,numupb)
//writef("t1=    "); prnum(t1,numupb)
//abort(4444)
  //writef("Set n3 to t1 rounded*n")

  copy(t1,numupb, n3,upb3)

  //writef("n1**n2= "); prnum(n3,upb3)

  // Restore the exponents of n1 and n2
  n1!1, n2!1 := e1, e2

//abort(985)
  //checknum(n3,upb3)
  RESULTIS TRUE
}

AND mulbyk(k, n1,upb1) = VALOF
{ LET sign = n1!0
  LET carry = 0

  standardize(n1,upb1)

  IF n1!2=0 RESULTIS TRUE

  IF k=0 DO
  { setzero(n1,upb1)
    RESULTIS TRUE
  }

  IF k<0 DO n1!0, k := ~n1!0, -k

  // The result sign is correct and k is non zero.
  // Multiply digits from the least significant end
  // dealing with carry.
  FOR i = upb1 TO 2 BY -1 DO
  { LET x = n1!i * k + carry
    n1!i  := x MOD 10000
    carry := x  /  10000
  }

  IF carry DO
  { // Shift the fractional part to the right one place
    // and adjust the exponent.
    LET lsdig = n1!upb1
    FOR i = upb1-1 TO 2 BY -1 DO n1!(i+1) := n1!i
    n1!1 := n1!1 + 1
    n1!2 := carry
    IF lsdig >= 5000 DO addcarry(n1,upb1)
  }

  //checknum(n1,upb1)
  RESULTIS TRUE
}

AND inv(n1,upb1, n2,upb2) = VALOF
{ // Standardize n1 if necessary, then if n1 is zero return FALSE,
  // otherwise standardize set n2 to the inverse of n1.
  // upb1 is assumed to be > 2.
  LET one = TABLE FALSE, 1, 0001   // The number +1.0
  LET sign = n1!0    // The sign of n1
  LET e = ?          // To hold the exponent of n1
  LET elim = -(numupb - 2 - numupb/4)

  LET t1 = VEC numupb
  AND t2 = VEC numupb
  AND t3 = VEC numupb
  AND t4 = VEC numupb

  //checknum(n1,upb1)

  IF n1!2=0 RESULTIS FALSE // Cannot take the inverse of zero.

  IF numcmp(one,2, n1,upb1)=0 DO
  { settok(1, n2,upb2) // The inverse of 1.0 is 1.0.
    RESULTIS TRUE
  }

  e := n1!1
  n1!0, n1!1 := FALSE, 0 // Set n1 to be in the range +0.0001 to +1.0000

  // Select initial guess
  { LET w = n1!2 * 1_0000 + n1!3
    // 10000 <= w <= 99999999
    LET a = muldiv(9999_9999, 1_0000, w)
    //writef("w=%z8  a=%z8*n", w, a)
    setzero(t1,numupb)
    t1!0 := FALSE         // Set positive
    t1!1 := 1             // Set the exponent
    t1!2 := a  /  1_0000  // and two radix digits
    t1!3 := a MOD 1_0000  // of the initial guess.
  }

  //writef("n1=            "); prnum(n1,upb1)
  //writef("initial guess= "); prnum(t1,numupb)

  { // Start of Newton-Raphson loop

again:
    //newline()
    //writef("Current guess for 1/n1 is in t1*n")
    //writef("n1= "); prnum(n1,upb1)
    //writef("t1= "); prnum(t1,numupb)
//abort(2200)
    mul(t1,numupb, n1,upb1, t2,numupb)  // Set t2 = t1*n1
    //newline()
    //writef("Set t2 = t1**n1*n")
    //writef("t1= "); prnum(t1,numupb)
    //writef("n1= "); prnum(n1,upb1)
    //writef("t1**n1= "); prnum(t2,numupb)
//abort(1001)

    //writef("*nCalling sub(one,2, t2,numupb, t3,numupb)*n")
    sub(one,2, t2,numupb, t3,numupb)    // Set t3 := 1 - t1*n1
    //writef("Set t3 to 1 - t2,   where t2 = t1**n1*n")
    //writef("one="); prnum(one,2)
    //writef("t2= "); prnum(t2,numupb)
    //writef("t3= "); prnum(t3,numupb)
//abort(1002)

    mul(t1,numupb, t3,numupb, t2,numupb)
    //writef("Set t2 = t1 ** (1 - t1**n1)*n")
    //writef("t1=    "); prnum(t1,numupb)
    //writef("t3=    "); prnum(t3,numupb)
    //writef("t2=    "); prnum(t2,numupb)
//abort(1003)

    add(t1,numupb, t2,numupb, t3,numupb)
    //writef("Set t3 = t1 + t1 ** (1 - t1**n1)*n")
    //writef("t3=    "); prnum(t3,numupb)
//abort(1004)

    //newline()
    //writef("n1=    "); prnum(n1,upb1)
    //writef("Previous guess for 1/n1 is in t1*n")
    //writef("t1=    "); prnum(t1,numupb)
    //writef("New guess for 1/n1 is in t3*n")
    //writef("t3=    "); prnum(t3,numupb)
//abort(1005)

    //newline()
    //writef("Compare the new and previous approximations*n")
    //writef("Leave the loop for inv if close enough*n")
    IF t3!1>100 DO
    { // The iteration for 1/n1 has diverged
      newline()
      writef("The iteration for 1/n1 has diverged*n")
      writef("n1=    "); prnum(n1,upb1)
      abort(999)
    }
//abort(7777)
    sub(t3,numupb, t1,numupb, t2,numupb)
    //writef("diff= "); prnum(t2,numupb)
    //writef("diff!1=%n  elim=%n*n", t2!1, elim)
    UNLESS iszero(t2,numupb) DO
    { IF t2!1 > elim DO
      { copy(t3,numupb, t1,numupb)
        //writef("It was not close enough so copy t3 to t1 reenter loop*n")
//abort(1006)

        GOTO again 
      }
    }
  }

  //writef("It was close enough so copy t3 to n2 and return from inv*n")
  //writef("e=%n  1-e=%n*n", e, 1-e)

  t3!0, t3!1 := sign, 1-e  // Set the sign and exponent of the result
  //writef("t3= "); prnum(t3,numupb)
//writef("Calling copy(t3,numupb, n2,upb2)*n")
  copy(t3,numupb, n2,upb2)
  //writef("n2= "); prnum(n2,upb2)
//abort(1007)

  n1!0, n1!1 := sign, e    // Restore the sign and exponent of n1

  //checknum(n2,upb2)
  RESULTIS TRUE
}

AND divbyk(k, n1,upb1) = VALOF
{ LET sign, carry = ?, 0
  LET e = n1!1

  standardize(n1,upb1)

//writef("divbyk k=%n*n", k)
//writef("n1=  "); prnum(n1,upb1)

  IF k=0    RESULTIS FALSE
  IF n1!2=0 RESULTIS TRUE

  sign := n1!0

  IF k<0 DO sign, k := ~sign, -k

  FOR i = 1 TO upb1-1 DO
  { LET x = carry*10000 + n1!(i+1)
    n1!i  := x  /  k
    carry := x MOD k
  }
  n1!upb1 := carry

  TEST n1!1
  THEN FOR i = upb1-1 TO 1 BY -1 DO n1!(i+1) := n1!i
  ELSE e := e-1

  n1!0 := sign
  n1!1 := e
//writef("n1=  "); prnum(n1,upb1)
//abort(9977)

  //checknum(n1,upb1)
  RESULTIS TRUE
}

AND div(n1,upb1, n2,upb2, n3,upb3) = VALOF
{ // This is a Newton-Raphson iterative version of division to
  // calculate 1/n2. It then multiplies 1/n2 by n1.

  LET t1 = VEC numupb
  LET t2 = VEC numupb

  //checknum(n1,upb1)
  //checknum(n2,upb2)

  IF n1!2=0 DO
  { setzero(n3,upb3)
    RESULTIS TRUE
  }

  IF n2!2=0 RESULTIS FALSE // Cannot divide by zero

  // Calculate 1/n2 using the interation x(n+1) = xn + xn*(1 - n2*xn)
  inv(n2,upb2, t1,numupb)

  //writef("n2=      "); prnum(n2,upb2)
  //writef("t1=1/n2= "); prnum(t1,numupb)
  // Multiply 1/n2 by t1

//writef("Calling mul(n1,upb1, t1,numupb, t2,numupb)*n")
  mul(n1,upb1, t1,numupb, t2,numupb)
  //writef("n1 ** 1/n2=  "); prnum(t2,numupb)

//writef("Calling copy(t2,numupb, n2,upb2)*n")
  copy(t2,numupb, n3,upb3)
  //writef("t2=    "); prnum(t2,numupb)
  //writef("n3=    "); prnum(n3,upb3)
//abort(998)

  //checknum(n3,upb3)
  RESULTIS TRUE
}

AND sqrt(n1,upb1, n2,upb2) = VALOF
{ // Set n2 to the square root of n1.
  LET rc, prevrc = ?, -2
  LET e = ?
  LET elim = -(numupb - 2 - numupb/4)
  LET t1  = VEC numupb
  AND t2  = VEC numupb
  AND t3  = VEC numupb

  // Use interation: x(n+1) = (xn + n1/xn)/2

  //writef("*nsqrt of "); prnum(n1,upb1)

  setzero(n2,upb2)

  IF iszero(n1) RESULTIS TRUE   // sqrt(0) = 0

  standardize(n1,upb1)
  // n1!2 will ceratinly be non zero

  IF n1!0 RESULTIS FALSE  // n1 must be positive

  e := n1!1  // Remember the exponent of n1 in e
  n1!1 := 0  // Cause n1 to be in range 0001 to 9999

  // n1 is greater than zero

  { // Choose a reasonable initial guess
    LET a = n1!2 * 10000 + n1!3 // 0001 <= a <= 9999_9999
    LET guess = 100_0000

    UNTIL muldiv(guess, guess, 1_0000_0000) >= a DO
      guess := guess + guess

//writef("guess = %13.8d  a = %13.8d e=%n*n", guess, a, e)
    guess := (guess + muldiv(a, 1_0000_0000, guess))>>1
//writef("guess = %13.8d*n", guess)
    guess := (guess + muldiv(a, 1_0000_0000, guess))>>1
//writef("guess = %13.8d*n", guess)
    guess := (guess + muldiv(a, 1_0000_0000, guess))>>1
//writef("guess = %13.8d*n", guess)
    guess := (guess + muldiv(a, 1_0000_0000, guess))>>1
//writef("guess = %13.8d*n", guess)

    // Place the initial guess in t1
    setzero(t1,numupb)
    t1!2, t1!3 := guess/10000, guess MOD 10000
  }

setzero(t2,numupb)
setzero(t3,numupb)

  { // Start of Newton=Raphson sqrt loop
again: 
    //writef("*nsqrt: Current guess for sqrt in t1*n")
    //writef("t1=  "); prnum(t1,numupb)
    //writef("n1=  "); prnum(n1,upb1)
//abort(1001)  
    div(n1,upb1, t1,numupb, t2,numupb) // t2 = n1/t1
    //writef("Set t2 = n1/t1*n")
    //writef("n1=  "); prnum(n1,upb1)
    //writef("t1=  "); prnum(t1,numupb)
    //writef("t2=  "); prnum(t2,numupb)

    add(t1,numupb, t2,numupb, t3,numupb) // t3 = t1 + n1/t1
    //writef("Set t3 = t1 + n1/t1*n")
    //writef("t3=  "); prnum(t3, numupb)

    divbyk(2, t3,numupb) // Set t3 := (t1 + n1/t1)/2
    //writef("Set t3 = (t1 + n1/t1)/2*n")
    //writef("t3=   "); prnum(t3,numupb)

    //writef("Compare t3 with the previous t1*n")
    //writef("Leave the loop if change is small enough*n")

    sub(t3,numupb, t1,numupb, t2,numupb)

    //writef("t1=  "); prnum(t1, numupb)
    //writef("t3=  "); prnum(t3, numupb)
    //writef("diff="); prnum(t2, numupb)

    UNLESS iszero(t2,numupb) DO
    { //writef("the difference was non zero*n")
      //writef("t3!1 + %n = %n*n", elim, t3!1 + elim) 
      //writef("t2!1 = %n*n", t2!1) 

      IF t3!1 + elim < t2!1 DO 
      { copy(t3,numupb, t1,numupb)
        //writef("Do the iteration again*n")
//abort(7778)
        GOTO again
      }
    }    
  }

  //writef("Iteration complete*n")

  t3!1 := e>=0 -> (e+1)/2, (e-1)/2
  n1!1 := e  // Restore the exponent of n1
  UNLESS (e&1)=0 TEST e>0 THEN divbyk(100, t3,numupb)
                          ELSE mulbyk(100, t3,numupb)

  // Test the accuracy of the square root.
  //writef("t3=   "); prnum(t3,numupb)
  //mul(t3,numupb, t3,numupb, t2,numupb)
  //writef("t3^2= "); prnum(t2,numupb)
  //writef("n1=   "); prnum(n1,upb1)
//abort(7777)

  copy(t3,numupb, n2,upb2)

  //mul(n2,upb2, n2,upb2, t2,numupb)
  //writef("n1=   "); prnum(n1,upb1)
  //writef("n2=   "); prnum(n2,upb2)
  //writef("n2^2= "); prnum(t2,numupb)
  //newline()
//abort(8888)

  //checknum(n2,upb2)
  RESULTIS TRUE
}

AND copy(n1,upb1, n2,upb2) = VALOF
{ // Set n2 = n1 rounded.
  LET p = upb1
  IF p > upb2 DO p := upb2

  FOR i =   0 TO   p  DO n2!i := n1!i
  FOR i = p+1 TO upb2 DO n2!i := 0     // Pad with zeroes

  IF p>upb2 & n1!(upb2+1) >= 5000 DO addcarry(n2,p)

  IF n2!2=0 RESULTIS standardize(n2,upb2)
  //checknum(n2,upb2)
  RESULTIS TRUE
}

AND copyu(n1,upb1, n2,upb2) = VALOF
{ // Set n2 = ABS n1 rounded.
  // The sign of the result is always positive.
  LET p = upb1
  IF p > upb2 DO p := upb2

  FOR i =   1 TO   p  DO n2!i := n1!i
  FOR i = p+1 TO upb2 DO n2!i := 0     // Pad with zeroes

  IF p>upb2 & n1!(upb2+1) >= 5000 DO addcarry(n2,p)

  n2!0 := FALSE  // Set the result to be positive
  IF n2!2=0 RESULTIS standardize(n2,upb2)
  //checknum(n2,upb2)
  RESULTIS TRUE
}

AND addcarry(n1,p) = VALOF
{ // Increment n1 at position p.
  // The digits from p+1 to the end of n1 are already all zero.
  FOR i = p TO 2 BY -1 DO
  { LET x = n1!i
    UNLESS x = 9999 DO { n1!i := x+1; RESULTIS TRUE }
    n1!i := 0
  }

  // There is a carry out of the senior digit position.
  // This can only happen if 1 was added to 9999 9999 .. 9999
  // so n1!2 to n1!p are all zero.
  n1!2 := 0001
  n1!1 := n1!1 + 1 // Correct the exponent

  //checknum(n1,p)
  RESULTIS TRUE
}

AND standardize(n1,upb1) = VALOF
{ LET p = 2
  UNTIL p>upb1 | n1!p DO p := p+1
  
  IF p>upb1 DO
  { // The number is zero
    n1!0, n1!1 := FALSE, 0 // Other elements are already zero.
    RESULTIS TRUE
  }

  UNLESS p=2 DO
  { // Shift the fractional part to the left
    // and adjust the exponent.
    FOR i = p TO upb1 DO n1!(2+i-p) := n1!i
    FOR i = upb1-p+1 TO upb1 DO n1!i := 0
    n1!1 := n1!1 - p + 2  // Correct the exponent
  }

  RESULTIS TRUE
}

AND radius(p, upb1, d,upb2) = VALOF
{ // p -> [x, y, z]
  //       x, y and z are numbers with upperbound upb1
  // d is set to the distance between the origin and (x,y,z).
  //   ie sqrt(x^2+y^2+z^2)

  LET t1 = VEC numupb
  LET t2 = VEC numupb
  LET t3 = VEC numupb
  LET t4 = VEC numupb
  LET t5 = VEC numupb

  UNLESS mul(p!0,upb1,  p!0,upb1,  t1,numupb) RESULTIS FALSE
  UNLESS mul(p!1,upb1,  p!1,upb1,  t2,numupb) RESULTIS FALSE
  UNLESS mul(p!2,upb1,  p!2,upb1,  t3,numupb) RESULTIS FALSE
  UNLESS add(t1,numupb, t2,numupb, t4,numupb) RESULTIS FALSE
  UNLESS add(t3,numupb, t4,numupb, t5,numupb) RESULTIS FALSE
  UNLESS sqrt(t5,numupb, d,upb2) RESULTIS FALSE

  RESULTIS TRUE
}

AND inprod(dir1,upb1, dir2,upb2, n3,upb3) = VALOF
{ // dir1 and dir2 are 3D vectors.
  // ie dir1 -> [dx1,dy1,dz1] and dir2 -> [dx2,dy2,dz2] where
  // upb1 is the upperbounds of dx1, dy1 and dz1
  // upb2 is the upperbounds of dx2, dy2 and dz2
  // n3 is set to the dx1*dx2+dy1*dy2*dz1*dz2
  // If dir1 and dir2 represent direction cosines, n3 will be the
  // cosine of the angle between them.
  LET t1 = VEC numupb
  AND t2 = VEC numupb
  AND t3 = VEC numupb
  AND t4 = VEC numupb

  mul(dir1!0,upb1, dir2!0,upb2, t1,numupb)
  mul(dir1!1,upb1, dir2!1,upb2, t2,numupb)
  mul(dir1!2,upb1, dir2!2,upb2, t3,numupb)
  add(t1,numupb, t2,numupb, t4,numupb)
  add(t3,numupb, t4,numupb, n3,upb3)
  //writef("dir1!0**dir2!0= "); prnum(t1,numupb)
  //writef("dir1!1**dir2!1= "); prnum(t2,numupb)
  //writef("dir1!2**dir2!2= "); prnum(t3,numupb)
  //writef("sum=            "); prnum(n3,upb3)
//abort(986)
  RESULTIS TRUE
}

AND crossprod(dir1,upb1, dir2,upb2, dir3,upb3) = VALOF
{ // dir1, dir2 and dir3 are 3D vectors whose components have
  // upperbounds upb1, upb2 and upb3, respectively.
  // If dir1 -> [a,b,c] and dir2 -> [x,y,z], the three
  // components of dir3 are set to bz-cy, cx-az and ay-bx,
  // respectively. The direction of dir3 will be orthogonal to the
  // plane specified by dir1 and dir2, and its length will be the
  // product of the lengths of dir1 and dir2 multiplied by the sine
  // of the angle between them. As a special case, if dir1=[1,0,0]
  // and dir2=[0,1,0] the dir3 will be [0,0,1].
  LET t1 = VEC numupb
  AND t2 = VEC numupb

  mul(dir1!1,upb1, dir2!2,upb2, t1,numupb) // t1 = bz
  mul(dir1!2,upb1, dir2!1,upb2, t2,numupb) // t2 = cy, cx-az and ay-bx,
  sub(t1,numupb,   t2,numupb, dir3!0,upb3) // dir3!0 = bz-cy

  mul(dir1!2,upb1, dir2!0,upb2, t1,numupb) // t1 = cx
  mul(dir1!1,upb1, dir2!2,upb2, t2,numupb) // t2 = az
  sub(t1,numupb,   t2,numupb, dir3!1,upb3) // dir3!0 = cx-az

  mul(dir1!0,upb1, dir2!1,upb2, t1,numupb) // t1 = ay
  mul(dir1!1,upb1, dir2!2,upb2, t2,numupb) // t2 = bx
  sub(t1,numupb,   t2,numupb, dir3!2,upb3) // dir3!0 = ay-bx

  RESULTIS TRUE
}

AND normalize(dir, upb) = VALOF
{ // This function causes a 3D vector dir to be scaled to make it unit length. 
  // dir -> [dx,dy,dz] with each coordinate having upperbound upb.
  // These numbers are divided by radius(dx,upb, dy,upb, dz,upb).
  // If (dx,dy,dz)=(0,0,0) it sets then to (1,0,0) and
  // returns FALSE
  LET d = VEC numupb
  LET t = VEC numupb

  UNLESS radius(dir,upb, d,numupb) RESULTIS FALSE

  IF iszero(d,numupb) DO
  {
    //writef("ERROR in normalize*n")
    //writef("dir!0= "); prnum(dir!0, upb)
    //writef("dir!1= "); prnum(dir!1, upb)
    //writef("dir!2= "); prnum(dir!2, upb)
    //writef("radius= "); prnum(d,numupb)
    settok(1, dir!0,upb)
    setzero(dir!1,upb)
    setzero(dir!2,upb)
    //writef("Set dir to (1,0,0) since dir was too small*n")
    //abort(999)
    RESULTIS FALSE
  }

  UNLESS div(dir!0,upb, d,numupb, t,numupb) RESULTIS FALSE
  copy(t,numupb, dir!0,upb)

  UNLESS div(dir!1,upb, d,numupb, t,numupb) RESULTIS FALSE
  copy(t,numupb, dir!1,upb)

  UNLESS div(dir!2,upb, d,numupb, t,numupb) RESULTIS FALSE
  copy(t,numupb, dir!2,upb)

  RESULTIS TRUE
}

AND exptok(k, n1,upb1, n2,upb2) BE
{ // Set n2 to n1^n rounded where n is an integer >= 0.
  LET P = VEC numupb
  AND R = VEC numupb
  AND T = VEC numupb
//writef("*nCalling copy(n1,%n, P,%n)*n", upb1, upb2)
  copy(n1,upb1, P,numupb) // To hold the next power of n1

//writef("*nTracing exptok*n*n")

//writef("P=         "); prnum(P,numupb)
  settok(1, R,numupb)     // To hold the result
//writef("R=         "); prnum(R,numupb)
//writef("k=%n   %10b*n", k, k)

  WHILE k DO
  { IF (k & 1)>0 DO
    { // Set R = R * P  ie multiply R by the current power of n1
      mul(R,numupb, P,numupb, T,numupb)
      copy(T,numupb, R,numupb)
//writef("k is odd so set R = R ** P*n")
//writef("R=         "); prnum(R,numupb)
    }
    // Set P to P * P
//writef("Set P = P ** P*n")
    mul(P,numupb, P,numupb, T,numupb)
    copy(T,numupb, P,numupb)
//writef("P=         "); prnum(P,numupb)
    k := k>>1
//writef("Set k = k>>1*n")
//writef("k=%n   %10b*n", k, k)
  }
//writef("Copy R rounded to the result n2*n")
  copy(R,numupb, n2,upb2)
//writef("R rounded: "); prnum(n2,upb2)
}

AND checknum(n1,upb1) BE
{ // The calls abort(999) if n1 is not in standard form.
  LET sign, e, d1 = n1!0, n1!1, n1!2
  UNLESS sign=TRUE | sign=FALSE DO
  { writef("n1 has a bad sign field*n")
    writef("n1= "); prnum(n1,upb1)
    abort(999)
    RETURN
  }
  IF d1=0 DO
  { // Check n1 represents zero.
    FOR i = 0 TO upb1 UNLESS n1!i=0 DO
    { writef("n1!2 is zero but other elements are not*n")
      writef("n1= "); prnum(n1,upb1)
      abort(999)
      RETURN
    }
    RETURN
  }
  // Check that all radiz digits are in range 0 to 9999
  FOR i = 2 TO upb1 UNLESS 0 <= n1!i <= 9999 DO
  { writef("Not all radix digits of n1 are in range 0 to 9999*n")
    writef("n1= "); prnum(n1,upb1)
    abort(999)
    RETURN
  }
}
