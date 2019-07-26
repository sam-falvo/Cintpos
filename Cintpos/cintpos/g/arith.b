/*

This file contains a library of functions to perform high precision
arithmetic. It header file is arith.h.

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

The library functions provided are as follows.

str2num(s, n,upb)     Set n to the number represented by string s
copy(n1,upb1, n2,upb2) Copy n1 to n2.
copyabs(n1,upb1, n2,upb2) Copy abs n1 to abs n2.
standardize(n1,upb1)  Replace n1 by n1 in standard form. That is
                      if n1 is zero all elements of n1 are set to zero
                      otherwise the fractional part is shifted left
                      until n1!2 is non zero correcting the exponent
                      appropriately.
setzero(n1,upb1)      Set n1 to zero
iszero(n1,upb1)       Return TRUE if n1 is zero

addu(n1,upb1, n2,upb2, n3,upb3) Set n3 to n1 plus n2 ignoring signs
add(n1,upb1, n2,upb2, n3,upb3)  Set n3 to n1 signed plus n2
subu(n1,upb1, n2,upb2, n3,upb3) Set n3 to n1 minus n2 ignoring signs and
                                assuming abs n1 >= abs n2
sub(n1,upb1, n2,upb2, n3,upb3)  Set n3 to n1 signed minus n2
mul(n1,upb1, n2,upb2, n3,upb3)  Set n3 to n1 multiplied by n2
inv(n1,upb1, n2,upb2)           Set n2 to the inverse of n1
div(n1,upb1, n2,upb2, n3,upb3)  Set n3 to n1 divided by n2
sqrt(n1,upb1, n2,upb2)          Set n2 to the square root of n1.
dist((n1,upb1, n2,upb2, n3,upb3, n4,upb4)  Set n4 to the square root
                                           of n1^2+n2^2+n3^2.
normalize((n1,upb1, n2,upb2, n3,upb3)  Replace each of n1, n2 and n3 by
                                the result of dividing them by d where
                                d is dist(n1,upb1, n2,upb2, n3,upb3).

numcmpu(n1,upb1, n2,upb2)  Return -1 if n1<n2   Unsigned comparison
                                   0 if n1=n2
                                  +1 if n1>n2
numcmp(n1,upb1, n2,upb2)   Return the signed comparison of n1 and n2.

prnum(n1, upb)     Output the number n1.
*/

LET str2num(s, n1, upb1) = VALOF
{ LET p = 0     // Count of decimal digits not including
                // leading zeroes
  LET fp   = -1 // Count of digits after the decimal point, if any.
  LET pos  =  2 // Position of next radix digit
  LET dig  =  0 // To hold the next radix digit
  LET dexp =  ?
  LET e = 0     // For the exponent specified by En
//writef("str2num: 1 n1!(upb1+1)=%n*n", n1!(upb1+1))
  n1!0 := -2  // No sign yet
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

  // Pad last radix digit by adding fractional zeroes.

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

  RESULTIS TRUE
}

AND settok(k, n1,upb1) BE
{ // k must be in range -9999 to +9999 
  setzero(n1,upb1)
  TEST k<0
  THEN n1!0, n1!1, n1!2 :=  TRUE, 1, -k
  ELSE n1!0, n1!1, n1!2 := FALSE, 1,  k
}

AND integerpart(n1,upb1) = VALOF
{ // Only two radix digits of integer part is allowed,
  // so the result is in the range -9999_9999 to +9999_9999.
  LET x = n1!2 * 10000 + n1!3
  LET e = n1!1
  IF e > 2 DO x := 1_0000_0000
  IF n1!0 DO x := -x
  IF e <= 0  RESULTIS 0
  IF e  = 1  RESULTIS x / 10000
  RESULTIS x
}

AND prnum(n, upb) BE
{ // Output a number p with d significant radix digits.
  writef("%c0.", n!0->'-','+')
  FOR i = 2 TO upb DO writef("%z4 ", n!i)
  IF n!1 DO writef("E%n", n!1)
  newline()
}

AND numcmpu(n1,upb1, n2,upb2) = VALOF
{ // Perform an unsigned comparison of two numbers in
  // standard form.
  // Return  1 if n1 > n2
  // Return  0 if n1 = n2
  // Return -1 if n1 < n2
  LET upb = upb1<=upb2 -> upb1, upb2
  // upb is the smaller upper bound
//writef("1: numcmpu: memoryfree => %n*n", memoryfree())

  // Deal with the cases when n1 or n2 is zero.
  IF n1!2=0 DO
  { IF n2!2=0 RESULTIS 0  // n1= 0  n2= 0
    RESULTIS -1           // n1= 0  n2~=0
  }
//writef("numcmpu: memoryfree => %n*n", memoryfree())
    
  IF n2!2=0 RESULTIS 1    // n1~=0  n2= 0
//writef("numcmpu: memoryfree => %n*n", memoryfree())

  // Neither n1 nor n2 is zero
  FOR i = 1 TO upb DO
  { LET a, b = n1!i, n2!i
    IF a > b RESULTIS  1  // n1 > n2
    IF a < b RESULTIS -1  // n1 < n2
//writef("numcmpu: i=%n memoryfree => %n*n", i, memoryfree())
  }

  IF upb1=upb2 RESULTIS 0
//writef("numcmpu: memoryfree => %n*n", memoryfree())


  TEST upb1>upb
  THEN FOR i = upb+1 TO upb1 IF n1!i RESULTIS  1
  ELSE FOR i = upb+1 TO upb2 IF n2!i RESULTIS -1
//writef("numcmpu: memoryfree => %n*n", memoryfree())

  RESULTIS 0
}

AND numcmp(n1,upb1, n2,upb2) = VALOF
{ // Perform signed comparison on two standardized numbers.
  // The result is -1, 0, or +1
  IF n1!0 DO
  { IF n2!0 RESULTIS - numcmpu(n1,upb1, n2,upb2) // n1< 0, n2< 0
    RESULTIS -1                                  // n1< 0, n2>=0
  }

  IF n2!0 RESULTIS 1                             // n1>=0, n2< 0

  RESULTIS numcmpu(n1,upb1, n2,upb2)             // n1>=0  n2>=0
}

AND neg(n1,upb1, n2,upb2) = VALOF
{ copy(n1,upb1, n2,upb2)
  // n2 is in standard form
  IF n1!2 DO n2!0 := ~ n1!0
}

AND iszero(n1,upb1) = VALOF
{ // n1 is zero if all the fraction digits are zero
  FOR i = 2 TO upb1 UNLESS n1!i=0 RESULTIS FALSE
  RESULTIS TRUE
}

AND addu(n1,upb1, n2,upb2, n3,upb3) = VALOF
{ // Add the absolute values of n1 and n2 leaving the result in n3
  // The sign n3!0 is not changed.
  LET carry = 0
  LET t1,u1 = n1,upb1 // To hold the number with the larger exponent
  LET t2,u2 = n2,upb2 // To hold the number with the smaller exponent
  LET offset = ?
  LET p, q = ?, ?

  IF iszero(n1,upb1) RESULTIS copyabs(n2,upb2, n3,upb3)
  IF iszero(n2,upb2) RESULTIS copyabs(n1,upb1, n3,upb3)
  
  // Neither n1 nor n2 are zero.

  IF n1!1<n2!1 DO
  { t1,u1 := n2,upb2
    t2,u2 := n1,upb1
  }

  // t1!1 >= t2!1     So t1 has the smaller exponent

  offset := t1!1-t2!1  // offset is >= 0

  p := u2
  q := u2+offset

  copyabs(t1,u1, n3,upb3)

  WHILE p >= 2 DO
  { IF q <= upb3 DO
    { LET x = n3!q + t2!p + carry
      n3!q  := x MOD 10000
      carry := x  /  10000
    }
    p, q := p-1, q-1
  }

  WHILE carry & q >= 2 DO
  { IF q <= upb3 DO
    { LET x = n3!q + carry
      n3!q  := x MOD 10000
      carry := x  /  10000
    }
    q := q-1
  }

  IF carry DO
  { // Shift the radix digits to the right by one position.
    FOR i = upb3-1 TO 2 BY -1 DO n3!(i+1) := n3!i
    n3!2 := carry
    n3!1 := n3!1 + 1 // Adjust the exponent
  }

  RESULTIS TRUE
}

AND subu(n1,upb1, n2,upb2, n3,upb3) = VALOF
{ // Set abs n3 = abs n2 - abs n1 assuming abs n1 >= abs n2
  // n3!0 is not changed.
  LET borrow = 0
  LET offset = ?
  LET p, q = ?, ?

  offset := n1!1-n2!1

  p := upb2
  q := upb2+offset

  copyabs(n1,upb1, n3,upb3)

  WHILE p >= 2 DO
  { IF q <= upb3 DO
    { LET x = n3!q - n2!p - borrow
      borrow := 0
      IF x<0 DO x, borrow := x+10000, 1
      n3!q := x
    }
    p, q := p-1, q-1
  }

  WHILE borrow & q >= 2 DO
  { IF q <= upb3 DO
    { LET x = n3!q - borrow
      borrow := 0
      IF x<0 DO x, borrow := x+10000, 1
      n3!q := x
    }
    q := q-1
  }

  IF borrow RESULTIS FALSE

  // Standardize the result
  p := 2
  UNTIL n3!p | p > upb3 DO p := p+1

  TEST p>upb3
  THEN n3!0, n3!1 := FALSE, 0 // The result is zero
  ELSE UNLESS p=2 DO
       { LET q = 2
         FOR i = p TO upb3 DO
         { n3!q := n3!i
           q := q+1
         }
         WHILE q <= upb3 DO 
         { n3!q := 0
           q := q+1
         }
         n3!1 := n3!1 - (p-2)  // Correct the exponent
       }

  RESULTIS TRUE
}

AND setzero(n1, upb1) BE FOR i = 0 TO upb1 DO n1!i := 0

AND add(n1,upb1, n2,upb2, n3,upb3) = VALOF
{ // Add signed numbers n1 and n2 placing the result in n3

  LET rc = ?

  IF n1!0=n2!0 DO
  { // eg +5 + +3 =>  +  5+3
    // eg -3 + -5 =>  -  5+3
    n3!0 := n1!0
    RESULTIS addu(n1,upb1, n2,upb2, n3,upb3)
  }

  rc := numcmpu(n1,upb1, n2,upb2)

  IF rc=0 RESULTIS setzero(n3,upb3)

  TEST n1!0
  THEN TEST rc>0
       THEN { // eg -5 + +3 =>  -  5-3
              n3!0 := TRUE
              RESULTIS subu(n1,upb1, n2,upb2, n3,upb3)
            }
       ELSE { // eg -3 + +5 =>  +  5-3
              n3!0 := FALSE
              RESULTIS subu(n2,upb2, n1,upb1, n3,upb3)
            }
  ELSE TEST rc>0
       THEN { // eg +5 + -3 =>  +  5-3
              n3!0 := FALSE
              RESULTIS subu(n1,upb1, n2,upb2, n3,upb3)
            }
       ELSE { // eg +3 + -5 =>  -  5-3
              n3!0 := TRUE
              RESULTIS subu(n2,upb2, n1,upb1, n3,upb3)
            }
}

AND sub(n1,upb1, n2,upb2, n3,upb3) = VALOF
{ // Subtract n2 from n1 using signed arithmetic placing
  // the result in n3.
  LET rc = ?

  IF n1!0~=n2!0 DO
  { // eg +5 - -3 =>  +  5+3
    // eg -3 + +5 =>  -  5+3
    n3!0 := n1!0
    RESULTIS addu(n1,upb1, n2,upb2, n3,upb3)
  }

  rc := numcmpu(n1,upb1, n2,upb2)

  IF rc=0 RESULTIS setzero(n3,upb3)

  TEST n1!0
  THEN TEST rc>0
       THEN { // eg -5 - -3 =>  -  5-3
              n3!0 := TRUE
              RESULTIS subu(n1,upb1, n2,upb2, n3,upb3)
            }
       ELSE { // eg -3 - -5 =>  +  5-3
              n3!0 := FALSE
              RESULTIS subu(n2,upb2, n1,upb1, n3,upb3)
            }
  ELSE TEST rc>0
       THEN { // eg +5 - +3 =>  +  5-3
              n3!0 := FALSE
              RESULTIS subu(n1,upb1, n2,upb2, n3,upb3)
            }
       ELSE { // eg +3 - +5 =>  -  5-3
              n3!0 := TRUE
              RESULTIS subu(n2,upb2, n1,upb1, n3,upb3)
            }
}


AND mul(n1,upb1, n2,upb2, n3,upb3) = VALOF
{ LET sign = n1!0 XOR n2!0       // Set the sign of the result
  LET exponent = n1!1 + n2!1 - 1 // Initial exponent

  setzero(n3,upb3)

  // Use n3!1 as an overflow radix digit set by docarry.

  FOR i = 2 TO upb1 DO
  { LET n1i = n1!i
    IF n1i DO
    { FOR j = 2 TO upb2 DO
      { LET p = i + j - 2    // The destination position
        IF p <= upb3 DO
        { LET x = n1i * n2!j
          LET y = n3!p + x
          n3!p := y
        }
      }
      docarry(n3, upb3)
    }
  }

  IF n3!1 DO // n3!1 is currently the carry produced by docarry.
  { // Shift the value digits to the right by one position.
    FOR i = upb3-1 TO 1 BY -1 DO n3!(i+1) := n3!i
    exponent := exponent + 1 // Adjust the exponent.
  }

  n3!1 := exponent
  n3!0 := sign
  RESULTIS TRUE
}

AND mulbyk(k, n1,upb1) = VALOF
{ LET sign = n1!0
  LET carry = 0

  IF k<=0 DO
  { IF k=0 DO { setzero(n1,upb1); RESULTIS TRUE }
    n1!0, k := ~n1!0, -k
  }

  FOR i = upb1 TO 2 BY -1 DO
  { LET x = n1!i * k + carry
    n1!i  := x MOD 10000
    carry := x  /  10000
  }

  IF carry DO
  { // Shift the fractional part to the right
    // and adjust the exponent.
    FOR i = upb1-1 TO 2 BY -1 DO n1!(i+1) := n1!i
    n1!1 := n1!1 + 1
    n1!2 := carry
  }
  RESULTIS TRUE
}

AND docarry(n1, upb1) = VALOF
{ // Deal with over size digits in the fractional part.
  // It using n1!1 as an overflow radix digit
  LET carry = 0
  FOR i = upb1 TO 1 BY -1 DO
  { LET x = n1!i + carry
    n1!i  := x MOD 10000
    carry := x  /  10000
  }

  IF carry DO
  { writef("Unexpected overflow in mul*n")
    abort(999)
    RESULTIS FALSE
  }
  RESULTIS TRUE
}

AND inv(n1,upb1, n2,upb2) = VALOF
{ // If n1 is zero return FALSE, otherwise
  // standardize n1 and set n2 to the inverse of n1
  LET one = TABLE FALSE, 1, 0001   // The number +1.0
  LET sign = n1!0
  LET e = ?
  LET elim = -(numupb - 2 - numupb/4)

  LET t1 = VEC numupb
  AND t2 = VEC numupb
  AND t3 = VEC numupb
  AND t4 = VEC numupb

  standardize(n1,upb1)

  IF n1!2=0 RESULTIS FALSE // Cannot take the inverse of zero.

  IF numcmp(one,2, n1,upb1)=0 DO
  { copy(one,2, n2,upb2) // The inverse of 1.0 is 1.0.
    RESULTIS TRUE
  }

  e := n1!1
  n1!0, n1!1 := FALSE, 0 // Set n1 to be in the range 0.0001 to 1.0000

  // Select initial guess
  { LET w = n1!2 * 1_0000 + n1!3
    // 10000 <= w <= 99999999
    LET a = muldiv(9999_9999, 1_0000, w)
    //writef("w=%z8  a=%z8*n", w, a)
    setzero(t1,numupb)
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

    mul(t1,numupb, n1,upb1, t2,numupb)  // Set t2 = t1*n1
    //newline()
    //writef("Set t2 = t1**n1*n")
    //writef("t1= "); prnum(t1,numupb)
    //writef("n1= "); prnum(n1,upb1)
    //writef("t2= "); prnum(t2,numupb)

    //writef("*nCalling sub(one,2, t2,numupb, t3,numupb)*n")
    sub(one,2, t2,numupb, t3,numupb) // Set t3 := 1 - t1*n1
    //writef("Set t3 to 1 - t2,   where t2 = t1**n1*n")
    //writef("one="); prnum(one,2)
    //writef("t2= "); prnum(t2,numupb)
    //writef("t3= "); prnum(t3,numupb)

    mul(t1,numupb, t3,numupb, t2,numupb)
    //writef("Set t2 = t1 ** (1 - t1**n1)*n")
    //writef("t1=    "); prnum(t1,numupb)
    //writef("t3=    "); prnum(t3,numupb)
    //writef("t2=    "); prnum(t2,numupb)

    add(t1,numupb, t2,numupb, t3,numupb)
    //writef("Set t3 = t1 + t1 ** (1 - t1**n1)*n")
    //writef("t3=    "); prnum(t3,numupb)

    //newline()
    //writef("n1=    "); prnum(n1,upb1)
    //writef("Previous guess for 1/n1 is in t1*n")
    //writef("t1=    "); prnum(t1,numupb)
    //writef("New guess for 1/n1 is in t3*n")
    //writef("t3=    "); prnum(t3,numupb)

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
        GOTO again 
      }
    }
  }

  //writef("It was close enough so copy t3 to n2 and return from inv*n")
  //writef("e=%n  1-e=%n*n", e, 1-e)
  t3!0, t3!1 := sign, 1-e  // Set the sign and exponent of the result
  copy(t3,numupb, n2,upb2)
  n1!0, n1!1 := sign, e    // Restore the sign and exponent of n1
  RESULTIS TRUE
}

AND divbyk(k, n1,upb1) = VALOF
{ LET carry = 0
  LET e = n1!1

//writef("divbyk k=%n*n", k)
//writef("n1=  "); prnum(n1,upb1)

  FOR i = 1 TO upb1-1 DO
  { LET x = carry*10000 + n1!(i+1)
    n1!i  := x  /  k
    carry := x MOD k
  }
  n1!upb1 := carry

  TEST n1!1
  THEN FOR i = upb1-1 TO 1 BY -1 DO n1!(i+1) := n1!i
  ELSE e := e-1

  n1!1 := e
//writef("n1=  "); prnum(n1,upb1)
//abort(9977)
  RESULTIS standardize(n1,upb1)  // For safety
}

AND div(n1,upb1, n2,upb2, n3,upb3) = VALOF
{ // This is a Newton-Raphson iterative version of division to
  // calculate 1/n2. It then multiplies 1/n2 by n1.

  LET t1 = VEC numupb

  IF iszero(n1,upb1) DO
  { setzero(n3,upb3)
    RESULTIS TRUE
  }

  // Calculate 1/n2 using the interation x(n+1) = xn + xn*(1 - n2*xn)
  inv(n2,upb2, t1,numupb)

  //writef("n2=    "); prnum(n2,upb2)
  //writef("1/n2=  "); prnum(t1,numupb)
  // Multiply 1/n2 by n1

  mul(t1,numupb, n1,upb1, n3,upb3)
  //writef("n2=    "); prnum(n2,upb2)
  //writef("1/n2=  "); prnum(t1,numupb)
  //writef("n1 ** 1/n2=  "); prnum(t3,upb3)

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

  // Use interation: x(n+1) = (xn + p/xn)/2

  //writef("*nsqrt of "); prnum(n1,upb1)

  setzero(n2,upb2)

  IF iszero(n1) RESULTIS TRUE   // sqrt(0) = 0

  standardize(n1,upb1)
  // n1!2 will be non zero

  IF n1!0 RESULTIS FALSE  // n1 must be positive

  e := n1!1  // Save the exponent of n1 in e
  n1!1 := 0  // Cause n1 to be in range 0001 to 9999

  // n1 is greater than zero

  { // Choose the initial guess
    LET a = n1!2 * 10000 + n1!3 // 0001 <= n1!2 <= 9999_9999
    LET guess = 100_0000

    UNTIL muldiv(guess, guess, 1_0000_0000) >= a DO
    { guess := guess + guess
    }

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
  UNLESS (e&1)=0 DO divbyk(100, t3,numupb)

  // Test the accuracy of the square root.
  //writef("t3=   "); prnum(t3,numupb)
  //mul(t3,numupb, t3,numupb, t2,numupb)
  //writef("t3^2= "); prnum(t2,numupb)
  //writef("n1=   "); prnum(n1,upb1)
//abort(7777)

  copy(t3,numupb, n2,upb2)

  mul(n2,upb2, n2,upb2, t2,numupb)
  //writef("n1=   "); prnum(n1,upb1)
  //writef("n2=   "); prnum(n2,upb2)
  //writef("n2^2= "); prnum(t2,numupb)
  //newline()
//abort(8888)
  RESULTIS TRUE
}

AND copy(n1,upb1, n2,upb2) = VALOF
{ // Set n2 to n1 leaving it in standard form.
  LET upb = upb1<upb2 -> upb1, upb2
  FOR i = 0 TO upb DO n2!i := n1!i
  FOR i = upb+1 TO upb2 DO n2!i := 0
  IF n2!2=0 RESULTIS standardize(n2,upb2)
  RESULTIS TRUE
}

AND copyabs(n1,upb1, n2,upb2) = VALOF
{ // Copy the exponent and fractional part of n1 into n2
  LET upb = upb1<upb2 -> upb1, upb2
  FOR i = 1 TO upb DO n2!i := n1!i
  FOR i = upb+1 TO upb2 DO n2!i := 0
  RESULTIS TRUE
}

AND standardize(n1,upb1) = VALOF
{ LET p = 2
  UNTIL p>upb1 | n1!p DO p := p+1
  
  IF p>upb1 DO
  { // The number is zero
    n1!0, n1!1 := FALSE, 0
    RESULTIS TRUE
  }

  UNLESS p=2 DO
  { // Shift the fractional part to the left
    // and adjust the exponent.
    FOR i = p TO upb1 DO n1!(2+i-p) := n1!i
    FOR i = upb1-p+1 TO upb1 DO n1!i := 0
    n1!1 := n1!1 - p + 2
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
  // upb1 is the upperbound of dx1,dy1 and dz1
  // upb2 is the upperbound of dx2,dy2 and dz2
  // n3 is set to the dx1*dx2+dy1*dy2*dz1*dz2
  // If dir1 and dir2 represent direction cosines, d will be the
  // cosine of the angle between the two directions specified.
  LET t1 = VEC numupb
  AND t2 = VEC numupb
  AND t3 = VEC numupb
  AND t4 = VEC numupb

  mul(dir1!0,upb1, dir2!0,upb2, t1,numupb)
  mul(dir1!1,upb1, dir2!1,upb2, t2,numupb)
  mul(dir1!2,upb1, dir2!2,upb2, t3,numupb)
  add(t1,numupb, t2,numupb, t4,numupb)
  add(t3,numupb, t4,numupb, n3,upb3)
  RESULTIS TRUE
}

AND normalize(dir, upb) = VALOF
{ // This function causes a 3D vector dir to be scaled to make it unit length. 
  // dir -> [dx,dy,dz] with each coordinate having upperbound upb.
  // These numbers are divided by radius(dx,upb, dy,upb, dz,upb)
  LET d = VEC numupb
  LET t = VEC numupb

  UNLESS radius(dir,upb, d,numupb) RESULTIS FALSE

  UNLESS div(dir!0,upb, d,numupb, t,numupb) RESULTIS FALSE
  copy(t,numupb, dir!0,upb)

  UNLESS div(dir!1,upb, d,numupb, t,numupb) RESULTIS FALSE
  copy(t,numupb, dir!1,upb)

  UNLESS div(dir!2,upb, d,numupb, t,upb) RESULTIS FALSE
  copy(t,numupb, dir!2,upb)

  RESULTIS TRUE
}



