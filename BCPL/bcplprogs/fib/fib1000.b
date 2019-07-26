/*

This program finds the position of the first fibonacci number having
1000 decimal digits. The first fibonacci number has position zero.
Ie fib(0)=0, fib(1)=1, fib(2)=1, fib(3)=2, fib(4)=3, fib(5)=5, etc

This is a naive implemetation using vectors of digits of radix 1000000.

*/

GET "libhdr"

MANIFEST {
  radix = 100_000_000
  digs  = 1000       // Number of decimal digits
  upb   = digs / 8 // 8 decimal digits per word
}

LET start() = VALOF
{ LET a = getvec(upb)
  AND b = getvec(upb)
  LET upba, upbb = ?, ?
  LET t, upbt = ?, ?
  LET n = ?
  LET w = 1
  LET k = (digs-1) / 8
  FOR i = 1 TO (digs-1) MOD 8 DO w := 10*w

  // Set a=0 and b=1
  FOR i = 0 TO upb DO a!i, b!i := 0, 0
  b!0 := 1
  upba, upbb := 0, 0
  n := 1 // n is the position of the fibonacci number in b

  { IF b!k >= w BREAK

    // b is greater than a
    upba := add(b,upbb, a,upba) // Set a to b + a

    n := n+1 // n is now the position of the fibonacci number in a
    //pr(n, a, upba)

    // Swap a and b
    t, upbt := a, upba
    a, upba := b, upbb
    b, upbb := t, upbt
  } REPEAT

  writef("The first fibonacci number with %n digits is at position %n*n",
          digs, n)

  freevec(a)
  freevec(b)
  RESULTIS TRUE
}

AND add(a,upba, b,upbb) = VALOF
{ // Add a to b assuming a is greater than b, ie upba>=upbb
  LET carry = 0

  FOR i = 0 TO upba DO
  { LET x = a!i + b!i + carry
    b!i   := x MOD radix
    carry := x  /  radix
  }
  IF carry DO
  { upba := upba+1
    b!upba := carry
  }

  RESULTIS upba
}

AND pr(n, a, upb) BE
{ writef("%i5: ", n)
  FOR i = upb TO 0 BY -1 DO writef(" %i8", a!i)
  newline()
}
