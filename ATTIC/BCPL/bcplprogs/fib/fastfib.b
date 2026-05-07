GET "libhdr"

MANIFEST {
  ArithGlobs=350
  numupb = 2+250+10  // Size of numbers used in the library,
                     // good for 1000 decimals 40 check digits.
  nupb   = numupb-5  // Size of numbers used in this program,
                     // allowing 5 guard digits
}

GET "arith.h"
GET "arith.b"       // Get the high precision library

GLOBAL {
  tracing:ug

  root5
  invroot5
  P           // To hold (1+sqrt(5))/2
  Q           // To hold (1-sqrt(5))/2
  One         // To hold 1.0
  pos         // Position of the fibonacci number
  F           // To hold fib(pos)
  T1          // Temp value
}

LET start() = VALOF
{ LET argv = VEC 50

  UNLESS rdargs("n/N,-t/S", argv, 50) DO
  { writef("Bad arguments*n")
    RESULTIS 0
  }

  pos := 5
  IF argv!0 DO pos  := !(argv!0) //  pos/N
  tracing := argv!1              // -t/S

  root5 := getvec(nupb)
  invroot5 := getvec(nupb)
  P := getvec(nupb)
  Q := getvec(nupb)
  F := getvec(nupb)
  One := getvec(nupb)
  T1  := getvec(nupb)
  
  settok(5, T1,nupb)
  sqrt(T1,nupb, root5,nupb)
  inv(root5,nupb, invroot5,nupb)

  IF tracing DO
  { writef("root5=*n")
    prnum(root5,nupb)
    // Check root5
    mul(root5,nupb, root5,nupb, T1,nupb)
    writef("root5^2=*n"); prnum(T1,nupb)
    newline()

    // Check invroot5
    writef("invroot5=*n")
    prnum(invroot5,nupb)
    mul(invroot5,nupb, invroot5,nupb, T1,nupb)
    writef("invroot5^2=*n")
    prnum(T1,nupb)
  }

  settok(1, One,nupb)

  // Set P to (1 + sqrt(5))/2
  add(One,nupb, root5,nupb, P,nupb)
  divbyk(2, P,nupb)
  IF tracing DO
  { writef("P = (1 + sqrt(5))/2 =*n")
    prnum(P,nupb)
    newline()
  }
  // Set Q to (1 - sqrt(5))/2
  sub(One,nupb, root5,nupb, Q,nupb)
  divbyk(2, Q,nupb)
  IF tracing DO
  { writef("Q = (1 - sqrt(5))/2 =*n")
    prnum(Q,nupb)
    newline()
  }

  //writef("Calling fib(%n, F,%n)*n", pos, nupb)
  fib(pos, F,nupb) // Compute fibonacci of pos

  writef("fib(%n) =*n", pos)
  prnum(F,nupb)

  { LET k, d1 = 4*F!1-4, F!2
    UNTIL d1 = 0 DO k, d1 := k+1, d1/10
    IF k<=0 DO k := 1
    writef("Number of decimal digits: %n*n", k)
  }

  freevec(root5)
  freevec(invroot5)
  freevec(One)
  freevec(P)
  freevec(Q)
  freevec(F)
  freevec(T1)

  RESULTIS 0
}

AND fib(n, n1,upb1) BE
{ LET rc = 0
  LET t1 = VEC numupb
  AND t2 = VEC numupb
  AND t3 = VEC numupb

  exptok(n, P,nupb, t1,nupb)
  IF tracing DO
  { writef("P^%n:*n", n)
    prnum(t1,nupb)
  }
  exptok(n, Q,nupb, t2,nupb)
  IF tracing DO
  { writef("Q^%n=*n",n)
    prnum(t2,nupb)
  }
  sub(t1,nupb, t2,nupb, t3,nupb)
  IF tracing DO
  { writef("P^%n-Q^%n=*n",n,n)
    prnum(t3,nupb)
  }
  mul(t3,nupb, invroot5,nupb, n1,upb1)
  IF tracing DO
  { writef("(P^%n-Q^%n) by sqrt(5) unrounded*n",n,n)
    prnum(n1,upb1)
  }
  rc := roundtoint(n1,upb1)

  UNLESS rc=0 | rc=9999_9999 DO
  { writef("Higer precision required, rc=%z8*n", rc)
    abort(999)
  }
}
