/*

This program performs either a Fourrier or Walsh transform on 2^ln
values foloowed by the inverse transform. The given ln must be between
3 and 16. The program outputs up to 32 of the initial values, the the
transformed values and the results of the inverse transform. The
program does not use complex arithmetic but integer arithmetic modulo
2^16+1. This satisfies all the algebraic properties of complex
arithmetic but does not suffer from rounding and truncation errors.

*/

GET "libhdr"

MANIFEST {
  modulus = #x10001  // 2**16 + 1
}

GLOBAL {
  data:ug  // A vector holding the original and transformed data.
  ln       // Specifies the size of the problem
  N        // N = 2^ln
  omega    // The smallest Nth root os unity other than 1.
  upb      // =N-1
  prupb    // =upb or 31 whichever is smaller.
}


LET start() = VALOF
{ LET argv = VEC 50

  UNLESS rdargs("ln/N,walsh/S", argv, 50) DO
  { writef("*nBad args for bigfft*n")
    RESULTIS 0
  }

  UNLESS argv!0 DO
  { writef("*nArgs: ln/N,walsh/S*n*n")

    writef("ln     Specifies the size of the problem*n")
    writef("       The transform will be performed on N=2^ln values*n")
    writef("walsh  Specifies that the Walsh transformation will be performed*n*n")

    RESULTIS 0
  }

  ln := !argv!0
  UNLESS 3<=ln<=16 DO
  { writef("*nBad ln value -- it must be between 3 and 16*n*n")
    RESULTIS 0
  }
  N     := 1<<ln              // N is a power of 2
  upb   := N-1
  prupb := upb<=31 -> upb, 31 // Upper bound for printing

  omega := 1

  UNLESS argv!1 WHILE omega < modulus DO
  { LET x = 1
    FOR i = 1 TO N DO
    { x := mul(x, omega)
      IF x=1 TEST i=N THEN GOTO rootfound
                      ELSE BREAK
    }
    omega := omega+1
  }

rootfound:
  TEST omega=1
  THEN { writef("Performing the Walsh transforn on %n words of data*n", N)
       }
  ELSE { writef("*nThe principal %nth root of unity is %n = %x5*n*n",
                 N, omega, omega)
         check(omega, N)   // check that omega and N are consistent
                           // ie omega^N = 1 and
                           //    omega^i ~= 1 all 1<=i<N
         writef("Performing FFT on %n data values*n*n", N, omega)
       }

  data := getvec(upb)

  FOR i = 0 TO upb DO data!i := i  // Set the test data
  pr(data, prupb)
// prints  -- Original data for FFT with ln=3
//     0     1     2     3     4     5     6     7

  fft(data, ln, omega)
  pr(data, prupb)
// prints   -- Transformed data
// 65017 26645 38448 37467 30114 19936 15550 42679

  fft(data, ln, div(1,omega))
  FOR i = 0 TO upb DO data!i := div(data!i, N)
  pr(data, prupb)
// prints  -- Restored data
//     0     1     2     3     4     5     6     7
  RESULTIS 0
}

AND fft(v, ln, w) BE  // ln = log2 n    w = nth root of unity
{ LET n = 1<<ln
  LET vn = v+n
  LET n2 = n>>1

  // First do the perfect shuffle
  reorder(v, n)

  // Then do all the butterfly operations
  FOR s = 1 TO ln DO
  { LET m  = 1<<s
    LET m2 = m>>1
    LET wk, wkfac = 1, w
    FOR i = s+1 TO ln DO wkfac := mul(wkfac, wkfac)
    FOR j = 0 TO m2-1 DO
    { LET p = v+j
      WHILE p<vn DO { butterfly(p, p+m2, wk); p := p+m }
      wk := mul(wk, wkfac)
    }
  }
}

AND butterfly(p, q, wk) BE { LET a, b = !p, mul(!q, wk)
                             !p, !q := add(a, b), sub(a, b)
                           }

AND reorder(v, n) BE
{ LET j = 0
  FOR i = 0 TO n-2 DO
  { LET k = n>>1
    // j is i with its bits is reverse order
    IF i<j DO { LET t = v!j; v!j := v!i; v!i := t }
    // k  =  100..00       10..0000..00
    // j  =  0xx..xx       11..10xx..xx
    // j' =  1xx..xx       00..01xx..xx
    // k' =  100..00       00..0100..00
    WHILE k<=j DO { j := j-k; k := k>>1 } //) "increment" j
    j := j+k                              //)
  }
}

AND check(w, n) BE
{ // Check that w is a principal nth root of unity
  LET x = 1
  FOR i = 1 TO n-1 DO { x := mul(x, w)
                        IF x=1 DO writef("omega****%n = 1*n", i)
                      }
  UNLESS mul(x, w)=1 DO writef("Bad omega**%n should be  1*n", n)
  //writef("%n = %x5 is the %nth root of unity*n*n", w, w, n)
}

AND pr(v, max) BE
{ FOR i = 0 TO max DO { writef("%I5 ", v!i)
                        IF i REM 8 = 7 DO newline()
                      }
  newline()
}

AND dv(a, m, b, n) = a=1 -> m,
                     a=0 -> m-n,
                     a<b -> dv(a, m, b REM a, m*(b/a)+n),
                     dv(a REM b, m+n*(a/b), b, n)


AND inv(x) = dv(x, 1, modulus-x, 1)

AND add(x, y) = VALOF
{ LET a = x+y
  IF a<modulus RESULTIS a
  RESULTIS a-modulus
}

AND sub(x, y) = add(x, neg(y))

AND neg(x)    = modulus-x

AND mul(x, y) = x=0 -> 0,
                (x&1)=0 -> mul(x>>1, add(y,y)),
                add(y, mul(x>>1, add(y,y)))

AND div(x, y) = mul(x, inv(y))
