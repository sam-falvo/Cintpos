/*

This program performs either a Fourrier or Walsh transform on 2^ln
values followed by the inverse transform. The given ln must be between
3 and 16. The program outputs up to 32 of the initial values, the
transformed values and the results of the inverse transform. The
program does not use complex arithmetic but integer arithmetic modulo
2^16+1. This satisfies all the algebraic properties of complex
arithmetic but does not suffer from rounding and truncation errors.

A typical run of this program is as follows.

4.500> bigfft 9

Initial data N=512 w=157

    0     1     2     3     4     5     6     7 
    8     9    10    11    12    13    14    15 
   16    17    18    19    20    21    22    23 
   24    25    26    27    28    29    30    31 

After FFT
65279 33612 53306   572 55468 34328 35024 43282 
34141 61547 19599 40098 22616 29665 42955 51991 
57800 50816 44555 19224 53222 52432 39732 52171 
52122 32105 46000 45288   656   943 39865  7146 

After inverse FFT
    0     1     2     3     4     5     6     7 
    8     9    10    11    12    13    14    15 
   16    17    18    19    20    21    22    23 
   24    25    26    27    28    29    30    31 

1.000> 
*/

GET "libhdr"

MANIFEST {
  modulus = #x10001  // 2**16 + 1
}

GLOBAL {
  data:ug  // A vector holding the original and transformed data.
  ln       // Specifies the size of the problem
  N        // N = 2^ln
  w        // The smallest Nth root os unity other than 1.
  upb      // =N-1
  prupb    // =upb or 31 whichever is smaller.
}


LET start() = VALOF
{ LET argv = VEC 50

  UNLESS rdargs("ln/N,walsh/S", argv, 50) DO
  { writef("*nBad args for bigfft*n")
    RESULTIS 0
  }

//FOR e = 0 TO 20 DO writef("2^%n = %n*n", e, pow(2, e))
//abort(1000)

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

  w := 1

  UNLESS argv!1 WHILE w < modulus DO
  { LET x = 1
    FOR i = 1 TO N DO
    { x := mul(x, w)
      IF x=1 TEST i=N THEN GOTO rootfound
                      ELSE BREAK
    }
    w := w+1
  }

rootfound:
  //TEST w=1
  //THEN writef("Performing the Walsh transform on %n data values*n", N)
  //ELSE writef("Performing FFT on %n data values, w=%n*n*n", N, w)
FOR e = 0 TO 3 DO writef("w%n = %i5*n", e, pow(w, e))
  writef("*nInitial data N=%n w=%n*n*n", N, w)
  data := getvec(upb)

  FOR i = 0 TO upb DO data!i := i  // Set the test data
  pr(data, prupb)
// prints  -- Original data for FFT with ln=3
//     0     1     2     3     4     5     6     7

  writef("After FFT*n")
  fft(data, ln, w)
  pr(data, prupb)
// prints   -- Transformed data
// 65017 26645 38448 37467 30114 19936 15550 42679

  writef("After inverse FFT*n")
  fft(data, ln, div(1,w))
  FOR i = 0 TO upb DO data!i := div(data!i, N)
  //writef("*nAfter dividing the results by %n*n", upb)
  pr(data, prupb)
// prints  -- Restored data
//     0     1     2     3     4     5     6     7
  RESULTIS 0
}

AND fftorig(v, ln, w) BE  // ln = log2 n    w = nth root of unity
{ LET n = 1<<ln
  LET vn = v+n
  LET n2 = n>>1

  writef("*nDoing FFT on %n data words, w=%n*n", n, w)
  pr(v, prupb)

  // First do the perfect shuffle
  shuffle(v, n)
  writef("After the perfect shuffle*n")
  pr(v, prupb)

  // Then do all the butterfly operations
  FOR s = 1 TO ln DO
  { LET m  = 1<<s
    LET m2 = m>>1
    // Find the appropriate power of w for this level (s)
    LET wk, wkfac = 1, w
    FOR i = s+1 TO ln DO wkfac := mul(wkfac, wkfac)

    FOR j = 0 TO m2-1 DO
    { LET p = v+j
      WHILE p<vn DO { butterfly(p, p+m2, wk); p := p+m }
      wk := mul(wk, wkfac)
    }
    writef("After phase %n*n", s)
    pr(v, prupb)
  }
}

AND fft(v, ln, w) BE  // ln = log2 n    w = nth root of unity
{ LET n = 1<<ln
  LET sep = 1

  //writef("*nDoing FFT on %n data words, w=%n*n", n, w)
  //pr(v, prupb)

  // First do the perfect shuffle
  shuffle(v, n)
  writef("After the perfect shuffle*n")
  pr(v, prupb)

  // Then do all the butterfly operations
  // Do all the butterfly operations
  FOR k = ln TO 1 BY -1 DO
  { FOR p = 0 TO n-1 DO
    { LET q = p XOR sep
      IF q<p DO
      { LET e = (q<<(k-1)) MOD n
        LET we = pow(w, e)
//writef("  q=%i2 p=%i2 e=%n", q, p, e) 
        butterfly(v+q, v+p, we)
      }
    }
    writef("*nAfter phase %n*n", ln-k+1)
    pr(data, prupb)
    sep := 2 * sep
  }
}

AND butterfly(p, q, we) BE { LET a, b = !p, mul(!q, we)
                             !p, !q := add(a, b), sub(a, b)
                           }

AND reverse(x, ln) = VALOF
{ LET res = x & 1

  WHILE ln>1 DO
  { x, ln := x>>1, ln-1
    res := res<<1 | x&1
  }

  RESULTIS res
}

AND shuffle(v, n) BE
{ FOR i = 0 TO n-1 DO
  { LET j = reverse(i, ln)
    IF i<j DO { LET t = v!j; v!j := v!i; v!i := t }
  }
}

AND pr(v, max) BE
{ FOR i = 0 TO max DO { writef("%i5 ", v!i)
                        IF i MOD 8 = 7 DO newline()
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

AND pow(x, e) = VALOF
{ LET res = 1
  LET xi = x
  WHILE e DO
  { IF (e&1)>0 DO res := mul(res, xi)
    e := e>>1
    xi := mul(xi, xi)
  }
  RESULTIS res
}
