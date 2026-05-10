GET "libhdr"

MANIFEST {
   Modulus = #x10001  // 2**16 + 1

//Omega = #x00003; N = #x10000
//Omega = #x0ADF3; N = #x01000

Omega = #x096ED; N = #x00400

//Omega = #x08000; N = #x00010
//Omega = #x0FFF1; N = #x00008

// Omega and N are chosen so that:  Omega**N = 1

   Upb     = N - 1  // N is a power of 2
   MSB     = N>>1
   LSB     = 1
}

STATIC { v; w }  

LET pr : v, upb BE
{ IF upb > Upb DO upb := Upb
  FOR i = 0 TO upb DO
  { writef("%5i ", v!i)
    IF i MOD 8 = 7 DO newline()
  }
  newline()
}

LET add : x, y => VALOF
{ LET a = x+y
  RESULTIS 0<=a<Modulus -> a, a-Modulus
}

LET sub : x, y => add(x, Modulus-y)

LET dv
: 1, m,    ?, ? => m
: 0, m,    ?, n => m-n
: a, m, >a b, n => dv(      a,         m, b MOD a, m*(b/a)+n)
: a, m,    b, n => dv(a MOD b, n*(a/b)+m,       b,         n)

LET mul : 0, ? => 0
        : x, y => (x&1)=0 -> mul(x>>1, add(y,y)),
                             add(y, mul(x>>1, add(y,y)))

LET inv : x => dv(x, 1, Modulus-x, 1)

LET ovr : x, y => mul(x, inv(y))

LET reorder
:   p,  <p, 0, ? BE RETURN
: [x], [y], 0, ? BE { LET t = x; x := y; y := t }
:   p,   q, a, b BE { LET a1, b1 =a>>1, b<<1
                      reorder(@p!a, @q!b, a1, b1)
                      reorder(p,    q,    a1, b1)
		    }

LET butterfly : [x], [y], wk BE
{ LET t = mul(y, wk)
  LET nx, ny = add(x, t), sub(x, t)
  x, y := nx, ny
}

LET fft
: nn, v, pp, msb BE { LET n, p = nn>>1, pp>>1
                      FOR i = 0 TO n-1 DO butterfly(@v!i, @v!(i+n), w!p)
                      IF n=1 RETURN
                      fft(n,   v,     p, msb)
                      fft(n, v+n, msb+p, msb)
		    }

LET dofft : v BE
{ w!0 := 1         // Nth roots of unity
  FOR i = 1 TO Upb DO w!i := mul(w!(i-1), Omega)
  fft(N, v, 0, MSB)
  reorder(v, v, MSB, LSB)
}

LET invfft : v BE
{ w!0 := 1         // w will hold the N inverse Nth roots of unity
  FOR i = 1 TO Upb DO w!i := ovr(w!(i-1), Omega)
  fft(N, v, 0, MSB)
  reorder(v, v, MSB, LSB)
  FOR i = 0 TO Upb DO v!i := ovr(v!i, N)
}

LET start : => VALOF
{ LET a = 1    // First check Omega has the right properties
  LET v1 = VEC Upb
  LET w1 = VEC Upb

  v := v1
  w := w1

  FOR i = 1 TO Upb DO
  { a := mul(a, Omega)
    IF a=1 DO writef("Omega****%n = 1*n", i)
  }
  UNLESS mul(a, Omega)=1 DO writef("Omega****%n ~= 1*n", N)

  FOR i = 0 TO Upb DO v!i := i

  pr(v, 15)
// prints  -- Original data
//     0     1     2     3     4     5     6     7
//     8     9    10    11    12    13    14    15

  dofft(v)

  pr(v, 15)
// prints   -- Transformed data
// 65017 26645 38448 37467 30114 19936 15550 42679
// 39624 42461 43051 65322 18552 37123 60445 26804

  invfft(v)

  pr(v, 15)
// prints  -- Inverse transform of transformed data
//     0     1     2     3     4     5     6     7
//     8     9    10    11    12    13    14    15
  RESULTIS 0
}


