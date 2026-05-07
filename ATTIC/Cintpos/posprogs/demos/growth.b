GET "libhdr"

GLOBAL { C:200 }

MANIFEST { Upb=100000; step=1000 }

LET start() = VALOF
{ LET k = 2
  LET count = 0
  C := getvec(Upb)

  C!0 := 1
  FOR n = 1 TO Upb DO
  {
//    C!n := n + C!(n/k) + C!(n*(k-1)/k)
//    C!n := n/5 + C!(n/k) + C!(n*(k-1)/k)
    C!n := 1 + C!(n/k) + C!(n*(k-1)/k)
//    C!n := 1 + C!(n/2)
//    C!n := 1 + C!(n-1)
//    C!n := 1 + C!(n-1) + C!(n/2)
  }

  FOR n = 0 TO Upb UNLESS n REM step DO
  { UNLESS count REM 10 DO writef("*n%i6: ", n)
    count := count+1
    writef(" %i7", C!n)
  }
  newline()
  freevec(C)
  RESULTIS 0
}
