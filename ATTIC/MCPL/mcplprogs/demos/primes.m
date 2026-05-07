MODULE primes

GET "mcpl.h"

MANIFEST Upb = 1000

FUN start : =>

  writef "\nTable of prime numbers\n\n"

  LET count=0, isprime=VEC Upb

  FOR i = 2 TO Upb DO isprime!i := TRUE

  FOR p = 2 TO Upb IF isprime!p DO
  { LET i = p*p
    UNTIL i>Upb DO { isprime!i := FALSE
                     i +:= p
                   }
    writef(" %3d", p)
    IF ++count MOD 10 = 0 DO writef "\n"
  }

  writef "\nEnd of output\n"

  RETURN 0
.


