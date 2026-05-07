GET "mcpl.h"

MANIFEST Upb=100

FUN new : args =>
  LET p=args!0, nx2=args!1, nx3=args!2, nx5=args!3
  LET x2=1, x3=1, x5=1
  cowait()        // End of initialisation.

  { LET val = x2<x3 -> (x2<x5 -> x2, x5),
                       (x3<x5 -> x3, x5)
    IF intflag() DO abort 98
    cowait(val)                   // Return next value.
    !p+++ := val
    IF val=x2 DO x2 := callco nx2
    IF val=x3 DO x3 := callco nx3
    IF val=x5 DO x5 := callco nx5
  } REPEAT
.
FUN mul : args =>
  LET p=args!0, k=args!1
  cowait()                  // End of initialisation.
   
  cowait(k * !p+++) REPEAT  // Return next value
.

FUN start : =>
  LET v = getvec Upb
  LET nx2  = initco(mul, 100, v, 2)
  LET nx3  = initco(mul, 100, v, 3)
  LET nx5  = initco(mul, 100, v, 5)
  LET next = initco(new, 100, v, nx2, nx3, nx5)
   
  FOR i = 1 TO Upb DO { writef(" %6d", callco(next))
                        UNLESS i MOD 10 DO newline()
                      }

  deleteco nx2; deleteco nx3; deleteco nx5; deleteco next
  freevec v
  RETURN 0
.
