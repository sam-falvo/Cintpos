MODULE t1

GET "mcpl.h"

FUN start : =>
  LET argv = VEC 10
  LET n = 99
   
  UNLESS rdargs("NUMBER", argv, 10) DO
  { writef("Bad argument for ABORT\n")
    RETURN 20
  }
   
  IF argv!0 DO n := str2numb(argv!0)

  abort n
  RETURN 0
.
