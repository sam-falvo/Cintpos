MODULE echo

GET "mcpl.h"

FUN start : =>
  LET argv = VEC 80

  IF rdargs("TEXT,N/S", argv, 80)=0 DO
  { writes("Bad argument for ECHO\n")
    RETURN 20
  }

  IF argv!0 DO writes(argv!0)

  UNLESS argv!1 DO newline()

  RETURN 0
.
