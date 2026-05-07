MODULE stack

GET "mcpl.h"
GET "mcli.h"

FUN start : => // PROMPT [PROMPT] prompt
  LET argv = VEC 15
  LET size = 0

  IF rdargs("SIZE", argv, 15) = 0 DO
  { writes("Parameters no good for PROMPT\n")
    RETURN 20
  }

  IF argv!0=0 DO
  { writef("current stack size is %d\n", cli_defaultstack)
    RETURN 0
  }

  size := str2numb(argv!0)

  IF size<100 DO
  { writes("suggested stack size too small*n")
    RETURN 20
  }

  cli_defaultstack := size
  RETURN 0
.

