MODULE prompt

GET "mcpl.h"
GET "mcli.h"

FUN start : => // prompt [[prompt] format]
  LET v = VEC 15

  IF rdargs("PROMPT",v,15) = 0 DO
  { writes("Parameters no good for PROMPT\n")
    RETURN 20
  }

  LET prompt = v!0
  UNLESS prompt DO prompt := "%d> "
  LET i = 0

  cli_prompt%i := prompt%i REPEATUNTIL prompt%i++=0

  RETURN 0
.
