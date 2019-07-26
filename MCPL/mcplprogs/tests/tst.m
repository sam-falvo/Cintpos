GET "mcpl.h"

FUN start :  =>
  LET v = VEC 1

  { LET ch = 111 }

  LET a = 123
  v!0 := freedom 1111
  v!1 := freedom 2222

  writef("%2d %2d\n", v!0, v!1)

  RETURN 0

FUN freedom:p => p
