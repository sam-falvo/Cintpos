SECTION "TEST"

GET "libhdr"

GLOBAL { test:ug }

LET test(x) = VALOF
{ writef("Test entered, x=%n*n")
  RESULTIS 0
}

.

SECTION "TEST1"

GET "libhdr"

GLOBAL { test1:ug+1 }

LET test1(x) = VALOF
{ writef("Test1 entered, x=%n*n")
  RESULTIS 0
}
