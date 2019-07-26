SECTION "DUMMYLIB"

GET "libhdr"

GLOBAL {
  dummylib:300
}

LET dummylib(x) = VALOF
{ writef("dummylib: entered*n", x)
  RESULTIS 123
}
