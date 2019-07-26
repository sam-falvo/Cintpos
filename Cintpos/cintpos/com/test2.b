SECTION "Test2"

GET "libhdr"

GLOBAL {
  stdin:ug
  stdout
}


LET start() = VALOF
{ LET argv  = VEC 30
  LET a, b, c = 0, 0, 0

  UNLESS rdargs("A,B,C", argv, 30) DO
  { writef("Bad arguments for test2*n")
    stop(20)
  }

  b := rootnode!rtn_tasktab + 7
  c := b!0

  IF argv!0 & string.to.number(argv!0) DO a := result2
  IF argv!1 & string.to.number(argv!1) DO b := result2
  IF argv!2 & string.to.number(argv!2) DO c := result2

  writef("a=%n  b=%n  c=%n*n", a, b, c)

  writef("!a = %n*n", !a)
  RESULTIS 0
}


