GET "libhdr"

MANIFEST {
  S = SLCT 4:8:5
}

LET start() = VALOF
{ LET x = 1
  x +:= 2
  x := S::x
  RESULTIS 0
}
