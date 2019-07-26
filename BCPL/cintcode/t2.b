GET "libhdr"

LET start() = VALOF
{ LET x = 1.2 #+ 2
  writef("Testing writee*n")
  writee(#x3F800000, 13)
  newline()
  writef("*n1.234=%13.3f  x=%13.3f*n", 1.234, x)
  RESULTIS 0
}

