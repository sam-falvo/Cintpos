GET "libhdr"

LET start() = VALOF
{ LET s1 = "Dvořák*n"
  writef(s1)

  FOR i = 1 TO s1%0 DO writef(" %x2", s1%i)
  newline()

  writef("Dvo*xC5*x99*xC3*xA1k*n")

  RESULTIS 0
  }
