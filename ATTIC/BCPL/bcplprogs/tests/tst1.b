?
GLOBAL { start:1; f:300 }

LET start() = VALOF
{ LET a = 12
  LET b = 24
  RESULTIS a/(2*a-b) + f(b)
}
