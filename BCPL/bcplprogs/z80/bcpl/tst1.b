GLOBAL {
start: 1
}

LET start() = VALOF
{ LET a, b, c = 0, 1, 15
  a := a + b
  b := b - c
  RESULTIS a+b
}
