GET "libhdr"

LET start() = VALOF
{ t(7, 5, 8)
  t(7000, 5000, 8000000)
  t(70000, 50000, 800000000)
  t(700000, 500000, 80000000000)
  t(7000000, 5000000, 8000000000000)
  t(70000000, 50000000, 800000000000000)
  RESULTIS 0
}

AND t(a, b, c) BE
{ //writef("%12i%-  %32b*n", n)
  LET q = muldiv(a, b, c)
  LET r = result2
  writef("muldiv(%n, %n, %n) => %n  remainder %n*n", a,b,c, q,r)
  newline()
}
