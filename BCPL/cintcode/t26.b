GET "libhdr"
GLOBAL { f:ug; g500:500 }

LET start() = VALOF
{ // This is to check that the slow interpreter detects
  // both p and pc being out of range.
  GOTO 116000000
  //GOTO g500
  //longjump(level(), g500)
  //longjump(16000000, g500)    // This causes stack overflow.
  RESULTIS f(0)
}

AND f(n) = VALOF
{ LET v = VEC 1000
  writef("@n=%n level() => %n*n", @n, level())
  abort(1234)
  RESULTIS f(n+1) // An efficient(!) recursive loop.
}
