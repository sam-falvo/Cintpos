GET "libhdr"

LET start() = VALOF
{ t(-1)
  t(0)
  t(15)
  t(222)
  t(223)
  t(224)
  t( 1000)
  t(-1000)
  t( 1000000)
  t(-1000000)
  t( 1000000000)
  t(-1000000000)
  t(-2)
writef("(-1)/2=%n*n", (-1)/2)
//writef("(1000000)/32=%32b*n", (1000000)/32)
//writef("(1000000)>>5=%32b*n", (1000000)>>5)
//writef("(-1000000)/32=%32b*n", (-1000000)/32)
//writef("(-1000000)>>5=%32b*n", (-1000000)>>5)
  RESULTIS 0
}

AND t(n) BE
{ //writef("%12i%-  %32b*n", n)
  wrn(n)
  wrn1(n)
  newline()
}

AND wrn(n) BE
{ writef("*n%12i%- %32b ", n)
  IF n=-1 DO
  { writef(" %i3%-(%8b)", 223)
    RETURN
  }
  IF 0<=n<223 DO    // This is the normal case
  { writef(" %i3%-(%8b)", n)
    RETURN
  }
  writef(" %i3%-(%8b)", 224 + (n&31))
  n := n & (~31)
  n := n/32
} REPEAT

AND wrn1(n) BE
{ writef("*n%12i%- %32b ", n)
  IF n=-1 DO
  { writef("%i3%-(%8b)", 223)
    RETURN
  }
  IF 0<=n<223 DO    // This is the normal case
  { writef(" %i3%-(%8b)", n)
    RETURN
  }
  writef(" %i3%-(%8b)", 224 + (n&31))
  n := n>>5
} REPEAT
