/*
This is a program designed to generate an interesting sound
when run under: raster sound.

Written by martin Richards (c) Sept 2016

A good choice is of rast2wav call is:

rast2wav b 20000
*/

GET "libhdr"

GLOBAL {
  stdin: ug
  c1; c2
  p;  q
}

LET start() = VALOF
{ FOR i = 1 TO 10 DO f(randno(50)/i+5, 20*i+10)
  RESULTIS 0
}

AND f(a, b) BE
{ c1 := a
  c2 := b
  p := 32
  q := 64
  !p, !q := 0, 0
  //abort(1000)
  writef("a=%i5  b=%i5*n", a,b)
  g(a, b)
}

AND g(x, y) BE
{ FOR t = 1 TO c1 DO
  { FOR i = 1 TO x DO !p := !p+!p
    FOR i = 1 TO y DO !q := !q+!q
  }
  FOR t = 1 TO c2 DO !p := !q
}
