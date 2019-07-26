GET "libhdr"

LET start() = VALOF
{ LET FLT x = 10.0
  LET FLT y = 12345.678

  //writef("12345.678 |%13.4e|%13.4f|*n", y, y)
  //y := x/3.0
  //writef("10.0/3.0  |%13.4e|%13.4f|*n", y, y)
  //y := -x/1.5 
  //writef("-10.0/1.5 |%13.4e|%13.4f|*n", y, y)

  FOR w = 1 TO 13 DO
  { writef("w=%i2 p=%i2 |", w, 3)
    writee(0.0, w, 3)
    writef("|")
    writee(y, w, 3)
    writef("|")
    writee(-y, w, 3)
    writef("|")
    writeflt(y, w, 3)
    writef("|")
    writeflt(-y, w, 3)
    writef("|*n")

  }

  FOR p = 0 TO 10 DO
  { writef("w=%i2 p=%i2 |", 10, p)
    writee(0.0, 10, p)
    writef("|")
    writee(y, 10, p)
    writef("|")
    writee(-y, 10, p)
    writef("|")
    writeflt(y, 10, p)
    writef("|")
    writeflt(-y, 10, p)
    writef("|*n")

  }

  RESULTIS 0
}


