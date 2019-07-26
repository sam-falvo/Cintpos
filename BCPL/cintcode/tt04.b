GET "libhdr"

LET start() = VALOF
{ FOR i = 0 TO 100 DO
  { LET w = 1<<i
    writef("%i3: %n*n", i, w)
    IF w=0 BREAK
  }
  writef("%n %n %16x %16x*n", bytesperword, bitsperword, minint, maxint)
  writef("%16x %16x*n", minint-1, minint-2)
  RESULTIS 0
}

