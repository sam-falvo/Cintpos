/*
This program prints the contents of the circular trace buffer.

Implementented by Martin Richards (c) September 2005
*/

SECTION "Tracebuf"

GET "libhdr"

LET start() = VALOF
{ LET buf = rootnode!rtn_trbuf
  AND p = 0

  UNLESS rootnode!rtn_trword = #xBFBFBFBF DO
  { writef("The trace buffer is not active*n")
    RESULTIS 0
  }

//trpush(#x1234)
//FOR i = 1 TO 10 DO trpush(i)
//trpush(#x1234)

  rootnode!rtn_trword := 0         // Disable the trace buffer

  writef("*nTracebuf: size=%n p=%n*n", buf!0-1, buf!1)

  p := buf!1
  FOR i = 0 TO buf!0-2 DO
  { IF i REM 8 = 0 DO newline()
    writef(" %x8", buf!p)
    p := p-1
    IF p<2 DO p := buf!0
  }
  newline()    
  rootnode!rtn_trword := #xBFBFBFBF

  RESULTIS 0
}
