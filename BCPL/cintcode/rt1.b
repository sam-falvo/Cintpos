GET "libhdr"

LET start() = VALOF
{ LET upbb = 200000
  LET upb  = upbb/4
  LET v = getvec(upb)
  FOR i = 0 TO upb DO v!i := f()
  FOR i = 1 TO 200000 DO
  { LET p = randno(upbb)
    LET x = 0 //v%p
    IF randno(1000) < 200 DO v!(p/4) := 0
  }
  FOR i = 0 TO upb DO v!i := f()

  RESULTIS 0
}

AND f() = VALOF
{ LET a = 0
  FOR i = 1 TO 1 DO a := a+1
  RESULTIS a
}
  
