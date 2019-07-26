GET "libhdr"

LET start() = VALOF
{ LET k = sys(Sys_settrcount, 0)
  LET p = 0

  writef("initial trcount=%n*n", k)
  delay(1000)
/*
  FOR i = 0 TO 4100 DO sys(Sys_trpush, i)
  k := sys(Sys_settrcount, -1)
  writef("After 4101 calls of trpush, trcount=%n*n", k)
  writef("The circular buffer values are:*n")
  FOR i = 0 TO 4100 DO
  { IF i MOD 10 = 0 DO writef("*n%i4: ", i)
    writef(" %i6", sys(Sys_gettrval, i))
  }
*/
  // Stop trpushing
  k := sys(Sys_settrcount, -1)
  writef("Number of trpush values = %n*n", k)
  p := k - 4096
  IF p<0 DO p := 0
  FOR i = p TO k-1 DO
  { LET val = sys(Sys_gettrval, i)
    IF (i-p) MOD 8 = 0 DO writef("*n%i4:", i-p)
    writef(" %i5:%x2", val & #xFFFFFF, val>>24) 
  }
  newline()
  
  RESULTIS 0
}
