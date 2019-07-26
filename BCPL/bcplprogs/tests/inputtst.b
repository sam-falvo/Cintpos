GET "libhdr"

LET start() = VALOF
{
  sawritef("*nType some characters terminated by '.'*n")

  sawritef("*nInput using rdch()*n")
  { LET ch = rdch()
    IF ch=endstreamch | ch='.' BREAK
    sawritef("ch=%i3", ch)
    IF ch>=32 DO sawritef(" '%c'", ch)
    sawrch('*n')
  } REPEAT

  sawritef("*nInput using sardch()*n")
  { LET ch = sardch()
    IF ch=endstreamch | ch='.' BREAK
    sawritef("ch=%i3", ch)
    IF ch>=32 DO sawritef(" '%c'", ch)
    sawrch('*n')
  } REPEAT

  sawritef("*nInput using sys(Sys_pollsardch)*n")
  { LET ch = sys(Sys_pollsardch)
    IF ch=endstreamch | ch='.' BREAK
    sawritef("ch=%i3", ch)
    IF ch>=32 DO sawritef(" '%c'", ch)
    sawrch('*n')
    delay(200)
  } REPEAT

  sawritef("*nEnd of test*n")
  RESULTIS 0
}
