GET "libhdr"
GET "manhdr"

LET start() = VALOF
{
  hold(3) // The console handler

  nl()

  { LET ch = drdch()
    IF ch=endstreamch BREAK
    sawritef("%i3:", ch)
    IF 32<=ch<=126 DO sawritef(" '%c'", ch)
    IF ch='.' BREAK
    nl()
  } REPEAT

  nl()
  unhold(3) // The console handler

  RESULTIS 0
}

AND nl() BE sawrch('*n')

AND drdch() = sendpkt(notinuse, -2, Action_ttyin, 0, 0)
