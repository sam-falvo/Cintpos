GET "libhdr"

LET start() = VALOF
{ // Hold the console handler
  hold(3)

  nl()

  { LET ch = sardch()
    IF ch=endstreamch BREAK
    sawritef("%i3:", ch)
    IF 32<=ch<=126 DO sawritef(" '%c'", ch)
    IF ch='.' BREAK
    nl()
  } REPEAT

  nl()
  // Unhold the console handler
  unhold(3)
  RESULTIS 0
}

AND nl() BE sawrch('*n')
