GET "libhdr"
GET "manhdr"

LET start() = VALOF
{
  setx(TRUE)

  newline()

  { LET ch = xrdch()
    IF ch=endstreamch BREAK
    writef("%i3:", ch)
    IF 32<=ch<=126 DO writef(" '%c'", ch)
    IF ch='.' BREAK
    newline()
  } REPEAT

  newline()
  setx(FALSE)

  RESULTIS 0
}

AND xrdch() = sendpkt(notinuse, 3, Action_exclusiverdch, 0, 0)

AND setx(flag) = sendpkt(notinuse, 3, Action_exclusiveinput, 0, 0, flag)
