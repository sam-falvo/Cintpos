GET "libhdr"

LET start() = VALOF
{
  newline()

  { LET ch = rdch()
    IF ch=endstreamch BREAK
    writef("%i3:", ch)
    IF 32<=ch<=126 DO writef(" '%c'", ch)
    IF ch='.' BREAK
    newline()
  } REPEAT

  newline()
  RESULTIS 0
}
