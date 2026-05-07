GET "libhdr"

LET start() = VALOF
{ wrs("ABCD*n")
  RESULTIS 0
}

AND wrs(s) BE FOR i = 1 TO s%0 DO sawrch(s%i)

