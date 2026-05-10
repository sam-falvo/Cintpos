GET "libhdr"

GLOBAL { a:ug }

LET start() = VALOF
{ LET x = 99
  LET p = @x
  !p := !p+1
  
  RESULTIS !p
}

