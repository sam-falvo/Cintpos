SECTION "FAST"

GET "libhdr"

LET start() = VALOF 
{ sys(Sys_setcount, -1)     // Select the fast interpreter.
  writef("Fast interpreter selected*n")
  RESULTIS 0
}
