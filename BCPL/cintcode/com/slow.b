SECTION "SLOW"

GET "libhdr"

LET start() = VALOF 
{ sys(Sys_setcount, maxint)     // Select the slow interpreter.
  writef("Slow interpreter selected*n")
  RESULTIS 0
}
