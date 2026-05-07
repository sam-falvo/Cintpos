SECTION "SLOW"

GET "libhdr"

LET start() = VALOF 
{ LET val = maxint
   
  sys(Sys_setcount, val)     // Select the slow or fast interpreter.

  writef("%s interpreter selected*n", val=-1 -> "Fast", "Slow")
  RESULTIS 0
}
