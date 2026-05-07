SECTION "REM"

GET "libhdr"

LET start() BE
{ LET ch=rdch()
  UNTIL ch=endstreamch | ch='*n' | ch=';' DO ch:=rdch()
}

