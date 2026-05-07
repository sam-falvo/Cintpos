SECTION "ABORT"

GET "libhdr"

LET start() = VALOF 
{ LET argv = VEC 10
  AND n = 99

  UNLESS rdargs("NUMBER/N", argv, 10) DO
  { sawritef("Bad argument for ABORT*n")
    RESULTIS 20
  }
   
  IF argv!0 DO n := !(argv!0)

  // Allow COHAND to send its newline character to the screen
  delay(200) // MR 25/9/03
  abort(n)
  RESULTIS 0
}
