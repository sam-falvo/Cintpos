SECTION "ABORT"

GET "libhdr"

LET start() = VALOF 
$( LET argv = VEC 10
   AND n = 99
   
   IF rdargs("NUMBER", argv, 10)=0 DO
   $( writef("Bad argument for ABORT*n")
      RESULTIS 20
   $)
   
   UNLESS argv!0=0 DO n := str2numb(argv!0)
   
   abort(n)
   RESULTIS 0
$)
