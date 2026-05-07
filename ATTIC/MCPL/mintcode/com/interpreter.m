GET "mcpl.h"

FUN start : =>
  LET argv=VEC 10, val=-1
   
   IF rdargs("FAST/S,SLOW/S", argv, 10)=0 DO
   { writef("Bad argument for INTERPRETER\n")
     RETURN 20
   }
   
   IF argv!0 DO val := -1 // select fast interpreter (mintasm) 
   IF argv!1 DO val := -2 // select slow interpreter (minterp) 
   
   sys(0, val) // make selection

   writef("%s interpreter selected\n", val=-1 -> "Fast", "Slow")
   RETURN 0
.
