// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "DELETE"

GET "g/libhdr.h"

LET start() BE
 $( LET v = VEC 80
    LET rc = 0
    TEST rdargs(",,,,,,,,,", v, 80)=0 THEN
    $( writes("Bad args*N")
       rc := 20
    $)
    ELSE
       FOR i = 0 TO 9 DO
       $( IF v!i=0 BREAK
          IF deleteobj(v!i)=0 DO
          $( writef("Can't delete %s*n", v!i)
             rc := 5
          $)
       $)
    stop(rc)
 $)
