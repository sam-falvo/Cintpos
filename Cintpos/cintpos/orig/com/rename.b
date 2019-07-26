// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "RENAME"

GET "g/libhdr.h"

LET start() BE
$( LET v = VEC 50
   IF rdargs("FROM/A,TO=AS/A", v, 50)=0 DO
   $( writes("Bad args*N")
      stop(20) $)
   IF renameobj(v!0, v!1)=0 DO
   $( LET res2 = result2
      writef("Can't rename %s as %s*n", v!0, v!1)
      result2 := res2
      stop(20) $)
$)
