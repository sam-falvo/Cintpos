// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "ECHO"

GET "g/libhdr.h"

LET start() BE
  $( LET v = VEC 80

     IF rdargs("",v,80) = 0 THEN
       RETURN

     IF v!0 = 0 THEN RETURN

     writes(v!0); wrch('*N')
  $)

