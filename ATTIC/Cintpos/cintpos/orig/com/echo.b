// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "ECHO"

GET "libhdr"

LET start() BE
  $( LET v = VEC 80

     IF rdargs("",v,80) = 0 RETURN

     IF v!0 = 0 RETURN

     writef("%s*n", v!0)
  $)

