// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "DAT"

GET "g/libhdr.h"

LET start() BE
$( LET v = VEC 14
   datstring(v)
   writef(" %S %S %S*N", v+10, v, v+5)
$)
