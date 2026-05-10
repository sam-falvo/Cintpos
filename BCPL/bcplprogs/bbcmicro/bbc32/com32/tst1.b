section "BBCLIB"
get "libhdr.h"
get "LIBHDR.h"
get "SYSHDR.h"
get "com32/BBCLIB.b"

.

section "TST1"

get "libhdr.h"
get "LIBHDR.h"
get "SYSHDR.h"

let start() = valof
$( 
   WRITEF("Hello World!*n")
   resultis 0
$)
