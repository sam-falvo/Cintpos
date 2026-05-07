// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

/*      REPEAT COMMAND

        Repeat a command line

        The stream character pointer is backspaced to
        the beginning of the line
*/


SECTION "REPEAT"

GET "g/libhdr.h"


GET "g/clihdr.h"


GET "g/iohdr.h"


LET start() BE
$( UNLESS input()!Scb_type<=0 DO
   $( writes("REPEAT not allowed in C command*n")
      stop(Return_severe)
   $)
   UNLESS testflags(4) UNTIL NOT unrdch() LOOP
$)
