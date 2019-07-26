// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "PROMPT"

GET "g/libhdr.h"
GET "g/clihdr.h"

LET start() BE // PROMPT [PROMPT] prompt
  $( LET v = VEC 15

     IF rdargs("PROMPT",v,15) = 0 THEN
       $( writes("Parameters no good for PROMPT*N")
          RETURN
       $)

     IF v!0=0 DO v!0 := "> "
     FOR j = 0 TO (v!0) % 0 DO
       cli_prompt % j := (v!0) % j
  $)
