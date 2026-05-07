// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

// Modified 15 Jan 82 by BJK to improve messages

SECTION "CHANGEPRI"

GET "libhdr"

LET start() BE
$( LET args             = VEC 50
   LET pri, task        = 0, taskid
   LET rdargs_string	= "TASK=PRIORITY/A,PRI"

   IF rdargs(rdargs_string, args, 50)=0
   THEN $( writef("Invalid args for key *"%S*"*N", rdargs_string); stop(20) $)

   IF args!1=0 DO
   $( args!1 := args!0
      args!0 := 0
   $)

   pri := strtonumb(args!1)
   UNLESS args!0=0 DO task := strtonumb(args!0)

   IF changepri(task,pri)=0
   THEN
     $(
     LET res2	= result2
     writes("CHANGEPRI failed*n")
//     writes("CHANGEPRI failed: "); fault(res2)
     result2	:= res2
     $)
$)

AND strtonumb(s) = VALOF
$( LET a = 0
   FOR i = 1 TO s%0 DO
   $( LET ch = s%i
      TEST '0'<=ch<='9' THEN a := 10*a + ch - '0'
                        ELSE RESULTIS 0
   $)
   RESULTIS a
$)
