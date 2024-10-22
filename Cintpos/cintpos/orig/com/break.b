// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

// Program to set the break flags of the specified task
SECTION "Break"
GET     "libhdr"


LET start() BE
    $(
    LET argv = VEC 20
    LET tasknum = ?
    LET flags = 0
    LET save_res2 = ?

    IF rdargs("task/a,all/s,b/s,c/s,d/s,e/s,f/s", argv, 20) = 0
    THEN $( writes("Bad args to BREAK*N"); stop(20) $)

    tasknum := stringval(argv!0)
    IF argv!1 ~= 0 THEN flags := flags | #B11111
    IF argv!2 ~= 0 THEN flags := flags | #B00001
    IF argv!3 ~= 0 THEN flags := flags | #B00010
    IF argv!4 ~= 0 THEN flags := flags | #B00100
    IF argv!5 ~= 0 THEN flags := flags | #B01000
    IF argv!6 ~= 0 THEN flags := flags | #B10000
    IF flags = 0 THEN flags := #B00001 // Default is B

    IF setflags(tasknum, flags) = 0
    THEN
      $( save_res2 := result2
         TEST result2 = 101
         THEN writef("Task %N does not exist*N", tasknum)
         ELSE writef("BREAK failed*N")
         result2 := save_res2
      $)
    $)


AND stringval(s) = VALOF
    $(  // converts a string to a number
    LET val = 0

    FOR j = 1 TO s%0
    DO
        $(
        UNLESS '0' <= s%j <= '9'
        THEN $( writef("Invalid char *'%C*' in task number*N", s%j)
                stop(20)
             $)
        val := val*10 + s%j - '0'
        $)

    RESULTIS val
    $)
