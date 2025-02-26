// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "WAIT"

GET "g/libhdr.h"

LET start() BE
 $( LET v = VEC 50
    LET secs = 0
    LET mins = 0
    LET n = 0

    IF rdargs(",SEC=SECS/S,MIN=MINS/S,UNTIL/K",v,50)=0 DO
       error(1)

    TEST v!3=0 THEN
    $( TEST v!0=0 THEN
          n := 1
       ELSE
       $( LET s = v!0
          FOR i = 1 TO s%0 DO
          $( LET ch = s%i
             UNLESS '0'<=ch<='9' DO error(2)
             n := n*10+ch-'0'
          $)
       $)

       TEST v!2 THEN
          mins := n
       ELSE
       $( mins := (n>>1)/30
          secs := n-mins*60 $)
    $)

    ELSE
    $( LET s = v!3
       LET hour = 0
       LET min = 0
       UNLESS s%0=5 DO error(3)
       FOR i=1 TO 5 DO
          UNLESS i=3 -> s%i=':', '0'<=s%i<='9' DO error(3)
       hour := (s%1-'0')*10+s%2-'0'
       IF hour>=24 DO error(3)
       min := (s%4-'0')*10+s%5-'0'
       IF min>=60 DO error(3)
       mins := hour*60+min-rootnode!Rtn_mins
       IF mins<0 DO mins := mins+24*60
    $)

    FOR i = 0 TO mins DO
       FOR j = 1 TO i=0 -> secs,60 DO
       $( delay(tickspersecond)
          IF testflags(1) DO
          $( writes("****BREAK*N")
             stop(10) $)
       $)
 $)


AND error(n) BE
 $( writes(n=1 -> "Bad args*N",
           n=2 -> "Error in number*N",
                  "Time should be HH:MM*N")
    stop(20)
 $)
