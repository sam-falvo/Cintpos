// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "INPUT"

GET "libhdr"

//GET "g/clihdr.h"

LET start() BE
  $( LET argv = VEC 50
     LET save = VEC 50
     LET tlen = ?
     LET term, stream = "/**", ?
     IF rdargs("TO/A,TERM/K",argv,50)=0 THEN
       $( writes("Bad args*N")
          stop(20)
       $)
     stream := findoutput(argv!0)
     IF stream=0 DO
     $( writef("Can't open %S*N", argv!0)
        stop(20)
     $)

     selectoutput(stream)

     IF argv!1 ~= 0 THEN term := argv!1
     tlen := term%0

     $( LET t, ch = 1, ?
        ch := rdch()
        WHILE t<=tlen & compch(ch,term%t)=0 & ch~='*N' DO
          $( save%t := ch
             t := t+1
             ch := rdch()
          $)
        IF t>tlen & ch='*N' THEN
          BREAK
        FOR j = 1 TO t-1 DO
          wrch(save%j)
        IF ch=endstreamch THEN GOTO ended
        wrch(ch)
        $( IF testflags(1) THEN
             $( writes("****BREAK*N")
                endwrite()
                stop(10)
             $)
           IF ch='*N' BREAK
           ch := rdch()
           IF ch=endstreamch THEN GOTO ended
           wrch(ch)
        $) REPEAT
     $) REPEAT

ended:
     endwrite()

  $)
