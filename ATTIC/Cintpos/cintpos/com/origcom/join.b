// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "JOIN"

GET "LIBHDR"

LET start() BE
 $( LET v = VEC 130
    LET s = VEC 12/bytesperword
    LET t = "T:JOIN-T00"
    LET oldoutput = output()
    LET inputstream = 0
    LET outputstream = 0
    LET rc = 0
    IF rdargs(",,,,,,,,,,,,,,,AS/A/K,CHARS/S", v, 100)=0 DO
    $( writef("Bad args*N")
       stop(20) $)
    FOR i = 0 TO t%0 DO s%i := t%i
    s%(s%0-1) := (taskid/10) REM 10 + '0'
    s%(s%0) := taskid REM 10 +'0'
    outputstream := findoutput(s)
    IF outputstream=0 DO
    $( writef("Can't open %S*N", s)
       stop(20) $)
    selectoutput(outputstream)
    FOR i = 0 TO 14 DO
    $( LET s = v!i
       IF s=0 BREAK
       inputstream := findinput(s)
       IF inputstream=0 DO
       $( endwrite()
          selectoutput(oldoutput)
          writef("Can't open %S*N", s)
          stop(20)
       $)
       selectinput(inputstream)
       TEST v!16 THEN
       $( LET ch = rdch()
          IF ch=endstreamch BREAK
          IF testflags(1) GOTO exit
          wrch(ch)
       $) REPEAT
       ELSE
       $( LET w = VEC 99
          LET n = 0
          $( n := readwords(w, 100)
             IF testflags(1) GOTO exit
             writewords(w, ABS n)
          $) REPEATUNTIL n<=0
       $)
       endread()
    $)
    endwrite()
    IF renameobj(s, v!15)=0 DO
    $( rc := 20
       selectoutput(oldoutput)
       writef("Can't rename %S as %S*N", s, v!15)
    $)
    stop(rc)

exit:
    endread()
    endwrite()
    selectoutput(oldoutput)
    writes("****BREAK*N")
    deleteobj(s)
    stop(10)
 $)
