// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "TYPE"

GET "libhdr"

GLOBAL
$(
inputstream:250
outputstream:251
numbers:252
linenumber:253
$)

LET start() BE
 $( LET argv = VEC 50
    LET rc = 0
    LET ch = 0
    LET oldoutput = output()

    inputstream := 0
    outputstream := 0

    IF rdargs("FROM/A,TO,OPT/K", argv, 50) = 0 THEN
     $( writes("Bad args*N")
        rc := 20
        GOTO exit
     $)

//sawritef("type: 1*n")
    inputstream := findinput(argv!0)
//sawritef("type: 2*n")
    IF inputstream = 0 THEN
     $( writef("Can*'t open %S*N", argv!0)
        rc := 20
        GOTO exit
     $)
    selectinput(inputstream)

    UNLESS argv!1=0 DO
     $( outputstream := findoutput(argv!1)
        IF outputstream=0 DO
         $( writef("Can*'t open %S*N", argv!1)
            rc := 20
            GOTO exit
         $)
        selectoutput(outputstream)
     $)

    numbers := FALSE

    UNLESS argv!2=0 DO
     $( LET opts = argv!2
        FOR i = 1 TO opts%0 SWITCHON capitalch(opts%i) INTO
        $( CASE 'N': numbers := TRUE
                     ENDCASE
        $)
     $)

    linenumber := 1

    $( LET tab = 0

       $( ch := rdch()
//sawritef("type: ch = %n*n", ch)
          IF testflags(1) DO
           $( UNLESS tab=0 DO wrch('*N')
              selectoutput(oldoutput)
              writes("****BREAK*N")
              rc := 5
              GOTO exit
           $)
          IF tab=0 DO
           $( IF ch=endstreamch GOTO exit
              IF numbers DO writef("%I5  ", linenumber)
           $)
          SWITCHON ch INTO
          $( CASE '*C': CASE '*N': CASE '*P':
                linenumber := linenumber+1
                wrch(ch)
                BREAK

             CASE '*E':
                linenumber := linenumber+1
                tab := 8
                LOOP

             CASE '*T':
                $( wrch('*S')
                   tab := tab+1
                $) REPEATUNTIL tab REM 8 = 0
                LOOP

             DEFAULT:
                wrch(ch)
                tab := tab+1
                LOOP

             CASE endstreamch:
//                wrch('*N')
                GOTO exit
          $)
       $) REPEAT
    $) REPEAT

exit:
    UNLESS inputstream=0 DO
     $( selectinput(inputstream)
        endread() $)
    UNLESS outputstream=0 DO
     $( selectoutput(outputstream)
        endwrite() $)
    stop(rc)
 $)
