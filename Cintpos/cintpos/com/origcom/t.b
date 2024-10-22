// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

// Command to execute a command sequence file with no parameter substitution.

SECTION "T"

GET "LIBHDR"
GET "CLIHDR"

LET start() BE
 $( LET v = VEC 25
    LET s = 0
    LET ifexistsopt        = ?

    IF rdargs("FILE/A,IFEXISTS/S", v, 25)=0 DO
    $( writes("Bad args*N")
       stop(20)
    $)

    ifexistsopt    := v!1 \= 0

    s := findinput(v!0)
    IF s=0 DO
    $( LET dir = currentdir
       currentdir := locatedir("SYS:S")
       s := findinput(v!0)
       freeobj(currentdir)
       currentdir := dir
       IF s = 0
       THEN TEST ifexistsopt
            THEN stop(0) // ifexists specified: no message, zero returncode
            ELSE $( writef("T: can't open %S*N", v!0); stop(20) $)
    $)

    IF cli.interactive DO testflags(2)
    UNLESS cli.currentinput=cli.standardinput DO
    $( selectinput(cli.currentinput)
       endread()
    $)
    cli.currentinput := s
 $)
