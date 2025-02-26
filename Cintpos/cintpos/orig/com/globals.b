// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "GLOBALS"

GET "LIBHDR"
GET "CLIHDR"
GET "IOHDR"

MANIFEST
$(
promptmax  = 15
filemax    = 10
pkt.arg7   = pkt.arg6+1
pkt.arg8   = pkt.arg7+1
pkt.arg9   = pkt.arg8+1
pkt.arg10  = pkt.arg9+1
pktmax     = pkt.arg10+1+promptmax+1+filemax
$)

LET start() BE
 $( LET parm.pkt = 0
    LET inscb, outscb = 0, 0
    LET number = 0
    LET ch = 0

    ch := rdch() REPEATWHILE ch='*S'

    IF ch='*N' | ch='*E' | ch=';' | ch=endstreamch DO
    $( writef("Current global vector size is %N*N", globsize)
       RETURN $)

    unrdch()
    number := readn()
    IF number<ug+50 DO
    $( writef("Number too small - at least %N required*N", ug+50)
       stop(20) $)

    UNTIL ch='*N' | ch='*E' | ch=';' | ch=endstreamch DO
       ch := rdch()

    parm.pkt := getvec(pktmax)
    inscb := getvec(scb.upb)
    outscb := getvec(scb.upb)
    IF parm.pkt=0 | inscb=0 | outscb=0 DO
    $( writes("Insufficient store*N")
       freevec(parm.pkt)
       freevec(inscb)
       freevec(outscb)
       stop(20)
    $)

    FOR i = 0 TO scb.upb DO
      inscb!i, outscb!i := 0, 0
    inscb!scb.id := id.inscb
    outscb!scb.id := id.outscb
    inscb!scb.pos, inscb!scb.end := 1, 1
    inscb!scb.buf := inscb+scb.arg1
    inscb!scb.buf%0 := 0

    parm.pkt!pkt.link := notinuse
    parm.pkt!pkt.taskid := taskid
    parm.pkt!pkt.arg1 := currentdir
    parm.pkt!pkt.arg2 := consoletask
    parm.pkt!pkt.arg3 := cli.commanddir
    parm.pkt!pkt.arg4 := cli.background
    parm.pkt!pkt.arg5 := cli.faillevel
    parm.pkt!pkt.arg6 := cli.defaultstack
    parm.pkt!pkt.arg7 := cli.standardinput
    parm.pkt!pkt.arg8 := cli.currentinput
    parm.pkt!pkt.arg9 := cli.standardoutput
    parm.pkt!pkt.arg10:= cli.currentoutput

    FOR i = 0 TO promptmax DO
       (parm.pkt+pkt.arg10+1)!i := cli.prompt!i
    FOR i = 0 TO filemax DO
       (parm.pkt+pkt.arg10+1+promptmax+1)!i := cli.commandfile!i

    cli.standardinput := inscb
    cli.currentinput := inscb
    cli.standardoutput := outscb
    cli.currentoutput := outscb

    currentdir := 0
    cli.commanddir := 0

    cli.background := TRUE

    tcb!tcb.seglist!3 := cli.module
    cli.module!(cli.module!1) := number
    cli.module := 0

    // Restart the task

    qpkt(parm.pkt)
 $)


LET cli.init(parm.pkt) = VALOF
  $( LET initialseg = tcb!tcb.seglist!3
     initio()
     currentdir := parm.pkt!pkt.arg1
     consoletask := parm.pkt!pkt.arg2
     cli.commanddir := parm.pkt!pkt.arg3
     cli.background := parm.pkt!pkt.arg4
     cli.faillevel := parm.pkt!pkt.arg5
     cli.defaultstack := parm.pkt!pkt.arg6
     cli.standardinput := parm.pkt!pkt.arg7
     cli.currentinput := parm.pkt!pkt.arg8
     cli.standardoutput := parm.pkt!pkt.arg9
     cli.currentoutput  := parm.pkt!pkt.arg10
     selectinput(cli.currentinput)
     selectoutput(cli.currentoutput)
     returncode := 0
     cli.returncode := 0
     cli.result2 := 0
     FOR i = 0 TO filemax DO
        cli.commandfile!i := (parm.pkt+pkt.arg10+1+promptmax+1)!i
     cli.module := 0
     FOR i = 0 TO promptmax DO
        cli.prompt!i := (parm.pkt+pkt.arg10+1)!i
     freevec(parm.pkt)
     tcb!tcb.seglist!3 := 0
     start := cli.undefglobval
     result2 := initialseg
     RESULTIS unloadseg
  $)
