// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "NEWCLI"

GET "libhdr"
//GET "g/clihdr.h"
//GET "g/iohdr.h"


LET start() BE
 $( LET argv = VEC 50
    LET pkt = VEC pkt.arg6
    LET task = 0

    IF rdargs("DEV",argv,50)=0 GOTO err
    IF argv!0=0 DO argv!0 := "**"
//sawritef("newcli: creating a task*n")
    FOR i = 1000 TO 501 BY -1 DO
    $( task := createtask(tcb!tcb.seglist,
                          tcb!tcb.stsiz, i)
       UNLESS task=0 BREAK
    $)
    IF task=0 GOTO err
//sawritef("newcli: task %n created*n", task)

    cli.module!(cli.module!1) := globsize
    rootnode!rtn.tasktab!task!tcb.seglist!3 := cli.module

//sawritef("newcli: sending startup pkt*n")
    IF sendpkt(-1, task, 0, -1, 0,
               copyobj(currentdir),
//               copydir(currentdir),
               consoletask,
               argv!0,
               copyobj(cli.commanddir),
//               copydir(cli.commanddir),
               cli.defaultstack,
               cli.prompt)=0 GOTO err

//sawritef("newcli: returning to CLI*n")
    RETURN

err:writes("NEWCLI failed*N")
    stop(20)
 $)


LET cli.init(pkt) = VALOF
 $( initio()
//sawritef("newcli: new task starting*n")
    currentdir := pkt!pkt.arg1
    consoletask := pkt!pkt.arg2
    cli.currentinput := findinput(pkt!pkt.arg3)
    selectinput(cli.currentinput)
//sawritef("newcli: new task starting*n")
    UNLESS cli.currentinput=0 DO
    $( cli.currentoutput := findoutput(pkt!pkt.arg3)
       selectoutput(cli.currentoutput)
    $)
//sawritef("newcli: new task starting*n")
    IF cli.currentinput=0 | cli.currentoutput=0 DO
    $( endread()
       endwrite()
       returnpkt(pkt,0,result2)
       result2 := taskid
       RESULTIS deletetask
    $)
//sawritef("newcli: new task starting*n")
    UNLESS compstring(pkt!pkt.arg3,"**")=0 DO
       consoletask := ABS cli.currentinput!scb.type
    cli.background := FALSE
    cli.standardinput := cli.currentinput
    cli.standardoutput := cli.currentoutput
    cli.commanddir := pkt!pkt.arg4
    returncode := 0
    cli.returncode := 0
    cli.faillevel  := cli.initialfaillevel
    cli.result2 := 0
    cli.commandfile%0 := 0
    cli.defaultstack := pkt!pkt.arg5
    cli.module := 0
//sawritef("newcli: new task starting*n")
    FOR i = 0 TO pkt!pkt.arg6%0 DO
      cli.prompt%i := pkt!pkt.arg6%i
//sawritef("newcli: new task starting*n")
    writef("New CLI task %N*N", taskid)
//sawritef("newcli: new task starting*n")
    tcb!tcb.seglist!3 := 0
    start := cli.undefglobval
    result2 := pkt
    RESULTIS qpkt
 $)
