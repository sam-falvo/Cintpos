// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "NEWCLI"

GET "g/libhdr.h"
GET "g/clihdr.h"
GET "g/iohdr.h"


LET start() BE
 $( LET argv = VEC 50
    LET pkt = VEC Pkt_arg6
    LET task = 0

    IF rdargs("DEV",argv,50)=0 GOTO err
    IF argv!0=0 DO argv!0 := "**"
//sawritef("newcli: creating a task*n")
    FOR i = 1000 TO 501 BY -1 DO
    $( task := createtask(tcb!Tcb_seglist,
                          tcb!Tcb_stsiz, i)
       UNLESS task=0 BREAK
    $)
    IF task=0 GOTO err
//sawritef("newcli: task %n created*n", task)

    cli_module!(cli_module!1) := globsize
    rootnode!Rtn_tasktab!task!Tcb_seglist!3 := cli_module

//sawritef("newcli: sending startup pkt*n")
    IF sendpkt(-1, task, 0, -1, 0,
               copyobj(currentdir),
//               copydir(currentdir),
               consoletask,
               argv!0,
               copyobj(cli_commanddir),
//               copydir(cli_commanddir),
               cli_defaultstack,
               cli_prompt)=0 GOTO err

//sawritef("newcli: returning to CLI*n")
    RETURN

err:writes("NEWCLI failed*N")
    stop(20)
 $)


LET cli_init(pkt) = VALOF
 $( initio()
//sawritef("newcli: new task starting*n")
    currentdir := pkt!Pkt_arg1
    consoletask := pkt!Pkt_arg2
    cli_currentinput := findinput(pkt!Pkt_arg3)
    selectinput(cli_currentinput)
//sawritef("newcli: new task starting*n")
    UNLESS cli_currentinput=0 DO
    $( cli_currentoutput := findoutput(pkt!Pkt_arg3)
       selectoutput(cli_currentoutput)
    $)
//sawritef("newcli: new task starting*n")
    IF cli_currentinput=0 | cli_currentoutput=0 DO
    $( endread()
       endwrite()
       returnpkt(pkt,0,result2)
       result2 := taskid
       RESULTIS deletetask
    $)
//sawritef("newcli: new task starting*n")
    UNLESS compstring(pkt!Pkt_arg3,"**")=0 DO
       consoletask := ABS cli_currentinput!Scb_type
    cli_background := FALSE
    cli_standardinput := cli_currentinput
    cli_standardoutput := cli_currentoutput
    cli_commanddir := pkt!Pkt_arg4
    returncode := 0
    cli_returncode := 0
    cli_faillevel  := Cli_initialfaillevel
    cli_result2 := 0
    cli_commandfile%0 := 0
    cli_defaultstack := pkt!Pkt_arg5
    cli_module := 0
//sawritef("newcli: new task starting*n")
    FOR i = 0 TO pkt!Pkt_arg6%0 DO
      cli_prompt%i := pkt!Pkt_arg6%i
//sawritef("newcli: new task starting*n")
    writef("New CLI task %N*N", taskid)
//sawritef("newcli: new task starting*n")
    tcb!Tcb_seglist!3 := 0
    start := cli_undefglobval
    result2 := pkt
    RESULTIS qpkt
 $)
