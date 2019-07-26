// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "run"

GET "libhdr"
//GET "g/clihdr.h"
//GET "g/iohdr.h"


LET start() BE
 $( LET command = VEC 1024/bytesperword
    LET l, cvec, svec = 0,?,?
    LET ch = rdch()
    LET task = 0
    LET failed = FALSE
sawritef("run: entered*n")

    // Copy the rest of the command line into command
    UNTIL ch = endstreamch DO
    $( IF l>=1023 DO
       $( failed := TRUE
          BREAK
       $)
       command%l := ch
       l := l+1
       IF ch='*N' | ch='*E' BREAK
       ch := rdch()
    $)
    IF failed GOTO err


    cvec := getvec((l-1)/bytesperword+1+Scb_upb)
    IF cvec=0 GOTO err

    svec := cvec+Scb_upb+1
    FOR j = 0 TO (l-1)/bytesperword DO
      svec!j := command!j

    FOR pri = 500 TO 1 BY -1 DO
    $( task := createtask(tcb!Tcb_seglist, tcb!Tcb_stsiz, pri)
       UNLESS task=0 BREAK
    $)

    IF task=0 DO
    $( freevec(cvec)
       GOTO err
    $)
//sawritef("run: task %n created*n", task)

    cvec!Scb_link := 0
    cvec!Scb_id   := Id_inscb
    cvec!Scb_type := 0
    cvec!Scb_buf  := svec
    cvec!Scb_pos  := 0
    cvec!Scb_end  := l
    cvec!Scb_func1 := 0
    cvec!Scb_func3 := 0

//sawritef("run: setting module globsize to %n*n", globsize)

    cli_module!(cli_module!1) := globsize
    rootnode!Rtn_tasktab!task!Tcb_seglist!3 := cli_module

//sawritef("run: sending startup packet*n")

    sendpkt(-1, task, 0, 0, 0,
                copyobj(currentdir),
                consoletask,
                cvec,
                copyobj(cli_commanddir),
                cli_defaultstack,
                cli_prompt)

//sawritef("run: startup packet returned*n")
    RETURN

err:writes("RUN failed*N")
    stop(20)
 $)


LET cli_init(parm_pkt) = VALOF
 $( initio()
    currentdir := parm_pkt!Pkt_arg1
    consoletask := parm_pkt!Pkt_arg2
    selectinput(parm_pkt!Pkt_arg3)
    selectoutput(findoutput("**"))

//sawritef("run: performing cli-init*n")

    cli_background := TRUE
    cli_standardinput := input()
    cli_currentinput := cli_standardinput
    cli_standardoutput := output()
    cli_currentoutput  := cli_standardoutput
    cli_commanddir := parm_pkt!Pkt_arg4
    returncode := 0
    cli_returncode := 0
    cli_faillevel  := Cli_initialfaillevel
    cli_result2 := 0
    cli_commandfile%0 := 0
    cli_defaultstack := parm_pkt!Pkt_arg5
    cli_module := 0
    FOR i = 0 TO parm_pkt!Pkt_arg6 % 0 DO
      cli_prompt%i := parm_pkt!Pkt_arg6 % i

    tcb!Tcb_seglist!3 := 0
    start := cli_undefglobval
    result2 := parm_pkt
    RESULTIS qpkt
 $)
