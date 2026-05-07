// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "NEWCLI"

GET "libhdr"

LET start(parm_pkt) BE
{ LET argv = VEC 50
  LET tasktab = rootnode!rtn_tasktab
  LET ctcb    = rootnode!rtn_crntask
  LET task = 0

  IF parm_pkt DO
  { // Starting as a CLI task
    sawritef("CLI task starting*n")
    abort(9999)
  }

  UNLESS rdargs("DEV",argv,50) GOTO err

  UNLESS argv!0 DO argv!0 := "**"

  // Try to create a new task with the same segments as the current task
  // ie  seglist!1 = KLIB etc
  //     seglist!2 = BLIB etc
  //     seglist!3 = 0
  //     seglist!4 = CLI module
  FOR pri = 1000 TO 501 BY -1 DO
  { task := createtask(ctcb!tcb_seglist, ctcb!tcb_stsiz, pri)
    IF task BREAK
  }
  UNLESS task GOTO err

  // cli_module is the newcli command module containing cli_init
  // Make it available to the newly created CLI task
  tasktab!task!tcb_seglist!3 := cli_module

  // Note that start in seglist!4 belonging to CLI will override
  // the start function in seglist!3 belonging to the newcli command.

  // Activate the new CLI task
  UNLESS sendpkt(-1, task, 0, -1, 0,
                  0, //copyobj(currentdir),
//                   copydir(currentdir),
                  consoletask,
                  argv!0,
                  0, //copyobj(cli_commanddir),
//                  copydir(cli_commanddir),
                  cli_defaultstack,
                  cli_prompt) GOTO err

  RETURN

err:
  writes("NEWCLI failed*n")
  stop(20)
 }

AND cli_init(pkt) = VALOF
{ LET ctcb = rootnode!rtn_crntask
  initio()
  currentdir  := pkt!pkt_arg1
  consoletask := pkt!pkt_arg2

  cli_currentinput := findinput(pkt!pkt_arg3)     // Typically *
  selectinput(cli_currentinput)

  UNLESS cli_currentinput=0 DO
  { cli_currentoutput := findoutput(pkt!pkt_arg3) // Typically *
    selectoutput(cli_currentoutput)
  }

  UNLESS cli_currentinput & cli_currentoutput DO
  { endread()
    endwrite()
    returnpkt(pkt,0,result2)
    result2 := taskid          // Cause this task to delete itself
    RESULTIS deletetask
  }

  UNLESS compstring(pkt!pkt_arg3,"**")=0 DO
     consoletask := cli_currentinput!scb_task
  //cli_background := FALSE
  cli_standardinput := cli_currentinput
  cli_standardoutput := cli_currentoutput
  cli_commanddir := pkt!pkt_arg4
  returncode := 0
  cli_returncode := 0
  cli_faillevel  := cli_initialfaillevel
  cli_result2 := 0
  cli_commandfile%0 := 0
  cli_defaultstack := pkt!pkt_arg5
  cli_module := 0

  FOR i = 0 TO pkt!pkt_arg6%0 DO
    cli_prompt%i := pkt!pkt_arg6%i

  writef("New CLI task %n*n", taskid)

  cli_status := clibit_newcli + clibit_eofdel

  // The newcli module is in cli_module of the creating CLI task
  // and will be unloaded by that task. To stop it being unloaded
  // again when this task is deleted its seglist entry is cleared.
  ctcb!tcb_seglist!3 := 0

  set_process_name("New_Cli")  // MR 3/2/03

  start := globword + 1

  // Cause the resident CLI code to return the startup packet to
  // the creating CLI task. Note that control will leave cli_init
  // BEFORE control reaches the point just after the call of
  // sendpkt in start defined in this module. When start returns,
  // the CLI will unload the newcli module (containing both start
  // and cli_init). 
  result2 := pkt
  RESULTIS qpkt

  // Note that the following alternative coding is INCORRECT
  // since the Cintcode for RESULTIS 0 may be executed after the
  // newcli module has been unloaded and the space possibly reused
  // for other purposes.
  //      qpkt(pkt)
  //      RESULTIS 0
}
