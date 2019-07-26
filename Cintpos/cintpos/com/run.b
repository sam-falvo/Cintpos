// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

// Modified by MR 10/02/14

SECTION "run"

GET "libhdr"

LET start() BE
{ LET stdout = output()
  LET tasktab = rootnode!rtn_tasktab
  LET ctcb    = rootnode!rtn_crntask
  LET task = 0
  LET ch = rdch()
  LET ramstream = findinoutput("RAM:")

  UNLESS ramstream GOTO err

  selectoutput(ramstream)

  // Copy the rest of the command line to ramstream
  UNTIL ch=endstreamch DO
  { wrch(ch)
    IF ch='*n' BREAK
    ch := rdch()
  }

  selectoutput(stdout)

  rewindstream(ramstream)

  // Create the new task
  FOR pri = 500 TO 1 BY -1 DO
  { task := createtask(ctcb!tcb_seglist, ctcb!tcb_stsiz, pri)
    IF task BREAK
  }

  UNLESS task DO { freevec(ramstream); GOTO err }

  // cli_module is the run command module containing cli_init
  // Make it available to the newly created CLI task
  tasktab!task!tcb_seglist!3 := cli_module

  // Note that start in seglist!4 belonging to CLI overrides
  // the start function in seglist!3 belonging to the run command.

  // Activate the new task, giving it some data
  // and wait for the startup packet to be returned.
  sendpkt(-1, task, 0, 0, 0,
              0,//copyobj(currentdir),
              consoletask,                 // The COHAND task
              ramstream,
              0,//copyobj(cli_commanddir),
              cli_defaultstack,            // The cli stack size
              cli_prompt)                  // The current prompt string

  // The current CLI will now unload the run module,
  // and the newly created CLI task will execute the commands
  // in the RAM stream.

//sawritef("run: returning to the CLI*n")
  RETURN

err:
  writes("RUN failed*n")
  stop(20)
 }


LET cli_init(parm_pkt) = VALOF
{ LET seglist = rootnode!rtn_crntask!tcb_seglist
  initio()
  currentdir  := parm_pkt!pkt_arg1
  consoletask := parm_pkt!pkt_arg2
  selectinput(parm_pkt!pkt_arg3) // Select the ram stream

  // The following line is not quite right for eg tcpcli tasks
  // Input/output should be directed to the controlling stream
  // not the keyboard/screen of the machine running the current CLI.
  selectoutput(findoutput("**"))

  //cli_background := TRUE     // Commented out 31/07/07 MR
  cli_standardinput := input()
  cli_currentinput := cli_standardinput
  cli_standardoutput := output()
  cli_currentoutput  := cli_standardoutput
  cli_commanddir := parm_pkt!pkt_arg4
  returncode := 0
  cli_returncode := 0
  cli_faillevel  := cli_initialfaillevel
  cli_result2 := 0
  cli_commandfile%0 := 0
  cli_defaultstack := parm_pkt!pkt_arg5
  cli_module := 0   // To stop this CLI from unloading anything.
  FOR i = 0 TO parm_pkt!pkt_arg6 % 0 DO
      cli_prompt%i := parm_pkt!pkt_arg6 % i

  cli_status := clibit_runcli + clibit_noprompt + clibit_eofdel

//  seglist!3 := 0  // Remove the run module from this tasks seglist.
//                  // Note that it will be unloaded by the CLI task
//                  // that created this one.

//  qpkt(parm_pkt)  // Return the startup packet to the creating CLI

//  start := globword+1

//  RESULTIS 0
//}

  // The newcli module is in cli_module of the creating CLI task
  // and will be unloaded by that task. To stop it being unloaded
  // again when this task is deleted its seglist entry is cleared.
  seglist!3 := 0

  set_process_name("Run_Cli") // MR 3/2/03

  start := globword + 1

  // Cause the resident CLI code to return the startup packet to
  // the creating CLI task. Note that control will leave cli_init
  // BEFORE control reaches the point just after the call of
  // sendpkt in start defined in this module. When start returns,
  // the CLI will unload the newcli module (containing both start
  // and cli_init). 
  result2 := parm_pkt
  RESULTIS qpkt

  // Note that the following alternative coding is INCORRECT
  // since the Cintcode for RESULTIS 0 may be executed after the
  // newcli module has been unloaded and the space possibly reused
  // for other purposes.
  //      qpkt(parm_pkt)
  //      RESULTIS 0
}


