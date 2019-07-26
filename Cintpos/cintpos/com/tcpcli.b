SECTION "TCPCLI"

GET "libhdr"

LET start(parm_pkt) BE
{ LET argv = VEC 50
  LET tasktab = rootnode!rtn_tasktab
  LET ctcb    = rootnode!rtn_crntask
  LET task = 0
  LET port = "8000"  // Default TCP port
  LET noprompt = FALSE

  IF parm_pkt DO
  { // Starting as a CLI task
    sawritef("TCPCLI task starting*n")
    abort(9999)
  }

  UNLESS rdargs("PORT,NOPROMPT/S",argv,50) DO
  { writef("Bad arguments for TCPCLI*n")
    GOTO err
  }

  IF argv!0 DO port := argv!0
  noprompt := argv!1
  IF noprompt DO port := -port

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

  // cli_module is the tcpcli command module containing cli_init
  // Make it available to the newly created CLI task
  // Note that start in seglist!4 belonging to CLI will override
  // the start function in seglist!3 belonging to the tcpcli command.
  tasktab!task!tcb_seglist!3 := cli_module
  // Don't let the CLI unload the tcpcli module -- this will be done
  // by the newly created CLI after a TCP connection has been made.
  cli_module := 0

again:
  writef("New TCPCLI task %n listening on port %s %s*n",
         task, ABS port, port<0 -> "", "no prompt")

  // Activate the new CLI task
  UNLESS sendpkt(-1, task, 0, -1, 0,
                  0, //copyobj(currentdir),
//                   copydir(currentdir),
                  consoletask,
                  port,
                  0, //copyobj(cli_commanddir),
//                  copydir(cli_commanddir),
                  cli_defaultstack,
                  cli_prompt) GOTO err

  RETURN  // Return to the CLI

err:
  writes("TCPCLI failed*n")
  stop(20)
 }

AND cli_init(pkt) = VALOF
{ LET curdir    = pkt!pkt_arg1
  LET cnsltask  = pkt!pkt_arg2
  LET port      = ABS pkt!pkt_arg3
  LET noprompt  = pkt!pkt_arg3<0
  LET comdir    = pkt!pkt_arg4
  LET dfltstack = pkt!pkt_arg5
  LET prompt    = pkt!pkt_arg6
  LET ctcb      = rootnode!rtn_crntask
  LET tcpcli    = ctcb!tcb_seglist!3 // The tcpcli module
  LET prfx      = "TCP::"
  LET name      = cli_data           // MR 10/7/03
//sawritef("tcpcli: cli_init entered*n")
  name%0 := 0
  FOR i = 1 TO prfx%0 DO { LET n = name%0 + 1; name%0, name%n := n, prfx%i }
  FOR i = 1 TO port%0 DO { LET n = name%0 + 1; name%0, name%n := n, port%i }
  // Typically name = "TCP::7000"
//sawritef("tcpcli: name=%s*n", name) 
  initio()

  currentdir  := curdir
  consoletask := cnsltask
//  cli_background := FALSE
//  cli_preloadlist := 0
  cli_commanddir := comdir
  returncode := 0
  cli_returncode := 0
  cli_faillevel  := cli_initialfaillevel
  cli_result2 := 0
  cli_commandfile%0 := 0          // Not in a command-command
  cli_defaultstack := dfltstack
  cli_module := 0

  // Mark as a TCP task
  // ie name of listening connection is in cli_data
  // and re-open connection on EOF (or endcli)
  cli_status := clibit_tcpcli  // MR 10/7/03
  FOR i = 0 TO name%0 DO cli_data%i := name%i
  FOR i = 0 TO prompt%0 DO cli_prompt%i := prompt%i
  IF noprompt DO cli_status := cli_status | clibit_noprompt
 
  // Stop the status command seeing this module
  ctcb!tcb_seglist!3 := 0 // The tcpcli module

  set_process_name("TCP_Cli") // MR 24/2/03

  qpkt(pkt) // Return the packet to the CLI that called tcpcli

  start := globword + 1

//sawritef("tcpcli: cli_init returning*n")

  // Return to the resident CLI to begin executing CLI commands,
  // getting it to unload this module (tcpcli) first.
  result2 := tcpcli
  RESULTIS unloadseg
}
