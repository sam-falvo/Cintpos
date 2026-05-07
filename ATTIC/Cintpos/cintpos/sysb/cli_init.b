SECTION "CLI_INIT"

GET "libhdr"

MANIFEST {
  maxglob=300
}

LET cli_init(parm_pkt) = VALOF
{ // This is the cli_init function for the main CLI (task 1)
  // parm_pkt is the startup pkt sent by the Idle task and
  // will not be returned. The cli_init function for other
  // CLI tasks (such as newcli, tcpcli and mbxcli) do cause
  // their startup packets to be returned.

  // This CLI starts up the other resident Cintpos tasks.

  LET pkt = VEC pkt_arg6
  LET prompt = "%5.3d %n> "
  LET dummy = maxglob
  LET machine_name = "solestreet"
  LET ctcb = rtn_crntask!rootnode
  LET initialseg = ctcb!tcb_seglist!3  // This segment!

  set_process_name("Root_Cli")

//sawritef("*nCLI_INIT: Writing A to Z to device -3 (tty output)*n")
//FOR i = 0 TO 25 DO sendpkt(-1, -3, 0, 0, 0, 'A'+i)
//sawritef("*n")

/*
sawritef("CLI_INIT: Testing device -2 (tty input)*n")
sawritef("CLI_INIT: Type some characters terminating with a dot '.'*n")

FOR i = 1 TO 10 DO
{ LET ch = sendpkt(-1, -2, 0, 0, 0)
  sawritef("CLI_INIT: ch = %i3 '%c'*n", ch, ch)
  IF ch=endstreamch DO sys(Sys_quit, 0) 
  IF ch='.' BREAK
}
*/
  // send startup pkt to COHAND, ttyin device=-2, ttyout device=-3
  sendpkt(notinuse, Task_consolehandler, 0, ?, ?, -2, -3)

  // send startup pkt to FH0
  sendpkt(notinuse, Task_filehandler, 0, ?, ?)

  // send startup pkt to MBXHAND
  sendpkt(notinuse, Task_mbxhandler, 0, ?, ?)

  // send startup pkt to TCPHAND
  sendpkt(notinuse, Task_tcphandler, 0, ?, ?)

  initio()
  selectoutput(findoutput("**"))
  selectinput(findinput("**"))

  // send startup pkt to DEBUG
  sendpkt(notinuse,Task_debug, 0, ?, ?)

  //cli_background := FALSE
  cli_standardinput := input()
  cli_currentinput := 0 //findinput("SYS:S.INITIAL-COMMANDS")
  IF cli_currentinput=0 DO
        cli_currentinput := cli_standardinput
  cli_standardoutput := output()
  cli_currentoutput  := cli_standardoutput
  cli_commanddir := 0 //locatedir("SYS:C")
  returncode := 0
  cli_returncode := 0
  cli_faillevel  := cli_initialfaillevel
  cli_result2 := 0
  cli_commandfile%0 := 0
  cli_defaultstack := cli_initialstack
  cli_module := 0


  // Mark as a main CLI task
  // ie exit from Cintpos if EOF received
  cli_status := clibit_maincli  // MR 31/8/05
//sawritef("CLI_INIT: just set cli_status = %b9*n", cli_status)

  FOR i = 0 TO prompt%0 DO cli_prompt%i := prompt%i

  start := globword+1     // Unset global start

  ctcb!tcb_seglist!3 := 0 // Remove reference to initialseg (CLI_INIT)
  result2 := initialseg   // Cause this segment to be unloaded by CLI

  RESULTIS unloadseg
}


