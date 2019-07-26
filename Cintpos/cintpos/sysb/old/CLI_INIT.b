SECTION "CLI_INIT"

GET "libhdr"
GET "clihdr"

MANIFEST {
maxglob=300
}

LET cli.init(parm_pkt) = VALOF
{ LET pkt = VEC pkt.arg6
  LET prompt = "%n> "
  LET initialseg = tcb!tcb.seglist!3
  LET dummy = maxglob
  LET machine_name = "cobham"
/*
sawritef("Writing ABCD to device -3 (tty output)*n")
FOR i = 1 TO 5 DO sendpkt(-1, -3, 0, 0, 0, "ABCD*n"%i)

sawritef("Testing device -2 (tty input)*n")
sawritef("Type some characters terminating with a dot '.'*n")

FOR i = 1 TO 10 DO
{ LET ch = sendpkt(-1, -2, 0, 0, 0)
  sawritef("ch = %x2 '%c'*n", ch, ch)
  IF ch='.' BREAK
}
*/
  // send startup pkt to COHAND
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

  cli.background := FALSE
  cli.standardinput := input()
  cli.currentinput := 0 //findinput("SYS:S.INITIAL-COMMANDS")
  IF cli.currentinput=0 DO
        cli.currentinput := cli.standardinput
  cli.standardoutput := output()
  cli.currentoutput  := cli.standardoutput
  cli.commanddir := 0 //locatedir("SYS:C")
  returncode := 0
  cli.returncode := 0
  cli.faillevel  := cli.initialfaillevel
  cli.result2 := 0
  cli.commandfile%0 := 0
  cli.defaultstack := cli.initialstack
  cli.module := 0
  FOR i = 0 TO prompt%0 DO cli.prompt%i := prompt%i

  tcb!tcb.seglist!3 := 0 // Remove reference to initialseg (CLI_INIT)
  start := globword+1    // Unset global start

  result2 := initialseg  // Cause this segment to be unloaded by CLI
  RESULTIS unloadseg
}


