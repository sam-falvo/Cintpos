SECTION "ENDCLI"

// The endcli command closes down a CLI task, normally causing it to
// commit suicide.

// If called while in a command-command, it first leaves the command-command
// closing its temporary input stream and deleting the command-command's
// file (whose name is in cli_filename). This behaviour is just as if
// the CLI received a ctrl-d flag while running a command-command.

// At the outermost level (ie not in a command-command) endcli marks the
// input stream as exhaused. This causes the CLI to leave its main
// command execution loop and close

GET "libhdr"

LET start() BE
{ LET argv = VEC 20

  UNLESS rdargs("FORCE/S", argv, 20) DO
  { writef("Bad arguments for endcli*n")
    RETURN
  }

  // Some CLIs (eg TCPCLI) normally continue to exist after reading EOF
  // or being terminated by endcli. If the FORCE switch is given, the
  // clibit_eofdel is set and this forces the CLI to commit suicide in
  // all contexts.

  IF argv!0 DO
    cli_status := cli_status | clibit_eofdel

  cli_status := cli_status | clibit_endcli // MR 10/7/03
  // Cause the standard input  stream to be exhausted
  cli_standardinput!scb_end := -1

  writef("CLI task %n ending*n", taskid)
  RETURN
}



AND error(f, a) BE
{ writef(f, a)
  stop(20)
}

