// (c) Copyright M. Richards, 27 June 2007

/*
This code is used by all CLIs including the task 1 CLI, Newclis,
mbxclis and tcpclis.

Change log

05/07/09
Added the uservar environment vector (upb=50) initialised to zeros
for user data not reset between CLI commands.

27/06/07
Added setseed(12345) since the random number seed is now a global
variable (not a static).

17/01/06
As suggested by Dave Lewis, ignore commands starting with a #. This
enables executable Unix command scripts using text like:
#!/usr/local/bin/cintsys -s
as the first line of the script file.
It is also useful for CLI comments.
21/5/2001
Changed manifest cli_initialstack to 50000 (previously 5000)
*/

SECTION "CLI"

GET "libhdr"
GET "manhdr" // Needed for: Action_getremipaddr

MANIFEST
{ namemax   = 25
  promptmax = 15
  filemax   = 10
}

LET start(parm_pkt) BE
{ // parm_pkt is only used in Cintpos
  LET prompt      = VEC promptmax
  LET commandname = VEC namemax
  LET commandfile = VEC filemax  // Command-command file name, if in use
  LET globbase    = @globsize    // globsize is global 0
  LET error       = FALSE
  LET dv          = VEC 10       // Used as private data by some CLIs
                                 // tcpcli uses it to hold the TCP
                                 // stream name.
  FOR i = 0 TO 10 DO dv!i := 0

  cli_data        := dv          // MR 10/7/03
  cli_status      := 0           // MR 10/7/03
  cli_prompt      := prompt
  cli_commandname := commandname
  cli_commandfile := commandfile
  cli_preloadlist := 0           // MR 28/6/07

  setseed(12345) // MR 27/6/07

  { LET f =   cli_init(parm_pkt)
    // f may be zero, or a function such as 
    //   deletetask or unloadseg with result2 set to a suitable argument
    IF f DO f(result2) // Must use result2 after cli_init returns.
  }


  { // Start of the CLI outer loop
    LET ch = 0
    LET cpumsecs = sys(Sys_cputime)

    // This point is reached on
    // (a) initial entry
    // (b) a TCP connection has been closed and should be re-opened
    // (c) a command-command file is exhausted or terminated by ctrl-d

//sawritef("CLI: start of outer CLI loop*n")

    IF (cli_status & clibit_tcpcli)~=0 DO
    { // Establish/re-establish the TCP connection
      // cli_data holds the TCP address
      LET remipaddr = 0

      set_process_name("TCP_Cli") // MR 31/8/05

//delay(tickspersecond*5)
//sawritef("CLI: calling findinput(%s)*n", cli_data)

      cli_standardinput  := findinput(cli_data)   // Typically TCP::7000
//sawritef("CLI: Waiting for the connection %s to be established*n", cli_data)

      remipaddr := sendpkt(-1, Task_tcphandler, Action_getremipaddr, 0, 0, 
                           cli_standardinput)
      UNLESS remipaddr DO
      { sawritef("CLI: Failed to connect to %s*n", cli_data)
        RETURN
      }

//sawritef("Connection %s established, remipaddr=%x8*n", cli_data, remipaddr)

//sawritef("CLI: calling findoutput(%s)*n", cli_data)
      cli_standardoutput := findoutput(cli_data)  // Typically TCP::7000

      UNLESS cli_standardinput & cli_standardoutput DO
      { IF cli_standardinput  DO endstream(cli_standardinput)
        IF cli_standardoutput DO endstream(cli_standardoutput)
//sawritef("CLI: findinput and/or findoutput on %s failed*n", cli_data)
        deletetask(taskid) // Cause this task to be deleted
      }

//sawritef("CLI: Connection %s established*n", cli_data)

      cli_currentinput  := cli_standardinput
      cli_currentoutput := cli_standardoutput
    }

execcoms:  // Execute commands
    selectinput(cli_standardinput)
    selectoutput(cli_currentoutput)
    ch := '*n'

//sawritef("*nCLI: execcoms: status = %b9 ch=%n*n", cli_status, ch)

    // Process current command stream until
    // either EOF is reached or endcli called.
    UNTIL ch=endstreamch | (cli_status & clibit_endcli)~=0 DO
    { LET item = ?
      LET oldcurrentinput = cli_currentinput

      //LET interactive = ~cli_background &
      //                   cli_currentinput = cli_standardinput

//sawritef("*nCLI: status = %b9 ch=%n*n", cli_status, ch)
      selectinput(cli_currentinput)

      // Set ch to the terminating character of the previous
      // command from this stream to help deciding whether to prompt.
//sawritef("*nCLI: set ch to last char of previous command*n")
      ch := unrdch() -> rdch(), '*n'
//sawritef("*nCLI: terminating char of previous command = %n*n", ch)

      TEST (cli_status & clibit_noprompt)=0 &
           cli_currentinput = cli_standardinput //&
           //~cli_background  //??????????? 
      THEN IF ch='*n' DO
           { // Output a prompt -- but only if
             //     1) prompting allowed,
             // and 2) commands are coming from standardinput, and
             // and 3) the previous command was terminated by a newline.
             LET hours, mins, secs, msecs = 0, 0, 0, 0
             LET datv = VEC 2
             datstamp(datv)
             // Assume new dat format
             msecs := datv!1
             secs  := msecs / 1000
             mins  := secs  / 60
             hours := mins  / 60
             msecs, secs, mins := msecs MOD 1000, secs MOD 60, mins MOD 60
             selectoutput(cli_currentoutput)
//sawritef("*nCLI: cos=%n currentoutput=%n standardoutput=%n*n",
//                 cos, cli_currentoutput, cli_standardoutput)

             // Calculate the CPU time used in msecs since issuing last prompt
             cpumsecs := sys(Sys_cputime) - cpumsecs

             writef(cli_prompt,
                    cpumsecs,   // CPU time used by the last command
                    taskid,
                    hours, mins, secs, msecs)
             deplete(cos)

             cpumsecs := sys(Sys_cputime)
           }
      ELSE { IF testflags(flag_d) DO  // ctrl-D flag set by COHAND
             { error := TRUE
               writes("****BREAK - CLI*n")
             }
//sawritef("*nCLI: not writing the prompt*n")
             IF error BREAK
           }

      error := FALSE
      cli_commandname%0 := 0

      //FOR i = 1 TO 5 DO
      //{ ch := rdch()
      //  sawritef("CLI: ch=%n*n",ch)
      //}

//sawritef("CLI: calling rditem*n")
      item := rditem(cli_commandname, namemax)
//sawritef("CLI: item=%n text=%s*n", item, cli_commandname)
      IF item=3 LOOP // Newline found  MR 28/11/02

//      ch := 0
      SWITCHON item INTO
      { DEFAULT: writef("Bad command syntax, item=%n*n", item)
                 ENDCASE

        CASE 0:  // EOF
                 IF cli_currentinput=cli_standardinput &
                    (cli_status & clibit_maincli)~=0 DO sys(Sys_quit, 0)
                 error := FALSE
                 BREAK

        CASE 1:  // Unquoted name
        CASE 2:  // Quoted name
               { LET p = cli_preloadlist
                 LET coptr = 0
//sawritef("*nCLI: name=%s*n", cli_commandname)
                 // If the command name is # or starts with a #, treat
                 // the command as a comment, ie skip to EOL or EOF.
                 IF cli_commandname%0 > 0 & cli_commandname%1 = '#' DO
                 { LET ch = ?
                   ch := rdch() REPEATUNTIL ch='*n' | ch=endstreamch
                   LOOP
                 }
p := 0 // No preload list ????????????????????
                 WHILE p DO            // Search in preloadlist.
                 { IF compstring(cli_commandname, @p!2)=0 DO
                   { cli_module := p!1
                     BREAK             // Module found.
                   }
                   p := !p
                 }
                 UNLESS cli_module DO
                   cli_module := loadseg(cli_commandname)

                 //UNLESS cli_module DO  // Removed MR 7/2/11
                 //{ LET dir = currentdir
                 //  currentdir := cli_commanddir
                 //  cli_module := loadseg(cli_commandname)
                 //  currentdir := dir
                 //}

                 // Initialise its globals and create the coroutine
                 IF cli_module & globin(cli_module) DO
                   coptr := createco(clihook, cli_defaultstack)
//sawritef("CLI: createco...) => %n*n", coptr); abort(1111)

                 TEST coptr=0
                 THEN { cli_result2 := result2
                        writef("Unknown command: %s*n", cli_commandname)
                      }
                 ELSE { // The command coroutine was successfully created
                        LET ctcb = rtn_crntask!rootnode
                        LET seglist = ctcb!tcb_seglist
                        testflags(flag_b)   // Clear ctrl-B flag
                        error := FALSE // MR 16/5/02 ?????????????????

                        cpumsecs := sys(Sys_cputime)

                        returncode := 0 // MR 27/7/04

                        // Enter the coroutine causing clihook to
                        // call start(0).
                        cli_returncode := callco(coptr, 0)
                        cli_result2    := result2

                        //IF cli_returncode >= cli_faillevel DO error := TRUE
//sawritef("CLI: returncode=%n reason=%n*n", cli_returncode, cli_result2)
//sawritef("CLI: calling deleteco(%n)*n", coptr)//; abort(1111)
                        deleteco(coptr)

                        // Re-initialise the system globals
                        FOR i = ug TO globbase!0 DO globbase!i := globword+i
                        globin(seglist!1) // Initialise KLIB segments
                        globin(seglist!2) // Initialise BLIB segments
                        globbase!1 := globword+1  // Unset start

                        selectinput (cli_currentinput)
                        selectoutput(cli_currentoutput)

                        IF (cli_status & clibit_comcom)=0 DO
                        { // Set ch to the most recent character read
                          // or '*n' if we cannot unrdch.
                          ch := unrdch() -> rdch(), '*n'
//sawritef("CLI: last ch of previous command = %n*n", ch)
                        }

                        IF cli_returncode >= cli_faillevel DO
                        { writef("%s failed returncode %n",
                                 cli_commandname, cli_returncode)
                          IF cli_result2 DO
                            writef(" reason %n", cli_result2)
                          newline()
                        }
                      }

//sawritef("*nCLI: unloading %s*n", cli_commandname) 
                 IF p=0 & cli_module DO unloadseg(cli_module)
                 cli_module := 0
                 ENDCASE
               }

        CASE 3:          // Newline
        CASE 4:  ENDCASE // Semicolon
      }

      // This point is reached when a command has been executed.
      // Unless currentinput has been changed (eg by the c command)
      // input must be skipped until ch is '*n', ';' or EOF.

//sawritef("CLI: testing if currentinput has changed*n")
//sawritef("*nCLI: cis=%n currentinput=%n oldcurrentinput=%n*n",
//                 cis, cli_currentinput, oldcurrentinput)
      UNLESS cli_currentinput=oldcurrentinput LOOP

//sawritef("CLI: find last ch of previous command*n")
      // Find the last ch of the previous command on currentinput
      ch := unrdch() -> rdch(), '*n'
//sawritef("CLI: obviously not stuck in rdch, ch=%n*n", ch)
      // Skip to the end of command line
      UNTIL ch='*n' | ch=';' | ch=endstreamch DO
      {
//sawritef("CLI: ignore chars to the end of command line, ch=%n*n", ch)
//abort(3333)
        ch := rdch()
      }
//sawritef("CLI: ch = %n*n", ch)

    } // End of inner command loop

//sawritef("CLI: just after inner command loop, EOF, endcli or BREAK*n")
    // This point reached by either
    //    (a) encountering EOF,
    // or (b) the endcli command called,
    // or (c) TCB flag_d set.

    IF (cli_status&clibit_comcom)~=0 DO 
    { // We were in a command-command, so delete the (temporary)
      // command file, revert to the previous command input
      // and unset the CLI comcom bit.
      cli_status := cli_status & ~clibit_comcom
      endread()
//sawritef("CLI: deleteting command file %s*n", cli_commandfile)
      deletefile(cli_commandfile)
      cli_commandfile%0 := 0
      cli_currentinput := cli_standardinput
      cli_faillevel := cli_initialfaillevel
      GOTO execcoms
    }

//sawritef("*nCLI hit EOF or endcli was called*n")

    // Unset endcli bit, if necessary
    cli_status := cli_status & ~clibit_endcli

    // Must be at EOF (or endcli encountered) at outermost level
    // Close all the streams
//sawritef("CLI: closing streams*n")

    endstream(cli_currentinput)
    endstream(cli_currentoutput)
    UNLESS cli_currentinput=cli_standardinput DO
       endstream(cli_standardinput)
    UNLESS cli_currentoutput=cli_standardoutput DO
       endstream(cli_standardoutput)
  } REPEATWHILE (cli_status&clibit_eofdel)=0

  // The CLI must now commit suicide
//sawritef("CLI: task %n committing suicide*n", taskid)

  // Free all the preloadlist modules
  WHILE cli_preloadlist DO
  { LET p = cli_preloadlist
    unloadseg(p!1)        // Unload the module 
    cli_preloadlist := !p
    freevec(p)            // Free the preload list node
  } 
//  freeobj(currentdir)
//  freeobj(cli_commanddir)
  deletetask(taskid)
}

