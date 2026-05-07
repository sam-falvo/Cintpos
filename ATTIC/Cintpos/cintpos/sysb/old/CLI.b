SECTION "CLI"

GET "libhdr"
GET "clihdr"

MANIFEST
{ namemax   = 25
  promptmax = 15
  filemax   = 10
}

LET start(parm.pkt) BE
{ LET prompt      = VEC promptmax
  LET commandname = VEC namemax
  LET commandfile = VEC filemax  // Command-command file name, if in use
  LET globbase    = @globsize    // globsize is global 0
  LET error       = FALSE
  LET datavec     = VEC 10       // Used by some CLIs
  FOR i = 0 TO 10 DO datavec!i := 0

  cli.data        := datavec       // MR 10/7/03
  cli.status      := 0             // MR 10/7/03
  cli.prompt      := prompt
  cli.commandname := commandname
  cli.commandfile := commandfile

  { LET f =   cli.init(parm.pkt)
    // f may be zero, or a function such as 
    //   deletetask or unloadseg with result2 set to a suitable argument
    IF f DO f(result2) // Must get result2 after calling cli.init
  }

  { LET ch = 0

    // process current command stream
    UNTIL ch=endstreamch DO
    { LET item = ?

      cli.interactive := NOT cli.background &
                         cli.currentinput = cli.standardinput
      selectinput(cli.currentinput)

      TEST cli.interactive
      THEN UNLESS unrdch() & rdch() = ';' DO
           { // Output a prompt only if interactive and
             // the previous command was not terminated by ';'.
             LET datv = VEC 2
             LET mins, secs = 0, 0
             datstamp(datv)
             mins := datv!1
             secs := datv!2/tickspersecond
             selectoutput(cli.standardoutput)
             writef(cli.prompt, taskid, mins/60, mins REM 60, secs)
             wrch('*e')
             selectoutput(cli.currentoutput)
           }
      ELSE { IF testflags(2) DO
             { error := TRUE
               writes("****BREAK - CLI*n")
             }
             IF error BREAK
           }

      error := FALSE
      cli.commandname%0 := 0
      item := rditem(cli.commandname, namemax)
//sawritef("CLI: item=%n text=%s*n", item, cli.commandname)

      ch := 0
      IF item DO
      { error := TRUE
        TEST item = 1  // Unquoted item -- treat as command name
        THEN { LET coptr = 0
               cli.module := loadseg(cli.commandname)

               UNLESS cli.module DO
               { LET dir = currentdir
                 currentdir := cli.commanddir
                 cli.module := saloadseg(cli.commandname)
                 currentdir := dir
               }

               IF cli.module & globin(cli.module) DO
                 coptr := createco(clihook, cli.defaultstack)

               TEST coptr=0
               THEN { cli.result2 := result2
                      writef("Can't load %s*n", cli.commandname)
                    }
               ELSE { LET seglist = tcb!tcb.seglist
                      testflags(1)
                      error := FALSE // MR 16/5/02 ?????????????????
                      returncode := callco(coptr, 0)

                      // Re-initialise the system globals
                      FOR i = ug TO globbase!0 DO globbase!i := globword+i
                      globin(seglist!1) // Initialise KLIB segments
                      globin(seglist!2) // Initialise BLIB segments
                      globbase!1 := globword+1  // Unset start

                      cli.result2    := returncode -> result2, 0
                      cli.returncode := returncode
                      returncode     := 0
                      IF cli.returncode < cli.faillevel DO error := FALSE

                      deleteco(coptr)
                      selectinput (cli.currentinput)
                      selectoutput(cli.currentoutput)

                      ch := unrdch() -> 0, '*n'

                      IF error & NOT cli.interactive DO
                        writef("%s failed returncode %n*n",
                               cli.commandname, cli.returncode)
                    }

               unloadseg(cli.module)
               cli.module := 0
             }

        ELSE writes("Error in command name*n")
      }

      // Skip to end of command line
//sawritef("CLI: skipping to end of command line*n")
      UNTIL ch='*n' | ch='*e' | ch=';' | ch=endstreamch DO ch := rdch()
//sawritef("CLI: ch = %n*n", ch)
    }

//IF ch=endstreamch DO
//  sawritef("*nCLI hit EOF*n")

    TEST cli.currentinput=cli.standardinput
    THEN { IF cli.background BREAK
           cli.standardinput!scb.end := -1
           //cli.standardinput!scb.arg1 := 1   //??????????
         }
    ELSE { endread()
//sawritef("CLI: deleteting command file %s*n", cli.commandfile)
           deleteobj(cli.commandfile)
           cli.currentinput := cli.standardinput
           cli.faillevel := cli.initialfaillevel
         }

  } REPEAT

  endread()
  endwrite()
  UNLESS cli.currentoutput=cli.standardoutput DO
  { selectoutput(cli.standardoutput)
    endwrite()
  }
//  freeobj(currentdir)
//  freeobj(cli.commanddir)
  deletetask(taskid)
}

