// (c) Copyright M. Richards, 21 July 1997

MODULE mcli

GET "mcpl.h"
GET "mcli.h"

MANIFEST
  Namemax    = 100,
  Promptmax  = 50,
  Filemax    = 100

FUN start : =>
  LET prompt      = CVEC Promptmax
  LET commandname = CVEC Namemax
  LET commandfile = CVEC Filemax
  LET globbase    = @globsize
  LET initprompt = "%d> "
   
  cli_prompt      := prompt
  cli_commandname := commandname
  cli_commandfile := commandfile

  FOR i = 0 TO Promptmax DO
  { LET ch = initprompt%i
    cli_prompt%i := ch
    IF ch=0 BREAK
  }
  cli_standardinput := input()
  cli_currentinput := cli_standardinput
  cli_standardoutput := output()
  cli_commandname%0 := 0
  cli_commandfile%0 := 0
  cli_defaultstack := Cli_initialstack
  cli_returncode := 0
  cli_result2 := 0
  cli_module := 0
  cli_preloadlist := 0
  cli_tallyflag := FALSE

  { LET ch=0, item

    writef(cli_prompt, 0)

    { // Start of main command loop
      LET msecs = 0
      cli_interactive :=  cli_currentinput=cli_standardinput

      item := rditem(cli_commandname, Namemax)

      MATCH item
      : 0 => // eof
             IF cli_currentinput=cli_standardinput DO sys(0, 0)
             BREAK

      : 1 | 2 => // unquoted name  or  quoted name
          LET p=cli_preloadlist, coptr=0

          UNTIL p=0 DO            // Search in preloadlist.
          { IF compstring(cli_commandname, @p!2)=0 DO
            { cli_module := p!1
              BREAK             // Module found.
            }
            p := !p
          }

          IF cli_module=0 DO
             cli_module := loadseg cli_commandname

          start := Globword+1 // Unset start

          UNLESS globin cli_module = 0 DO
                 coptr := createco(clihook, cli_defaultstack)

          TEST coptr=0
          THEN { cli_result2 := result2
                 writef("Can't load %s\n", cli_commandname)
               }
          ELSE { msecs := sys 30
                 IF cli_tallyflag DO
                 { cli_tallyflag := FALSE
                   sys 4              // Turn on tallying
                 }

                 // Transfer control to the command.
                 cli_returncode := callco(coptr, 0)

                 sys 5                 // Turn off tallying
                 cli_result2 := result2

                 msecs := sys 30 - msecs

                 // Unset user globals
                 FOR i = Ug TO globsize DO globbase!i := Globword+i

                 // Restore the library globals
                 globin(rootnode!Rtn_msyslib)
                 globin(rootnode!Rtn_mlib)
                 globin(rootnode!Rtn_mcli)

                 deleteco coptr
                 selectinput  cli_currentinput
                 selectoutput cli_standardoutput

                 UNLESS cli_returncode < Cli_faillevel DO
                        writef("%s failed returncode %d\n",
                                cli_commandname, cli_returncode)
               }

          IF p=0 AND cli_module~=0 DO unloadseg cli_module
          cli_module := 0

      : 3 | 4 =>  // \n or ;

      :  => writes "Error in command name\n"
      .

      ch := '\n'
      IF unrdch() DO ch := rdch()
      // Skip to end of line unless last command terminated by nl or ;
      UNTIL ch='\n' OR ch=';' OR ch=Endstreamch DO ch := rdch()
      IF ch='\n' AND cli_currentinput=cli_standardinput DO
                                            writef(cli_prompt, msecs)

      IF intflag() DO {  writes("****BREAK - CLI\N")
                         BREAK
                      }
    } REPEAT

    UNLESS cli_currentinput=cli_standardinput DO
    { endread()
      UNLESS cli_commandfile%0=0 DO { deletefile cli_commandfile
                                      cli_commandfile%0 := 0
                                    }
      cli_currentinput := cli_standardinput
      selectinput cli_currentinput
    }
  } REPEAT
.