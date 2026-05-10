// (c) Copyright M. Richards, 23 April 2010

/* Change log

13/10/2025
Sind loadseg now looks in directory $BCPLROOT/cin if a command is not
found in the current directory or the directories specified by
$BCPLPATH. So provided the commands are in the standard place it is
not necessary to define the environment variable BCPLPATH.

23/04/2010
Changed to new datstamp format, but still use 3 word date stamps
since 16-bit BCPL requires 3 words.

27/06/2007
Added setseed(12345) since the random number seed is now a global
variable (not a static).

17/01/2006
As suggested by Dave Lewis, ignore commands starting with a #. This
enables executable Unix command scripts using text like:
#!/usr/local/bin/cintsys -s
as the first line of the script file.
21/05/2001
Changed manifest cli_initialstack to 50000 in CLIHDR (previously 5000)
*/

SECTION "CLI"

GET "libhdr.h"

MANIFEST
{ namemax   = 25
  promptmax = 15
  filemax   = 10
}

LET start(parm_pkt) BE // parm_pkt only used in Cintpos
{ LET prompt      = VEC promptmax
  LET commandname = VEC namemax
  LET commandfile = VEC filemax  // Command-command file name, if in use
  LET globbase    = @globsize    // globsize is global 0
  LET cpumsecs    = sys(Sys_cputime)
  LET initprompt  = "%5.3d> "
  //LET initprompt = "%5.3d %+%2i:%2z:%2z.%3z> " // Useful for debugging
  LET datavec     = VEC 10       // Used as private data by some CLIs
                                 // tcpcli uses it to hold the TCP
                                 // stream name.
  FOR i = 0 TO 10 DO datavec!i := 0
  cli_data        := datavec       // MR 10/7/03
  cli_status      := 0             // MR 10/7/03
  cli_prompt      := prompt
  cli_commandname := commandname
  cli_commandfile := commandfile

  FOR i = 0 TO initprompt%0 DO
    cli_prompt%i := initprompt%i

  cli_standardinput := input()
  cli_currentinput := cli_standardinput
  cli_standardoutput := output()
  cli_commandname%0 := 0
  cli_commandfile%0 := 0
  cli_defaultstack := cli_initialstack
  cli_returncode := 0
  cli_result2 := 0
  cli_module := 0
  cli_preloadlist := 0
  cli_tallyflag := FALSE
  cli_faillevel := cli_initialfaillevel

  setseed(12345) // MR 27/6/07

// Test that stdin and stdout work
  
  selectinput(cli_standardinput)
  selectoutput(cli_standardoutput)
/*
  writes("*nType a line of input*n")

  { LET ch = rdch()
    IF ch='*n' | ch=endstreamch BREAK
    writef("ch=%i3", ch)
    IF ch>32 DO writef("  '%c'*n", ch)
    newline()
  } REPEAT
*/  
  IF rootnode!rtn_boottrace DO
    sawritef("cli: now entering the main CLI loop*n")

  { LET ch, item = '*n', ?

    { // Start of main command loop
      //cli_interactive :=  cli_currentinput=cli_standardinput

      // Possibly output the prompt
      UNLESS rootnode!rtn_quietflag DO
        IF ch='*n' & cli_currentinput=cli_standardinput DO
        { LET hours, mins, secs = 0, 0, 0
          LET days, msecs, flag = 0, 0, 0 
          datstamp(@days)
          secs  := msecs  /  1000
          msecs := msecs MOD 1000
          mins  := secs   /  60
          hours := mins   /  60
          mins  := mins  MOD 60
          secs  := secs  MOD 60

          // Calculate msecs since issuing last prompt
          cpumsecs := sys(Sys_cputime) - cpumsecs
//sawritef("cli: about to write the prompt, cli_tallyflag=%n*n", cli_tallyflag)
//abort(1002)
//wrch('A')
//wrch('*n')
//abort(1003)
//writef("A*n")
//abort(1006)
          writef(cli_prompt,
                 cpumsecs, // msecs used by last command
                 0,        // The task number, if running under Cintpos
                 hours, mins, secs, msecs) // The time of day
//abort(1007)
          deplete(cos)
//abort(1008)

          cpumsecs := sys(Sys_cputime)
//abort(1009)
        }
      item := rditem(cli_commandname, namemax)
      //sawritef("cli: item=%n*n", item)
//abort(1010)
//sys(Sys_tracing, TRUE)

      SWITCHON item INTO
      { CASE 0: // The item was: eof
      //abort(1022)
          IF cli_currentinput=cli_standardinput DO sys(Sys_quit, 0, 999)
          BREAK

        CASE 1: // The item was: unquoted name
        CASE 2: // The item was: quoted name
        { LET p, coptr = cli_preloadlist, 0
          // If the command name is # or starts with a #,
          // treat the command as a comment,
          // ie skip to just before EOL or EOF.
          IF cli_commandname%0 > 0 & cli_commandname%1 = '#' DO
          { LET ch = ?
            ch := rdch() REPEATUNTIL ch='*n' | ch=';' | ch=endstreamch
            IF ch='*n' DO unrdch()
            LOOP
          }
 
          WHILE p DO            // Search in preloadlist.
          { IF compstring(cli_commandname, @p!2)=0 DO
            { cli_module := p!1
              BREAK             // Module found.
            }
            p := !p
          }
//sawritef("cli: About to load command %s*n", cli_commandname)
//abort(1011)
          UNLESS cli_module DO cli_module := loadseg(cli_commandname)

          start := globword+1 // Unset start
          UNLESS globin(cli_module)=0 DO
            coptr := createco(clihook, cli_defaultstack)
	  //sawritef("cli: coptr=%n cli_tallylag=%n*n", coptr, cli_tallyflag)
          TEST coptr=0
          THEN { cli_result2 := result2
                 writef("Can't load %s*n", cli_commandname)
               }
          ELSE { 
	         TEST cli_tallyflag
                 THEN { // Turn on tallying for one command.
			cli_tallyflag := FALSE
			
		        //sawritef("cli: Turn on tallying for one CLI cammand*n")
			//sys(Sys_tracing, TRUE)
			//delay(10000);
		        sys(Sys_tally, TRUE)
			// Enter the newly loaded CLI command
                        cli_returncode := callco(coptr, 0)
                        cli_result2 := result2
		        sys(Sys_tally, FALSE)
			//sys(Sys_tracing, FALSE)
			
                        sys(Sys_setraster, 5, 0) // Close the raster file
			                         // if it is open.
                      }
		 ELSE {
		        //sawritef("cli: Execute one non tallying command*n")
                        cli_returncode := callco(coptr, 0)
                        cli_result2 := result2
		      }
		      
                 // Unset user globals
                 FOR i = ug TO globsize DO
                   globbase!i := globword + i

                 // Restore the library globals
                 globin(rootnode!rtn_blib)
                 //globin(rootnode!rtn_cli)

                 deleteco(coptr)
                 selectinput (cli_currentinput)
                 selectoutput(cli_standardoutput)

                 IF cli_returncode >= cli_faillevel DO
                 { writef("%s failed returncode %n",
                           cli_commandname, cli_returncode)
                   IF cli_result2 DO
                     writef(" reason %n", cli_result2)
                   newline()
                 }
               }

          IF p=0 & cli_module DO unloadseg(cli_module)
          cli_module := 0
        }

        CASE 3: // The item was: '*n'
        CASE 4: // The item was: ';'
          ENDCASE

        DEFAULT:// Unknown item. 
          writes("Error in command name*n")
      }

      ch := '*n'
      IF unrdch() DO ch := rdch()
      // Skip to end of line unless last command terminated by nl or ;
      UNTIL ch='*n' | ch=';' | ch=endstreamch DO ch := rdch()

      IF intflag() DO { writes("****BREAK - CLI*N")
                        BREAK
                      }
    } REPEAT

    IF (cli_status & clibit_comcom) ~= 0 DO
    { // We were within a command-command, so close the stream
      endstream(cli_currentinput)
      cli_currentinput := cli_standardinput
      selectinput(cli_currentinput)

//sawritef("cli: deleting command file %s*n", cli_commandfile)

      // and delete the command file
      IF cli_commandfile%0 DO { sys(Sys_deletefile, cli_commandfile)
                                cli_commandfile%0 := 0
                              }
      cli_status := cli_status & ~clibit_comcom
    }
  } REPEAT
}
