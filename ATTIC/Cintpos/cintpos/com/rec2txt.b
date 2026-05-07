/*
This program replaces the timing characters in a recorded console
session made by the record command by text of the form [d..d] where
d..d is a decimal number representing the delay in ticks.
The resulting file can be converted back to rec format using
the command txt2rec.

Martin Richards (c) May 2009

21/05/09 MR
First implementation based on playtime.
*/

SECTION "REC2TXT"


GET "libhdr"

MANIFEST {
  time_char  = 255 // Escape introducing timing info
  time_tick1 = 254 // Special escape for 1 tick
}


LET start() BE
{ LET stdin  = input()
  LET stdout = output()
  LET instreamname = 0
  LET outstreamname = 0
  LET instream = 0
  LET outstream = 0

  LET argv = VEC 50

  UNLESS rdargs("FROM/a,TO/k", argv, 50) DO
  { writes("Invalid args to REC2TXT*n")
    GOTO fin
  }

  IF argv!0 DO instreamname  := argv!0
  IF argv!1 DO outstreamname := argv!1

  instream := stdin
  instream := findinput(instreamname)
  UNLESS instream DO
  { writef("Can't open %s*n", instreamname)
    GOTO fin
  }

  outstream := stdout
  IF outstreamname DO outstream := findoutput(outstreamname)
  UNLESS outstream DO
  { writef("Can't open %s*n", instreamname)
    GOTO fin
  }

  selectinput(instream)
  selectoutput(outstream)

  { // Main loop
    LET ch = binrdch()
    LET ticks = 0
    IF ch = endstreamch BREAK
    UNLESS ch=time_char | ch=time_tick1 DO
    { wrch(ch)
      LOOP
    }

    TEST ch = time_tick1
    THEN ticks := ticks + 1
    ELSE { // > 1 tick
           ch := binrdch()
           ticks := ticks + ch
         } REPEATWHILE ch = 255

    writef("[#%n]", ticks)
  } REPEAT

  newline()

fin:
  UNLESS instream =stdin  DO endstream(instream)
  UNLESS outstream=stdout DO endwrite()//endstream(outstream)
}
