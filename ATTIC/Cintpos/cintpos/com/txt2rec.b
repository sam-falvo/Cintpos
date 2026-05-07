/*
This program converts a text file produced by rec2txt back
to to form produced by the record program. i.e. it replaces
text of the form [d..d] with the corresponding characters
reprenting the delay.


Martin Richards (c) May 2009

21/05/09 MR
First implementation based on playtime.
*/

SECTION "TXT2REC"


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
  { writes("Invalid args to TXT2REC*n")
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
    LET ch = rdch()
next:
    IF ch = endstreamch BREAK
    // check for [ #d..d ]
    IF ch = '[' DO
    { LET ticks = 0
      LET str = VEC 10
      // Check that [ is followed by #
      ch := rdch()
      UNLESS ch='#' DO
      { // Not a tick item
        wrch('[')
//abort(1000)
        GOTO next
      }
      // [ was followed by # so read decimal digits
      FOR i = 1 TO 10 DO
      { ch := rdch()
        UNLESS '0'<=ch<='9' BREAK
        str%0, str%i := i, ch
        ticks := 10*ticks + ch - '0'
      }
      // The digits should be followed by ]
      UNLESS ch=']' DO
      { // Not a tick item
        wrch('[')
        wrch('#')
        writes(str)
        GOTO next
      }
      // It was a tick item and ticks holds the count
      IF ticks=0 LOOP
      IF ticks=1 DO { binwrch(time_tick1); LOOP }
      binwrch(time_char)
      WHILE ticks>=255 DO { binwrch(255); ticks := ticks-255 }
      binwrch(ticks)
      LOOP
    }

    wrch(ch)
  } REPEAT

fin:
  UNLESS instream =stdin  DO endstream(instream)
  UNLESS outstream=stdout DO endwrite()//endstream(outstream)
}
