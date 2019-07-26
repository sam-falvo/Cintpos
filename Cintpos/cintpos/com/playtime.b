// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory


// Program to calculate the time it would take
// to PLAYBACK a file produced by RECORD.

SECTION "Playtime"

GET "libhdr"

MANIFEST
    {
    time_char    = 255 // Escape introducing timing info
    time_tick1   = 254 // Special escape for 1 tick
    msecspertick = 20
    }


LET start() BE
{ LET argv = VEC 30
  LET instream = ?
  LET mins, msecs = 0, 0

  UNLESS rdargs("From/a", argv, 30) DO
  { writes("Invalid args to PLAYTIME*N")
    stop(20)
  }

  instream := findinput(argv!0)
  UNLESS instream DO
  { writef("Can't open %S*N", argv!0)
    stop(20)
  }
  selectinput(instream)

  { // Main loop
    LET ch = binrdch()
    IF ch = endstreamch BREAK
//writef("%7.3d: ch = %i3  '%c'*n", msecs, ch, 32<=126 -> ch, ' ')
    UNLESS ch=time_char | ch=time_tick1 LOOP

    // Tick encoding:

    // FE                = 1 tick
    // FF FF .. FF hh    = 255 + 255 + ... + hh ticks

    TEST ch = time_tick1
    THEN msecs := msecs + msecspertick
    ELSE { // > 1 tick
           ch := binrdch()
           msecs := msecs + ch * msecspertick
         } REPEATWHILE ch = 255
//abort(1000)
  } REPEAT

  endread()

  mins  := msecs  /  60000

  msecs := msecs MOD 60000

  TEST mins=0 & msecs=0
  THEN writef("%s is not in PLAYBACK format*n", argv!0)
  ELSE { writes("Playback would take ")
         IF mins DO writef("%n min%-%ps ", mins)
         writef("%5.3d secs*n", msecs)
       }
}

