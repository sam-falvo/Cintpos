// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory


// Program to calculate the time it would take
// to PLAYBACK a file produced by RECORD.

SECTION "Playtime"

GET "libhdr"

MANIFEST
    $(
    time_char  = 255 // Escape introducing timing info
    time_tick1 = 254 // Special escape for 1 tick
    $)


LET start() BE
    $(
    LET argv = VEC 30
    LET instream = ?
    LET mins, secs, ticks = 0, 0, 0

    IF rdargs("From/a", argv, 30) = 0
    THEN
      $(
      writes("Invalid args to PLAYTIME*N")
      stop(20)
      $)

    instream := findinput(argv!0)
    IF instream=0
    THEN
      $(
      writef("Can't open %S*N", argv!0)
      stop(20)
      $)
    selectinput(instream)

      $( // Main loop
      LET ch = rdch()
      IF ch = endstreamch THEN BREAK
      UNLESS (ch=time_char) | (ch=time_tick1) THEN LOOP

      TEST ch = time_tick1
      THEN ticks := ticks + 1
      ELSE
        $( // > 1 tick
        ch := rdch()
        ticks := ticks + ch
        $) REPEATWHILE ch = 255

      secs := secs + (ticks/tickspersecond)
      ticks := ticks REM tickspersecond
      mins  := mins + secs/60
      secs  := secs REM 60
      $) REPEAT

    endread()

    TEST (mins=0) & (secs=0)
    THEN writef("%S is not in PLAYBACK format*N", argv!0)
    ELSE
      $(
      writes("Playback would take ")
      UNLESS mins=0 THEN writef("%N min%S ", mins,
                                 (mins=1) -> "", "s")
      writef("%N sec%S*N", secs, (secs=1) -> "", "s")
      $)
    $)

