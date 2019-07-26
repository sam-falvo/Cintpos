GET "mcpl.h"

FUN start : =>
  LET argv = VEC 50

  UNLESS rdargs("FROM", argv, 50) DO
  { writes "Bad argument for SPEAK\n"
    RETURN 20
  }
  LET aufilename = "au/hello.au"
  IF argv!0 DO aufilename := argv!0
  LET aufile = findinput aufilename
  UNLESS aufile DO
  { writef("Can't open %s\n", aufilename)
    RETURN 20
  }
  selectinput aufile

  LET speaker = findoutput "/dev/audio"
  UNLESS speaker DO
  { writes "Can't open /dev/audio\n"
    endread()
    RETURN 20
  }
  selectoutput speaker

  { LET ch = rdch()
    IF ch=Endstreamch BREAK
    wrch ch
  } REPEAT

  endread()
  endwrite()
  RETURN 0