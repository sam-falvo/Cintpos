// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "PLAYFAST"

GET "libhdr"

LET start() BE
{ LET args = VEC 50
  LET instream = 0
  LET outstream= 0

  IF rdargs("FROM/A,TO/K", args, 50)=0 DO
  { writes("bad arguments for playfast*n")
     stop(10)
  }

  instream := findinput(args!0)
  UNLESS instream DO
  { writef("can't open %s*n", args!0)
    stop(10)
  }
  selectinput(instream)
  IF args!1 DO
  { outstream := findoutput(args!1)
    UNLESS outstream DO
    { writef("can't open %s*n", args!1)
      endread()
      stop(10)
    }
    selectoutput(outstream)
  }

  rdch := binrdch

  { LET ch = binrdch()
    IF ch=endstreamch BREAK
    IF testflags(flag_b) DO
    { newline()
      BREAK
    }
    IF ch='*c' LOOP
    IF ch<254 DO { wrch(ch); LOOP }
//sawritef("tick ch=%i3 (%o3)*n", ch, ch)
    writef("[%x2]", ch)
    WHILE ch=255 DO
    { ch := binrdch()
//sawritef("tick ch=%i3 (%o3)*n", ch, ch)
      writef("[%x2]", ch)
    }
//abort(1000)
  } REPEAT

  endread()
  IF outstream DO endwrite()
}
