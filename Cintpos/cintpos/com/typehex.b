// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

GET "libhdr"

LET start() BE
{ LET args      = VEC 50
  LET sysout    = output()
  LET instream  = 0
  LET outstream = 0
  LET word      = -1
  LET wdcount   = 0

  IF rdargs("FROM/A,TO/K", args, 50) = 0 THEN
  { writes("bad arguments for TYPEHEX*n")
    stop(20)
  }

  instream := findinput(args!0)
  IF instream = 0 DO
  { writef("can't open %s*n", args!0)
    stop(20)
  }
  selectinput(instream)

  IF args!1 DO
  { outstream := findoutput(args!1)
    UNLESS outstream DO
    { writef("can't open %s*n", args!1)
      endread()
      stop(20)
    }
    selectoutput(outstream)
  }

  { LET len = readwords(@ word, 1)
    writef("%x8",word)
    wdcount:=wdcount+1
    wrch(wdcount REM 8 ->'*s','*n')
    word := -1

    UNLESS len=1 BREAK

    IF testflags(flag_b) DO
    { selectoutput(sysout)
      writes("*n******BREAK*N")
      GOTO out
    }
  } REPEAT

  IF wdcount REM 8 DO newline()

out:
  endread()
  UNLESS sysout=outstream DO endstream(outstream)
}
