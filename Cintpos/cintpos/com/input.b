// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "INPUT"

GET "libhdr"

LET start() BE
{ LET argv = VEC 50
  LET save = VEC 50
  LET tlen = ?
  LET term, stream = "/**", ?
  UNLESS rdargs("TO/A,TERM/K",argv,50) DO
  { writes("Bad args*N")
    stop(20)
  }
  stream := findoutput(argv!0)
  UNLESS stream DO
  { writef("Can't open %s*n", argv!0)
    stop(20)
  }

  selectoutput(stream)

  IF argv!1 DO term := argv!1
  tlen := term%0

  { LET t, ch = 1, ?
    ch := rdch()
    WHILE t<=tlen & compch(ch,term%t)=0 & ch~='*n' DO
    { save%t := ch
      t := t+1
      ch := rdch()
    }
    IF t>tlen & ch='*n' BREAK
    FOR j = 1 TO t-1 DO wrch(save%j)
    IF ch=endstreamch GOTO ended
    wrch(ch)
    { IF testflags(flag_b) DO
      { writes("****BREAK*n")
        endwrite()
        stop(10)
      }
      IF ch='*n' BREAK
      ch := rdch()
      IF ch=endstreamch GOTO ended
      wrch(ch)
    } REPEAT
  } REPEAT

ended:
  endwrite()
}
