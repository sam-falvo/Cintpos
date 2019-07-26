SECTION "SKIP"

GET "libhdr"


GLOBAL { ch: ug }



LET start() = VALOF
{ LET arg= VEC 50
  LET label= ""
  LET found = FALSE
  LET s = VEC 255/bytesperword // For strings of length <= 255

  UNLESS rdargs("DESTINATION-LABEL",arg,50) DO
  { writes("Bad argument spec for Skip*n")
    stop(return_hard)
  }

  IF (cli_status & clibit_comcom) = 0 DO
  { writes("Skip is only allowed in command-commands*n")
    stop(return_hard, 0)
  }

  IF arg!0 DO label := arg!0

//sawritef("skip: destination-label=%s*n", label)
  ch:='*n'
  UNTIL found | ch=endstreamch DO
  { ch := rdch() REPEATWHILE ch=' ' | ch='*n'
    rdstr(s)
//sawritef("skip: s=%s*n", s)
    IF compstring(s, "lab")=0 DO
    { WHILE ch=' ' DO ch:=rdch()
      rdstr(s)
//sawritef("skip: lab found label=%s s=%s*n", label, s)
      found := compstring(s, label)=0
    }
    // Ignore the rest of the line
    UNTIL ch='*n' | ch=endstreamch DO ch := rdch()
  }

  UNLESS found DO
  { writef("Label *"%s*" not found by Skip*n", label)
    stop(return_hard, 0)
  }

  result2 := 0
  RESULTIS 0
}



AND rdstr(s) BE
{ // Until the next newline, space or EOF copy up to 255 input characters
  // other than '*c' into s, leaving the length in s%0.
  LET i=0

  UNTIL ch='*N' | ch=endstreamch | ch=' ' | i=255 DO
  { UNLESS ch='*c' DO
    { i := i+1
      s%i := ch
    }
    ch := rdch()
  }

  s%0 := i
}

