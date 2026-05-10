SECTION "JOIN"

GET "libhdr"

LET start() = VALOF
{ LET argv = VEC 130
  LET temp = "TEMPFILE"
  LET tofilename = 0
  LET stdout = output()
  LET stdin = input()
  LET instream = 0
  LET outstream = 0
  LET rc = 0
  IF rdargs(",,,,,,,,,,,,,,,TO/A/K", argv, 100)=0 DO
  { writef("Bad args for JOIN*N")
    RESULTIS 20
  }
  
  tofilename := argv!15

  IF compstring(tofilename, "**")=0 DO temp := tofilename

  outstream := findoutput(temp)
  UNLESS outstream DO { writef("Can't open %s for output*n", temp)
                        rc := 20
                        GOTO ret
                      }
  selectoutput(outstream)
  
  FOR i = 0 TO 14 DO
  { LET filename = argv!i
    UNLESS filename BREAK  // No more files to join
    instream := findinput(filename)
    UNLESS instream DO  { selectoutput(stdout)
                          writef("Can't open %s for input*n", filename)
                          rc := 20
                          GOTO ret
                        }
    selectinput(instream)

    { LET ch = rdch()
      IF ch=endstreamch BREAK
      IF intflag() DO { selectoutput(stdout)
                        writes("****BREAK*n")
                        rc := 10
                        GOTO ret
                      }
      wrch(ch)
    } REPEAT

    endstream(instream)
    instream := 0
  }
  
  UNLESS outstream=stdout DO
  { endstream(outstream)
    outstream := 0
  }

  UNLESS compstring(tofilename, "**")=0 DO
  { // Copy temp to tofilename
    instream := findinput(temp)
    outstream := findoutput(tofilename)
    UNLESS instream DO  { selectoutput(stdout)
                          writef("Can't open %s for input*n", temp)
                          rc := 20
                          GOTO ret
                        }
    
    UNLESS outstream DO  { selectoutput(stdout)
                           writef("Can't open %s for output*n", tofilename)
                           rc := 20
                           GOTO ret
                        }
		      
    selectinput(instream)
    selectoutput(outstream)
    
    { LET ch = rdch()
      IF ch=endstreamch BREAK
      IF intflag() DO { selectoutput(stdout)
                        writes("****BREAK*n")
                        rc := 10
                        GOTO ret
                      }
      wrch(ch)
    } REPEAT

    endstream(instream)
    instream := 0
    endstream(outstream)
    outstream := 0
  }
  
ret:
  IF outstream UNLESS outstream=stdout DO
                 endstream(outstream)
  IF instream  UNLESS instream=stdin DO
                 endstream(instream)

  selectoutput(stdout)
  UNLESS compstring(tofilename, "**")=0 DO deletefile(temp)
  RESULTIS rc
}
