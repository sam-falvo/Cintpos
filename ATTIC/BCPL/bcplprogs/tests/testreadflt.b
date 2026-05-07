GET "libhdr"

LET start() = VALOF
{ LET argv = VEC 50
  LET stdin = input()
  LET instream = 0
  LET filename = "floatdata"

  UNLESS rdargs("file", argv, 50) DO
  { writef("Bad arguments for testreadflt*n")
    RESULTIS 0
  }

  IF argv!0 DO filename := argv!0

  instream := findinput(filename)
  UNLESS instream DO  
  { writef("Cannot open file: %s*n*n", filename)
    RESULTIS 0
  }

  selectinput(instream)

  { LET val = readflt()
    IF result2 BREAK
    writef("%15g*n%15.1f %15.2f %15.3f %15.4f %15.5f %15.6f %15.7f*n",
            val,  val,  val,  val,  val,  val,  val,  val)
  } REPEAT

  UNLESS instream=stdin DO endread()
  selectinput(stdin)
  writef("End of test*n")
  RESULTIS 0
}
