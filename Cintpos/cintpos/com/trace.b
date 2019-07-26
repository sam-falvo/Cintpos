/*
trace on   Turn on low level tracing

trace dump   Output the low level trace buffer to standard output

trace to <file>  Output the low level trace buffer to the specified file

Implementented by Martin Richards (c) March 2010
*/

SECTION "Trace"

GET "libhdr"

GLOBAL {
  stdin:ug
  stdout
  toname
  tostream
  layout
  dumping
}

LET start() = VALOF
{ LET q = sys(Sys_settrcount, -1) // Disable low level tracing
  LET argv = VEC 50

  stdin := input()
  stdout := output()
  toname := 0
  tostream := 0

  dumping := TRUE

  UNLESS rdargs("TO/K,ON/S", argv, 50) DO
  { writef("Bad args for TRACE*n")
    GOTO fin
  }

  IF argv!0 DO toname := argv!0         // TO/K
  IF argv!1 DO dumping := FALSE         // ON/S

  UNLESS dumping DO
  { writef("*nLow level tracing turned ON*n")
    sys(Sys_settrcount, 0)
    GOTO fin
  }

  IF toname DO
  { tostream := findoutput(toname)
    UNLESS tostream DO
    { writef("Trouble with file %s*n", toname)
      GOTO fin
    }
    writef("Dumping low level trace buffer to file %s*n", toname)
    selectoutput(tostream)
  }

  dump(q)

fin:
  IF tostream & tostream~=stdout DO endstream(tostream)
  selectoutput(stdout)

  RESULTIS 0
}

AND dump(q) BE
{ LET p = q-4096
  LET layout = 0
  LET time = 0
  IF p<0 DO p := 0
  WHILE p<q DO
  { LET val = sys(Sys_gettrval, p)
    LET flag = val>>24
    val := val & #xFFFFFF
    TEST flag=#x66
    THEN time := val
    ELSE { 
           IF layout MOD 4 = 0 DO newline()
           layout := layout+1
           writef(" %i6/%x2/%c/%6.3d",
                   val, flag, (32<=val<=126 -> val, ' '), time)
         }
    p := p+1
  }
  newline()
}
