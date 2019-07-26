// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

/*
usage:

alarm [+]hh:mm:ss <message>
alarm [+]mm:ss <message>
alarm [+]ss <message>

+ means relative delay
*/

SECTION "ALARM"

GET "libhdr"

GLOBAL
{ timestr : ug
  timestrp
  timestrupb
  ch
}

MANIFEST
{ secsperday  = 24*60*60
}

LET start() = VALOF
{ LET argv  = VEC 50
  AND tv = VEC 14 // Used by datstamp and datstring
  AND secs  = 0
  AND relative = FALSE
  AND secstowait = ?
  AND nowsecs, nowticks = ?, ?
  AND message = "no message"

  UNLESS rdargs("time=at/A,message", argv, 50) DO error()

  timestr, timestrp := argv!0, 1
  timestrupb := timestr%0
  IF argv!1 DO message := argv!1

  rch()
  IF ch='+' DO { relative := TRUE; rch() }

  IF rdn()  DO secs := result2
  IF ch=':' DO { rch()
                 IF rdn() DO secs := 60*secs + result2
               }
  IF ch=':' DO { rch()
                 IF rdn() DO secs := 60*secs + result2
               }
  UNLESS ch=endstreamch DO error()

//writef("*ndelaying %s%n secs*n", (relative->"+", " "), secs)
//GOTO fin

  datstamp(tv)
  nowsecs := tv!1 / 1000

  IF relative DO secs := secs + nowsecs
  IF secs < nowsecs DO secs := secs + secsperday

  { LET hour = secs / 60 / 60 MOD 24
    LET min  = secs / 60 MOD 60
    LET sec  = secs MOD 60
    writef("Alarm set for %n:%z2:%n message: %s*n", hour, min, sec, message)
  }

  { // Loop
    datstamp(tv)
    nowsecs := tv!1 / 1000
    //writef("secs=%n nowsecs=%n*n", secs, nowsecs)

    IF secs <= nowsecs BREAK
    delay(1000) // Wait one second (=1000 msecs)
    IF testflags(flag_b) DO
    { writef("Alarm for %s cancelled*N", timestr)
      GOTO fin
    }
  } REPEAT

  FOR j = 1 TO 50 DO wrch(7) // ASCII Bells
  deplete(cos)
  writef("*n****** Alarm: time is %s - %s*n", datstring(tv) + 5, message)

fin:
  RESULTIS 0
}

AND rch() BE TEST timestrp>timestrupb
THEN ch := endstreamch
ELSE { ch := timestr%timestrp
       timestrp := timestrp + 1
     }

AND rdn() = VALOF
{ LET val, ok = 0, FALSE
  WHILE '0'<=ch<='9' DO { ok, val := TRUE, 10*val+ch-'0'; rch() }
  result2 := val
  RESULTIS ok
}


AND error() BE
{ writes("Invalid parameter: format is [+][[hours:]minutes:]seconds*n")
  stop(0, 0)
}
