// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "WAIT"

GET "libhdr"

MANIFEST {
  msecspermin = 60000
  msecsperhour = msecspermin*60
  msecsperday = msecsperhour*24
}

LET start() = VALOF
{ LET argv = VEC 50
  LET hour, min, sec = 0, 0, 0
  LET days, msecs, ticks = 0, 0, 0
  LET daysnow, msecsnow, ticksnow = 0, 0, 0
  LET n = 0

  // Get the date and time now
  datstamp(@daysnow)

  UNLESS rdargs("N/N,SEC=SECS/S,MIN=MINS/S,UNTIL/K", argv, 50) DO
  { error(1)
    RESULTIS 20
  }

  UNLESS argv!3 DO                   // UNTIL/K
  { n := 1
    IF argv!0 DO n := !(argv!0)      // N/N

    TEST argv!2                      // MINS/S
    THEN msecs := n * 60000
    ELSE msecs := n * 1000
    msecs := msecs + msecsnow
    days := daysnow
    IF msecs > msecsperday DO days, msecs := days+1, msecs-msecsperday
//writef("wait: daysnow=%n msecsnow=%n days=%n msecs=%n*n",
//        daysnow, msecsnow, days, msecs)
  }

  IF argv!3 DO                       // UNTIL/K
  { LET s = argv!3
    UNLESS s%0=5 DO { error(3); RESULTIS 20 }
    FOR i=1 TO 5 UNLESS i=3 -> s%i=':', '0'<=s%i<='9' DO
    { error(3)
      RESULTIS 20
    }
    hour := (s%1-'0')*10+s%2-'0'
    IF hour>=24 DO error(3)
    min := (s%4-'0')*10+s%5-'0'
    IF min>=60 DO
    { error(3)
      RESULTIS 20
    }
    msecs := hour*msecsperhour + min*60000
    days := daysnow
    IF msecs<msecsnow DO days := days+1
  }

  //{ LET str = VEC 14
  //  dat_to_strings(@days, str)
  //  sawritef("Delaying until %s %s*n", str, str+5)
  //}


  { LET ds, ms, tks = ?, ?, ?
//sawritef("wait: Calling datstamp(%n)*n", @ds)
    datstamp(@ds)
    // Assume new dat format
    IF (ds>days) |
       (ds=days & ms>=msecs) BREAK
//sawritef("wait: ds=%n ms=%n*n", ds, ms)
//sawritef("wait: delaying for 500 msecs until days=%n msecs=%n*n",
//             days, msecs)
    delay(500)
    IF testflags(flag_b) DO
    { writes("****BREAK*N")
      RESULTIS 10
    }
  } REPEAT

//sawritef("wait: done*n")

  RESULTIS 0
}


AND error(n) BE
{ writes(n=1 -> "Bad args*N",
         n=2 -> "Error in number*N",
                "Time should be HH:MM*N")
}
