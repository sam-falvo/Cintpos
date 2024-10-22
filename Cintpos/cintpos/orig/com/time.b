// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "TIME"

GET "libhdr"

GLOBAL { timev : ug }

LET start() BE
{ // If no parameter is supplied, the current time
  // is printed.
  // If a valid time in the form HH:MM:SS is given,
  // then the system time is set to that value.

  LET v = VEC 50
  LET len = ?
  LET ticks = 0
  LET mins = 0
  LET hours, secs = ?, ?

  UNLESS rdargs("is", v, 50) DO error()

  timev := v!0
  IF timev DO len := timev%0

  TEST timev = 0
  THEN { // want to print time
         datstring(v)
         writes(v+5)
         newline()
       }
  ELSE { // want to set time to given value
         // Allow mins &/or secs to be omitted

         SWITCHON len INTO
         { CASE 8: // Full time
                   secs := digit(7)*10 + digit(8)
                   IF secs > 59 THEN error()
                   ticks := secs * tickspersecond

           CASE 6: // Just hours and mins
           CASE 5: mins := digit(4)*10 + digit(5)
                   IF mins > 59 THEN error()

           CASE 3: // Just hours
           CASE 2: hours := digit(1)*10 + digit(2)
                   IF hours > 23 THEN error()
                   mins := mins + (hours*60)
                   ENDCASE

           DEFAULT: error()
         }

         // Put new values in rootnode
         rootnode ! rtn.mins := mins
         rootnode ! rtn.ticks := ticks
       }
}

AND digit(n) = VALOF
{ LET d = timev%n
  UNLESS '0' <= d <= '9' THEN error()
  RESULTIS d - '0'
}

AND error() BE
{ writes("Invalid parameter:  format is HH:MM:SS or HH:MM or HH*N")
  stop(20)
}
