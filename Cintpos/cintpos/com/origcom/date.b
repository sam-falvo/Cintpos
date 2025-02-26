// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

// Purpose:
// To print or set the system date
//
// Use:
//      DATE
//      prints out the current system date
//           DD-MMM-YY
//      DATE <dayname>
//           Yesterday/Today/Tomorrow
//      resets the system date
//      <dayname> refers to today or a future day
//
// Authors:
// Brian Knight         1978 October
// Paul Bond            1979 April

SECTION "date"

GET "g/libhdr.h"

GET "g/clihdr.h"

LET start() BE
$(
   LET string_to_dat_overlay = "l/string-to-dat"
//   LET string_to_dat_overlay = "SYS:L.STRING-TO-DAT"
   LET standout, ver_stream = output(), 0
   LET v = VEC 50

   IF rdargs("IS,TO=VER/K", v, 50) = 0 THEN error()

   UNLESS v!1 = 0 THEN
   $( ver_stream := findoutput(v!1)
      IF ver_stream = 0 THEN
      $( writef("******Can*'t open %S*N", v!1)
         stop(20)
      $)
      selectoutput(ver_stream)
   $)

   TEST v!0 = 0
   THEN
   $( datstring(v)
      writef("%S %S*N", v+10, v)
        // print date
   $)
   ELSE
   $( // want to set new date
      LET stamp = VEC 2
      IF callseg(string_to_dat_overlay, stamp, v!0) = 0 THEN
      $( TEST result2 = 0
         THEN error()
         ELSE writef("******Can't load %S*N", string_to_dat_overlay)
         stop(20)
      $)
      rootnode!Rtn_days := stamp!0 + [result2=1 -> 7, 0]
   $)

   UNLESS output() = standout THEN
   $( endwrite()
      selectoutput(standout)
   $)
$)

AND error() BE
$(
   writes("******Bad args - use DD-MMM-YY or <dayname> or yesterday etc.*N")
   stop(20)
$)
