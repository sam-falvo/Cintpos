// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

SECTION "PLAYFAST"

GET "g/libhdr.h"

LET start() BE
$( LET args = VEC 50
   LET instream = 0
   LET outstream= 0

   IF rdargs("FROM/A,TO/K", args, 50)=0 DO
   $( writes("bad arguments for playfast*n")
      stop(10)
   $)

   instream := findinput(args!0)
   IF instream=0 DO
   $( writef("can't open %s*n", args!0)
      stop(10)
   $)
   selectinput(instream)
   UNLESS args!1=0 DO
   $( outstream := findoutput(args!1)
      IF outstream=0 DO
      $( writef("can't open %s*n", args!1)
         endread()
         stop(10)
      $)
      selectoutput(outstream)
   $)

   $( LET ch = rdch()
      IF ch=endstreamch BREAK
      IF testflags(1) THEN
      $( newline()
         BREAK
      $)
      IF ch='*c' LOOP
      IF ch<254 DO wrch(ch)
      WHILE ch=255 DO ch := rdch()
   $) REPEAT
   endread()
   UNLESS outstream=0 DO endwrite()
$)
