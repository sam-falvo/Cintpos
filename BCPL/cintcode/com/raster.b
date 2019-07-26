/*
Written by M. Richards  (c) 11 Sep 2016

Typical usage when run under rastsys:

> raster count 1000 scale 12 to RASTER
> bcpl com/bcpl.b to junk
> raster close

This will generate the file RASTER starting something like:

K1000 S12          1000 instruction per raster line, 12 bytes per pixel
W10B3W1345B1N      10 white 3 black 1345 white 1 black newline
W13B3W12B2N        etc
...

This can be converted to a PostScript image using the rast2ps command.

If the sound option is given to raster it will generate a bit stream
file derived from the fifth bit of every memory reference. typical
usage:

raster sound
bcpl com/bcpl.b to junk
raster close

The bit stream file can be converted into a .wav file using rast2wav.
*/


SECTION "RASTER"

GET "libhdr"

LET start() = VALOF
{ LET argv = VEC 40
  AND outstream = 0
  AND tallyv = rootnode!rtn_tallyv
  AND oldout = output()
  AND count, scale = 1000, 12
  AND tofilename = "RASTER"      // The default filename
  AND sound = FALSE
  AND closing = FALSE

  UNLESS rdargs("COUNT/N,SCALE/N,TO/K,SOUND/S,CLOSE/S,HELP/S",argv, 40) DO
  { writes("Bad arguments for RASTER*n")
    RESULTIS 20
  }

  IF argv!0 DO count := !(argv!0)       // COUNT/N
  IF argv!1 DO scale := !(argv!1)       // SCALE/N
  IF argv!2 DO tofilename := argv!2     // TO/K
  sound := argv!3                       // SOUND/S
  closing := argv!4                     // CLOSE/S
  
  IF argv!5 DO                          // HELP/S
  { writes("*nTypical usage:*n*n")
    writes("    raster count 1000 scale 12 to rastdata*n")
    writes("    bcpl com/bcpl.b to junk*n")
    writes("    ...*n")
    writes("    raster close*n")
    writes("    rast2ps*n")
    writes("*nor*n*n")
    writes("    raster sound*n")
    writes("    bcpl com/bcpl.b to junk*n")
    writes("    ...*n")
    writes("    raster close*n")
    writes("    rast2wav*n")

    writes("*n    Remember to use rastsys (NOT cintsys)*n")
    RESULTIS 0
  }

  // First test to see if rastering is available.
  IF sys(Sys_setraster, 3)=0 DO
  { writes("Rastering is not available*n")
    RESULTIS 20
  }

//writef("args: %n %n %n %n*n", argv!0, argv!1, argv!2, argv!3)
//writef("tofilename=%s count=%n scale=%n sound=%n*n", tofilename, count, scale, sound)

  // If raster has no arguments it will close the raster file.
  IF closing DO
  { LET res = sys(Sys_setraster, 5, 0) // Attempt to close raster file
    TEST res=0 THEN writes("Raster file closed*n")
               ELSE writes("Unable to close raster file*n")
    RESULTIS 0
  }
   
  TEST sound
  THEN { // Generate a file containing a bit stream of the
         // fifth bit of the address of every memory reference
         writef("Raster generating a bit stream of sound*n")
         sys(Sys_setraster, 4, 1)  // Set sound generation
       }
  ELSE { writef("*nRastering to file %s with count = %n and scale = %n*n",
                tofilename, count, scale)
//sawritef("Calling setraster 4 0*n")
         sys(Sys_setraster, 4, 0)  // Set raster generation
//sawritef("Calling setraster 1 %n*n", count)
         sys(Sys_setraster, 1, count)
//sawritef("Calling setraster 2 %n*n", scale)
         sys(Sys_setraster, 2, scale)
       }

  // sound=1 or count and scale have been set in rastlib

  // Specify the raster file and start rastering
//sawritef("Calling setraster 0 %s*n", tofilename)
  UNLESS sys(Sys_setraster, 0, tofilename) DO // Attempt to open raster file
  { writef("Trouble opening raster file: %s*n", tofilename)
    RESULTIS 20
  }

  cli_tallyflag := TRUE  // Tell the CLI to start rastering
  RESULTIS 0
}
