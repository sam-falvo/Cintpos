/*******************************************************************************
**              (C) Copyright  Tripos Research Group 1980                     **
**             University of Cambridge Computer Laboratory                    **
********************************************************************************

           #######    ######   ##    ##    ####    ########  ########
           ########  ########  ##    ##   ######   ########  ########
           ##    ##  ##        ##    ##  ##    ##     ##        ##
           ########  #######   ##    ##  ########     ##        ##
           #######         ##  ## ## ##  ##    ##     ##        ##
           ##  ##          ##  ########  ##    ##     ##        ##
           ##   ##   ########  ###  ###  ##    ##  ########     ##
           ##    ##   ######   ##    ##  ##    ##  ########     ##

********************************************************************************
**      Author : Mike Richardson                                     1980     **
*******************************************************************************/


SECTION "RSWAIT"
GET     "LIBHDR"
GET     "MANHDR"

MANIFEST
$(
   discinfo.type         = 0
   discinfo.unit         = 1
   discinfo.state        = 2
   discinfo.space        = 3
   discinfo.used         = 4
   discinfo.alloc        = 5

   disctype.floppy       = 90
   disctype.cart         = 91
   disctype.big          = 92
   disctype.fs           = 93
   disctype.real         = 94

   discstate.writeprotected   = 80
   discstate.discnotvalidated = 81
   discstate.discvalidated    = 82

    lock.task                 = 3
$)


LET start () BE
$(
   LET fhtask           = ?
   AND   argv           = VEC 20
   AND rdargs.string    = "device,quiet/s"

   LET device, unit     = ?, ?
   AND devid            = ?
   AND infovec          = VEC discinfo.alloc

   LET res              = rdargs ( rdargs.string, argv, 20 )
   AND task             = currentdir = 0 -> task.filehandler,
                                            currentdir ! lock.task
   AND quiet            = ?

   IF res = 0
   THEN $( writef("Bad args for keys *"%s*"*n", rdargs.string); stop(20) $)


   fhtask := argv ! 0 = 0 -> task, devicetask ( argv!0 )
   quiet  := argv ! 1 \= 0

   IF ( fhtask = 0 ) THEN
   $(
      writef ( "*"%S*" is not a mounted device*N", argv ! 0 )
      stop   ( 20 )
   $)


   res := sendpkt ( notinuse, fhtask, action.discinfo, ?, ?, infovec )

   UNLESS res DO
   $(
      writef ( "error in reply to *"discinfo*" reply - rswait failed*N" )
      stop   ( 20 )
   $)

   device := infovec ! discinfo.type
   devid  := device = disctype.floppy -> "DF",
             device = disctype.cart   -> "DK",
             device = disctype.big    -> "DP",
             device = disctype.fs     -> "FS",
             device = disctype.real   -> "DR", 0
   unit   := infovec ! discinfo.unit


   IF infovec ! discinfo.state = discstate.writeprotected THEN
   $(
      writef ( "Disc %S%N: is write protected*N", devid, unit )
      FINISH
   $)

   IF infovec ! discinfo.state = discstate.discvalidated THEN
   $(
      UNLESS quiet
      THEN writef ( "Restart already completed for %S%N:*N", devid, unit )
      FINISH
   $)

   UNLESS quiet
   THEN writef ( "Task %N: waiting for restart on %S%N to finish*N",
                  taskid, devid, unit                          )

   $(
      delay ( tickspersecond )

      sendpkt ( notinuse, fhtask, action.discinfo, ?, ?, infovec )

      IF infovec ! discinfo.state = discstate.discvalidated THEN BREAK

      IF testflags ( 1 ) THEN
      $(
         writes ( "******BREAK*N" )
         BREAK
      $)
   $) REPEAT
$)
