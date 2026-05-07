/*******************************************************************************
**              (C) Copyright  Tripos Research Group 1980                     **
**             University of Cambridge Computer Laboratory                    **
********************************************************************************

 ######    ########   ######     #####   ########  ##    ##  ########   ######
 #######   ########  ########   #######  ########  ###   ##  ########  ########
 ##    ##     ##     ##        ##           ##     ####  ##  ##        ##    ##
 ##    ##     ##     #######   ##           ##     ## ## ##  ######    ##    ##
 ##    ##     ##           ##  ##           ##     ##  ####  ##        ##    ##
 ##    ##     ##           ##  ##           ##     ##  ####  ##        ##    ##
 #######   ########  ########   #######  ########  ##   ###  ##        ########
 ######    ########   ######     #####   ########  ##    ##  ##         ######

********************************************************************************
**      Author : Mike Richardson                                     1980     **
*******************************************************************************/


SECTION "DISCINFO"
GET     "LIBHDR"
GET     "MANHDR"
GET     "IOHDR"

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
   LET fhtask, lvfhtask = ?, ?
   AND   argv           = VEC 20
   AND prefix           = VEC 30 / bytesperword

   LET device, unit     = ?, ?
   AND devok            = ?
   AND infovec          = VEC discinfo.alloc

   LET res              = rdargs ( "device", argv, 20 )
   AND task             = currentdir = 0 -> task.filehandler,
                                            currentdir ! lock.task

   UNLESS ( res = 0 ) | ( argv ! 0 = 0 ) DO
            res := splitname ( prefix, ':', argv ! 0, 1 )



   fhtask := argv ! 0 = 0 -> task, devicetask ( argv ! 0 )

   IF fhtask = 0 THEN
   $(
      writef ( "*"%S*" is not a mounted device*N", argv ! 0 )
      stop   ( 20 )
   $)

   res := sendpkt ( notinuse, fhtask, action.discinfo, ?, ?, infovec )

   UNLESS res DO
   $(
      writef ( "error in reply to *"discinfo*" request - discinfo failed*N" )
      stop   ( 20 )
   $)

   device := infovec ! discinfo.type
   devok  := ( device = disctype.floppy ) |
             ( device = disctype.cart   ) |
             ( device = disctype.big    ) |
             ( device = disctype.real   ) |
             ( device = disctype.fs     )


   unit   := infovec ! discinfo.unit


   TEST NOT devok
   THEN
        writef ( "Disc is of unrecognised type ( code %N ) unit %N*N",
                  device, unit
               )
   ELSE
        writef ( "Disc is %S: ( %S )*N",
                  VALOF
                     $(
                        LET ass = rootnode ! rtn.info ! info.assignments

                        UNTIL ( ass = 0 ) |
                              ( ass ! ass.task = fhtask &
                                ass ! ass.dir  = 0 )
                                                     DO ass := ass ! ass.link

                        RESULTIS ass = 0 -> "???", ass + ass.name
                     $),
                  device = disctype.floppy -> "floppy disc",
                  device = disctype.cart   -> "cartridge drive",
                  device = disctype.big    -> "big disc pack",
                  device = disctype.fs     -> "fileserver filing system",
                                              "unspecified real drive" )

   SWITCHON infovec ! discinfo.state INTO
   $(
      CASE discstate.writeprotected    : writes ( "Disc is write protected*N" )
                                         ENDCASE

      CASE discstate.discnotvalidated  : writes ( "Disc is not validated*N" )
                                         ENDCASE

      CASE discstate.discvalidated     : ENDCASE

      DEFAULT                 : writef ( "Unrecognised disc state code %N*N",
                                          infovec ! discinfo.state )
   $)

   IF device \= disctype.fs THEN
     $(
        IF infovec ! discinfo.state = discstate.discvalidated THEN
           writef ( "Total %N blocks = %N used + %N free ( %N%% free )*N",
                     infovec ! discinfo.space , infovec ! discinfo.used,
                     infovec ! discinfo.space - infovec ! discinfo.used,
           muldiv (  infovec ! discinfo.space - infovec ! discinfo.used, 100,
                     infovec ! discinfo.space
                  ) )

        writef ( "Block size is %N bytes*N", infovec ! discinfo.alloc )
     $)
$)
