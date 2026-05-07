// (C) Copyright 1981 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

// Modified 23 July 1981 by BJK: can be CALLSEGed to set user's default
//                               directory. (Use arg of -1)

// Modified 21 November 1981 by CGG: uses "HOME:" as default directory


SECTION "SET"

GET "LIBHDR"
GET "CLIHDR"

LET start(start.arg) BE
  $( LET argv               = VEC 50
     LET running.as.command = start.arg=0 // Otherwise, assume CALLSEGed
     LET newdir             = currentdir
     LET olddir             = currentdir
     LET comdir             = cli.commanddir
     LET rc                 = 0

     TEST running.as.command
     THEN IF rdargs("DIR,COMDIR/K", argv, 50)=0
          THEN $( writes("Bad args*N"); stop(20) $)
     ELSE $( argv!0 := 0; argv!1 := 0 $)


     IF argv!0=0 & argv!1=0 THEN
     $(  argv!0 := "HOME:"
         currentdir := 0
     $)


     UNLESS argv!0=0 DO
     $( LET dir = locatedir(argv!0)
        TEST dir=0 THEN
        $( writef("Can't find %S*N", argv!0)
           rc := 20
        $)
        ELSE
           newdir := dir
     $)

     UNLESS argv!1=0 DO
     $( LET dir = locatedir(argv!1)
        TEST dir=0 THEN
        $( writef("Can't find %S*N", argv!1)
           rc := 20
        $)
        ELSE
           comdir := dir
     $)

     UNLESS olddir=newdir DO freeobj(olddir)
     UNLESS cli.commanddir=comdir DO freeobj(cli.commanddir)
     currentdir := newdir
     cli.commanddir := comdir

     IF running.as.command THEN stop(rc)
  $)
