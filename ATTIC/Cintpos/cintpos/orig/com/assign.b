/***********************************************************************
**             (C) Copyright 1980  TRIPOS Research Group              **
**            University of Cambridge Computer Laboratory             **
************************************************************************

         ####     ######    ######   ########   ######   ##    ##
        ######   ########  ########  ########  ########  ###   ##
       ##    ##  ##        ##           ##     ##        ####  ##
       ########  #######   #######      ##     ##  ####  ## ## ##
       ##    ##        ##        ##     ##     ##    ##  ##  ####
       ##    ##        ##        ##     ##     ##    ##  ##  ####
       ##    ##  ########  ########  ########  ########  ##   ###
       ##    ##   ######    ######   ########   ######   ##    ##

************************************************************************
**    Author:   Adrian Aylward                        April 1980      **
***********************************************************************/



SECTION "ASSIGN"

GET "LIBHDR"
GET "IOHDR"
GET "FH2MANIFESTS"

LET start() BE
 $( LET argv = VEC 50
    LET name = VEC 30/bytesperword
    LET dir = 0
    LET rc = 0

    IF rdargs("NAME,DIR,LIST/S", argv, 50)=0 DO
    $( writes("Bad args*N")
       stop(20)
    $)

    TEST argv!0=0 THEN
       IF argv!1\=0 DO
       $( writes("No NAME specified*N")
          rc := 20
       $)
    ELSE
    $( LET ptr = splitname(name, ':', argv!0, 1)
       UNLESS (ptr-1)=argv!0%0 DO
       $( writef("Invalid device name %S*N", argv!0)
          rc := 20
       $)
    $)

    UNLESS argv!1=0 DO
    $( dir := locatedir(argv!1)
       IF dir=0 DO
       $( writef("Can't find %S*N", argv!1)
          rc := 20
       $)
    $)

    IF argv!0\=0 & rc<20 DO
    $( LET a = rootnode!rtn.info+info.assignments
       UNTIL !a=0 DO
       $( LET ass = !a
          IF compstring(name, ass+ass.name)=0 DO
          $( LET dir = ass!ass.dir
             TEST dir=0 THEN
             $( writef("Can't cancel %S*N", name)
                rc := 20
             $)
             ELSE TEST argv!1 = 0
                  THEN
                    $(
                    !a := !ass // Remove old assignment
                    freeobj(dir)
                    freevec(ass)
                    $)
                  ELSE $( writef("*"%s:*" is already assigned*n", name); rc := 20 $)
             BREAK
          $)
          a := ass
       $)
    $)

    IF argv!1\=0 & rc<20 DO
    $( LET size = name%0/bytesperword
       LET ass = getvec(ass.name+size)
       TEST ass=0 THEN
       $( writes("Run out of store*N")
          rc := 20
       $)
       ELSE
       $( ass!ass.link := rootnode!rtn.info!info.assignments
          ass!ass.task := dir!lock.task
          ass!ass.type := dt.disc
          ass!ass.dev := 0
          ass!ass.dir := dir
          FOR i = 0 TO size DO
             ass!(ass.name+i) := name!i
          rootnode!rtn.info!info.assignments := ass
          dir := 0
       $)
    $)

    UNLESS argv!2=0 DO
    $( LET ass = rootnode!rtn.info!info.assignments
       UNTIL ass=0 DO
       $( writef("Task %I2  %t6 %S:*N",
                 ass!ass.task,
                 [ass!ass.dir\=0 -> "dir",
                   (ass!ass.type=dt.disc -> "disc", "device")],
                 ass+ass.name)
          ass := ass!ass.link
       $)
    $)

    freeobj(dir)
    stop(rc)
 $)
