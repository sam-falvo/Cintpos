// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory


// Modified 21 November 1981 by CGG: uses "HOME:" as default directory



SECTION "INIT"

GET "LIBHDR"
GET "CLIHDR"

LET start() BE
 $( LET argv = VEC 15
    LET newdir = 0
    LET olddir = currentdir
    LET initstream = 0

    IF cli.background DO
    $( writes("INIT is illegal in background mode*N")
       stop(20)
    $)

    IF rdargs("USER", argv, 15)=0 DO
    $( writes("Bad args*N")
       stop(20)
    $)

    IF argv!0=0 DO
    $( argv!0 := "HOME:"
       currentdir := 0
    $)

    currentdir := 0
    newdir := locatedir(argv!0)
    IF newdir=0 DO
    $( writef("Can't find %S*N", argv!0)
       currentdir := olddir
       stop(20)
    $)

    currentdir := newdir
    UNLESS newdir=olddir DO freeobj(olddir)

    IF cli.interactive DO testflags(2)
    UNLESS cli.currentinput=cli.standardinput DO
    $( selectinput(cli.currentinput)
       endread()
       cli.currentinput := cli.standardinput
    $)
    initstream := findinput("INIT-SEQUENCE")
    UNLESS initstream=0 DO
       cli.currentinput := initstream
 $)
