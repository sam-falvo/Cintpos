// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

// This version is for use with file handlers which regard files
// as vectors of bytes (e.g. the fileserver file handler).
// The default copying mode is CHARS.

SECTION "COPY"

GET "LIBHDR"
GET "CLIHDR"
GET "IOHDR"

LET START() BE
 $( LET argv = VEC 50
    LET outstream, instream  = 0, 0
    LET oldoutput = output()
    LET words = VEC 99
    LET res = ?
    LET broken = FALSE
    LET rc = 20

    IF rdargs("FROM/A,TO/A,WORDS/S,CHARS/S",argv,50)=0 |
       argv!2 & argv!3 DO
    $( writes("Bad args*N")
       GOTO exit
    $)

    instream := findinput(argv!0)
    IF instream=0 DO
    $( writef("Can't open %S*N",argv!0)
       GOTO exit
    $)
    selectinput(instream)

    outstream := findoutput(argv!1)
    IF outstream=0 DO
    $( writef("Can't open %S*N", argv!1)
       GOTO exit
    $)
    selectoutput(outstream)

    // CHARS mode is the default (i.e. ARGV!2 = 0)

//  UNLESS argv!2 | argv!3 DO
//     argv!2 := instream!scb.type=task.filehandler &
//               outstream!scb.type=task.filehandler

    rc := 0

    TEST argv!2 THEN
    $( res := readwords(words, 100)
       IF testflags(1) DO
       $( broken := TRUE
          BREAK
       $)
       writewords(words, ABS res)
    $) REPEATUNTIL res<=0
    ELSE
    $( LET ch = rdch()
       IF ch=endstreamch BREAK
       IF testflags(1) DO
       $( broken := TRUE
          BREAK
       $)
       wrch(ch)
    $) REPEAT

    IF broken DO
    $( selectoutput(oldoutput)
       writes("****BREAK*N")
       selectoutput(outstream)
       rc := 10
    $)

exit:
    UNLESS outstream=0 DO endwrite()
    UNLESS instream=0 DO endread()
    stop(rc)
 $)
