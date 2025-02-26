// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

GET "LIBHDR"

GLOBAL $( buffer     :  ug
          ptr        :  ug + 1
          lim        :  ug + 2
       $)

MANIFEST $( buffer.size = 150 $)

LET start() BE
  $( LET args      = VEC 50
     LET sysout    = output()
     LET instream  = 0
     LET outstream = 0
     LET word      = ?
     LET wdcount   = 0
     LET buf       = VEC buffer.size - 1

     buffer       := buf
     lim, ptr     := 0, -1

     IF rdargs("FROM/A,TO/K", args, 50) = 0 THEN
       $( writes("bad arguments for TYPEHEX*n")
          stop(20)
       $)

     instream := findinput(args!0)
     IF instream = 0 THEN
       $( writef("can't open %s*n", args!0)
          stop(20)
       $)
     selectinput(instream)

     IF args!1 \= 0 THEN
       $( outstream := findoutput(args!1)
          IF outstream = 0 THEN
            $( writef("can't open %s*n", args!1)
               endread()
               stop(20)
            $)
          selectoutput(outstream)
       $)

     WHILE getword(@ word) DO
       $( writef("%X4",word)
          IF testflags(1) THEN
            $( selectoutput(sysout)
               writes("*N******BREAK*N")
               GOTO out
            $)

          wdcount:=wdcount+1
          wrch(wdcount REM 16 = 0 ->'*N','*S')
       $)

     IF wdcount REM 16 \= 0 THEN
        wrch('*N')

  out:
     endread()
     IF outstream \= 0 THEN
       $( selectoutput(outstream)
          endwrite()
       $)
  $)

AND getword(addr) = VALOF
  $( ptr := ptr + 1
     IF ptr >= lim THEN
       $( lim := ABS readwords(buffer, buffer.size)
          IF lim = 0 THEN RESULTIS FALSE
          ptr := 0
       $)
     ! addr := buffer ! ptr
     RESULTIS TRUE
  $)
