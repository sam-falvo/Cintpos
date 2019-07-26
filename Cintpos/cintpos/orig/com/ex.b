/***********************************************************************
**             (C) Copyright 1980  TRIPOS Research Group              **
**            University of Cambridge Computer Laboratory             **
************************************************************************

  ########  #      #    ####    ##    ##  ########  ##    ##  ########
  ########  ##    ##   ######   ###  ###  ########  ###   ##  ########
  ##         ##  ##   ##    ##  ########     ##     ####  ##  ##
  ######       ##     ########  ## ## ##     ##     ## ## ##  ######
  ##          #  #    ##    ##  ##    ##     ##     ##  ####  ##
  ##         ##  ##   ##    ##  ##    ##     ##     ##  ####  ##
  ########  ##    ##  ##    ##  ##    ##  ########  ##   ###  ########
  ########  #      #  ##    ##  ##    ##  ########  ##    ##  ########

************************************************************************
**    Author:   Brian Knight                          March 1980      **
***********************************************************************/



// This version of EXAMINE for both the fileserver based and the
// FH-III real-disc filing systems -
// the latter modifications done by Mike Richardson
//
// Modifications:
// 26 feb 81 by BJK: Whole command no longer fails when an exinfo
//                   packet comes back with a non-zero RC.
//                   Warning message instead.
//  8 jul 81 by MFR: Longer info vector to cope with commented FH3
//		   : Also to use comments!!
// 16 Jul 81 by BJK: Message improved, and rc passed back, when
//                   primary object not available.

SECTION "EX"

GET "sys:sys.fsfh.bcpl.fsfh"
GET "pattern"
GET "fh3manifests"
GET "manhdr"

MANIFEST
    $(
    argvupb             = 50
    size.exinfo         = info.upb			// from fh3manifests

    exinfo.name         = dirent.name
    exinfo.fskey        = dirent.fskey
    exinfo.dirent.type  = dirent.type
    exinfo.hdr.type     = exinfo.hdr + file.type
    exinfo.file.byte.size=exinfo.hdr + file.byte.size
    exinfo.creation.dat = exinfo.hdr + file.creation.dat
    $)


GLOBAL
    $(
    fhtask      : ug
    quickopt    : ug + 1
    objname     : ug + 2
    disctype    : ug + 3
    $)



LET start() BE
    $(
    LET argv            = VEC argvupb
    LET v1              = VEC argvupb // For pattern matching
    LET pattern         = VEC argvupb
    LET string.to.dat.overlay = "sys:l.string-to-dat"
    LET rc              = 0
    LET save.result2    = ?
    LET lock            = 0
    LET p, s            = ?, ?
    LET keysopt         = ?
    LET datesopt        = ?
    LET nodatesopt      = ?
    LET patternopt      = ?
    LET nentries        = 0
    LET seg             = loadseg("sys:l.dat-to-strings")
    LET access          = ?
    LET date            = VEC 14
    LET stamp           = VEC 2
    LET since.stamp     = VEC 2
    LET upto.stamp      = VEC 2
    LET day             = datstamp(stamp)!0
    LET entryinfo       = VEC size.exinfo-1
    LET creation.day    = ?
    LET total           = 0
    LET oldoutput       = output()
    LET tostream        = ?
    LET breakcondition  = FALSE
    LET more.entries    = FALSE
    LET is.file         = ?
    LET disc.infovec    = VEC discinfo.alloc
    LET rdargs.string   = "DIR,P=PAT/K,KEYS/S,DATES/S,NODATES/S,TO/K,S/K,SINCE/K,UPTO/K,QUICK/S"

    IF rdargs(rdargs.string, argv, argvupb)=0 | [argv!3\=0 & argv!4\=0]
    THEN
      $(
      writef("Args no good for key *"%S*"*N", rdargs.string)
      rc := 20
      GOTO exit
      $)

    since.stamp!0, since.stamp!1, since.stamp!2 := -1, -1, -1
    upto.stamp!0,  upto.stamp!1,  upto.stamp!2 := maxint, maxint, maxint
    patternopt  := (argv!1 \= 0) | (argv!6 \= 0)

    UNLESS argv!7=0 & argv!8=0
      IF callseg(string.to.dat.overlay, since.stamp, argv!7) = 0 |
         callseg(string.to.dat.overlay, upto.stamp,  argv!8) = 0
      THEN
      $( TEST result2 = 0
         THEN writes("****** Invalid *'UPTO*' or *'SINCE*' parameter - ignored*N")
         ELSE writef("****** Can't load %S - *'UPTO*' or *'SINCE*' parameter ignored*N", string.to.dat.overlay)
      $)

    objname      := argv!0
    keysopt      := argv!2\=0
    datesopt     := argv!3\=0 | day=0
    nodatesopt   := argv!4\=0
    tostream     := argv!5
    quickopt     := argv!9 \= 0

    IF patternopt
    THEN
      $(
      // Construct the pattern
      // The form is "#?(<s>)#?/(<p>)" so that the filenames
      // printed are those which have <s> as a substring
      // or match the pattern <p>.
      // Defaults for <s> and <p> are "!" which will
      // never match, except when neither is specified
      // in which case <s> and <p> are set to "%" so that all
      // filenames are listed.
      // Pattern cannot overflow, as its vector
      // is the same size as the rdargs vector.
      p := argv!1
      s := argv!6=0 -> (p=0 -> "%", "!"), argv!6
      IF p=0 THEN p := "!"

      pattern%0 := 0
      concat(pattern, "#?(")
      concat(pattern, s)
      concat(pattern, ")#?/(")
      concat(pattern, p)
      concat(pattern, ")")
      $)

    UNLESS tostream=0 DO
     $( LET stream = findoutput(tostream)
        TEST stream = 0
        THEN
        $( writef("****** Can't open %S*N", tostream)
           rc := 20
           GOTO exit
        $)
        ELSE selectoutput(stream)
     $)

    TEST seg=0
    THEN nodatesopt := TRUE
    ELSE globin(seg)  // Must be after Findoutput

    IF objname=0 THEN objname := currentdir=0 -> ":", "current directory"

    TEST compstring(objname, "current directory")=0
    THEN lock:= currentdir
    ELSE lock:= locateobj(objname)

    IF lock=0 DO
     $( save.result2 := result2
        writef("Can't examine *"%S*": ", objname)
        fault(save.result2)
        result2      := save.result2
        rc := 20
        GOTO exit
     $)

    fhtask      := lock ! lock.task

    IF sendpkt ( notinuse, fhtask, action.discinfo, ?, ?, disc.infovec ) = 0
       THEN
         $(
            writes ( "discinfo request failed*N" )
            rc := 20
            GOTO exit
         $)

    disctype := disc.infovec ! discinfo.type

    IF NOT examine.object(lock, entryinfo)
    THEN
      $(
      save.result2 := result2
      writef("Can't examine *"%S*": ", objname)
      fault(save.result2)
      result2      := save.result2
      rc := 20
      GOTO exit
      $)

    TEST disctype = disctype.fs
    THEN
         is.file := entryinfo ! exinfo.hdr.type = type.file
    ELSE
         is.file := entryinfo ! exinfo.hdr.type < 0

    TEST is.file
    THEN
      $(
      // Object is a file - fill in the first part of the entryinfo to suit

      FOR i=0 TO upb.name DO (entryinfo+exinfo.name)!i := objname!i
      entryinfo ! exinfo.dirent.type :=
          ( disctype = disctype.fs ) -> type.file, st.file
      $)
    ELSE
      $(
      // Object is a directory
      // See if it is empty, by trying to inspect the first entry

      more.entries      := exinfo(lock, entryinfo)

      IF NOT more.entries
      THEN
        $(
        writes("Directory *"")
        writename(lock)
        writes("*" is empty*n")
        GOTO exit
        $)


      UNLESS seg=0 DO start(stamp, date)
      IF compstring(objname, "current directory") \= 0
      THEN
        $(
        writes("Directory *"")
        writename(lock)

        TEST seg=0
        THEN writes("*"*N")
        ELSE writef("*" on %S %S*N", date+10, date)
        $)
      $)

    IF patternopt
    THEN UNLESS cmplpat(pattern, v1) DO
          $( writes("Bad pattern for EX*N")
             rc := 20
             GOTO exit
          $)

     $( LET type, count = ?, ?
        IF testflags(1) DO $( breakcondition := TRUE; BREAK $)

        IF patternopt
        THEN UNLESS match(pattern, v1, entryinfo+exinfo.name) GOTO next

        type := entryinfo ! exinfo.dirent.type

        UNLESS quickopt
        THEN
          $(
          UNLESS seg=0 DO start(entryinfo+exinfo.creation.dat, date)

          UNLESS compstamp(entryinfo+exinfo.creation.dat, since.stamp) >= 0 GOTO next
          UNLESS compstamp(entryinfo+exinfo.creation.dat, upto.stamp)  <= 0 GOTO next
          $)


          $(
          LET len       = byteget(entryinfo+exinfo.name, 0)

          writes("  ")
          FOR i=1 TO len DO wrch(byteget(entryinfo+exinfo.name, i))
          FOR i=len+1 TO 18 DO wrch(' ')
          $)

        IF keysopt THEN
           TEST disctype = disctype.fs
           THEN
                writef ( " [%I5 %I5]", lock ! lock.last.examined,
                                       entryinfo ! dirent.link )
           ELSE
                writef ( " [%I5]", entryinfo ! dirent.link )

        IF quickopt THEN $( newline(); GOTO next $)

        TEST ( disctype = disctype.fs ) ->
                        ( type = type.dir ), ( type > 0 )
        THEN writes(" dir            ")
        ELSE
          $(
          // File: write out size, in full if small, else as nK.
          // or in blocks if a real disc

          writes("     ")
          TEST disctype = disctype.fs
          THEN
            $(
               write.32(entryinfo + exinfo.file.byte.size)
               writes(" bytes")
            $)
          ELSE
            $(
               LET n = entryinfo ! ( exinfo.file.byte.size + 1 )

               writef ( "%I4 block%s", n, n = 1 -> " ", "s" )
            $)
          $)



        IF disctype = disctype.fs THEN
        $(
        // Write out the protection flags

        access  := entryinfo ! dirent.access.bits

        writes("  ")
        TEST (access&access.read) \= 0   THEN wrch('R') ELSE wrch(' ')
        TEST (access&access.write) \= 0  THEN wrch('W') ELSE wrch(' ')
        TEST (access&access.delete) \= 0 THEN wrch('D') ELSE wrch (' ')
        $)

        // The object may be open for writing in the fileserver
        // in which case the header info will not be accessible.
        // The file handler will return it as all zeros, so the
        // header type field will be zero.

        TEST entryinfo ! exinfo.hdr.type = 0
        THEN writes("  **********  open  ***********n")
        ELSE
          $(
          creation.day    := entryinfo ! exinfo.creation.dat

          TEST creation.day=0 | nodatesopt
          THEN newline()
          ELSE
            $( LET dayname = day=creation.day   -> "Today",
                             day=creation.day+1 -> "Yesterday",
                             day=creation.day-1 -> "Tomorrow",
                             day<=creation.day+7 -> date+10,
                                                    date
               IF datesopt DO dayname := date
               writef("  %T9 %S*N", dayname, date+5)
            $)
          $)
        IF [ disctype \= disctype.fs ] & [ ( entryinfo + info.comment ) % 0 \= 0 ]
           THEN  writef ( ": %S*N", entryinfo + info.comment )

    next:
        nentries := nentries+1
        total := total+ [type=3 -> count+1, 1]

        // Examine next entry

        IF is.file THEN BREAK // No entries !

        more.entries := exinfo(lock, entryinfo)

     $) REPEATWHILE more.entries

exit:
    save.result2  := result2
    unloadseg(seg)
    UNLESS oldoutput=output() DO
     $( endwrite()
        selectoutput(oldoutput)
     $)
    IF breakcondition DO writes("****BREAK*N")
//    Writef("%N entr%S processed*N", NEntries,
//        NEntries=1 -> "y", "ies")

    UNLESS lock=-1 | lock=currentdir THEN freeobj(lock)
    result2       := save.result2
    stop(rc)
 $)


AND compstamp(s1, s2) = VALOF
    $( // negative if s1 < s2
    //    zero     if s1 = s2
    //    positive if s1 > s2

    FOR i = 0 TO 2 DO UNLESS s1!i=s2!i DO RESULTIS s1!i-s2!i
    RESULTIS 0
    $)


AND examine.object(lock, entryinfo) =
    sendpkt(notinuse, fhtask, action.examineobject, ?, ?, lock, entryinfo)




AND exinfo(lock, entryinfo) = VALOF
    $(
    LET res = sendpkt(notinuse, fhtask, action.examinenext, ?, ?, lock, entryinfo, NOT quickopt)
    IF NOT res
    THEN IF result2 \= error.NoMoreEntries
         THEN
           $(
//           writes("EX failed because ")
           writes("****** NEXT ENTRY INVALID: ")
           fault(result2)

           // Let command proceed after warning
           res := TRUE
           $)
    RESULTIS res
    $)


AND writename(lock) BE
 $(
    writes(objname)
 $)


AND concat(str1, str2) BE
 $(
// Adds the second string to the first
    LET len = str1%0
    FOR j=1 TO str2%0 DO len, str1%len := len+1, str2%j
    str1%0 := len
 $)


AND byteget(v,b) = [v!(b/2) >> (1-(b&1))*8] & #XFF



AND write.32(numvec) BE
    $(
    // Write out the 32 bit number in NUMVEC!0 and NUMVEC!1, in full
    // is the number is less than 10000, otherwise as nK.
    // The minimum field width is 5 characters.

    LET ms.size   = numvec!0
    LET ls.size   = numvec!1

    TEST (ms.size=0) & (0 <= ls.size < 10000)
    THEN writef("%u4 ", ls.size) // Small number
    ELSE writef("%u4K", (ms.size << 6) | (ls.size >> 10))
    $)
