SECTION "LIBRARY"

GET "libhdr"

MANIFEST $( yes      =  TRUE
            no       = FALSE
            secword  = #xFDDF
         $)


// TRIPOS library command.  It loads an object module
//  and adds the segment to the end of the BLIB
//  library chain. (!!)
// It will also cancel a loaded section.

LET start() BE
// LIBRARY [[FROM] file] [OVERRIDE] [CANCEL secname]]

  $( LET cancelname = 0
     LET cancelvec  = VEC 3
     LET blibe      = (tcb.seglist ! tcb) + 2
     LET blib       = !blibe
     LET segment    = 0

     LET argv       = VEC 50

     IF rdargs("from,override/s,cancel/k", argv, 50) = 0 THEN
       $( writes("Invalid parameters*N")
          stop(20)
       $)

     // If necessary, remember CANCEL value.

     IF argv!2 DO
       $( cancelname := cancelvec
          copy.section.name(argv!2, cancelname)
       $)

     // Deal with FROM parameter

     IF argv!0 DO
       $( LET secname = VEC 3

          segment    := argv!0

          IF segment = 0 THEN
            $( writef("Load of *"%S*" failed with code %N*N",
                 argv!0, result2)
               stop(20)
            $)

          // See if segment is already loaded

          IF extract.section.name(segment+1, secname) & argv!1 = 0 &
             (cancelname = 0 | compstring(secname, cancelname) \= 0) &
             find.section(blib, secname) \= 0 THEN
            $( writef("Library %S already loaded*N", secname)
               unloadseg(segment)
               stop(20)
            $)

          // Now that the library has been loaded, initialise
          //  its globals.  (In case the original library is
          //  about to be cancelled)!.

          IF globin(segment) = 0 THEN
            writes("Warning: global initialisation ended*
                   * in error*N")

          // Now add to the end of the chain

          !findptr(blibe, 0) := segment
       $)


     // Deal with CANCEL parameter

     IF cancelname DO
       $( LET sec  = find.section(blib, cancelname)

          TEST sec = 0 THEN
            $( writef("Failed to find section *"%S*"*N",cancelname)
               returncode := 10
            $)
           ELSE
            // Delete the section
            $( LET secp  = findptr(blibe, sec)
               !secp := !sec
               freevec(sec)
            $)

       $)

  $)



AND findptr(lv.chain, hunk) = VALOF
  $( UNTIL !lv.chain = hunk | !lv.chain = 0 DO
       lv.chain := !lv.chain
     RESULTIS lv.chain
  $)



AND copy.section.name(name, v) BE
  $( FOR j = 1 TO 7 DO
       v%j := (j > name%0 -> ' ', name%j)
     v%0 := 7
  $)



AND extract.section.name(hunk, v) = VALOF
  // Returns true if there is a valid section name.
  $( LET size = hunk!0
     IF size >= 11 & hunk!1 = secword & (hunk+2)%0 = 7 /*was 17 */ THEN
       $( copy.section.name(hunk+2, v)
sawritef("hunk %n has section name %s*n", hunk, hunk+2)
          RESULTIS yes
       $)
sawritef("hunk %n size %n has no section name %s*n", hunk, size, hunk+2)
     RESULTIS no
  $)



AND find.section(list, name) = VALOF
  $( UNTIL list = 0 DO
       $( LET v = VEC 3
sawritef("name = %s*n", @list!3)
          IF extract.section.name(list+1, v) &
             compstring(v, "**************") \= 0 &
             compstring(v, name)              = 0 THEN
            BREAK
          list := !list
       $)
     RESULTIS list
  $)
