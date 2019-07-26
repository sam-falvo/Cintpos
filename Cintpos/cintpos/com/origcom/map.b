// (C) Copyright 1992 Martin Richards

// Command to map the store allocation under the Cintpos system.
// It counts the store used for its own code, stack
// and workspace as free.
//
// It is based on the map command from the Tripos Operating System
//
// MAP [BLOCKS] [CODE] [NAMES] [MAPSTORE] [TO file]
//     [PIC]
//
//     BLOCKS gives size and address of every block
//     CODE   gives layout of loaded code sections
//     NAMES  gives space used for routine names,
//            section names, and global initialisation info.
//     TO     specifies an output file
//     PIC    gives the allocation map in picture form

SECTION "MAP"
GET "g/libhdr.h"
GET "g/clihdr.h"  // Just for cli_module

MANIFEST $(
         sectword  = #xFDDF   // SECTION and Entry marker words.
         entryword = #xDFDF
         maxpri    = 32767

         sectnamesize 	= (7 + bytesperword)/bytesperword
         routnamesize 	= (7 + bytesperword)/bytesperword
         nameoffset 	= -routnamesize
         vecupb 	= 499
         $)

LET start() BE
$(  LET blocklist 	= rootnode ! Rtn_blklist
    LET a 		= blocklist
    LET topofstore 	= rootnode ! Rtn_memsize // * 1024
    LET free, used, n 	= 0, 0, 0
    LET blocks 		= sagetvec(vecupb)
    LET argv 		= VEC 50
    LET largest_free 	= 0
    LET joinfree 	= 0
    LET blocksopt 	= FALSE
    LET namesopt 	= FALSE
    LET codeopt 	= FALSE
    LET mapstore 	= FALSE
    LET picopt 		= FALSE
    LET mapstack 	= currco - 1  // stackbase - 1
    LET mapcode  	= cli_module-1  // Section vector
    LET sectnames, routnames, globinit = 0, 0, 0
    LET constr 		= output()
    LET outstr 		= 0
    LET oldpri 		= ((rootnode ! Rtn_tasktab) ! taskid) ! Tcb_pri
    LET rdargs_string	= "BLOCKS/S,NAMES/S,CODE/S,MAPSTORE/S,TO/K,PIC/S"

    IF rdargs(rdargs_string, argv, 50) = 0 DO
    $( writef("MAP: Bad args for key string *"%S*"*N", rdargs_string)
       freevec(blocks)
       stop(20)
    $)

    IF blocks = 0 DO
    $( writes("MAP: Not enough store for workspace*N")
       stop(20)
    $)

    blocksopt := argv!0
    namesopt := argv!1
    codeopt := argv!2
    IF argv!3 DO mapstore, codeopt, namesopt := TRUE, TRUE, TRUE
    IF argv!4 DO
    $( outstr := findoutput(argv!4)
       IF outstr = 0 DO
       $( writef("Can't open %S*N",argv!4)
          freevec(blocks)
          stop(20)
       $)
       selectoutput(outstr)
    $)

    picopt := argv!5

    // Take highest task priority for critical section
    // (Should always be available if we are running)
    changepri(taskid, maxpri)

    UNTIL !a = 0 DO
    $( LET size = !a
       LET next = ?
       IF a = mapstack | a = mapcode | a = blocks-1 DO
          size := size + 1 // Make it look free

       blocks!n := size; n := n + 1
       TEST (size & 1) = 0
       THEN $( // Used block
               used := used + size
               IF joinfree > largest_free
               THEN largest_free := joinfree
               joinfree := 0
            $)
       ELSE $( size := size-1
               free := free + size
               joinfree := joinfree + size
            $)

       next := a + size
       UNLESS size>=0 & next<=topofstore DO
       $( changepri(taskid, oldpri)
          writef("******Store chain corrupt!!*N*
                 *Noticed at %n*n", a)
          BREAK
       $)
       a := next
       IF n > vecupb DO
       $( writef("*N****** Too many blocks for MAP's workspace*N*
                 ******* Only the first %N mapped*N", vecupb+1)
          BREAK
       $)
    $)

    IF joinfree > largest_free DO largest_free := joinfree

    changepri(taskid, oldpri)

    // Now print what we've found
    newline()

    IF blocksopt DO
    $( writes("Map of free and allocated store*N*N")

       a := blocklist

       FOR i = 0 TO n-1 DO
       $(   LET s = blocks!i
            LET free = (s&1)=1

            IF testflags(1) GOTO exit

            writef("%i7: ", a)
            TEST free
            THEN $( writes("free ") ; s := s - 1 $)
            ELSE    writes("alloc")

            writef("%i6 words", s)
            IF a = mapstack     DO writes(" MAP's stack")
            IF a = mapcode      DO writes(" MAP's code")
            IF a = (blocks-1)   DO writes(" MAP's workspace")
            IF a = (rootnode-1) DO writes(" Rootnode")
            IF a!3 = sectword  & (a+4)%0 = 7  DO
                                writef(" Section %s", a+4)
            newline()
            a := a + s
       $)

       writef("Top of block list = %n*n", a)
       topofstore := a
    $)

    writef("Largest contiguous free area: ")
    writeu(largest_free, 0); writes(" words*N")
    writes("Totals: "); writeu(used+free, 0)
    writes(" words available, "); writeu(used, 0)
    writes(" used, "); writeu(free, 0)
    writes(" free*N*N")

    IF picopt DO
    $( // Print as a picture
       LET alloc = TRUE
       LET next_block = blocklist
       LET num=0
       LET col = 0
       LET grainsize = topofstore/(20*64) + 1
       LET addr = 0

       WHILE addr < topofstore DO
       $( LET some_alloc = alloc
          LET some_free  = NOT alloc

          UNLESS addr <= next_block | num > n DO
          $( // Move to next block
             alloc := ((blocks!num) & 1) = 0
             TEST alloc THEN some_alloc := TRUE
                        ELSE $( some_free := TRUE
                                next_block := next_block - 1
                             $)
             next_block := next_block + blocks!num
             num := num+1
          $)

          IF col REM 64 = 0 DO writef("*n%i6  ", addr)
          wrch (num > n -> '?',  // No info
                some_alloc   -> (some_free -> 'a', '@'), '.')
          col, addr := col+1, addr+grainsize
       $)
       newline()
    $)

   // Print the layout of the code sections, assuming
   // that they will not change under our feet!
   // Also, add up space used for section names, routine
   // names, and global initialisation information.

   a := blocklist

   IF codeopt | namesopt DO
   $(   IF codeopt THEN writes("Layout of loaded code*N*N")

        FOR i = 0 TO n-1 DO
        $( LET s = blocks!i
           IF testflags(1) THEN BREAK
           IF (s&1)=1
           THEN $( a := a+s-1; LOOP $) // Free block

           IF a!3=sectword & (a+4)%0=7 DO
           $( // We've found a BCPL section
              LET goffset = ?

              IF codeopt DO
                writef("*n%i7: Section %S - length %i4 words*n",
                          a+2,        a+4,         a!2)


              sectnames := sectnames + sectnamesize + 1

              // Count space for global initialisation
              // table
              goffset := a+2 + a!2 // Word after highest
                                  // referenced global
              $( globinit := globinit+2
                 goffset  := goffset-2
              $) REPEATUNTIL !goffset=0 // End of list

              IF namesopt FOR j=5 /* skip some junk! */ TO a!2 DO
              $( LET addr = a+j // Avoid comparing signed addresses

                 IF addr!(nameoffset-1) = entryword &
                    good_routine_name(addr + nameoffset) DO
                 $( // BCPL entry sequence
                    routnames := routnames + routnamesize + 1
                    IF mapstore DO
                      writef("%i7: %S*N", addr, addr+nameoffset)
                 $)
              $)
           $)
           a := a+s // Point to next block
        $)

        IF namesopt DO writef("*NRoutine names: %I5 words*N*
                              *Section names: %I5 words*N*
                              *Global initialisation info: %N words*N",
                               routnames, sectnames, globinit)
   $)

exit:
   freevec(blocks)
   UNLESS outstr = 0 THEN endwrite()
   selectoutput(constr)
$)


AND good_routine_name(string) = VALOF
$( UNLESS string%0 = 7 RESULTIS FALSE

   FOR i = 1 TO 7 DO
   $( LET ch = string%i

      UNLESS 'A'<=ch<='Z' | 'a'<=ch<='z' | '0'<=ch<='9' |
             ch='.' | ch='_' | ch=' ' RESULTIS FALSE
   $)

   RESULTIS TRUE
$)

/**************************************************************************
|| (C) Copyright 1979 Tripos Research Group
||     University of Cambridge
||     Computer Laboratory

|| Command to map the store allocation under TRIPOS
|| It counts the store used for its own code, stack
|| and workspace as free.
||
|| MAP [BLOCKS] [CODE] [NAMES] [MAPSTORE] [TO file]
||     [PIC]
||
||     BLOCKS gives size and address of every block
||     CODE   gives layout of loaded code sections
||     NAMES  gives space used for routine names,
||            section names, and global initialisation info.
||     TO     specifies an output file
||     PIC    gives the allocation map in picture form

SECTION "MAP"
GET "LIBHDR"
GET "CLIHDR"  // Just for cli.module

MANIFEST $(
         secword 	= 12345
         libword 	= 23456
         maxpri 	= 32767
         sectnamesize 	= (17 + bytesperword)/bytesperword
         routnamesize 	= (7 + bytesperword)/bytesperword
         nameoffset 	= -routnamesize

         vecupb 	= 499
         $)

LET start() BE
    $(
    LET blocklist 	= rootnode ! rtn.blklist
    LET a 		= blocklist
    LET topofstore 	= (rootnode ! rtn.memsize) * 1024
    LET free, used, n 	= 0, 0, 0
    LET blocks 		= getvec(vecupb)
    LET v 		= VEC 50
    LET largest.free 	= 0
    LET joinfree 	= 0
    LET blocksopt 	= FALSE
    LET namesopt 	= FALSE
    LET codeopt 	= FALSE
    LET mapstore 	= FALSE
    LET picopt 		= FALSE
    LET mapstack 	= stackbase - 1
    LET mapcode  	= cli.module-1  || Section vector
    LET sectnames, routnames, globinit = 0, 0, 0
    LET constr 		= output()
    LET outstr 		= 0
    LET oldpri 		= ((rootnode ! rtn.tasktab) ! taskid) ! tcb.pri
    LET rdargs.string	= "BLOCKS/S,NAMES/S,CODE/S,MAPSTORE/S,TO/K,PIC/S"

    IF rdargs(rdargs.string, v, 50) = 0
    THEN
      $(
      writef("MAP: Bad args for key string *"%S*"*N", rdargs.string)
      freevec(blocks)
      stop(20)
      $)

    IF blocks = 0
    THEN $( writes("MAP: Not enough store for workspace*N"); stop(20) $)

    IF v!0 \= 0 THEN blocksopt := TRUE
    IF v!1 \= 0 THEN namesopt := TRUE
    IF v!2 \= 0 THEN codeopt := TRUE
    IF v!3 \= 0 THEN mapstore, codeopt, namesopt := TRUE, TRUE, TRUE
    IF v!4 \= 0
    THEN
        $(
        outstr := findoutput(v!4)
        IF outstr = 0
        THEN $( writef("Can't open %S*N",v!4); freevec(blocks); stop(20) $)
        selectoutput(outstr)
        $)

    IF v!5 \= 0 THEN picopt := TRUE

    // Take highest task priority for critical section
    // (Should always be available if we are running)
    changepri(taskid, maxpri)

    UNTIL !a = 0
    DO
      $(
      LET size = !a
      LET next = ?
      IF (a = mapstack) | (a = mapcode) | (a = blocks-1)
      THEN size := size + 1 || Make it look free

      blocks!n := size; n := n + 1
      TEST (size & 1) = 0
      THEN
          $( || Used block
          used := used + size
          IF (joinfree >> 1) > (largest.free >> 1)
          THEN largest.free := joinfree
          joinfree := 0
          $)
      ELSE
          $(
          size := size-1
          free := free + size
          joinfree := joinfree + size
          $)

      next := a + size
      IF ((a >> 1) >= (next >> 1)) | // Wrap around
         ((next >> 1) > ((topofstore-1) >> 1))
      THEN
        $(
        changepri(taskid, oldpri)
        writes("******Store chain corrupt!!*N*
               *Noticed at ")
        writeu(a,0); newline()
        BREAK
        $)
      a := next
      IF n > vecupb
      THEN
          $(
          writef("*N****** Too many blocks for MAP's workspace*N*
                 ******* Only the first %N mapped*N", vecupb+1)
          BREAK
          $)
      $)

    IF (joinfree >> 1) > (largest.free >> 1)
    THEN largest.free := joinfree

    changepri(taskid, oldpri)

    // Now print what we've found
    newline()

    IF blocksopt
    THEN
        $(
        writes("Map of free and allocated store*N*N")


        a := blocklist

        FOR i = 0 TO n - 1
        DO
            $(
            LET s = blocks!i
            LET free = (s&1)=1

            IF testflags(1)
            THEN GOTO exit

            writeu(a, 5); writes(": ")
            TEST free
            THEN $( writes("free ") ; s := s - 1 $)
            ELSE    writes("alloc")

            writeu(s, 6); writes(" words")
            IF a = mapstack writes(" (MAP's stack)")
            IF a = mapcode  writes(" (MAP's code)")
            IF a = (blocks-1) writes(" (MAP's workspace)")
            newline()
            a := a + s
            $)

        writes("Top of block list = ")
        writeu(a, 0); newline()
        $)

    writef("Largest contiguous free area: ")
    writeu(largest.free, 0); writes(" words*N")
    writes("Totals: "); writeu(used+free, 0)
    writes(" words available, "); writeu(used, 0)
    writes(" used, "); writeu(free, 0)
    writes(" free*N*N")

    IF picopt
    THEN
      $( // Print as a picture
      LET alloc = TRUE
      LET next.block = blocklist
      LET num=0
      LET col = 0

      writes("    0  ")
      FOR sample = 31 TO (topofstore-1)>>1 BY 32
      DO $(
         LET some.alloc = alloc
         LET some.free  = NOT alloc

         UNLESS (sample <= (next.block >> 1)) | (num > n)
         DO
           $( // Move to next block
           alloc := ((blocks!num) & 1) = 0
           TEST alloc
           THEN some.alloc := TRUE
           ELSE
             $( some.free := TRUE
                next.block := next.block - 1
             $)
           next.block := next.block + blocks!num
           num := num+1
           $)

         col := col+1
         IF col=65
         THEN $( writef("*N%U5  ", (sample-31)*2); col := 1 $)
         wrch (num > n -> '?',  // No info
               some.alloc   -> (some.free -> 'a', '@'), '.')
         $)
      newline()
      $)

    || Print the layout of the code sections, assuming
    || that they will not change under our feet!
    || Also, add up space used for section names, routine
    || names, and global initialisation information.

    a := blocklist

    IF codeopt | namesopt
    THEN
        $(
        IF codeopt THEN writes("Layout of loaded code*N*N")

        FOR i = 0 TO n-1
        DO $(
           LET s = blocks!i
           IF testflags(1) THEN BREAK
           IF (s&1)=1
           THEN $( a := a+s-1; LOOP $) // Free block

           IF (a!3 = secword) & ((a+4)%0 = 17)
           THEN
             $( // We've found a BCPL section
             LET goffset = ?

             IF codeopt
             THEN
               $(
               writeu(a+2, 5)
               writef(": %S - length ", a+4)
               writeu(a!2, 4)
               writes(" words*N")
               $)

             sectnames := sectnames + sectnamesize + 1

             // Count space for global initialisation
             // table
             goffset := a+2 + a!2 // Word after highest
                                  // referenced global
               $(
               globinit := globinit+2
               goffset  := goffset-2
               $) REPEATUNTIL !goffset=0 // End of list

             IF namesopt
             THEN
               FOR j=5 TO a!2 // skipping some junk
               DO $(
                  LET addr = a+j // Avoid comparing signed addresses

                  TEST good.routine.name(addr + nameoffset)
                  THEN
                    $( // BCPL entry sequence
                    LET libw.entry = addr!(nameoffset-1) = libword

                    routnames := routnames + routnamesize + (libw.entry->1,0)
                    IF mapstore
                    THEN
                      $(
                      writeu(addr, 6)
                      writes(libw.entry -> ": **", ":  ")
                      writef("%S*N", addr+nameoffset)
                      $)
                    $)
                  ELSE IF !addr = countword
                       THEN
                         $( // Profile count
                         writef("%u6:  ", addr)
                         writedp(addr!2, addr!1); newline()
                         $)
                  $)
             $)
           a := a+s // Point to next block
           $)

        IF namesopt
        THEN writef("*NRoutine names: %I5 words*N*
                     *Section names: %I5 words*N*
                     *Global initialisation info: %N words*N",
                     routnames, sectnames, globinit)
        $)

exit:
    freevec(blocks)
    UNLESS outstr = 0 THEN endwrite()
    selectoutput(outstr)
    $)



AND writeu(n, d) BE
    $(
    // Writes n as an unsigned number in a field of d places
    LET a = (n >> 1)/5

    TEST a = 0
    THEN FOR j=1 TO d-1 wrch(' ') // To prevent "00"
    ELSE writed(a, d-1)

    wrch( (((n >> 1) - a*5) << 1) + (n & 1) + '0')
    $)



AND writedp(msw, lsw) BE
    $(
    // Write a double precision integer in a field of 9 places
    // Assume bitsperword=16
    // Assume top 3 bits of msw are zero.

    msw := (msw << 3) + (lsw >> 13)
    lsw := lsw & #B0001111111111111
    msw := muldiv(msw, 8192, 10000)
    lsw := lsw + result2
    IF lsw >= 10000 THEN msw, lsw := msw+1, lsw-10000

    // Write msw half
    TEST msw=0 THEN writes("     ") ELSE writed(msw, 5)

    // Write lsw half
    $( LET v = VEC 3
       LET zero.supp = msw=0
       FOR pos = 3 TO 0 BY -1
       DO $( v!pos := lsw REM 10 + '0'
             lsw := lsw/10
          $)

       FOR pos = 0 TO 3
       DO $( LET d = v!pos
             TEST zero.supp & d='0' & pos<3
             THEN wrch(' ')
             ELSE $( wrch(d); zero.supp := FALSE $)
          $)
    $)
    $)


AND good.routine.name(string) = VALOF
    $(
    IF string%0 \= 7 THEN RESULTIS 0

    FOR i=1 TO 7
    DO $(
       LET ch = capitalch(string%i)

       UNLESS ('A' <= ch <= 'Z') | ('0' <= ch <= '9') | (ch = '.') | (ch = ' ')
       THEN RESULTIS FALSE
       $)

    RESULTIS TRUE
    $)
*************************************************************************/