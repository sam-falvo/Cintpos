// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

// Program which tries to identify a vector
// given its address

SECTION "IDvec"
GET "LIBHDR"


MANIFEST
    $(
    secword = 12345
    $)

STATIC
    $( // So as not to zap globals when CALLSEGed
    addr = 0
    ptr.found = FALSE
    tasktab   = 0
    devtab    = 0
    $)



LET start(addr.or.zero) BE
    $(
    LET argv = VEC 30
    tasktab   := rtn.tasktab ! rootnode
    devtab    := rtn.devtab  ! rootnode
    ptr.found := FALSE // So repeated execution works

    TEST addr.or.zero = 0
    THEN
      $( // Running as command
      IF rdargs("address/a", argv, 30) = 0
      THEN
        $(
        writes("Bad arguments to IDVEC*N")
        stop(20)
        $)

      addr := stringval(argv!0)
      $)
    ELSE addr := addr.or.zero // Called by CALLSEG

    addr := addr | 1 // All GETVEC vectors have odd
                     // addresses
    IF (addr!(-1) & 1) = 1
    THEN
      $( writes("Vector is not allocated!*N")
         RETURN // in case in CALLSEG
      $)

    addr := addr+1 // So it will not be found as
                   // IDVEC's global 150 !!

    IF (addr-1) = tasktab
    THEN $( mywritef("Task table*N"); RETURN $)
    IF (addr-1) = devtab
    THEN $( mywritef("Device table*N"); RETURN $)

    // Search tasks
    FOR t=1 TO tasktab!0
    DO searchtask(t)

    // Search devices
    FOR d=1 TO devtab!0
    DO searchdev(d)

    UNLESS ptr.found
    THEN
      $(
      writes("Cannot identify this vector")

      TEST addr.or.zero = 0
      THEN
        $(
        writes(" - first 10 words are:*N")
        FOR z=-1 TO 8 // addr is address + 1
        DO writef("%I6  %X4  *'%C%C*'*N", addr!z,
                  addr!z, (addr!z)>>8, addr!z)
        $)
      ELSE newline()
      $)
    $)


AND searchtask(t) BE
    $(
    LET tcb, sbase, gbase, segl = tasktab!t, ?, ?, ?
    IF tcb=0 THEN RETURN // No such task
    IF (addr-1)=tcb
    THEN $( mywritef("TCB of task %N*N", t); RETURN $)

    // Look down segment list
    segl := tcb.seglist ! tcb
    IF (addr-1) = segl
    THEN mywritef("Segment list of task %N*N", t)

    FOR x=1 TO segl!0
    DO $(
       LET sec= segl!x
       UNTIL sec=0
       DO $(
          IF (addr-1) = sec
          THEN mywritef("Code section of task %N: %S*N", t,
                     (addr-1)!2 = secword -> addr-1+3,
                                         "<no name>")
          sec := !sec
          $)
       $)

    // Don't carry on if task is dead
    IF (tcb.state ! tcb & state.dead) = state.dead
    THEN RETURN

    // Inspect stack
    sbase := tcb.sbase ! tcb
    IF (addr-1) = sbase
    THEN $( mywritef("Stack of task %N*N", t); RETURN $)

    // Is it a coroutine stack?
    $( LET cstack = sbase!0
    UNTIL cstack=0
    DO $(
       IF cstack=(addr-1)
       THEN $( mywritef("Coroutine stack of task %N*N",t)
               RETURN
            $)
       cstack := cstack!0
       $)

    // Inspect global vector
    gbase := tcb.gbase ! tcb

    IF (addr-1)=gbase
    THEN $( mywritef("Global vector of task %N*N", t)
            RETURN
         $)

    FOR gn=1 TO gbase!0
    DO IF (addr-1) = gbase!gn
       THEN mywritef("Pointed to by global %N of task %N*N",gn ,t)

    $)

    FOR s=0 TO (tcb.stsiz ! tcb) - 1
    DO IF sbase!s = (addr-1)
       THEN mywritef("Pointed to by stack location %N of *
                   *task %N*N", sbase+s, t)
    $)


AND searchdev(d) BE
    $(
    LET dcb = devtab!d
    IF dcb=0 THEN RETURN // No such device

    IF (addr-1)=dcb
    THEN $( mywritef("DCB of device -%N*N", d); RETURN $)

    IF (addr-1) = !dcb
    THEN $( mywritef("Driver of device -%N*N", d); RETURN $)
    $)


AND stringval(s) = VALOF
    $(  || converts a string to a number
    LET val = 0
    LET neg = ?
    LET char1 = ?


    TEST s%1 = '-'
    THEN neg, char1 := TRUE, 2
    ELSE neg, char1 := FALSE, 1

    FOR j = char1 TO s%0
    DO
        $(
        UNLESS '0' <= s%j <= '9'
        THEN $( writef("Invalid char *'%C*' in number*N", s%j)
                stop(20)
             $)
        val := val*10 + s%j - '0'
        $)

    RESULTIS val
    $)

AND mywritef(f,a,b,c,d) BE
    $(
    ptr.found := TRUE
    writef(f,a,b,c,d)
    $)
