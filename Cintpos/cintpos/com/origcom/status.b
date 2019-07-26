/***********************************************************************
**             (C) Copyright 1979  TRIPOS Research Group              **
**            University of Cambridge Computer Laboratory             **
************************************************************************

        ######   ########    ####    ########  ##    ##   ######
       ########  ########   ######   ########  ##    ##  ########
       ##           ##     ##    ##     ##     ##    ##  ##
       #######      ##     ########     ##     ##    ##  #######
             ##     ##     ##    ##     ##     ##    ##        ##
             ##     ##     ##    ##     ##     ##    ##        ##
       ########     ##     ##    ##     ##     ########  ########
        ######      ##     ##    ##     ##      ######    ######

************************************************************************
**    Author:   Brian Knight                          March 1978      **
***********************************************************************/



SECTION "STATUS"

GET "g/libhdr.h"
GET "g/clihdr.h"

MANIFEST $( secword = #xFDDF $)

LET start() BE
    $(
    LET tasktab = rootnode ! Rtn_tasktab
    LET cliseg = (tcb ! Tcb_seglist)!4
    LET argv = VEC 40
    LET tcbinfo, seginfo = ?, ?
    LET cliinfo = ?
    LET lower, upper = 1, tasktab!0
    IF rdargs("TASK,FULL/S,TCB/S,SEGS/S,CLI=ALL/S", argv, 40) = 0
    THEN $( writes("Args no good*N"); RETURN $)

    IF ( argv ! 1 = 0 ) &
       ( argv ! 2 = 0 ) &
       ( argv ! 3 = 0 ) &
       ( argv ! 4 = 0 ) &
       ( argv ! 0 = 0 ) THEN argv ! 4 := 1

    tcbinfo := (argv!1 ~= 0) | (argv!2 ~= 0)
    seginfo := (argv!1 ~= 0) | (argv!3 ~= 0)
    cliinfo := seginfo | (argv!4 ~= 0)

    IF argv!0 ~= 0
    THEN
      $( // Only give status of specified task
      LET n = stringval(argv!0)

      IF (n < lower) | (n > upper) | (tasktab!n = 0)
      THEN $( writef("Task %N does not exist*N", n); RETURN $)

      lower, upper := n, n
      $)

    FOR j = lower TO upper
    DO
      $(
      LET taskcb = tasktab!j
      LET state = taskcb ! Tcb_state
      LET flags = taskcb ! Tcb_flags
      LET dead = (state & State_dead) = State_dead

      IF testflags(1) BREAK

      UNLESS taskcb = 0
      THEN
        $(
        writef("Task %i2:", taskcb ! Tcb_taskid)

        IF tcbinfo
        THEN
            $(
            writef(" pri %N,", taskcb ! Tcb_pri)
            UNLESS dead
            THEN writef(" stk %N, gv %N,",
                        taskcb ! Tcb_stsiz,
                        (taskcb ! Tcb_gbase) ! 0)
            $)

        TEST dead
        THEN writes(" dead")
        ELSE
            $(
            IF (state & NOT State_pkt) = 0
            THEN TEST j=taskid
                 THEN writes(" running") // Current task
                 ELSE writes(" suspended (in qpkt)")
            IF (state & State_wait)   ~= 0 writes(" waiting")
            IF (state & State_int)    ~= 0 writes(" interrupted")
            $)

        IF (state & State_hold) ~= 0 writes(" held")
        IF (flags & Flag_break) ~= 0 writes(" broken")
        IF (state & State_pkt)  ~= 0 writes(" with packet(s)")

        UNLESS cliinfo & NOT seginfo THEN newline()

        IF seginfo | cliinfo
          $( LET segl = taskcb ! Tcb_seglist
             LET printed = FALSE

             FOR j=1 TO segl!0
             DO $( LET seg = segl!j
                   UNTIL seg=0
                   DO $(
                      IF testflags(1) THEN stop(10)
                      IF seginfo | (NOT printed & j>=3)
                      THEN
                        $(
                        wrch(' ')
                        write_sectname(seg)
                        printed := TRUE
                        $)

                      TEST seg = cliseg
                      THEN // This is a CLI task
                        $( LET s = (taskcb ! Tcb_gbase) ! Cli_module_gn
                           TEST s = 0
                           THEN writes(" No command loaded")
                           ELSE
                             $(
                             writes(" Loaded as command: ")
                             write_sectname(s)
                             $)
                        $)
                      ELSE IF seginfo THEN newline()
                      seg := !seg
               $)
                $)
            newline()
            $)

        $)
      $)
    $)


AND stringval(s) = VALOF
    $(  // converts a string to a number
    LET val = 0

    FOR j = 1 TO s%0
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


AND write_sectname(s) BE
    TEST (s!2 = secword) & ((s+3)%0 = 7)
    THEN FOR i=1 TO 7 DO wrch( [s+3]%i )
    ELSE writes("???????")
