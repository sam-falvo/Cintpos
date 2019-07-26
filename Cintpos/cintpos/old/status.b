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
**    Author:    Brian Knight                         March 1978      **
**                                                                    **
**    14/12/2001 Martin Richards  Modified for Cintpos                **
***********************************************************************/



SECTION "status"

GET "libhdr"
GET "clihdr"

MANIFEST $( secword = #xFDDF }

LET start() BE
{ LET tasktab = rootnode ! rtn.tasktab
  LET ctcb    = rootnode ! rtn.crntask
  LET seglist = ctcb ! tcb.seglist
  LET cliseg = seglist!4    // The CLI segment
  LET argv = VEC 40
  LET tcbinfo, seginfo = ?, ?
  LET cliinfo = ?
  LET lower, upper = 1, tasktab!0
  UNLESS rdargs("TASK,FULL/S,TCB/S,SEGS/S,CLI=ALL/S", argv, 40) DO
  { writes("Args no good*n")
    stop(20)
  }

  IF ( argv ! 0 = 0 ) &                    // TASK
     ( argv ! 1 = 0 ) &                    // FULL
     ( argv ! 2 = 0 ) &                    // TCB
     ( argv ! 3 = 0 ) &                    // SEGS
     ( argv ! 4 = 0 ) DO argv ! 4 := 1     // CLI

  tcbinfo := (argv!1 ~= 0) | (argv!2 ~= 0)
  seginfo := (argv!1 ~= 0) | (argv!3 ~= 0)
  cliinfo := seginfo | (argv!4 ~= 0)

  IF argv!0 DO
  { // Only give status of specified task
    LET n = stringval(argv!0)

    UNLESS 1 <= n <= tasktab!0 & tasktab!n DO
    { writef("Task %n does not exist*n", n)
      stop(20)
    }

    lower, upper := n, n
  }

  FOR j = 1 TO tasktab!0 DO
  { LET taskcb = tasktab!j
    LET state = taskcb ! tcb.state
    LET flags = taskcb ! tcb.flags
    LET dead = (state & State_dead) = State_dead

    IF testflags(flag_b) BREAK

    IF taskcb DO
    { writef("Task %i2:", taskcb ! tcb.taskid)
      writef(" %tF ", @taskcb!tcb.namebase)  // MR 3/2/03

      IF tcbinfo DO
      { writef(" pri %i4,", taskcb ! tcb.pri)
        UNLESS dead DO
          writef(" stk %i5, gv %i5,",
                 taskcb ! tcb.stsiz,
                 (taskcb ! tcb.gbase) ! 0)
      }

      TEST dead
      THEN writes(" dead")
      ELSE { IF (state & NOT State_pkt) = 0 TEST j=taskid
               THEN writes(" running") // Current task
               ELSE writes(" suspended (in qpkt)")
             IF (state & State_wait) ~= 0 DO writes(" waiting")
             IF (state & State_int)  ~= 0 DO writes(" interrupted")
           }

      IF (state & State_hold) ~= 0 DO writes(" held")
      IF (flags & Flag_break) ~= 0 DO writes(" broken")
      IF (state & State_pkt)  ~= 0 DO writes(" with packet(s)")

      UNLESS cliinfo & NOT seginfo THEN newline()

      IF seginfo | cliinfo
      { LET segl = taskcb ! tcb.seglist
        LET printed = FALSE

        FOR j=1 TO segl!0 DO
        { LET seg = segl!j
          WHILE seg DO
          { IF testflags(flag_b) DO stop(10)
            IF seginfo | (NOT printed & j>=3) DO
            { wrch(' ')
              write_sectname(seg)
              printed := TRUE
            }

            TEST seg = cliseg
            THEN { // This is a CLI task
                   LET s = (taskcb ! tcb.gbase) ! cli.module.gn
                   TEST s = 0
                   THEN writes(" No command loaded")
                   ELSE { writes(" Loaded command: ")
                          write_sectname(s)
                        }
                 }
            ELSE IF seginfo DO newline()
            seg := !seg
          }
        }
        newline()
      }

    }
  }
}


AND stringval(s) = VALOF
{ // converts a string to a number
  LET val = 0

  FOR j = 1 TO s%0 DO
  { UNLESS '0' <= s%j <= '9' DO
    { writef("Invalid char *'%c*' in number*N", s%j)
      stop(20)
    }
    val := val*10 + s%j - '0'
  }

  RESULTIS val
}


AND write_sectname(s) BE
  TEST (s!2 = secword) & ((s+3)%0 = 7)
  THEN writes(s+3)
  ELSE writes("???????")
