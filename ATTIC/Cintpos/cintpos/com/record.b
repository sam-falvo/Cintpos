/*
Record command   (c) Martin Richards April 21/4/03

29/4/03 MR
Modified to use new features in COHAND
21/4/03 MR
Modified to run under Cintpos
*/

SECTION "RECORD"

GET "libhdr"
GET "manhdr"
 
MANIFEST {
  record_stop = -1
}

LET start() BE // RECORD [TO] file [NOTIME] | OFF
{ LET argv  = VEC 50
  LET ttab  = rtn_tasktab ! rootnode
  LET tcb   = rtn_crntask ! rootnode        // The current TCB
  LET cohandid = scb_task ! cli_standardoutput // MR 21/4/03
  LET ctcb  = ttab ! cohandid                  // TCB of COHAND

  // Find the current ttyin and ttyout devices used by the
  // console handler.
  LET inid = sendpkt(notinuse, cohandid, Action_devices,
                     0, 0,  // res1, res2
                     0, 0)  // arg1, arg2
  LET outid = result2

  LET code  = 0

  UNLESS rdargs("TO,OFF/S",argv,50) DO
  { writes("Bad parameters for RECORD*n")
    stop(return_hard)
  }

  IF argv!0 & argv!1 DO
  { writes("Can't have both OFF and TO arguments*n")
    stop(return_soft)
  }

  IF argv!1 DO                // Recording off
  { IF outid < 0 DO
    { writes("Recording wasn't on!*n")
      stop(return_soft)
    }
    newline()
    // Cause the recording task to close down and delete itself
    sendpkt(notinuse, outid, record_stop)
    RETURN
  }

  IF argv!0 DO                // Start recording
  { LET segl = VEC 3

    IF outid > 0 DO
    { writes("Recording already on!*n")
      stop(return_soft)
    }

    code := loadseg("cin/recordtask")

    segl!0 := 3
    segl!1 := tcb!tcb_seglist!1
    segl!2 := tcb!tcb_seglist!2
    segl!3 := code

    UNLESS code DO
    { writes("Failed to load task cin/recordtask*n")
      stop(return_hard)
    }

    outid := createtask(segl, 700, tcb_pri!ctcb - 50)

    UNLESS outid DO
    { writes("Failed to make task*n")
      unloadseg(code)
      stop(return_hard)
    }

//sawritef("*nrecord: task %n created*n", outid)

    // Send a startup packet to the recording task
    UNLESS sendpkt(notinuse, outid, ?,
                   ?, ?,   // res1, res2
                   argv!0, cohandid, currentdir) DO
    { writef("Can't open %s for RECORD*n",argv!0)
      deletetask(outid)
      unloadseg(code)
      stop(return_hard)
    }
    RETURN
  }

  writef("Must have either TO or OFF*n")
}




