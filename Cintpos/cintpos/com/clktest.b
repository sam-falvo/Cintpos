/*
This command attempts to test the accuracy of clock interrupts
(c) Martin Richards 27/02/2010

*/

SECTION "CLKTEST"

GET "libhdr"
 
GLOBAL {
  counter:ug
  count
  msecs
  tab
  countertask
  code
}

LET start(pkt) BE // clktest [count] [msecs]
{ LET ptr  = 0
  LET argv = VEC 50
  LET tcb  = rtn_crntask ! rootnode   // The current TCB

  IF pkt DO
  { // This is the body of the counter task
    ptr := pkt!pkt_a1

    // Increment the counter as fast as possible (for ever)
    !ptr := !ptr + 1 REPEATUNTIL testflags(flag_a)

    // Make sure this task returns to dead state before it is deleted
    changepri(taskid, maxint)
    // Return the startup pkt to the CLI
    qpkt(pkt)
    RETURN
  }

  count := 256
  msecs := 5
  tab := 0
  countertask := 0
  counter := 0
  code := 0

  UNLESS rdargs("COUNT/n,MSECS/n",argv,50) DO
  { writes("Bad parameters for CLKTEST*n")
    stop(return_hard)
  }

  IF argv!0 DO count := !(argv!0)   //   COUNT/n
  IF argv!1 DO msecs := !(argv!1)   //   MSECS/n

  tab := getvec(count)

  UNLESS tab DO
  { writef("getvec failure*n")
    RETURN
  }

  { LET segl = VEC 3

//    code := loadseg("cin/clktest")
    code := loadseg("clktest")
    UNLESS code DO
    { writef("Unable to load cin/clktest*n")
      GOTO fin
    }
    segl!0 := 3
    segl!1 := tcb!tcb_seglist!1
    segl!2 := tcb!tcb_seglist!2
    segl!3 := code

    // Create the counter task
    countertask := createtask(segl, 1700, tcb_pri!tcb - 50)

    UNLESS countertask DO
    { writes("Failed to make the counter task*n")
      stop(return_hard)
    }

writef("*nclktest: counter task %n created*n", countertask)
writef("*nclktest running with count=%n and msecs=%n*n", count, msecs)

    // Send a startup packet to the counter task


    { LET startpkt = VEC pkt_a1
      startpkt!pkt_link := notinuse
      startpkt!pkt_taskid := countertask
      startpkt!pkt_a1 := @counter

      qpkt(startpkt)

      FOR i = 0 TO count-1 DO
      { LET counter0 = counter
        // Counter is continually incremented by the counter task
        delay(msecs)
        tab!i := counter - counter0
      }

      // Tell the counter task to return to dead state
      setflags(countertask, flag_a)


      UNLESS taskwait()=startpkt DO
        writef("Wrong pkt received*n")

      FOR i = 0 TO count-1 DO
      { IF i MOD 8 = 0 DO newline()
        writef(" %i7", tab!i)
      }
      newline()
    }
  }

fin:
  writef("*nDeleting counter task %n*n", countertask)
  IF countertask DO
  { LET rc = deletetask(countertask)
    LET r2 = result2
    UNLESS rc DO
      writef("deletetask() => %n, result2=%n*n", rc, r2)
  }
  IF tab  DO freevec(tab)
  IF code DO unloadseg(code)
}

