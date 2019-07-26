// This is a program to demonstrate various synchronisation
// mechanisms implemented using coroutines and multi-event tasks.

GET "libhdr"

GLOBAL {
  killco: ug     // The killer coroutine
  cosendpkt
  findpkt
  pktlist
  multi_done     // The multi-event loop terminated when this
                 // becomes TRUE
  mainco_busy
  multi_count    // Count of existing multi-event coroutines
  channel        // For occum channels test
  log_wait_queue // For lock_logfile test
  tracing
}

LET start() BE
{
  //gomultievent(occamchannelsfn, 300)

  //writef("End of Occam channel demo*n*n")

  gomultievent(locksfn, 300)
}

AND occamchannelsfn() BE // The multi-event main coroutine body
//{ LET k, n = 100, 50
{ LET k, n = 50, 1000
  LET cptr, source_co = 0, 0

  multi_done := FALSE
  multi_count := 0

  tracing := FALSE

  writef("Test Occam Style Channels*n")

  writef("*nSending %n numbers via %n copy coroutines*n*n", k, n)

  cptr := initco(sinkfn, 300, n+1)

  FOR i = 1 TO n DO
    cptr := initco(copyfn, 300, cptr, n-i+1)

  source_co := initco(sourcefn, 300, cptr, 0)

  IF tracing DO writef("All coroutines created*n*n")

  callco(source_co, k) // Tell sourceco to send k numbers 
}

AND sourcefn(args) BE
{ LET nextco = args!0
  LET id = args!1
  LET k = cowait()
  LET channel = 0
  LET out_chan_ptr = @channel

  multi_count := multi_count + 1

  callco(nextco, out_chan_ptr)
 
  IF tracing DO
    sawritef("srce%i4: out_chan_ptr=%n k=%n*n*n", id, out_chan_ptr, k)

  FOR val = 1 TO k DO
  { //IF tracing DO
      sawritef("srce%i4: sending number %n*n", id, val)
    cowrite(out_chan_ptr, val)
  }
  //IF tracing DO
    sawritef("srce%i4: sending number %n*n", id, 0)
  cowrite(out_chan_ptr, 0)
  IF tracing DO sawritef("srce%i4: dying*n", id)
  multi_count := multi_count - 1
  IF multi_count=0 DO multi_done := TRUE
  die()
}

AND copyfn(args) BE
{ LET nextco = args!0
  LET id     = args!1
  LET channel = 0
  LET in_chan_ptr, out_chan_ptr = cowait(), @channel

  multi_count := multi_count + 1
  
  callco(nextco, out_chan_ptr)
  IF tracing DO sawritef("copy%i4: in_chan_ptr=%n out_chan_ptr=%n*n",
                        id, in_chan_ptr, out_chan_ptr)

  { LET val = coread(in_chan_ptr)
    IF tracing DO sawritef("copy%i4: recving number %n*n", id, val)
    IF randno(1000)=13 DO
    { //IF tracing DO
        sawritef("copy%i4: delay for one second before sending %n*n",
                                      id, val)
      delay(1000)
    }
    IF tracing DO sawritef("copy%i4: sending number %n*n", id, val)
    cowrite(out_chan_ptr, val)
    UNLESS val BREAK
  } REPEAT

  IF tracing DO sawritef("copy%i4: dying*n", id, currco)
  multi_count := multi_count - 1
  IF multi_count=0 DO multi_done := TRUE
  die()
}

AND sinkfn(args) BE
{ LET id = args!0
  LET in_chan_ptr = cowait()

  multi_count := multi_count - 1

  IF tracing DO
    sawritef("sink%i4: in_chan_ptr=%n*n", id, in_chan_ptr)

  { LET val = coread(in_chan_ptr)
    //IF tracing DO
      sawritef("sink%i4: recving number %n*n", id, val)
    UNLESS val BREAK
  } REPEAT

  //IF tracing DO
    writef("sink%i4: dying*n", id)

  multi_count := multi_count - 1
  IF multi_count=0 DO multi_done := TRUE
  die()
}

AND coread(ptr) = VALOF
{ LET cptr = !ptr
  TEST cptr
  THEN { !ptr := 0             // Clear the channel word
         RESULTIS resumeco(cptr, currco)
       }
  ELSE { !ptr := currco    // Set channel word to this coroutine
         RESULTIS cowait() // Wait for value from cowrite
       }
}

AND cowrite(ptr, val) BE
{ LET cptr = !ptr
  TEST cptr
  THEN { !ptr := 0
         callco(cptr, val) // Send val to coread
       }
  ELSE { !ptr := currco
          callco(cowait(), val)
       }
}


AND locksfn() BE
{ multi_done := FALSE
  writef("Testing Locks*n")
  log_wait_queue := 0
  multi_count    := 0  // Hold the number of multi-event coroutines
  //FOR i = 1 TO 40 DO
  FOR i = 1 TO 4 DO
  { LET co = createco(lockcofn, 300)
    IF co DO { multi_count := multi_count+1
//sawritef("Starting coroutine %n: multi_count=%n*n", i, multi_count)
               callco(co, i)
             }
  }
}

AND lockcofn(n) = VALOF
{ FOR i = 1 TO 5 DO
  { // delay for between 0 and 2 secs
    delay(2000 * randno(1000) / 1000) 
    lock_logfile()
    //newline()
    writef("coroutine %i3: message %i3 hello*n", n, i)
    delay(200)
    writef("coroutine %i3: message %i3 world*n", n, i)
    unlock_logfile()
  }
  // Cause this coroutine to commit suicide.
  //sawritef("Coroutine committing suicide, multi_count=%n*n", multi_count)
  multi_count := multi_count-1
  IF multi_count=0 DO multi_done := TRUE
  
  die()
}

AND lock_logfile() BE
  TEST log_wait_queue = 0
  THEN log_wait_queue := -1            // Mark as locked
  ELSE { LET link, co = 0, currco      // Make lock node [link, co]
         LET ocis, ocos = cis, cos
         TEST log_wait_queue=-1
         THEN log_wait_queue := @link  // Make a list of length one
         ELSE { LET p = log_wait_queue // or append the lock node
                WHILE !p DO p := !p    // to the end of the queue.
                !p := @link
              }
         cowait() // Suspend until unlock_logfile() is called
         // We now own the lock and log_wait_queue will be non zero
         cis, cos := ocis, ocos
       }

AND unlock_logfile() BE
  TEST log_wait_queue = -1
  THEN log_wait_queue := 0             // Mark as unlocked
  ELSE { LET co = log_wait_queue!1     // Dequeue the first lock node
         log_wait_queue := !log_wait_queue
         UNLESS log_wait_queue DO log_wait_queue := -1
         callco(co) // Give control to the first coroutine
       }


AND gomultievent(maincofn, size) = VALOF
{ // maincofn is the body of the main coroutine.
  // It is only called when the task is running in multi-event mode.
  // If given the value 0, it creates and initialises the multi-event
  // coroutines, otherwise it is given packets to process provided
  // mainco_busy is FALSE.
  // killco is created to provide a convenient means of letting
  // multi-event coroutines commit suicide.
  // The multi-event loop terminated when multi_done becomes TRUE
  // and there are no packets in either wkq or pktlist.
  LET res = FALSE
  LET wkq = 0              // List of packets to be processed by mainco
  LET oldsendpkt = sendpkt // previous version of sendpkt

  // Create the multi-event main and killer coroutine
  LET mainco = createco(maincofn, size)

  killco := createco(deleteco, 200)
  UNLESS mainco & killco GOTO fin

  res := TRUE
  multi_done := TRUE   // Count of multi-event coroutines
  mainco_busy := FALSE // mainco is ready to process packets

  sendpkt, pktlist := cosendpkt, 0  // Enter multi-event mode

  callco(mainco, 0) // Tell mainco to initialise everything

  UNTIL multi_done & wkq=0 & pktlist=0 DO
  { // Start of the multi-event loop
    LET pkt = taskwait()
    LET co  = findpkt(pkt) // See if it is owned by a coroutine

    TEST co
    THEN callco(co, pkt)   // If so give it to its coroutine
    ELSE TEST mainco_busy
         THEN { // mainco is bust so append the pkt to the end of wkq
                LET p = @wkq
                WHILE !p DO p := !p
                !pkt, !p := 0, pkt
              }
         ELSE { // mainco is not busy so give the pkt to mainco
                callco(mainco, pkt)
                IF mainco_busy | wkq=0 BREAK
                // mainco is not busy and has another pkt to process
                // so de-queue it and give it to mainco
                pkt  := wkq
                wkq  := !pkt
                !pkt := notinuse
              } REPEAT

//sawritef("mainco: count=%i2 done=%i2 wkq=%n list=%n*n",
//          multi_count, multi_done, wkq, pktlist)
  }

  sendpkt := oldsendpkt         // Return to single event mode

fin:
//writef("gomultievent deleting mainco and killco*n")
  IF mainco DO deleteco(mainco) // and delete the main coroutine
  IF killco DO deleteco(killco) // and the killer coroutine
  RESULTIS res
}

// This function overrides the standard Cintpos version of sendpkt.
AND cosendpkt(link, id, act, r1, r2, a1, a2, a3, a4, a5, a6) = VALOF
{ // The following variables form the pktlist node.
  // Functions other than cosendpkt and findpkt may manipulate pktlist
  // so its format must only be changed with care.
  LET next,      pkt,     co, ocis, ocos, ocurrentdir =
      pktlist, @link, currco,  cis,  cos,  currentdir

//sawritef("cosendpkt: sending pkt=%n from %n to %n pktlist=%n*n",
//          @link, taskid, id, pktlist)
//abort(1000)

  // Safety check -- cosendpkt cannot be called from the root coroutine
  IF currco!co_parent<=0 DO
  { sawritef(
     "cosendpkt: co=%n: can't cosendpkt %n(id=%n,type=%n) from root coroutine*n",
       currco, pkt, id, act)
    abort(999)
  }

  pktlist := @next       // Insert it at the head of pktlist

//sawritef(
//   "cosendpkt co=%n: calling qpkt %n(id=%n,op=%n,a1=%n,a2=%n)*n",
//     currco, pkt, id, act, a1, a2)

  UNLESS qpkt(pkt) DO
  { sawritef("cosendpkt co=%n: cosendpkt -- qpkt failure*n", currco)
    abort(181)
  }

//sawritef("cosendpkt co=%n: calling cowait*n",
//  currco, pkt, pkt!pkt_id, pkt!pkt_op)

  { LET p = cowait() // Safety check -- we must resume with the
    IF p=pkt BREAK   // expected packet.
    sawritef("cosendpkt co=%n: received wrong pkt=%n should be %n*n", currco, p, pkt)
    abort(182)
  } REPEAT

//sawritef("cosendpkt co=%n: resumes with pkt %n(r1=%n,r2=%n)*n",
//  currco, pkt, pkt!pkt_r1, pkt!pkt_r2)

  // Restore the saved globals
  cis, cos, currentdir := ocis, ocos, ocurrentdir 

  result2 := r2
  RESULTIS r1
}

AND findpkt(pkt) = VALOF
{ LET a = @pktlist
//sawritef("findpkt: task=%n from %n => ", taskid, pkt!pkt_id)//; abort(999)

  // Search pktlist for the relevant item
  { LET p = !a
    UNLESS p DO
    { //sawritef("not found*n")
      RESULTIS 0   // Not found
    }
    IF p!1 = pkt DO
    { //sawritef("%n*n", p)
      !a := !p            // Remove from pktlist
      RESULTIS p!2        // The coroutine
    }
    a := p
  } REPEAT
}

// Die is called when a coroutine wishes to commit suicide.
AND die() BE resumeco(killco, currco)


