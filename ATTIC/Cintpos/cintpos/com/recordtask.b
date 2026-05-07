/*
The record task used by the record command

Implemented by Martin Richards (c) April 21/4/2003

24/02/09 MR
Move cosendpkt, findpkt and pktlist to this module from dlib

29/4/03 MR
Modified to use new COHAND facilities

21/4/02 MR  
Modified to run under Cintpos
*/


SECTION "REC-TASK"

GET "libhdr"
GET "manhdr"

MANIFEST {
  record_stop =   -1
  //act_ttyout  = 1000
  time_char   = #377
  time_tick1  = #376
  time_unit   = 1 // One tick units
  msecspertick = 20
  maxdelay    = 60*60*1000 / msecspertick // One hour
}

GLOBAL {
  inid : ug
  outid
  ttyoutco
  workco
  lasttime
  stream
  cohandid
  stopping
  stopped

  pktlist
  cosendpkt
  findpkt
}

/* res := cosendpkt(link, id, act, r1, r2, a1, a2, a3, a4, a5, a6)

This routine is a substitute sendpkt for multi-event tasks.  It can
only be called when running as a non root coroutine of a task.  A
packet-coroutine pair is placed in pktlist before dispatching the
packet (using qpkt) so that when the packet returns to this task this
coroutine can be reactivated.  The globals cis, cos, pvsline and
currentdir are saved and restored by cosendpkt.

Multi-event mode is entered by executing

     pktlist, sendpkt := 0, cosendpkt

Re-implemented by MR 7/6/02, futher modified MR 7/5/03
*/

LET cosendpkt(link, id, act, r1, r2, a1, a2, a3, a4, a5, a6) = VALOF
{ // The following local variables form the pktlist node.
  // Functions other than cosendpkt may manipulate it
  // so its format must not change.
  LET next,      pkt,     co, ocis, ocos, ocurrentdir =
      pktlist, @link, currco,  cis,  cos,  currentdir


//sawritef("DLIB cosendpkt: sending pkt=%n from %n to %n pktlist=%n*n",
//          @link, taskid, id, pktlist)
//abort(1000)

  // Safety check -- cosendpkt cannot be called from the root coroutine
  IF currco!co_parent<=0 DO
  { sawritef(
     "DLIB co=%n T%n: can't cosendpkt %n(id=%n,type=%n) from root coroutine*n",
       currco, taskid, pkt, id, act)
    abort(999)
  }

  pktlist := @next       // Insert it at the head of pktlist

//sawritef("DLIB:   co=%n: cosendpkt %n(id=%n,type=%n) calling cowait*n",
//  currco, pkt, pkt!pkt_id, pkt!pkt_type)

//sawritef(
//   "DLIB: co=%n T%n: cosendpkt calling qpkt %n(id=%n,type=%n, arg1=%n,arg2=%n)*n",
//     currco, taskid, pkt, id, act, a1, a2)

  UNLESS qpkt(pkt) DO
  { sawritef("DLIB co=%n: cosendpkt -- qpkt failure*n", currco)
    abort(181)
  }

  { LET p = cowait() // Safety check -- we must resume with the
    IF p=pkt BREAK   // expected packet.
    sawritef("DLIB co=%n T=%n: cosendpkt: received wrong pkt=%n should be %n*n",
              currco, taskid,  p, pkt)
    abort(182)
  } REPEAT

//sawritef("DLIB:   co=%n: cosendpkt %n(res1=%n,res2=%n) resumes*n",
//  currco, pkt, pkt!pkt_res1, pkt!pkt_res2)

  // Restore the saved globals
  cis, cos, currentdir := ocis, ocos, ocurrentdir 

  result2 := r2
  RESULTIS r1
}

/* cptr := findpkt(pkt)

This routine searches the pktlist for the specified packet. If found
the list item is dequeued and a pointer to the coroutine is
returned. Zero is returned if the packet is not found in pktlist.
*/

AND findpkt(pkt) = VALOF
{ LET a = @pktlist
//sawritef("DLIB findpkt: task=%n from %n => ", taskid, pkt!pkt_id)

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


LET start(pkt) BE // Recording task
{ LET oldsendpkt = sendpkt
  LET res1, res2 = 0, 0

  initio()
  set_process_name("Recording task")
  stream     := findoutput(pkt_arg1 ! pkt)
  cohandid   := pkt_arg2 ! pkt              // COHAND task id
  currentdir := pkt_arg3 ! pkt

//sawritef("recordtask: findoutput(%s) => %n*n", pkt!pkt_arg1, stream)

  inid := sendpkt(notinuse, cohandid, Action_devices,
                  0, 0,  // res1, res2
                  0, 0)  // arg1, arg2
  outid := result2
  ttyoutco := 0
  workco   := 0

//sawritef("recordtask: inid=%n outid=%n*n", inid, outid)

  UNLESS stream DO
  { res1 := FALSE
    GOTO fin
  }

  UNLESS scb_type ! stream = scbt_file DO
  { writef("recordtask: output must go to a file*n")
    endstream(stream)
    res1 := FALSE
    GOTO fin
  }

  ttyoutco := createco(ttyoutfn, 700)
  workco   := createco(workfn,   700)
  UNLESS ttyoutco & workco DO
  { writef("recordtask: Unable to create the coroutines*n")
    res1 := FALSE
    GOTO fin
  }

//sawritef("recordtask: returning startup packet to the CLI*n")
  returnpkt(pkt, TRUE)   // Return the startup packet

  selectoutput(stream)

  // Cause COHAND to send ttyout characters to this task
  sendpkt(notinuse, cohandid, Action_devices,
          0, 0,       // res1, res2
          0, taskid)  // arg1, arg2

  lasttime := ticks()         // Initialise the ticks counter

  // Enter multi-event mode
  pktlist, sendpkt := 0, cosendpkt

  stopping, stopped := FALSE, FALSE

//sawritef("recordtask: entering main record loop*n")
  // Initially we are waiting for a ttyout packet from COHAND
  // or a stop packet from a CLI

  { LET p  = taskwait()
    LET co = findpkt(p)
    UNLESS co DO co := workco

    // The only packets expected are:
    //   1) a ttyout packet from COHAND
    //   2) packets belonging to the ttyout coroutine
    //      including ttyout packet from the ttyout device
    //      and FH0 packets used when writing the recording to disc
    //   3) a stop packet from a CLI ending the recording.

    callco(co, p) // co is the ttyout coroutine
  } REPEATUNTIL stopped

  // Resume single-event mode
  sendpkt := oldsendpkt

  // Finish off...
fin:
//sawritef("recordtask: finishing off*n")
  IF stream   DO endstream(stream)
  IF ttyoutco DO deleteco(ttyoutco)
  IF workco   DO deleteco(workco)

  // Delete this task and its code
  endtask(rootnode!rtn_crntask!tcb_seglist!3)
}

AND workfn(pkt) BE SWITCHON pkt!pkt_type INTO
{ DEFAULT:    // Unexpected packet
sawritef("recordtask: Unexpected packet received from %n type=%n*n",
          pkt!pkt_id, pkt!pkt_type)
//abort(1000)
           qpkt(pkt)
           RETURN

  CASE record_stop:            // Cause recording to stop
           stopping := TRUE
           IF stream DO { endwrite(stream); stream := 0 }
           qpkt(pkt)
           // This task will delete itself when the ttyout packet arrives
           RETURN

  CASE Action_ttyout:  // A ttyout ch received from COHAND
                      // Send the character to both
                      // the recording and the original device
                      // then return the pkt to COHAND
           callco(ttyoutco, pkt)
}

AND ttyoutfn(pkt) BE
{ // Body of ttyoutco running in multi-event mode
  LET ch = pkt!pkt_arg1
  LET time = ticks()
  LET diff = (time - lasttime)/time_unit
  lasttime := time

  // First send ch to the original device
  sendpkt(notinuse, outid, Action_ttyout,
          0, 0,    // res1, res2
          ch)      // arg1

  IF stopping DO
  { // A character has been received from COHAND
    // after 'record off' has been called.
    // The recording file is already closed, so
    // all we have to do is close down and delete
    // the recording task.

    // Cause COHAND to send the next ttyout character to the original
    // tty device.
    sendpkt(notinuse, cohandid, Action_devices,
            0, 0,      // res1, res2
            0, outid)  // arg1, arg2
    // It is now safe to return the ttyout packet to COHAND
    // (it will not come here again).
    qpkt(pkt)
    stopped := TRUE
    RETURN
  }

  // Possibly write the tick count
  IF diff>0 TEST diff>1
            THEN { wrch(time_char)
                   // Limit the maximum delay
                   IF diff>maxdelay DO diff := maxdelay
                   WHILE diff >= 255 DO
                   { wrch(255)
                     diff := diff - 255
                   }
                   wrch(diff | 256) 
                 }
            ELSE wrch(time_tick1)

  // Now write the character
  wrch(ch)
  qpkt(pkt) // Send the ttyout packet back to COHAND
  // Wait for next ttyout packet from COHAND
}


AND ticks() = VALOF // Gets time in 'ticks', ignoring overflow
{ LET tv = VEC 2    // tv => [days, msecs]
  datstamp(tv)
  RESULTIS tv!1 / msecspertick
}



