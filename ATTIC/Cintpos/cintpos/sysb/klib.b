SECTION "KLIB"

GET "libhdr"

/*

Error numbers:

101  Bad task or device identifier
102  Invalid priority
103  Insufficient store
104  Device table full
105  Task table full
106  Failure to initialise device
107
108  Task not deletable
109  Packet not found
110  Task already held
111  Invalid link field

*/

LET retres(a) = a  // return the result in the A register
                   // (retres -> RTN instruction)

/***************************************************************
*                                                              *
*                 res := qpkt(pkt)                             *
*                                                              *
* This function queues the pkt onto the work Q                 *
* of its destination task or device.                           *
* Packet offset P_ID >  0 -> destination is a task             *
*                    = -1 ->     ..      .. clock              *
*                    < -1 ->     ..      .. a device           *
* If the packet is Qed successfully then the task id of the    *
* sender is inserted in this field.                            *
*                                                              *
* On return:                                                   *
*    RES ~= 0    OK                                            *
*    RES  = 0    Error                                         *
*                RESULT2 = 101   Invalid id                    *
*                RESULT2 = 111   Invalid link field            *
*                                                              *
***************************************************************/
 
LET qpkt(pkt) = VALOF
{ LET tcb, currid, destid = ?, ?, ?

  UNLESS pkt_link!pkt=notinuse DO
  { sawritef("T%z2 KLIB: qpkt: pkt=%n link not = notinuse*n",
              taskid, pkt)
    abort(999)
    result2 := 111
    RESULTIS 0
  }
  sys(Sys_setst, 1)
  tcb := rtn_crntask!rootnode
  currid, destid := tcb_taskid!tcb, pkt_id!pkt
//sawritef("KLIB: qpkt: sending pkt from %n to %n*n", currid, destid)

taskdest:
  IF destid>0 DO                        // Is destination a task?
  { LET tasktab = rtn_tasktab!rootnode  // Yes
    LET dtcb, p = ?, ?
//sawritef("KLIB: qpkt: the destination is a task*n")
    UNLESS destid<=tasktab!0 & tasktab!destid DO
    { sys(Sys_setst, 0)
      result2 := 101  // Bad destination task number
      RESULTIS 0
    }
 
    dtcb := tasktab!destid
    pkt_link!pkt := 0       // Fill in end-of-list marker
    pkt_id!pkt := currid    // Fill in return task id

    UNLESS tcb_wkq!dtcb DO
    { // The wkq was empty
      tcb_wkq!dtcb := pkt
      tcb_state!dtcb := tcb_state!dtcb + #b0001  // Set the PKT bit
//sawritef("KLIB: qpkt: sent to a task that had no pkts*n")
      IF tcb_pri!dtcb > tcb_pri!tcb DO       
      { // The dest task has higher priority
        sys(Sys_saveregs, @ tcb_regs!tcb) // Suspend the current task
        tcb_st!tcb := 0
        tcb_pc!tcb := retres
//sawritef("KLIB: qpkt: the dest task had higher priority*n")
        srchwk(dtcb)  // give control to the destination task
      }
      // otherwise just return successfully from qpkt
      sys(Sys_setst, 0)
      RESULTIS 1
    }

    // The wkq was not empty -- so no scheduling necessary
    p := tcb_wkq!dtcb

    // Put pkt at end of wkq
    WHILE pkt_link!p DO p := pkt_link!p
    pkt_link!p := pkt

    sys(Sys_setst, 0)
    RESULTIS 1           // Return from qpkt
  }

  IF destid<0 DO                     // Was the destination a device?
  { LET devtab = rtn_devtab!rootnode // Yes
    LET id, dcb, p = -destid, ?, ?

    // Special treatment for TTYOUT device
    IF destid=-3 DO
    { sys(Sys_sawrch, pkt_arg1!pkt)  // Write the character immediately
      destid := currid
      currid := -3
//sawritef("*nklib qpkt: returning pkt to task %n*n", destid)
      GOTO taskdest                  // return the packet to current task
    }

    // Special treatment for Clock device
    IF destid=-1 DO
    { // Insert the packet into the clock queue
      LET p = @ rtn_clwkq!rootnode
      LET cpkt = !p
      LET delaymsecs = pkt_arg1!pkt
      LET days, msecs, ticks  = ?, ?, ?

      MANIFEST { msecsperday=24*60*60*1000 }
      datstamp(@days)
      // Assume new dat format
      // Calculate the date stamp of the completion time
      msecs := msecs + delaymsecs
      IF msecs>=msecsperday DO
      { days := days + msecs MOD msecsperday
        msecs := msecs / msecsperday
      }
      pkt_res1!pkt := days
      pkt_res2!pkt := msecs

//sawritef("KLIB: qpkt: inserting clock pkt from task %n*n", currid)
//sawritef("KLIB: qpkt: days=%n msecs=%n*n", days, msecs)

      WHILE cpkt DO
      { LET pktdays, pktmsecs = pkt_res1!cpkt, pkt_res2!cpkt
        IF days<pktdays BREAK
        IF days=pktdays & msecs<pktmsecs BREAK
        p := cpkt
        cpkt := !p
      }

      // Insert pkt in the queue at this point
      !p, !pkt := pkt, cpkt
      pkt_id!pkt := currid      // Record the sender

//sawritef("KLIB: qpkt: queuing clk pkt, r1=%n r2=%n*n",
//          pkt!pkt_r1, pkt!pkt_r2)

//      { LET q = rtn_clwkq!rootnode
//        LET days, msecs, n = 0, 0, 0
//        datstamp(@days)
//        sawritef("klib: time now %i6 %i8*n", days, msecs)
//        WHILE q DO
//        { n := n+1
//          sawritef("klib: Clkq %i2: %i6 %i8*n", n, pkt_r1!q, pkt_r2!q)
//          q := !q
//        }
//        sawritef("clkq length =%n*n", n)
//      }

      sys(Sys_setst, 0)         // Enable interrupts
      RESULTIS 1                // Return from qpkt
    }

    // Non-special device packet
    UNLESS 2<=id<=devtab!0 & devtab!id DO
    { result2 := 101       // Bad device identifier
      sys(Sys_setst, 0)
      RESULTIS 0
    }

    dcb := devtab!id
    pkt_link!pkt := 0       // Fill in end-of-list marker
    pkt_id!pkt := currid    // Fill in return task id
    IF Dcb_wkq!dcb DO
    { // Append the packet to a non empty wkq.
      LET p = Dcb_wkq!dcb

      WHILE pkt_link!p DO p := pkt_link!p
      pkt_link!p := pkt

      sys(Sys_setst, 0)
      RESULTIS 1
    }

    // The device had an empty wkq so call its start function
    Dcb_wkq!dcb := pkt
//IF destid<=-4 DO { 
// sawritef("KLIB: sending start to dev %n, pkt=%n*n", destid, pkt)
// abort(1000)
//}
    sys(Sys_devcom, dcb, Devc_start, 2222) // Start the device
    sys(Sys_setst, 0)
    RESULTIS 1
  }

  // This point is only reached if destid=0
  sys(Sys_setst, 0)
  result2 := 101    // Bad destination identifier
  RESULTIS 0
}


/***************************************************************
*                                                              *
*                 ID :=    DQPKT(ID,PKT)                       *
*                                                              *
* Attempts to dequeue PACKET from the work Q of the specified  *
* device or task. If not found there then it attempts to remove*
* the pkt from the work Q of the calling  task.                *
*                                                              *
* On return:                                                   *
*    ID (~=0) = Id of device or task whose WKQ contained the   *
*               packet                                         *
*    ID = 0     Error                                          *
*               RESULT2 = 101   Invalid id                     *
*               RESULT2 = 109   Packet not found               *
* (The ID field of the packet is set to the id of the WKQ on   *
* in which the packet was found provided that this is not the  *
* id of the current task.)                                     *
*                                                              *
***************************************************************/

LET dqpkt(id, pkt) = VALOF   // NOT fully tested ???????????????
{ LET tcb, dcb, q = 0, 0, 0
//sawritef("KLIB: dqpkt(%n, %n) entered*n", id, pkt)
  sys(Sys_setst, 1)
  sys(Sys_lockirq)   // Stop any device from touching packets on
                     // it work queue or generating interrupts

  // Note that while ST=1, cinterp will not accept interrupts and
  // that it is the interrupt service routine (run under cinterp)
  // that de-queues packets from the clock queue and other device
  // work queues.

rep:
  tcb := 0
  // Decide whether we are dequeing from 
  // the clock (id=-1), a task (id>0) or a device (id<-1)
  TEST id=-1
  THEN q := @ rtn_clwkq!rootnode
  ELSE TEST id>0
       THEN { LET tasktab = rtn_tasktab!rootnode
              IF 1<=id<=tasktab!0 DO tcb := tasktab!id
              UNLESS tcb DO { result2 := 101
sawritef("T%z2 KLIB: dqpkt: tcb for task %n not found*n", taskid, id)
                              sys(Sys_unlockirq)
                              sys(Sys_setst, 0)
                              RESULTIS 0
                            }
              q := @ tcb_wkq!tcb
            }
       ELSE { LET devtab, devid = rtn_devtab!rootnode, -id
              IF 1<=devid<=devtab!0 DO dcb := devtab!devid
              UNLESS dcb DO { result2 := 101
sawritef("T%z2 KLIB: dqpkt: dcb for device %n not found*n", taskid, id)
                              sys(Sys_unlockirq)
                              sys(Sys_setst, 0)
                              RESULTIS 0
                            }
              q := @ Dcb_wkq!dcb
            }

  // pkt is the packet
  // id  is the dev/task id to which the packet was sent
  // q   is pointer to head of queue where the packet might be found
  // tcb is the tcb of task id, if id>0
  // dcb id the dcb of dev -id, if id<-1

//sawritef("KLIB: dqpkt: pkt=%n id=%n q=%n tcb=%n dcb=%n*n",
//          pkt, id, q, tcb, dcb)
//abort(1000)

  WHILE q & !q~=pkt DO q := !q // search for pkt

  UNLESS q DO
  { // The packet was not found
    // So look in the current task's wkq, unless already done
    LET crntcb = rtn_crntask!rootnode
sawritef("KLIB: dqpkt: pkt=%n was not found on wkq id=%n*n", pkt, id)
    UNLESS tcb=crntcb DO { id := tcb_taskid!crntcb; GOTO rep }
    // It was not found at either original destination
    // or our own wkq.
sawritef("KLIB: dqpkt: pkt=%n was not found anywhere*n", pkt)
    result2 := 109     // Packet not found
    sys(Sys_unlockirq)
    sys(Sys_setst, 0)
    RESULTIS 0
  }

  // We have found the pkt on the wkq of task/device id
sawritef("KLIB: dqpkt: pkt=%n was found on wkq id=%n*n", pkt, id)
//abort(1001)

  TEST id=-1
  THEN { // Remove pkt from the clock work queue
         LET npkt = pkt!pkt_link
         pkt!pkt_link :=  notinuse
         !q := npkt
         IF npkt DO pkt_arg1!npkt := pkt_arg1!npkt + pkt_arg1!pkt
       }
  ELSE TEST id>0
       THEN { LET npkt = pkt_link!pkt
              pkt!pkt_link :=  notinuse
              !q := npkt 
              UNLESS tcb_wkq!tcb DO
                tcb_state!tcb := tcb_state!tcb & #b1110
            }
       ELSE { LET npkt = pkt_link!pkt
              IF Dcb_wkq!dcb=pkt DO
              { // The packet is at the start of a device wkq.
                // We should call the device stop routine for this pkt
                // and the start routine for the next if any
                // remembering that the start routine may instantly
                // release pkts to higher priority tasks so the
                // current task may have to be suspended.
                // All this is currently not implemented.
                npkt := npkt // a dummy statement
              }
              !q := npkt              
//sawritef("KLIB: dqpkt: pkt %n dequeued from device %n*n", pkt, id)
            }

  pkt_link!pkt := notinuse
  sys(Sys_unlockirq)
  sys(Sys_setst, 0)
  RESULTIS id  // Identifier of task/device where the packet was found
}

/***************************************************************
*                                                              *
*                pkt := taskwait()                             *
*                                                              *
* This is a BCPL callable function of no arguments. The current*
* task is suspended as long as it has an empty work queue.     *
* When the task resumes execution (which may be immediately    *
* the result (PKT) will be the (dequeued) first packet of the  *
* task's work queue.                                           *
*                                                              *
***************************************************************/


LET taskwait() = VALOF
{ LET tcb, pkt = ?, ?
  sys(Sys_setst, 1)           // Disable interrupts
  tcb := rtn_crntask!rootnode
  pkt := tcb_wkq!tcb

//IF tcb!tcb_taskid=4 DO
//  sawritef("KLIB: taskwait => task=%n wkq pkt=%n state=%n*n",
//            tcb!tcb_taskid, pkt!pkt_id, tcb!tcb_state)

  IF pkt DO
  { // There is a pkt on the wkq
    LET npkt = pkt_link!pkt
    tcb_wkq!tcb := npkt
    UNLESS npkt DO
      tcb_state!tcb := tcb_state!tcb & #b1110 // Remove the PKT bit
    sys(Sys_setst, 0)        // Enable interrupts
    pkt_link!pkt := notinuse
//UNLESS -3<=pkt!pkt_id<=-2 | 2<=pkt!pkt_id<=5 | 2<=tcb!tcb_taskid<=5 DO
//  sawritef("KLIB: taskwait => pkt from %n to %n type=%n*n",
//            pkt!pkt_id, tcb!tcb_taskid, pkt!pkt_type)
    RESULTIS pkt
  }

  // No pkt on wkq so must change to WAIT state
  // Don't allow taskwait to clear the HOLD bit
  // (sadebug may allow a task to run with the HOLD bit set)
  tcb_state!tcb := tcb_state!tcb | #b0100   // MR 13/01/05
  // Note that interrupts are currently disabled.
  sys(Sys_saveregs, @ tcb_regs!tcb)
  tcb_st!tcb := 0       // Ensure interrupts are enabled when resuming.
  tcb_pc!tcb := retres  // Resume on a RTN instruction.

  srchwk(tcb_link!tcb)  // Give control to a lower priority task.

  RESULTIS 0 // Never reached
}


/****************************************************************
*                                                              *
*                  RES := HOLD(TASKID)                         *
*                                                              *
* This function sets the HOLD bit in the TCB of the specified  *
* task.  It enters the scheduler if it holds itself.           *
*                                                              *
* On return:                                                   *
*    RES ~= 0    OK -- RES is the TCB of the task just held.   *
*    RES  = 0    Error                                         *
*                RESULT2 = 101   Bad taskid                    *
*                RESULT2 = 110   Task already held             *
*                                                              *
***************************************************************/

LET hold(id) = VALOF
{ LET tasktab, tcb, state = ?, 0, ?
  sys(Sys_setst, 1)    // Disable interrupts
  tasktab := rtn_tasktab!rootnode
  IF 1<=id<=tasktab!0 DO tcb := tasktab!id
  UNLESS tcb DO { result2 := 101
                  sys(Sys_setst, 0)
                  RESULTIS 0
                }
  state := tcb_state!tcb
  UNLESS (state & #b0010) = 0 DO // Already held
  { result2 := 110      // Cannot hold a task that is already held.
    sys(Sys_setst, 0)
    RESULTIS 0
  }
  tcb_state!tcb := state | #b0010 // Set the hold bit.
  IF tcb=rtn_crntask!rootnode DO  // Holding self.
  { sys(Sys_saveregs, @ tcb_regs!tcb)
    tcb_st!tcb := 0
    tcb_a!tcb  := tcb
    tcb_pc!tcb := retres
    srchwk(tcb_link!tcb)  // Give control to a lower priority task.
  }

  // We have just held a different task so the current task
  // can continue to run.
  sys(Sys_setst, 0)        // Enable interrupts.
  RESULTIS tcb       // tcb is the TCB of the task just held.
}

/***************************************************************
*                                                              *
*                RES := RELEASE(TASKID)                        *
*                                                              *
* This function releases a held task. The task scheduler is    *
* used to select the next task to run.                         *
*                                                              *
* On return:                                                   *
*    RES ~= 0    OK                                            *
*    RES  = 0    Error                                         *
*                RESULT2 = 101   Invalid id                    *
*                                                              *
***************************************************************/
 
LET release(id) = VALOF
{ LET tasktab, ctcb, dtcb, state = ?, ?, 0, ?
  sys(Sys_setst, 1)
  tasktab := rtn_tasktab!rootnode
  ctcb    := rtn_crntask!rootnode
  IF 1<=id<=tasktab!0 DO dtcb := tasktab!id
  UNLESS dtcb DO { result2 := 101
                   sys(Sys_setst, 0)
                   RESULTIS 0
                 }

  tcb_state!dtcb := tcb_state!dtcb & #b1101 // Clear the hold bit

  IF tcb_pri!dtcb > tcb_pri!ctcb DO
  { // Releasing a higher pri task, so suspend self
    sys(Sys_saveregs, @ tcb_regs!ctcb)
    tcb_st!ctcb := 0
    tcb_a!ctcb  := dtcb
    tcb_pc!ctcb := retres
    srchwk(dtcb)  // Give control to the higher priority task
  }

  sys(Sys_setst, 0)
  RESULTIS dtcb
}


/****************************************************************
*                                                               *
*               RES := TESTFLAGS(FLAGS)                         *
*                                                               *
* Tests and clears the flags of the current task.               *
* On return:                                                    *
*    RES  = FALSE (=0)  None of the specified flags were set    *
*    RES  = TRUE (-1)   At least one specified flag was set     *
*    RESULT2 = the flags that were changed                      *
*                                                               *
****************************************************************/
 
LET testflags(flags) = VALOF
{ LET tcb = rtn_crntask!rootnode
  LET res = ?
  sys(Sys_setst, 1)
  res := tcb_flags!tcb
  tcb_flags!tcb := res & ~flags    // clear specified flags
  sys(Sys_setst, 0)
  result2 := res & flags
  RESULTIS result2 ~= 0
}


/****************************************************************
*                                                               *
*                 RES := SETFLAGS(TASKID,FLAGS)                 *
*                                                               *
* Sets TCB flags of the specified task.                         *
*                                                               *
* On return:                                                    *
*    RES ~= 0    OK                                             *
*                RESULT2 = previous flag setting                *
*    RES  = 0    Error                                          *
*                RESULT2 = 101    Invalid id                    *
*                                                               *
****************************************************************/
 
LET setflags(id, flags) = VALOF
{ LET tasktab, dtcb = ?, 0

  sys(Sys_setst, 1)
  tasktab := rtn_tasktab!rootnode

  IF 1<=id<=tasktab!0 DO dtcb := tasktab!id

  TEST dtcb
  THEN { result2        := tcb_flags!dtcb
         tcb_flags!dtcb := tcb_flags!dtcb | flags // Set specified flags
       }
  ELSE   result2 := 101

  sys(Sys_setst, 0)
  RESULTIS dtcb
}


/*****************************************************************
*                                                                *
*            ID := CREATETASK(SEGLIST,STSIZE,PRI)                *
*                                                                *
* This function creates a task using the first free slot in the  *
* task table. It gets space for a copy of the segment list and a *
* TCB and initialises them, and inserts the TCB in the task table*
* and priority chain.                                            *
*                                                                *
* On return:                                                     *
*    ID = the id of the created task (>0)                        *
*    ID = 0      Error                                           *
*                RESULT2 = 102   Invalid priority                *
*                RESULT2 = 103   Insufficient store              *
*                RESULT2 = 105   Task table full                 *
*                                                                *
*****************************************************************/

LET createtask(segl, stacksize, pri) = VALOF
{ LET tasktab = rtn_tasktab!rootnode
  LET id      = 0
  LET seglist = ?
  LET tcb     = ?

//sawritef("KLIB: createtask: stacksize=%n pri=%n*n", stacksize, pri)

  sys(Sys_setst, 1)  // MR 11/3/03

  // Check the priority is ok
  tcb := rtn_tcblist!rootnode
  WHILE tcb DO
  { IF tcb_pri!tcb=pri DO { result2 := 102; GOTO ret }
    tcb := tcb_link!tcb
  }

  // Find a suitable task identifier
  FOR i = 1 TO tasktab!0 UNLESS tasktab!i DO { id := i; BREAK  }
  UNLESS id DO { result2 := 105; GOTO ret }

  // Allocate the segment table
  seglist := getvec(segl!0)
  UNLESS seglist DO { id, result2 := 0, 103
                      GOTO ret
                    }
  FOR i = 0 TO segl!0 DO seglist!i := segl!i

  // Allocate the task control block
  tcb := getvec(tcb_upb)
  UNLESS tcb DO { freevec(seglist)
                  id, result2 := 0, 103
                  GOTO ret
                }

  FOR i = 0 TO tcb_upb DO tcb!i := 0

  tcb_taskid!tcb   := id
  tcb_pri!tcb      := pri
  tcb_state!tcb    := #b1100 // DEAD state
  tcb_stsiz!tcb    := stacksize
  tcb_seglist!tcb  := seglist

  tasktab!id := tcb

  // Insert the new TCB into the tcblist.
  { LET p = @rtn_tcblist!rootnode
    LET t = !p
    WHILE t & tcb_pri!t > pri DO { p := t; t := !p }
    !tcb := t
    !p := tcb
  }

ret:
  sys(Sys_setst, 0)
  RESULTIS id
}

/*****************************************************************
*                                                                *
*               RES := DELETETASK(TASKID)                        *
*                                                                *
* This function deletes a task which must have an empty work Q   *
* and either be the current task, or be dead. Its segment list   *
* is freed and the TCB removed from the task table and the       *
* priority chain and then freed. If it was the current task then *
* the task deactivation code is entered to free the stack and    *
* global vector.                                                 *
*                                                                *
* On return:                                                     *
*    RES ~= 0   OK                                               *
*    RES  = 0   Error                                            *
*               RESULT2 = 101   Invalid id                       *
*               RESULT2 = 108   Task not deletable               *
*                                                                *
*****************************************************************/
 
LET deletetask(id) = VALOF
{ LET tasktab = rtn_tasktab!rootnode
  LET ctcb = rtn_crntask!rootnode
  LET dtcb, p = 0, ?
//sawritef("KLIB: deletetask(%n) entered*n", id)
  sys(Sys_setst, 1)

  IF 1<=id<=tasktab!0 DO dtcb := tasktab!id
  UNLESS dtcb DO { sys(Sys_setst, 0)
                   result2 := 101   // Invalid id
//sawritef("KLIB: deletetask(%n) task not found*n", id)
//                   RESULTIS 0
                 }

  IF tcb_wkq!dtcb |
     (tcb_state!dtcb & #b1100) ~= #b1100 & dtcb ~= ctcb DO
  { // The task is not deletable because
    // either its wkq is non empty
    // or     its in a non dead state and is not the current task
    sys(Sys_setst, 0)
    result2 := 108
//sawritef("KLIB: deletetask(%n) can't delete task, wkq=%n state=%b4 dtcb=%n ctcb=%n*n",
//          id, tcb_wkq!dtcb, tcb_state!dtcb, dtcb, ctcb)
    RESULTIS 0
  }

//sawritef("KLIB: deletetask(%n) removing TCB from tasktab*n", id)
  tasktab!id := 0           // clear the tasktab entry

//sawritef("KLIB: deletetask(%n) removing TCB from tasklist*n", id)
  p := @ rootnode!rtn_tcblist
  UNTIL !p=dtcb DO p := !p  // Find the tcb in the tcb list
  !p := !dtcb               // and remove it from the list

//sawritef("KLIB: deletetask(%n) freeing the segment list*n", id)
  freevec(tcb_seglist!dtcb) // Free its segment list
//sawritef("KLIB: deletetask(%n) freeing the TCB*n", id)
  freevec(dtcb)             // and free the tcb

  IF dtcb=rootnode!rtn_crntask DO
  { // We are in a dangerous state. We have already deleted our
    // own segl and tcb but are still using the global vector
    // and stack. We must return these and then enter the scheduler.
    // This is ok since interrupts are disabled and there is no
    // chance that another task can run, so nothing else will
    // allocate or free store until srchwk has run.
//sawritef("KLIB: deletetask(%n) freeing our own stack*n", id)
    freevec(ctcb!tcb_sbase)
//sawritef("KLIB: deletetask(%n) freeing our own global vector*n", id)
    freevec(ctcb!tcb_gbase)
//sawritef("KLIB: deletetask(%n) entering srchwk*n", id)
    srchwk(!p)  // !p is the next lower priority task
  }

  // We were deleting some other task
  sys(Sys_setst, 0)
//sawritef("KLIB: deletetask(%n) successful deletion*n", id)
  RESULTIS -1
}

/*****************************************************************
*                                                                *
*               RES := DELETESELF(PKT, SEG)                      *
*                                                                *
* This function first calls qpkt to return the packet if pkt is  *
* non zero, then calls unloadseg(seg) if seg is non zero, before *
* deleting the current task. This code is in KLIB since it would *
* be unsafe for it to be in a segment that may be unloaded       *
* while it is being executed.                                    *
*                                                                *
* On return:                                                     *
*    RES ~= 0   OK, but, of course, it does not return if OK!!   *
*    RES  = 0   Error                                            *
*               RESULT2 = 101   Invalid id                       *
*               RESULT2 = 108   Task not deletable               *
*                                                                *
*****************************************************************/
 
LET deleteself(pkt, seg) = VALOF
{ IF pkt UNLESS qpkt(pkt) RESULTIS FALSE
  IF seg DO unloadseg(seg)
  RESULTIS deletetask(taskid)
}

/*****************************************************************
*                                                                *
*                RES := CHANGEPRI(TASKID,PRI)                    *
*                                                                *
* This routine alters the priority of a task. Its TCB is moved to*
* the new position in the priority chain, and the task scheduler *
* entered if necessary.                                          *
*                                                                *
* On return:                                                     *
*    RES ~= 0     OK                                             *
*    RES  = 0     Error                                          *
*                 RESULT2 = 101    Invalid id                    *
*                 RESULT2 = 102    Invalid priority              *
*                                                                *
*****************************************************************/

LET changepri(id, pri) = VALOF
{ LET tasktab = rtn_tasktab!rootnode
  LET ctcb    = rtn_crntask!rootnode
  LET dtcb, p, q = 0, ?, ?
  LET cpri, opri = ?, ?

  sys(Sys_setst, 1)

  IF 1<=id<=tasktab!0 DO dtcb := tasktab!id
  IF dtcb=0 DO { result2 := 101
                 sys(Sys_setst, 0)
                 RESULTIS 0
               }

  opri := tcb_pri!dtcb  // Remember the old priority

  IF pri<=0 DO {
prierr:          result2 := 102
                 sys(Sys_setst, 0)
                 RESULTIS 0
               }

  p := @ rootnode!rtn_tcblist
  q := p
  UNTIL !p=dtcb DO p := !p // Find the tcb in the tcb list
  !p := !dtcb              // and remove from the list

  { LET t = !q  // Find the new position in the tcb list
    LET tpri = tcb_pri!t
    IF tpri=pri DO { !p := dtcb // put it back in the tcb list
                     GOTO prierr
                   }
    IF tpri<pri BREAK
    q := t
  } REPEAT

  !dtcb := !q              // Link it into the list
  !q := dtcb
  tcb_pri!dtcb := pri      // Give it the new priority

  cpri := tcb_pri!ctcb     // Find pri of crntask

  // Don't bother to optimise the scheduling

  sys(Sys_saveregs, @ tcb_regs!ctcb) // Save our own registers
  tcb_a !ctcb := TRUE // Set to return with non zero result
  tcb_st!ctcb := 0    // with interrupts enabled
  tcb_pc!ctcb := retres
  srchwk(rootnode!rtn_tcblist)  // Search the entire tcb list
}



/*****************************************************************
*                                                                *
*                 ID :=  CREATEDEV(DCB)                          *
*                                                                *
* This function creates a device using the first free slot in the*
* device table. The DCB should have already been linked to a     *
* device driver.                                                 *
*                                                                *
* On return:                                                     *
*    ID = the device id (<0)                                     *
*       = 0      Error                                           *
*                RESULT2 = 104   Device table full               *
*                RESULT2 = 106   Failure to initialise device    *
*                                                                *
*****************************************************************/

LET createdev(dcb) = VALOF
{ LET devid = 0
  LET devtab = rtn_devtab!rootnode
  sys(Sys_setst, 1)   // Enter kernel mode
  // Find smallest available device number
  FOR i = 1 TO devtab!0 UNLESS devtab!i DO
  { devid := i
    BREAK
  }
//sawritef("KLIB: createdev(%n) devid=%n*n", dcb, -devid)
  TEST devid
  THEN { Dcb_devid!dcb := -devid
//sawritef("KLIB: createdev calling create for  dev=%n*n", -devid)
         TEST sys(Sys_devcom, dcb, Devc_create, 1111)
         THEN devtab!devid := dcb
         ELSE result2, devid := 106, 0
       }
  ELSE  result2, devid := 104, 0 // Device table is full
//sawritef("KLIB: createdev(%n) => %n*n", dcb, -devid)
  sys(Sys_setst, 0)   // Return to user mode
  RESULTIS -devid
}

/*****************************************************************
*                   DCB := DELETEDEV(DEVID)                      *
*                                                                *
* This function closes down the device, and deallocates the      *
* device id, but does not return the DCB to free store (just     *
* as createdev did not allocate the DCB. It returns any packets  *
* still on its WKQ to the requesting tasks with res1 and res2    *
* both set to -1.                                                *
*                                                                *
* On return:                                                     *
*    DCB = BCPL ptr to the DCB (~=0)                             *
*    DCB = 0    Error                                            *
*               RESULT2 = 101      Invalid device id             *
*                                                                *
*****************************************************************/

LET deletedev(devid) = VALOF
{ LET devtab = rtn_devtab!rootnode
  LET n      = -devid
  LET dcb    = 0

  sys(Sys_setst, 1)     // Enter kernel mode

  // The clock device (devid=-1) cannot be deleted.
  IF 2<=n<=devtab!0 DO dcb := devtab!n

  UNLESS dcb DO
  { result2 := 101      // Invalid device id
sawritef("KLIB: deletedev(%n) unknown device*n", devid); abort(999)
    sys(Sys_setst, 0)   // Return to user mode
    RESULTIS 0
  }

//sawritef("KLIB: deletedev(%n) calling sys(Sys_devcom ...) dcb=%n*n", devid, dcb)
  // Close down the device
  sys(Sys_devcom, dcb, Devc_destroy, 3333)
  // The above call does not return until it has finished with the DCB

  devtab!n := 0         // Clear the devtab entry

  IF Dcb_wkq!dcb DO
  { // There were packets still on it wkq
    LET tasktab = rtn_tasktab!rootnode 
    LET ctcb    = rtn_crntask!rootnode
    LET htcb    = ctcb
    LET pkt     = Dcb_wkq!dcb
    Dcb_wkq!dcb := 0

//sawritef("KLIB: deletedev(%n) with non empty wkq=%n*n", devid, Dcb_wkq!dcb)

    // Return each pkt to its client task setting res1 and res2 to -1

    WHILE pkt DO
    { LET next = pkt_link!pkt
      LET destid = pkt!pkt_id
      LET dtcb, p = ?, ?
//sawritef("KLIB: qpkt: the destination is a task*n")
      UNLESS 0 < destid <= tasktab!0 & tasktab!destid DO
      { // The packets task no longer exists
        sys(Sys_setst, 0)
        result2 := 101  // Bad destination task number
        RESULTIS 0
      }
 
      dtcb := tasktab!destid
      pkt_link!pkt := 0       // Fill in end-of-list marker
      pkt_id!pkt   := devid   // Fill in the device id
      pkt_res1!pkt := -1      // Fill in res1 = -1
      pkt_res2!pkt := -1      // Fill in res2 = -1

      TEST tcb_wkq!dtcb
      THEN { // The wkq was not empty -- so no scheduling necessary
             LET p = tcb_wkq!dtcb

             // Append pkt at end of wkq
             WHILE pkt_link!p DO p := pkt_link!p
             pkt_link!p := pkt
           }
      ELSE { // The wkq was empty
             tcb_wkq!dtcb := pkt  // Make a unit list
             tcb_state!dtcb := tcb_state!dtcb | #b0001  // Set the PKT bit
//sawritef("KLIB: qpkt: sent to a task that had no pkts*n")
             IF tcb_pri!dtcb > tcb_pri!htcb DO htcb := dtcb
           }
      pkt := next
    }

    UNLESS htcb=ctcb DO
    { // We must give control to a higher priority task
      sys(Sys_saveregs, @ tcb_regs!tcb) // Suspend the current task
      tcb_a!tcb  := dcb
      tcb_st!tcb := 0
      tcb_pc!tcb := retres
//sawritef("KLIB: deletedev: giving control to a higher priority task*n")
      srchwk(htcb)  // give control to the destination task
    }

    // Otherwise just return successfully from deletedev
  }

//sawritef("KLIB: deletedev(%n) => %n, successful*n", devid, dcb)
  sys(Sys_setst, 0)   // Return to user mode
  RESULTIS dcb
}


/*****************************************************************
*                                                                *
*                         GLOBIN(SEG)                            *
*                                                                *
* This function initialises the globals defined in the given     *
* segment. It returns -1, or 0 if an error is detected - an      *
* attempt to initialise a global beyond the upperbound given in  *
* GLOBSIZE. GLOBIN is defined in KLIB since it is called from    *
* the scheduler in ACTIV.                                        *
*                                                                *
*****************************************************************/

//AND globin(segl) = sys(Sys_globin, segl)

/*****************************************************************
*                                                                *
*                 RES := GETVEC(UPPERBOUND)                      *
*                                                                *
* Returns the BCPL pointer to a vector with at least the given   *
* upper bound. (In fact the upper bound is rounded up to the     *
* next even number) The word at offset -1 of the vector contains *
* the length of the store block and should not be touched by the *
* user. Runs at level 7 but returns to level 0 each time round   *
* the search loop, which can be lengthy.                         *
*                                                                *
* On return:                                                     *
*      RES ~= 0    OK                                            *
*      RES  = 0    Error                                         *
*                  RESULT2 = 103    Insufficient store           *
*                                                                *
* Abort 197        Block list corrupt                            *
*                                                                *
*****************************************************************/

AND getvec(upb) = sys(Sys_getvec, upb)

/*****************************************************************
*                                                                *
*                           FREEVEC(V)                           *
*                                                                *
* This BCPL callable routine frees the vector V, which should    *
* have been obtained from GETVEC. It aborts the task if an error *
* is detected. If the vector is zero the call has no effect      *
* No BCPL stack or Global vector are required.                   *
* It runs at any level.                                          *
*                                                                *
* FREEVEC must return via register R since this is assumed in    *
* ACTIV.                                                         *
*                                                                *
* Abort 197        Block list corrupt                            *
*                                                                *
*****************************************************************/

AND freevec(p) = sys(Sys_freevec, p)

/*****************************************************************
*                                                                *
*                           srchwk(tcb)                          *
*                                                                *
* This is the Tripos scheduler. Its argument is the              *
* highest priority tcb that could possibly run. It searches      *
* down the tcb list from that point until it finds a task to     *
* run, then transfers control to it appropriately.               *
* This function does not return normally since it just starts    *
* Cintcode execution from a new state, either starting a new     *
* task or resuming the execution of an interrupted task.         *
*                                                                *
*****************************************************************/

LET srchwk(tcb) BE
{ // srchwk never returns -- it always transfers control
  // by calling: sys(Sys_rti, ...)

//sawritef("KLIB: srchwk: tcb=%n id=%n state=%bA*n",
//                        tcb, tcb_taskid!tcb, tcb_state!tcb)

  SWITCHON tcb_state!tcb INTO
  { DEFAULT:     // The task list is corrupt
                 sawritef("KLIB: The task list is corrupt*n")
                 sawritef("KLIB: tcb=%n id=%n state=%b4*n",
                           tcb, tcb_taskid!tcb, tcb_state!tcb)
                 abort(999)

    CASE #b0010: // Run Held
    CASE #b0011: // Run Held with pkt
    CASE #b0110: // Wait Held
    CASE #b0111: // Wait Held with pkt
    CASE #b1010: // Interrupted Held
    CASE #b1011: // Interrupted Held with pkt
    CASE #b1110: // Dead Held
    CASE #b1111: // Dead Held with pkt
    CASE #b0100: // Wait
    CASE #b1100: // Dead
         // Search for another task to run.
         // Note that the Idle task (at the end of the task list)
         // is always ready to run.
         tcb := tcb_link!tcb
         LOOP

    CASE #b1101: // Dead with pkt
         // Activate a dead task. Give it a new stack and global
         // vector. Initialise its global vector then call start
         // leaving it to get the first pkt from the wkq.

//sawritef("KLIB: srchwk: CASE 1101*n")

       { LET stsiz = tcb_stsiz!tcb
         LET sbase = getvec(stsiz+6) // Allocate a new stack
         LET gbase = getvec(1000)    // and global vector

//sawritef("KLIB: srchwk: sbase=%n gbase=%n*n", sbase, gbase)

         UNLESS sbase & gbase DO
         { sawritef("Insufficient space to activate task %n*n",
                     tcb_taskid!tcb)
           IF sbase DO freevec(sbase)
           IF gbase DO freevec(gbase)
           tcb_state!tcb := #b1111     // DEAD HELD with PKT
           tcb := tcb_link!tcb
           LOOP                        // Find another task to run
         }

         sbase!0, gbase!0 := stsiz+6, 1000 // Info for starttask

         tcb_gbase!tcb := gbase 
         tcb_sbase!tcb := sbase

         // Setup the initial running environment for this task.
         tcb_a !tcb := 0          // A
         tcb_b !tcb := 0          // B
         tcb_c !tcb := 0          // C
         tcb_p !tcb := sbase<<2   // P the newly allocated stack
         tcb_g !tcb := gbase<<2   // G the newly allocated global vector
         tcb_st!tcb := 0          // Interrupts enabled
         tcb_pc!tcb := starttask  // PC
         tcb_count!tcb := -1      // Count set to infinity

         // Now enter starttask by pretending to be in
         // interrupted state.
       }

    CASE #b1000: // Interrupted
    CASE #b1001: // Interrupted with pkt
         // Transfer control to an interrupted task
         tcb_state!tcb := tcb_state!tcb & #b0001 // State 0000 or 0001

    CASE #b0000: // Run
    CASE #b0001: // Run with pkt
         // Resume execution of a task that previously lost control
         // to a higher priority task
         rtn_crntask!rootnode := tcb
//FOR i=0 TO 7 DO sawritef("R%n=%n*n", i, tcb!(tcb_regs+i))
         sys(Sys_rti, @ tcb_regs!tcb)


    CASE #b0101: // Wait with pkt
    // Transfer control to a task that now has the packet it
    // was waiting for (in taskwait).
        { LET pkt = tcb_wkq!tcb
          LET wkq = pkt_link!pkt
          tcb_wkq!tcb := wkq
          tcb_state!tcb := wkq -> #b0001, #b0000
          pkt_link!pkt := notinuse
          tcb_a!tcb := pkt // Put pkt in the result register
          rtn_crntask!rootnode := tcb
          sys(Sys_rti, @ tcb_regs!tcb)
        }
  }
} REPEAT


AND starttask(fn, size, c) BE
{ // This is run with st=0 ie interrupts are enabled, but
  // rootnode!rtn_crntask!tcb_active is FALSE until the coroutine
  // environment and globals are properly setup.
  LET p = @fn - 3
  LET g = @globsize
  LET seglist = ?
  LET stsiz = p!0 - 6
  LET tcb = rtn_crntask!rootnodeaddr // Not rootnode -- MR 26/7/04
  // The only initialised global is globsize (global 0).
  // Initialise the task's remaining globals.
  FOR i = 1 TO g!0 DO g!i := globword + i
  rootnode := rootnodeaddr // MR 26/7/04
  sys := rtn_sys!rootnode

  // Set all stack locations except the part currently in use
  FOR q = p+20 TO p + p!0 DO !q := stackword // MR 1/12/03

  // Now setup the coroutine environment
  currco           := p
  currco!co_pptr   := 0
  currco!co_parent := -1    // Mark as root coroutine.
  currco!co_list   := 0
  currco!co_fn     := starttask
  currco!co_size   := stsiz
  currco!co_c      := 0
  colist           := currco

  // Now initialise the remaining globals
  ///////tcb       := rtn_crntask!rootnode
  taskid    := tcb_taskid!tcb

  // Initialise the global functions. The order of initialisation
  // is important for the commands: newcli and run.
  // For CLI tasks, seglist!3 contains cli_init
  //           and  seglist!4 contains start belonging to CLI
  seglist  := tcb_seglist!tcb
  FOR i = 1 TO seglist!0 DO sys(Sys_globin, seglist!i)

// Cannot call sawritef any earlier!!
//sawritef("KLIB: starting task %n with stackbase %n*n", taskid, currco)

  // Now enter the main function of the task
  tcb_active!tcb := TRUE
  start(taskwait())
  tcb_active!tcb := FALSE

  // Return the task to DEAD state

  sys(Sys_setst, 1)

  tcb_state!tcb := tcb_wkq!tcb -> #b1101, // DEAD with PKT
                                  #b1100  // DEAD

  // The following code breaks the normal rules because
  // it runs in DEAD state and continues to use its own 
  // stack and global vectors even after returning them
  // to free store -- its ok because interrupts are disabled
  // and no space allocation is done until a normal running
  // environment is entered by the call of sys(Sys_rti,..)
  // in srchwk.

  freevec(tcb_gbase!tcb); tcb_gbase!tcb := 0
  freevec(tcb_sbase!tcb); tcb_sbase!tcb := 0
  srchwk(tcb)
}
