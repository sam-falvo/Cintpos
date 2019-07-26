SECTION "KLIB"

GET "libhdr"

LET retres(a) = a  // return the result in the A register
                   // (retres -> RTN instruction)

LET abort(code,a,b,c) = VALOF
{ //sawritef("abort in KLIB called*n")
  RESULTIS sys(Sys_quit, code)
}

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
*                                                              *
***************************************************************/
 
LET qpkt(pkt) = VALOF
{ LET tcb, currid, destid = ?, ?, ?

  UNLESS pkt.link!pkt=notinuse DO
  { sawritef("KLIB: qpkt: pkt=%n link not = notinuse*n", pkt)
    abort(999)
    result2 := 101
    RESULTIS 0
  }
  sys(Sys_setst, 1)
  tcb := rtn.crntask!rootnode
  currid, destid := tcb.taskid!tcb, pkt.id!pkt
//sawritef("qpkt: sending pkt from %n to %n*n", currid, destid)

  IF destid=-1 DO             // Is destination the clock?
  { LET ticks = pkt.arg1!pkt  // Yes

    IF ticks>0 DO
    { // Insert the packet into the clock queue
      LET p = @ rtn.clwkq!rootnode
      LET cpkt = !p
      WHILE cpkt DO
      { LET dt = pkt.res1!cpkt
        IF ticks <= dt BREAK
        ticks := ticks - dt // MR 25/2/03
        p := cpkt
        cpkt := !p
      }

      // insert pkt in the queue at this point
      !p, !pkt := pkt, cpkt

      pkt.id!pkt := currid      // record the sender
      pkt.res1!pkt := ticks     // ticks to go
      IF cpkt DO                // correct cpkt ticks to go
        pkt.res1!cpkt := pkt.res1!cpkt - ticks

      sys(Sys_setst, 0)         // Enable interrupts
      RESULTIS 1                // Return from qpkt
    }

    // ticks was <=0 so return the packet back to the sender
    pkt.res1!pkt := ticks
    destid := currid
    currid := -1
    // fall through with destid>0
  }

  IF destid>0 DO                        // Is destination a task?
  { LET tasktab = rtn.tasktab!rootnode  // Yes
    LET dtcb, p = ?, ?
//sawritef("qpkt: the destination is a task*n")
    UNLESS destid<=tasktab!0 & tasktab!destid DO
    { sys(Sys_setst, 0)
      result2 := 101  // Bad destination task number
      RESULTIS 0
    }
 
    dtcb := tasktab!destid
    pkt.link!pkt := 0       // fill in end of list marker
    pkt.id!pkt := currid    // fill in return task id

    UNLESS tcb.wkq!dtcb DO
    { // The wkq was empty
      tcb.wkq!dtcb := pkt
      tcb.state!dtcb := tcb.state!dtcb + #b0001  // Set the PKT bit
//sawritef("qpkt: sent to a task that had no pkts*n")
      IF tcb.pri!dtcb > tcb.pri!tcb DO       
      { // The dest task has higher priority
        sys(Sys_saveregs, @ tcb.regs!tcb) // Suspend the current task
        tcb.st!tcb := 0
        tcb.pc!tcb := retres
//sawritef("qpkt: the dest task had higher priority*n")
        srchwk(dtcb)  // give control to the destination task
      }
      // otherwise just return successfully from qpkt
      sys(Sys_setst, 0)
      RESULTIS 1
    }

    // The wkq was not empty -- so no sceduling necessary
    p := tcb.wkq!dtcb

    // Put pkt at end of wkq
    WHILE pkt.link!p DO p := pkt.link!p
    pkt.link!p := pkt

    sys(Sys_setst, 0)
    RESULTIS 1           // Return from qpkt
  }

  IF destid<-1 DO                     // Was the destination a device?
  { LET devtab = rtn.devtab!rootnode  // Yes
    LET id, dcb, p = -destid, ?, ?
//IF destid=-3 DO sawritef("qpkt: to device %n ch %x2*n", destid, pkt.arg1!pkt)
    UNLESS 2<=id<=devtab!0 & devtab!id DO
    { result2 := 101       // Bad device identifier
      sys(Sys_setst, 0)
      RESULTIS 0
    }
    dcb := devtab!id
    pkt.link!pkt := 0       // fill in end of list marker
    pkt.id!pkt := currid    // fill in return task id
    IF Dcb_wkq!dcb DO
    { // Append the packet to a non empty wkq.
      LET p = Dcb_wkq!dcb

      UNTIL pkt.link!p=0 DO p := pkt.link!p
      pkt.link!p := pkt

      sys(Sys_setst, 0)
      RESULTIS 1
    }

    // The device had an empty wkq so call its start function
    Dcb_wkq!dcb := pkt
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
  THEN q := @ rtn.clwkq!rootnode
  ELSE TEST id>0
       THEN { LET tasktab = rtn.tasktab!rootnode
              IF 1<=id<=tasktab!0 DO tcb := tasktab!id
              UNLESS tcb DO { result2 := 101
sawritef("KLIB: dqpkt: tcb for task %n not found*n", id)
                              sys(Sys_unlockirq)
                              sys(Sys_setst, 0)
                              RESULTIS 0
                            }
              q := @ tcb.wkq!tcb
            }
       ELSE { LET devtab, devid = rtn.devtab!rootnode, -id
              IF 1<=devid<=devtab!0 DO dcb := devtab!devid
              UNLESS dcb DO { result2 := 101
sawritef("KLIB: dqpkt: dcb for device %n not found*n", id)
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

//sawritef("KLIB: dqpkt: pkt=%n id=%n q=%n tcb=%n dcb=%n*n",pkt,id,q,tcb,dcb)
//abort(1000)

  WHILE q & !q~=pkt DO q := !q // search for pkt

  UNLESS q DO
  { // The packet was not found
    // So look in the current task's wkq, unless already done
    LET crntcb = rtn.crntask!rootnode
sawritef("KLIB: dqpkt: pkt=%n was not found on wkq id=%n*n", pkt, id)
    UNLESS tcb=crntcb DO { id := tcb.taskid!crntcb; GOTO rep }
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
         LET npkt = pkt!pkt.link
         pkt!pkt.link :=  notinuse
         !q := npkt
         IF npkt DO pkt.arg1!npkt := pkt.arg1!npkt + pkt.arg1!pkt
       }
  ELSE TEST id>0
       THEN { LET npkt = pkt.link!pkt
              pkt!pkt.link :=  notinuse
              !q := npkt 
              UNLESS tcb.wkq!tcb DO
                tcb.state!tcb := tcb.state!tcb & #b1110
            }
       ELSE { LET npkt = pkt.link!pkt
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

  pkt.link!pkt := notinuse
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
  tcb := rtn.crntask!rootnode
  pkt := tcb.wkq!tcb

  IF pkt DO
  { // There is a pkt on the wkq
    LET npkt = pkt.link!pkt
    tcb.wkq!tcb := npkt
    UNLESS npkt DO tcb.state!tcb := #b_0000 // change from 0001 to 0000
    sys(Sys_setst, 0)        // Enable interrupts
    pkt.link!pkt := notinuse
    RESULTIS pkt
  }

  // no pkt on wkq so must change to WAIT state
  tcb.state!tcb := #b0100
  sys(Sys_saveregs, @ tcb.regs!tcb)
  tcb.st!tcb := 0            // Ensure interrupts enabled when resuming
  tcb.pc!tcb := retres       // Resume on a RTN instruction

  srchwk(tcb.link!tcb)       // Give control to a lower priority task

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
*    RES ~= 0    OK                                            *
*    RES  = 0    Error                                         *
*                RESULT2 = 101   Bad taskid                    *
*                RESULT2 = 110   Task already held             *
*                                                              *
***************************************************************/

LET hold(id) = VALOF
{ LET tasktab, tcb, state = ?, 0, ?
  sys(Sys_setst, 1)
  tasktab := rtn.tasktab!rootnode
  IF 1<=id<=tasktab!0 DO tcb := tasktab!id
  UNLESS tcb DO { result2 := 101
                  sys(Sys_setst, 0)
                  RESULTIS 0
                }
  state := tcb.state!tcb
  UNLESS (state & #b0010) = 0 DO // already held
  { result2 := 110
    sys(Sys_setst, 0)
    RESULTIS 0
  }
  tcb.state!tcb := state + #b0010 // set the hold bit
  IF tcb=rtn.crntask!rootnode DO  // holding self
  { sys(Sys_saveregs, @ tcb.regs!tcb)
    tcb.st!tcb := 0
    tcb.a!tcb  := tcb
    tcb.pc!tcb := retres
    srchwk(tcb.link!tcb)  // give control to the next task
  }

  sys(Sys_setst, 0)
  RESULTIS tcb
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
  tasktab := rtn.tasktab!rootnode
  ctcb    := rtn.crntask!rootnode
  IF 1<=id<=tasktab!0 DO dtcb := tasktab!id
  UNLESS dtcb DO { result2 := 101
                   sys(Sys_setst, 0)
                   RESULTIS 0
                 }

  tcb.state!dtcb := tcb.state!dtcb & #b1101 // clear the hold bit

  IF tcb.pri!dtcb > tcb.pri!ctcb DO
  { // Releasing a higher pri task, so suspend self
    sys(Sys_saveregs, @ tcb.regs!ctcb)
    tcb.st!ctcb := 0
    tcb.a!ctcb  := dtcb
    tcb.pc!ctcb := retres
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
{ LET tcb = rtn.crntask!rootnode
  LET res = ?
  sys(Sys_setst, 1)
  res := tcb.flags!tcb
  tcb.flags!tcb := res & ~flags    // clear specified flags
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
*    RES  = 0    Error                                          *
*                RESULT2 = 101    Invalid id                    *
*                                                               *
****************************************************************/
 
LET setflags(id, flags) = VALOF
{ LET tasktab, dtcb = ?, 0
  sys(Sys_setst, 1)
  tasktab := rtn.tasktab!rootnode

  IF 1<=id<=tasktab!0 DO dtcb := tasktab!id
  UNLESS dtcb DO { result2 := 101
                   sys(Sys_setst, 0)
                   RESULTIS 0
                 }

  tcb.flags!dtcb := tcb.flags!dtcb | flags // Set specified flags

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
{ LET tasktab = rtn.tasktab!rootnode
  LET id      = 0
  LET p       = ?
  LET seglist = ?
  LET tcb     = ?

  sys(Sys_setst, 1)  // MR 11/3/03

  // Check the priority is ok
  tcb := rtn.tcblist!rootnode
  WHILE tcb DO
  { IF tcb.pri!tcb=pri DO { result2 := 102; GOTO ret }
    tcb := tcb.link!tcb
  }

  // Find a suitable task identifier
  FOR i = 1 TO tasktab!0 UNLESS tasktab!i DO { id := i; BREAK  }
  UNLESS id DO { result2 := 105; GOTO ret }

  // Allocate the segment table
  seglist := getvec(segl!0)
  UNLESS seglist DO { id, result2 := 0, 103; GOTO ret }
  FOR i = 0 TO segl!0 DO seglist!i := segl!i

  // Allocate the task contol block
  tcb := getvec(tcb.upb)
  UNLESS tcb DO { freevec(seglist)
                  id, result2 := 0, 103
                  GOTO ret
                }

  FOR i = 0 TO tcb.upb DO tcb!i := 0

  tcb.taskid!tcb   := id
  tcb.pri!tcb      := pri
  tcb.state!tcb    := #b1100 // DEAD state
  tcb.stsiz!tcb    := stacksize
  tcb.seglist!tcb  := seglist

  tasktab!id := tcb

  // Insert the new TCB into the tcblist.
  { LET t = !p
    WHILE t & tcb.pri!t > pri DO { p := t; t := !p }
    !tcb := t
    !p := tcb
    RESULTIS id
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
{ LET tasktab = rtn.tasktab!rootnode
  LET ctcb = rtn.crntask!rootnode
  LET dtcb, p = 0, ?

  sys(Sys_setst, 1)

  IF 1<=id<=tasktab!0 DO dtcb := tasktab!id
  IF dtcb=0 DO { result2 := 101
                 sys(Sys_setst, 0)
                 RESULTIS 0
               }

  IF tcb.wkq!dtcb |
     (tcb.state!dtcb & #b1100) ~= #b1100 & dtcb ~= ctcb DO
  { result2 := 108
    sys(Sys_setst, 0)
    RESULTIS 0
  }

  tasktab!id := 0  // clear the tasktab entry

  p := @ rootnode!rtn.tcblist
  UNTIL !p=dtcb DO p := !p // Find the tcb in the tcb list
  !p := !dtcb              // and remove it from the list

  safreevec(tcb.seglist!dtcb) // Free its segment list
  safreevec(dtcb)          // and free the tcb

  IF dtcb=rootnode!rtn.crntask DO
  { // We are in a dangerous state. We have already deleted our
    // own segl and tcb but are still using the global vector
    // and stack. We must return these and then enter the scheduler.
    // This is ok since interrupts are disabled and there is no
    // chance that another task can run, so nothing else will
    // allocate or free store until srchwk has run.
    safreevec(ctcb!tcb.sbase)
    safreevec(ctcb!tcb.gbase)
    srchwk(!p)  // !p is the next lower priority task
  }

  // We were deleting some other task
  sys(Sys_setst, 0)
  RESULTIS -1
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
{ LET tasktab = rtn.tasktab!rootnode
  LET ctcb    = rtn.crntask!rootnode
  LET dtcb, p, q = 0, ?, ?
  LET cpri, opri = ?, ?

  sys(Sys_setst, 1)

  IF 1<=id<=tasktab!0 DO dtcb := tasktab!id
  IF dtcb=0 DO { result2 := 101
                 sys(Sys_setst, 0)
                 RESULTIS 0
               }

  opri := tcb.pri!dtcb  // Remember the old priority

  IF pri<=0 DO {
prierr:          result2 := 102
                 sys(Sys_setst, 0)
                 RESULTIS 0
               }

  p := @ rootnode!rtn.tcblist
  q := p
  UNTIL !p=dtcb DO p := !p // Find the tcb in the tcb list
  !p := !dtcb              // and remove from the list

  { LET t = !q  // Find the new position in the tcb list
    LET tpri = tcb.pri!t
    IF tpri=pri DO { !p := dtcb // put it back in the tcb list
                     GOTO prierr
                   }
    IF tpri<pri BREAK
    q := t
  } REPEAT

  !dtcb := !q              // link it into the list
  !q := dtcb
  tcb.pri!dtcb := pri      // give it the new priority

  cpri := tcb.pri!ctcb     // Find pri of crntask

  // Don't bother to optimise the scheduling

  sys(Sys_saveregs, @ tcb.regs!ctcb) // Save our own registers
  tcb.a !ctcb := TRUE // Set to return with non zero result
  tcb.st!ctcb := 0    // with interrupts enabled
  tcb.pc!ctcb := retres
  srchwk(rootnode!rtn.tcblist)  // Search the entire tcb list
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
  LET devtab = rtn.devtab!rootnode
  sys(Sys_setst, 1)   // Enter kernel mode
  FOR i = 1 TO devtab!0 UNLESS devtab!i DO
  { devid := i
    BREAK
  }
  TEST devid
  THEN { Dcb_devid!dcb := -devid
         TEST sys(Sys_devcom, dcb, Devc_create, 1111)
         THEN devtab!devid := dcb
         ELSE result2, devid := 106, 0
       }
  ELSE  result2, devid := 104, 0
  sys(Sys_setst, 0)   // Return to user mode
//sawritef("KLIB: createdev(%n) => %n*n", dcb, -devid)
  RESULTIS -devid
}

/*****************************************************************
*                   DCB := DELETEDEV(DEVID)                      *
*                                                                *
* This function deletes a device, which must have an empty WKQ.  *
* It closes down the device, and deallocates the device id, but  *
* does not return the DCB to free store (just as createdev did   *
* not allocate the DCB.                                          *
* On return:                                                     *
*    DCB = BCPL ptr to the DCB (~=0)                             *
*    DCB = 0    Error                                            *
*               RESULT2 = 101      Invalid id                    *
*               RESULT2 = 107      Work queue not empty          *
*                                                                *
*****************************************************************/

LET deletedev(devid) = VALOF
{ LET devtab = rtn.devtab!rootnode
  LET n      = -devid
  LET dcb    = 0

  sys(Sys_setst, 1)     // Enter kernel mode

  // The clock device (devid=-1) cannot be deleted.
  IF 2<=n<=devtab!0 DO dcb := devtab!n
  UNLESS dcb DO
  { result2 := 101      // Invalid id
sawritef("KLIB: deletedev(%n) unknown device*n", devid); abort(999)
    sys(Sys_setst, 0)   // Return to user mode
    RESULTIS 0
  }

  IF Dcb_wkq!dcb DO
  { result2 := 107      // Wkq not empty
sawritef("KLIB: deletedev(%n) non empty wkq=%n*n", devid, Dcb_wkq!dcb); abort(999)
    sys(Sys_setst, 0)   // Return to user mode
    RESULTIS 0
  }

//sawritef("KLIB: deletedev(%n) calling sys(Sys_devcom ...) dcb=%n*n", devid, dcb)
  sys(Sys_devcom, dcb, Devc_destroy, 3333)
  // The above call does not return until it has finished with the DCB

  devtab!n := 0         // Clear the devtab entry

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

AND getvec(upb) = sagetvec(upb)

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

AND freevec(p) = safreevec(p)

/*****************************************************************
*                                                                *
*                           srchwk(tcb)                          *
*                                                                *
* This is the Tripos scheduler. Its argument is the              *
* highest priority tcb that could possibly run. It searches      *
* down the tcb list from that point until it finds a task to     *
* run, then transfers control to it appropriately                *
*                                                                *
*****************************************************************/


LET srchwk(tcb) BE
{ // srchwk never returns -- it always transfers control
  // by calling: sys(Sys_rti, ...)

  SWITCHON tcb.state!tcb INTO
  { DEFAULT:     // The task list is corrupt
                 sawritef("KLIB: The task list is corrupt*n")
                 sawritef("KLIB: tcb=%n id=%n state=%n*n",
                           tcb, tcb.taskid!tcb, tcb.state!tcb)
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
         // Search for another task to run
         // The Idle task (at the end of the task list) is 
         // always ready to run.
         tcb := tcb.link!tcb
         LOOP

    CASE #b1101: // Dead with pkt
         // Activate a dead task. Give it a stack and global vector,
         // Initialise the global vector then call start leaving it
         // to get the first pkt from the wkq

       { LET stsiz = tcb.stsiz!tcb
         LET sbase = sagetvec(stsiz+6) // Allocate a stack
         LET gbase = sagetvec(1000)    // and global vector

         UNLESS sbase & gbase DO
         { sawritef("Insufficient space to activate task %n*n",
                     tcb.taskid!tcb)
           IF sbase DO freevec(sbase)
           IF gbase DO freevec(gbase)
           tcb.state!tcb := #b1111    // DEAD HELD with PKT
           tcb := tcb.link!tcb
           LOOP                       // Find another task to run
         }

         sbase!0, gbase!0 := stsiz+6, 1000 // Info for starttask

         tcb.gbase!tcb := gbase 
         tcb.sbase!tcb := sbase

         // Setup the initial running environment for this task 
         tcb.a !tcb := 0          // A
         tcb.b !tcb := 0          // B
         tcb.c !tcb := 0          // C
         tcb.p !tcb := sbase<<2   // P the newly allocated stack
         tcb.g !tcb := gbase<<2   // G the newly allocated global vector
         tcb.st!tcb := 0          // Interrupts enabled
         tcb.pc!tcb := starttask  // PC
         tcb.count!tcb := -1      // Count set to infinity

         // Now enter starttask by pretending to be interrupted
       }

    CASE #b1000: // Interrupted
    CASE #b1001: // Interrupted with pkt
         // Transfer control to an interrupted task
         tcb.state!tcb := tcb.state!tcb & #b0001

    CASE #b0000: // Run
    CASE #b0001: // Run with pkt
         // Resume execution of a task that previously lost control
         // to a higher priority task
         rtn.crntask!rootnode := tcb
//FOR i=0 TO 7 DO sawritef("R%n=%n*n", i, tcb!(tcb.regs+i))
         sys(Sys_rti, @ tcb.regs!tcb)


    CASE #b0101: // Wait with pkt
    // Transfer control to a task that now has the packet it
    // was waiting for (in taskwait).
        { LET pkt = tcb.wkq!tcb
          LET wkq = pkt.link!pkt
          tcb.wkq!tcb := wkq
          tcb.state!tcb := wkq -> #b0001, #b0000
          pkt.link!pkt := notinuse
          tcb.a!tcb := pkt // Put pkt in the result register
          rtn.crntask!rootnode := tcb
          sys(Sys_rti, @ tcb.regs!tcb)
        }
  }
} REPEAT


AND starttask(fn, size, c) BE
{ // This is run with st=0 ie interrupts are enabled, but
  // rootnode!rtn.crntask!tcb.active is FALSE until the coroutine
  // environment and globals are properly setup.
  LET p = @fn - 3
  LET g = @globsize
  LET seglist = ?
  LET stsiz = p!0 - 6
  LET tcb = rtn.crntask!rootnode
  // The only initialised global is globsize (global 0).
  // Initialise the task's remaining globals.
  FOR i = 1 TO g!0 DO g!i := globword + i
  sys := rtn.sys!rootnode

  // Set all stack locations except the part currently in use
  FOR q = p+20 TO p + p!0 DO !q := stackword

  // Now setup the coroutine environment
  currco           := p
  currco!co.pptr   := 0
  currco!co.parent := -1    // Mark as root coroutine.
  currco!co.list   := 0
  currco!co.fn     := starttask
  currco!co.size   := stsiz
  currco!co.c      := 0
  colist           := currco

  // Now initialise the remaining globals
  ///////tcb       := rtn.crntask!rootnode
  taskid    := tcb.taskid!tcb

  // Initialise the global functions. The order of initialisation
  // is important for the commands: newcli and run.
  // For CLI tasks, seglist!3 contains cli.init
  //           and  seglist!4 contains start belonging to CLI
  seglist  := tcb.seglist!tcb
  FOR i = 1 TO seglist!0 DO sys(Sys_globin, seglist!i)

//// Cannot call sawritef any earlier!!
//sawritef("KLIB: starting task with stackbase %n*n", currco)

  // Now enter the main function of the task
  tcb.active!tcb := TRUE
  start(taskwait())
  tcb.active!tcb := FALSE

  // Return the task to DEAD state

  sys(Sys_setst, 1)

  tcb.state!tcb := tcb.wkq!tcb -> #b1101, // DEAD with PKT
                                  #b1100  // DEAD

  // The following code breaks the normal rules because
  // it runs in DEAD state and continues to use its own 
  // stack and global vectors even after returning them
  // to free store -- its ok because interrupts are disabled
  // and no space allocation is done until a normal running
  // environment is entered by the call of sys(Sys_rti,..)
  // in srchwk.

  freevec(tcb.gbase!tcb); tcb.gbase!tcb := 0
  freevec(tcb.sbase!tcb); tcb.sbase!tcb := 0
  srchwk(tcb)
}
