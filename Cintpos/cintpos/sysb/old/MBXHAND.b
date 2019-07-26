/*
This is the source of the MBX handler task that implements mailbox
streams.

A mailbox item is lines of text (typically terminated by '*n') held
in a FIFO queue associated with the mailbox. They can be sent by any
task having output stream attached to the mailbox, and can be removed
by any task having an input stream attached to the mailbox.

An empty mailbox is created the first time a stream is attached to
it, and deleted when its last attached stream closes provided its FIFO
is empty.

A mailbox item may not exceed 1024 bytes in length and the size of a
mailbox FIFO buffer is 4096 bytes.

Write operations are done in the order they are received, and possibly
block until there is sufficient buffer space available.

Read operations are done in the order they are received, and possibly
block until a mailbox item is available.

In the present implementation, reading from a mailbox stream cannot
yield endstreamch.

Implemented by Martin Richards (c) April 2002

*/

SECTION "MBXHAND"

GET     "libhdr"
GET     "manhdr"

MANIFEST {
 scbblen = 1024  // SCB buffer size in bytes
 mbxblen = 4096  // MBX buffer size in bytes
 namelen =   32  // Mailbox names must be less than 32 chars long

 mbx.link = 0    // Link to next mailbox
 mbx.rp          // Position of next char to read
 mbx.wp          // Position of next char to write
 mbx.refcount    // Count of SCBs referring to this mailbox
 mbx.rdq         // List of pkts waiting to read
 mbx.wrq         // List of pkts waiting to write
 mbx.namebase    // Place holding the mailbox name
 mbx.bufbase = mbx.namebase + namelen/bytesperword + 1 // Base of buffer 
 mbx.upb     = mbx.bufbase  + mbxblen/bytesperword + 1 
}

GLOBAL {
 mbxlist:ug      // List of mailboxes
}

LET start ( init_pkt ) BE
{ qpkt(init_pkt)  // Return startup pkt

  mbxlist := 0    // Initially there are no mailboxes

  // Main action loop
  { LET pkt,  res = taskwait(), ?
    LET type, scb = pkt!pkt.type, pkt!pkt.arg1

//sawritef("MBXHAND: received pkt %n type %n*n", pkt, type)
    SWITCHON type INTO
    { CASE Action_findinput: 
         { LET name = pkt!pkt.arg3
           //sawritef("MBXHAND: findinput scb %n file %s*n", scb, name)
           res := mbxfindstream(scb, name, mbxrdfn, 0)
           returnpkt(pkt, res, result2)
           //pr()
           LOOP
         }

      CASE Action_findoutput:
         { LET name = pkt!pkt.arg3
           //sawritef("MBXHAND: findoutput scb %n file %s*n", scb, name)
           res := mbxfindstream(scb, name, 0, mbxwrfn)
           returnpkt(pkt, res, result2)
           //pr()
           LOOP
         }

      CASE Action_close:
           //sawritef("MBXHAND: close scb %n*n", scb)
           res := mbxendfn(scb)
           returnpkt(pkt, res, result2)
           //pr()
           LOOP

      CASE Action_read:  // replenish buffer
         { LET mbx = scb!scb.fd
           //sawritef("MBXHAND: Action_read scb=%n mbx=%n*n", scb, mbx)
           // Attempt to read a mailbox item
           TEST mbx!mbx.rdq=0 & mbxreadfn(scb, mbx)
           THEN { // Read was successful, so return the pkt.
                  returnpkt(pkt, TRUE, 0)
                  // Deal with a pending write pkts, if any.
                  WHILE mbx!mbx.wrq
                  { LET pkt = mbx!mbx.wrq
                    LET scb = pkt!pkt.arg1
                    UNLESS mbxwritefn(scb, mbx) BREAK
                    // Pending write was successful, so
                    // remove it from wrq
                    mbx!mbx.wrq := !pkt
                    // and return it
                    !pkt := notinuse
                    returnpkt(pkt, TRUE, 0)
                    // Go round the loop again, since one read
                    // may have made room for several writes.
                  }
                  // Either the current write is blocked, or
                  // there are no pending writes.
                }
           ELSE { // No mailbox item available so put the pkt
                  // on the rdq
                  LET p = @mbx!mbx.rdq
                  //sawritef("MBXHAND: reading blocked scb %n*n", scb)
                  WHILE !p DO p := !p  // Find the end of rdq
                  !p, !pkt := pkt, 0   // Append the pkt
                }
           //pr()
           LOOP
         }

      CASE Action_write: // deplete buffer
         { LET mbx = scb!scb.fd
           //sawritef("MBXHAND: Action_write scb=%n mbx=%n*n", scb, mbx)
           // Attempt to write from the scb buffer into the mailbox
           TEST mbx!mbx.wrq=0 & mbxwritefn(scb, mbx)
           THEN { // Writing was successful, so return the pkt.
                  returnpkt(pkt, TRUE, 0)
                  // and deal with pending read pkts, if any.
                  WHILE mbx!mbx.rdq DO
                  { LET pkt = mbx!mbx.rdq
                    LET scb = pkt!pkt.arg1
                    UNLESS mbxreadfn(scb, mbx) BREAK
                    // Read was successful so remove from the queue
                    mbx!mbx.rdq := !pkt
                    // and return it
                    !pkt := notinuse
                    returnpkt(pkt, TRUE, 0)
                    // Try again since it is just possible that
                    // one write will release more than one reads
                  }
                }
           ELSE { // Writing was not successful, so just
                  // append the pkt on the wrq.
                  LET p = @mbx!mbx.wrq
                  //sawritef("MBXHAND: writing blocked scb %n*n", scb)
                  WHILE !p DO p := !p  // Find the end of wrq
                  !p, !pkt := pkt, 0   // Append the pkt
                }
           //pr()
           LOOP
         }

      DEFAULT:  // Unknown or unimplemented operation
           sawritef("MBXHAND: illegal op %n scb %n*n", type, scb)
           abort(306)
           returnpkt(pkt, 0, 0)
           LOOP
    }
  } REPEAT
}

AND mbxrdfn   (scb) = sendpkt(notinuse, scb!scb.task, Action_read,  0, 0, scb)
AND mbxwrfn   (scb) = sendpkt(notinuse, scb!scb.task, Action_write, 0, 0, scb)
AND mbxclosefn(scb) = sendpkt(notinuse, scb!scb.task, Action_close, 0, 0, scb)


AND mbxfindstream(scb, name, rdfn, wrfn) = VALOF
{ LET buf = getvec(scbblen/bytesperword)
  LET mbx = findmbx(name)

  UNLESS buf & mbx DO
  { IF buf DO freevec(buf)
    IF mbx DO closembx(mbx)
    RESULTIS 0
  }

  scb!scb.type    := scbt.mbx
  scb!scb.task    := taskid
  scb!scb.buf     := buf
  scb!scb.rdfn    := rdfn
  scb!scb.wrfn    := wrfn
  scb!scb.endfn   := mbxclosefn
  scb!scb.fd      := mbx
  scb!scb.pos     := 0
  scb!scb.end     := 0
  scb!scb.bufend  := scbblen

  RESULTIS 1
}

// mbxreadfn(scb, mbx) attempts to read bytes from the mailbox FIFO
//                     into the scb buffer.
//
// It returns:  TRUE  if successful, or
//              FALSE if the mailbox is empty.

AND mbxreadfn(scb, mbx)  = VALOF
{ LET buf    = scb!scb.buf
  LET end    = 0
  LET bufend = scb!scb.bufend
  LET mbxbuf = @mbx!mbx.bufbase
  LET rp, wp = mbx!mbx.rp, mbx!mbx.wp

  // wp=rp means the buffer is empty

//sawritef("MBXHAND: mbxrdfn scb %n rp=%n wp=%n*n", scb, rp, wp)

  IF rp=wp RESULTIS FALSE     // No mailbox item available

  UNTIL rp=wp | end>=bufend DO
  { LET ch = mbxbuf%rp
    rp := (rp+1) REM mbxblen
    buf%end := ch
    end := end+1
    IF ch='*n' | ch='*c' | ch='*p' | ch='*e' BREAK
  }

  mbx!mbx.rp := rp
//sawritef("MBXHAND: mbxrdfn scb %n rp=%n wp=%n*n", scb, rp, wp)
  scb!scb.pos, scb!scb.end := 0, end

  RESULTIS TRUE           // Successful return
}  

// mbxwritefn(scb) attempts to write bytes from the scb buffer into
//                 its mailbox FIFO.
//
// It returns:  TRUE  if successful, or
//              FALSE if there is insufficient room in the FIFO or
//                       there is an earlier write pending.

AND mbxwritefn(scb, mbx) = VALOF
{ LET buf    = scb!scb.buf
  LET pos    = scb!scb.pos
  LET mbxbuf = @mbx!mbx.bufbase
  LET rp, wp = mbx!mbx.rp, mbx!mbx.wp

  // wp is always >= rp  modulo mbxblen
  // wp=rp means the buffer is empty

//sawritef("MBXHAND: mbxwrfn scb %n pos %n rp=%n wp=%n*n", scb, pos, rp, wp)

  IF (rp - wp + mbxblen - 1) REM mbxblen < pos RESULTIS FALSE

  FOR i = 0 TO pos-1 DO
  { LET ch = buf%i
    mbxbuf%wp := ch
    wp := (wp+1) REM mbxblen
  }

  mbx!mbx.wp := wp
  scb!scb.pos := 0  // Reset the buffer position
  RESULTIS TRUE
}  

AND mbxendfn(scb) = VALOF
{ LET fd  = scb!scb.fd
  LET buf = scb!scb.buf
//sawritef("MBXHAND: mbxendfn scb %n*n", scb)
  IF buf DO freevec(buf)
  RESULTIS closembx(fd)
}

// findmbx(name) finds the mailbox with given name in the list mbxlist,
//               creating a new mailbox if necessary.
//
// It returns:   0  on failure
//         or:   a pointer to the mailbox.

AND findmbx(name) = VALOF
{ LET p = mbxlist

  IF name%0>namelen RESULTIS 0 // The mailbox name is too long

  WHILE p DO
  { UNLESS compstring(@p!mbx.namebase, name) DO
    { // The mailbox already exits
      p!mbx.refcount := p!mbx.refcount + 1
      RESULTIS p
    }
    p := p!mbx.link
  }

  // Make a new mailbox
  p := getvec(mbx.upb)
  UNLESS p RESULTIS 0          // Unable to allocate the space

  // Initialise all the mailbox fields
  FOR i = 0 TO mbx.upb DO p!i := 0
  p!mbx.refcount := 1
  FOR i = 0 TO name%0 DO (p+mbx.namebase)%i := name%i

  // Link the mailbox into the head of mbxlist
  p!mbx.link := mbxlist
  mbxlist    := p

  RESULTIS p                   // Successful return
}

// closembx(mbx) decrements the reference count and returns the mailbox
//               to free store if its FIFO is empty.
//
// It returns:  TRUE  if the mailbox was deleted
//              FALSE otherwise

AND closembx(mbx) = VALOF
{ LET p = @mbxlist
  LET count = mbx!mbx.refcount-1
  mbx!mbx.refcount := count

  UNLESS count<=0 & mbx!mbx.rp=mbx!mbx.wp RESULTIS FALSE

  // The mailbox is empty and no scbs refer to it so it must be deleted

  { LET q = !p
    IF q=mbx DO
    { !p := !mbx      // Remove the mailbox from the list
      freevec(mbx)    // and return its space
      RESULTIS TRUE
    }
    p := q
  } REPEAT

  sawritef("MBXHAND: closembx mailbox not in the list*n")
  abort(999)
  RESULTIS FALSE
}

AND pr() BE
{ LET p = mbxlist
  sawrch('*n')
  WHILE p DO
  { sawritef("Box: %s refcount: %n rp:%n wp:%n",
               p+mbx.namebase, p!mbx.refcount, p!mbx.rp, p!mbx.wp)
    IF p!mbx.rdq DO
    { LET q = p!mbx.rdq
      sawritef("  rdq:")
      WHILE q DO
      { sawritef(" %n", q!pkt.taskid)
        q := !q
      }
    }
    IF p!mbx.wrq DO
    { LET q = p!mbx.rdq
      sawritef("  wrq:")
      WHILE q DO
      { sawritef(" %n", q!pkt.taskid)
        q := !q
      }
    }
    p := !p
  }
  sawrch('*n')
}

