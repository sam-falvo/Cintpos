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

SECTION "MBXHAND"                   // Main control section

GET     "libhdr"
GET     "manhdr"

MANIFEST {
 scbblen = 1024  // SCB buffer size in bytes
 mbxblen = 4096  // MBX buffer size in bytes
 namelen =   32  // Mailbox names must be less than 32 chars long

 mbx_link = 0    // Link to next mailbox
 mbx_rp          // Position of next char to read
 mbx_wp          // Position of next char to write
 mbx_refcount    // Count of SCBs referring to this mailbox
 mbx_rdq         // List of pkts waiting to read
 mbx_wrq         // List of pkts waiting to write
 mbx_namebase    // Place holding the mailbox name
 mbx_bufbase = mbx_namebase + namelen/bytesperword + 1 // Base of buffer 
 mbx_upb     = mbx_bufbase  + mbxblen/bytesperword + 1 
}

GLOBAL {
 mbxlist:ug      // List of mailboxes
}

LET start ( init_pkt ) BE
{ set_process_name("MBX_Handler") // MR 3/2/03

  qpkt(init_pkt)  // Return startup pkt

  mbxlist := 0    // Initially there are no mailboxes

  // Main action loop
  { LET pkt,  res = taskwait(), ?
    LET type, scb = pkt!pkt_type, pkt!pkt_arg1

//sawritef("MBXHAND: received pkt %n type %n*n", pkt, type)
    SWITCHON type INTO
    { CASE Action_findinput: 
         { LET name = pkt!pkt_arg3
           //sawritef("MBXHAND: findinput scb %n file %s*n", scb, name)
           res := mbxfindstream(scb, name, mbxrdfn, 0)
           returnpkt(pkt, res, result2)
           //pr()
           LOOP
         }

      CASE Action_findoutput:
         { LET name = pkt!pkt_arg3
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
         { LET mbx = scb!scb_fd
           //sawritef("MBXHAND: Action_read scb=%n mbx=%n*n", scb, mbx)
           // Attempt to read a mailbox item
           TEST mbx!mbx_rdq=0 & mbxreadfn(scb, mbx)
           THEN { // Read was successful, so return the pkt.
                  returnpkt(pkt, TRUE, 0)
                  // Deal with a pending write pkts, if any.
                  WHILE mbx!mbx_wrq
                  { LET pkt = mbx!mbx_wrq
                    LET scb = pkt!pkt_arg1
                    UNLESS mbxwritefn(scb, mbx) BREAK
                    // Pending write was successful, so
                    // remove it from wrq
                    mbx!mbx_wrq := !pkt
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
                  LET p = @mbx!mbx_rdq
                  //sawritef("MBXHAND: reading blocked scb %n*n", scb)
                  WHILE !p DO p := !p  // Find the end of rdq
                  !p, !pkt := pkt, 0   // Append the pkt
                }
           //pr()
           LOOP
         }

      CASE Action_write: // deplete buffer
         { LET mbx = scb!scb_fd
           //sawritef("MBXHAND: Action_write scb=%n mbx=%n*n", scb, mbx)
           // Attempt to write from the scb buffer into the mailbox
           TEST mbx!mbx_wrq=0 & mbxwritefn(scb, mbx)
           THEN { // Writing was successful, so return the pkt.
                  returnpkt(pkt, TRUE, 0)
                  // and deal with pending read pkts, if any.
                  WHILE mbx!mbx_rdq DO
                  { LET pkt = mbx!mbx_rdq
                    LET scb = pkt!pkt_arg1
                    UNLESS mbxreadfn(scb, mbx) BREAK
                    // Read was successful so remove from the queue
                    mbx!mbx_rdq := !pkt
                    // and return it
                    !pkt := notinuse
                    returnpkt(pkt, TRUE, 0)
                    // Try again since it is just possible that
                    // one write will release more than one reads
                  }
                }
           ELSE { // Writing was not successful, so just
                  // append the pkt on the wrq.
                  LET p = @mbx!mbx_wrq
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

AND mbxrdfn   (scb) = sendpkt(notinuse, scb!scb_task, Action_read,  0, 0, scb)
AND mbxwrfn   (scb) = sendpkt(notinuse, scb!scb_task, Action_write, 0, 0, scb)
AND mbxclosefn(scb) = sendpkt(notinuse, scb!scb_task, Action_close, 0, 0, scb)


AND mbxfindstream(scb, name, rdfn, wrfn) = VALOF
{ LET buf = getvec(scbblen/bytesperword)
  LET mbx = findmbx(name)

  UNLESS buf & mbx DO
  { IF buf DO freevec(buf)
    IF mbx DO closembx(mbx)
    RESULTIS 0
  }

  scb!scb_type    := scbt_mbx
  scb!scb_task    := taskid
  scb!scb_buf     := buf
  scb!scb_rdfn    := rdfn
  scb!scb_wrfn    := wrfn
  scb!scb_endfn   := mbxclosefn
  scb!scb_fd      := mbx
  scb!scb_pos     := 0
  scb!scb_end     := 0
  scb!scb_bufend  := scbblen

  RESULTIS 1
}

// mbxreadfn(scb, mbx) attempts to read bytes from the mailbox FIFO
//                     into the scb buffer.
//
// It returns:  TRUE  if successful, or
//              FALSE if the mailbox is empty.

AND mbxreadfn(scb, mbx)  = VALOF
{ LET buf    = scb!scb_buf
  LET end    = 0
  LET bufend = scb!scb_bufend
  LET mbxbuf = @mbx!mbx_bufbase
  LET rp, wp = mbx!mbx_rp, mbx!mbx_wp

  // wp=rp means the buffer is empty

//sawritef("MBXHAND: mbxrdfn scb %n rp=%n wp=%n*n", scb, rp, wp)

  IF rp=wp RESULTIS FALSE     // No mailbox item available

  UNTIL rp=wp | end>=bufend DO
  { LET ch = mbxbuf%rp
    rp := (rp+1) REM mbxblen
    buf%end := ch
    end := end+1
    IF ch='*n' | ch='*c' | ch='*p' BREAK
  }

  mbx!mbx_rp := rp
//sawritef("MBXHAND: mbxrdfn scb %n rp=%n wp=%n*n", scb, rp, wp)
  scb!scb_pos, scb!scb_end := 0, end

  RESULTIS TRUE           // Successful return
}  

// mbxwritefn(scb) attempts to write bytes from the scb buffer into
//                 its mailbox FIFO.
//
// It returns:  TRUE  if successful, or
//              FALSE if there is insufficient room in the FIFO or
//                       there is an earlier write pending.

AND mbxwritefn(scb, mbx) = VALOF
{ LET buf    = scb!scb_buf
  LET pos    = scb!scb_pos
  LET mbxbuf = @mbx!mbx_bufbase
  LET rp, wp = mbx!mbx_rp, mbx!mbx_wp

  // wp is always >= rp  modulo mbxblen
  // wp=rp means the buffer is empty

//sawritef("MBXHAND: mbxwrfn scb %n pos %n rp=%n wp=%n*n", scb, pos, rp, wp)

  IF (rp - wp + mbxblen - 1) REM mbxblen < pos RESULTIS FALSE

  FOR i = 0 TO pos-1 DO
  { LET ch = buf%i
    mbxbuf%wp := ch
    wp := (wp+1) REM mbxblen
  }

  mbx!mbx_wp := wp
  scb!scb_pos := 0  // Reset the buffer position
  RESULTIS TRUE
}  

AND mbxendfn(scb) = VALOF
{ LET fd  = scb!scb_fd
  LET buf = scb!scb_buf
//sawritef("MBXHAND: mbxendfn scb %n*n", scb)
  //IF buf DO freevec(buf)  // buf is now freed by endstream()
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
  { UNLESS compstring(@p!mbx_namebase, name) DO
    { // The mailbox already exits
      p!mbx_refcount := p!mbx_refcount + 1
      RESULTIS p
    }
    p := p!mbx_link
  }

  // Make a new mailbox
  p := getvec(mbx_upb)
  UNLESS p RESULTIS 0          // Unable to allocate the space

  // Initialise all the mailbox fields
  FOR i = 0 TO mbx_upb DO p!i := 0
  p!mbx_refcount := 1
  FOR i = 0 TO name%0 DO (p+mbx_namebase)%i := name%i

  // Link the mailbox into the head of mbxlist
  p!mbx_link := mbxlist
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
  LET count = mbx!mbx_refcount-1
  mbx!mbx_refcount := count

  UNLESS count<=0 & mbx!mbx_rp=mbx!mbx_wp RESULTIS FALSE

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
               p+mbx_namebase, p!mbx_refcount, p!mbx_rp, p!mbx_wp)
    IF p!mbx_rdq DO
    { LET q = p!mbx_rdq
      sawritef("  rdq:")
      WHILE q DO
      { sawritef(" %n", q!pkt_taskid)
        q := !q
      }
    }
    IF p!mbx_wrq DO
    { LET q = p!mbx_rdq
      sawritef("  wrq:")
      WHILE q DO
      { sawritef(" %n", q!pkt_taskid)
        q := !q
      }
    }
    p := !p
  }
  sawrch('*n')
}

