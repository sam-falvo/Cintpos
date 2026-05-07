// This is the Cintpos version of system dependent library DLIB

SECTION "DLIB"

GET "libhdr"
GET "manhdr"

MANIFEST {
  char_bs = 8
  buflen  = 4096 // Must equal the block size
}


// sysabort should be removed
LET sysabort(code, arg2) BE
{ sawritef("DLIB: sysabort(%n, %n) called*n", code, arg2)
  abort(999)
}

LET devicetask(name) = VALOF
{ // Takes a device name string and returns the task number of the
  // corresponding device handler.
  // The device name may be a filing system directory, in which case
  // a pointer to a shared directory lock is returned in result2?????

  LET v    = VEC 256/bytesperword
  LET dir  = 0//currentdir
  LET ptr  = splitname(v, ':', name, 1)
  LET task = Task_filehandler //?

//sawritef("DLIB: v=%s name=%s*n", v, name)
  TEST ptr=0 | ptr=2
  THEN TEST dir=0
       THEN { // No current directory - take root of standard file handler
              task := Task_filehandler
            }
       ELSE { task := Task_filehandler ////dir!lock.task
              IF ptr=2 DO dir := 0 // Root of this filing system (i.e. ':')
            }
  ELSE { // Look in assignments list
         LET ass = 0 //rootnode!rtn_info!Info_assignments
         // ass -> [link, task, dir, type, dev, name...]

         // First check whether it is a mail box
         UNLESS compstring(v, "MBX") DO
         { result2 := 0
           RESULTIS Task_mbxhandler
         }

         // Check whether it is an interactive TCP stream
         UNLESS compstring(v, "TCP") DO
         { result2 := 0
           RESULTIS Task_tcphandler
         }

         // Check whether it is a block oriented TCP stream
         UNLESS compstring(v, "NET") DO
         { result2 := 0
           RESULTIS Task_tcphandler
         }

         { // Loop
           UNLESS ass DO
           { result2 := 555 // Error_device_not_mounted
             //abort(999)
             RESULTIS Task_filehandler //0
           }

           UNLESS compstring(v, ass+Ass_name) DO
           { dir := ass!Ass_dir
             task := ass!Ass_task
             BREAK
           }

           ass := ass!Ass_link
         } REPEAT
       }

  result2 := dir
  RESULTIS task
}

AND findstream(name, id, path) = VALOF // MR 8/5/03
{ LET act = id=id_inscb     -> Action_findinput,
            id=id_outscb    -> Action_findoutput,
            id=id_appendscb -> Action_findappend,
            id=id_inoutscb  -> Action_findinoutput, 0
  LET scb = ?
  LET res, task = 0, ?
  LET prefix = VEC 31

//TEST path
//THEN sawritef("DLIB: findstream(%s, %n, %s)*n", name, id, path)
//ELSE sawritef("DLIB: findstream(%s, %n, 0)*n", name, id)
//sawritef("DLIB: currentdir=%n*n", currentdir)
//sawritef("DLIB: findstream: id=%n act=%n*n", id, act)

  scb := getvec(scb_upb)
  UNLESS scb RESULTIS 0

//sawritef("findstream: task %i2 allocating scb =%i6 for %s*n", taskid, scb, name)
  // Clear all scb fields
  FOR i = 0 TO scb_upb DO scb!i := 0

  scb!scb_id := id  // id_inscb, id_outscb, id_inoutscb or id_appendscb

  // Copy (truncated) stream name into the scb
  { LET len = name%0
    LET scbname = @scb!scb_name
    LET maxlen = scb_maxnamelen
    IF len>scb_maxnamelen DO len := scb_maxnamelen
    FOR i = 1 TO len DO scbname%i := name%i
    scbname%0 := len
  }

  IF compstring(name, "**")=0 DO
  { // Console stream -- ie keyboard or screen
//sawritef("DLIB: sending pkt to task=%n act=%n*n", consoletask, act)
    res := sendpkt(notinuse, consoletask, act, ?, ?, scb)
    UNLESS res DO { freevec(scb); RESULTIS 0 }
    RESULTIS scb
  }

  splitname(prefix, ':', name, 1)

  IF compstring(prefix, "NIL")=0 DO
  { // On reading always gives endstreamch
    // On writing always throws away the data
    scb!scb_wrfn := nilwrfn
//sawritef("DLIB: Opening stream to/from NIL:*n")
    RESULTIS scb 
  }

  IF compstring(prefix, "RAM")=0 DO
  { // Open a RAM stream.
    // It is always a read/write stream.
    // Its buffer is automatically extended as needed.
    // All stream buffers are allocated using getvec and
    // must be freed using freevec when the stream is closed.
    // Note that rewindstream, note, point and record access
    // all work with RAM streams.
    LET buf = getvec(1024/bytesperword) // Initial allocation
    UNLESS buf DO { freevec(scb); RESULTIS 0 }
    scb!scb_type    := scbt_ram
    scb!scb_id      := id_inoutscb
    scb!scb_buf     := buf
    scb!scb_pos     := 0     // Initial position
    scb!scb_end     := 0     // End of valid data
    scb!scb_bufend  := 1024  // Current size of buf

    scb!scb_wrfn    := ramwrfn // This expands the buffer
//sawritef("DLIB: Opening stream to/from RAM:*n")
    RESULTIS scb 
  }

  // Look in assignments list
  task := devicetask(name)
//sawritef("DLIB: devicetask returned %n*n", task)

  TEST task
  THEN { // task is the taskid of FH0, MBXHAND, TCPHAND etc
//sawritef("DLIB: sending pkt to task %n name=%s*n", task, name)
         res := sendpkt(notinuse, task, act,
                        ?, ?, scb, result2, name, currentdir, path)
       }
  ELSE { // See if there is a loadable FIND routine for this device
//sawritef("DLIB: findstream: not calling callseg(SYS:L.FIND...)*n")
         res := 0 //callseg("SYS:L.FIND", act, scb, name, currentdir,path)
         //IF res DO scb := res
       }

//sawritef("DLIB: findstream %s res %n  scb %n*n", name, res, scb)
  UNLESS res DO { freevec(scb); RESULTIS 0 }
  RESULTIS scb
}

AND ramwrfn(scb) = VALOF
{ // Replace the buffer by one of twice the size
  LET oldbufend = scb!scb_bufend
  LET oldbufupb = oldbufend/bytesperword
  LET oldbuf    = scb!scb_buf
  LET newbufend = oldbufend*2
  LET newbufupb = newbufend/bytesperword
  LET newbuf    = getvec(newbufupb)
  UNLESS newbuf RESULTIS FALSE
  FOR i = 0 TO oldbufupb DO newbuf!i := oldbuf!i
  scb!scb_buf, scb!scb_bufend := newbuf, newbufend
  freevec(oldbuf)
  RESULTIS TRUE
}

AND nilwrfn(scb) = VALOF
{ scb!scb_pos := 0 // Throw away the buffer contents (if any)
  RESULTIS TRUE
}

AND locatedir (dirname) = VALOF
{ LET len = dirname%0
  LET upb = len / bytesperword
  LET str = getvec(upb)
  UNLESS str RESULTIS 0
  FOR i = 0 TO len DO str%i := dirname%i
//sawritef("DLIB: locatedir(%s)*n", dirname)
  RESULTIS str
}
/*
AND deletefile (filename) = VALOF // Needs more work
{ LET task = Task_filehandler
  //writef("DLIB deletefile: %s*n", filename)
  RESULTIS sendpkt(notinuse, task, Action_deleteobject, 0, 0, filename)
}

AND renamefile (oldfile, newfile) = VALOF // Needs more work
{ LET task = Task_filehandler
  RESULTIS sendpkt(notinuse, task, Action_renameobject, 0, 0, oldfile, newfile)
}
*/

AND note(scb, posv) = VALOF
{ LET type = scb!scb_type
  AND task = scb!scb_task
  IF type=scbt_file DO
    RESULTIS sendpkt(-1, task, Action_note, 0, 0, scb, posv)
  // MR 29/7/04
  IF type=scbt_ram DO
  { posv!0 := 0
    posv!1 := scb!scb_pos
    RESULTIS TRUE
  }
  RESULTIS FALSE // Must be a disc file
}

AND point(scb, posv) = VALOF
{ LET type = scb!scb_type
  AND id   = scb!scb_id
  AND task = scb!scb_task

//sawritef("DLIB: point posv!0=%n posv!1=%n*n", posv!0, posv!1)
  IF type=scbt_file &  // Check that scb is a suitable stream
     (id=id_inscb | id=id_inoutscb) DO
    RESULTIS sendpkt(-1, task, Action_point, 0, 0, scb, posv)

  // MR 29/7/04
  IF type=scbt_ram & posv!0=0 & 0<=posv!1<=scb!scb_bufend DO
  { scb!scb_pos := posv!1
    RESULTIS TRUE
  }

  RESULTIS FALSE
}

AND endtask(seg) BE
{ unloadseg(seg)
  deletetask(taskid)
  abort(180)  // Should never reach this point
}

AND delay(msecs) = sendpkt(notinuse, -1, 0, // link device type
                           0, 0,            // res1 res2
                           msecs)           // arg1

AND delayuntil(days, msecs) = VALOF
{ MANIFEST { msecsperday = 24*60*60*1000 }

  { LET ds, ms, tks = ?, ?, ?
    datstamp(@ds)
    // Assume new dat format
    ds := days - ds
    ms := msecs - ms
    IF ms<0 DO ds, ms := ds-1, ms + msecsperday
    IF ds<0 RESULTIS 0
    IF ds=0 DO
    { delay(ms)
      RESULTIS 0
    }
    delay(1000)
  } 
}

AND returnpkt(pkt, res1, res2) = VALOF
{ pkt!pkt_link := notinuse
  pkt!pkt_res1 := res1
  pkt!pkt_res2 := res2
  RESULTIS qpkt(pkt)
}

AND initio() BE
{ cis, cos := 0, 0
  currentdir, consoletask := 0, Task_consolehandler
  returncode := 0
}

// Routine to set the task name. It copies the given name into the
// TCB of the current task.

AND set_process_name(name, a, b, c) BE
{ LET taskname = @rootnode!rtn_crntask!tcb_namebase
  LET oldin, oldout = cis, cos
  LET ramstream = findinoutput("RAM:")

  selectoutput(ramstream)
  writef(name, a,b,c)

//sawritef("DLIB: calling writef(ABC)*n", a,b,c)
//abort(1000)
//writef("ABC", a,b,c)
//abort(1000)
  rewindstream(ramstream)
  selectinput(ramstream)
//abort(1001)
  taskname%0 := 0
  FOR i = 1 TO 15 DO
  { LET ch = rdch()
    IF ch=endstreamch BREAK
    taskname%i := ch
    taskname%0 := i
  }
//sawritef("DLIB: set_process_name: taskname=%s*n", taskname)
//abort(1002)

  endstream(ramstream)
  cis := oldin
  cos := oldout
}

AND getremipaddr(scb) = VALOF
{ UNLESS scb!scb_type=scbt_tcp | scb!scb_type=scbt_net DO
  { result2 := 0
    RESULTIS 0
  }
  RESULTIS sendpkt(-1, scb!scb_task, Action_getremipaddr, 0,0, scb)
}

// Changed MR 23/11/01
// This is the default definition of sendpkt. It just sends the packet
// (using qpkt) and then waits for it to be returned (using taskwait).
// A task running in multi-event mode must replace sendpkt by a private
// version (normally called cosendpkt) which sends the packet and then
// suspends the current coroutine after putting the packet in pktlist
// so that it can be given to the suspended coroutine when the packet
// returns to the task.
// The functions gomultievent, findpkt and cosendpkt and the variable
// pktlist are now defined in application code and not in the standard
// Cintpos library.

AND sendpkt(link,id,type,r1,r2,a1,a2,a3,a4,a5,a6) = VALOF
{ UNLESS qpkt(@link) DO abort(181)

  { LET p = taskwait()
    // Safety check
    IF p = @link DO { result2 := r2; RESULTIS r1 }
    sawritef("Task %n: sendpkt received the wrong packet %n*n",
              taskid, p)
    FOR i = 0 TO 9 DO sawritef("p!%n = %n*n", i, p!i)
    abort(182, p) // Not the expected packet
    qpkt(p)       // Return the unexpected packet to its sender
  } REPEAT
}

// peercom sends writef(form, a, b, c, d) to a CLI on streamname
// and returns TRUE if the reply is ACK, FALSE otherwise.
AND peercom(streamname, reply_str, upb, form, a, b, c, d) = VALOF
{ LET oldin, oldout = input(), output()
  LET cliout, cliin = 0, 0
  LET remipaddr = 0
  LET res = FALSE

  IF streamname DO
  { FOR i = 1 TO 5 DO
    { cliout := findoutput(streamname)
      IF cliout BREAK
      delay(1000) // Delay for a second
    }

    //sawritef("DLIB: peercom: Waiting for the connection to be established*n")

    remipaddr := getremipaddr(cliout)

    UNLESS remipaddr DO
    { sawritef("DLIB: peercom: Failed to connect to %s*n", streamname)
      GOTO ret
    }

    sawritef("DLIB: peercom: Connection %s established, remipaddr=%x8*n",
              streamname, remipaddr)

    cliin  := findinput (streamname)
  }

  UNLESS cliout & cliin DO
  { UNLESS streamname DO streamname := "<unset>"
    sawritef("*nDLIB: peercom: unable to open peer CLI %s*n", streamname)
    GOTO ret
  }

//sawritef("DLIB: peercom: CLI streams opened to %s*n", streamname)
selectoutput(cliout)
selectinput(cliin)

sawritef("DLIB: peercom: sending ")
sawritef(form, a, b, c, d)

  // Send a CLI command line
  writef(form, a, b, c, d)
  deplete(cos)

  selectoutput(oldout)

//sawritef("DLIB: peercom: reading the reply from remote CLI*n")
  // Get the reply, if any, with 5 second timeout.
  settimeout(cliin, 5000)

  // Wait for reply from remote CLI
  reply_str%0 := 0
  FOR p = 1 TO upb DO
  { LET ch = rdch()
    IF ch<'*s' BREAK
    reply_str%0, reply_str%p := p, ch
  }

sawritef("DLIB: peercom: received '%s' from remote CLI*n", reply_str)
  res := TRUE

ret:
  endstream(cliin)
  endstream(cliout)

  RESULTIS res
}

