SECTION "FH0"

GET     "libhdr"
GET     "manhdr"

MANIFEST { buflen = 4096 // Must equal the block size
}

LET start ( init_pkt ) BE
{ qpkt(init_pkt)  // return startup pkt

  { LET pkt  = taskwait()
    LET type = pkt ! pkt.type

//sawritef("FH0: received pkt %n type %n*n", pkt, type)
    SWITCHON type INTO
    { CASE Action_findinput: 
           //sawritef("FH0: findinput scb %n file %s*n",
           //                         pkt!pkt.arg1, pkt!pkt.arg3)
           fh0findinput(pkt, pkt!pkt.arg1, pkt!pkt.arg3, pkt!pkt.arg4, pkt!pkt.arg5)
           LOOP

      CASE Action_findoutput:
           //sawritef("FH0: findoutput scb %n file %s*n",
           //                          pkt!pkt.arg1, pkt!pkt.arg3)
           fh0findoutput(pkt, pkt!pkt.arg1, pkt!pkt.arg3, pkt!pkt.arg4)
           LOOP

      CASE Action_findinoutput:
           //sawritef("FH0: findinoutput scb %n file %s*n",
           //                          pkt!pkt.arg1, pkt!pkt.arg3)
           fh0findinoutput(pkt, pkt!pkt.arg1, pkt!pkt.arg3, pkt!pkt.arg4)
           LOOP

      CASE Action_closeinput :
      CASE Action_closeoutput:
      CASE Action_closeinoutput:
      CASE Action_close:
         { LET scb = pkt!pkt.arg1
           //sawritef("FH0: close scb %n*n", scb)
           fh0endfn(pkt, scb)
           LOOP
         }

      CASE Action_deleteobject:
         { LET file = pkt!pkt.arg1
           //sawritef("FH0: deleteobject %s*n", file)
           returnpkt(pkt, sadeletefile(file), 0)
           LOOP
         }

      CASE Action_renameobject:
         { LET oldfile = pkt!pkt.arg1
           LET newfile = pkt!pkt.arg2
           //sawritef("FH0: renameobject %s as %s*n", oldfile, newfile)
           returnpkt(pkt, sarenamefile(oldfile, newfile), 0)
           LOOP
         }

      CASE Action_read:  // replenish buffer
         { LET scb, res = pkt!pkt.arg1, ?
           //IF pkt!pkt.id=8 DO
//             sawritef("FH0: Action_read scb=%n pkt=%n from=%n*n",
//               scb, pkt, pkt!pkt.id)
           res := fh0readfn(scb)
           returnpkt(pkt, res, result2)
           LOOP
         }

      CASE Action_write: // deplete buffer
         { LET scb, res = pkt!pkt.arg1, ?
           //IF pkt!pkt.id=8 DO
//             sawritef("FH0: Action_write scb=%n pkt=%n from %n*n",
//               scb, pkt, pkt!pkt.id)
           res := fh0writefn(scb)
           returnpkt(pkt, res, result2)
           LOOP
         }

      CASE Action_note: // note(scb, posv)
         { LET scb = pkt!pkt.arg1
           LET posv = pkt!pkt.arg2
           returnpkt(pkt, fh0note(scb, posv), 0)
           LOOP
         }

      CASE Action_point: // point(scb, posv)
         { LET scb = pkt!pkt.arg1
           LET posv = pkt!pkt.arg2
//sawritef("FH0: Action_point scb=%n pkt=%n from=%n*n",
//        scb, pkt, pkt!pkt.id)
           returnpkt(pkt, fh0point(scb, posv), 0)
           LOOP
         }

      DEFAULT:  // Unknown or unimplemented operation
           sawritef("FH0: unknown op %n scb %n*n",
                     pkt!pkt.type, pkt!pkt.arg1)
           abort(306)
           LOOP
    }
  } REPEAT
}

// fh0rdfn is placed in the scb and is called by replish in the client task.
// It sends a packet to FH0 which then calls fh0readfn to do the replenishment.
AND fh0rdfn(scb) = sendpkt(notinuse, scb!scb.task, Action_read, 0, 0, scb)

// fh0wrfn is placed in the scb and is called by deplete in the client task.
// It sends a packet to FH0 which then calls fh0writefn to do the depletion.
AND fh0wrfn(scb) = sendpkt(notinuse, scb!scb.task, Action_write, 0, 0, scb)

// fh0closefn is placed in the scb and is called by endstream in the client task.
// It sends a packet to FH0 which then calls fh0endn to close the file.
AND fh0closefn(scb) = sendpkt(notinuse, scb!scb.task, Action_close, 0, 0, scb)

AND trfilename(name, currdir, filename) BE
{ LET p = 0
  IF currdir & name%0 & name%1~='/' & name%1~='\' DO
  { LET len = currdir%0
    LET lastch = currdir%len
    IF lastch='/' | lastch=':' DO len := len-1
    IF len DO
    { FOR i = 1 TO len DO { p :=  p+1; filename%p := currdir%i }
      p := p+1
      filename%p := '/'
    }
  }
  FOR i = 1 TO name%0 DO { p :=  p+1; filename%p := name%i }
  filename%0 := p
  FOR i = 1 TO p IF filename%i=':' DO filename%i := '/'
//TEST currdir
//THEN sawritef("FH0: trfilename %s %s => %s*n", currdir, name, filename)
//ELSE sawritef("FH0: trfilename %s => %s*n", name, filename)
}

AND fh0findinput(pkt, scb, name, currdir, path) BE
{ LET fp, buf, res1, res2 = 0, 0, 1, 0
  LET filesize = 0
  LET filename = VEC 50
  trfilename(name, currdir, filename)
//IF path DO 
//  sawritef("FH0: findinput name=%s filename=%s path=%s*n", name, filename, path)
  // open the file for input
  fp := sys(Sys_openread, filename, path)
  IF fp=0 DO
  { res1, res2 := 0, 100
    GOTO ret
  }

  filesize :=sys(Sys_filesize, fp)

  // allocate a buffer
  buf := sagetvec(buflen/bytesperword)
  IF buf=0 DO
  { sys(Sys_close, fp) // First close the file
    res1, res2 := 0, 101
    GOTO ret
  }
//sawritef("FH0: fh0findinput scb %n fp %n buf %n*n", scb, fp, buf)
    
  scb!scb.type    := scbt.file
  scb!scb.task    := taskid
  scb!scb.buf     := buf
  scb!scb.rdfn    := fh0rdfn
  scb!scb.wrfn    := 0  // An input stream cannot be depleted
  scb!scb.endfn   := fh0closefn
  scb!scb.fd      := fp
  scb!scb.bufend  := buflen
  scb!scb.write   := FALSE   // No data waiting to be written
  scb!scb.blength := buflen  // MR 15/3/02
  scb!scb.block   := 0       // MR 29/7/02
  scb!scb.lblock  := filesize/buflen //+ 1  // MR 16/4/02 MR 29/7/02
  scb!scb.ldata   := filesize REM buflen  // MR 16/4/02
//sawritef("fh0findinput: lblock=%n*n", scb!scb.lblock)

  // Initialise the buffer by reading the first block
  fh0getbuf(scb)
  res2 := result2

ret:
  pkt!pkt.res1, pkt!pkt.res2 := res1, res2
//sawritef("FH0: returning pkt %n  res1 %n  res2 %n*n", pkt, res1, res2)
  qpkt(pkt)
}

AND fh0findoutput(pkt, scb, name, currdir) BE
{ LET fp, buf, res1, res2 = 0, 0, 1, 0
  LET filename = VEC 50
  trfilename(name, currdir, filename)

  // open the file for output
  fp := sys(Sys_openwrite, filename)
  UNLESS fp DO
  { res1, res2 := 0, 100
    GOTO ret
  }

  // allocate a buffer
  buf := sagetvec(buflen/bytesperword)
  UNLESS buf DO
  { sys(Sys_close, fp) // First close the file
    res1, res2 := 0, 101
    GOTO ret
  }

//sawritef("FH0: fh0findoutput scb %n  %n  buf %n*n", scb, fp, buf)

  scb!scb.type    := scbt.file
  scb!scb.task    := taskid
  scb!scb.buf     := buf
  scb!scb.rdfn    := 0       // Can't replenish output streams
  scb!scb.wrfn    := fh0wrfn
  scb!scb.endfn   := fh0closefn
  scb!scb.fd      := fp
  scb!scb.bufend  := buflen
  scb!scb.write   := FALSE   // No data waiting to be written
  scb!scb.blength := buflen
  scb!scb.block   := 0       // MR 29/7/02
  scb!scb.lblock  := 0       // This is an empty file currently, MR 29/7/02
  scb!scb.ldata   := 0
//sawritef("fh0findoutput: lblock=%n*n", scb!scb.lblock)

  scb!scb.pos     := 0       // The buffer has no valid data initially
  scb!scb.end     := 0

ret:
  pkt!pkt.res1, pkt!pkt.res2 := res1, res2
//sawritef("FH0: fh0findoutput pkt %n  res1 %n  res2 %n*n", pkt, res1, res2)
  qpkt(pkt)
}

AND fh0findinoutput(pkt, scb, name, currdir) BE
{ LET fp, buf, res1, res2 = 0, 0, 1, 0
  LET filesize = 0
  LET filename = VEC 50
  trfilename(name, currdir, filename)

  // open the file for input and output
  fp := sys(Sys_openreadwrite, filename)
//sawritef("FH0: open %s in inout mode => %n*n", filename, fp)
  UNLESS fp DO
  { res1, res2 := 0, 100
    GOTO ret
  }

  filesize :=sys(Sys_filesize, fp)

  // allocate a buffer
  buf := sagetvec(buflen/bytesperword)
  UNLESS buf DO
  { sys(Sys_close, fp) // First close the file
    res1, res2 := 0, 101
    GOTO ret
  }
//sawritef("FH0: buflen = %n*n", buflen)
//sawritef("FH0: fh0findinoutput scb %n  %n  buf %n*n", scb, fp, buf)

  scb!scb.type    := scbt.file
  scb!scb.task    := taskid
  scb!scb.buf     := buf
  scb!scb.rdfn    := fh0rdfn
  scb!scb.wrfn    := fh0wrfn
  scb!scb.endfn   := fh0closefn
  scb!scb.fd      := fp
  scb!scb.bufend  := buflen
  scb!scb.write   := FALSE   // No data waiting to be written
  scb!scb.blength := buflen  // MR 15/3/02
  scb!scb.block   := 0 //1 MR 29/7/02
  scb!scb.lblock  := filesize/buflen //+ 1  // MR 16/4/02 MR 29/7/02
  scb!scb.ldata   := filesize REM buflen  // MR 16/4/02
//sawritef("fh0findinoutput: lblock=%n*n", scb!scb.lblock)

  // Initialise the buffer by reading the first block
  fh0getbuf(scb)
  res2 := result2

ret:
  pkt!pkt.res1, pkt!pkt.res2 := res1, res2
//sawritef("FH0: fh0findoutput pkt %n  res1 %n  res2 %n*n", pkt, res1, res2)
  qpkt(pkt)
}

AND fh0falsefn(scb)  = FALSE

AND fh0readfn(scb)  = VALOF
{ LET block, lblock = scb!scb.block, scb!scb.lblock
  LET pos,   end    = scb!scb.pos,   scb!scb.end
//sawritef("FH0: fh0readfn scb %n pos %n end %n*n", scb, pos, end)
//sawritef("FH0: fh0readfn block %n lblock %n*n", block, lblock)
  IF pos<end      RESULTIS TRUE  // Data still available in current buffer
  IF block=lblock RESULTIS FALSE // End-of-file

  IF scb!scb.write DO fh0putbuf(scb)  // Write block if necessary

  IF end>=buflen DO block := block+1  // Advance block if necessary
  scb!scb.block, scb!scb.pos := block, 0
//sawritef("FH0: fh0rdfn block %n pos %n end %n*n", scb!scb.block, pos, end)

  UNLESS fh0getbuf(scb) RESULTIS FALSE  // Read data into the buffer

  // Safety check
  end := scb!scb.end
  UNLESS end=buflen | lblock = scb!scb.block DO
  { sawritef("FH0: fh0readfn block %n lblock %n end %n*n",
              scb!scb.block,  lblock, scb!scb.pos,  end)
    abort(9999)
  }
  
  RESULTIS TRUE              // The buffer is not empty
}  
   
AND fh0writefn(scb) = VALOF
{ LET block, lblock = scb!scb.block, scb!scb.lblock
  LET pos, end = scb!scb.pos, scb!scb.end
  LET len = ?
//sawritef("FH0: fh0wrfn scb %n pos %n end %n*n", scb, pos, end)
//sawritef("FH0: fh0wrfn block %n lblock %n*n", block, lblock)
  IF scb!scb.write DO fh0putbuf(scb) // Write current block if necessary

  IF pos<scb!scb.bufend RESULTIS TRUE  // Still room in the current buffer
  // Move to next block
  block := block+1
  scb!scb.block, scb!scb.pos, scb!scb.end := block, 0, 0
  IF block>lblock DO
  { scb!scb.lblock := block    // Last block is empty
    RESULTIS TRUE
  }

  IF scb!scb.id=id.inoutscb UNLESS fh0getbuf(scb) DO
  { sawritef("FH0: fh0wrfn getbuf failed block=%n lblock=%n pos=%n end=%n*n",
              scb!scb.block, scb!scb.lblock, pos, end)
    abort(1102)
  }

//sawritef("FH0: fh0wrfn block=%n lblock=%n pos=%n end=%n*n",
//          scb!scb.block, scb!scb.lblock, pos, end)

  RESULTIS TRUE
}  

   
AND fh0endfn(pkt, scb) BE
{ LET id = scb!scb.id
//sawritef("FH0: fh0endfn scb %n*n", scb)
  IF scb!scb.write DO fh0putbuf(scb)
  sys(Sys_close, scb!scb.fd)
  safreevec(scb!scb.buf)
  qpkt(pkt)
}

// Result TRUE: posv contains the stream block and pos
//       FALSE: scb was not a file stream
AND fh0note(scb, posv) = VALOF
{ //UNLESS scb!scb.type=scbt.file RESULTIS FALSE
  posv!0 := scb!scb.block
  posv!1 := scb!scb.pos
//sawritef("FH0: note => %n %n*n", posv!0, posv!1)
  RESULTIS TRUE
}

// Set the stream position to that specified in posv.  If the
// new position is in a different block the buffer may have to
// be written out and new data read in.
// It returns TRUE if successful.

AND fh0point(scb, posv) = VALOF
{ LET blkno  = posv!0
  LET pos    = posv!1
  LET id     = scb!scb.id
  LET block  = scb!scb.block
  LET lblock = scb!scb.lblock
  LET end    = scb!scb.end
//sawritef("FH0: point  %n %n*n", posv!0, posv!1)

//sawritef("FH0: fh0point block=%n lblock=%n blkno=%n pos=%n end=%n*n",
//               block, lblock,  blkno, pos, end)

  UNLESS scb!scb.type=scbt.file &  // Must be a readable disc file
         (id=id.inscb | id=id.inoutscb) RESULTIS FALSE

  IF pos=0 & blkno=lblock+1 DO blkno, pos := lblock, buflen
 
//sawritef("FH0: fh0point block=%n lblock=%n blkno=%n pos=%n end=%n*n",
//               block, lblock,  blkno, pos, end)

//  IF blkno<=0 DO blkno, pos := 0, 0 // Cannot position before start of file

  // Safety check
  // Make sure the position is within the file
  IF blkno<0 | 
     blkno>lblock |
     blkno=lblock & pos > (block=lblock -> end, scb!scb.ldata)  DO
  { sawritef("FH0: fh0point beyond end of file, blkno=%n pos=%n*n", blkno, pos)
    sawritef("block=%n end=%n lblock=%n posv=(%n,%n)*n",
              block, end, lblock, posv!0, posv!1)
    abort(999)
  }

  IF blkno=block DO
  { // The new position is in the current block
    scb!scb.block := blkno
    scb!scb.pos   := pos
//sawritef("FH0: fh0point setting scb block=%n pos=%n*n", blkno, pos)
    RESULTIS TRUE // Success
  }

  // The move is to a different block, so must read a block
  // but first check if the current block must be written
  IF scb!scb.write DO
  { //sawritef("FH0: fh0point write block %n*n", scb!scb.block) 
    UNLESS fh0putbuf(scb) DO abort(5001)
  }

  scb!scb.block := blkno  // Set the new position
 
//sawritef("FH0: fh0point read block %n*n", blkno)

  UNLESS fh0getbuf(scb) DO
  { sawritef("FH0: fh0point fh0getbuf failed block %n => %n*n", blkno, end)
    abort(5001)
  }

  // Safety check
  UNLESS scb!scb.end=buflen |
         blkno=lblock & end>=scb!scb.ldata DO
  { sawritef("FH0 fh0point: safety check failed*n")
    sawritef("FH0 fh0point: blkno %n pos %n*n", blkno, pos)
    sawritef("FH0 fh0point: end %n buflen %n*n", scb!scb.end, buflen)
    sawritef("FH0 fh0point: block %n lblock %n*n", blkno, lblock)
    sawritef("FH0 fh0point: end %n ldata %n*n", end, scb!scb.ldata)
    abort(5001)
  }

  scb!scb.pos   := pos  // Set the desired offset

//sawritef("FH0: fh0point after getbuf blkno %n pos %n*n", blkno, pos)
  RESULTIS TRUE
}

// putbuf is only used on disc file streams. It writes the scb's buffer
// to file. The file is positioned before the write. If the last block
// is being written ldata is set to end and this number of bytes written
// to disc.
// It returns TRUE if successful, FALSE otherwise
AND fh0putbuf(scb) = VALOF
{ LET end    = scb!scb.end       // Number of bytes of valid data in buf
  LET block  = scb!scb.block
  LET offset = buflen*block      // File offset of buffer's first byte MR 29/7/02

  UNLESS end RESULTIS TRUE       // Nothing in buffer to write
//sawritef("FH0: putbuf seeking offset %n (block %n)*n", offset, block)
  UNLESS sys(Sys_seek, scb!scb.fd, offset) RESULTIS FALSE
//sawritef("FH0: putbuf write %n bytes at offset %n*n", end, offset)

  // The size of a file can only change when writing its last block
  // so ldata only needs correcting when this happens
  IF block = scb!scb.lblock DO scb!scb.ldata := end

//sawritef("FH0: putbuf write block %n*n", block)
  RESULTIS sys(Sys_write, scb!scb.fd, scb!scb.buf, end) >= 0
}

// fh0getbuf reads a block into the scb's buffer.
// If successful
//      it sets pos=0 and end to the end of valid data
//      and returns TRUE
// On failure
//      it returns FALSE

AND fh0getbuf(scb) = VALOF
{ LET fd      = scb!scb.fd
  LET block   = scb!scb.block
  LET offset  = buflen*block    // MR 29/7/02
  LET end     = ? 

//sawritef("FH0: fh0getbuf seeking start of block %n (offset %n)*n", block, offset)
  UNLESS sys(Sys_seek, fd, offset) RESULTIS FALSE
//sawritef("FH0: fh0getbuf file position now %n*n", sys(Sys_tell, fd))

//sawritef("FH0: fh0getbuf calling sys_read*n", block, offset)
//sawritef("FH0: fh0getbuf read block %n*n", block)
  end := sys(Sys_read, fd, scb!scb.buf, buflen)
//sawritef("FH0: fh0getbuf read => %n*n", end)
//sawritef("FH0: fh0getbuf block=%n lblock=%n ldata=%n*n",
//               block, scb!scb.lblock, scb!scb.ldata)
//UNLESS end=buflen DO abort(1101)
  IF end<0 RESULTIS FALSE // Unable to read
  scb!scb.pos, scb!scb.end := 0, end 
  RESULTIS TRUE
}





