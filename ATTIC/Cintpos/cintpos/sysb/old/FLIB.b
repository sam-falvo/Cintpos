SECTION "FLIB"

GET "libhdr"

MANIFEST  $( info.trace.state = 8 $)

// Changed MR 23/11/01
LET sendpkt(link,id,type,res1,res2,a1,a2,a3,a4,a5,a6) = VALOF
{ LET destination = id

  IF qpkt(@link) DO
  { { LET p = pktwait(destination, @link)
      IF p = @link BREAK
      abort(182, p) // Not the expected packet
    } REPEAT
    result2 := res2
    RESULTIS res1
  }
  abort(181)
}

// sysabort should be removed
AND sysabort(code, arg2) BE
{ sawritef("FLIB: sysabort(%n, %n) called*n", code, arg2)
  abort(999)
}

// Not used
AND open.for.output(name, recsiz, maxrec) = VALOF
{ sawritef("FLIB: open.for.output(%s,%n,%n) called*n", name, recsiz, maxrec) 
  RESULTIS findrelstream(name, id.outscb, recsiz << 2, maxrec)
}

AND open.for.input(name) = findrelstream(name, id.inscb)

AND open.for.update(name) = findrelstream(name, id.inoutscb)

AND setrecordlength(scb, length) = VALOF  // length is in words
{ LET old = scb!scb.reclen / bytesperword
  scb!scb.reclen := length * bytesperword // but reclen is in bytes
  RESULTIS old
}

AND recordnote(scb) = VALOF // The first record has number 0
{ LET rec = muldiv(scb!scb.block, scb!scb.blength, scb!scb.reclen)
  IF scb!scb.block < 0 RESULTIS 0
  RESULTIS rec + (result2 + scb!scb.pos)/(scb!scb.reclen)
}

// MR 28/7/02: The first record of a file has number 0 (not 1)
AND recordpoint(scb, recno) = VALOF
{ LET pvec = VEC 1
  UNLESS scb!scb.type=scbt.file DO
  { sawritef("FLIB recordpoint: only works on a disc file*n")
    abort(999)
    RESULTIS FALSE
  }
  IF recno<0 DO   // The first record has number 0
  { sawritef("FLIB: recordpoint recno=%n*n", recno)
    abort(1000)
    recno := 0
  }
//sawritef("recordpoint: muldiv(%n,%n,%n)*n",
//          scb!scb.reclen, recno, scb!scb.blength)
//abort(1000)
  pvec!0 := muldiv(scb!scb.reclen, recno, scb!scb.blength) // MR 29/7/02
  pvec!1 := result2
//sawritef("FLIB: recordpoint: recno %n => %n %n*n",
//          recno, pvec!0, pvec!1)
//abort(1000)
//IF pvec!0>2 DO abort(8888)
  RESULTIS point(scb, pvec)
}

// Position an inout stream to its end
// This should be removed.
AND appendstream(scb) = VALOF //?????????????????????????????
$(  LET lblock = scb!scb.lblock
    LET ldata = scb!scb.ldata
    LET pvec = VEC 1
sawritef("FLIB: appendstream called*n"); abort(999)
    UNLESS scb!scb.id=id.inoutscb RESULTIS FALSE
    IF scb!scb.block=lblock & scb!scb.end>ldata DO
        ldata := scb!scb.end
    pvec!0, pvec!1 := lblock, ldata
    UNLESS point(scb, pvec) RESULTIS FALSE
//    scb!scb.pos := scb!scb.end
    RESULTIS TRUE
$)

// Position to start of stream
AND rewindstream(scb) = VALOF // MR 17/3/02
{ LET pvec = VEC 1
  pvec!0, pvec!1 := 0, 0      // MR 6/8/02
  RESULTIS point(scb, pvec)
}

// Advance stream position by n words
AND stepstream(scb, n) = VALOF
{ LET pvec = VEC 1
  LET bytes, len = n * bytesperword, scb!scb.blength
  LET blocks = bytes / len
  bytes := bytes REM len
  note(scb, pvec)
  pvec!1 := pvec!1 + bytes
  IF pvec!1 < 0   DO pvec!0, pvec!1 := pvec!0 - 1, pvec!1 + len
  IF pvec!1 > len DO pvec!0, pvec!1 := pvec!0 + 1, pvec!1 - len
  pvec!0 := pvec!0 + blocks
  RESULTIS pvec!0 < 1 -> FALSE, point(scb, pvec)
}

AND freeobj(obj) BE freevec(obj)

AND copydir(dir) = VALOF
{ LET v = sharevec((dir%0)/bytesperword)
  IF v FOR i = 0 TO dir%0 DO v%i := dir%i
//sawritef("FLIB: copydir called*n")
//abort(999)
  RESULTIS v
}

AND setbulk(scb, no.records) = VALOF
{ //sawritef("FLIB: setbulk entered*n")
  //abort(9999)
  rewindstream(scb)
  { LET reclen = scb!scb.reclen / bytesperword
    LET v = getvec(reclen-1)
    LET oldout = output()
    UNLESS v RESULTIS FALSE
    FOR i = 0 TO reclen-1 DO v!i := 0
    selectoutput(scb)
    FOR i = 0 TO no.records - 1 DO writewords(v, reclen)
//sawritef("FLIB: setbulk calling selectoutput(%x8)*n", oldout)
//abort(9997)
    selectoutput(oldout)
    freevec(v)
  }
  rewindstream(scb)
  RESULTIS TRUE
}



