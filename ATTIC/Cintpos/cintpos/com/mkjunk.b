SECTION "mkjunk"

GET "libhdr"

LET start() = VALOF
{ LET argv = VEC 30
  LET stdout = output()
  LET toname = "junk"
  LET scb, size = 0, 4096*3+10 // in bytes
  LET pv = VEC 1

  UNLESS rdargs("NAME,SIZE/N", argv, 30) DO
  { writef("Bad arguments for mkjunk*n")
    stop(20)
  }

  IF argv!0 DO toname := argv!0
  IF argv!1 DO size := !(argv!1)

  writef("mkjunk: %s  size: %n*n", toname, size)

  scb := findoutput(toname)
  UNLESS scb DO
  { writef("Can't open file '%s'*n", toname)
    stop(20)
  }
  selectoutput(scb)

  { LET rowstart = 0
    FOR i = 1 TO size DO 
    { LET col = i REM 64
      LET ch = i-1
      IF col=1 DO ch := rowstart/10000
      IF col=2 DO ch := rowstart/1000
      IF col=3 DO ch := rowstart/100
      IF col=4 DO ch := rowstart/10
      IF col=5 DO ch := rowstart
      ch := ch REM 10 + '0'
      IF col=6 DO ch := ':'
      IF col=0 DO ch, rowstart := '*n', i
      wrch(ch)
    }
  }
  endwrite()

  scb := findinoutput(toname)
  UNLESS scb DO
  { writef("Can't open file '%s'*n", toname)
    stop(20)
  }
  selectoutput(scb)

// Since 29/7/02 the first block of a file has number 0 (not 1)
// Since 29/7/02 the first record of a file has number 0 (not 1)
  putdata(  0, 1, 20)
  putdata(  0, 1, 40)
  putdata(  1, 1, 10)
  putdata(  2, 1, 20)
  putdata(  0, 2, 30)
  putdata(  1, 2, 40)
  putdata(  0, 3, 20)
//  putdata(  3, 0, 11)  // Bad -- One byte beyond the end of file
  putdata(  3, 0, 10)  // OK

  endwrite()
  selectoutput(stdout)
  RESULTIS 0
}

AND putdata(blkno, rec, pos) BE
{ LET posv= VEC 1
  posv!0 := blkno
  posv!1 := rec*64+pos
  UNLESS point(cos, posv) DO
  { sawritef("*nUnable to point on this stream*n")
sawritef("id=%x8 type=%n *n", cos!scb_id, cos!scb_type)
    RETURN
  }
  writef("#%n=%n=%n#", blkno, rec, pos)
}


