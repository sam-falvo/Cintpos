SECTION "mkjunk"

GET "libhdr"

LET start() = VALOF
{ LET argv = VEC 30
  LET stdout = output()
  LET out, size = 0, 4096*3+10
  LET pv = VEC 1

  UNLESS rdargs("NAME,SIZE", argv, 30) DO
  { writef("Bad arguments for mkjunk*n")
    stop(20)
  }

  UNLESS argv!0 DO argv!0 := "junk"
  IF argv!1 & string.to.number(argv!1) DO size := result2

  writef("mkdata: %s  size: %n*n", argv!0, size)

  out := findoutput(argv!0)
  UNLESS out DO
  { writef("Can't open file '%s'*n", argv!0)
    stop(20)
  }
  selectoutput(out)

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
      IF col=6 DO ch := ' '
      IF col=0 DO ch, rowstart := '*n', i
      wrch(ch)
    }
  }
  endwrite()
  selectoutput(stdout)
  RESULTIS 0
}


