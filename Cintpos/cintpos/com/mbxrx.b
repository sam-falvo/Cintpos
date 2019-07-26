SECTION "mbxrx"

GET "libhdr"

GLOBAL {
  stdin:ug
}

LET start() = VALOF
{ LET argv    = VEC 30
  LET mbx, count, msecs = 0, 10, 1000
  LET mbxname = "MBX:junk"

  stdin := input()

  UNLESS rdargs("-n/N,-d/N,-b/K", argv, 30) DO
  { writef("Bad arguments for mbxrx*n")
    stop(20)
  }

  IF argv!0 DO count := !(argv!0)
  IF argv!1 DO msecs := !(argv!1)
  IF argv!2 DO mbxname := argv!2

//  UNLESS mbxname%1='M' & mbxname%2='B' & mbxname%3='X' & mbxname%4=':' DO
//  { writef("%s is not a mail box*n", mbxname)
//    stop(20)
//  }

  writef("count: %n  msecs: %n  from: %s*n", count, msecs, mbxname)
  delay(msecs)

  mbx := findinput(mbxname)
  UNLESS mbx DO
  { writef("Can't open mail box '%s'*n", mbxname)
    stop(20)
  }

  selectinput(mbx)

  FOR i = 1 TO count DO
  { LET ch = ?
    LET mes = VEC 256
    FOR j = 1 TO 1024 DO
    { ch := rdch()
      IF ch='*n' | ch=endstreamch BREAK
      mes%0, mes%j := j, ch
    }
    IF ch=endstreamch BREAK
    sawritef("T%i2 received: *"%s*"*n", taskid, mes)
    delay(msecs)
  }

  UNLESS mbx=stdin DO endread()
  selectinput(stdin)

  sawritef("mbxrx task %n done*n", taskid)

  RESULTIS 0
}



