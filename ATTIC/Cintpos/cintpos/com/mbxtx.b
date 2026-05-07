SECTION "mbxtx"

GET "libhdr"

GLOBAL {
  stdout:ug
}

LET start() = VALOF
{ LET argv    = VEC 30
  LET mbx, count, msecs = 0, 10, 1000
  LET mbxname = "MBX:junk"

  stdout := output()

  UNLESS rdargs("-n/N,-d/N,-b/K", argv, 30) DO
  { writef("Bad arguments for mbxtx*n")
    stop(20)
  }

  IF argv!0 DO count := !(argv!0)
  IF argv!1 DO msecs := !(argv!1)
  IF argv!2 DO mbxname := argv!2

//  UNLESS mbxname%1='M' & mbxname%2='B' & mbxname%3='X' & mbxname%4=':' DO
//  { writef("%s is not a mail box*n", mbxname)
//    stop(20)
//  }

  writef("count: %n  msecs: %n  to: %s*n", count, msecs, mbxname)
  delay(msecs)

  mbx := findoutput(mbxname)
  UNLESS mbx DO
  { writef("Can't open mail box '%s'*n", mbxname)
    stop(20)
  }

  selectoutput(mbx)
  FOR i = 1 TO count DO
  { sawritef("T%i2 sending:  *"Message %i4 from task %i2 box %s*"*n",
              taskid,               i,            taskid, mbxname)
    writef("Message %i4 from task %i2 box %s*n", i, taskid, mbxname)
    delay(msecs)
  }

  selectoutput(stdout)
  UNLESS mbx=stdout DO endstream(mbx)

  sawritef("End of mbxtx task %n*n", taskid)

  RESULTIS 0
}



