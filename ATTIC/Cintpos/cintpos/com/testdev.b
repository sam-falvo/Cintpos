SECTION "testdev"

GET "libhdr"

LET start() = VALOF
{ LET argv = VEC 50
  LET dcb = getvec(Dcb_upb)
  LET devtype = 0
  LET id = 0
  FOR i = 0 TO Dcb_upb DO dcb!i := 0

  UNLESS rdargs("DEVTYPE,A,B,C", argv, 50) DO
  { writef("Bad argument for TESTDEV*n")
    RESULTIS 20
  }

  IF argv!0 & string_to_number(argv!0) DO devtype := result2 

  Dcb_type!dcb := devtype
  id := createdev(dcb)
  Dcb_intson!dcb := TRUE
  UNLESS id DO
  { writef("Unable to create device of type %n*n", devtype)
    RESULTIS 20
  }
  sawritef("testdev: device %n created*n", id)
//FOR i=0 TO Dcb_upb DO writef("dcb %i2: %i9*n", i, dcb!i)
 
  SWITCHON devtype INTO
  { DEFAULT:  writef("Unknown device type %n*n", devtype)
              ENDCASE

    CASE Devt_clk:    testclk(id);        ENDCASE
    CASE Devt_ttyin:  testttyin(id);      ENDCASE
    CASE Devt_ttyout: testttyout(id);     ENDCASE
    CASE Devt_fileop: testfileop(id);     ENDCASE
    CASE Devt_tcpdev: testtcpdev(id);     ENDCASE
  }

  deletedev(id)
  writef("testdev: device %n deleted*n", id)
  RESULTIS 0
}

AND testclk(id) BE
{ LET p1 = TABLE -1, 0, 0, 0, 0, 0
  LET p2 = TABLE -1, 0, 0, 0, 0, 0
  LET p3 = TABLE -1, 0, 0, 0, 0, 0

  writef("testdev: res = %n*n", sendpkt(-1, id, 0, 0, 0, 1))

  writef("Testing clk device*n")
  pkt_id!p1, pkt_arg1!p1 := id, 1
  qpkt(p1)
  pkt_id!p2, pkt_arg1!p2 := id, 2
  qpkt(p2)
  pkt_id!p3, pkt_arg1!p3 := id, 3
  qpkt(p3)

  FOR i = 1 TO 3 DO
  { LET pkt = taskwait()
    writef("testdev: res = %n*n", pkt_res1!pkt)
  }
}

AND testttyin(id) BE
{ writef("Testing ttyin device*n")
  FOR i = 1 TO 10 DO
  { LET res = sendpkt(-1, id, 0, 0, 0, i)
    writef("testdev: res = %n*n", res)
  }
}

AND testttyout(id) BE
{ writef("Testing ttyout device*n")
  FOR i = 0 TO 9 DO
  { LET res = sendpkt(-1, id, 0, 0, 0, i)
    writef("testdev: res = %n*n", res)
  }
  newline()
}

AND testfileop(id) BE
  FOR i = 1 TO 10 DO
  { LET res = sendpkt(-1, id, 0, 0, 0, i)
    writef("testdev: res = %n*n", res)
  }

AND testtcpdev(id) BE
  FOR i = 1 TO 10 DO
  { LET res = sendpkt(-1, id, 0, 0, 0, i)
    writef("testdev: res = %n*n", res)
  }

