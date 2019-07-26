SECTION "tcptx"

MANIFEST {
 Tcp_name2ipaddr =  1   // name         => ipaddr
 Tcp_name2port   =  2   // name         => port
 Tcp_socket      =  3   //              => fd
 Tcp_reuseaddr   =  4   // flag         => rc
 Tcp_sndbufsz    =  5   // size         => rc
 Tcp_rcvbufsz    =  6   // size         => rc
 Tcp_bind        =  7   // ipaddr, port => rc
 Tcp_connect     =  8   // ipaddr, port => fd
 Tcp_listen      =  9   // fd, n        =>
 Tcp_accept      = 10   // fd           => fd
 Tcp_recv        = 11   // buf, len     => n
 Tcp_send        = 12   // buf, len     => n
 Tcp_close       = 13   //              =>
}

GET "libhdr"

LET start() = VALOF
{ LET argv = VEC 50
  LET host = "localhost"
  LET port = "9000"
  LET n = 3

  UNLESS rdargs("HOST,PORT,N", argv, 50) DO
  { writef("Bad argument for TCPTX*n")
    RESULTIS 20
  }

  IF argv!0 DO host := argv!0
  IF argv!1 DO port := argv!1
  IF argv!2 & string_to_number(argv!2) DO n := result2

  tcptx(host, port, n)

  RESULTIS 0
}


AND newdcb(type) = VALOF
{ LET dcb = getvec(Dcb_upb)
  LET id = 0
  FOR i = 0 TO Dcb_upb DO dcb!i := 0
  Dcb_type!dcb := type
sawritef("tcptx: newdcb creating device*n")
  id := createdev(dcb)
sawritef("tcptx: TCP device id = %n*n", id)
  UNLESS id DO
  { freevec(dcb)
    dcb := 0
  }
  Dcb_intson!dcb := TRUE
  RESULTIS dcb
}


AND tcptx(hname, pname, n) BE
{ LET dcb = newdcb(Devt_tcpdev)
  LET buf = VEC 1000
  LET mess = "Hello world*n"
  LET len = mess%0
  LET id, s, rc = 0, 0, 0
  LET ipaddr, port = 0, 0
  
  UNLESS dcb DO
  { writef("tcptx: Unable to create a DCB*n")
    stop(20)
  }

  id := Dcb_devid!dcb
  sawritef("tcptx: TCP device %n created*n", id)
/*
  FOR i = 3 TO 5000 DO
  { LET s = sendpkt(-1, id, Tcp_socket, 0, 0)
    writef("%i5: new socket %n*n", i, s)
    // Under Linux the largest value of s is 1023
    IF s<=0 BREAK
  }
abort(1000)
  FOR s = 3 TO 5000 DO
  { LET rc = sendpkt(-1, id, Tcp_close, 0, 0, s)
    writef("closing socket %n, rc=%n*n", s, rc)
    IF rc<0 BREAK
  }
  deletedev(id)
  freevec(dcb)
RETURN
*/

  ipaddr  := sendpkt(-1, id, Tcp_name2ipaddr, 0, 0, hname)
  sawritef("tcptx: Host %s => %x8*n", hname, ipaddr)
  port    := sendpkt(-1, id, Tcp_name2port, 0, 0, pname)
  sawritef("tcptx: Port %s => %n*n", hname, port)
  s  := sendpkt(-1, id, Tcp_socket, 0, 0)
  sawritef("tcptx: Socket %n*n", s)
  // try to connect with a 5000 msecs timeout
  { rc  := sendpkt(-1, id, Tcp_connect, 0, 0, s, ipaddr, port, 5000)
    sawritef("tcptx: connect rc=%n*n", rc)
    IF rc=-2 DO
    { sawritef("tcptx: connect timed out after 5 seconds*n")
      GOTO fin
    }
    UNLESS rc BREAK
    sawritef("tcptx: will try again in 1 second*n")
    delay(1000)
    IF testflags(flag_b) GOTO fin
  } REPEAT

  sawritef("tcptx: will try again in 1 second*n")

  FOR i = 1 TO len DO buf%(i-1) := mess%i

  sawritef("Sending '%s' to host: %s port: %s  %n times*n",
                     mess,        hname, pname, n)

  FOR i = 1 TO n DO
    rc := sendpkt(-1, id, Tcp_send, 0, 0, s, buf, len)

  rc := sendpkt(-1, id, Tcp_close, 0, 0, s)

  sawritef("Return code = %n*n", rc)

fin:
  //writef("tcptx: deleting tcp device %n*n", id)
  deletedev(id)
  freevec(dcb)
  //writef("tcptx: tcp device %n deleted*n", id)
}

