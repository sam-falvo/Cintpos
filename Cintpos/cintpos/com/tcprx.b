SECTION "tcprx"

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
  LET hostname = 0 // Accept any host
  LET portname = "9000"
  LET n = 3

  UNLESS rdargs("HOST,PORT", argv, 50) DO
  { writef("Bad argument for TCPTX*n")
    RESULTIS 20
  }

  IF argv!0 DO hostname := argv!0
  IF argv!1 DO portname := argv!1

  tcprx(hostname, portname)

  RESULTIS 0
}


AND newdcb(type) = VALOF
{ LET dcb = getvec(Dcb_upb)
  LET id = 0
  UNLESS dcb RESULTIS 0
  FOR i = 0 TO Dcb_upb DO dcb!i := 0
  Dcb_type!dcb := type
  id := createdev(dcb)
  UNLESS id DO { freevec(dcb); dcb := 0 }
  Dcb_intson!dcb := TRUE
  RESULTIS dcb
}


AND tcprx(hostname, portname) BE
{ LET dcb = newdcb(Devt_tcpdev)
  LET id  = Dcb_devid!dcb  // OK even if dcb=0
  LET buf = VEC 100
  LET s, s1, rc = 0, 0, 0
  LET retcode = 0
  LET ipaddr, port = 0, 0
  
  UNLESS dcb DO
  { writef("tcprx: Unable to create a DCB*n")
    retcode := 20
    GOTO ret
  }

  ipaddr := sendpkt(-1, id, Tcp_name2ipaddr, 0, 0, hostname)
  IF hostname=0 DO hostname := "0"
  writef("Host %s => %x8*n", hostname, ipaddr)
  port   := sendpkt(-1, id, Tcp_name2port, 0, 0, portname)
  writef("Port %s => %n*n", portname, port)
  s  := sendpkt(-1, id, Tcp_socket, 0, 0)
  IF s<0 DO
  { writef("tcprx: socket failed*n")
    retcode := 20
    GOTO ret
    stop(20)
  }

  rc  := sendpkt(-1, id, Tcp_reuseaddr, 0, 0, s, 1)
  IF rc<0 DO
  { writef("tcprx: reuseaddr failed*n")
    retcode := 20
    GOTO ret
    stop(20)
  }

  rc  := sendpkt(-1, id, Tcp_bind, 0, 0, s, ipaddr, port)
  IF rc<0 DO
  { writef("tcprx: bind failed*n")
    retcode := 20
    GOTO ret
    stop(20)
  }

  rc  := sendpkt(-1, id, Tcp_listen, 0, 0, s, 5)
  IF s<0 DO
  { writef("tcprx: listen failed*n")
    retcode := 20
    GOTO ret
    stop(20)
  }

  s1  := sendpkt(-1, id, Tcp_accept, 0, 0, s)
  IF s<0 DO
  { writef("tcprx: accept failed*n")
    retcode := 20
    GOTO ret
    stop(20)
  }

  sawritef("tcprx: Socket s=%n  s1=%n ipaddr=%x8*n", s, s1, result2)

  { LET n = sendpkt(-1, id, Tcp_recv, 0, 0, s1, buf, 1)
writef("recv returned n=%n*n", n)
    IF n<=0 BREAK
    FOR i = 0 TO n-1 DO wrch(buf%i)
  } REPEAT

  rc := sendpkt(-1, id, Tcp_close, 0, 0, s)

  sawritef("Return code = %n*n", rc)

ret:
  
  IF dcb DO 
  { //writef("tcprx: tcp deleting device %n*n", id)
    deletedev(id)
    freevec(dcb)
    //writef("tcprx: tcp device %n deleted*n", id)
  }
}

