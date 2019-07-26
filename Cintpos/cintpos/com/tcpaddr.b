SECTION "testtcp"

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
  LET dcb = newdcb(Devt_tcpdev)
  LET id = Dcb_devid!dcb // OK even if dcb=0
  LET host, port = 0, 0

  UNLESS dcb DO
  { writef("Unable to create a dcb*n")
    RESULTIS 20
  }

  UNLESS rdargs("HOST,PORT", argv, 50) DO
  { writef("Bad argument for TCPADDR*n")
    RESULTIS 20
  }

  IF argv!0 DO host := argv!0
  IF argv!1 DO port := argv!1

  trygetaddr(id, host, port)

/*
  trygetaddr(id, "1.2.3.4", "5678")
  trygetaddr(id, "localhost", "echo")
  trygetaddr(id, "shep.cl.cam.ac.uk", "ftp")
  trygetaddr(id, "shep.cl.cam.ac.uk", "yyyy")
  trygetaddr(id, "badaddress.cl.cam.ac.uk", "ftp")
  trygetaddr(id, "meopham", 0)
  trygetaddr(id, 0, "ftp")
*/
  //writef("tcpaddr: tcp deleting device %n*n", id)
  deletedev(id)
  freevec(dcb)
  //writef("testtcp: tcp device %n deleted*n", id)
  RESULTIS 0
}

AND trygetaddr(id, hname, pname) BE
{ LET ipaddr, port = 0, 0
  //writef("testtcp: sending a pkt to device %n*n", id)
  ipaddr := sendpkt(-1, id, Tcp_name2ipaddr, 0, 0, hname)
  port   := sendpkt(-1, id, Tcp_name2port,   0, 0, pname)
  IF hname=0 DO hname := "0"
  IF pname=0 DO pname := "0"
  writef("testtcp: getaddr %s %s => %x8 %n*n",
          hname, pname, ipaddr, port)
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

