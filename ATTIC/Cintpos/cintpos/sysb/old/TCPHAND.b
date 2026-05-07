/*
This is the source of the TCP handler task that implements TCP/IP
streams.

TCP connection names are normally of the form: "TCP:host:port", for
example "TCP:spice:echo" or "TCP:127.0.0.1:9000". Note that either
names or numbers can be used for both the host and port. If the port
is omitted, port 9000 is used by default. The host can be omitted, as
in "TCP::9050" or "TCP:".  This is used to allow streams to be opened
using the specified local port which then waits for the connection to
be etablished by some other machine.

Any number of input and output streams may be attached to the same
connection. The connection is created when the first stream is opened
and deleted when the last stream attached to it is closed.  Read and
write operations using the same connection can be interspersed, and
will done in the order the requests were received by TCPHAND.

Streams with names starting with TCP: are interactive with
transmission typically triggered by '*n'.  Such streams are normally
used for communication with terminals and serial devices such as
printers.  For applications involving the rapid transfer of large
amounts of data it is preferable to use streams with names starting
with NET:. Such streams normally transmit the data in blocks of 4096
bytes.  Both kinds of stream use the same TCPHAND handler task.


Implemented by Martin Richards (c) 14 May 2002

*/

SECTION "TCPHAND"                   // Main control section

GET     "libhdr"
GET     "manhdr"

MANIFEST {
 tcpblen =  256  // TCP stream buffer size in bytes
 netblen = 4096  // NET stream buffer size in bytes
 namelen =   32  // Connection names must be less than 32 chars long

 tcp.link = 0    // Link to next TCP connection
 tcp.rddcb       // DCB for reading from this connection
 tcp.wrdcb       // DCB for writing to this connection
 tcp.rddevid     // TCP device id for reading from this connection
 tcp.wrdevid     // TCP device id for writing to this connection
 tcp.ipaddr      // IP address (host format)
 tcp.port        // Port number (host format)
 tcp.sock        // Connection socket (for both reading and writing)
 tcp.refcount    // Number of SCBs attached to this connection
 tcp.connected   // TRUE if connection has been established
 tcp.q           // Queue of request packets waiting for this connection
 
 tcp.namebase    // Place holding the connection name
 tcp.upb  = tcp.namebase + namelen/bytesperword + 1
}

GLOBAL {
 tcplist : ug    // List of connection control blocks
 finddevid       // Main TCP device for IPADDR loopups etc
 findco          // Coroutine to handle findinput and findoutput
 findcofn        // Main function for findco
 findcofree      // findco is ready for deal with another request
 pktq            // Queue of pending packets for findco
}

MANIFEST {   // TCP device commands
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


LET newdcb() = VALOF
{ LET dcb = getvec(Dcb_upb)
  LET id = 0
  UNLESS dcb RESULTIS 0
  FOR i = 0 TO Dcb_upb DO dcb!i := 0
  dcb!Dcb_type := Devt_tcpdev
  id := createdev(dcb)
  UNLESS id DO { freevec(dcb); RESULTIS 0 }
  Dcb_intson!dcb := TRUE
  RESULTIS dcb
}

LET start ( init_pkt ) BE
{ LET pkt, dcb = 0, 0
  qpkt(init_pkt)  // Return startup pkt

  tcplist := 0    // Initially there are no connections

  findco := createco(findcofn, 500)
  findcofree := TRUE       // findco is initially ready for work
  UNLESS findco DO
  { sawritef("TCPHAND: unable to create findco*n")
    abort(999)
  }

  //sawritef("findcofree = %s*n", findcofree -> "TRUE", "FALSE")
  pktq := 0                // No pending task packets for findco

  // Create the TCP device used by findco for ipaddr lookup etc
  dcb := newdcb()
  UNLESS dcb DO
  { sawritef("TCPHAND: unable to create TCP address device*n")
    abort(999)
  }
  finddevid := dcb!Dcb_devid

  { // Main action loop for TCPHAND

    TEST pktq & findcofree
    THEN { pkt := pktq      // Get a pending packet for findco
           pktq, !pkt := !pktq, notinuse
         }
    ELSE pkt := taskwait()  // Get a device or task packet

    TEST pkt!pkt.id=finddevid
    THEN callco(findco, pkt)
    ELSE handlepkt(pkt)

  } REPEAT                // Process next device or task packet
}


AND handlepkt(pkt) BE
{ LET type, scb, res = pkt!pkt.type, pkt!pkt.arg1, ?

//sawritef("*nTCPHAND: received pkt %n type %n*n", pkt, type)

  SWITCHON type INTO
  { // The following four packet types are all handled by findco
    CASE Action_findinput:    // args: scb  -  name
    CASE Action_findoutput:   // args: scb  -  name
    CASE Action_close:        // args: scb
    CASE Tcp_accept:          // args: sock clientid scb
//sawritef("*nTCPHAND: findinput/output/close/accept pkt=%n*n", pkt)
       UNLESS findcofree DO
       { // findco is busy so append the packet to the end of the
         // pending packet queue
         LET p = @pktq
         WHILE !p DO p := !p
         !p, !pkt := pkt, 0
//sawritef("*nTCPHAND findco pkt %n from %n queued*n", pkt, pkt!pkt.id)
         RETURN
       }
       callco(findco, pkt) // Give to packet to findco
       RETURN

    CASE Action_read:
         // args: scb
         // replenish the scb buffer
//         sawritef("TCPHAND: Action_read scb=%n*n", scb)
         // Attempt to read via a connection item
         tcpread(pkt, scb)
         // tcpread will have returned the packet
         //pr()
         RETURN
  
    CASE Action_write:
         // args: scb
         // deplete the scb buffer
         //sawritef("TCPHAND: Action_write scb=%n tcp=%n*n", scb, scb!scb.fd)
         // Attempt to write via a connection item
         tcpwrite(pkt, scb)
         // tcpwrite will have returned the packet
         //pr()
         RETURN

    CASE Tcp_close:
         // Close pkt received from TCP device
         // Forward it to the client task
       { LET rc       = pkt!pkt.res1
         LET sock     = pkt!pkt.arg1
         LET clientid = pkt!pkt.arg2
           
         //sawritef("TCPHAND tcpfindstream: close rc=%n*n", rc)
         IF rc<0 DO
         { sawritef("TCPHAND: close of socket %n failed*n", sock)
           abort(999)
           RETURN
         }

         // Now return the packet to the client task
         pkt!pkt.id := clientid
         qpkt(pkt)
//sawritef("TCPHAND handlepkt: findcofree = %n*n", findcofree)
         findcofree := TRUE
         RETURN
       }

    CASE Tcp_recv:  // Packet returned from the TCP read device
       { LET n        = pkt!pkt.res1
         LET res2     = pkt!pkt.res2
         LET clientid = pkt!pkt.arg4
         scb         := pkt!pkt.arg5
//         sawritef("TCPHAND: Tcp_recv pkt back from device*n")
         //sawritef("TCPHAND: Tcp_recv n=%n*n", n)
         //sawritef("TCPHAND: Tcp_recv buf=%n len=%n*n", pkt!pkt.arg2,pkt!pkt.arg3)

         // Update scb and return pkt to client task
         scb!scb.pos  := 0
         scb!scb.end  := n
         // and return pkt to client task
         pkt!pkt.id   := clientid
         pkt!pkt.res1 := n>0  // TRUE if at least one character received
         qpkt(pkt)
         RETURN
       }

    CASE Tcp_send:  // Packet returned from the TCP write device
       { LET rc       = pkt!pkt.res1
         LET res2     = pkt!pkt.res2
         LET clientid = pkt!pkt.arg4
         scb         := pkt!pkt.arg5
         //sawritef("TCPHAND: Tcp_send pkt back from device, rc=%n*n", rc)
         // Update scb and return pkt to client task
         scb!scb.pos  := 0
         scb!scb.end  := 0
         // and return pkt to client task
         pkt!pkt.id   := clientid
         pkt!pkt.res1 := rc>=0
         qpkt(pkt)
         RETURN
       }


    DEFAULT:  // Unknown or unimplemented operation
         sawritef("TCPHAND: illegal op %n scb %n*n", type, scb)
         abort(306)
         returnpkt(pkt, 0, 0)
         RETURN
  }
}

// findcofn is the main function of the coroutine (findco) that
// processes findinput, findoutput and close requests.
// It deals with the Tcp_accept packet when it returns for a
// TCP device in the course of opening a stream. These requests
// are done serially in the order they are received. Each request may
// involve several interactions with the find device (with id finddevid)
// using cosendpkt to send packets to this device. TCPHAND gives any
// packets received from this device to findco provided it is free.
// The variable findcofree is TRUE when findco is ready to begin
// processing another request. If findco is not free when a packet to
// be processed by it is received by TCPHAND, the packet is 
// appended to pktq, for processing later.

// Operations, other than accept requests, that are concerned with
// opening and closing TCP streams do not involve long delays and are
// done using the find device. An accept request may be blocked for an
// long time waiting for some other machine to make a connection. Such
// a request uses a device belonging to the TCP connection item. 

AND findcofn(pkt) BE
{ LET type = pkt!pkt.type

  // Start processing a findco request, these have types:
  // Action_findinput, Action_findoutput, Action_close,
  // and Tcp_accept
 
  findcofree := FALSE // Until this request dealt with
//sawritef("findcofree = %s*n", findcofree -> "TRUE", "FALSE")

  SWITCHON type INTO
  { CASE Action_findinput:    // args: scb   -    name
         //sawritef("*nTCPHAND findco: findinput file %s*n", pkt!pkt.arg3)
         tcpfindstream(pkt, tcprdfn, 0)
         //pr()
         ENDCASE

    CASE Action_findoutput:   // args: scb   -    name
         //sawritef("*nTCPHAND tcpfindoutput file %s*n", pkt!pkt.arg3)
         tcpfindstream(pkt, 0, tcpwrfn)
         //pr()
         ENDCASE

    CASE Action_close:        // args: scb
       { LET scb = pkt!pkt.arg1
         //sawritef("TCPHAND: close scb %n*n", scb)
         tcpendfn(pkt)
         //pr()
         ENDCASE
       }

    CASE Tcp_accept:          // args: sock  clientid  scb
       { LET sock     = pkt!pkt.res1
         LET ipaddr   = pkt!pkt.res2
         LET lsock    = pkt!pkt.arg1
         LET clientid = pkt!pkt.arg2
         LET scb      = pkt!pkt.arg3
         LET tcp      = scb!scb.fd
         LET rc       = 0

         //sawritef("TCPHAND: accept received by findco %n*n", scb)

         IF sock<0 DO
         { sawritef("TCPHAND: accept failed*n")
           abort(998)           
           //????
         }

         //sawritef("TCPHAND findco: connection establisted ipaddr %x8 sock %n*n",
         //            ipaddr, sock)

         // Remember the connection socket.
         tcp!tcp.sock := sock
         tcp!tcp.connected := TRUE  // The connection is now connected

         // Close the listening socket
         //sawritef("TCPHAND findco: closing the listening socket %n*n", lsock)
         rc := cosendpkt(-1, finddevid, Tcp_close, 0, 0, lsock)

         IF rc<0 DO
         { sawritef("TCPHAND findco: close socket %n failed*n", lsock)
           abort(999)
         }

         // Return the packet to the original client.
         pkt!pkt.id := clientid
         returnpkt(pkt, TRUE, 0)

         // Release any packet that were waiting for this accept to happen
//sawritef("TCPHAND findco: tcp!tcp.q=%n*n", tcp!tcp.q)
         IF tcp!tcp.q DO
         { LET p = @pktq // Append them to pktq
           WHILE !p DO p := !p
           !p := tcp!tcp.q
           tcp!tcp.q := 0
         }
         ENDCASE
       }

    DEFAULT:  // Unknown or unimplemented operation for findco
         sawritef("TCPHAND: illegal op %n for findco*n", type)
         abort(306)
         returnpkt(pkt, 0, 0)
  }

  // findco is now free to deal with another request
  findcofree := TRUE
//sawritef("findcofree = %s*n", findcofree -> "TRUE", "FALSE")
}


AND tcprdfn   (scb) = sendpkt(notinuse, scb!scb.task, Action_read,  0, 0, scb)
AND tcpwrfn   (scb) = sendpkt(notinuse, scb!scb.task, Action_write, 0, 0, scb)
AND tcpclosefn(scb) = sendpkt(notinuse, scb!scb.task, Action_close, 0, 0, scb)

// tcpfindstream opens an input or output stream attached to a TCP
// connection. It is only called within the findco coroutine.
// It fills in the scb for the stream appropriately.
// If successful, it returns TRUE.
// On failure,    it returns FALSE with an error code in result2.

AND tcpfindstream(pkt, rdfn, wrfn) BE
{ LET clientid = pkt!pkt.id
  LET scb  = pkt!pkt.arg1
  LET name = pkt!pkt.arg3
  LET sock, s1, devid = 0, 0, 0
  LET ipaddr, port = 0, 0
  LET tcp = findtcp(name, pkt)
  LET type = result2
  LET buf, buflen = 0, type=scbt.tcp -> 256, 4096
  LET rc = 0

  buf := getvec(buflen/bytesperword)  
//sawritef("TCPHAND tcbfindstream: buf %n size %n allocated*n",
//          buf, buflen/bytesperword)
  scb!scb.type    := type
  scb!scb.task    := taskid
  scb!scb.buf     := buf
  scb!scb.rdfn    := rdfn
  scb!scb.wrfn    := wrfn
  scb!scb.endfn   := tcpclosefn
  scb!scb.fd      := tcp
  scb!scb.pos     := 0
  scb!scb.end     := 0
  scb!scb.bufend  := buflen

  IF tcp=-1 | buf=0 DO
  { // findtcp or getvec failed
    //sawritef("TCPHAND tcpfindstream: findtcp or getvec failed*n")
//?????????????? free tcp and buf if necessary
    returnpkt(pkt, FALSE, result2)
    RETURN
  }

  UNLESS tcp DO
  { // Waiting for the connection to be established
    //sawritef("TCPHAND tcpfindstream: waiting for a connection*n")
    // Do not return the packet to the client yet.
    RETURN
  }

  sock := tcp!tcp.sock

  UNLESS sock DO
  { // Create a socket, if necessary
    //sawritef("TCPHAND tcpfindstream: using dev %n to create a socket*n", finddevid)
    sock := cosendpkt(-1, finddevid, Tcp_socket, 0, 0)
    UNLESS sock>0 DO
    { sawritef("TCPHAND tcbfindstream: unable to create a socket*n")
      abort(999)
      GOTO bad
    }
    //sawritef("TCPHAND tcpfindstream: Socket %n*n", sock)

    tcp!tcp.sock := sock
  }

  IF rdfn & tcp!tcp.rddcb=0 DO
  { // Create a read TCP device, if necessary. 
    LET dcb = newdcb()
    UNLESS dcb DO
    { sawritef("TCPHAND tcbfindstream: unable to create a rd DCB*n")
      abort(999)
      GOTO bad
    }
    tcp!tcp.rddcb, tcp!tcp.rddevid := dcb, dcb!Dcb_devid
    //sawritef("TCPHAND tcpfindstream: created rd dev %n*n", dcb!Dcb_devid)
  }

  IF wrfn & tcp!tcp.wrdcb=0 DO
  { // Create a TCP write device, if necessary. 
    LET dcb = newdcb()
    UNLESS dcb DO
    { sawritef("TCPHAND tcbfindstream: unable to create a wr DCB*n")
      abort(999)
      GOTO bad
    }

    tcp!tcp.wrdcb, tcp!tcp.wrdevid := dcb, dcb!Dcb_devid
    //sawritef("TCPHAND tcpfindstream: created wr dev %n*n", dcb!Dcb_devid)
  }

  IF tcp!tcp.connected DO
  { // The connection is already connected
    // so just return the packet to the client task
    returnpkt(pkt, TRUE, 0)
    RETURN
  }

  ipaddr, port := tcp!tcp.ipaddr, tcp!tcp.port

  IF ipaddr DO
  { // Make a connection
    rc := cosendpkt(-1, finddevid, Tcp_connect,
                     0, 0, sock, ipaddr, port)
    IF rc<0 DO
    { sawritef("TCPHAND tcpfindstream: connect rc=%n*n", rc)
      GOTO bad
    }
    //sawritef("*nTCPHAND findstream: connect successful on socket %n*n", sock) 

    // Return the packet to the client task
    returnpkt(pkt, TRUE, 0)
    RETURN
  }

  // Listen for a connection

  //sawritef("TCPHAND tcpfindstream: send bind request*n")
  rc := cosendpkt(-1, finddevid, Tcp_bind,
                   0, 0, sock, ipaddr, port)
  IF rc<0 DO
  { sawritef("TCPHAND findstream: bind failed*n")
    GOTO bad
  }

  //sawritef("TCPHAND tcpfindstream: set reuseaddr*n")
  rc  := cosendpkt(-1, finddevid, Tcp_reuseaddr, 0, 0, sock, 1)
  IF rc<0 DO
  { sawritef("TCPHAND: reuseaddr failed*n")
    GOTO bad
  }

  //sawritef("TCPHAND tcpfindstream: send listen request*n")
  rc  := cosendpkt(-1, finddevid, Tcp_listen, 0, 0, sock, 5)
  IF rc<0 DO
  { sawritef("TCPHAND: listen failed*n")
    GOTO bad
  }

  // Wait for a connection to be established
  // using device devid, not finddevid
  // when the packet returns it will be processed by
  // the main loop in TCPHAND

  // Choose a suitable device other than finddevid for the accept request
  IF wrfn DO devid := tcp!tcp.wrdevid
  IF rdfn DO devid := tcp!tcp.rddevid
  UNLESS devid DO
  { sawritef("TCPHAND tcbfindstream: bad devid*n")
    abort(999)
    GOTO bad
  }

  //sawritef("TCPHAND tcpfindstream: send accept request to socket %n*n", sock)
  // Send an accept request to this device
  pkt!pkt.id   := devid
  pkt!pkt.type := Tcp_accept
  pkt!pkt.arg1 := sock

  // Save various values in the packet for use when the packet is
  // returned to TCPHAND. The remaining work to be done is to close
  // the listening socket and then return the packet to the original
  // client task.
  pkt!pkt.arg2 := clientid
  pkt!pkt.arg3 := scb
  //sawritef("TCPHAND tcpfindstream: send accept request to %n*n", devid)
  qpkt(pkt)

  // Do not return the packet to the client task yet.
  // (This will be done when the accept packet returns to TCPHAND.)

  // In the mean time, findco can deal with other requests
  RETURN

bad:
  IF buf DO freevec(buf)
  IF tcp DO closetcp(tcp)
  // Return the packet indicating failure to the client task
  returnpkt(pkt, FALSE, 0)
}

AND tcpread(pkt) BE
{ LET clientid = pkt!pkt.id
  LET scb      = pkt!pkt.arg1
  LET buf      = scb!scb.buf
  LET n        = scb!scb.bufend
  LET tcp      = scb!scb.fd
  LET rddevid  = tcp!tcp.rddevid
  LET sock     = tcp!tcp.sock

//sawritef("TCPHAND tcpread: request %n bytes from sock %n*n", n, sock)

  // If the stream cannot be read from, return the packet
  // with an error indication
  UNLESS rddevid DO
  { pkt!pkt.res1 := 0
    pkt!pkt.res2 := 1 // Can't read????????????
    qpkt(pkt)
    RETURN
  }

  // Send the pkt on to the TCP device
  pkt!pkt.id   := rddevid
  pkt!pkt.type := Tcp_recv
  pkt!pkt.arg1 := sock
  pkt!pkt.arg2 := buf
  pkt!pkt.arg3 := n
  pkt!pkt.arg4 := clientid
  pkt!pkt.arg5 := scb
  qpkt(pkt)
}

AND tcpwrite(pkt, scb) BE
{ LET clientid = pkt!pkt.id
  LET buf      = scb!scb.buf
  LET n        = scb!scb.pos
  LET tcp      = scb!scb.fd
  LET wrdevid  = tcp!tcp.wrdevid
  LET sock     = tcp!tcp.sock

//sawritef("TCPHAND tcpwrite: send %n bytes to sock %n*n", n, sock)
//FOR i = 0 TO n-1 DO sawritef("buf%%%i2 = %n '%c'*n", i, buf%i, buf%i)

  // If the stream cannot be read from, return the packet
  // with an error indication
  UNLESS wrdevid DO
  { pkt!pkt.res1 := 0
    pkt!pkt.res2 := 1 // Can't write????????????
    qpkt(pkt)
    RETURN
  }

//sawritef("TCPHAND tcpwrite: send data to socket %n*n", sock)

  // Send the pkt on to the TCP device
  pkt!pkt.id   := wrdevid
  pkt!pkt.type := Tcp_send
  pkt!pkt.arg1 := sock
  pkt!pkt.arg2 := buf
  pkt!pkt.arg3 := n
  pkt!pkt.arg4 := clientid
  pkt!pkt.arg5 := scb
  qpkt(pkt)
  // When it returns to TCPHAND it will be redirected
  // to the client task (clientid).
}

AND tcpendfn(pkt) BE
{ LET scb = pkt!pkt.arg1
  LET buf = scb!scb.buf
  LET tcp = scb!scb.fd
  LET res = closetcp(tcp)
  //sawritef("TCPHAND tcpendfn buf = %n*n", buf)
  IF buf DO { freevec(buf); scb!scb.buf := 0 }
  returnpkt(pkt, res, result2) // Return the pkt to the client task
  //sawritef("TCPHAND tcpendfn: closed scb %n*n", scb)
}

// findtcp(name, pkt)
//               finds the connection with given name. It finds the
//               IP address and port number and then searches
//               tcplist for a matching connection control block.
//               If one is not found, a new one is created and linked
//               into the list. It does not create either the read
//               or write TCP devices. This is left to tcpfindstream
//               to deal with if necessary. findtcp is only called
//               when the coroutine findco is running.
//
// It returns:  -1  on failure
//                  result2 = error code
//               0  if a connection is being made and pkt is on
//                  the tcp.q  
//         or:   a pointer to the connection
//                  result2 = the type (scbt_tcp or scbt_net)

AND findtcp(name, pkt) = VALOF
{ LET tcp = tcplist
  LET ipaddr, port, type = 0, 0, 0
  LET devname  = VEC 32/bytesperword
  LET hostname = VEC 32/bytesperword
  LET portname = VEC 32/bytesperword
  LET pos = splitname(devname, ':', name, 1)

  UNLESS compstring(devname, "NET") DO type := scbt.net
  UNLESS compstring(devname, "TCP") DO type := scbt.tcp
  UNLESS type RESULTIS -1  // Bad TCP name

  hostname%0, portname%0 := 0, 0
  IF pos DO pos := splitname(hostname, ':', name, pos)
  IF pos DO pos := splitname(portname, ':', name, pos)
//sawritef("*nTCPHAND findtcp: dev:%s host:%s port:%s*n", 
//          devname, hostname, portname)
  UNLESS hostname%0 DO hostname := 0
  UNLESS portname%0 DO portname := "9000"

  ipaddr := cosendpkt(-1,finddevid,Tcp_name2ipaddr,0,0,hostname)
  port   := cosendpkt(-1,finddevid,Tcp_name2port,  0,0,portname)
//sawritef("*nTCPHAND findtcp: ipaddr:%x8 port:%n*n", ipaddr, port)

  IF ipaddr=-1 | port=-1 DO
  { sawritef("*nTCPHAND findtcp: Bad address host:%n port:%n*n", 
              ipaddr, port)
    RESULTIS -1
  }

  WHILE tcp DO
  { IF ipaddr = tcp!tcp.ipaddr & port = tcp!tcp.port DO
    { // The connection already exits
      tcp!tcp.refcount := tcp!tcp.refcount + 1
      result2 := type
      UNLESS tcp!tcp.connected DO
      { LET p = @tcp!tcp.q  // Append the packet to the queue
        WHILE !p DO p := !p
        !p, !pkt := pkt, 0
//sawritef("*nTCPHAND findtcp: existing disconnected tcp item %n found*n", tcp)
        RESULTIS 0
      }
//sawritef("*nTCPHAND findtcp: existing connected tcp item %n found*n", tcp)
      RESULTIS tcp
    }
    tcp := tcp!tcp.link
  }

  // Make a new connection
  tcp := getvec(tcp.upb)

  // Initialise all the connection fields
  FOR i = 0 TO tcp.upb DO tcp!i := 0

  tcp!tcp.ipaddr    := ipaddr
  tcp!tcp.port      := port
  tcp!tcp.refcount  := 1
  tcp!tcp.connected := FALSE

  FOR i = 0 TO name%0 DO (tcp+tcp.namebase)%i := name%i

  // Link the connection into the head of tcplist
  tcp!tcp.link := tcplist
  tcplist      := tcp

  result2 := type
//sawritef("*nTCPHAND findtcp: new tcp item %n created*n", tcp)
  RESULTIS tcp                   // Successful return
}

// closetcp(tcp) decrements the reference count and, if it is
//               now zero, it closes the socket (using finddevid),
//               deletes the read and write devices (if any) and
//               returns the tcp control block to free store.
//               It runs in the findco coroutine.
//
// It returns:  TRUE  if the connection was deleted
//              FALSE otherwise

AND closetcp(tcp) = VALOF
{ LET p = @tcplist
  LET refcount = tcp!tcp.refcount-1
  tcp!tcp.refcount := refcount

  IF refcount RESULTIS FALSE

  // The connection has no attached streams so it must be deleted

  { LET q = !p
    IF q=tcp DO
    { LET sock = tcp!tcp.sock
      LET rc = cosendpkt(-1, finddevid, Tcp_close, 0, 0, sock)
      IF rc<0 DO
      { sawritef("TCPHAND: unable to close socket %n*n", sock)
        abort(999)
      }

      !p := !tcp      // Remove the connection from the list
      
      // Delete the devices, if any
      IF tcp!tcp.rddevid DO deletedev(tcp!tcp.rddevid)
      IF tcp!tcp.wrdevid DO deletedev(tcp!tcp.wrdevid)
      IF tcp!tcp.rddcb   DO freevec(tcp!tcp.rddcb)
      IF tcp!tcp.wrdcb   DO freevec(tcp!tcp.wrdcb)
      freevec(tcp)    // and return its space
      RESULTIS TRUE
    }
    p := q
  } REPEATWHILE p

  sawritef("TCPHAND: closetcp connection not in the list*n")
  abort(999)
  RESULTIS FALSE
}

// This is only called in the findco coroutine
AND cosendpkt(link, id, type, r1, r2, a1, a2, a3, a4, a5, a6) = VALOF
{ 
//sawritef("TCPHAND cosendpkt: currco: %n sending pkt to %n*n", currco, id)
  UNLESS qpkt(@link) DO
  { sawritef("TCPHAND cosendpkt: qpkt failed*n")
    abort(999)
  }
  UNLESS cowait()=@link DO
  { sawritef("TCPHAND: cosendpkt error*n")
    abort(999)
  }
  result2 := r2
  RESULTIS r1
}


AND pr() BE
{ LET p = tcplist
  sawrch('*n')
  WHILE p DO
  { LET q = p!tcp.q
    sawritef("Connection: rd %i3 wr %i3 refs %n  name %s   q:",
              p!tcp.rddevid, p!tcp.wrdevid, p!tcp.refcount, p+tcp.namebase)
    WHILE q DO
    { sawritef(" %n", q!pkt.id)
      q := !q
    }
    sawrch('*n')
    p := !p
  }
}


