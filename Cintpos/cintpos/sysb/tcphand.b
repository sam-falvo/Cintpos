/*
This is the source of the TCP handler task that implements TCP/IP
streams.

TCP connection names are normally of the form: "TCP:host:port", for
example "TCP:spice:echo" or "TCP:127.0.0.1:9000". Note that either
names or numbers can be used for both the host and port. If the port
is omitted, port 9000 is used by default. The host can be omitted, as
in "TCP::9050" or "TCP:".  Such streams listen on the specified local
port waiting for a connection to be established by some other machine.

Any number of input and output streams may be attached to the same
connection. The connection is created when the first stream is opened
and deleted when the last stream attached to it is closed.  Read and
write operations using the same connection can be interspersed, and
will done in the order the requests were received by TCPHAND.

Streams with names starting with TCP: are interactive causing
transmission to be triggered typically by '*n'.  Such streams are
normally used for communication with terminals and serial devices such
as printers.  For applications involving the rapid transfer of large
amounts of data, it is preferable to use streams with names starting
with NET:. Such streams normally transmit the data in blocks of 4096
bytes.  Both kinds of stream use the same TCPHAND handler task.

Timeouts

The timeout field (scb_timeout) of an SCB specifies how long an
operation (findinput, findoutput, read, write or close) is permitted
to take. Timeout values are integers in milli-second units. A timeout
value of zero means there is no timeout and the operation will take as
long and needed. A timeout value of -1 means that only operations that
can be done without blocking will take place.

findinput or
findoutput
   res1=0  res2>0  An error has occurred.
   res1=0  res2=-1 Connection closed by remote host
   res1=0  res2=-2 A timeout occurred before a connection was made.
   res1~=0         res1 is the SCB for the connection. The connection
                   may not yet be fully established. For instance, if
                   the host IP address was not specified, the socket
                   may be listening on the given port waiting for a
                   connection to be accepted. Read, write and get
                   remote IP address requests will wait for the
                   connection to be established or may time out. Example:

                   LET scb = findinput("TCP::7000") // Returns quickly
                   LET rc = 0
                   // scb=0  result2>0  Error
                   // scb=0  result2=-1 Connection closed by remote host
                   // scb=0  result2=-2 Timeout
                   // scb~=0            scb ok
                   settimeout(scb, 5000)   // Set timout of 5 secs
                   rc := getremipaddr(scb) // Waits for connection
                   // rc=0  result2>1      Error
                   // rc=0  result2=-1     Closed by remote host
                   // rc=0  result2=-2     Timeout before connection
                   // rc=0  result2=-3     No connection yet (polling)
                   // rc=0  result2=-4     No timeout was specified and
                                           no connection has been established
                                           in the last 15 seconds.
                   // rc~=0                rc is the remote IP address
                                           of an established
                                           connection

read or
write
   res1=0  res2>0  An error has occurred
   res1=0  res2=-1 EOF or connection closed by peer
   res1=0  res2=-2 A timeout occurred and no bytes were transferred.
   res1=0  res2=-3 No bytes transferred in polling mode
   res1=0  res2=-4 No timeout was specified and no bytes have been
                   transferred in the last 15 seconds.
   res1~=0         No timeout occurred and res1
                   bytes have been transferred.
             
close
   res1=0  res2>0  An error has occurred
   res1=0  res2=-2 A timeout occurred before the connection was closed.
   res1=-1 res2=0  The connection was was closed successfully.

Implemented by Martin Richards (c) 4 November 2003

*/

SECTION "TCPHAND"                   // Main control section

GET     "libhdr"
GET     "manhdr"

MANIFEST {
 tcpblen =  256  // TCP stream buffer size in bytes
 netblen = 4096  // NET stream buffer size in bytes
 namelen =   63  // Connection names must <= 63 chars long

 // Fields of a TCP connection node
 tcp_link = 0    // Link to next TCP connection
 tcp_rddcb       // DCB for reading from this connection, or zero
 tcp_wrdcb       // DCB for writing to this connection, or zero
 tcp_rddevid     // TCP device id for reading from this connection
 tcp_wrdevid     // TCP device id for writing to this connection
 tcp_ipaddr      // IP address (host format)
 tcp_port        // Port number (host format)
 tcp_sock        // Connection socket (for both reading and writing)
 tcp_refcount    // Number of SCBs attached to this connection
 tcp_state       // =0  Not connected
                 // =1  connecting -- typically listen called
                 // =2  accept called
                 // =3  connection fully established
                 // =4  accept time out
                 // =5  error occurred
 tcp_remipaddr   // IP address of remote host after the connection is made
                 // =0 until the connection is established
 tcp_q           // Queue of request packets waiting for the connection
                 // to be fully established established or for an error or
                 // time out to occur. When this happens all the packets
                 // on this queue will be transferred to connq.
 tcp_namebase    // Place holding the connection name (max length of 63 chars)
 tcp_upb  = tcp_namebase + (namelen+1)/bytesperword + 1
}

GLOBAL {
 tcplist : ug    // List of connection control blocks
 finddevid       // TCP device used for IPADDR lookups etc
 workco          // Coroutine to handle findinput and findoutput
 workcofn        // Main function for workco
 workcofree      // TRUE when workco is ready to deal with another request
 workq           // List of packets waiting to be processed by workco
 connq           // List of packets that can now be processed because the
                 // connections they were waiting for have been established.
                 // These packets will be for read, write, getremipaddr and
                 // close requests.

 pktlist         // List of packet-coroutine pairs used by cosendpkt and findpkt
 cosendpkt
 findpkt
 tracing    // Conditionally causes the tracing of tcp actions.
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
 Tcp_listen      =  9   // fd, n        => rc
 Tcp_accept      = 10   // sock, tcp,   -, timeout => fd
 Tcp_recv        = 11   // sock, buf, len, timeout, id, scb => n
 Tcp_send        = 12   // sock, buf, len, timeout, id, scb => n
 Tcp_close       = 13   // sock         => rc
}


/* res := cosendpkt(link, id, act, r1, r2, a1, a2, a3, a4, a5, a6)

This routine is a substitute sendpkt for multi-event tasks.  It can
only be called when running as a non root coroutine of a task.  A
packet-coroutine pair is placed in pktlist before dispatching the
packet (using qpkt) so that when the packet returns to this task this
coroutine can be reactivated.  The globals cis, cos, pvsline and
currentdir are saved and restored by cosendpkt.

Multi-event mode is entered by executing

     pktlist, sendpkt := 0, cosendpkt

Re-implemented by MR 7/6/02, futher modified MR 7/5/03
*/

LET cosendpkt(link, id, act, r1, r2, a1, a2, a3, a4, a5, a6) = VALOF
{ // The following local variables form the pktlist node.
  // Functions other than cosendpkt may manipulate it
  // so its format must not change.
  LET next,      pkt,     co, ocis, ocos, ocurrentdir =
      pktlist, @link, currco,  cis,  cos,  currentdir


//sawritef("DLIB cosendpkt: sending pkt=%n from %n to %n pktlist=%n*n",
//          @link, taskid, id, pktlist)
//abort(1000)

  // Safety check -- cosendpkt cannot be called from the root coroutine
  IF currco!co_parent<=0 DO
  { sawritef(
     "DLIB co=%n T%n: can't cosendpkt %n(id=%n,type=%n) from root coroutine*n",
       currco, taskid, pkt, id, act)
    abort(999)
  }

  pktlist := @next       // Insert it at the head of pktlist

//sawritef("DLIB:   co=%n: cosendpkt %n(id=%n,type=%n) calling cowait*n",
//  currco, pkt, pkt!pkt_id, pkt!pkt_type)

//sawritef(
//   "DLIB: co=%n T%n: cosendpkt calling qpkt %n(id=%n,type=%n, arg1=%n,arg2=%n)*n",
//     currco, taskid, pkt, id, act, a1, a2)

  UNLESS qpkt(pkt) DO
  { sawritef("DLIB co=%n: cosendpkt -- qpkt failure*n", currco)
    abort(181)
  }

  { LET p = cowait() // Safety check -- we must resume with the
    IF p=pkt BREAK   // expected packet.
    sawritef("DLIB co=%n T=%n: cosendpkt: received wrong pkt=%n should be %n*n",
              currco, taskid,  p, pkt)
    abort(182)
  } REPEAT

//sawritef("DLIB:   co=%n: cosendpkt %n(res1=%n,res2=%n) resumes*n",
//  currco, pkt, pkt!pkt_res1, pkt!pkt_res2)

  // Restore the saved globals
  cis, cos, currentdir := ocis, ocos, ocurrentdir 

  result2 := r2
  RESULTIS r1
}

/* cptr := findpkt(pkt)

This routine searches the pktlist for the specified packet. If found
the list item is dequeued and a pointer to the coroutine is
returned. Zero is returned if the packet is not found in pktlist.
*/

AND findpkt(pkt) = VALOF
{ LET a = @pktlist
//sawritef("DLIB findpkt: task=%n from %n => ", taskid, pkt!pkt_id)

  // Search pktlist for the relevant item
  { LET p = !a
    UNLESS p DO
    { //sawritef("not found*n")
      RESULTIS 0   // Not found
    }
    IF p!1 = pkt DO
    { //sawritef("%n*n", p)
      !a := !p            // Remove from pktlist
      RESULTIS p!2        // The coroutine
    }
    a := p
  } REPEAT
}


LET newdcb() = VALOF // Create a TCP device
{ LET dcb = getvec(Dcb_upb)
//sawritef("TCPHAND: newdcb: dcb=%n*n", dcb)
  UNLESS dcb RESULTIS 0
  FOR i = 0 TO Dcb_upb DO dcb!i := 0
  dcb!Dcb_type := Devt_tcpdev
//sawritef("TCPHAND: newdcb: calling createdev(dcb)*n")
  UNLESS createdev(dcb) DO { freevec(dcb); RESULTIS 0 }
  Dcb_intson!dcb := TRUE
  RESULTIS dcb
}

LET start ( init_pkt ) BE
{ LET pkt, dcb = 0, 0
//sawritef("TCPHAND: start entered*n")

  tracing := FALSE
  //tracing := TRUE

  set_process_name("TCP_Handler") // MR 3/2/03

  IF tracing DO sawritef("tcphand: Tracing is on*n")

  tcplist := 0        // Initially there are no connections
  workcofree := FALSE // workco is not yet ready for work
  workq := 0          // List request packets for workco
  connq := 0          // List packets ready to be processed because the
                      // connections they were waiting for have been
                      // established

  // Create the TCP device used by workco for ipaddr lookup etc
  dcb := newdcb()
  UNLESS dcb DO
  { sawritef("TCPHAND: unable to create TCP address device*n")
    abort(999)
  }
  finddevid := dcb!Dcb_devid
//sawritef("TCPHAND: finddevid=%n*n", finddevid)

  workco := createco(workcofn, 500)
  UNLESS workco DO
  { sawritef("TCPHAND: unable to create workco*n")
    abort(999)
  }

  workcofree := TRUE       // workco is now ready for work
//sawritef("workcofree = %s*n", workcofree -> "TRUE", "FALSE")

  qpkt(init_pkt)  // Return startup pkt

  // Change to multi-event mode.
  pktlist, sendpkt := 0, cosendpkt

  IF tracing DO sawritef("tcphand: Entering the main loop*n")

  { // Start of main event loop for TCPHAND

    WHILE workcofree & workq DO
    { pkt := workq        // A workco request that can now be processed
      workq := !workq     // De-queue the first workq packet
      callco(workco, pkt)
    }

    // There are currently no workco requests that can be processed.
    TEST connq 
    THEN { pkt := connq    // A request that can now be processed because the
           connq := !connq // connection it was waiting for has just
                           // been fully established.
//sawritef("TCPHAND: main loop  pkt=%n dequeued from connq*n", pkt)
         }
    ELSE { LET co = ?
//sawritef("TCPHAND: main loop calling taskwait()*n")
           pkt := taskwait()
           co  := findpkt(pkt)
           IF co DO { callco(co, pkt); LOOP }
         }

    handlepkt(pkt)
//sawritef("TCPHAND: main loop  workcofree=%n workq=%n connq=%n*n",
//          workcofree, workq, connq)
  } REPEAT
}

AND handlepkt(pkt) BE SWITCHON pkt!pkt_type INTO
{ // The following four requests are handled by workco when it is free

  CASE Action_debug:      // Dump tcp blocks
       dumptcplist()
       returnpkt(pkt, TRUE, 0)
       RETURN


  CASE Action_findinput:  // a1: scb a3: name
  CASE Action_findoutput: // a1: scb a3: name
  CASE Action_close:      // a1: scb
  CASE Tcp_accept:        // a1: sock a2: tcp a4: timeout
       TEST workcofree
       THEN { // The request can be handled immediately
//sawritef("*nTCPHAND: handlepkt pkt %n(%i2,%n) given to workco*n",
//          pkt, pkt!pkt_id, pkt!pkt_type)
              callco(workco, pkt)
            }
       ELSE { // Workco is busy so append the packet to workq
              LET p = @workq
              WHILE !p DO p := !p
              !p, !pkt := pkt, 0
//sawritef("*nTCPHAND: handlepkt pkt %n(%i2,%n) appended to workq*n",
//              pkt, pkt!pkt_id, pkt!pkt_type)
              // This packet will be processed when workco becomes free.
            }
       RETURN


  CASE Action_getremipaddr: // arg1: scb
       tcpgetremipaddr(pkt)
       RETURN


  CASE Action_read:
       // This action arises from a replenish call in the client task
       // arg1: scb

       IF tracing DO sawritef("TCPHAND: Action_read pkt=%n*n", pkt)

       // Attempt to read via a connection item by issuing a recv
       // request to the appropriate TCP device. Tcpread returns
       // immediately without returning the packet to the client.
       // This is done when the recv packet comes back from the
       // TCP device.
       tcpread(pkt)
       //pr()
       RETURN
  
  CASE Tcp_recv:  // Packet returned from a TCP device
                  // indicating the completion of a recv request.
     { LET n        = pkt!pkt_res1
       LET res2     = pkt!pkt_res2
       LET clientid = pkt!pkt_arg5
       LET scb      = pkt!pkt_arg6

       IF tracing DO
       { sawritef("TCPHAND: Tcp_recv pkt=%n back from device, n=%n clientid=%n*n",
                   pkt, n, clientid)
         sawritef("TCPHAND: sending pkt=%n to task %n buf=%n n=%n len=%n*n",
                   pkt, clientid, pkt!pkt_arg2, n, pkt!pkt_arg3)
       }

       // Update the scb and return the pkt to client task
       scb!scb_pos  := 0
       scb!scb_end  := n
       pkt!pkt_id   := clientid
       pkt!pkt_res1 := n>0  // TRUE if at least one character received
       qpkt(pkt)            // Send pkt back to the client task
       RETURN
     }

  CASE Action_write:
       // This action arises from a deplete call in the client task
       // arg1: scb

       IF tracing DO sawritef("TCPHAND: Action_write pkt=%n*n", pkt)

       // Attempt to write via a connection item by issuing a send
       // request to the appropriate TCP device. Tcpwrite returns
       // immediately without returning the packet to the client.
       // This is done when the send packet comes back from the
       // TCP device.
       tcpwrite(pkt)
       //pr()
       RETURN

  CASE Tcp_send:  // Packet returned from the TCP write device
                  // indicating the completion of Action_write
     { LET rc       = pkt!pkt_res1
       LET res2     = pkt!pkt_res2
       LET clientid = pkt!pkt_arg5
       LET scb      = pkt!pkt_arg6

       IF tracing DO
       { sawritef("TCPHAND: Tcp_send pkt back from device, rc=%n clientid=%n*n",
                  rc, clientid)
       }
       // Update scb and return pkt to client task
       scb!scb_pos  := 0
       scb!scb_end  := 0
       pkt!pkt_id   := clientid
       pkt!pkt_res1 := rc>=0
       qpkt(pkt)
       RETURN
     }


  DEFAULT:  // Unknown or unimplemented operation
       sawritef("TCPHAND: unknown type=%n arg1=%n*n",
                 pkt!pkt_type, pkt!pkt_arg1)
       abort(306)
       returnpkt(pkt, 0, 0)
       RETURN
}

AND dumptcplist() BE // Added by MR 26/1/04
{ LET t = tcplist

  sawrch('*n')

  WHILE t DO
  { sawritef("rdev=%i3 wdev=%i3 refs=%i2 state=%n %s*n",
              t!tcp_rddevid, t!tcp_wrdevid, t!tcp_refcount,
              t!tcp_state, @t!tcp_namebase)
    t := t!tcp_link
  }

  sawrch('*n')
}

// Workcofn is the main function of the coroutine (workco) that
// processes findinput, findoutput and close requests.
// It also deals with the Tcp_accept packet when it returns from a
// TCP device in the course of opening a stream. Each request may
// involve several interactions with the find device (with id finddevid)
// before the processing of the request is complete.

// When workco is ready to start processing a new request, the variable
// workcofree is TRUE and the new request can be dispatched to it by the
// call: callco(workco, pkt). If, on the other hand, it is still busy
// processing a previous request, workcofree will be FALSE and 
// request packets will be appended to the queue workq. Workco will
// complete processing all request packets in workq before setting
// workcofree to TRUE and suspending itself.

// Workco sends packets to the find device using cosendpkt and when, such
// packets return to TCPHAND, they are recognised and passed immediately
// to workco.

// Operations, other than accept requests, that are concerned with
// opening and closing TCP streams do not typically involve long delays
// and are done using the find device. An accept request may be blocked
// for an long time waiting for a connection to be established.
// Such a request uses a device belonging to the TCP connection item. 

AND workcofn(pkt) BE
{ // Start processing a workco request, these have types:

  // Action_findinput   Client request to open a connection for input
  // Action_findoutput  Client request to open a connection for output
  // Action_close       Client request to close a connection
  // Tcp_accept         Packet returned from a TCP device indicating
  //                    that a connection has been accepted.
 
  workcofree := FALSE // Until the processing of this request is complete.
//sawritef("workcofree = %s*n", workcofree -> "TRUE", "FALSE")

  SWITCHON pkt!pkt_type INTO
  { CASE Action_findinput:    // args: scb   -    name
         IF tracing DO
           sawritef("*nTCPHAND: workco: findinput file %s*n", pkt!pkt_arg3)
         tcpfindstream(pkt, tcprdfn, 0)
         IF tracing DO
           sawritef("*nTCPHAND: workco: returned from tcpfindstream*n")
         //pr()
         workcofree := TRUE
         RETURN

    CASE Action_findoutput:   // args: scb   -    name
         IF tracing DO
           sawritef("*nTCPHAND: workcofn: findoutput file %s*n", pkt!pkt_arg3)
         tcpfindstream(pkt, 0, tcpwrfn)
         IF tracing DO
           sawritef("*nTCPHAND: workcofn: returned from tcpfindoutput*n")
         //pr()
         workcofree := TRUE
         RETURN

    CASE Action_close:        // arg: scb
         IF tracing DO
           sawritef("TCPHAND: close scb %n*n", pkt!pkt_arg1)
         tcpendfn(pkt)
         //pr()
         workcofree := TRUE
         RETURN

    CASE Tcp_accept:          // a1: sock a2: tcp
       // An accept packet returned from a tcp device
       // res1=0   res2=-1  Connection closed by remote host
       // res1=0   res2=-2  Accepted timed out 
       // res1=0   res2=-3  No connection yet (polling)
       // res1=0   res2>0   Error
       // res1~=0           res1 is the connection socket

//sawritef("TCPHAND: handlepkt: accept pkt returned with res1=%n res2=%n*n",
//           pkt!pkt_res1, pkt!pkt_res2)

       IF pkt!pkt_res1 DO // Connection established
       { LET sock     = pkt!pkt_res1  // The connection socket
         LET ipaddr   = pkt!pkt_res2  // ipaddr of remote machine
         LET lsock    = pkt!pkt_arg1  // The listening socket
         LET tcp      = pkt!pkt_arg2  // The control block
         LET rc       = 0


//sawritef("TCPHAND: accept pkt returned from dev %n from ipaddr=%x8 sock=%n*n",
//                   pkt!pkt_id, ipaddr, sock)

         freevec(pkt) // Return the packet to free store

         IF sock<0 DO
         { sawritef("TCPHAND: accept failed*n")
           abort(998)           
         }

         IF tracing DO
           sawritef("TCPHAND: workco: connection established ipaddr %n.%n.%n.%n sock %n*n",
                    ipaddr>>24&255,ipaddr>>16&255,ipaddr>>8&255,ipaddr&255, sock)
//pr()

         tcp!tcp_sock      := sock   // The connection socket
         tcp!tcp_remipaddr := ipaddr // The remote IP address
         tcp!tcp_state     := 3      // The connection is now established

         // Release any packets that were waiting for this accept to happen
         // by appending them all to the end of workq.

         IF tcp!tcp_q DO
         { LET p = @connq        // Append them to connq
//sawritef("TCPHAND: workco: appending tcp!tcp_q=%n onto the end of connq*n",
//          tcp!tcp_q)
           WHILE !p DO p := !p
           !p := tcp!tcp_q
           tcp!tcp_q := 0
         }

         // Close the listening socket
         IF tracing DO
           sawritef("TCPHAND: workco: closing the listening socket %n*n", lsock)
         rc := sendpkt(-1, finddevid, Tcp_close, 0, 0, lsock)

         IF rc<0 DO
         { sawritef("TCPHAND: workco: close listening socket %n failed*n", lsock)
           abort(999)
         }

         workcofree := TRUE
         RETURN
       }

       // res1=0   res2=-1  Connection closed by remote host
       // res1=0   res2=-2  Accept timed out 
       // res1=0   res2=-3  No connection yet (polling)
       // res1=0   res2>0   Error

       // Let any pending packet have a go
       { LET tcp = pkt!pkt_arg2
         LET res2 = pkt!pkt_res2
         LET p = tcp!tcp_q
         tcp!tcp_q := 0

//sawritef("TCPHAND: accept pkt from dev=%n, res1=%n res2=%n*n",
//                 pkt!pkt_id, pkt!pkt_res1, pkt!pkt_res2)

         freevec(pkt) // Return the packet to free store

         // Return all pending packets with res1=0 and res2 set

         WHILE p DO
         { LET next = !p
           !p := -1
           returnpkt(p, 0, res2)
           p := next
         }

         tcp!tcp_state := 1 // Put back to listening state
//sawritef("TCPHAND: state := 1 tcp queue=%n*n", tcp!tcp_q)
//abort(1000)

         workcofree := TRUE
         RETURN
       }

    DEFAULT:  // Unknown or unimplemented operation for workco
         sawritef("TCPHAND: illegal op %n for workco*n", pkt!pkt_type)
         abort(306)
         returnpkt(pkt, 0, 1)
         workcofree := TRUE
         RETURN
  }

  // Should never reach here
  sawritef("TCPHAND: Programming error*n")
  abort(999)
  workcofree := TRUE
}

// Functions to put in SCBs -- these will be called in the context of
// the client's task (and global vector)
AND tcprdfn   (scb) = sendpkt(notinuse, scb!scb_task, Action_read,  0, 0, scb)
AND tcpwrfn   (scb) = sendpkt(notinuse, scb!scb_task, Action_write, 0, 0, scb)
AND tcpclosefn(scb) = sendpkt(notinuse, scb!scb_task, Action_close, 0, 0, scb)

// tcpfindstream opens an input or output stream attached to a TCP
// connection. It is only called within the workco coroutine.
// It fills in the given scb appropriately.
// If successful, it returns TRUE.
// On failure,    it returns FALSE with an error code in result2.

// CASE 1.  
// If the remote host is given it will make the following requests
// to finddevid:

//     name2ipaddr
//     name2port
//     socket

// It will then create a DCB for input or output, if necessary.

// Finally it will issue a connect request and return the original
// request packet to the client. This will usually succeed or fail
// quickly or timeout in 5 seconds.


// CASE 2.
// It the remote host is not given it will issue listen and accept
// requests to the TCP device and return the packet probably before
// a connection has been established. Subsequent read or write requests
// will be held up until the connection is actually made (by some remote
// host), or there is a timeout or fault.

AND tcpfindstream(pkt, rdfn, wrfn) BE
{ LET scb    = pkt!pkt_arg1
  LET name   = pkt!pkt_arg3
  LET sock, devid, rc = 0, 0, 0
  LET ipaddr, port = 0, 0
  LET tcp    = findtcp(name, pkt)
  LET type   = result2
  LET buflen = 0
  LET buf    = 0
//sawritef("TCPHAND: tcpfindstream: %s for task %n*n", name, pkt!pkt_id) 
  UNLESS tcp DO
  { // Error
    returnpkt(pkt, FALSE, result2)
    RETURN
  }

  buflen := type=scbt_tcp -> tcpblen, netblen
  buf    := getvec(buflen/bytesperword)
 
//sawritef("TCPHAND: tcpfindstream: tcp=%n buf=%n size=%n allocated*n",
//          tcp, buf, buflen/bytesperword)

  scb!scb_type   := type
  scb!scb_task   := taskid
  scb!scb_buf    := buf
  scb!scb_rdfn   := rdfn
  scb!scb_wrfn   := wrfn
  scb!scb_endfn  := tcpclosefn
  scb!scb_fd     := tcp
  scb!scb_pos    := 0
  scb!scb_end    := 0
  scb!scb_bufend := buflen

  UNLESS tcp & buf DO
  { // findtcp or getvec failed
    sawritef("TCPHAND: tcpfindstream: findtcp or getvec failed*n")
    IF tcp DO closetcp(tcp)
    IF buf DO { freevec(buf); scb!scb_buf := 0 }
    returnpkt(pkt, FALSE, result2)
    RETURN
  }

  ipaddr, port := tcp!tcp_ipaddr, tcp!tcp_port

//sawritef("TCPHAND: tcpfindstream: tcp=%n scb=%n ip=%x8 port=%n*n",
//          tcp, scb, ipaddr, port)
//sawritef("TCPHAND: tcpfindstream: rdfn=%n tcp!tcp_rddcb=%n*n",
//          rdfn, tcp!tcp_rddcb)

  // Create a read TCP device, if necessary. 
  IF rdfn & tcp!tcp_rddcb=0 DO
  { LET dcb = newdcb()
    UNLESS dcb DO
    { sawritef("TCPHAND: tcpfindstream: unable to create a rd DCB*n")
      abort(999)
      GOTO bad
    }
//sawritef("TCPHAND: tcpfindstream: creating rd dev dcb=%n*n", dcb)
//sawritef("TCPHAND: tcpfindstream: creating rd dev %n*n", dcb!Dcb_devid)
    tcp!tcp_rddcb, tcp!tcp_rddevid := dcb, dcb!Dcb_devid
//sawritef("TCPHAND: tcpfindstream: created rd dev %n*n", dcb!Dcb_devid)
  }

//sawritef("TCPHAND: tcpfindstream: wrfn=%n tcp!tcp_wrdcb=%n*n",
//          wrfn, tcp!tcp_wrdcb)
  // Create a TCP write device, if necessary. 
  IF wrfn & tcp!tcp_wrdcb=0 DO
  { LET dcb = newdcb()
    UNLESS dcb DO
    { sawritef("TCPHAND: tcpfindstream: unable to create a wr DCB*n")
      abort(999)
      GOTO bad
    }
    tcp!tcp_wrdcb, tcp!tcp_wrdevid := dcb, dcb!Dcb_devid
//sawritef("TCPHAND: tcpfindstream: created wr dev %n*n", dcb!Dcb_devid)
  }

  // It is currently not connecting so we will make the connection
  // by calling

  //     socket
  //     listen (with a timeout of 5 seconds)

//sawritef("TCPHAND: tcpfindstream: tcp!tcp_state=%n*n", tcp!tcp_state)

  IF tcp!tcp_state=0 DO
  { // We must start to make a connection
    tcp!tcp_state := 1   // Stop anyone else from doing this
//sawritef("TCPHAND: state := 1*n")
//abort(1001)

    // state will become 3 when a connection is fully established.

    // Create a socket for the communication channel. This should respond
    // quickly so there is no need to use a different device.
//sawritef("TCPHAND: tcpfindstream: using dev %n to create socket*n", finddevid)
    sock := sendpkt(-1, finddevid, Tcp_socket, 0, 0)
    UNLESS sock>0 DO
    { sawritef("TCPHAND: tcpfindstream: unable to create a socket*n")
      abort(999)
      GOTO bad
    }
//sawritef("TCPHAND: tcpfindstream: Just allocated socket %n*n", sock)

    tcp!tcp_sock := sock  // Save the socket in the control block

    // Now attempt to make the connection

    IF ipaddr DO
    { // Make a connection to a specified host
//sawritef("TCPHAND: tcpfindstream: Issuing connect request ipaddr=%x8 port=%n*n",
//          ipaddr, port)
      // This could take a while if the connection cannot be made.
      // All other workco work will be held up ??????????????????????????
      // We need to use a shortish timeout, eg 5 or 10 seconds.
      // Try a timeout of 5000 msecs
      rc := sendpkt(-1, finddevid, Tcp_connect, 0, 0, sock, ipaddr, port, 5000)
      IF rc<0 DO
      { //sawritef("TCPHAND: tcpfindstream: connect rc=%n ipaddr=%x8 port=%n*n",
        //          rc, ipaddr, port)
//abort(1000)
        GOTO bad
      }

      tcp!tcp_remipaddr  := ipaddr // IP address of the remote machine

//sawritef("*nTCPHAND: findstream: connection established on socket %n*n",
//          sock) 
      tcp!tcp_state  := 3  // The connection is now fully established.

      // Release packets waiting in tcp_q by appending them to connq.
      // These are read, write, getremipaddr or close??? packets
      IF tcp!tcp_q DO // List of packets waiting for state=3
      { LET p = @connq
//sawritef("TCPHAND: workco: appending tcp!tcp_q=%n onto the end of connq*n",
//          tcp!tcp_q)
        WHILE !p DO p := !p
        !p := tcp!tcp_q
        tcp!tcp_q := 0
//sawritef("*nTCPHAND: findstream: packets appended to connq*n")
      }
    
//sawritef("TCPHAND: tcpfindstream: returning pkt=%n to client %n*n",
//          pkt, pkt!pkt_id)
      // Return the packet to the client.
      returnpkt(pkt, TRUE, 0)
      RETURN
    }

// No host specified so listen and accept a connection from a remote host.

//sawritef("TCPHAND: tcpfindstream: setting reuseaddr sock=%n*n", sock)
    rc  := sendpkt(-1, finddevid, Tcp_reuseaddr, 0, 0, sock, 1)
    IF rc<0 DO
    { sawritef("TCPHAND: reuseaddr failed*n")
      GOTO bad
    }

    // Listen for a connection from an unspecified host and await a
    // connection request from some remote machine.

//sawritef("TCPHAND: tcpfindstream: sending bind request sock=%n ip=%x8 port=%n*n",
//           sock, ipaddr, port)
    rc := sendpkt(-1, finddevid, Tcp_bind, 0, 0, sock, ipaddr, port)
    IF rc<0 DO
    { sawritef("TCPHAND: findstream: bind failed*n")
      GOTO bad
    }

//sawritef("TCPHAND: tcpfindstream: sending listen request sock=%n*n", sock)
    rc  := sendpkt(-1, finddevid, Tcp_listen, 0, 0, sock, 5)
    IF rc<0 DO
    { sawritef("TCPHAND: listen failed*n")
      GOTO bad
    }

//sawritef("TCPHAND: tcpfindstream: listen returned dev %n socket %n*n",
//          finddevid, sock)

    returnpkt(pkt, TRUE, 0) // Stream scb is now listening
                            // but has not yet accepted a connection.
    RETURN
  }

  // We are in one of the following states:

  // state 1 -- A previous call of findinput or findoutput is
  //            currently trying to connect the stream, typically
  //            having called listen.
  // state 2 -- Waiting for the reply fromaccept call.
  // state 3 -- The connection is already fully established.

  // The state is 1, 2 or 3 so either a previous call of findinput
  // or findoutput is connecting the stream, or the connection is
  // already fully established. So just return the packet to the client.

//sawritef("TCPHAND: tcpfindstream: returning pkt=%n to client %n*n",
//                pkt, pkt!pkt_id)
//sawritef("TCPHAND: tcpfindstream: state=%n name=%s*n", tcp!tcp_state, name)

  returnpkt(pkt, TRUE, 0)
  RETURN

bad:
//sawritef("TCPHAND: tcpfindstream: reached label bad: name=%s buf=%n tcp=%n*n",
//          name, buf, tcp)
//pr()
//abort(1000)
  IF buf DO freevec(buf)
//IF tcp DO sawritef("TCPHAND: tcpfindstream: calling closetcp at label bad*n")
  IF tcp DO closetcp(tcp)
  // Return the packet indicating failure to the client task
//sawritef("TCPHAND: tcpfindstream: calling returnpkt at label bad*n")
  returnpkt(pkt, FALSE, 0)
}


AND chkconn(pkt) = VALOF
{ // If the connection is fully established it returns TRUE

  // If the connection is not fully established it returns the
  // packet with res1=0  res2>0   for error
  //         and res1=0  res2=-1  connection closed by remote host
  //         and res1=0  res2=-2  for timeout
  //         and res1=0  res2=-2  no connection yet (polling)
  // then returns FALSE
  
  LET scb   = pkt!pkt_arg1
  LET tcp   = scb!scb_fd
  LET state = tcp!tcp_state

  IF state=3 RESULTIS TRUE // Connection fully established

  // Append the packet onto the end of tcp_q so that it can
  // be deal with when the connection is fully established
  { LET a = @tcp!tcp_q
    WHILE !a DO a := !a
    !pkt := 0
    !a := pkt
  }

  // Nothing more to do if accept still outstanding
  IF state=2 RESULTIS FALSE

  IF state=4 DO
  {
//sawritef("TCPHAND: chkconn: error state=4*n")
    returnpkt(pkt, 0, 1)
    RESULTIS FALSE
  }

  IF state=0 DO
  {
//sawritef("TCPHAND: chkconn: error state=0*n")
    returnpkt(pkt, 0, 1)
    RESULTIS FALSE
  }

  IF state=1 DO
  { // Listen has been called but accept is not outstanding
    LET devid = ?
    LET sock = tcp!tcp_sock

//sawritef("TCPHAND: chkconn: state=%n*n", state)

    tcp!tcp_state := 2  // Waiting for accept to complete

    // Choose a suitable device other than finddevid for the accept request
    devid := tcp!tcp_wrdevid
    UNLESS devid DO devid := tcp!tcp_rddevid
    UNLESS devid DO
    { sawritef("TCPHAND: chkconn: bad devid*n")
      abort(999)
      GOTO bad
    }

//sawritef("TCPHAND: chkconn: accept request=>dev %n socket %n*n", devid, sock)
    // Send an accept request to this device
    { LET accpkt = getvec(pkt_arg6) // Allocate a packet for the accept request
      UNLESS accpkt DO
      {
//sawritef("TCPHAND: chkconn: getvec failure socket %n*n", sock)
        returnpkt(pkt, 0, 2)        // getvec failure
        RETURN
      }

      accpkt!pkt_link := notinuse
      accpkt!pkt_id   := devid
      accpkt!pkt_type := Tcp_accept
      accpkt!pkt_arg1 := sock            // The listening socket
      accpkt!pkt_arg2 := tcp
      accpkt!pkt_arg4 := scb!scb_timeout // MR 4/11/03
      accpkt!pkt_arg5 := pkt             // MR 6/11/03
      qpkt(accpkt)

      // state is currently 2.

      // In due course some remote host may establish a connection on this port
      // or there may be an error or timeout. This will cause the
      // accpkt to be returned to this task. The state will then change
      // to 3 (connection fully established), 4 (timeout) or 5 (error)
      // and any packets waiting in tcp_q will be appended to connq
      // for processing.
    }
  }
  RESULTIS FALSE // Not yet fully connected

bad:
sawritef("TCPHAND: chkconn: reached label bad: tcp=%n*n", tcp)
//pr()
//abort(1000)
//  IF buf DO freevec(buf) // Do this somewhere else ?????????????
//IF tcp DO sawritef("TCPHAND: chkconn: calling closetcp at label bad*n")
  IF tcp DO closetcp(tcp)
  // Return the packet indicating failure to the client task
//sawritef("TCPHAND: waitcon: calling returnpkt at label bad*n")
  RESULTIS FALSE
}

// getremipaddr
//   res1=0   res2>0    error
//   res1=0   res2=-1   connection closed by remote host
//   res1=0   res2=-2   timeout
//   res1=0   res2=-3   no connection yet (polling)
//   res1~=0            res1 is the remote IP address
AND tcpgetremipaddr(pkt) BE IF chkconn(pkt) DO
{ // Connection fully established or timeout
  LET scb = pkt!pkt_arg1
  LET tcp = scb!scb_fd
//sawritef("*nTCPHAND: tcpgetremipaddr scb=%n tcp=%n*n", scb, tcp)
//sawritef("*nTCPHAND: tcpgetremipaddr returning pkt=%n(%n,%n) remipaddr=%x8*n",
//          pkt, pkt!pkt_id, pkt!pkt_type, tcp!tcp_remipaddr)
//sawritef("*nTCPHAND: tcpgetremipaddr state=%n*n", tcp!tcp_state)
//abort(2222)
  TEST tcp!tcp_state=3
  THEN returnpkt(pkt, tcp!tcp_remipaddr, 0) // Successful return
  ELSE returnpkt(pkt, 0, 1)  // Error
}

// tcpread 
// It calls chkconn to check whether the connection is fully established
// If it is not chkconn attempt to complete the connection, typically
// appending pkt to tcpq so that it may be processed in due course.
// If the connection is established it will perform the read and
// return the packet.
// If the connection timed out in accept it will return the packet
// indicating timeout.
AND tcpread(pkt) BE IF chkconn(pkt) DO
{ LET clientid = pkt!pkt_id     // The connection is fully established
  LET scb      = pkt!pkt_arg1
  LET buf      = scb!scb_buf
  LET n        = scb!scb_bufend
  LET tcp      = scb!scb_fd
  LET rddevid  = tcp!tcp_rddevid
  LET sock     = tcp!tcp_sock

//sawritef("TCPHAND: tcpread: tcp=%n request %n bytes from sock %n*n", tcp, n, sock)
//abort(1000)

  // If the stream cannot be read from, return the packet
  // with an error indication
  UNLESS rddevid DO
  { pkt!pkt_res1 := FALSE // Error: cannot read from this stream
    pkt!pkt_res2 := 1     // Error code ???????
    qpkt(pkt)
    RETURN
  }

//sawritef("TCPHAND: tcpread: sending recv pkt=%n sock %n state=%n*n",
//    pkt, sock, tcp!tcp_state)
//sawritef("TCPHAND: tcpread: sending recv pkt=%n sock=%n tcp=%n*n",
//    pkt, sock, tcp)

  // Send this pkt to the TCP read device
  // When it returns from the device, it will be redirected the client task.
  pkt!pkt_id   := rddevid
  pkt!pkt_type := Tcp_recv   // Receive upto n bytes into buf via sock.
  pkt!pkt_arg1 := sock
  pkt!pkt_arg2 := buf
  pkt!pkt_arg3 := n
  pkt!pkt_arg4 := scb!scb_timeout // Timeout value in msecs
  pkt!pkt_arg5 := clientid
  pkt!pkt_arg6 := scb
  qpkt(pkt)
}

AND tcpwrite(pkt) BE IF chkconn(pkt) DO
{ LET clientid = pkt!pkt_id     // The connection is fully established
  LET scb      = pkt!pkt_arg1
  LET buf      = scb!scb_buf
  LET n        = scb!scb_pos
  LET tcp      = scb!scb_fd
  LET wrdevid  = tcp!tcp_wrdevid
  LET sock     = tcp!tcp_sock

//sawritef("TCPHAND: tcpwrite: send %n bytes to sock %n*n", n, sock)
//FOR i = 0 TO n-1 DO sawritef("buf%%%i2 = %n '%c'*n", i, buf%i, buf%i)

  // If the stream cannot be written to, return the packet
  // with an error indication
  UNLESS wrdevid DO
  { pkt!pkt_res1 := 0
    pkt!pkt_res2 := 1
sawritef("TCPHAND: tcpwrite: not a TCP write device*n")
    qpkt(pkt)
    RETURN
  }

//sawritef("TCPHAND: tcpwrite: send data to socket %n*n", sock)

  // Send this pkt to the TCP write device.
  // When it returns from the device, it will be redirected back to the
  // client task.
  pkt!pkt_id   := wrdevid
  pkt!pkt_type := Tcp_send   // Send n bytes from buf via sock
  pkt!pkt_arg1 := sock
  pkt!pkt_arg2 := buf
  pkt!pkt_arg3 := n
  pkt!pkt_arg4 := scb!scb_timeout   // Timeout value in msecs
  pkt!pkt_arg5 := clientid
  pkt!pkt_arg6 := scb
  qpkt(pkt)
}

AND tcpendfn(pkt) BE
{ LET scb = pkt!pkt_arg1
  LET buf = scb!scb_buf
  LET tcp = scb!scb_fd
  LET res = closetcp(tcp)
//  sawritef("TCPHAND: tcpendfn buf = %n*n", buf); pr()
  IF buf DO
  {
//sawritef("TCPHAND: task %i2  tcpendfn: freeing buf = %n scb = %n*n", taskid, buf, scb)
    //freevec(buf)
    //scb!scb_buf := 0
  }
  returnpkt(pkt, res, result2) // Return the pkt to the client task
  //sawritef("TCPHAND: tcpendfn: closed scb %n*n", scb)
}

// findtcp(name, pkt)
// finds the connection control block in tcplist whose IP address and port
// number correspond to the given name. It searches tcplist. If a matching
// block is not found, it creates one and links it into tcplist.

// It does not create either the read or write TCP devices. This is left
// to tcpfindstream to deal with if necessary.

// This function is only called from within the findco coroutine.

// It returns:   0  on failure
//                  result2 = error code
//         or:  >0  a pointer to the connection
//                  result2 = the type (scbt_tcp or scbt_net)

AND findtcp(name, pkt) = VALOF
{ LET ipaddr, port, type = 0, 0, 0
  LET tcp      = tcplist
  LET devname  = VEC 32/bytesperword
  LET hostname = VEC 32/bytesperword
  LET portname = VEC 32/bytesperword
  LET pos      = splitname(devname, ':', name, 1)

  UNLESS compstring(devname, "NET") DO type := scbt_net
  UNLESS compstring(devname, "TCP") DO type := scbt_tcp
  UNLESS type DO { result2 := 2; RESULTIS 0 } // Bad TCP name
  hostname%0, portname%0 := 0, 0
  IF pos DO pos := splitname(hostname, ':', name, pos)
  IF pos DO pos := splitname(portname, ':', name, pos)
//sawritef("*nTCPHAND: findtcp: dev:'%s' host:'%s' port:'%s'*n", 
//          devname, hostname, portname)
  UNLESS hostname%0 DO hostname := 0
  UNLESS portname%0 DO portname := "9000"

//sys(Sys_tracing, TRUE)
  ipaddr := sendpkt(-1,finddevid,Tcp_name2ipaddr,0,0,hostname)
//sawritef("*nTCPHAND: findtcp: ipaddr:%x8*n", ipaddr)
  port   := sendpkt(-1,finddevid,Tcp_name2port,  0,0,portname)
//sawritef("*nTCPHAND: findtcp: port:%n*n", port)

  IF ipaddr=-1 | port=-1 DO
  { sawritef("*nTCPHAND: findtcp: Bad address host:%n port:%n*n", 
              ipaddr, port)
    result2 := 2 // Error -- bad name or port
    RESULTIS 0
  }

  WHILE tcp DO // Search for matching connection item
  { IF ipaddr = tcp!tcp_ipaddr & port = tcp!tcp_port DO
    { // A matching connection already exits
      tcp!tcp_refcount := tcp!tcp_refcount + 1
//sawritef("*nTCPHAND: findtcp: matching tcb %n found*n", tcp)
//sawritef("*nTCPHAND: findtcp: refcount=%n state=%n*n", 
//          tcp!tcp_refcount, tcp!tcp_state)
      result2 := type
      RESULTIS tcp
    }
    tcp := tcp!tcp_link
  }

  // Make a new connection control block
  tcp := getvec(tcp_upb)

  UNLESS tcp DO
  { sawritef("TCPHAND: findtcp: getvec failure*n")
    result2 := 2 // Error
    RESULTIS 0
  }

  // Initialise all its fields
  FOR i = 0 TO tcp_upb DO tcp!i := 0

  tcp!tcp_ipaddr   := ipaddr
  tcp!tcp_port     := port
  tcp!tcp_refcount := 1
  tcp!tcp_state    := 0  // No attempt at connection yet

  IF name%0>=namelen DO name%0 := namelen // Truncate name if necessary
  FOR i = 0 TO name%0 DO (tcp+tcp_namebase)%i := name%i

  // Link the connection into the head of tcplist
  !tcp    := tcplist
  tcplist := tcp

  result2 := type       // scbt_net or scbt_tcp
//sawritef("*nTCPHAND: findtcp: new tcp item %n created*n", tcp)
  RESULTIS tcp          // Successful return
}

// closetcp(tcp) decrements the reference count and, if it is
//               now zero, it closes the socket (using finddevid),
//               deletes the read and write devices (if any) and
//               returns the tcp control block to free store.
//               It runs in the workco coroutine.
//
// It returns:  TRUE  if the connection was deleted
//              FALSE otherwise

AND closetcp(tcp) = VALOF
{ LET p = @tcplist // The list is linked through element zero.
  LET refcount = tcp!tcp_refcount-1
  tcp!tcp_refcount := refcount

  IF refcount RESULTIS FALSE

  // The connection has no attached streams so it must be deleted

//sawritef("TCPHAND: deleting an unused connection item tcp=%n*n", tcp)

  { LET q = !p
    IF q=tcp DO
    { LET sock = tcp!tcp_sock
      LET rc = sendpkt(-1, finddevid, Tcp_close, 0, 0, sock)
      IF rc<0 DO
      { sawritef("TCPHAND: unable to close socket %n*n", sock)
        abort(999)
      }

      !p := !tcp      // Remove the connection from the list
      
//sawritef("TCPHAND: rddevid=%n wrdevid=%n*n", tcp!tcp_rddevid, tcp!tcp_wrdevid)

      // Delete the devices, if any
      IF tcp!tcp_rddevid DO deletedev(tcp!tcp_rddevid)
      IF tcp!tcp_wrdevid DO deletedev(tcp!tcp_wrdevid)
      IF tcp!tcp_rddcb   DO freevec  (tcp!tcp_rddcb)
      IF tcp!tcp_wrdcb   DO freevec  (tcp!tcp_wrdcb)
      freevec(tcp)    // and return its space
      RESULTIS TRUE
    }
    p := q
  } REPEATWHILE p

  sawritef("TCPHAND: closetcp connection not in the list*n")
  abort(999)
  RESULTIS FALSE
}

AND pr() BE
{ LET p = tcplist
  sawrch('*n')
  WHILE p DO
  { LET q = p!tcp_q
    sawritef("Connection: rd %i3 wr %i3 refs %n  name %s ipaddr=%x8*n",
              p!tcp_rddevid, p!tcp_wrdevid,
              p!tcp_refcount, p+tcp_namebase, p!tcp_ipaddr)
    sawritef("queue: ")
    WHILE q DO
    { sawritef(" %n", q!pkt_id)
      q := !q
    }
    sawrch('*n')
    p := !p
  }
}
