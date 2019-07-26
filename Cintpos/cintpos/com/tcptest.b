/*
This is a benchmark test for TCP/IP communication under Cintpos

Written by Martin Richards (c) February 2002

The Cintpos command tcptest has argument format: -n/K,-k/K,-s/K,-h/K,-t/S

It can be used to set up exercise  TCP/IP communication links
between two machines, one called the master and the other the slave.

                MASTER m/c                SLAVE m/c

                source -----------------> 8000+1
                7000+1 <----------------- copy
                copy   -----------------> 8000+2
                7000+2 <----------------- copy
                copy   -----------------> 8000+3
                  .                         .
                  .                         .
                       -----------------> 8000+K
                7000+K <----------------- copy
                sink

On the master machine the coroutine source generates a stream
consisting of n blocks of given size sending directed to port 8001 on
the slave machine. The copy coroutines copy data received on one port
to a port on the other machine, and sink just reads and discards data
from its port.

If the host argument is not given, the master and slave coroutines
run on the same machine. So, for example:

tcptest -n 10

which is equivalent to

tcptest -n 10 -k 50 -s 1000

will cause a stream of 10x1000 bytes to be directed at port 8001 which
will be copied to port 7001 and then to port 8002 until eventually the
data reaches the sink coroutine on port 7050 where it will be thrown
away. When all the data has been transferred, statistics of the run
including the total time taken will be output and the tcptest command
terminated

To run the benchmark on two different machines xxx and yyy, say, first
setup a slave on machine xxx by typing for example:

tcptest -k 50

Then, on machine yyy, type:

tcptest -n 10 -k 50 -s 1000 -h xxx

Again when all the data has been transferred, the tcptest commands on
both machines will terminate after outputing appropriate statistics.

The option -t causes the test to generate trace output.

The default values for -n -k and -s are 100, 50 and 1000 respectively.
*/


SECTION "tcptest"

GET "libhdr"

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

 magicno = 12345678
}

GLOBAL
{ mcopycov:   ug
  scopycov
  sourceco
  sinkco
  sockcount     // Number of open sockets
  initcount     // Number of coroutines still initialising
  tracing
}

LET watchaddr(a) BE
{ sawritef("watch address: %n*n", a)
  sys(Sys_watch, a)
}

LET start() BE
{ LET argv = VEC 50
  LET n, k, size, hostname = 100, 50, 1000, 0
  LET master = FALSE  // Set to TRUE if n>0
  LET slave  = FALSE  // Set to TRUE if n=0 or no host specified

  UNLESS rdargs("-n/K,-k/K,-s/K,-h/K,-t/S", argv, 50) DO
  { sawritef("Bad argument for TCPTEST*n")
    stop(20)
  }

  IF argv!0 & string_to_number(argv!0) DO n := result2
  IF argv!1 & string_to_number(argv!1) DO k := result2
  IF argv!2 & string_to_number(argv!2) DO size := result2
  IF argv!3 DO hostname := argv!3
  tracing := argv!4

  IF n>0 DO master := TRUE
  IF n=0 | hostname=0 DO slave := TRUE
  IF hostname=0 DO hostname := "localhost"

  mcopycov := getvec(k)  // Vector of master copy coroutines
  scopycov := getvec(k)  // Vector of slave copy coroutines

  UNLESS mcopycov & scopycov DO
  { sawritef("More space needed*n")
    IF mcopycov DO freevec(mcopycov)
    IF scopycov DO freevec(scopycov)
    stop(20)
  }

  FOR i = 0 TO k DO mcopycov!i, scopycov!i := 0, 0
  sourceco, sinkco := 0, 0

  sockcount, initcount := 0, 0

  IF master DO
     sawritef("*nRunning master with n=%n k=%n size=%n slave %s*n",
                                 n,   k,   size,   hostname)

  IF master DO
  { sawritef("Creating the sink coroutine*n")
    sinkco := initco(sink, 500, 7000+k)
  }

  IF slave DO
  { sawritef("Creating %n slave copy coroutines*n", k)
    FOR i = 1 TO k DO scopycov!i := initco(copy, 500, 8000+i, 7000+i)
  }

  IF master DO
  { IF k>1 DO sawritef("Creating %n master copy coroutines*n", k-1)
    FOR i = 1 TO k-1 DO mcopycov!i := initco(copy, 500, 7000+i, 8001+i)
  }

  IF master DO
  { sawritef("Creating the source coroutine*n")
    sourceco := initco(source, 500, n, size, hostname, 8001)
  }

  // Now run an event loop until all the coroutines
  // have initialised themselves

  { LET pkt = taskwait() // Hopefully a TCP packet owned by a coroutine
    
//    sawritef("init pkt type=%n to cortn=%n initcount=%n*n",
//              pkt_type!pkt, pkt_arg6!pkt, initcount)

//abort(1000)
    UNLESS pkt_arg5!pkt=magicno DO
    { sawritef("Bad magicno in packet*n")
      abort(999)
      BREAK
    }

    callco(pkt_arg6!pkt, pkt) // Give it to the owner
  } REPEATWHILE initcount

  sawritef("All coroutines are now initialised*n")

//abort(1000)
  TEST sourceco 
  THEN { sawritef("Data transmission started*n")
         callco(sourceco, 12345)  // Start the source running
       }
  ELSE sawritef("Running as slave*n")

  // Now run an event loop until all the coroutines have finished

  WHILE sockcount DO
  { LET pkt = taskwait() // Hopefully a TCP packet owned by a coroutine

//abort(1002, pkt)    
//    sawritef("work pkt type=%n to cortn=%n sockcount=%n*n",
//              pkt_type!pkt, pkt_arg6!pkt, sockcount)

    UNLESS pkt_arg5!pkt=magicno DO
    { sawritef("Bad magicno in packet*n")
      abort(999)
      BREAK
    }

    callco(pkt_arg6!pkt, pkt) // Give it to the owner
  }

  // Close down and delete the coroutines

  sawritef("Deleting all the coroutines*n")

  IF sourceco DO deleteco(sourceco)
  FOR i = 1 TO k DO
  { IF scopycov!i DO deleteco(scopycov!i)
    IF mcopycov!i DO deleteco(mcopycov!i)
  }
  IF sinkco DO deleteco(sinkco)

  freevec(mcopycov)
  freevec(scopycov)

  sawritef("tcptest finished*n")
}


AND newdcb(type) = VALOF
{ LET dcb = getvec(Dcb_upb)
  LET id = 0
  UNLESS dcb RESULTIS 0
  FOR i = 0 TO Dcb_upb DO dcb!i := 0
  Dcb_type!dcb := type
  id := createdev(dcb)
  UNLESS id DO { freevec(dcb); dcb := 0 }
//sawritef("newdcb: id = %n*n", id)
  Dcb_intson!dcb := TRUE
  RESULTIS dcb
}


AND source(args) = VALOF
{ LET n, size, hostname, port = args!0, args!1, args!2, args!3
  LET dcb = newdcb(Devt_tcpdev)
  LET id  = Dcb_devid!dcb  // OK even if dcb=0
  LET buf = getvec((size-1)/bytesperword)
  LET s, rc = 0, 0
  LET ipaddr = 0

  initcount := initcount+1

  // Put data into the buffer
  FOR i = 0 TO size-1 DO buf%i := "ABCDEFG*n"%((i&7)+1)

  IF tracing DO
     sawritef("Source cortn %n n=%n size=%n hostname=%s port=%n*n",
               currco, n, size, hostname=0 -> "0", hostname, port)
  
  UNLESS dcb DO
  { sawritef("source: Unable to create a DCB*n")
    RESULTIS 0
  }

  UNLESS hostname DO hostname := "localhost"

  ipaddr  := cosendpkt(-1, id, Tcp_name2ipaddr, 0, 0, hostname, 0, 0, 0, 0, 0)

  IF tracing DO sawritef("source: ipaddr=%x8 port=%n*n", ipaddr, port)

  sockcount := sockcount+1
  s := cosendpkt(-1, id, Tcp_socket, 0, 0, 0, 0, 0, 0, 0, 0)
  IF s<0 DO
  { sawritef("source: socket failed*n")
    sockcount := sockcount-1
    RESULTIS -1
  }
  IF tracing DO sawritef("source: new socket %n*n", s)
  IF tracing DO sawritef("source: Now initialised using Socket s=%n*n", s)
  initcount := initcount-1

  UNLESS cowait()=12345 DO // Wait to be explicitly started
    sawritef("source: startup error*n")
  // Source started
  IF tracing DO sawritef("source: started*n")
  //sys(Sys_usleep, 5000)

  // try to connect with a 5000 msecs timeout
  rc := cosendpkt(-1, id, Tcp_connect, 0, 0, s, ipaddr, port, 5000, 0, 0)

  IF rc DO
  { sawritef("source: connect failed ipaddr=%x8 port=%n*n", ipaddr, port)
    RESULTIS -1
  }

  FOR i = 1 TO n DO
  { LET sent = 0
    IF tracing DO sawritef("source: sending %n bytes to port %n*n", size, port)
    sent := cosendpkt(-1, id, Tcp_send, 0, 0, s, buf, size, 0, 0, 0)
//    sawritef("Source: sent %n*n", sent)
  }

  rc := cosendpkt(-1, id, Tcp_close, 0, 0, s, 0, 0, 0, 0, 0)
  IF rc DO sawritef("Source: close failed*n")
  sockcount := sockcount-1

//  sawritef("source: deleting device %n*n", id)
  deletedev(id)
  freevec(dcb)
  IF tracing DO sawritef("source: device %n deleted*n", id)
  freevec(buf)
  RESULTIS 0
}

AND copy(args) = VALOF
{ LET iport, oport = args!0, args!1
  LET indcb  = newdcb(Devt_tcpdev)
  LET outdcb = newdcb(Devt_tcpdev)
  LET inid   = Dcb_devid!indcb
  LET outid  = Dcb_devid!outdcb
  LET bytecount = 0
  LET buf    = VEC 256

  LET ipaddr = 0
  LET s, in, out, rc = 0, 0, 0, 0


  initcount := initcount+1

  IF tracing DO sawritef("Copy cortn %n on port %n*n", currco, iport)
  UNLESS indcb DO
  { sawritef("Can't create a DCB*n")
    RESULTIS 0
  }

  s := listen(inid, iport)

  initcount := initcount-1

  IF tracing DO sawritef("copy: waiting to accept a connection on port %n*n", iport)

  sockcount := sockcount+1
  in := cosendpkt(-1, inid, Tcp_accept, 0, 0, s, 0, 0, 0, 0, 0)
  ipaddr := result2
  IF tracing DO sawritef("copy: new socket %n*n", in)
  IF tracing DO sawritef("copy: connection accepted on port %n from %x8*n",
                          iport, ipaddr)




  sockcount := sockcount+1
  out := cosendpkt(-1, outid, Tcp_socket, 0, 0, 0, 0, 0, 0, 0, 0)
  IF out<0 DO
  { sawritef("copy: socket failed*n")
    sockcount := sockcount-1
    RESULTIS -1
  }
  IF tracing DO sawritef("copy: new socket %n*n", out)
  IF tracing DO sawritef("copy: using output Socket s=%n*n", out)

  // try to connect with a 5000 msecs timeout
  rc := cosendpkt(-1, outid, Tcp_connect, 0, 0, out, ipaddr, oport, 5000, 0, 0)
  IF rc DO
  { sawritef("source: connect failed ipaddr=%x8 port=%n*n", ipaddr, oport)
    RESULTIS -1
  }

  IF tracing DO sawritef("copy: Connection made to port %n via socket s=%n*n",
                          oport, out)



  { LET n = cosendpkt(-1, inid, Tcp_recv, 0, 0, in, buf, 1024, 0, 0, 0)
    IF tracing DO sawritef("copy: received %n bytes from port %n*n", n, iport)
    UNLESS n BREAK    // Test for EOF
    IF tracing DO sawritef("copy: sending %n bytes to port %n*n", n, oport)
    n := cosendpkt(-1, outid, Tcp_send, 0, 0, out, buf, n, 0, 0, 0)
    bytecount := bytecount + n
  } REPEAT


  IF tracing DO sawritef("copy: %n->%n finishing after reading %n bytes*n", 
                          iport, oport, bytecount)

  rc := cosendpkt(-1, outid, Tcp_close, 0, 0, out, 0, 0, 0, 0, 0)
  IF rc DO sawritef("copy: close s1 failed*n")
  sockcount := sockcount-1

  rc := cosendpkt(-1, inid, Tcp_close, 0, 0, in, 0, 0, 0, 0, 0)
  IF rc DO sawritef("copy: close s1 failed*n")
  sockcount := sockcount-1

  rc := cosendpkt(-1, inid, Tcp_close, 0, 0, s, 0, 0, 0, 0, 0)
  IF rc DO sawritef("copy: close s failed*n")
  sockcount := sockcount-1

  deletedev(inid)
  freevec(indcb)
  deletedev(outid)
  freevec(outdcb)
  IF tracing DO sawritef("copy: %n->%n coroutine done after copying %n bytes*n",
                         iport, oport,  bytecount)
  RESULTIS 0
}

AND sink(args) = VALOF
{ LET iport = args!0
  LET s, s1, rc, ipaddr = 0, 0, 0, 0
  LET dcb = newdcb(Devt_tcpdev)
  LET id = Dcb_devid!dcb
  LET bytecount = 0
  LET buf = VEC 256

  initcount := initcount+1

  IF tracing DO sawritef("Sink cortn %n on port %n*n", currco, iport)
  UNLESS dcb DO
  { sawritef("Can't create a DCB*n")
    RESULTIS 0
  }

  s := listen(id, iport)

  initcount := initcount-1

  IF tracing DO sawritef("sink: waiting to accept a connection on port %n*n", iport)

  sockcount := sockcount+1
  s1 := cosendpkt(-1, id, Tcp_accept, 0, 0, s, 0, 0, 0, 0, 0)
  ipaddr := result2
  IF tracing DO sawritef("sink: new socket %n*n", s1)
  IF tracing DO sawritef("sink: accepted connection on port %n from %x8*n",
                         iport, ipaddr)

  { LET n = cosendpkt(-1, id, Tcp_recv, 0, 0, s1, buf, 1024, 0, 0, 0)
    IF tracing DO sawritef("sink: received %n bytes*n", n)
    UNLESS n BREAK    // Test for EOF
    bytecount := bytecount + n
  } REPEAT

  sawritef("sink finishing after reading %n bytes*n", bytecount)

  rc := cosendpkt(-1, id, Tcp_close, 0, 0, s1, 0, 0, 0, 0, 0)
  IF rc DO sawritef("sink: close s1 failed*n")
  sockcount := sockcount-1

  rc := cosendpkt(-1, id, Tcp_close, 0, 0, s, 0, 0, 0, 0, 0)
  IF rc DO sawritef("sink: close s failed*n")
  sockcount := sockcount-1

  deletedev(id)
  freevec(dcb)
  IF tracing DO sawritef("Sink coroutine done*n")
  RESULTIS 0
}

AND listen(id, port) = VALOF
// id is the id of a TCP device, port to the port to listen on.
// Result:  the connected socket
// result2: the sender's ipaddr
{ LET s, s1, rc = 0, 0, 0
  LET ipaddr = cosendpkt(-1, id, Tcp_name2ipaddr, 0, 0, 0, 0, 0, 0, 0, 0)

  sockcount := sockcount+1

  s  := cosendpkt(-1, id, Tcp_socket, 0, 0, 0, 0, 0, 0, 0, 0)
  IF s<0 DO
  { sawritef("listen: socket failed*n")
    sockcount := sockcount-1
    RESULTIS -1
  }
  IF tracing DO sawritef("listen: new socket %n*n", s)

  rc  := cosendpkt(-1, id, Tcp_bind, 0, 0, s, ipaddr, port, 0, 0, 0)
  IF rc<0 DO
  { sawritef("listen: bind failed*n")
    RESULTIS -1
  }

  rc  := cosendpkt(-1, id, Tcp_listen, 0, 0, s, 5, 0, 0, 0, 0)
  IF rc<0 DO
  { sawritef("listen: listen failed*n")
    RESULTIS -1
  }
  RESULTIS s
}

AND cosendpkt(link, id, type, r1, r2, a1, a2, a3, a4, a5, a6) = VALOF
{ LET p, q = @link, 0
//a5:=0; watchaddr(@a5)
  a5, a6 := magicno, currco
//  sawritef("cortn %n sending pkt %n to %n type %n %n %n %n*n",
//           a6, p, id, type, a1, a2, a3)
//FOR i=0 TO pkt_arg6 DO sawritef("%i2: %n*n", i, p!i)
  qpkt(p)
  q := cowait()
//abort(1234, p, q)
  UNLESS p=q DO
  { sawritef("Unexpected packet received in cosendpkt*n")
    sawritef("want: ") 
    FOR i = 0 TO pkt_arg6 DO sawritef("%i6", p!i)
    sawritef("*n")
    sawritef(" got: ") 
    FOR i = 0 TO pkt_arg6 DO sawritef("%i6", q!i)
    sawritef("*n")
    abort(999)
  }
  result2 := r2
  RESULTIS r1
}
  
