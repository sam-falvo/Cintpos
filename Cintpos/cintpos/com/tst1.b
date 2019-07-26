GET "libhdr"
GET "manhdr"

GLOBAL { g759:759 }

LET start() BE
{ LET name = "TCP:localhost:10002"
  LET oldin, oldout = input(), output()
  LET in, out = 0, 0
  LET ch = 0
  LET remipaddr = 0

  writef("*nCalling findinput(%s)*n", name)
  in := findinput(name)
  UNLESS in DO
  { writef("Can't open %n*n", name)
    RETURN
  }

  writef("Waiting for the connection to be established*n")

  remipaddr := sendpkt(-1, Task_tcphandler, Action_getremipaddr, 0, 0, in)
  UNLESS remipaddr DO
  { writef("Failed to connect to %s*n", name)
    RETURN
  }

  writef("Connection %s now established, remipaddr=%x8*n", name, remipaddr)

  writef("*nCalling findoutput(%s)*n", name)
  out := findoutput(name)
  UNLESS out DO
  { writef("Can't open %n*n", name)
    RETURN
  }
 
  selectinput(in)
  selectoutput(out)

  // Send a command line
  writes("echo hello; wait 5; echo there*n")

  selectoutput(oldout)
  settimeout(in, 10000) // 10 seconds

  { ch := rdch()
    IF ch<0 BREAK // EOF, timeoutch or pollingch
    wrch(ch)
  } REPEAT

  writef("*nReceived character %n*n", ch)
  writef("*nClosing the connection on %s*n", name)
  endstream(in)
  endstream(out)
}



