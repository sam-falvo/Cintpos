/*
This is a test for the timeout features of TCP/IP communication

Written by Martin Richards (c) April 2003

*/


SECTION "timeouts"

GET "libhdr"
//GET "manhdr"

GLOBAL
{ mainco:ug      // Main controller not running as root
  sourceco       // The coroutine that creates the data stream 
  mcopycov       // Vector of master copy coroutines
  scopycov       // Vector of slave copy coroutines
  sinkco         // The coroutine that throws away the data
  killco         // The coroutine use to kill others
  initialising   // Number of coroutines currently still initialising
                 // or -1 when all initialisation has been completed
  cocount        // Number of existing source, sink and copy coroutines

  tracing        // Controls tracing output
  stdout         // Standard output for tracing info

  n              // Number of blocks of data generated by sourceco
  k              // Number of slave copy coroutines
  size           // The number of bytes in each block of data
  hostname
  master         // TRUE if this machine should run
                 // the sourceco, sinkco and the k-1 master copy coroutine(s).
  slave          // TRUE if this machine should run
                 // the k slave copy coroutine(s).
}


LET trace(format, a,b,c,d,e,f,g) BE
{ LET out = output()
  selectoutput(stdout)
//  sawritef(format, a,b,c,d,e,f,g)
abort(1000)
  writef(format, a,b,c,d,e,f,g)
  selectoutput(out)
}

LET start() BE
{ LET oldout, oldin = output(), input()
  LET name = "tcp::7501"
  LET ipaddr = 0
  LET timeout = 5000               // Test a timeout of 5 seconds
  LET timeoutact = -2
  LET stream = 0
  LET oldsendpkt = sendpkt
  LET argv = VEC 50
  stdout := output()

  newline()
  writef("Text received from TCP::7501 will be output as*n")
  writef("decimal numbers and in character form*n")
  newline()
  writef("The following characters have special meanings:*n*n")
  writef("0     Set timeout to     0  -- no timeout*n")
  writef("+     Set timeout to  5000  -- a timeout of 5 seconds*n")
  writef("                               and timeoutact to -2*n")
  writef("-     Set timeout to -5000  -- polling mode timeout of 5 seconds*n")
  writef("A     Set timeoutact to  0  -- ignore 5 second timeout*n")
  writef("B     Set timeoutact to -1  -- make 5 second timeout generate endstreamch*n")
  writef("C     Set timeoutact t0 -2  -- make 5 second timeout generate timeoutch*n")
  writef(".     Close the connection*n")
  newline()
  stream := findinput(name)

  UNLESS stream DO
  { writef("Unable to open %s*n", name)
    RETURN
  }
  writef("Waiting for a connection on %s*n*n", name)
  writef("Use 'conn -p 7501 -h localhost' to provide some input*n")

  selectinput(stream)
  timeout, timeoutact := 8000, -2
  settimeout(stream, timeout, timeoutact)
  writef("*ntimeout set to %n  timeoutact=%n*n*n", timeout, timeoutact)

  { LET res2 = 0
//writef("timeouts: about to call getremipaddr*n")
//abort(1111)
    ipaddr := getremipaddr(stream)
    res2 := result2
    IF ipaddr BREAK
    //writef("timeouts: getremipaddr ipaddr=%x8 result2=%n*n", ipaddr, res2)
    IF res2=-2 DO
    { LET act = stream!scb_timeoutact
      IF act=-1 DO
      { writef("Attempt to make a connection timed out*n")
        writef("so abort the test*n")
        GOTO fin
      }
      writef("TIMEOUT -- try for a connection once more*n")
      writef("*nset timeoutact=-1*n*n")
      settimeoutact(stream, -1)
    }
  } REPEAT

  writef("*nConnection established with machine ipaddr=%n.%n.%n.%n*n",
          ipaddr>>24 & 255, ipaddr>>16 & 255, ipaddr>>8 & 255, ipaddr & 255)

  timeout, timeoutact := 5000, -2
  settimeout(stream, timeout, timeoutact)
  writef("*nset timeout=%n  timeoutact=%n*n*n", timeout, timeoutact)

  { LET ch = rdch() // Receive data from the TCP connection
    IF ch=endstreamch BREAK

    IF ch=pollingch DO
    { writef("rdch() => pollingch, delaying 5 seconds*n")
      delay(5000)
      LOOP
    }
    IF ch=timeoutch DO
    { writef("rdch() => timeoutch*n")
      LOOP
    }

    writef("rdch() => %i3 '%c'*n", ch, ch)

    IF ch='.' BREAK

    IF ch='-' DO
    { writef("*nset timeout=-1, timeoutact=0*n*n")
      settimeout(stream, -1, 0)
      LOOP
    }

    IF ch='+' DO
    { writef("*ntimeout set to 5000, timoutact=-2*n*n")
      settimeout(stream, 5000, -2)
      LOOP
    }

    IF ch='0' DO
    { writef("*nset timeout=0*n*n")
      settimeout(stream, 0, 0)
      LOOP
    }

    IF ch='A' DO
    { writef("*nset timeoutact=0*n*n")
      settimeoutact(stream, 0)
      LOOP
    }

    IF ch='B' DO
    { writef("*nset timeoutact=-1*n*n")
      settimeoutact(stream, -1)
      LOOP
    }

    IF ch='C' DO
    { writef("*nset timeoutact=-2*n*n")
      settimeoutact(stream, -2)
      LOOP
    }

    //abort(1000)
  } REPEAT

fin:
  endread()
  selectoutput(oldout)

  sawritef("End of test*n")
}




