/*

This program provides a test harness for a TCP/IP application.  It will send
specified bytes down specified TCP connections at specified times and
produce a trace file of all bytes received.

It read a data file such as the following:

# Specify the lines

L  0 S tcp::7000
L 81 S tcp::7001          P errors
L  3 C tcp:localhost:9911 P bcr


# specify timed events

T 0:20       L 3   D "2z00021g*n"
T 0:21.312   L 3   D "2z00022*n"
T 13         L 3   D "xxx"
T 8          L 3   D "aaa"
T 5          L 3   D "yyy"
T 15         L 3   D "zzz"

Comments start with a hash (#).

TCP connections are given line numbers and are specified by
statements with the following syntax:

L <line number> S <tcp address> [P <protocol>]
L <line number> C <tcp address> [P <protocol>]

The former specified that the harness acts as a server for that connection,
and the latter runs as a client. No protocols are yet implemented.

A statements of the form:

T mins:secs.msecs L <line number> D <text>

causes <text> to be sent down the specified line starting at the
specified time relative to the start of the run.

*/

SECTION "harness"

GET "libhdr"

GLOBAL {
  stdin:ug
  stdout
  datafilename
  datastream
  tostream
  time0
  atstartofline

  lineno
  ch
  lex
  token
  lexval

  textv      // Vector to hold text strings
  textp      // textv<=textp<=textt
  textt

  datav      // Vector to hold line nodes and event nodes
  datap      // datav<=datap<=datat
  datat

  linev      // Vector to hold pointers to line nodes
             // Line data held in linev!0 to linev!lineupb

  timev      // Vector to hold pointers to sorted event nodes
  timep      // Events are in timev!1 to timev!timep
  timet      // 0<=timep<=timet

  cocount    // Number of coroutines still active
  workcount  // Number of coroutines with events still to do

  pktlist
  cosendpkt
  findpkt
}

MANIFEST {
s_line = 1
s_server
s_client
s_protocol
s_time
s_data
s_eof

textupb = 10000
lineupb =  1000
dataupb = 20000
timeupb =  5000

// Line nodes
l_type=0      // s_server or s_client
l_addr        // The TCP address
l_prot        // The protocol string
l_wkq         // List of events for this line
l_wrco        // The write coroutine for this line
l_rdco        // The read  coroutine for this line
l_size
l_upb = l_size-1

// Event nodes
e_link=0      // Link to next event for this line
e_time        // Time in msecs for this event to be activated
e_line        // Line to send the data
e_data        // data to send
e_size
e_upb = e_size-1
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


LET start() = VALOF
{ LET argv  = VEC 30

  datafilename := "tests/t1"

  stdin, stdout := input(), output()

  datastream, tostream := 0, 0
  textv, datav, linev, timev := 0, 0, 0, 0

  UNLESS rdargs("DATA,TO/K", argv, 30) DO
  { writef("Bad arguments for harness*n")
    stop(20)
  }

  IF argv!0 DO datafilename := argv!0
  writef("Data file: %s*n", datafilename)

  textv := getvec(textupb)
  textp := textv
  textt := textv + textupb

  datav := getvec(dataupb)
  datap := datav
  datat := datav + dataupb

  timev := getvec(timeupb)
  timep := 0                  // Events are in timev!1 to timev!timep

  linev := getvec(lineupb)

  UNLESS textv & datav & linev & timev DO
  { writef("More memory needed*n")
    GOTO fin
  }

//writef("textv = %n to %n upb=%n*n", textv, textt, textupb)
//writef("datav = %n to %n upb=%n*n", datav, datat, dataupb)
//writef("timev = %n to %n upb=%n*n", timev, timev+timeupb, timeupb)
//writef("linev = %n to %n upb=%n*n", linev, linev+lineupb, lineupb)

  FOR i = 0 TO lineupb DO linev!i := 0
  FOR i = 0 TO timeupb DO timev!i := 0

  UNLESS getdata(datafilename) RESULTIS 0

  prlines()
  prevents()
  newline()

  fire_events()

  //sawritef("Returned from fire_events*n")

fin:
  IF textv DO freevec(textv)
  IF datav DO freevec(datav)
  IF timev DO freevec(timev)
  IF linev DO freevec(linev)

  IF tostream DO endstream(tostream)

  writef("*nHarness finished*n")

  RESULTIS 0
}

AND error(mess, a, b, c, d) BE
{ LET oldout = output()
  selectoutput(stdout)
  writef(mess, a, b, c, d)
  selectoutput(oldout)
}

AND lex() BE
{
  //sawritef("lex: ch = %c*n", ch)
  
  SWITCHON capitalch(ch) INTO
  { DEFAULT:  error("Bad data input, line %n: ch=%n '%c'*n", lineno, ch, ch)

    CASE '#': ch := rdch() REPEATUNTIL ch='*n' | ch=endstreamch
              LOOP

    CASE endstreamch:
              token := s_eof
              RETURN

    CASE '*n': lineno := lineno + 1
    CASE '*c':
    CASE '*s':
    CASE '*t': ch := rdch()
               LOOP

    CASE 'T':
              lexval := rdtime()
              token := s_time
              RETURN

    CASE 'L':
              lexval := rdnumb()
              token := s_line
              RETURN

    CASE 'D': lexval := rdtext()
              token := s_data
              RETURN

    CASE 'S': lexval := rdtext()
              token := s_server
              RETURN

    CASE 'C': lexval := rdtext()
              token := s_client
              RETURN

    CASE 'P': lexval := rdtext()
              token := s_protocol
              RETURN
  }
} REPEAT

AND rdtext() = VALOF
{ LET ok = FALSE
  LET res, p = textp, 0
  LET string = FALSE

  // Ignore leading spaces
  ch := rdch() REPEATWHILE ch=' '

  IF ch='*"' DO { string := TRUE; ch := rdch() }

  UNTIL ch=endstreamch DO
  { // Get the next byte of data
//sawritef("ch = %i3 '%c'*n", ch, ch)
    TEST string
    THEN SWITCHON ch INTO
         { DEFAULT:  ENDCASE

           CASE '**': ch := rdch()
                      SWITCHON capitalch(ch) INTO
                      { DEFAULT:   ENDCASE
                        CASE 'T':  ch := '*t'; ENDCASE
                        CASE 'S':  ch := '*s'; ENDCASE
                        CASE 'N':  ch := '*n'; ENDCASE
                        CASE 'C':  ch := '*c'; ENDCASE
                        CASE '*"': ch := '*"'; ENDCASE
                        CASE '**': ch := '**'; ENDCASE

                        CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
                        CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
                                 { LET val = 0
                                   { val := 10*val + ch - '0'
                                     ch := rdch()
                                   } REPEATWHILE '0'<=ch<='9'
                                   unrdch()
                                   ch := val
                                   ENDCASE
                                 }
                      }
                      ENDCASE

            CASE '*n': writef("bad text, *" missing*n")
                       BREAK
            CASE '*"': ch := rdch()
                       BREAK
          }
    ELSE IF ch=' ' | ch='*n' BREAK
    p := p+1
    res%p := ch
//sawritef("p=%i2 ch= %i3 '%c'*n", p, ch, ch)
    ch := rdch()
  }

  res%0 := p
  textp := textp + p/bytesperword + 1


//sawritef("rdtext: ")
//FOR i = 1 TO res%0 DO sawritef("%c", res%i)
//sawritef("*n")
//abort(1000)
  RESULTIS res
}

AND rdtime() = VALOF
{ // return a time in msecs
  LET ok = FALSE
  LET mins, secs, msecs = 0, 0, 0

  // ddd             secs
  // ddd:dd          mins:secs
  // ddd:dd.ddd      mins:secs.msecs

  // The result is in msecs

  mins := rdnumb()

  UNLESS ch=':' | ch='.' RESULTIS mins*1000  // ddd treated as .ddd

  TEST ch=':' THEN secs := rdnumb()
              ELSE { secs := mins; mins := 0 }
  IF ch='.' DO msecs := rdnumb()


  RESULTIS (mins*60 + secs)*1000 + msecs
}

AND rdnumb() = VALOF
{ LET ok = FALSE
  LET res = 0

  ch := rdch() REPEATWHILE ch=' '

  WHILE '0'<=ch<='9' DO
  { res := 10*res + ch - '0'
    ok := TRUE
    ch := rdch()
  }
  UNLESS ok DO writef("Bad number on line %n*n", lineno)

  RESULTIS res
}

AND getdata(datafilename) = VALOF
{
  datastream := findinput(datafilename)
  UNLESS datastream DO
  { writef("Can't open file '%s'*n", datafilename)
    stop(20)
  }

  selectinput(datastream)

  lineno := 1
  ch := rdch()

nxt:
  lex()
  IF FALSE DO // ############## test lex ####################
  { LET char = '?'
    writef("token=%n ", token)
    SWITCHON token INTO
    { DEFAULT:         writef("*nUnknown token %i2*n", token)
                       //abort(999)
                       GOTO nxt

      CASE s_eof:      writef("EOF*n")
                       //abort(1000)
                       GOTO ret

      CASE s_server:   char := 'S'; GOTO wr
      CASE s_client:   char := 'C'; GOTO wr
      CASE s_data:     char := 'D'; GOTO wr
      CASE s_protocol: char := 'P'; GOTO wr

      wr:              writef("%c %s*n", char, lexval)
                       //abort(1000)
                       GOTO nxt
      CASE s_line:     writef("L %n*n", lexval)
                       //abort(1000)
                       GOTO nxt
      CASE s_time:     writef("T %n:%i2:%z3*n", lexval/60000,
                                 lexval/1000 REM 100,
                                 lexval REM 1000)
                       //abort(1000)
                       GOTO nxt
    }
  } // ############ end of lex test ##########################


  // Parse the data file
  { 
    SWITCHON token INTO
    { DEFAULT:     writef("Bad data file, line %n*n", lineno)
                   lex()
                   LOOP

      CASE s_eof:  BREAK

      CASE s_line: // Parse a line declaration
                 { LET ln = lexval
                   LET addr = 0
                   LET type = 0
                   LET prot = 0
                   LET node = datap

                   lex()
                   IF token=s_server |
                      token=s_client   DO { type, addr := token, lexval; lex() }
                   IF token=s_protocol DO { prot := lexval; lex() }

                   // Build a line node
                   FOR i = 0 TO l_upb DO node!i := 0
                   node!l_type := type
                   node!l_addr := addr
                   node!l_prot := prot
                   datap := datap+l_size

                   linev!ln := node
                   ENDCASE
                 }

      CASE s_time: // Parse an event
                 { LET t = lexval
                   LET ln = 0
                   LET data = 0
                   LET event = datap

                   lex()
                   UNLESS token=s_line DO
                   { writef("Bad data line %n: *n", lineno)
                     LOOP
                   }
                   ln := lexval
                   lex()
                   UNLESS token=s_data DO
                   { writef("Bad data line %n: *n", lineno)
                     LOOP
                   }
                   data := lexval
                   lex()

                   // Build a timed event node
                   FOR i = 0 TO e_upb DO event!i := 0
                   event!e_time := t
                   event!e_line := ln
                   event!e_data := data
                   datap := datap+e_size

                   timep := timep + 1
                   timev!timep := event

                   ENDCASE
                 }

    }
  } REPEAT

  // Sort events by increasing activation time
//writef("sorting events*n")
  sort(timev, timep)

ret:
  IF datastream DO endstream(datastream)
  RESULTIS TRUE
}

AND prlines() BE  // Print the line data
{ writef("*nLines*n*n")
  FOR i = 0 TO lineupb DO
  { LET p = linev!i
    IF p DO
    {
      //writef("Line node at %i5:  ", p)
      writef("Line %i3: ", i)
      writef("%c", p!l_type=s_server -> 'S', 'C')
      IF p!l_addr DO writef(" address %s", p!l_addr)
      IF p!l_prot DO writef(" protocol %s", p!l_prot)
      newline()
    }
  }
}

AND prevents() BE // Print the event data
{ writef("*nEvents*n*n")
  FOR i = 0 TO timep DO
  { LET p = timev!i
    IF p DO wrevent(p)
  }
}

AND wrevent(p) BE
{ LET t, ln, data = p!e_time, p!e_line, p!e_data
  LET mins = t / 60000
  LET secs = t / 1000 REM 60
  LET msecs = t REM 1000
  //writef("event node at %n:  ", p)
  sawritef("%i3:%z2.%z3  L%i3 D *"", mins, secs, msecs, ln)

//writef(" data at %n:  ", data)
  wrdatastring(data)
  sawritef("*"*n")
  //delay(1000)
//abort(3333)
}

AND wrdatastring(s) BE FOR i = 1 TO s%0 DO
{ LET char = s%i
  SWITCHON char INTO
  { DEFAULT:   sawrch(char); LOOP
    CASE '*t': sawritef("**t"); LOOP
    CASE '*s': sawritef(" ");   LOOP
    CASE '*n': sawritef("**n"); LOOP
    CASE '*c': sawritef("**c"); LOOP
  }
}


AND sort(v, upb) BE // Sort v!1 to v!upb
{ LET m = 1
  UNTIL m>upb DO m := m*3 + 1  // Find first suitable value in the
                                // series:  1, 4, 13, 40, 121, 364, ...
  { m := m/3
    FOR i = m+1 TO upb DO
    { LET vi = v!i
      LET j = i
      { LET k = j - m
        IF k<=0 | v!k!e_time < vi!e_time BREAK
        v!j := v!k
        j := k
      } REPEAT
      v!j := vi
    }
  } REPEATUNTIL m=1
}

AND fire_events() BE
{ LET curmsecs = 0
  LET oldsendpkt = sendpkt

  // Create the line coroutines 
  FOR line = 0 TO lineupb IF linev!line DO
  { LET node = linev!line
    LET wrco = createco(linewrfn, 300)
    LET rdco = createco(linerdfn, 300)
    //sawritef("creating wr coroutine %n for line %n node %n*n", wrco, line, node)
    //sawritef("creating rd coroutine %n for line %n node %n*n", rdco, line, node)
    node!l_wrco := wrco
    node!l_rdco := rdco
  }

  // Form events lists for each line

  FOR p = 1 TO timep DO
  { LET event  = timev!p
    LET line = event!e_line
    LET node = linev!line
    LET a = @node!l_wkq

    // Append the event onto the end of the work queue
    WHILE !a DO a := @(!a)!e_link
    event!e_link := 0
    !a := event

    //newline()
    //wrevent(event)
    //sawritef("appended event to the end of the wkq for line %n*n", line)
  }

//sawritef("*nentering multi-event mode after 1 sec delay*n")

delay(1000)
  //time0 := gettime()
  atstartofline := TRUE
 
  // Change to multi-event mode
  sendpkt, pktlist := cosendpkt, 0
  cocount := 0           // Count of active line coroutines
  workcount := 0         // Line coroutines with outstanding events

  // Start the server coroutines
//sawritef("*nStarting server coroutines*n")

  FOR line = 0 TO lineupb IF linev!line DO
  { LET node = linev!line
    UNLESS node!l_type=s_server LOOP
    //sawritef("*nstarting server wr coroutine for line %n*n", line)
    callco(node!l_wrco, line)
    //sawritef("*nstarting server rd coroutine for line %n*n", line)
    callco(node!l_rdco, line)
  }

//abort(9999)
//sawritef("*nStarting client coroutines*n")
  // Start the client coroutines
  FOR line = 0 TO lineupb IF linev!line DO
  { LET node = linev!line
    UNLESS node!l_type=s_client LOOP
    //sawritef("*nstarting client wr coroutine for line %n*n", line)
    callco(node!l_wrco, line)
    //sawritef("*nstarting client rd coroutine for line %n*n", line)
    callco(node!l_rdco, line)
  }

//abort(8888)
  sawritef("*nStarting the event sequence*n*n")

//sawritef("*nentered multi-event mode and coroutines all stated*n")
//sawritef("workcount=%n*n", workcount)
//sawritef("cocount=%n*n", cocount)

   //time0 := gettime()

  // Perform the event loop
  WHILE cocount DO
  { LET pkt = taskwait()
    LET co = findpkt(pkt)
//sawritef("*npkt=%n received for co=%n*n", pkt, co)
//sawritef("*ncocount=%n workcount=%n*n", cocount, workcount)

    IF co DO { callco(co, pkt)
               LOOP
             }
sawritef("*nunexpected pkt=%n received workcount=%n*n", pkt, workcount)
abort(999)
    qpkt(pkt) // Return an unexpected packet
  }

  // All events have been processed

  // Return to single event mode
  sendpkt := oldsendpkt

//sawritef("returned to single-event mode*n")

//sawritef("deleting all coroutines*n")

  // Close down all the line coroutines
  FOR line = 0 TO lineupb IF linev!line DO
  { LET node = linev!line
    LET cptr = node!l_wrco
//sawritef("deleting coroutine for line %n*n", line)
    deleteco(cptr)
  }    
//sawritef("all coroutines deleted*n")
}

AND linewrfn(line) = VALOF
{ LET node      = linev!line
  LET type      = node!l_type
  LET addr      = node!l_addr
  LET prot      = node!l_prot
  LET event     = node!l_wkq
  LET curmsecs  = 0
  LET outstream = 0

  cocount   := cocount+1
  workcount := workcount+1

  { outstream := findoutput(addr)
    IF outstream BREAK
    //writef("Attempt to write to %s failed*n", addr)
    delay(1000)
    IF workcount=0 GOTO fin
  } REPEAT
//  selectoutput(outstream)

//FOR i = 1 TO 10 DO
//{ writef("wrfn for line %n running*n", line)
//  delay(1000*2)
//}
//IF outstream DO endstream(outstream)
//workcount := workcount-1
//cocount := cocount-1
//RESULTIS 0


  WHILE event DO
  { LET newmsecs = event!e_time
    LET data = event!e_data
    LET delaymsecs = newmsecs - curmsecs
    LET v = VEC 3
//newline()
//wrevent(event)
//sawritef("*nlinefn: line=%n curmsecs=%n newmsecs=%n*n", line, curmsecs, newmsecs)
    IF delaymsecs<0 DO delaymsecs := 0
    //sawritef("*nlinefn: delaying for %n msecs*n", delaymsecs)
    IF delaymsecs DO delay(delaymsecs)
    curmsecs := newmsecs
    //sawritef("%i3:%z3 *n", curmsecs/1000,
    //                       curmsecs REM 1000)
    UNLESS atstartofline DO newline()
    atstartofline := FALSE
    wrtime()
    writef("%i3<<< %s", line, data)
    UNLESS data%(data%0)='*n' DO newline()
    atstartofline := TRUE
    //wrevent(event)
    { LET oldout = output()
      selectoutput(outstream)
      writef("%s", data)        // send data down the line
      deplete(cos)
      selectoutput(oldout)
    }

    event := event!e_link
  }

  //sawritef("All events for line %n done*n", line)
fin:
  IF outstream DO endstream(outstream)

  workcount := workcount-1
  cocount := cocount-1
  RESULTIS 0
}

AND linerdfn(line) = VALOF
{ LET node      = linev!line
  LET type      = node!l_type
  LET addr      = node!l_addr
  LET prot      = node!l_prot
  LET curmsecs  = 0
  LET instream = 0
  LET tracestream = 0
  LET len = 0
  LET buf = VEC 128

  cocount := cocount+1

//FOR i = 1 TO 10 DO
//{ wrtime()
//  writef("rdfn for line %n running*n", line)
//  delay(1000*3)
//}
//cocount := cocount-1
//RESULTIS 0


  { instream := findinput(addr)
    IF instream BREAK
    IF workcount=0 GOTO fin
    //writef("Attempt to read from %s failed*n", addr)
    delay(1000)
  } REPEAT
//  tracestream := findoutput("mbx:harness")

  selectinput(instream)
  settimeout(instream, 5000)
//  selectoutput(tracestream)

  { LET ch = rdch() // Read the first character of a line
    IF ch=timeoutch DO
    { IF workcount=0 BREAK
      //writef("%i3>>> TIMEOUTCH*n", line)
      LOOP
    }
    len := 0
    UNTIL ch='*n' | ch<0 | len>128 DO
    { UNLESS ch='*c' DO { len := len+1
                          buf%len := ch
                        }
      ch := rdch()
    }
    UNLESS atstartofline DO newline()
    atstartofline := FALSE
    wrtime()
    writef("%i3>>> ", line)
    FOR i = 1 TO len DO wrch(buf%i)
    UNLESS buf%len='*n' DO newline()
    IF ch=endstreamch BREAK
    atstartofline := TRUE
  } REPEAT
    

//  sawritef("Reading from line %n terminated*n", line)
fin:
  IF instream    DO endstream(instream)
  IF tracestream DO endstream(tracestream)

  cocount := cocount-1
  RESULTIS 0
}

AND gettime() = VALOF
{ LET v = VEC 1
  datstamp(v)
  //sawritef("datestamp: %n %i4*n", v!0, v!1)
  RESULTIS v!1 // msecs since midnight
}

AND wrtime() BE
{ LET days, msecs = 0, 0
  LET mins, secs = 0, 0
  datstamp(@days)
  //sawritef("datestamp: %n %i4  %i4*n", days, msecs)
  secs := msecs/1000
  mins := (secs / 60) MOD 60
  secs := secs MOD 60
  msecs := msecs MOD 1000
  writef("%i2:%z2.%z3 ", mins, secs, msecs)
}




