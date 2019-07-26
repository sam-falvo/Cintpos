/*
Playback is a command to play back a recorded console session
made by the record command.

Martin Richards (c) April 2003

24/02/09 MR
Moved cosendpkt, findpkt and pktlist into this module (from dlib)

28/4/03 MR
Modified to use the actions exclusiveinput, exclusiverdch and devices
in COHAND

21/4/03 MR
Modified to run under Cintpos
*/

SECTION "PLAYBACK"

GET "libhdr"
GET "manhdr"


GLOBAL
{ singlestep  : ug
  stopping
  character
  charactervalid
  delay_ticks

  notiming
  internal_ticks
  ticks_at_next_character

  last_was_lf
  break_at_lf

  kbdinco   // Kbd input coroutine
  clockco      // Clock coroutine
  dataco       // Coroutine to read characters from the recording file

  kbddone      // TRUE when kbdinco can be deleted
  clockdone    // TRUE when clockco can be deleted
  datadone     // TRUE when dataco can be deleted

  cohandgv     // COHAND's global vector

  cohandid
  kbddevid
  screendevid

  pktlist
  cosendpkt
  findpkt
}

MANIFEST
{ playback_output  =  2000
  kbd_input        =  2001

  time_unit        =  1        // One tick
  time_tick1       =  #376
  time_char        =  #377

  msecspertick     =  20

  yes              =  TRUE
  no               = FALSE

  // Globals used in COHAND
  //Gn_output_devtaskid =  ug  // See tripos/COHAND.b
  //Gn_input_devtaskid
  //Gn_input_pkts
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


LET start() BE // PLAYBACK [FROM] file [WAIT] [NOTIME]
{ LET argv = VEC 30
  LET retcode = 0
  LET playdata = 0
  LET out_pkt      = VEC pkt_arg1        // for screen characters
  LET time_expired = no       // Character should be sent to screen
  LET pkt_back     = yes      // Packet back from screen device
  LET ttab = rootnode ! rtn_tasktab
  LET ctsk = scb_task ! cli_standardoutput
  LET ctcb = ttab ! ctsk       // TCB of COHAND
  LET oldsendpkt = sendpkt

  cohandgv := tcb_gbase ! ctcb  // COHAND's global vector

//sawritef("playback: cohandid %n ctcb %n cohandgv %n*n", cohandid, ctcb, cohandgv)

  clockco := 0
  kbdinco := 0
  dataco  := 0

  UNLESS rdargs("FROM/A,WAIT/S,NOTIME/S",argv,30) DO
  { writes("Incorrect parameters for PLAYBACK*N")
    retcode := return_hard
    GOTO fin
  }

  singlestep  := argv!1  // TRUE for single stepping mode
  notiming    := argv!2  // TRUE for fast playback mode
  stopping    := FALSE
  cohandid    := ctsk
  kbddevid    := sendpkt(notinuse, cohandid, Action_devices,
                         0, 0,   // res1, res2
                         0, 0)   // arg1, arg2
  screendevid := result2
//sawritef("playback: COHAND devices in=%n out=%n*n", kbddevid, screendevid)

  last_was_lf := yes
  break_at_lf := no

  IF screendevid > 0 DO
  { writes("Can't PLAYBACK during RECORDing!*n")
    retcode := return_soft
    GOTO fin
  }

  playdata := findinput(argv!0)

  UNLESS playdata DO
  { writef("Can't open %s for PLAYBACK*n", argv!0)
    retcode := return_hard
    GOTO fin
  }

  UNLESS scb_type!playdata=scbt_file DO
  { writef("Can only PLAYBACK from a file*n")
    retcode := return_hard
    GOTO fin
  }


  dataco  := createco(datafn,  5000)
  clockco := createco(clockfn, 2000)
  kbdinco := createco(kbdfn,   4000)

  UNLESS dataco & clockco & kbdinco DO
  { writes("Insufficient memory*n")
    retcode := return_hard
    GOTO fin
  }

  // Set COHAND to exclusive input mode
  sendpkt(notinuse, cohandid, Action_exclusiveinput,
          0, 0,  // res1, res2
          TRUE)  // arg1
//sawritef("playback: now using exclusive input mode*n")

  selectinput(playdata)

  // Setup the packet for sending characters to the screen
  out_pkt!pkt_link := notinuse
  out_pkt!pkt_id   := screendevid
  out_pkt!pkt_type := playback_output

  charactervalid := FALSE // character (from recording) is not yet valid

  // Enter multi-event mode
  pktlist, sendpkt := 0, cosendpkt

  internal_ticks, ticks_at_next_character := 0, 0
  singlestep := FALSE

  // Start the coroutines
  callco(clockco)
  callco(kbdinco)
  callco(dataco) // Request first character from the recording

  // Now enter the event loop
  UNTIL clockdone & kbddone & charactervalid DO
  { LET p  = taskwait()
    LET co = findpkt(p)

//UNLESS p!pkt_id=-1 DO
//sawritef("p=%n from %n type=%n co=%n*n", p, p!pkt_id, p!pkt_type, co)

    TEST co
    THEN callco(co, p)
    ELSE SWITCHON p!pkt_type INTO
    { DEFAULT: sawritef("Unexpected pkt=%n type=%n*n", p, p!pkt_type)
               abort(1000)
               ENDCASE

      CASE playback_output:
               pkt_back := yes
               ENDCASE

      CASE Action_exclusiveinput:
      CASE Action_exclusiverdch:
               callco(kbdinco, p)
               ENDCASE
    }

    IF stopping LOOP

    IF charactervalid DO
    { IF character = endstreamch
      { stopping := TRUE
//sawritef("EOF reached, telling kbdco to stop*n")
        // Tell keyboard coroutine to stop
        callco(kbdinco, 0)
        // The clock coroutine will stop on the next tick
        // because stopping is TRUE
        LOOP
      }

      ticks_at_next_character := ticks_at_next_character + delay_ticks
      delay_ticks := 0

      IF internal_ticks >= ticks_at_next_character |
         (notiming & NOT singlestep) IF pkt_back DO
      { // The next character is available and due to be output
        internal_ticks := singlestep -> -1, ticks_at_next_character

        // Output the next character
sawrch(character)
//sawritef("writing character %n '%c'*n", character, character)
        out_pkt!pkt_arg1 := character
        qpkt(out_pkt)
        pkt_back := no

        TEST character = '*n'
        THEN { last_was_lf := TRUE
               IF break_at_lf DO
               { break_at_lf := no
                 singlestep  := yes
               }
             }
        ELSE last_was_lf := FALSE

        callco(dataco) // Request next character from the recording
      }
    }
  }

//sawritef("playback: stopping*n")

  // Finished..tidy up..
//sawritef("playback: put the COHAND input pkts back*n")

  // Return to single event mode
  sendpkt := oldsendpkt
//sawritef("playback: back in single event mode*n")

  UNLESS last_was_lf DO wrch('*n')

fin:
  IF clockco  DO deleteco(clockco)
  IF dataco   DO deleteco(dataco)
  IF kbdinco  DO deleteco(kbdinco)
  IF playdata DO endstream(playdata)

  stop(retcode)
}

AND clockfn() = VALOF
// This is the body of clockco.
// It stop when stopping=TRUE and sets clockdone to TRUE
{ clockdone := FALSE
  UNTIL stopping DO
  { delay(msecspertick)            // Delay for one tick
    UNLESS singlestep DO
    { internal_ticks := internal_ticks+1
    }
    //IF internal_ticks MOD 100 = 0 DO
    //  sawritef("clockfn: internal_ticks=%n*n", internal_ticks)
  }
  clockdone := TRUE
}

AND kbdfn() = VALOF
// This is the body of the keyboard coroutine
// It disables the COHAND task and takes over the keyboard device.
// Keyboard characters control the playback operation.
{ LET kbdinpkt     = VEC pkt_arg6
  LET stoppkt      = VEC pkt_arg6
  LET kbdinpktback = FALSE
  LET stoppktback  = FALSE
  LET stoppktsent  = FALSE
  LET pkt, ch = 0, 0

  rdch := binrdch  // MR 1/7/04

  kbddone := FALSE

  // Setup the exclusive keyboard input packet
  kbdinpkt!pkt_link := notinuse
  kbdinpkt!pkt_id   := cohandid
  kbdinpkt!pkt_type := Action_exclusiverdch
  kbdinpkt!pkt_res1 := 0
  kbdinpkt!pkt_res2 := 0
  kbdinpkt!pkt_arg1 := 0

  // Setup the exclusive keyboard input packet
  stoppkt!pkt_link := notinuse
  stoppkt!pkt_id   := cohandid
  stoppkt!pkt_type := Action_exclusiveinput
  stoppkt!pkt_res1 := 0
  stoppkt!pkt_res2 := 0
  stoppkt!pkt_arg1 := FALSE

  // Request a keyboard character from COHAND
  qpkt(kbdinpkt)

  UNTIL stoppktback & kbdinpktback DO
  { IF stopping UNLESS stoppktsent DO
    {
//sawritef("sending stop packet*n")
      stoppktsent := TRUE
      qpkt(stoppkt)
      LOOP
    }

    pkt := cowait()

    IF pkt=kbdinpkt DO
    { LET ch = pkt!pkt_res1
      kbdinpktback := TRUE
      IF stopping LOOP

//sawritef("kbdfn: ch=%i3  '%c'*n", ch, ch)
      SWITCHON ch INTO
      { DEFAULT:   // Ignore unexpected character
                   ENDCASE

        CASE ' ':  // Enter single step mode
                   internal_ticks := ticks_at_next_character
                   singlestep := yes
//sawritef("*nSingle step mode*n")
                   ENDCASE

        CASE '*e': // Write the rest of the current line
                   // then enter single stepping mode
//sawritef("*nEnter single step mode at end of current line*n")
                   break_at_lf := yes

        CASE '*n': // Leave single stepping mode
//sawritef("*nLeave single step mode*n")
                   internal_ticks := ticks_at_next_character
                   singlestep := no
                   ENDCASE

        CASE 'f':  // Enter fast playback mode
        CASE 'F':  notiming := yes
//sawritef("*nFast playback mode*n")
                   ENDCASE

        CASE 's':  // Enter normal playback mode
        CASE 'S':  notiming := no
//sawritef("*nNormal playback mode*n")
                   ENDCASE

        CASE 'q':  // Quit
        CASE 'Q':  IF stopping LOOP
//sawritef("*nQuitting*n")
                   stopping := TRUE
                   LOOP
      }
      kbdinpktback := FALSE
      qpkt(kbdinpkt) // Request another keyboard character
      LOOP
    }

    IF pkt=stoppkt DO { stoppktback := TRUE; LOOP }

    // It must be a close down request
//sawritef("Must be a close down request*n")
    stopping := TRUE
  }

//sawritef("kbddone set to TRUE*n")
  kbddone := TRUE
  cowait()
  abort(999)
}

AND datafn() = VALOF
// This is the main function of the dataco coroutine
// It sets the variables character and delay_ticks to the
// next character to output and the number of ticks to delay
// by before it should be output.
// It call cowait() with charactervalid=TRUE if the character
// is valid. If it calls cowait (from sndpkt) when charactervalid
// is FALSE, it is waiting for input from the recording data file.
// If it reaches EOF it sets stopping to TRUE and commits suicide.
// If stopping is set to TRUE by kbdinco, dataco will commit
// suicide the next time it is activated.

{ charactervalid := FALSE

  IF stopping DO
  { character, delay_ticks := -1, 0
    charactervalid := TRUE
    cowait()
    LOOP
  }

  character := rdch()
//sawritef("ch = %i3 '%c'*n", character, 32<=character<=126 -> character, ' ')
  SWITCHON character INTO
  { DEFAULT:                // No delay
    CASE endstreamch:       // EOF
               delay_ticks := 0
               ENDCASE

    CASE time_tick1:        // Single tick delay
               character, delay_ticks := rdch(), 1
               ENDCASE

    CASE time_char:         // Multiple tick delay
             { LET t = 0
               { character := rdch()
                 IF character = endstreamch DO
                 { delay_ticks := 0
                   ENDCASE
                 }
                 t := t + character
               } REPEATUNTIL character<255
               character, delay_ticks := rdch(), t
               ENDCASE
             }
  }

//sawritef("*ndelay_ticks=%n*n", delay_ticks)
  charactervalid := TRUE
//sawritef("datafn: ch = %n '%c' ticks=%n*n",
//          character, 32<=character<=126 -> character, ' ', delay_ticks)
  cowait()
//abort(1000)
} REPEAT

