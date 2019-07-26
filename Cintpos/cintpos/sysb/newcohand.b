// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

/*
19/03/09
Made several minor changes including changes to the @ escape
sequences.

30/7/01
Modified for Cintpos
*/


// TRIPOS console handler version 3.3
SECTION "COHAND"

GET "libhdr"
GET "manhdr"

MANIFEST { 
  input_buf_upb   = 128
  echo_buf_upb    = 255 // A circular buffer of 256 byte to echo
  echo_mask       = 255
  safety_area     = 12

  // The buf structure  is used to hold completed input lines ready to
  // be sent to client tasks.
  buf_id = 1  // buf -> [link, id, end, size, <buffer>]
  buf_end
  buf_data_size

  ch_bell          =   7      // ASCII character set
  ch_bs            =   8
  ch_tab           =   9
  ch_lf            =  10      // BCPL newline character
  ch_cr            =  13
  ch_esc           =  27
  ch_rubout        = 127      // Sometimes the encoding of backspace

  ch_delete_char   = 255

  console_line_chars = 4096   // Bytes
  console_line_words = console_line_chars/bytesperword + 1 // Words
}



GLOBAL {
  out_devid : ug  // The id of the screen device
  in_devid        // The id of the keyboard device

  tagged_messages // =TRUE if output lines must identify their tasks
  in_pkt_list     // List of read pkts recieved from clients
  out_pkt_list    // List of write pkts received from clients
  line_list       // List of lines ready to send to clients
  ttyout_pkt      // The pkt used to send characters to the screen device
  out_pkt_back    // =TRUE if ttyout_pkt not in use

  exclusiveinput  // =TRUE if in eclusive input mode
  xinpkts         // List of exclusicerdch pkts recieved from clients

  curr_in_taskno  // Currently selected task receiving input
  curr_out_taskno // Currently selected task generating output
                  // =-1 means any task

  out_co          // Coroutine to echo character and client output lines
  in_co           // Coroutine to process characters from the keyboard

  input_buf       // Current incomplete input line
  input_p         // Position in input_buf of latest character added
  echo_buf        // Circular buffer of up to 256 characters to echo 
  echo_p          // Position of the latest character placed in echo_buf
  echo_q          // Position of the latest character output from echo_buf
                  // echo_p=echo_q means there are no outstanding
                  // characters to echo
  reflect_on      // =TRUE if keyboard characters are to be echoed

  in_line_complete
  bell_pending    // =TRUE causes the bell character to be sent to the
                  // screen the next time the ttyout packet is available
                  // It indicates a error has been detected.
}


LET actendinput(scb) = VALOF
{ // Although this is defined in cohand.b it is called in
  // the client's task (with the client's global vector).

  //IF scb_buf ! scb  DO sawritef("actendinput: calling freevec(%n)*n",
  //                              scb_buf ! scb - buf_data_size)
  IF scb_buf ! scb  DO freevec(scb_buf ! scb - buf_data_size)
  scb_buf ! scb := 0  // was -1 MR 25/3/02
  RESULTIS TRUE
}

AND actread(scb) = VALOF
{ // Although this is defined in cohand.b it is called in
  // the client's task (with the client's global vector).

  actendinput(scb) // Free the buffer if necessary

  // Check for @q typed previously

  // IF scb_arg1 ! scb = 0 RESULTIS FALSE
  scb_buf!scb := sendpkt(-1, scb_task!scb, Action_read)

  // Check for @q typed now

  // IF result2 <= 0 DO
  // { scb_arg1 ! scb := 0
  //   result2 := - result2
  // }

  scb_pos ! scb := 0  // MR 25/3/02
  scb_end ! scb := result2
  RESULTIS result2 > 0
}

LET actendoutput(scb) = VALOF
{ // Although this is defined in cohand.b it is called in
  // the client's task (with the client's global vector).

  LET buf = scb!scb_buf
  AND pos = scb!scb_pos
  IF buf DO
  { sendpkt(notinuse, scb_task!scb, Action_write,
            ?, ?,     // r1 and r2
            buf, pos) // Buffer and byte count
    freevec(buf)      // MR 16/3/09
    scb!scb_buf := 0
  }
  RESULTIS TRUE
}

AND actwrite(scb) = VALOF
{ // Although this is defined in cohand.b it is called in
  // the client's task (with the client's global vector).

  actendoutput()     // Output current buffer
  // Allocate a new buffer for the next line
  scb!scb_buf := getvec(console_line_words)
  scb!scb_pos := 0
  scb!scb_end := 0   // End of valid data
  scb!scb_bufend := console_line_chars
//sawritef("actwrite co=%n: allocated new output buffer %n*n", currco, scb!scb_buf)
  RESULTIS scb_buf ! scb ~= 0
}

LET start(parm_pkt) BE
{ LET ttyin_pkt1  = VEC pkt_r2
  LET ttyin_pkt2  = VEC pkt_r2
  LET ttyout_pkt1 = VEC pkt_a1
  LET self_immolation_pkt = 0
  LET ibuf = VEC (input_buf_upb + 1) / bytesperword
  LET obuf = VEC echo_buf_upb / bytesperword

  input_buf, echo_buf := ibuf, obuf

  set_process_name("Console_Handler") // MR 3/2/03

  // Assigning cohandwrch to wrch allows the console handler to
  // use writef to write characters to the screen.
  wrch := cohandwrch                  // MR 19/10/04

  in_pkt_list  := 0  // List of client read packets
  out_pkt_list := 0  // List of client write packets
  line_list   := 0  // List of buffers containing completed
                             // lines from the keyboard

  exclusiveinput, xinpkts := FALSE, 0 // MR 28/4/03

  ttyout_pkt := ttyout_pkt1
//sawritef("COHAND: starting*n")

  out_co := createco(check_tty_output, 300)
//sawritef("COHAND: out cptr %n*n", out_co)

  in_co  := createco(handle_input, 300)
//sawritef("COHAND:  in cptr %n*n", in_co)

  in_devid  := pkt_a1 ! parm_pkt
  out_devid := pkt_a2 ! parm_pkt

  pkt_link ! ttyout_pkt := notinuse
  pkt_id   ! ttyout_pkt := out_devid
  pkt_type ! ttyout_pkt := Action_ttyout

  out_pkt_back := TRUE

  pkt_link ! ttyin_pkt1 := notinuse
  pkt_id   ! ttyin_pkt1 := in_devid
  pkt_type ! ttyin_pkt1 := Action_ttyin

  qpkt(ttyin_pkt1)

  pkt_link ! ttyin_pkt2 := notinuse
  pkt_id   ! ttyin_pkt2 := in_devid
  pkt_type ! ttyin_pkt2 := Action_ttyin

  qpkt(ttyin_pkt2)

  // Finished with the parameter packet so send back
  qpkt(parm_pkt)

  // Keyboard input is initially directed to task 1 (the main CLI)
  curr_in_taskno := 1

  // Initialise states, etc...

  curr_out_taskno := -1
  reflect_on      := TRUE

  tagged_messages := FALSE  // TRUE causes an abort 110 Callco fault
  bell_pending    := FALSE

  echo_p, echo_q := -1, -1

  input_p := -1

  // Initialise coroutines

  callco(in_co)
  callco(out_co)

  { LET pkt = taskwait()

//IF pkt!pkt_id=-3 DO
//sawritef("COHAND: pkt=%n received from %n res1=%n*n", pkt, pkt!pkt_id, pkt!pkt_r1)

    SWITCHON pkt_type ! pkt INTO

    { DEFAULT: qpkt(pkt); LOOP

      CASE Action_findinput:
      { LET scb = pkt!pkt_a1
        scb!scb_type   := scbt_console
        scb!scb_task   := taskid
        scb!scb_buf    := 0
        scb!scb_pos    := 0
        scb!scb_end    := 0
        scb!scb_bufend := 0
        scb!scb_rdfn   := actread
        scb!scb_endfn  := actendinput
        returnpkt(pkt, TRUE, 0)
        LOOP
      }

      CASE Action_findoutput:
      { LET scb = pkt!pkt_a1
        scb!scb_type   := scbt_console
        scb!scb_task   := taskid 
        scb!scb_buf    := 0
        scb!scb_pos    := 0
        scb!scb_end    := 0
        scb!scb_bufend := 0
        scb!scb_wrfn   := actwrite
        scb!scb_endfn  := actendoutput
        returnpkt(pkt, TRUE, 0)
        LOOP
      }

      CASE Action_findinoutput:
        sawritef("COHAND: cannot open stream in inout mode*n")
        abort(999)
        returnpkt(pkt, TRUE, 0)
        LOOP

      CASE Action_read:
      { LET buf = extract_item(@ line_list, pkt_id ! pkt)
        TEST buf
        THEN transmit(pkt, buf) // A line was waiting to be read
        ELSE append_item(@ in_pkt_list, pkt)
        ENDCASE 
      }

      CASE Action_write:
//sawritef("COHAND: Action_write pkt=%n*n", pkt)
        append_item(@ out_pkt_list, pkt)
        ENDCASE

      CASE Action_ttyin:
      { // A character has been received by the keyboard device.
        // If in exclusive input mode give it to the first waiting
        // exclusive input packet. Ignore the character if no
        // packet is waiting.
        // If not in exclusive mode, give the character to the
        // input coroutine for processing.
        LET ch   = pkt!pkt_r1
        LET res2 = pkt!pkt_r2 // =0 if ch ok
//sawritef("COHAND: ttyin pkt=%n res1=%n res2=%n*n", pkt, ch, res2)

        // Giveup reading when the ttyin device is exhausted
        UNLESS ch=endstreamch DO qpkt(pkt) // MR 31/08/05
//sawritef("COHAND: ttyin ch=%n xinpkts=%n*n", ch, xinpkts)

        TEST exclusiveinput
        THEN IF xinpkts DO
             { // Dequeue the first xin pkt and return it
               // to the client with the newly arrived character
               LET p = xinpkts
               xinpkts := p!pkt_link
               p!pkt_link := notinuse
               p!pkt_r1, p!pkt_r2 := ch, res2
//sawritef("COHAND: returning excl'rdch pkt=%n to task %n ch='%c'*n",
//          pkt, p!pkt_id, ch)
               qpkt(p)
             }
        ELSE UNLESS res2 DO
             { // If the character is valid give it to
               // the in coroutine.
               //sawritef("COHAND: giving %n to in_co*n", ch)
               callco(in_co, ch)
             }
        ENDCASE
      }

      CASE Action_ttyout:
               out_pkt_back := TRUE
               ENDCASE

      CASE Action_exclusiverdch: // Rdch in exclusive input mode
        // If in exclusive input mode append the packet to the
        // end of xinpkts to await for a packet from the keyboard
        // device.
        IF exclusiveinput DO
        { LET p = xinpkts
          pkt!pkt_link := 0
          TEST p
          THEN { WHILE !p DO p := !p
                 !p := pkt
               }
          ELSE { xinpkts := pkt
               }
//sawritef("cohand: exclusiverdch pkt received, xinpkts=%n*n", xinpkts)
          ENDCASE
        }
        // If not in exclusive input mode, return the packet
        // immediately.
        pkt!pkt_r1 := -1 // EOF character
        pkt!pkt_r2 := -1 // Mark result as invalid
//sawritef("COHAND: returning excl'rdch pkt=%n with EOF to task %n*n",
//          pkt, pkt!pkt_id)
        qpkt(pkt)
        ENDCASE

      CASE Action_exclusiveinput: // Set/Reset Exclusive input mode
        exclusiveinput := pkt!pkt_a1
//sawritef("COHAND: exclusiveinput=%n*n", exclusiveinput)
        UNLESS exclusiveinput DO
        { // Return all the exclusiverdch packets
          LET p = xinpkts
          WHILE p DO
          { LET np = !p
             p!pkt_link := notinuse
             p!pkt_r1 := -1 // EOF character
             p!pkt_r2 := -1 // Mark result as invalid
//sawritef("COHAND: returning excl'rdch pkt=%n with EOF to task %n*n",
//          pkt, p!pkt_id)
             qpkt(p) // Return all exclusive rdch packets
             p := np
           }
         }
         qpkt(pkt) // Return set/reset exclusive packet
         ENDCASE

      CASE Action_devices: // Get/Set keyboards and screen device ids
        pkt!pkt_r1 := in_devid
        pkt!pkt_r2 := out_devid
        IF pkt!pkt_a1 DO in_devid  := pkt!pkt_a1
        IF pkt!pkt_a2 DO out_devid := pkt!pkt_a2
        qpkt(pkt)
        ENDCASE

      CASE Action_self_immolation:
        // Suicide order.
        // Allow shared output, to clear the queue.
        curr_out_taskno := -1
        self_immolation_pkt := pkt
        ENDCASE
    }

    IF out_pkt_back TEST bell_pending
    THEN { bell_pending := FALSE
           pkt_link ! ttyout_pkt := notinuse
           pkt_id ! ttyout_pkt := out_devid
           pkt_a1 ! ttyout_pkt := ch_bell
sawritef("*nSending BELL to the screen *x07*n")
           qpkt(ttyout_pkt)
           out_pkt_back := FALSE
         }
    ELSE callco(out_co)
  } REPEAT
} // End of START

AND getch() = VALOF
{ LET ch = cowait()
//sawritef("getch: result of cowait() = %x2 '%c'*n", char, char)

  IF ch = ch_lf | ch = ch_cr DO ch := '*n'

//sawritef("getch: char = %x2 '%c'*n", char, char)
  RESULTIS ch

} REPEAT

AND value(ch) = '0' <= ch <= '9' -> ch - '0',
                'A' <= ch <= 'F' -> ch - 'A' + 10,
                'a' <= ch <= 'f' -> ch - 'a' + 10,
                 100

AND getnum(radix, res) = VALOF
{ FOR i = 1 TO 2 DO
  { LET ch = getch()
    LET val = value(ch)
    UNLESS val < radix DO
    { bell_pending := TRUE
      IF i=2 DO
      { put_echo('*b') // Unecho the first digit
        put_echo(' ')
        put_echo('*b')
      }
      RESULTIS -1  // To indicate an error
    }
    put_echo(ch)   // Echo a good character
    res := res * radix + val
  }
  RESULTIS res
}

AND handle_input() BE
{ // Start of character processing loop
  LET ch           = getch()
  LET stream_ended = FALSE

/*
sawritef("handle_input: ch = %i3 echo_q=%i3 echo_p=%i3*n", ch, echo_q, echo_p)
sawritef("*necho_buf:  ")
{ LET q = echo_q
  UNTIL q = echo_p DO
  { q := (q+1) & echo_mask
    sawritef(" %i3", echo_buf%q)
  }
}
sawritef("*ninput_buf: input_p=%i3 ", input_p)
FOR i = 0 TO input_p DO sawritef(" %i3", input_buf%i)
sawrch('*n')
*/
  
  SWITCHON ch INTO
  { DEFAULT:  // All non special characters
      put_echo(ch)
      put_input_char(ch)
      LOOP

    CASE '@':   // Escape sequences
    { put_echo(ch)
      ch := getch()

      SWITCHON capitalch(ch) INTO

      { DEFAULT:   // Bad escape character
          bell_pending := TRUE
          put_echo('*b')  // Unecho the @ character
          put_echo(' ')
          put_echo('*b')
          LOOP

        CASE 'A':  // Set flag 1
          put_echo(ch)
          setflags(curr_in_taskno, #b00001)
          LOOP

        CASE 'B':  // Set flag 2
          put_echo(ch)
          setflags(curr_in_taskno, #b00010)
          LOOP

        CASE 'C':  // Set flag 3
          put_echo(ch)
          setflags(curr_in_taskno, #b00100)
          LOOP

        CASE 'D':  // Set flag 4
          put_echo(ch)
          setflags(curr_in_taskno, #b01000)
          LOOP

        CASE 'E':  // Send incomplete line to the
                   // currently selected task
          put_echo(ch)
          in_line_complete := TRUE
          GOTO line_completed

        CASE 'H':  // Hold the currently selected task
          put_echo(ch)
          hold(curr_in_taskno)
          LOOP

        CASE 'F':              // Throw away all completed input lines.
          unloadseg(line_list)
        CASE 'L':              // Throw away the current incomplete input line.
          put_echo(ch)
          reflect_on := TRUE
          input_p := -1
          put_echo('*n')
          LOOP

        CASE 'Q':  // EOF
          stream_ended := TRUE
          put_echo('*n')
          //IF input_p >= 0 DO put_input_char('*n')
          in_line_complete := TRUE
          GOTO line_completed

        CASE 'S':  //  @sdd
        { LET n = ?
          put_echo(ch)
          n := getnum(10, 0)  // get a 2 digit decimal number
          IF n<0 DO
          { // There was an error
            bell_pending := TRUE
            GOTO unecho2 // Unecho @S
          }
          curr_in_taskno := n
          curr_out_taskno := -1 // Output allowed from any task
          LOOP
        }

        CASE 'T':  //  @tdd
        { LET n = ?
          put_echo(ch)
          n := getnum(10, 0)  // get a 2 digit decimal number
          IF n<0 DO
          { // There was an error
            bell_pending := TRUE
            GOTO unecho2 // Unecho @T
          }
          curr_in_taskno  := n
          curr_out_taskno := n // Output allowed from task n only
          LOOP
        }

        CASE 'U':  // Unhold the currently selected task
          put_echo(ch)
          unhold(curr_in_taskno)
          LOOP

        CASE 'X':  // Hex character input
        { put_echo(ch)
          ch := getnum(16, 0)  // get a 2 digit hex number
          IF ch<0 DO
          { // There was an error
            bell_pending := TRUE
            GOTO unecho2 // Unecho @X
          }
          put_input_char(ch) 
          LOOP
        }

        CASE 'Y':  // Toggle message tags
          put_echo(ch)
          tagged_messages := NOT tagged_messages
          LOOP

        CASE 'Z':  // Toggle reflection
          put_echo(ch)
          reflect_on := NOT reflect_on
          LOOP

        CASE '0': CASE '1': CASE '2': CASE '3': // Octal char input
        CASE '4': CASE '5': CASE '6': CASE '7':
        { put_echo(ch)
          ch := getnum(8, ch - '0')  // get the next 2 digit octal digits
          IF ch<0 DO
          { // There was an error
            bell_pending := TRUE
            GOTO unecho2 // Unecho @d
          }
          put_input_char(ch) 
          LOOP
        }

        CASE '@':  // Normal escape
          put_echo(ch)
          put_input_char(ch)
          LOOP

      } // End of escape sequences switch

sawritef("System error handling an escape sequence*n")
      abort(999)
    } // End of CASE '@':

    CASE ch_rubout:
    CASE ch_bs:
      IF input_p >= 0 DO input_p := input_p - 1
      GOTO unecho1 // Rubout one character

    CASE '*n':
      put_echo(ch)
      put_input_char(ch)
      in_line_complete := TRUE
      GOTO line_completed

    CASE endstreamch:
      stream_ended := TRUE
//sawritef("EOF received from stdin*n")
      put_echo('*n')    // ???????????????????? MR 31/8/05
      // IF input_p >= 0 DO put_input_char('*n')
      in_line_complete := TRUE
      GOTO line_completed
  } // End of main switch

line_completed:

  { // Allocate a buffer for the completed line
    // and copy the current line into it.
    LET buf = getvec((input_p+1)/bytesperword+buf_data_size+1)
    // buf = 0 or -> [link, id, size, <characters>]

    IF buf DO
    { LET chv = @ buf ! buf_data_size
      LET pkt = extract_item(@ in_pkt_list, curr_in_taskno)


      buf!buf_id := curr_in_taskno
      buf!buf_end := input_p + 1
//sawritef("*ncopying to charv=%n input_p=%n*n", charv, input_p)
//      FOR j=0 TO input_p DO sawritef(" %x2", input_buf % j)
//sawrch('*n')
      FOR j = 0 TO input_p DO chv % j := input_buf % j
      input_p := -1

      IF stream_ended DO  buf ! buf_end := - buf ! buf_end

      TEST pkt                   // If there is a read pkt waiting 
      THEN transmit(pkt, buf)    // send the buffer to the client
      ELSE append_item(@ line_list, buf) // else queue the buffer.

      LOOP
    }

    // Failed to allocate a buffer for the completed input line.
    bell_pending := TRUE
    LOOP
  }


unecho2:
  put_echo('*b'); put_echo(' '); put_echo('*b')
unecho1:
  put_echo('*b'); put_echo(' '); put_echo('*b')
} REPEAT


AND check_tty_output() BE
{ // This routine outputs one buffer or one echo line
  // each time round its main loop

  TEST echo_p = echo_q
  THEN { // No echo output so try for task buffer.
         LET p = extract_item(@ out_pkt_list, curr_out_taskno)

         IF p DO // Task output request pkt found
         { LET buf = pkt_a1 ! p
           LET end = pkt_a2 ! p

           IF tagged_messages DO writef("%z2: ", pkt_id ! p)

           FOR i = 0 TO end-1 DO wrch(buf % i)

           !p := notinuse

           qpkt(p) // Return the packet to the client -- MR 27/8/03

           LOOP
         }
       }

  ELSE { // Echo line waiting so output it.
         
         { LET ch = ?
           WHILE echo_q = echo_p DO workwait()

           echo_q := (echo_q + 1) & echo_mask
           ch := echo_buf % echo_q
           wrch(ch)

           IF ch = '*n' BREAK

         } REPEAT

         LOOP
       }

   // Nothing to output so wait.
   workwait()

} REPEAT

AND print(ch) BE
{ // Lower level output routine
//IF out_devid>0 DO
//   sawritef("*nprint: sending to device %n ch=%i3 '%c'*n", out_devid, ch, ch)
  pkt_a1 ! ttyout_pkt := ch
  pkt_id ! ttyout_pkt := out_devid
  out_pkt_back := FALSE
  qpkt(ttyout_pkt)
  workwait()
}

AND cohandwrch(ch) BE TEST ch = '*n'
                      THEN { print(ch_cr); print(ch_lf) }
                      ELSE { print(ch) }

AND workwait() BE
{ // Waits for PKT or more work for output.
  cowait()
}

AND extract_item(a, id) = VALOF
{ LET p = !a
  // p=0  or  p -> [link, id, ...]
  // Select a matching item or the first if id<0
  IF id>0 UNTIL p=0 | p!1=id DO { a := p; p := !a }
  // Unlink the selected item if found.
  IF p DO !a := !p
  RESULTIS p // Return the selected item or zero.
}

AND append_item(a, item) BE
{ WHILE !a DO a := !a
  !a, !item := item, 0
}

AND transmit(pkt, buf) BE
{ !pkt := notinuse
  pkt_r1 ! pkt := @ buf ! buf_data_size // Vector of bytes
  pkt_r2 ! pkt := buf ! buf_end       // Number of bytes in the vector
//IF pkt_id!pkt=5 DO
//  sawritef("COHAND: sending pkt %n to task %n *n", pkt, pkt_id!pkt)
  qpkt(pkt)
}

AND put_echo(ch) BE
{ IF ch = '*n' DO reflect_on := TRUE
  IF reflect_on DO
  { LET p = (echo_p + 1) & echo_mask
    IF p=echo_q DO { bell_pending := TRUE; RETURN } // Echo buffer overflow
    echo_p := p 
    echo_buf % p := ch
  }
}

AND put_input_char(ch) = VALOF
{ // Puts a character into the input buffer.
  IF input_p >= input_buf_upb UNLESS in_line_complete DO
  { bell_pending := TRUE
abort(1234)
    RESULTIS FALSE
  }
  input_p := input_p+1
  input_buf % input_p := ch
  RESULTIS TRUE
}

