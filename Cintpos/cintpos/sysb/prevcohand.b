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
  input_buffer_upb   = 128
  echo_buffer_upb    = 127 // A circular buffer
  echo_mask          = 127
  safety_area        = 12

  // buf is used to hold completed input lines ready to
  // be sent to client tasks.
  buf_id = 1  // buf -> [link, id, end, size, <buffer>]
  buf_end
  buf_data_size

  char_bell          =   7      // ASCII character set
  char_bs            =   8
  char_tab           =   9
  char_lf            =  10      // BCPL newline character
  char_cr            =  13
  char_esc           =  27
  char_rubout        = 127      // Sometimes the encoding of backspace

  line_delete_char   = 254
  char_delete_char   = 255

  console_line_chars = 4096   // Bytes
  console_line_words = console_line_chars/bytesperword + 1 // Words
}



GLOBAL {
  out_devid : ug
  in_devid

  tagged_messages
  in_pkt_list
  out_pkt_list
  line_list
  ttyout_pkt
  out_pkt_back

  exclusiveinput
  xinpkts

  curr_in_taskno    // Currently selected task receiving input
  curr_out_taskno   // Currently selected task generating output
                    // =-1 means any task

  out_coroutine
  in_coroutine

  input_buffer
  input_ptr
  echo_buffer
  echo_iptr
  echo_optr
  reflect_on

  rubout_started

  in_line_ready
  bell_pending

  esc_done_p  // For longjump
  esc_done_l
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
  LET ibuf = VEC (input_buffer_upb + 1) / bytesperword
  LET obuf = VEC echo_buffer_upb / bytesperword

  input_buffer, echo_buffer := ibuf, obuf

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

  out_coroutine := createco(check_tty_output, 300)
//sawritef("COHAND: out cptr %n*n", out_coroutine)

  in_coroutine  := createco(handle_input, 300)
//sawritef("COHAND:  in cptr %n*n", in_coroutine)

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
  rubout_started  := FALSE

  echo_iptr, echo_optr := -1, -1

  input_ptr := -1

  // Initialise coroutines

  callco(in_coroutine)
  callco(out_coroutine)

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
      { LET qb = findpkt(@ line_list, pkt_id ! pkt)
        LET qp = add_to_queue(@ in_pkt_list, pkt)
        IF !qb DO transmit(qp,qb) // Line is waiting
        ENDCASE 
      }

      CASE Action_write:
//sawritef("COHAND: Action_write pkt=%n*n", pkt)
        add_to_queue(@ out_pkt_list, pkt)
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
               //sawritef("COHAND: giving %n to in_coroutine*n", ch)
               callco(in_coroutine, ch)
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
          ELSE { xinpkts := p
               }
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

    IF out_pkt_back DO callco(out_coroutine)

    // If the packet is now here, then suicide can
    //  be done safely, since nothing is being output
    //  at the moment.

    IF out_pkt_back & self_immolation_pkt ~= 0 DO
    // See the plot of "Ruddigore".
    { (pkt_a1!self_immolation_pkt)(self_immolation_pkt)
      RETURN
    }
  } REPEAT
} // End of START

AND read() = VALOF
{ LET char = cowait()
//sawritef("read: result of cowait() = %x2 '%c'*n", char, char)

  IF char = char_lf DO char := char_cr

  in_line_ready := char=char_cr | char=char_esc

  IF in_line_ready DO char := (char=char_cr -> '*n', '*e')

  // Check for end of rubout verify sequence

  IF char ~= char_rubout & rubout_started DO
  { rubout_started := FALSE
    put_echo('?')
  }

  // Put in echo buffer.

  put_echo(char)

  IF reflect_on & (echo_optr - echo_iptr) & echo_mask<=safety_area DO
  { signal_error()
    LOOP
  }
//sawritef("read: char = %x2 '%c'*n", char, char)
  RESULTIS char

} REPEAT

AND readesc() = VALOF
{ LET c = capitalch(read())
  IF c = 'N' DO longjump(esc_done_p, esc_done_l)
  RESULTIS c
}

AND readnum(radix,n) = VALOF
{ LET i = 2
  WHILE i > 0 DO
  { LET c = readesc()
    LET v = '0' <= c <= '9' -> c - '0',
            'A' <= c <= 'F' -> c - 'A' + 10,
                               100
    TEST v < radix THEN { n := n * radix + v; i := i - 1 }
                   ELSE signal_error()
  }
  RESULTIS n
}

AND handle_input() BE
{ LET char         = read()
  LET stream_ended = FALSE
//sawritef("handle_input: char = %i3*n", char)
  esc_done_p := level()
  esc_done_l := done

  IF char = '@' DO // Escape combination
  { char := read()

    SWITCHON char INTO

    { CASE 'A': CASE 'a':  // Set flag 1
        setflags(curr_in_taskno, #b00001)
        ENDCASE

      CASE 'B': CASE 'b':  // Set flag 2
        setflags(curr_in_taskno, #b00010)
        ENDCASE

      CASE 'C': CASE 'c':  // Set flag 3
        setflags(curr_in_taskno, #b00100)
        ENDCASE

      CASE 'D': CASE 'd':  // Set flag 4
        setflags(curr_in_taskno, #b01000)
        ENDCASE

      CASE 'E': CASE 'e':  // Send incomplete line to the
                           // currently selected task
        //setflags(curr_in_taskno, #b10000)
        ENDCASE
     
      CASE 'F': CASE 'f':  // Throw away lines.
        unloadseg(line_list)
        line_list := 0

      CASE 'H': CASE 'h':  // Hold the currently selected task
        hold(curr_in_taskno)
        ENDCASE

      CASE 'L': CASE 'l':  // Throw away line.
        reflect_on := TRUE
        input_ptr:=-1
        put_echo(line_delete_char)
        ENDCASE

      CASE 'Q': CASE 'q':  // EOF
        in_line_ready := TRUE
        stream_ended := TRUE
        put_echo('*n')
        IF input_ptr >= 0 DO put_input_char('*n')
        GOTO not_escape

      CASE 'S': CASE 's':  //  @sdd
        curr_in_taskno:=readnum(10, 0)
        curr_out_taskno := -1
        ENDCASE

      CASE 'T': CASE 't':  //  @tdd
        curr_in_taskno:=readnum(10, 0)
        curr_out_taskno := curr_in_taskno
        ENDCASE

      CASE 'U': CASE 'u':  // Unold the currently selected task
        unhold(curr_in_taskno)
        ENDCASE

      CASE 'X': CASE 'x':  // Hex character input
        put_input_char(readnum(16, 0))
        ENDCASE

      CASE 'Y': CASE 'y':  // Toggle message tags
        tagged_messages := NOT tagged_messages
        ENDCASE

      CASE 'Z': CASE 'z':  // Toggle reflection
        reflect_on := NOT reflect_on
        ENDCASE

      CASE '0': CASE '1': CASE '2': CASE '3': // Octal char input
      CASE '4': CASE '5': CASE '6': CASE '7':
        put_input_char(readnum(8, char - '0'))
        ENDCASE

      DEFAULT:  // Normal escape
        put_input_char(char)
        ENDCASE
    }

    GOTO done
  }

  IF char = char_rubout | char = char_bs DO
  { unecho()
    put_echo(char_delete_char)
    IF input_ptr >= 0 DO input_ptr := input_ptr - 1
    LOOP
  }

  IF char=endstreamch DO
  { in_line_ready := TRUE
    stream_ended := TRUE
//sawritef("EOF received from stdin*n")
    put_echo('*n')    // ???????????????????? MR 31/8/05
    IF input_ptr >= 0 DO put_input_char('*n')
    GOTO not_escape
  }

  UNLESS put_input_char(char) LOOP

not_escape:

  IF in_line_ready DO
  { LET buffer = getvec((input_ptr+1)/bytesperword+buf_data_size+1)// fudge MR 10/3/3
    LET charv = buffer + buf_data_size

    IF buffer DO
    { LET qp = findpkt(@ in_pkt_list, curr_in_taskno)
      LET qb = add_to_queue(@ line_list, buffer)

      buffer!buf_id := curr_in_taskno
      buffer!buf_end  := input_ptr + 1
//sawritef("*ncopying to charv=%n input_ptr=%n*n", charv, input_ptr)
//      FOR j=0 TO input_ptr DO sawritef(" %x2", input_buffer % j)
//sawrch('*n')
      FOR j = 0 TO input_ptr DO charv % j := input_buffer % j

      IF stream_ended DO  buffer ! buf_end := - buffer ! buf_end

      IF !qp DO transmit(qp,qb)

      input_ptr:=-1
      LOOP
    }

    signal_error()
    input_ptr:=input_ptr - 1
  }

done:
  LOOP

} REPEAT


AND check_tty_output() BE
{ // This routine outputs one buffer or one echo line
  // each time round its main loop

  TEST echo_iptr = echo_optr
  THEN { // No echo output so try for task buffer.
         LET q = (curr_out_taskno=-1 -> @ out_pkt_list,
                  findpkt(@ out_pkt_list, curr_out_taskno))
         LET p = !q

         IF p DO // Task output found
         { LET buf = pkt_a1 ! p
           LET end = pkt_a2 ! p

           !q := !p;
           !p := notinuse

           IF tagged_messages DO writef("%z2: ", pkt_id ! p)

           FOR i = 0 TO end-1 DO wrch(buf % i)

           qpkt(p) // Return the packet to the client -- MR 27/8/03

           LOOP
         }
       }

  ELSE { // Echo line waiting: output it:
         LET c = ?

         { WHILE echo_optr = echo_iptr DO workwait()

           echo_optr := echo_optr + 1
           c := echo_buffer % (echo_optr & echo_mask)
           IF c = char_delete_char DO
           { writes("*X08 *X08")
             LOOP
           }
           wrch(c)

           IF c = '*N' | c = '*E' BREAK

         } REPEAT

         LOOP
   }

   // No work whatsoever: wait
   workwait()

} REPEAT

AND print(ch) BE
{ // Lower level output routine
  pkt_a1 ! ttyout_pkt := ch
  pkt_id ! ttyout_pkt := out_devid
  qpkt(ttyout_pkt)
  out_pkt_back := FALSE
  workwait()
}

AND cohandwrch(ch) BE
{ // Higher level output routine
  TEST ch = '*n'
  THEN { print(char_cr)
         print(char_lf)
       }
  ELSE { print(ch)
       }
}

AND workwait() BE
{ // Waits for PKT or more work for output.
  cowait()
  IF bell_pending DO
  { bell_pending := FALSE
    print(char_bell)
  }
}

AND extract_item(a, id) = VALOF // Not used yet ?????
{ LET p = !a
  // p=0  or  p -> [link, id, ...]
  // Select a matching item or the first if id<0
  IF id<0 UNTIL p=0 | p!1=id DO { a := p; p := !a }
  // Unlink the selected item if found.
  IF p DO !a := !p
  RESULTIS p // Return the selected item or zero.
}

AND findpkt(lv_queue, task) = VALOF
{ LET p = !lv_queue
  IF p=0 | pkt_id!p=task RESULTIS lv_queue
  lv_queue := p
} REPEAT


AND add_to_queue(lv_q, item) = VALOF
{ WHILE !lv_q DO lv_q := !lv_q
  !lv_q, !item := item, 0
  RESULTIS lv_q
}

AND transmit(lv_pq, lv_bq) BE
{ LET b = !lv_bq
  LET p = !lv_pq
  !lv_bq, !lv_pq := !b, !p
  !p := notinuse
  pkt_r1 ! p := b + buf_data_size // Vector of bytes
  pkt_r2 ! p := b ! buf_end       // Number of bytes in the vector
//IF pkt_id!p=5 DO sawritef("COHAND: sending pkt %n to task %n *n", p, pkt_id!p)
  qpkt(p)
}

AND put_echo(char) BE IF reflect_on | char = '*N' | char = '*E' DO
{ echo_iptr := echo_iptr + 1
  echo_buffer % (echo_iptr & echo_mask) := char
  reflect_on := TRUE
}

AND unecho() BE
  // Remove a recently added character (which has not yet been echoed)
  // from the echo buffer.
  IF reflect_on DO echo_iptr := echo_iptr - 1

AND put_input_char(char) = VALOF
{ // Puts a character into the input buffer.
  IF input_ptr>=input_buffer_upb UNLESS in_line_ready DO
  { signal_error()
    RESULTIS FALSE
  }
  input_ptr:=input_ptr+1
  input_buffer % input_ptr := char
  RESULTIS TRUE
}

AND signal_error() BE
{ bell_pending := TRUE
  unecho()
}
