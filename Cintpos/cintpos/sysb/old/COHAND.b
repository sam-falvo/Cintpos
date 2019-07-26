// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

/*
30/7/01 Modified for Cintpos
*/


// TRIPOS console handler version 3.3
SECTION "COHAND"

GET "libhdr"
GET "manhdr"

MANIFEST { cg                 =  ug

           case_offset        = 'A' - 'a'
           case_mask          = NOT (- case_offset)

           n_ttyin_pkts       = 2

           input_buffer_upb   = 128
           echo_buffer_upb    = 127
           safety_area        = 12
           echo_mask          = echo_buffer_upb

           default_width      = 80

           pkt.charg          = pkt.arg1
           pkt.chres          = pkt.res1
           pkt.bufarg         = pkt.arg1
           pkt.bufres         = pkt.res1
           pkt.endarg         = pkt.arg2
           pkt.endres         = pkt.res2

           buf_task           = pkt.id
           buf_end            = buf_task + 1
           buf_data_size      = buf_end  + 1

           ttyin_pkt.upb      = pkt.res2
           ttyout_pkt.upb     = pkt.arg1

           char_cr            = #015       // ASCII character set
           char_rubout        = #177
           char_esc1          = #033
           char_esc2          = #033
           char_ctrla         = #001
           char_ctrlb         = #002
           char_ctrlc         = #003
           char_ctrld         = #004
           char_ctrle         = #005
           char_bs            = #010
           char_lf            = #012
           char_tab           = #011

           char_delete_char   = #377
           line_delete_char   = #376

           at_l = 1   // Classification of characters that can follow @
           at_st
           at_x
           at_octdig
           at_f
           at_q
           at_z

           in_stsiz           = 100   // Coroutine stack sizes
           out_stsiz          = 100

           console_line_chars = 1024
           console_line_words = console_line_chars/bytesperword + 1
         }



GLOBAL { char_bell              :  cg +   0
         terminal_width         :  cg +   1
         do_tabs                :  cg +   2
         escapeout              :  cg +   3
         output_devtaskid       :  cg +   4
         input_devtaskid        :  cg +   5
         input_pkts             :  cg +   6
         rubout_vdu             :  cg +   7
         print_check            :  cg +   8
         tagged_messages        :  cg +   9

         pending_input_queue    :  cg +  10
         pending_output_queue   :  cg +  11
         ttyout_pkt             :  cg +  12
         out_pkt.back           :  cg +  13

         current_task_number    :  cg +  15
         shared_output          :  cg +  16

         out_coroutine          :  cg +  17
         in_coroutine           :  cg +  18

         input_buffer           :  cg +  20
         input_ptr              :  cg +  21
         echo_buffer            :  cg +  22
         echo_iptr              :  cg +  23
         echo_optr              :  cg +  24
         pending_line_queue     :  cg +  25
         reflect_on             :  cg +  26

         rubout_started         :  cg +  27

         number_of_escapes      :  cg +  30
         original_string        :  cg +  31
         escape_table           :  cg +  32

         cr_or_esc              :  cg +  33
         bell_pending           :  cg +  34

         carriage_position      :  cg +  35
         print_table            :  cg +  36

         esc_done               :  cg +  40
         esc_done_p             :  cg +  41
      }


LET actendinput(scb) = VALOF
{ IF scb.buf ! scb  DO freevec(scb.buf ! scb - buf_data_size)
  scb.buf ! scb := 0  // was -1 MR 25/3/02
  RESULTIS TRUE
}

AND actread(scb) = VALOF
{ actendinput(scb) // Free the buffer if necessary

  // Check for @q typed previously

  // IF scb.arg1 ! scb = 0 RESULTIS FALSE
  scb.buf!scb := sendpkt(-1, scb.task!scb, Action_read)

  // Check for @q typed now

  // IF result2 <= 0 DO
  // { scb.arg1 ! scb := 0
  //   result2 := - result2
  // }

  scb.pos ! scb := 0  // MR 25/3/02
  scb.end ! scb := result2
  RESULTIS result2 > 0
}

LET actendoutput(scb) = VALOF
{ IF scb.buf ! scb THEN
    sendpkt(-1,scb.task!scb,Action_write,?, ?, scb.buf ! scb, scb.pos ! scb)
  // COHAND will have freed the buffer
  scb.buf ! scb := 0
  RESULTIS TRUE
}

AND actwrite(scb) = VALOF
{ // Get new buffer for next line
  actendoutput()
  scb.buf ! scb := getvec(console_line_words)
  scb.bufend ! scb := console_line_chars
  scb.end ! scb := 0   // End of valid data
  scb.pos ! scb := 0
  RESULTIS scb.buf ! scb ~= 0
}

LET start(parm_pkt) BE
{ LET ttyin_pkts = VEC n_ttyin_pkts * (ttyin_pkt.upb+1) -1
  LET ttyout_pkt.space = VEC ttyout_pkt.upb
  LET ibuf = VEC (input_buffer_upb + 1) / bytesperword
  LET obuf = VEC echo_buffer_upb / bytesperword

  LET self_immolation_pkt = 0

  input_buffer, echo_buffer := ibuf, obuf

  pending_input_queue, pending_output_queue := 0,0

  out_pkt.back := TRUE
  ttyout_pkt := ttyout_pkt.space
//sawritef("COHAND: starting*n")

  out_coroutine := createco(check_tty_output,out_stsiz)
//sawritef("COHAND: out cptr %n*n", out_coroutine)

  in_coroutine  := createco(handle_input,    in_stsiz)
//sawritef("COHAND:  in cptr %n*n", in_coroutine)

  input_devtaskid  := pkt.arg1 ! parm_pkt
  output_devtaskid := pkt.arg2 ! parm_pkt

  pkt.id     ! ttyout_pkt := output_devtaskid
  pkt.link   ! ttyout_pkt := notinuse
  pkt.type   ! ttyout_pkt := Action_ttyout

  input_pkts := ttyin_pkts
//sawritef("COHAND: 1*n")

  FOR j=1 TO n_ttyin_pkts DO
  { pkt.id     ! ttyin_pkts := input_devtaskid
    pkt.link   ! ttyin_pkts := notinuse
    pkt.type   ! ttyin_pkts := Action_ttyin

    qpkt(ttyin_pkts)
    ttyin_pkts := ttyin_pkts + ttyin_pkt.upb + 1
  }

  original_string := "@BLSTXFQZ01234567"

  escape_table := TABLE
     0, '@', '*B', -at_l,
     -at_st, -at_st,  -at_x, -at_f,-at_q,  -at_z,
     -at_octdig, -at_octdig, -at_octdig, -at_octdig,
     -at_octdig, -at_octdig, -at_octdig, -at_octdig

  number_of_escapes := original_string % 0

  // Finished with parameter packet...send back...
  qpkt(parm_pkt)

  // Current task is 1:
  current_task_number := 1

  // Initialise states, etc...
  terminal_width := default_width

  carriage_position := 0
  print_table:= TABLE #B0011111110000000, // SI  to NUL
                      #B0000100000000000, // DLE to US
                      #B1111111111111111, // /   to SP
                      #B1111111111111111, // ?   to 0
                      #B1111111111111111, // O   to @
                      #B1111111111111111, // _   to P
                      #B1111111111111111, // o   to `
                      #B0111111111111111  // DEL to p

  char_bell := #X07

  do_tabs         := TRUE
  escapeout       := TRUE
  rubout_vdu      := TRUE
  print_check     := TRUE
  shared_output   := TRUE
  reflect_on      := TRUE

  tagged_messages := FALSE
  bell_pending    := FALSE
  rubout_started  := FALSE

  echo_iptr, echo_optr := -1, -1

  input_ptr := -1
  pending_line_queue := 0

  // Initialise coroutines

  callco(in_coroutine)
  callco(out_coroutine)

  { LET pkt = taskwait()

    SWITCHON pkt.type ! pkt INTO

    { DEFAULT: qpkt(pkt); LOOP

      CASE Action_findinput:
             { LET scb = pkt!pkt.arg1
               scb!scb.type   := scbt.console
               scb!scb.task   := taskid
               scb!scb.buf    := 0
               scb!scb.pos    := 0
               scb!scb.end    := 0
               scb!scb.bufend := 0
               scb!scb.rdfn   := actread
               scb!scb.endfn  := actendinput
               returnpkt(pkt, -1, 0)
               LOOP
             }

      CASE Action_findoutput:
             { LET scb = pkt!pkt.arg1
               scb!scb.type   := scbt.console
               scb!scb.task   := taskid 
               scb!scb.buf    := 0
               scb!scb.pos    := 0
               scb!scb.end    := 0
               scb!scb.bufend := 0
               scb!scb.wrfn   := actwrite
               scb!scb.endfn  := actendoutput
               returnpkt(pkt, -1, 0)
               LOOP
             }

      CASE Action_findinoutput:
               sawritef("COHAND: cannot open stream in inout mode*n")
               abort(999)
               returnpkt(pkt, -1, 0)
               LOOP

      CASE Action_read:
             { LET qb = findpkt(@ pending_line_queue, pkt.id ! pkt)
               LET qp = add_to_queue(@ pending_input_queue, pkt)
               IF !qb \= 0 DO transmit(qp,qb) // Line is waiting
               ENDCASE 
             }

      CASE Action_write:
               add_to_queue(@ pending_output_queue,pkt)
               ENDCASE

      CASE Action_ttyin:
             { LET char = (pkt.chres ! pkt) & #177
               LET res2 = pkt.res2 ! pkt
               qpkt(pkt)
               IF res2 = 0 DO callco(in_coroutine,char)
               ENDCASE
             }

       CASE Action_ttyout:
               out_pkt.back := TRUE
               ENDCASE

       CASE Action_self_immolation:
               // Suicide order.
               // Allow shared output, to clear the queue.
               shared_output       := TRUE
               self_immolation_pkt := pkt
               ENDCASE
     }

     IF out_pkt.back DO callco(out_coroutine)

     // If the packet is now here, then suicide can
     //  be done safely, since nothing is being output
     //  at the moment.

     IF out_pkt.back & self_immolation_pkt ~= 0 DO
     // See the plot of "Ruddigore".
     { (pkt.arg1!self_immolation_pkt)(self_immolation_pkt)
       RETURN
     }
  } REPEAT
} // End of START

AND read() = VALOF
{ LET char = cowait()
//sawritef("read: result of cowait() = %x2 '%c'*n", char, char)

  IF char = char_lf DO char := char_cr

  cr_or_esc := char=char_cr | char=char_esc1 | char=char_esc2

  IF cr_or_esc DO char := (char=char_cr -> '*N', '*E')

  // Check for CTRL/A, B, C, D or E
  //   CTRL/A      Hold the task
  //   CTRL/B      Set flag bit 0
  //   CTRL/C      Set flag bit 1
  //   CTRL/D      Set flag bit 2
  //   CTRL/E      set flag bit 3

  IF char = char_ctrla DO
  { hold(current_task_number)
    LOOP
  }

  IF char_ctrlb <= char <= char_ctrle DO
  { setflags(current_task_number, 1 << (char - char_ctrlb))
    LOOP
  }

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
  IF c = 'N' DO longjump(esc_done_p, esc_done)
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
//sawritef("handle_input: char = %x2 '%c'*n", char, char)
  esc_done_p := level()

  IF char = '@' DO // Escape combination
  { LET radix = 8

    char := readesc()

    FOR j = 1 TO number_of_escapes IF char = original_string % j DO
    { LET item = escape_table ! j

      SWITCHON -item INTO

      { CASE at_f:  // Throw away lines.
                    unloadseg(pending_line_queue)
                    pending_line_queue := 0

        CASE at_l:  // Throw away line.
                    reflect_on := TRUE
                    input_ptr:=-1
                    TEST rubout_vdu THEN put_echo(line_delete_char)
                                    ELSE put_echo('*N')

        CASE at_st:  current_task_number:=readnum(10,0)
                     shared_output := char = 'S'
                     ENDCASE

        CASE at_q:   cr_or_esc := TRUE
                     stream_ended := TRUE
                     put_echo('*N')
                     IF input_ptr >= 0 DO put_input_char('*N')
                     GOTO not_escape

        CASE at_z:   reflect_on := NOT reflect_on
                     ENDCASE

        CASE at_x:   radix := 16
                     char := '0'

        CASE at_octdig:
                     item := readnum(radix,char - '0')

        DEFAULT:  // Normal escape
                     put_input_char(item)
      }

       GOTO esc_done
     }

     signal_error()

   } REPEAT  // Until legal escape


   IF char = char_rubout | char = char_bs DO
   { unecho()
     IF rubout_vdu DO
     { put_echo(char_delete_char)
       IF input_ptr >= 0 DO input_ptr := input_ptr - 1
       LOOP
     }
     IF input_ptr >= 0 DO
     { UNLESS rubout_started DO
       { rubout_started := TRUE
         put_echo('?')
       }
       put_echo(input_buffer%input_ptr)
       put_echo('_')
       input_ptr := input_ptr - 1
     }
     LOOP
   }

  IF char = char_tab & do_tabs THEN
  { LET n = ((input_ptr + 1) & #177770) + 6
    TEST n < input_buffer_upb
    THEN { unecho()
           FOR j = input_ptr TO n DO
           { put_echo(' ')
             put_input_char(' ')
           }
         }
    ELSE signal_error()
    LOOP
  }

  UNLESS put_input_char(char) LOOP

not_escape:

  IF cr_or_esc DO
  { LET buffer = getvec(input_ptr/bytesperword+buf_data_size)
    LET char_buffer = buffer + buf_data_size

    IF buffer DO
    { LET qp=findpkt(@ pending_input_queue, current_task_number)
      LET qb = add_to_queue(@ pending_line_queue, buffer)

      buffer!buf_task := current_task_number
      buffer!buf_end  := input_ptr + 1

      FOR j=0 TO input_ptr DO char_buffer % j := input_buffer % j

      IF stream_ended DO  buffer ! buf_end := - buffer ! buf_end

      IF !qp DO transmit(qp,qb)

      input_ptr:=-1
      LOOP
    }

    signal_error()
    input_ptr:=input_ptr - 1
  }

esc_done: LOOP

} REPEAT


AND check_tty_output() BE
{ // This routine outputs one buffer or one echo
  // line each time round its main loop

  TEST echo_iptr = echo_optr
  THEN { // Try for task buffer.
         LET q = (shared_output -> @ pending_output_queue,
                  findpkt(@ pending_output_queue, current_task_number))
         LET p = !q

         IF p DO // Output request!
         { LET buf = pkt.bufarg ! p
           LET end = pkt.endarg ! p

           !q := !p; !p := notinuse

           IF tagged_messages DO writef("%n: ", pkt.id ! p)

           qpkt(p)

           FOR i = 0 TO end-1 DO wrch(buf % i)

           freevec(buf)

           LOOP
         }
       }

  ELSE { // Echo line waiting: output it:
         LET c = ?

         { WHILE echo_optr = echo_iptr DO workwait()

           echo_optr := echo_optr + 1
           c := echo_buffer % (echo_optr & echo_mask)
           IF c = char_delete_char & rubout_vdu DO
           { IF carriage_position > 0 DO writes("*X08 *X08")
             LOOP
           }
           IF c = line_delete_char & rubout_vdu DO
           { LET cp = carriage_position
             print(char_cr)
             FOR j = 1 TO cp DO print(' ')
             print(char_cr)
             BREAK
           }
           wrch(c)

           IF c = '*N' | c = '*E' BREAK

         } REPEAT

         LOOP

   }

   // No work whatsoever: wait
   workwait()

} REPEAT

AND wrch(ch) BE
{ // Higher level output routine
  TEST ch = '*N'
  THEN { print(char_cr)
         print(char_lf)
       }
  ELSE { LET c = ch & #177  // No parity
         LET word, bit = c >> 4, c & 15
         IF ch = '*P' & print_check DO print('*N')
         TEST ((print_table ! word >> bit) & 1) = 0 & print_check
         THEN TEST escapeout
              THEN writef("@X%X2",ch)
              ELSE print('?')
         ELSE print(ch)
       }
}

AND print(ch) BE
{ // Lower level output routine
  LET ci = 1
  IF ch = '*e' RETURN
  IF ch = char_lf | ch = #X07 DO ci := 0
  IF ch = char_bs             DO ci := -1

  IF terminal_width <= 0 DO terminal_width := default_width

  TEST ch = char_cr
  THEN carriage_position := 0
  ELSE { IF print_check & carriage_position+ci > terminal_width DO wrch('*n')
         carriage_position := carriage_position + ci
       }

  pkt.charg  ! ttyout_pkt := ch
  pkt.id     ! ttyout_pkt := output_devtaskid
  qpkt(ttyout_pkt)
  out_pkt.back := FALSE
  workwait()
}

AND workwait() BE
{ // Waits for PKT or more work for output.
  cowait()
  IF bell_pending DO
  { bell_pending := FALSE
    print(char_bell)
  }
}

AND findpkt(lv_queue, task) = VALOF
{ //LET p = !lv_queue
  //UNLESS p | pkt.id!p=task RESULTIS lv_queue
  //lv_queue := p
  UNTIL !lv_queue = 0 | pkt.id ! (!lv_queue) = task DO lv_queue := !lv_queue
  RESULTIS lv_queue
}


AND add_to_queue(lv_q, item) = VALOF
{ LET q = findpkt(lv_q, -1)
  //WHILE !lv_q DO lv_q := !lv_q
  !q := item
  !item := 0
  RESULTIS q
}

AND transmit(lv_pq, lv_bq) BE
{ LET b = !lv_bq
  LET p = !lv_pq
  !lv_bq, !lv_pq := !b, !p
  !p := notinuse
  pkt.bufres ! p := b + buf_data_size
  pkt.endres ! p := b ! buf_end
//IF pkt.id!p=5 DO sawritef("COHAND: sending pkt %n to task %n *n", p, pkt.id!p)
  qpkt(p)
}

AND put_echo(char) BE IF reflect_on | char = '*N' | char = '*E' DO
{ echo_iptr := echo_iptr + 1
  echo_buffer % (echo_iptr & echo_mask) := char
  reflect_on := TRUE
}

AND unecho() BE IF reflect_on DO echo_iptr := echo_iptr - 1

AND put_input_char(char) = VALOF
{ // Puts character into buffer.
  IF input_ptr>=input_buffer_upb & NOT cr_or_esc DO
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
