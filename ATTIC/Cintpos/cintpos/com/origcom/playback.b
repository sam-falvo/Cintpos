SECTION "PLAYBACK"

GET "g/libhdr.h"
GET "g/iohdr.h"
GET "g/clihdr.h"


GLOBAL $( non_continuous  : ug
          new_buffer      : ug +  1
          new_res         : ug +  2
          new_result2     : ug +  3
          character       : ug +  4
          pkts_pending    : ug +  5
          filpkt          : ug +  6
          delay_pkt       : ug +  7
          oldsendpkt      : ug +  8
          notiming        : ug +  9
          buffer_available: ug + 10
          read_pending    : ug + 11
       $)

MANIFEST $( playback_output  =  2000
            playback_input   =  2001
            playback_delay   =  2002

            time_unit        =  1
            time_tick1       =  #376
            time_char        =  #377

            yes              =  TRUE
            no               = FALSE

            // Globals used in COHAND
            Gn_output_devtaskid =  ug + 4  // 204
            Gn_input_devtaskid  =  ug + 5  // 205
            Gn_input_pkts       =  ug + 6  // 206
         $)


LET start() BE // PLAYBACK [FROM] file [WAIT] [NOTIME]
  $( LET argv = VEC 30
     LET playdata = ?
     LET internal_ticks = 0
     LET ticks_at_next_character = ?
     LET chpktst   = VEC 10
     LET delayp    = VEC Pkt_delay
     LET out_pkt   = VEC Pkt_arg1
     LET in_pkts   = VEC 2*Pkt_res2 + 1
     LET filp      = VEC Pkt_arg3
     LET rticks    = createco(read_ticks, 200)
     LET time_expired, Pkt_back = no, yes
     LET last_was_lf = yes
     LET break_at_lf =  no
     LET ttab = rootnode ! Rtn_tasktab
     LET ctsk = - Scb_type ! cli_standardoutput
     LET ctcb = ttab ! ctsk
     LET cgv = Tcb_gbase ! ctcb
     LET buf = 0
//sawritef("playback: ctsk %n ctcb %n cgv %n*n", ctsk, ctcb, cgv)
//sawritef("playback: input devtaskid %n  output devtaskid %n*n", 
//             cgv!Gn_input_devtaskid, cgv!Gn_output_devtaskid)
     oldsendpkt := sendpkt

     filpkt    := filp
     delay_pkt := delayp

     pkts_pending := 0

     IF rdargs("FROM/A,WAIT/S,NOTIME/S",argv,30) = 0 THEN
       $( writes("Incorrect parameters for PLAYBACK*N")
          stop(Return_hard)
       $)

     non_continuous := argv ! 1 ~= 0
     notiming       := argv ! 2 ~= 0

     IF cgv ! Gn_output_devtaskid > 0 THEN
       $( writes("Can't PLAYBACK during RECORDing!*N")
          stop(Return_soft)
       $)

     playdata := findinput(argv!0)

     IF playdata = 0 | Scb_type ! playdata < 0 THEN
       $( writef("Can't open %S for PLAYBACK*N",argv!0)
          IF playdata ~= 0 THEN
            $( selectinput(playdata)
               endread()
            $)
          stop(Return_hard)
       $)



     // Check that the coroutine was created OK.

     IF rticks = 0 THEN
       $( writes("Failed to create coroutine for PLAYBACK*N")
          endread()
          stop(Return_hard)
       $)

     // Get the extra stream buffer.

     buf := getvec(200)

     IF buf = 0 THEN
       $( writes("Insuffucient store for PLAYBACK buffer*N")
          deleteco(rticks)
          endread()
          stop(Return_hard)
       $)


     // Set up the stream buffers

     sendpkt := mysendpkt1
     selectinput(playdata)
//MR     character := rdch()
//MRsawritef("playback: first character = %n*n", character)

//MR     new_buffer := Scb_buf ! cis
//MR     Scb_buf ! cis := buf

//MR     Scb_end ! cis := -1

     sendpkt := mysendpkt


     // Set up the various packets...
     $( LET inp = in_pkts
        FOR j = 1 TO 2 DO
          $( Pkt_link ! inp := notinuse
             Pkt_taskid ! inp := cgv ! Gn_input_devtaskid
             Pkt_type ! inp := playback_input
//sawritef("playback: qpkt %n to device %n*n", inp, Pkt_taskid ! inp)
             qpkt(inp)
             inp := inp + Pkt_res2 + 1
          $)
     $)


     Pkt_link ! out_pkt := notinuse
     Pkt_taskid ! out_pkt := cgv ! Gn_output_devtaskid
     Pkt_type ! out_pkt := playback_output

     Pkt_link ! delay_pkt := notinuse
     Pkt_taskid ! delay_pkt := -1           // The clock device id
     Pkt_type ! delay_pkt := playback_delay
     Pkt_delay ! delay_pkt := time_unit

     ticks_at_next_character := callco(rticks)

//sawritef("playback: character = %n*n", character)

     IF character = endstreamch GOTO file_end

//sawritef("playback: holding COHAND*n")

     // Obliterate the Console Handler...

     hold(ctsk)

     // Dequeue the COHAND's two tty input packets
     $( LET inp = cgv ! Gn_input_pkts
        FOR j = 1 TO 2 DO
          $( chpktst ! j := dqpkt(cgv!Gn_input_devtaskid, inp) ~= 0
             inp := inp + Pkt_res2 + 1
          $)
     $)

     buffer_available := yes

     read_pending     := no

     // Start timer packet bouncing....
     UNLESS non_continuous THEN
     {
//sawritef("playback: qpkt %n to device %n*n", 
//         delay_pkt, Pkt_taskid ! delay_pkt)
       qpkt(delay_pkt)
     }

     $( LET p = taskwait()

        SWITCHON Pkt_type ! p INTO

          $( CASE playback_output:
               pkts_pending := pkts_pending - 1
               Pkt_back := yes
               ENDCASE

             CASE playback_input:
               $( LET ch = (Pkt_res1 ! p) & #177
                  qpkt(p)
                  SWITCHON ch INTO
                    $( CASE ' ':
                         // Force next character
                         dqpkt(-1,delay_pkt)
                         internal_ticks :=
                           ticks_at_next_character
                         non_continuous := yes
                         ENDCASE

                       CASE #33: // Up to end of line...
                         break_at_lf := yes

                       CASE #15:
                         internal_ticks :=
                           ticks_at_next_character
                         IF non_continuous THEN
                           $( qpkt(delay_pkt)
                              non_continuous := no
                           $)
                         ENDCASE

                       CASE 'f': CASE 'F':
                         notiming := yes
                         ENDCASE

                       CASE 's': CASE 'S':
                         notiming := no
                         ENDCASE

                       CASE 'q': CASE 'Q':
                         BREAK

                   $)
               $)
               ENDCASE

             CASE playback_delay:
               internal_ticks := internal_ticks + 1
               qpkt(p)
               ENDCASE

             CASE 'R': // From filing system..
               pkts_pending := pkts_pending - 1
               new_res     := Pkt_res1 ! p
               new_result2 := Pkt_res2 ! p
               buffer_available := yes
               IF read_pending THEN
                 $( read_pending := no
                    ticks_at_next_character := callco(rticks) +
                        ticks_at_next_character
                    IF non_continuous THEN
                      internal_ticks :=
                        ticks_at_next_character - 1
                 $)
               ENDCASE

          $)

        IF [(internal_ticks - ticks_at_next_character) >= 0 |
           (notiming & NOT non_continuous)] & NOT read_pending THEN
          $( time_expired := yes
             internal_ticks := ticks_at_next_character
          $)

        IF time_expired & Pkt_back THEN
          $( Pkt_arg1 ! out_pkt := character
             qpkt(out_pkt)
             pkts_pending := pkts_pending + 1
             Pkt_back := no

             last_was_lf := character = #12

             IF last_was_lf & break_at_lf THEN
               $( break_at_lf := no
                  non_continuous := yes
                  dqpkt(-1,delay_pkt)
               $)

             ticks_at_next_character := callco(rticks) +
               ticks_at_next_character
             IF non_continuous THEN
               internal_ticks := ticks_at_next_character - 1

             IF character = endstreamch THEN
               BREAK

             time_expired := no
          $)

     $) REPEAT



file_end:
sawritef("playback: EOF reached*n")

     // Finished..tidy up..
sawritef("playback: put the COHAND input pkts back*n")

     // Put COHAND's two tty input packets back
     $( LET inp = cgv ! Gn_input_pkts
        FOR j = 1 TO 2 DO
          $( IF chpktst ! j THEN
               $( qpkt(inp)                // Send to tty input device
                  Pkt_taskid ! inp := ctsk // Make it look as COHAND sent it
               $)
             inp := inp + Pkt_res2 + 1
          $)
     $)
sawritef("playback: dequeue playback's input pkts*n")

     $( LET inp = in_pkts
        FOR j = 1 TO 2 DO
          $( dqpkt(cgv ! Gn_input_devtaskid, inp)
             inp := inp + Pkt_res2 + 1
          $)
     $)

sawritef("playback: dequeue playback's delay pkt*n")
     dqpkt(-1,delay_pkt)

sawritef("playback: release the COHAND*n")

     release(ctsk)

sawritef("playback: call taskwait() %n times*n", pkts_pending)
     FOR j = 1 TO pkts_pending DO
       taskwait()

     freevec(new_buffer)

     deleteco(rticks)

     sendpkt := oldsendpkt

     // Now a sequential task again...
     UNLESS last_was_lf THEN
       wrch('*N')

     endread()
  $)


AND mysendpkt1(link,id,type,r1,r2,a1,a2,a3) = VALOF
  $( new_res := oldsendpkt(link,id,type,r1,r2,a1,a2,a3)
     new_result2 := result2
     RESULTIS new_res
  $)


AND mysendpkt(link,id,type,r1,r2,a1,a2,a3) = VALOF
  $( IF new_res ~= 0 THEN
       $( FOR j = 0 TO Pkt_arg3 DO
            filpkt ! j := (@ link) ! j
          Pkt_arg2 ! filpkt := new_buffer
          FOR j = 0 TO (ABS new_res) - 1 DO
            a2 ! j := new_buffer ! j
          qpkt(filpkt)
          buffer_available := no
          pkts_pending := pkts_pending + 1
       $)
     result2 := new_result2
     RESULTIS new_res
  $)


AND read_ticks() = VALOF
  $( LET t = 0
//sawritef("playback: read_ticks called*n")
     character := getch()
//sawritef("playback: got character = %n*n", character)

     IF character = time_tick1 THEN
       $( t := 1
          character := getch()
       $)
     IF character = time_char THEN
       $(
          $( character := getch()
             IF character = endstreamch THEN
               BREAK
             t := t + character
          $) REPEATUNTIL character < 255
          character := getch()
       $)
     RESULTIS t
  $)


AND getch() = VALOF
  $( IF Scb_pos ! cis >= Scb_end ! cis &
        NOT buffer_available THEN
       $( read_pending := yes
          cowait(0)
       $)
//sawritef("playback: getch: calling rdch*n")
     RESULTIS rdch()
  $)
