SECTION "REC-TASK"

GET "libhdr"
//GET "g/iohdr.h"

MANIFEST $( record_stop =   -1
            act_ttyout  = 1000
            time_char   = #377
            time_tick1  = #376
            time_unit   = 1 // One tick units
         $)


GLOBAL $( fhpkt.queue   : 250
          other_scb     : 251
          pkts_needed   : 252
       $)

LET start(pkt) BE // Recording task
  $( LET stream = ?
     LET pkts = VEC pkt.arg3 * 2 + 1
     LET cgv  = pkt.arg2 ! pkt
     LET ctask= pkt.arg3 ! pkt
     LET original_dev = cgv ! 204

     LET spkt = ?
     LET can_stop, stopping = TRUE,FALSE

     LET oldsendpkt = sendpkt

     LET timing = pkt.arg5 ! pkt
     LET lasttime = ?

     pkts_needed := 0

     cis, cos := 0, 0                             //!!!!!

     currentdir := pkt.arg4 ! pkt

     stream := findoutput(pkt.arg1 ! pkt)

     other_scb := getvec(scb.upb)                 //!!!!

     IF stream = 0 | other_scb = 0 THEN
       $( returnpkt(pkt,0)
          RETURN
       $)

     IF scb.type ! stream < 0 THEN
       $( selectoutput(stream)
          endwrite()
          returnpkt(pkt,0)
          RETURN
       $)

     returnpkt(pkt,-1)


     FOR j = 0 TO scb.upb DO
       other_scb ! j := stream ! j

     selectoutput(other_scb)

     // Force a buffer into each scb:
     FOR j = 1 TO 2 DO
     $( wrch(' ')
        scb.pos ! cos := scb.pos ! cos - 1
        selectoutput(stream)
     $)

     fhpkt.queue := pkts
     !pkts := pkts + pkt.arg2 + 1

     sendpkt := mysendpkt

     cgv ! 204 := taskid

     lasttime := ticks()

     $( LET p = taskwait()

        SWITCHON pkt.type ! p INTO

          $( CASE record_stop:
               stopping := TRUE
               spkt := p
               IF can_stop THEN
                 $( cgv ! 204 := original_dev
                    BREAK
                 $)
               ENDCASE

             CASE act_ttyout:
               TEST pkt.taskid ! p = original_dev THEN
                 $( // Relay to handler...
                    pkt.taskid ! p := ctask
                    can_stop := TRUE
                    IF stopping THEN
                      $( cgv ! 204 := original_dev
                         qpkt(p)
                         BREAK
                      $)
                    qpkt(p)
                 $)
                ELSE
                 $( // Relay to device...
                    IF timing THEN
                      $( LET time = ticks()
                         LET diff = (time - lasttime)/time_unit
                         lasttime := time
                         IF diff > 0 THEN
                           TEST diff > 1 THEN
                             $( wrch(time_char)
                                WHILE diff >= 255 DO
                                  $( wrch(255)
                                     diff := diff - 255
                                  $)
                                wrch(diff)
                             $)
                            ELSE
                             wrch(time_tick1)
                      $)

                    wrch(pkt.arg1 ! p)
                    pkt.taskid ! p := original_dev
                    qpkt(p)
                    can_stop := FALSE
                 $)
               ENDCASE


             CASE 'W': // From filehandler
               pkts_needed := pkts_needed - 1
               !p := fhpkt.queue; fhpkt.queue := p
               ENDCASE

          $)

     $) REPEAT


     // Finish off...

     sendpkt := oldsendpkt

     UNTIL pkts_needed = 0 DO
       $( taskwait()
          pkts_needed := pkts_needed - 1
       $)

     freevec(other_scb)
     freevec(scb.buf ! other_scb)

     endwrite()
     qpkt(spkt)
  $)


AND mysendpkt(link,id,type,res1,res2,arg1,arg2,arg3) = VALOF
  $( LET s = cos
     LET p = fhpkt.queue; fhpkt.queue := !p
     FOR j = 0 TO pkt.arg3 DO
       p ! j := (@ link) ! j
     qpkt(p)
     pkts_needed := pkts_needed + 1
     cos := other_scb
     other_scb := s
     RESULTIS -1
  $)


AND ticks() = VALOF // Gets time in 'ticks', ignoring overflow
  $( LET m1, m2 = rootnode ! rtn.ticks, rootnode ! rtn.mins
     $( LET m3, m4 = rootnode ! rtn.ticks, rootnode ! rtn.mins
        IF m1 = m3 & m2 = m4 THEN
          BREAK
        m1, m2 := m3, m4
     $) REPEAT
     RESULTIS m2 * 60 * tickspersecond + m1
  $)



