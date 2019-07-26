// This is the body of the IDLE task

SECTION "IDLE"

GET "libhdr"

LET start(pkt) BE
{ 
//  sawritef("IDLE: task started  pkt = %n*n", pkt)

  set_process_name("Idle_task")

  pkt_link!pkt := -1
  pkt_id!pkt   :=  1   // Send it to the CLI (task 1)
  pkt_type!pkt :=  0
//sawritef("IDLE: Send a startup pkt (%n,%n,%n) to the CLI task*n",
//          pkt_link!pkt, pkt_id!pkt, pkt_type!pkt
//        )

  // Send a startup pkt to the CLI (task 1)
  qpkt(pkt)
  // The CLI will not return it

//  sawritef("IDLE: continuing to run*n")

  { //sawritef("IDLE: calling sys(Sys_waitirq, 2000)*n");

    // Since SIGINT cannot do a cond_signal, we have to poll for intflag
    // but some other devices may request interrupts so we also have to do
    // a cond_wait. Cond_timedwait allows us to wait with a time out.
//sawritef("idle: waiting*n")
    sys(Sys_waitirq, 20)  // Wait for irq with 20 msec timeout
    //sys(Sys_waitirq, 2000)  // Wait for irq with 2000 msec timeout
    //sys(Sys_trpush, #x1D000000)
  } REPEAT

  //sawritef("IDLE: Returning to Unix from IDLE task*n")
  sys(Sys_quit,0)
}
