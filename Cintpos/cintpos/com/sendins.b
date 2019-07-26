SECTION "send"

GET "libhdr"

GLOBAL { task: 200; count: 201 }

LET start() BE 
{ LET pkt = VEC 2
  LET argv = VEC 50

  UNLESS rdargs("TASK,COUNT", argv, 50) DO
  { writef("Bad arguments for SEND*n")
    stop(20)
  }

  task, count := 7, 1_000_000

  IF argv!0 & string.to.number(argv!0) DO task  := result2
  IF argv!1 & string.to.number(argv!1) DO count := result2

  pkt!0, pkt!1, pkt!2 := notinuse, task, count

  sawritef("*nqpkt      instructions: %i5*n",
              instrcount(qpkt, pkt))
  
  sawritef("taskwait  instructions: %i5*n",
              instrcount(taskwait))

  sawritef("sendpkt   instructions: %i5*n",
              instrcount(sendpkt, notinuse, task, count, 0, 0))
}

