SECTION "send"

GET "libhdr"

LET start() = VALOF
{ LET argv = VEC 50
  LET pkt = VEC 2
  LET count, bounce_id = 3000, ?

  UNLESS rdargs("BOUNCEID/A,COUNT", argv, 50) DO
  { writes("Bad arguments for SEND*n")
    RESULTIS 20
  }
 
  bounce_id := str2numb(argv!0)
  IF argv!1 DO count := str2numb(argv!1)

  writef("Sending a packet to task %n, %n times*n",
          bounce_id, count)

  pkt!0, pkt!1, pkt!2 := -1, bounce_id, count

  { LET n = pkt!2
    IF n=0 BREAK
    pkt!2 := n-1
    qpkt(pkt)
    pkt := taskwait()
  } REPEAT

  writes("done*n")
  RESULTIS 0
}

