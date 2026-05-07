SECTION "send"

GET "g/libhdr.h"

LET start() BE 
{ LET pkt = VEC 2
  pkt!0, pkt!1, pkt!2 := -1, 6, 1000000

  writes("Starting*n")

  { LET n = pkt!2
    IF n=0 BREAK
    pkt!2 := n-1
    qpkt(pkt)
    pkt := taskwait()
  } REPEAT

  writes("done*n")
}

