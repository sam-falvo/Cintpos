GET "libhdr"

LET start() = VALOF
{ FOR ch = 'A' TO 'Z' DO
  { sendpkt(-1, -3, 0, 0, 0, ch/0)
    sendpkt(-1, -3, 0, 0, 0, '*n')
  }
  RESULTIS 0
}
