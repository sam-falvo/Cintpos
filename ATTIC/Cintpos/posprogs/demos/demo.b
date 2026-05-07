GET "libhdr"

LET start() = VALOF
{ LET n = 0

  writef("Please give me a number: ")
  deplete(cos)

  n := readn()

  UNLESS n RESULTIS 0

  writef("Your number was %n and its square is %n*n", n, n*n)
} REPEAT
