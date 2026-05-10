GET "libhdr"

LET start() = VALOF
{ LET t = TABLE
178,
100,
300,
200,
100,
250,
250,
100,
160,
266,
458,
0
LET sum = 0
LET i = 0
WHILE t!i DO
{ writef("%6.2d*n", t!i)
sum := sum+t!i
  i := i+1
}
writef("------*n")
writef("%6.2d*n", sum)
RESULTIS 0
}
