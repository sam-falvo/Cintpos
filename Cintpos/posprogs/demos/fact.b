SECTION "fact"

GET "libhdr"

LET f(n) = n=0 -> 1, n*f(n-1)

LET start() = VALOF
$( FOR i = 1 TO 10 DO
     writef("f(%i2) = %i8*n", i, f(i))
   RESULTIS 0
$)
