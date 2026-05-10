GET "libhdr"

LET f
: 0, ?, a => a
: n, b, a => f(n-1, a+b, b)

AND fib
: n => f(n, 1, 0)

AND try : n BE writef("fib %i2 = %i2*n", n, fib(n))

LET start : => VALOF
{ FOR i= 0 TO 10 DO try(i)
  RESULTIS 0
}
