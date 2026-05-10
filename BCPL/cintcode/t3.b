
GET "libhdr"

LET start() = VALOF
{
  LET a = 0
  LET b = -1
  LET c = 1
  LET d = 10
  LET e = 11
  LET f = 255
  LET g = -255
  LET h = 256
  LET i = -256
  LET j = #x7FFF
  LET k = -#x7FFF
  LET l = #x8000
  LET m = -#x8000
  LET n = 100000
  LET o = -100000
writef("a=%n*n", a)
writef("b=%n*n", b)
writef("c=%n*n", c)
writef("d=%n*n", d)
writef("e=%n*n", e)
writef("f=%n*n", f)
writef("g=%n*n", g)
writef("h=%n*n", h)
writef("i=%n*n", i)
writef("j=%n*n", j)
writef("k=%n*n", k)
writef("l=%n*n", l)
writef("m=%n*n", m)
writef("n=%n*n", n)
writef("o=%n*n", o)

RESULTIS 0
}
