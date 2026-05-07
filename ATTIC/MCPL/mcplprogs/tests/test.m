GET "mcpl.h"

FUN start : =>
  FOR a = 1 TO 255 DO
   FOR b = a+1 TO 255 UNLESS a&b DO
     FOR c = b+1 TO 255 UNLESS (a|b)&c DO
     { LET k = ok(a, b, c)
       IF k>4 DO writef("%8b %8b %8b   %d\n", a,b,c, k)
     }

FUN ok : a, b, c =>
  LET all = a+b+c
  ok1(all, 0) +
  ok1(all, a) +
  ok1(all, b) +
  ok1(all, c) +
  ok1(all, a+b) +
  ok1(all, b+c) +
  ok1(all, c+a) +
  ok1(all, a+b+c)

FUN ok1 : all, w =>
  LET w1 = (w*5)>>1 & all
  RETURN w=w1 -> 1, 0
   