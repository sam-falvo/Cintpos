GET "libhdr"

LET start() = VALOF
{
  tst(  5.123)
  tst( -5.234)
  tst( 10.345)
  tst(-100.456)
  tst(-1000.456)
  tst(-10000.456)
  tst(-0.0000456)
  tst(-0.00000456)
  tst(-0.000000456)

}

AND tst(x) BE
{  writef("x=%8x  |%13.5f|   |%13.5e|*n", x, x, x)
}

/*
-0.0000456

val=45600005 e=-12 neg=TRUE  n=8
n=8 w=13 p=5

  <-------------output-------------->
19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 S  S  S  S  S  S  -  0. 0  0  0  0  4  5  6  0  0  0  0  5
 |                 |  |              5 after rounding
 t                 s  q=-e           |
                                     r=q-p
                                     f=n-1
*/

