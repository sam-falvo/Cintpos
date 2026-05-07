GET "libhdr"

LET start() = VALOF
{ LET x = -127
  //writef("Test %8b %n  %6.1f*n", x, x, FLOAT x)  // 5n fails on 64 bit cintcode
  //wrdz(-123, 6, FALSE, TRUE)
//newline()
  (-10)(222) // cause a negative pc fault
  writen(127)   // Fails on 64 bit cintcode
newline()
  writen(-127)   // Fails on 64 bit cintcode
newline()
  writed(127, 0)   // Fails on 64 bit cintcode
newline()
  writed(-127, 0)   // Fails on 64 bit cintcode
newline()
  //writed( 123, 6)
  wrd(-123, 0)    // works ok on 64 bit cintcode
newline()
  wrd(-123, 6)    // works ok on 64 bit cintcode
newline()
  //writef("x<0 => %x2   x=%x3*n", x<0, x)
  //x := -x
  //writef("x<0 => %x2   x=%x3*n", x<0, x)
//  t(-112233.445566778899)

//  t( 9999999000.000)
//  t( 9999.999000000)
//  t(-99999.99000000)
//  t( 999999.9000000)
//  t(-9999999.000000)
//  t( 99999990.00000)
//  t(-0.9999999000000)

  RESULTIS 0
}

AND t(a) BE
{ LET a1 = sys(Sys_flt, fl_unmk, a)
  LET e1 = result2

  writef(" a1=%16x  e1=%x2",  a1,  e1)
  writef(" a1=%10i  e1=%i2",  a1,  e1)
  writef("   |%12.4f|*n", a)
}

AND wrdz(n, d, zeroes, neg) BE
{ // n     is the number to output
  // d     is the field width
  // zeroes    =TRUE is leading zeroes are to be output
  //           as zeroes. If FALSE leading zeroes are
  //           replaced by spaces.
  // neg       -TRUE if a minus sign is required.
  LET t = VEC 20
  LET i = 0
  LET k = -n

  IF neg DO { d := d - 1; k := n }

  { t!i := -(k MOD 10)
    k   := k/10
    i   := i + 1
  } REPEATWHILE k

  newline()
FOR j = 0 TO i-1 DO writef(" %x1  %4.1f", t!j, FLOAT (t!j))
newline()

  IF neg & zeroes DO wrch('-')
  FOR j = i+1 TO d DO wrch(zeroes -> '0', '*s')
  IF neg & ~zeroes DO wrch('-')
  FOR j = i-1 TO 0 BY -1 DO wrch(t!j+'0')
}

AND wrd(n, d) BE wrdz(n, d, FALSE, n<0)

