GET "libhdr"

LET start() = VALOF
{ testslct()
  RESULTIS 0
}

AND t(x, y) BE writef("%x8 %x8  %i2*n", x,y, x=y)

AND testslct() BE
{ MANIFEST {
    S0_0_0 = SLCT 0      // Full word offset 0
    S0_0_1 = SLCT 1      // Full word offset 1
    S0_4_0 = SLCT 4:0    // 28-bit field, shift of 4, offset 0 
    S8_4_1 = SLCT 8:4:1  //  8-bit field, shift of 4, offset 1 
    S8_0_0 = SLCT 8:0:0  // ls 8 bits, offset 0
  }

  LET a, b = #x12345678, #xFEDCBA98  // Two bit patterns
  LET x, y = a, b  // A two word test record
  LET r = @x       // Pointer to the record
  
  //t(S0_0_0::r, #x12345678)  // 1000
  //t(S0_0_1::r, #xFEDCBA98)  // 1001
  //t(S0_4_0::r, #x01234567)  // 1002
  t((SLCT 0:4:1)::r, #x000000A9)  // 1003
  //t(S8_4_1::r, #x000000A9)  // 1003
  //t(S8_0_0::r, #x00000078)  // 1004
  //x, y := a, b
  //S0_0_0::r := #x21436587;   t(x, #x21436587)  // 1005
  //x, y := a, b
  //S0_0_1::r := #xEFCDAB89;   t(y, #xEFCDAB89)  // 1006
  //x, y := a, b
  //S0_4_0::r := #xEFCDAB89;   t(x, #xEFCDAB898)  // 1007 ?????????
  //x, y := a, b
  //S8_4_1::r := #xA9876543;   t(y, #xFEDCB438)  // 1008
  //x, y := a, b
  //S8_0_0::r := #xCBA98765;   t(x, #x12345665)  // 1009
}
