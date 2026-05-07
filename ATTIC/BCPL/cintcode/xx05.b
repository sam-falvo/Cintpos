GET "libhdr"

MANIFEST { S0_0_0=SLCT 0:0:0 }

LET start() = VALOF
{ LET x, y, z, s, t, u = #x1234, #x5678, 20, 30, 40, 50
  LET v = @x

  y := v   // y = v!1

  v!1 !:= 2  // Equiv to:  v!1 := v!1 ! 2

  //writef("x=%8x y=%8x z=%8x s=%n t=%n u=%n v=%n*n", x, y, z, s, t, u, v)

  //SLCT 8:16:0 OF p +:= 1

  RESULTIS 0
}
