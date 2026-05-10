/*
This test program generated the file grtst.bmp representing
and image.

Implemented by Martin Richards (c) May 2014
*/


GET "libhdr"

// Insert the graphics library
//MANIFEST { g_grbase=450 }

GET "graphics"
GET "graphics.b"


LET start() = VALOF
{ LET mode = mode24bit    // The default mode
  LET stdout = output()
  LET xsize, ysize = 1000, 800
//  LET xsize, ysize = 10, 20
//  LET xsize, ysize = 3, 5
  LET argv = VEC 50

  UNLESS rdargs("b8/s,b8alt/s,b24/s", argv, 50) DO
  { writef("Bad arguments for gtst, format: b8/s,b8alt/s,b24/s*n")
    RESULTIS 0
  }

  IF argv!0 DO mode := mode8bit     // b8/s
  IF argv!1 DO mode := mode8bitalt  // b8alt
  IF argv!2 DO mode := mode24bit    // b24/s
  
  UNLESS opengraphics(xsize, ysize, mode) DO
  { writef("Unable to open the graphics library*n")
    GOTO fin
  }

  //currcolour := col_blue
  //drawpoint(0, 0)
  //currcolour := col_green
  //drawpoint(1, 0)
  //currcolour := col_red
  //drawpoint(0, 1)
  //currcolour := col_white
  //drawpoint(1, 1)

  //currcolour := col_green
  //fillcircle(100, 200, 50)
  //fillrect(0, 0, 50, 100)
  //fillrect(0, 0, xsize/2, ysize/2)

  currcolour := col_black
  FOR x = 1 TO xsize-2 DO
  { drawpoint33(x, 1)
    drawpoint33(x, ysize-2)
  }

  FOR y = 1 TO ysize-2 DO
  { drawpoint33(1,       y)
    drawpoint33(xsize-2, y)
  }

  moveto(10, ysize-20)
  FOR ch = 33 TO 127 DO
  { IF ch='A' | ch='0' | ch='a' DO drawch('*n')
    drawch(ch)
  }

  currcolour := col_majenta
  fillcircle(300,200, 65)
  currcolour := col_blue
  drawcircle(350,250,65)

  currcolour := col_majenta
  fillrect(150, 40, 200=150, 140-40)
  currcolour := col_cyan
  drawrect(180, 50, 240-180, 110-50)

  currcolour := col_blue
  fillrndrect(350, 40, 400-350, 140-40, 10)
  currcolour := col_green
  drawrndrect(380, 50, 440=380, 110-50, 11)

  currcolour := col_red
  fillrndrect(580, 60, 660-580, 230-60, 120)
  currcolour := col_green
  drawrndrect(620, 70, 790-620, 110-70, 130)

  moveto(200, 150)
  currcolour := col_green
  drawby(-10,  40)
  drawby(-40,  10)
  drawby(-40, -10)
  drawby(-10, -40)
  drawby( 10, -40)
  drawby( 40, -10)
  drawby( 40,  10)
  drawby( 10,  40)

  currcolour := col_red
  drawby(50, 40)
  drawstr("Hello There")
  moveby(-9*11, -13)
  drawstr("Good day!")

  { // Plot a trajectory
    LET g = 25_000_000
    LET xc, yc = 700, 300 // The centre
    LET x, y = 700, 100
    LET xdot, ydot = 12_000, 0_000
    currcolour := col_red
    fillcircle(xc, yc, 20)
    FOR i = 1 TO 185 DO 
    { LET dx = x-xc
      LET dy = y-yc
      LET dist = ABS dx + ABS dy
      LET d2 = dx*dx + dy*dy
      currcolour := col_black
      drawpoint33(x, y)
      xdot := xdot*996/1000 - muldiv(g, dx, dist*d2)
      ydot := ydot*996/1000 - muldiv(g, dy, dist*d2)
      x := x + xdot/1000
      y := y + ydot/1000
    }
  }

//abort(1003)
draw:
  wrgraph("grtst.bmp")

fin:
  closegraphics()

  RESULTIS 0
}
