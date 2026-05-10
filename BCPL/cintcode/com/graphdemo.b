GET "libhdr"

// Insert the graphics library
//MANIFEST { g_grbase=450 }

GET "graphics.h"
GET "graphics.b"


LET start() = VALOF
{ LET stdout = output()
  LET xsize, ysize = 1000, 700

  UNLESS opengraphics(xsize, ysize, mode8bit) DO
  { writef("Unable to open the graphics library*n")
    GOTO fin
  }

  FOR x = 1 TO xsize-2 DO
  { currcolour := col_red
    drawpoint33(x, 1)
    drawpoint33(x, ysize-2)
    moveto(x, 4)
    currcolour := 255*x/xsize
    drawby(0, 20)    
  }

  FOR y = 1 TO ysize-2 DO
  { currcolour := col_red
    drawpoint33(1, y)
    drawpoint33(xsize-2, y)
  }

  moveto(10, ysize-20)
  currcolour := col_black
  FOR ch = 33 TO 127 DO
  { IF ch='A' | ch='0' | ch='a' DO drawch('*n')
    drawch(ch)
  }

  currcolour := col_majenta
  fillcircle(300, 200, 65)
  currcolour := col_blue
  drawcircle(350, 250, 65)

  currcolour := col_majenta
  fillrect(150, 40, 200, 140)
  currcolour := col_cyan
  drawrect(180, 50, 240, 110)

  currcolour := col_blue
  fillrndrect(350, 40, 400, 140, 10)
  currcolour := col_green
  drawrndrect(300, 50, 440, 110, 11)

  currcolour := col_red
  fillrndrect(500, 60, 660, 230, 50)
  currcolour := col_green
  drawrndrect(620, 70, 790,  110, 130)

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
  drawstr("good day!")

  { // Plot a trajectory
    LET g = 25_000_000
    LET xc, yc = 700, 300 // The centre
    LET x, y = 700, 100
    LET xdot, ydot = 12_000, 0_000
    currcolour := col_red
    fillcircle(xc, yc, 20)

    currcolour := col_black
    FOR i = 1 TO 185 DO 
    { LET dx = x-xc
      LET dy = y-yc
      LET dist = ABS dx + ABS dy
      LET d2 = dx*dx + dy*dy
      drawpoint33(x, y)
      xdot := xdot*996/1000 - muldiv(g, dx, dist*d2)
      ydot := ydot*996/1000 - muldiv(g, dy, dist*d2)
      x := x + xdot/1000
      y := y + ydot/1000
    }
  }

wr:
  wrgraph("pic.bmp")

fin:
  closegraphics()

  RESULTIS 0
}
