/*
This program can be run by one of the following commands

grpalette b8       Create an image using 8 bit pixels showing the
                   colours of the default palette as an array of
                   small coloured circles.

grpalette b8alt    Create an image using 8 bit pixels showing the
                   colours of an alternative palette as an array of
                   small coloured circles.

grpalette b24      Create an image using 24 bit pixels illustrating what
                   can be done with considerably more than 256 colours.

Implemented by Martin Richards (c) July 2020

If running under Linux try the following sequence on commands.

cd ~/distribution/BCPL/bcplprogs/tests
cintsys
c b grpalette          compile grpalette
palette
ctrl-c
gimp palette.bmp       displays an image using 24-bit pixels
ctrl-c
palette b8
gimp palette.bmp       displays a palette of 256 colours using 8-bit pixels

*/


GET "libhdr"

//MANIFEST { g_grbase=450 }

// Insert the graphics library
GET "graphics.h"
GET "graphics.b"


LET start() = VALOF
{ LET stdout = output()
  LET xsize, ysize = 1000, 800
  LET mode = mode24bit
  LET argv = VEC 50
  LET s, s2 = 700, 350

  UNLESS rdargs("b8/s,b8alt/s,b24/s", argv, 50) DO
  { writef("The only valid arguments are b8 or b8alt*n")
    RESULTIS 0
  }

  IF argv!0 DO mode := mode8bit
  IF argv!1 DO mode := mode8bitalt
  IF argv!2 DO mode := mode24bit
  
  UNLESS opengraphics(xsize, ysize, mode) DO
  { writef("Unable to create the %nx%x graphics canvas*n", xsize, ysize)
    GOTO fin
  }

  currcolour := col_white
  fillrect(0, 0, xsize, ysize)

  currcolour := col_black
  FOR x = 1 TO xsize-2 DO
  { drawpoint33(x, 1)
    drawpoint33(x, ysize-2)
  }

  FOR y = 1 TO ysize-2 DO
  { drawpoint33(1, y)
    drawpoint33(xsize-2, y)
  }

  IF bmpmode=mode24bit FOR y = 0 TO s FOR x = 0 TO s DO
  { // Draw a square of colours when mode is mode24bit
    LET r = 255 * x / s     // 0 to 255   red
    LET g = 255 * y / s     // 0 to 255   green
    LET b = 255 - (r+g)/2   // 0 to 255   blue
    LET i = ((s2-x)*(s2-x)+(s2-y)*(s2-y))/s2 // 0 to 2*s2
    IF i > s2 DO i := s2
    i := 10 + 6*i/s2        // 10 to 16    brightness

    // Adjust the brightness
    r := r * i / 16
    g := g * i / 16
    b := b * i / 16
      
    currcolour := r<<16 | g<<8 | b
    drawpoint(150+x, 50+y)
  }

  
  UNLESS bmpmode=mode24bit FOR col = 0 TO 255 DO
  { // Draw the palette for mode8bit and mode8bitalt
    LET x = 150 + (col MOD 16) * 45
    LET y =  60 + (col  /  16) * 45
    currcolour := col_red
    fillcircle(x, y, 20)
    currcolour := col
    IF bmpmode=mode24bit DO
    { // col =  i i  r r  g g  b b
      LET i = col>>6 & 3 // 0 to 3   dark to bright
      LET r = col>>4 & 3 // 0 to 3   red
      LET g = col>>2 & 3 // 0 to 3   green
      LET b = col    & 3 // 0 to 3   blue
      r := (255 * (r+1)/4) * (i+5)/8
      g := (255 * (g+1)/4) * (i+5)/8
      b := (255 * (b+1)/4) * (i+5)/8
      currcolour := r<<16 | g<<8 | b
    }
    fillcircle(x, y, 17)
  }

  //writef("Calling wrgraph*n")
  wrgraph("palette.bmp")

fin:
  closegraphics()

  RESULTIS 0
}
