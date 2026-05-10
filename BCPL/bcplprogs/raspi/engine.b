GET "libhdr"
GET "sdl.h"
GET "sdl.b"                 // Insert the library source code
.
GET "libhdr"
GET "sdl.h"


GLOBAL {
  col_black:ug
  col_blue
  col_green
  col_yellow
  col_red
  col_majenta
  col_cyan
  col_white
  col_darkred
  col_gray
  col_lightyellow
  col_lightred
}

LET start() = VALOF
{ LET title = "First SDL Demo"
  //sawritef("engine calling initsdl*n")
  UNLESS initsdl() DO
  { writef("ERROR: Unable to initialise SDL*n")
    RESULTIS 0
  }
//sawritef("engine calling mkscreen*n")
  mkscreen(title, 600, 400)
//sawritef("engine returned from mkscreen*n")
  col_black       := maprgb(  0,   0,   0)
  col_blue        := maprgb(  0,   0, 255)
  col_green       := maprgb(  0, 255,   0)
  col_yellow      := maprgb(  0, 255, 255)
  col_red         := maprgb(255,   0,   0)
  col_majenta     := maprgb(255,   0, 255)
  col_cyan        := maprgb(255, 255,   0)
  col_white       := maprgb(255, 255, 255)
  col_darkred     := maprgb(128,   0,   0)
  col_gray        := maprgb( 70,  70,  70)
  col_lightyellow := maprgb(128, 255, 255)
  col_lightred    := maprgb(255, 128, 128)

  fillsurf(col_gray)

  setcolour(col_cyan)
  // Draw the title centred near the bottom of the screen
  drawf((screenxsize - title%0 * (fontW+charHsep))/2,
        screenysize/20,
	title)

  setcolour(col_red)               // Rails
  moveto( 100,  80)
  drawby( 400,   0)
  drawby(   0, -10)
  drawby(-400,   0)
  drawby(   0,  10)

  setcolour(col_black)             // Wheels
  drawfillcircle(250, 100, 25)
  drawfillcircle(350, 100, 25)
  setcolour(col_green)
  drawfillcircle(250, 100, 20)
  drawfillcircle(350, 100, 20)

  setcolour(col_blue)              // Base
  drawfillrect(200, 110, 400, 130)

  setcolour(col_majenta)           // Boiler
  drawfillrect(225, 135, 330, 170)

  setcolour(col_darkred)           // Cab
  drawfillrndrect(340, 135, 400, 210, 15)
  setcolour(col_lightyellow)
  drawfillrndrect(350, 170, 380, 200, 10)

  setcolour(col_lightred)          // Funnel
  drawfillrect(235, 175, 255, 210)

  setcolour(col_white)             // Smoke
  drawfillcircle(265, 235, 15)
  drawfillcircle(295, 250, 12)
  drawfillcircle(325, 255, 10)
  drawfillcircle(355, 260,  7)

wr:
  updatescreen()   //Update the screen
  sdldelay(10_000) //Pause for 10 secs
  closesdl()       //Quit SDL

  RESULTIS 0
}
